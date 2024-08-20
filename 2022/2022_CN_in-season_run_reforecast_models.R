# Load packages and functions ------------------------------------------------------

pkgs <- c("tidyverse","bbmle","ggpmisc","reshape2","glmmTMB","lme4","DHARMa","MuMIn")
#install.packages(pkgs)

library(tidyverse); theme_set(theme_bw(base_size = 16))
library(bbmle)
library(ggpmisc)
library(reshape2)
library(glmmTMB)
library(lme4)
library(DHARMa)
library(MuMIn)

#Overdispersion check from Bolker (https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-for-overdispersioncomputing-overdispersion-factor)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

# Load predictor data ----------------------------------------------------

bs_cn <- read.csv("CN_return_predictors.csv") %>% 
  filter(!year <= 2001) %>% #Exclude 2001 data due to inseason management changes
  #Bryan also excludes 2000 data, not sure why
  mutate(SEAK_adults = SEAK_a4 + SEAK_a5 + SEAK_a6) #%>% # Exclude Age 3s in 2022 (no CWTs applied)
  #mutate(across(NTR_a2:NTR_a6, as.numeric),
  #       NTR_adults = NTR_a4 + NTR_a5 + NTR_a6) 



# NTR & SEAK recoveries model ---------------------------------------------

bs_cn %>% 
  mutate(SEAK_NTR = SEAK_adults  +NTR_adults) %>% 
  pivot_longer(SEAK_adults:SEAK_NTR) %>% 
  filter(value > 0) %>% 
  ggplot(aes(value, Somass_term_adult_return)) +
  facet_wrap(~name, nrow = 1, scales = "free_x") + 
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(x = "Expanded CWT recoveries (log-transformed)",
       y = "Somass terminal adult Chinook return\n(log-transformed)")


# Week 82 CPUE model ----------------------------------------------

#Plot the relationship
(wk82.p <- ggplot(bs_cn, aes(wk_82_cpue, Somass_term_adult_return)) +
   geom_point() +
   geom_text(aes(label=year),hjust=0, vjust=0) +
   #coord_cartesian(ylim = c(0,150000)) +
   #scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
   labs(y = "Somass return", x = "Week 82 CPUE") 
)

wk82.p +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")

#Use log transform
wk82.rf <- lm(log(Somass_term_adult_return) ~ log(wk_82_cpue), bs_cn)
summary(wk82.rf) #weak

#Log transformation helpful?
wk82.rf2 <- update(wk82.rf, exp(.) ~ exp(.))
summary(wk82.rf2) #Yes, the linear relationship is considerably worse

#Plot the predictions
wk82.pred <- bs_cn %>% #Generate new dataset to cast predicitons into
  select(year, wk_82_cpue) %>% 
  filter(!is.na(wk_82_cpue))

wk82.pred <- cbind(wk82.pred, predict(wk82.rf, wk82.pred, interval = "pred")) %>% 
  mutate(across(fit:upr, exp)) 

wk82.p +
  geom_smooth(data = wk82.pred, aes(y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk82.pred %>% filter(year == 2021), 
                  aes(y=fit,ymin=lwr,ymax = upr), 
                  colour = "red") +
  scale_y_continuous(labels = scales::comma)
#High uncertainty


# Week 83 CPUE model ----------------------------------------------

#Plot the relationship
(wk83.p <- ggplot(bs_cn, aes(wk_83_cpue, Somass_term_adult_return)) +
   #geom_point() +
   geom_label(aes(label=year),hjust=0, vjust=0, alpha = 0.25) +
   #scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
   #coord_cartesian(ylim = c(0,150000)) +
   labs(y = "Somass adult return", x = "Week 83 CPUE")
)

wk83.p +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log", labels = scales::comma)
#Unclear whether log transformation is helping

#Log-log regression model
wk83.rf <- lm(log(Somass_term_adult_return) ~ log(wk_83_cpue), data = bs_cn)
#log-linear
wk83.rf2 <- update(wk83.rf, .~exp(.))
#linear
wk83.rf3 <- update(wk83.rf, exp(.)~exp(.))

bbmle::AICctab(wk83.rf,wk83.rf2,wk83.rf3)

# Log-log relationship showing strongest R^2
map(set_names(list(wk83.rf,wk83.rf2,wk83.rf3),
              c("log-log","log-linear","linear")), 
              ~summary(.x)$adj.r.squared)

#Summary and some plots
plot(wk83.rf, labels.id = bs_cn$year) #Some outliership in 2009 and 2014; acceptable leverage
# Best to keep 2009. 

#Dataframe for plotting prediction
wk83.pred <- bs_cn %>% #Generate new dataset to cast predictions into
  select(year, wk_83_cpue) %>% 
  filter(!is.na(wk_83_cpue)) %>%
  # Store predictions
  cbind(.,predict(wk83.rf,., interval = "pred", level = .75)) %>% 
  mutate(across(fit:upr, exp)) # reverse the log transformation


#Plot
wk83.p +
  #scale_x_continuous(trans = "log") +
  geom_smooth(data = wk83.pred, aes(y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk83.pred %>% filter(year == 2022), 
                  aes(y=fit,ymin=lwr,ymax = upr), 
                  colour = "red") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0,300000, by = 50000)) +
  annotate("text", x = 1, y = 200000, label = "italic(R)^2 == 0.61", parse = TRUE)
# 133k assumed.


# Week 84 cum CPUE model ------------------------------------------------------

#Plot the linear relationship
wk84.p <- ggplot((bs_cn 
                   #%>% melt(measure.vars = c("wk_84_cpue", "wk_84_cum_cpue"))
                   ), 
                  aes(#value, 
                      wk_84_cum_cpue,
                      Somass_term_adult_return)) +
   #facet_grid(~variable) +
   geom_point() +
   #scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
   #coord_cartesian(ylim = c(0,150000)) +
   labs(y = "Somass return", x = "Week 84 cumulative CPUE")

# Show log-linear relationship
wk84.p +
  scale_y_continuous(trans = "log", labels = scales::comma)
#Unclear whether log transformation will help

#Log-log regression model on week 84 CPUE
wk84.rf <- lm(log(Somass_term_adult_return) ~ log(wk_84_cpue), data = bs_cn)
summary(wk84.rf)$adj.r.squared #0.61
#log-linear
summary(wk84.rf2 <- update(wk84.rf, .~exp(.)))$adj.r.squared #0.62
#linear
summary(wk84.rf3 <- update(wk84.rf, exp(.)~exp(.)))$adj.r.squared #0.66

#Log-log regression model on cumulative CPUE to week 84 (includes weeks 82 & 83)
summary(wk84.rf4 <- update(wk84.rf, .~log(wk_84_cum_cpue)))$adj.r.squared #0.66
#log-linear
summary(wk84.rf5 <- update(wk84.rf4, .~exp(.)))$adj.r.squared #0.67
#linear
summary(wk84.rf6 <- update(wk84.rf4, exp(.)~exp(.)))$adj.r.squared #0.70
# Linear on cumulative CPUE seems to be working best. 

#Summary and some plots
#plot(wk84.rf6, labels.id = bs_cn$year) #Looks ok

#Plot of residuals by year
ggplot(bs_cn %>% 
         filter(year != max(year)) %>% 
         mutate(res = residuals(wk84.rf6)),
       aes(x = year, y = res)) + 
  scale_x_continuous(n.breaks = length(unique(bs_cn$year)), minor_breaks = NULL) +
  geom_segment(aes(xend=year, yend=0), color="blue") +
  geom_point() +
  labs(y = "Residual value", x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Plot predictions
wk84.pred <- bs_cn %>% #Generate new dataset to cast predicitons into
  select(year, wk_84_cum_cpue) %>% 
  filter(!is.na(wk_84_cum_cpue))

#Add the model predictions
wk84.pred <- (cbind(wk84.pred, predict(wk84.rf6, wk84.pred, interval = "pred"))
              #%>% mutate(across(fit:upr, exp))
)

#Plot
wk84.p +
  #scale_x_continuous(trans = "log") +
  geom_smooth(data = wk84.pred, aes(y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk84.pred %>% filter(year == max(year)), 
                  aes(y=fit,ymin=lwr,ymax = upr), 
                  colour = "red")

# Week 91 cum CPUE model ------------------------------------------------------

#Plot the linear relationship
(wk91.p <- ggplot((bs_cn 
                   #%>% melt(measure.vars = c("wk_91_cpue", "wk_91_cum_cpue"))
), 
aes(#value, 
  wk_91_cum_cpue,
  Somass_term_adult_return)) +
  geom_label(aes(label=year),hjust=0, vjust=0, alpha = 0.25) +
  #facet_grid(~variable) +
  #geom_point() +
  #scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
  #coord_cartesian(ylim = c(0,150000)) +
  labs(y = "Somass return", x = "Week 91 cumulative CPUE")
)

# Show log-linear relationship
wk91.p +
  #scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log", labels = scales::comma)
#Unclear whether log transformation will help

#Log-log regression model on week 91 CPUE
wk91.rf <- lm(log(Somass_term_adult_return) ~ log(wk_91_cpue), data = bs_cn)
summary(wk91.rf)$adj.r.squared #0.60
#log-linear
summary(wk91.rf2 <- update(wk91.rf, .~exp(.)))$adj.r.squared #0.44
#linear
summary(wk91.rf3 <- update(wk91.rf, exp(.)~exp(.)))$adj.r.squared #0.43

#Log-log regression model on cumulative CPUE to week 91 (includes weeks 82 & 83)
summary(wk91.rf4 <- update(wk91.rf, .~log(wk_91_cum_cpue)))$adj.r.squared #0.70
#log-linear
summary(wk91.rf5 <- update(wk91.rf4, .~exp(.)))$adj.r.squared #0.72
#linear
summary(wk91.rf6 <- update(wk91.rf4, exp(.)~exp(.)))$adj.r.squared #0.74
# Linear on cumulative CPUE seems to be best. 

#Summary and some plots
#plot(wk91.rf6, labels.id = bs_cn$year) #Looks ok

#Plot of residuals by year
ggplot(bs_cn %>% 
         filter(year != max(year)) %>% 
         mutate(res = residuals(wk91.rf6)),
       aes(x = year, y = res)) + 
  scale_x_continuous(n.breaks = length(unique(bs_cn$year)), minor_breaks = NULL) +
  geom_segment(aes(xend=year, yend=0), color="blue") +
  geom_point() +
  labs(y = "Residual value", x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Plot predictions
wk91.pred <- bs_cn %>% #Generate new dataset to cast predicitons into
  select(year, wk_91_cum_cpue) %>% 
  filter(!is.na(wk_91_cum_cpue))

#Add the model predictions
wk91.pred <- (cbind(wk91.pred, predict(wk91.rf6, wk91.pred, interval = "pred"))
              #%>% mutate(across(fit:upr, exp))
)

#Plot
wk91.p +
  #scale_x_continuous(trans = "log") +
  geom_smooth(data = wk91.pred, aes(y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk91.pred %>% filter(year == max(year)), 
                  aes(y=fit,ymin=lwr,ymax = upr), 
                  colour = "red")


# SEAK troll CWT recoveries cohort analysis --------------------------------

#Melt data for multivariate analysis
bs_cn_melt <- bs_cn %>% 
  dplyr::select(year,matches("Somass_a\\d$")|matches("SEAK_a\\d")) %>% 
  pivot_longer(-year, names_to = c(".value","age"),
               names_sep = "_") %>% 
  mutate(age = sub(".","", age)) %>% 
  mutate_at(c("age", "year"), ~factor(.)) %>% 
  filter(!age == 2 & !age == 6) %>% 
  droplevels()

# Plot linear relationships
ggplot(bs_cn_melt, aes(SEAK, Somass)) +
  facet_wrap(~age, nrow = 1, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm")

# Plot log relationships
ggplot(bs_cn_melt, aes(log(SEAK+.5), log(Somass+.5))) + #add .5 for 0 observations (log(0) doesn't exist)
  facet_wrap(~age, nrow = 1, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm")
# Looks a bit tighter, especially for 3s and 4s


#Multivariate model (*log transformed*)
try(seak_coh <- lmer(log(Somass+.5) ~ age*log(SEAK+0.5) + (1|year),
                     bs_cn_melt,
                     control=lmerControl(optCtrl=list(ftol_abs=1e-10, maxfun=1e7),
                                         optimizer="bobyqa",
                                         check.nobs.vs.nlev="ignore",
                                         check.nobs.vs.nRE="ignore"))
)

#Check coefficient and RE values
summary(seak_coh)

#Let's try plotting the predictions to see how the model fits

# Base plot with raw data
(seak.p <- ggplot(bs_cn_melt, aes(SEAK+.5,Somass+.5, colour = age)) +
    geom_point(alpha = .6,size = 2.5) +
    scale_y_continuous(trans = "log", 
                       labels = scales::number_format(accuracy = 1, big.mark = ",")) +
    scale_x_continuous(trans = "log",
                       labels = scales::number_format(accuracy = 1, big.mark = ",")) +
    labs(x = "Expanded RCH catch in SEAK", y = "Somass return")
)

#Create new dataframe for model predictions
seak_coh_pred <- with(bs_cn_melt, expand.grid(
  age = levels(age),
  year = levels(year),
  SEAK = seq.int(min(SEAK),max(SEAK), by = 10) #Expand SEAK
)) %>% 
  filter(!year == "2021") #Remove 2021 data for CI calculations below

#Cast predictions into the dataframe
seak_coh_pred <- merTools::predictInterval(seak_coh, seak_coh_pred,
                                 level = .95, type = "linear.prediction",
                                 which = "fixed") %>% 
  mutate_all(~exp(.)-.5) %>% #Reverse the response transformation
  cbind(., seak_coh_pred) %>% 
  group_by(SEAK, age) %>% 
  summarize(fit = mean(fit), upr = mean(upr), lwr = mean(lwr))


#Make a small table with predictions for 2021 returns
pred_2021 <- merTools::predictInterval(seak_coh, bs_cn_melt %>% filter(year == "2021"),
                             level = .95, type = "linear.prediction",
                             which = "fixed") %>% 
  mutate_all(~exp(.)-.5) %>% 
  cbind(., bs_cn_melt %>% filter(year == "2021"))

#Add timeseries and 2021 predictions to plot of raw data
seak.p +
  geom_smooth(data = seak_coh_pred, aes(y = fit), method = "lm", se = F) +
  geom_ribbon(data = seak_coh_pred, 
              aes(y = fit, ymin = lwr, ymax = upr, fill = age),
              colour = NA, alpha = .2) +
  geom_pointrange(data = pred_2021, aes(y = fit, ymin = lwr, ymax = upr, shape = age),
                  colour = "black")

# The SEAK model is predicting a final return of 63k adults. 
pred_2021 %>% 
  summarise(across(fit:lwr, sum))

#No guarantee this R^2 is accurate since the three regressions should be considered separately
r.squaredGLMM(seak_coh)

#The individual relationships
summary(lm(log(Somass+0.5)~log(SEAK+0.5), data = bs_cn_melt %>% filter(age == 3)))$r.squared #0.70
summary(lm(log(Somass+0.5)~log(SEAK+0.5), data = bs_cn_melt %>% filter(age == 4)))$r.squared #0.72
summary(lm(log(Somass+0.5)~log(SEAK+0.5), data = bs_cn_melt %>% filter(age == 5)))$r.squared #0.84

# Combined wk 83 & SEAK CWT recovery multiple regression ------------------

# Step 1: three predictors are available from rec CPUE:
# - wk83 (19-26 August 2020) CPUE
# - wks 83 & 83 (16-26 August 2020) average CPUE
# - wk83 cumulative CPUE
# Need to pick which of the above will inform the re-forecast

# Make melted df for plotting these relationships
bs_cn_wk83.melt <- bs_cn %>%  
  melt(measure.vars = c("wk_83_cpue",
                        "wks_83.84_cpue",
                        "wk_83_cum_cpue"))

#Plot
ggplot(bs_cn_wk83.melt, aes(value, Somass_term_adult_return, group = variable)) +
  facet_wrap(~variable, ncol = 1) +
  #geom_point() +
  geom_label(aes(label=year),hjust=0, vjust=0, alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", lty = 2) +
  stat_poly_eq(formula = log(y)~x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE) + 
  scale_y_continuous(trans = "log", 
                     labels = scales::number_format(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(limits = c(0,1.8))


#Linear model
wk83_rf <- lm(Somass_term_adult_return ~ wk_83_cum_cpue + SEAK_adults, bs_cn)
summary(wk83_rf)$adj.r.squared #.64

#Model summary
plot(wk83_rf, labels.id = bs_cn$year) #Behaving pretty well

#Does log transformation improve the fit?
wk83_rf2 <- update(wk83_rf, log(.) ~ log(wk_83_cum_cpue) + log(SEAK_adults))
summary(wk83_rf2)$adj.r.squared #.63; slightly weaker--don't use

# Maybe log-linear?
wk83_rf3 <- update(wk83_rf, log(.) ~ .)
summary(wk83_rf3)$adj.r.squared #.63; slightly weaker--don't use


#Plot of residuals by year
ggplot(bs_cn %>% 
         filter(!(year == max(year))) %>% 
         mutate(res = residuals(wk83_rf2)),
       aes(x = year, y = res)) + 
  scale_x_continuous(n.breaks = length(unique(bs_cn$year)-1), minor_breaks = NULL) +
  geom_segment(aes(xend=year, yend=0), color="blue") +
  geom_point() +
  labs(y = "Residual value", x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Save cpue val for 2022 (need to filter by it a few times below)
wk83cpue_2022 <- subset(bs_cn, year == max(year))$wk_83_cum_cpue[1]

#New dataframe to house predictions
wk83.mr.pred <- with(bs_cn, expand.grid(
  SEAK_adults = mean(bs_cn$SEAK_adults),
  wk_83_cum_cpue = seq(min(wk_83_cum_cpue), 
                       max(wk_83_cum_cpue), 
                       length.out = 100))) %>% 
  add_row(SEAK_adults = (bs_cn %>% filter(year == max(year)) %>% select(SEAK_adults))[1,1],
          wk_83_cum_cpue = wk83cpue_2022) %>% 
  cbind(., predict(wk83_rf2, ., interval = "pred", level = .75)) %>% 
  mutate(across(fit:upr, exp))


#Plot
ggplot(bs_cn, aes(wk_83_cum_cpue, Somass_term_adult_return)) +
  #geom_point() +
  geom_label(aes(label=year),hjust=0, vjust=0, alpha = 0.25) +
  labs(y = "Somass return", x = "Week 83 cumulative CPUE") +
  geom_smooth(data = wk83.mr.pred %>% filter(!(wk_83_cum_cpue == wk83cpue_2022)), 
              aes(x = wk_83_cum_cpue,
                  y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk83.mr.pred %>% filter(wk_83_cum_cpue == wk83cpue_2022), 
                  aes(x = wk_83_cum_cpue,y=fit,ymin=lwr,ymax = upr),
                  colour = "red") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ",")
                     #,trans = "log"
  )

wk83.mr.pred %>% 
  filter(wk_83_cum_cpue == wk83cpue_2022)



# Combined wk 84 & SEAK CWT recovery multiple regression ------------------

# Step 1: three predictors are available from rec CPUE:
# - wk84 (19-26 August 2020) CPUE
# - wks 83 & 84 (16-26 August 2020) average CPUE
# - wk84 cumulative CPUE
# Need to pick which of the above will inform the re-forecast

# Make melted df for plotting these relationships
bs_cn_wk84.melt <- bs_cn %>%  
  melt(measure.vars = c("wk_83_cpue",
                        "wk_84_cpue",
                        "wks_83.84_cpue",
                        "wk_84_cum_cpue"))

#Plot
ggplot(bs_cn_wk84.melt, aes(value, Somass_term_adult_return, group = variable)) +
  facet_wrap(~variable, ncol = 1) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "red", lty = 2) +
  stat_poly_eq(formula = log(y)~x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE) + 
  scale_y_continuous(trans = "log", 
                     labels = scales::number_format(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(limits = c(0,1.8))


#Linear model
wk84_rf <- lm(Somass_term_adult_return ~ wk_84_cum_cpue + SEAK_adults, bs_cn)
summary(wk84_rf)$adj.r.squared #.71

#Model summary
#plot(wk84_rf, labels.id = bs_cn$year) #Behaving pretty well

#Does log transformation improve the fit?
wk84_rf2 <- update(wk84_rf, log(.) ~ log(wk_84_cum_cpue) + log(SEAK_adults))
summary(wk84_rf2)$adj.r.squared #Slightly weaker--don't use

# Log-linear?
wk84_rf3 <- update(wk84_rf, log(.) ~ wk_84_cum_cpue + SEAK_adults)
summary(wk84_rf3)$adj.r.squared #Slightly weaker--don't use


#Plot of residuals by year
ggplot(bs_cn %>% 
         filter(year != max(year)) %>% 
         mutate(res = residuals(wk84_rf)),
       aes(x = year, y = res)) + 
  scale_x_continuous(n.breaks = length(unique(bs_cn$year)), minor_breaks = NULL) +
  geom_segment(aes(xend=year, yend=0), color="blue") +
  geom_point() +
  labs(y = "Residual value", x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot predictions

# Save cpue val for 2022 (need to filter by it a few times below)
wk84cpue_2022 <- subset(bs_cn, year == max(year))$wk_84_cum_cpue[1]

#New dataframe to house predictions
wk84.mr.pred <- with(bs_cn, expand.grid(
  SEAK_adults = mean(bs_cn$SEAK_adults),
  wk_84_cum_cpue = seq(min(wk_84_cum_cpue), 
                       max(wk_84_cum_cpue), 
                       length.out = 100))) %>% 
  add_row(SEAK_adults = (bs_cn %>% filter(year == max(year)) %>% select(SEAK_adults))[1,1],
          wk_84_cum_cpue = wk84cpue_2022) %>% 
  cbind(., predict(wk84_rf, ., interval = "pred", level = .95))


#Plot
ggplot(bs_cn, aes(wk_84_cum_cpue, Somass_term_adult_return)) +
  #geom_point() +
  geom_label(aes(label=year),hjust=0, vjust=0, alpha = 0.25) +
  labs(y = "Somass return", x = "Week 84 cumulative CPUE") +
  geom_smooth(data = wk84.mr.pred %>% filter(!(wk_84_cum_cpue == wk84cpue_2022)), 
              aes(x = wk_84_cum_cpue,
                  y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk84.mr.pred %>% filter(wk_84_cum_cpue == wk84cpue_2022), 
                  aes(x = wk_84_cum_cpue,y=fit,ymin=lwr,ymax = upr),
                  colour = "red") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ",")
                     #,trans = "log"
  )

# List forecast prediction
wk84.mr.pred %>% 
  filter(wk_84_cum_cpue == wk84cpue_2022)


# Combined wk 91 & SEAK CWT recovery multiple regression ------------------

# Step 1: three predictors are available from rec CPUE:
# - wk91 (19-26 August 2020) CPUE
# - wks 83 & 91 (16-26 August 2020) average CPUE
# - wk91 cumulative CPUE
# Need to pick which of the above will inform the re-forecast

# Make melted df for plotting these relationships
bs_cn_wk91.melt <- bs_cn %>%  
  melt(measure.vars = c("wk_84_cpue",
                        "wk_91_cpue",
                        "wks_84.91_cpue",
                        "wk_91_cum_cpue"))

#Plot
ggplot(bs_cn_wk91.melt, aes(value, Somass_term_adult_return, group = variable)) +
  facet_wrap(~variable, ncol = 1) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "red", lty = 2) +
  stat_poly_eq(formula = log(y)~x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE) + 
  scale_y_continuous(trans = "log", 
                     labels = scales::number_format(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(limits = c(0,1.8))

#The week 83 CPUE performs best.

#Linear model
wk91_rf <- lm(Somass_term_adult_return ~ wk_91_cum_cpue + SEAK_adults, bs_cn)
summary(wk91_rf)$adj.r.squared #.74

#Model summary
#plot(wk91_rf, labels.id = bs_cn$year) #Behaving pretty well

#Does log transformation improve the fit?
wk91_rf2 <- update(wk91_rf, log(.) ~ log(wk_91_cum_cpue) + log(SEAK_adults))
summary(wk91_rf2)$adj.r.squared #Slightly weaker--don't use

#Plot of residuals by year
ggplot(bs_cn %>% 
         filter(year != max(year)) %>% 
         mutate(res = residuals(wk91_rf)),
       aes(x = year, y = res)) + 
  scale_x_continuous(n.breaks = length(unique(bs_cn$year)), minor_breaks = NULL) +
  geom_segment(aes(xend=year, yend=0), color="blue") +
  geom_point() +
  labs(y = "Residual value", x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot predictions

#New dataframe to house predictions
wk91.mr.pred <- with(bs_cn, expand.grid(
  SEAK_adults = (bs_cn %>% filter(year == max(year)) %>% select(SEAK_adults))[1,1],
  wk_91_cum_cpue = seq(min(wk_91_cum_cpue), 
                       max(wk_91_cum_cpue), 
                       length.out = 100)
))

# Cast predictions on data
wk91.mr.pred <- cbind(wk91.mr.pred, predict(wk91_rf, wk91.mr.pred, interval = "pred"))

# Prediction for 2021****************
pred.mr.2021 <- bs_cn %>% 
  filter(year == max(year)) %>%
  select(year,wk_91_cum_cpue,SEAK_adults) %>% 
  mutate(predictions = predict(wk91_rf,., interval = "pred") %>% as_tibble()) %>% 
  reduce(predictions)
#************Not working yet

#Plot
ggplot(bs_cn, aes(wk_91_cum_cpue, Somass_term_adult_return)) +
  geom_point() +
  labs(y = "Somass return", x = "Week 91 cumulative CPUE") +
  geom_smooth(data = wk91.mr.pred, aes(x = wk_91_cum_cpue,
                                       y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk91.mr.pred %>% filter(wk_91_cum_cpue == max(wk_91_cum_cpue)), 
                  aes(x = wk_91_cum_cpue,y=fit,ymin=lwr,ymax = upr),
                  colour = "red") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ",")
                     #,trans = "log"
  )




# Retro analysis of forecast performance ----------------------------------

# Save dataframe of years trimmed back to 2012
retro_df <- map_df(seq.int(min(bs_cn$year)+10,max(bs_cn$year)),
                   ~ bs_cn %>% filter(year<=.x), .id = "fcst_yr") %>% 
  group_by(fcst_yr) %>% 
  mutate(fcst_yr = max(year)) %>% 
  ungroup() %>% 
  mutate(Somass_term_adult_return = if_else(year == fcst_yr,NA_integer_,Somass_term_adult_return))

#Split up the years to their own dataframes
retro.dfs <- retro_df %>% split(.$fcst_yr)

# How does r squared change over the years?
retro.dfs %>% 
  map(~update(wk91.rf6,data = .)) %>% 
  map_dfr(~summary(.)$r.squared)

# Predictions for each year 2012-2021 from linear regression
wk91.retro <- retro.dfs %>% 
  map(~update(wk91.rf6, data = .)) %>% 
  map2(.,retro.dfs, predict) %>% 
  map_dfr(~tail(.,n=1) %>% as.numeric(), .id = "year") %>% 
  pivot_longer(cols = everything(),
               names_to = "year",
               values_to = "prediction") %>% 
  mutate(across(everything(),as.numeric))


# PLot
ggplot(wk91.retro, aes(as.numeric(year), as.numeric(prediction))) +
  geom_point(colour = "red", size = 1.5) +
  geom_line(colour = "red") +
  geom_point(data = bs_cn, aes(y = Somass_term_adult_return)) +
  geom_line(data = bs_cn, aes(y = Somass_term_adult_return)) +
  labs(y = "Somass adult Chinook return", x = NULL) +
  scale_y_continuous(breaks = seq(0,200000, by = 50000), labels = scales::comma) +
  scale_x_continuous(breaks = seq(2002,2021, by = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Week 91 CPUE linear regression")

# Predictions for each year 2012-2021 from multiple regression
mr.retro <- retro.dfs %>% 
  map(~update(wk91_rf, data = .)) %>% 
  map2(.,retro.dfs, predict) %>% 
  map_dfr(~tail(.,n=1) %>% as.numeric(), .id = "year") %>% 
  pivot_longer(cols = everything(),
               names_to = "year",
               values_to = "prediction") %>% 
  mutate(across(everything(),as.numeric))


#Plot
ggplot(mr.retro, aes(as.numeric(year), as.numeric(prediction))) +
  geom_point(colour = "red", size = 1.5) +
  geom_line(colour = "red") +
  geom_point(data = bs_cn, aes(y = Somass_term_adult_return)) +
  geom_line(data = bs_cn, aes(y = Somass_term_adult_return)) +
  labs(y = "Somass adult Chinook return", x = NULL) +
  scale_y_continuous(breaks = seq(0,200000, by = 50000), labels = scales::comma) +
  scale_x_continuous(breaks = seq(2002,2021, by = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Week 91 CPUE & SEAK recoveries multiple regression")


# MAPEs
map(list(wk91.retro,mr.retro), 
    ~left_join(.x, bs_cn %>% select(year, Somass_term_adult_return)) %>% 
      drop_na %>% 
      mutate(diff = abs(Somass_term_adult_return - prediction),
             APE = diff/Somass_term_adult_return) %>% 
      summarize(MAPE = mean(APE), RMSE = sqrt(sum(diff^2)/n())),
    .id = "model")

# Concision improvement for future: 
# use pmap/pmap2 rather than map/map2 to fit multiple models simultaneously 
# Will result in 3D lists that take some care to unpack appropriately. 
# Can do the full process in a single pipeline.

         
# Forecast performances for post-season review ---------------------------

# Need to line up the 5 forecast models showing:
# - r^2
# - predicted final run size (95% PI)
# - cpue value used for the prediction

rf_rev <- as_tibble(rbind(
  wk82.pred %>% filter(year == 2020) %>% select(fit:upr),
  wk83.pred %>% filter(year == 2020) %>% select(fit:upr),
  wk84.pred %>% filter(year == 2020) %>% select(fit:upr),
  wk83.mr.pred %>% filter(fit == max(fit)) %>% select(fit:upr),
  wk84.mr.pred %>% filter(fit == max(fit)) %>% select(fit:upr)
)) %>% 
  cbind(., c(
    summary(wk82.rf)$adj.r.squared,
    summary(wk83.rf2)$adj.r.squared,
    summary(wk84.rf6)$adj.r.squared,
    summary(rf)$adj.r.squared,
    summary(wk84_rf)$adj.r.squared
  )
) %>% 
  rename(r2 = 4) %>% 
  mutate(round(across(fit:upr), digits = -2))

#CPUE values
bs_cn %>% 
  filter(year == 2020) %>% 
  select(wk_82_cpue,wk_83_cpue,wk_84_cum_cpue,SEAK_adults) %>% 
  pivot_longer(everything())

