#updated from Nick's 2023 code and keeping it here just in case
#not used for 2026
# Load packages and functions ------------------------------------------------------

library(tidyverse); theme_set(theme_bw(base_size = 16))
library(ggrepel)
library(readxl)
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

curr_yr <- 2026

# Load predictor data ----------------------------------------------------

bs_cn <- read_xlsx("CN_return_predictors_assemblyMaster.xlsx",
                   sheet = "CN_return_predictors") %>% 
  filter(!year <= 2001) |> #Exclude 2001 data due to inseason management changes
  rowwise() |>
mutate(Somass_term_adult_return = as.numeric(Somass_term_adult_return))




# Holistic approach -------------------------------------------------------

# Load interview data
cpue <- read_xlsx("CN_return_predictors_assemblyMaster.xlsx",
                  sheet = "CREEL Interview Summary 2026") |> 
  rename_with(tolower) |> 
  filter(asscd_txt %in% c("Adipose fin not chkd", "Complete Form", "Fish not seen for ID"), # Interviews w/these comments used to calculate CPUE
         #!statsub %in% c("23L", "23F"), # Very little data is associated with these areas
         str_detect(statsub, "^23[[:upper:]]")) |> # Remove offshore areas
  mutate(statsub = case_when( # Combine subareas that were split in 2011
    statsub %in% c("23G", "23P", "23O") ~ "23O+P", # in 2011, 23G was split into 23O and 23P
    statsub %in% c("23H", "23Q", "23N") ~ "23Q+N", # in 2011, 23H was split into 23Q and 23N
    TRUE ~ statsub)) |>  
  group_by(year, month, statsub, sw_2026, prop_rch) |> 
  summarise(cn_all_k = sum(cn_all_k),
            boat_trips = n()) |> 
  left_join(select(bs_cn, year, Somass_term_adult_return)) |> # Add column with adult return data
  rename("return" = "Somass_term_adult_return") |> 
  mutate(return = as.numeric(return)) |>
  filter(sw_2026 %in% c(82, 83, 84, 91, 92)) |> # Keep only the mid-Aug to mid-Sept stat weeks
  pivot_longer(cols = cn_all_k:boat_trips) |> 
  pivot_wider(names_from = sw_2026,
              values_from = value) |> 
  rowwise() |> 
  # Sum kept chinook and interviews across stat week periods
  mutate(cum83 = `82` + `83`,
         cum84 = sum(c_across(`82`:`84`), na.rm = TRUE),
         cum91 = sum(c_across(`83`:`91`), na.rm = TRUE),
         cum92 = sum(c_across(`83`:`92`), na.rm = TRUE),
         `8384` = `83` + `84`,
         `8491` = `84` + `91`) |> 
  ungroup() |> 
  pivot_longer(cols = matches("[[:digit:]]{2,}"),
               names_to = "period",
               values_to = "value") |> 
  pivot_wider(names_from = name,
              values_from = value) |> 
  # Calculate CPUE and RCH cpue
  mutate(cpue = cn_all_k / boat_trips,
         rch_cpue = cpue * prop_rch)

# Minimalist version of the data
cpue_minimal <- cpue |> 
  filter(!if_any(c(return, cn_all_k, boat_trips), is.na)) |> 
  select(year, period, cn_all_k, boat_trips, prop_rch, statsub, return)


# Make list of all possible subarea combinations
combo_n <- seq.int(4,5) # set list of #s of subarea combos to try 

subarea_combos <- list(unique(cpue$statsub)) |> 
  rep(length(combo_n)) |> 
  map2(combo_n, ~ combn(x = .x, m = .y, FUN = list),
       .id = "list_n") |> 
  unlist(recursive = FALSE)


# Save list of dataframes with combined CPUE calculated for each grouping of subareas, then fit LMs
tmp <- set_names(subarea_combos, 
                 nm = map(subarea_combos, ~paste(.x, collapse = "_"))) |> 
  map(~ filter(cpue_minimal, statsub %in% .x) |> 
        group_by(year, return, period, prop_rch) |> 
        summarize(across(cn_all_k:boat_trips, ~sum(.x, na.rm = TRUE))) |> 
        ungroup() |> 
        mutate(cpue = cn_all_k / boat_trips,
               rch_cpue = cpue * prop_rch) |> 
        filter(!(is.na(cpue) | is.nan(cpue))),
      .progress = TRUE)
#Takes a very long time

# Fit all possible predictors with lm and save
cpue_lms <- cpue |> 
  filter(!(is.na(cpue) | is.nan(cpue))) |> 
  mutate(ln_cpue = log(cpue + 0.0001),
         ln_rch_cpue = log(rch_cpue + 0.0001),
         ln_return = log(return)) |> 
  pivot_longer(cols = contains("cpue"),
               names_to = "predictor",
               values_to = "x") |> 
  pivot_longer(cols = contains("return"),
               names_to = "response",
               values_to = "y") |> 
  mutate(group = paste(statsub, period, predictor, response, sep = "-")) |> 
  group_by(group) |> 
  filter(!(all(is.na(x)) | all(is.na(y)))) %>% # Remove all groups where no cpue data exist
  split(.$group) |> 
  map(~lm(y ~ x, data = .x))


# Extract R2 from all models
cpue_r2s <- cpue_lms |> 
  map_df(broom::glance,
         .id = "object") |> 
  filter(df.residual > 4,
         !(is.na(p.value) | is.nan(p.value))) |> 
  separate(object, 
           into = c("subarea", "period", "predictor", "response"),
           sep = "-") |> 
  select(subarea:adj.r.squared, p.value, AIC) |> 
  mutate(model = case_when(
    str_detect(predictor, "ln") & str_detect(response, "ln") ~ "log-log",
    str_detect(predictor, "ln") & !str_detect(response, "ln") ~ "log-lin",
    !str_detect(predictor, "ln") & str_detect(response, "ln") ~ "lin-log",
    TRUE ~ "linear"
  ) |> 
    fct_relevel(c("lin-log", "linear", "log-lin", "log-log"), after = 0),
  correction = if_else(str_detect(predictor, "rch"), "corrected", "raw"),
  period = fct_reorder(period, adj.r.squared),
  subarea = fct_reorder(subarea, adj.r.squared)) 


# Which options yield the best R2 values, on average?
purrr::set_names(c("subarea", "period", "model", "correction")) |> 
  map(~ cpue_r2s |> 
        select(.data[[.x]], adj.r.squared) |> 
        ggplot(aes(x = adj.r.squared)) +
        facet_wrap(~.data[[.x]]) +
        geom_density(aes(fill = .data[[.x]])) +
        guides(fill = "none")) # R^2 are skewed, so summarize by medians


# Average r2 values across all predictor levels and arrange largest to smallest
purrr::set_names(names(select(cpue_r2s, !predictor:adj.r.squared))) |> 
  map(~ cpue_r2s |> 
        group_by(.data[[.x]]) |> 
        summarize(med_r2 = median(adj.r.squared, na.rm = TRUE)) |> 
        arrange(desc(med_r2)))


# Plot r2 values
cpue_r2s |> 
  ggplot(aes(period, subarea)) +
  facet_grid(correction ~ model, space = "free_y", scales = "free_y") +
  geom_tile(aes(fill = adj.r.squared)) +
  scale_fill_viridis_c(option = "H", limits = c(0, 1)) +
  coord_cartesian(expand = FALSE)


# Get background map for plotting CREEL subareas
library(sf)

# Load Shapefile with CREEL subareas
creel_shp <- sf::st_read("./Creel_Survey_Areas") |> 
  filter(STATAREA == 23) |> # Keep only PFMA 23
  right_join(cpue_r2s, by = c("subareaid" = "subarea")) |> 
  mutate(period = factor(period, levels = c("82", "83", "cum83", "84", "8384", "cum84", "91", "8491", "cum91", "92", "cum92")))


# Plot
creel_shp %>%
  split(.$period) |> 
  imap(~ ggplot(.x) +
        facet_grid(correction ~ model) +
        geom_sf(aes(fill = adj.r.squared),
                colour = "black") +
        geom_sf_text(aes(label = subareaid), colour = "red") +
        coord_sf(crs = "EPSG:3857") +
        scale_fill_viridis_c(option = "H", limits = c(0, 1)) +
        ggtitle(.y)
  )


# Save basemap
library(ggmap)
ggmap::register_stadiamaps(key = "5e09d544-f654-4e99-b369-fa67ea4f60db")
Barkley <- get_stadiamap(bbox = c(left = -125.587, bottom = 48.753, right = -124.708, top = 49.145), # Bounding box
                         maptype = "stamen_terrain", 
                         zoom = 11)
ggmap::ggmap(Barkley)


# Week 82 CPUE model ----------------------------------------------

#Plot the relationship
(wk82.p <- ggplot(bs_cn, aes(wk_82_cpue, Somass_term_adult_return)) +
   geom_point() +
   geom_text_repel(aes(label=year)) +
   #coord_cartesian(ylim = c(0,150000)) +
   scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
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
  geom_pointrange(data = wk82.pred %>% filter(year == curr_yr), 
                  aes(y=fit,ymin=lwr,ymax = upr), 
                  colour = "red") +
  scale_y_continuous(labels = scales::comma)
#High uncertainty


# Week 83 CPUE model ----------------------------------------------


#Plot the relationship
(wk83.p <- ggplot(bs_cn, aes(wk_83_cpue, Somass_term_adult_return)) +
   geom_point() +
   geom_text_repel(aes(label=year), 
                   min.segment.length = 0) +
   #scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
   #coord_cartesian(ylim = c(0,150000)) +
   labs(y = "Somass adult return", x = "Week 83 CPUE")
)

wk83.p +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log", labels = scales::comma)
#Unclear whether log transformation is helping

# What percentile of the observed range does the 2023 value correspond to?
bs_cn |> 
  mutate(percent_rank = rank(wk_83_cpue)/length(wk_83_cpue)) |> 
  select(year, percent_rank, wk_83_cpue) |> 
  filter(year == curr_yr)


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
plot(wk83.rf, labels.id = bs_cn$year, ask = F) #Some outliership in 2009, 2014, 2022; acceptable leverage
# 2009 remains the biggest outlier, but is not skewing the model enough to justify its removal

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
  geom_pointrange(data = wk83.pred %>% filter(year == curr_yr), 
                  aes(y=fit,ymin=lwr,ymax = upr), 
                  colour = "red") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0,300000, by = 50000)) +
  annotate("text", x = 1, y = 200000, label = "italic(R)^2 == 0.62", parse = TRUE)


# Week 84 cum CPUE model ------------------------------------------------------

#Plot the linear relationship
(wk84.p <- ggplot((bs_cn 
                   #%>% melt(measure.vars = c("wk_84_cpue", "wk_84_cum_cpue"))
), 
aes(#value, 
  wk_84_cum_cpue,
  Somass_term_adult_return)) +
  #facet_grid(~variable) +
  geom_point() +
  geom_text_repel(aes(label=year)) +
  #scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
  #coord_cartesian(ylim = c(0,150000)) +
  labs(y = "Somass return", x = "Week 84 cumulative CPUE")
)

# What percentile of the observed range does the 2023 value correspond to?
bs_cn |> 
  mutate(percent_rank = rank(wk_84_cum_cpue)/length(wk_84_cum_cpue)) |> 
  select(year, percent_rank, wk_84_cum_cpue) |> 
  filter(year == curr_yr)


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
summary(wk84.rf4 <- update(wk84.rf, .~log(wk_84_cum_cpue)))$adj.r.squared #0.61
#log-linear
summary(wk84.rf5 <- update(wk84.rf4, .~exp(.)))$adj.r.squared #0.58
#linear
summary(wk84.rf6 <- update(wk84.rf4, exp(.)~exp(.)))$adj.r.squared #0.56
# log-log on cumulative CPUE seems to be working best. 

#Summary and some plots
plot(wk84.rf4, labels.id = bs_cn$year, ask = FALSE) #Looks ok

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
wk84.pred <- (cbind(wk84.pred, predict(wk84.rf4, wk84.pred, interval = "pred", level = .75))
              %>% mutate(across(fit:upr, exp))
)

#Plot
wk84.p +
  #scale_x_continuous(trans = "log") +
  geom_smooth(data = wk84.pred, aes(y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk84.pred %>% filter(year == max(year)), 
                  aes(y=fit,ymin=lwr,ymax = upr), 
                  colour = "red")+
  annotate("text", x = 1, y = 200000, label = "italic(R)^2 == 0.61", parse = TRUE)

# Week 91 cum CPUE model ------------------------------------------------------

#Plot the linear relationship
(wk91.p <- ggplot((bs_cn 
                   %>% melt(measure.vars = c("wk_91_cpue", "wk_91_cum_cpue"))
), 
aes(value, 
  #wk_91_cum_cpue,
  Somass_term_adult_return)) +
  geom_point() +
  geom_text_repel(aes(label=year)) +
  facet_grid(~variable) +
  #geom_point() +
  scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
  #coord_cartesian(ylim = c(0,150000)) +
  labs(y = "Somass return", x = "Week 91 cumulative CPUE")
)


# What percentile of the observed range does the 2023 value correspond to?
bs_cn |> 
  mutate(percent_rank = rank(wk_91_cum_cpue)/length(wk_91_cum_cpue)) |> 
  select(year, percent_rank, wk_91_cum_cpue) |> 
  filter(year == curr_yr)


# Show log-linear relationship
wk91.p +
  #scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log", labels = scales::comma) +
  geom_smooth(method = "lm") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) 
  #Unclear whether log transformation will help

#Log-log regression model on week 91 CPUE
wk91.rf <- lm(log(Somass_term_adult_return) ~ log(wk_91_cpue), data = bs_cn)
summary(wk91.rf)$adj.r.squared #0.20
#log-linear
summary(wk91.rf2 <- update(wk91.rf, .~exp(.)))$adj.r.squared #0.14
#linear
summary(wk91.rf3 <- update(wk91.rf, exp(.)~exp(.)))$adj.r.squared #0.09

#Log-log regression model on cumulative CPUE to week 91 (includes weeks 82 & 83)
summary(wk91.rf4 <- update(wk91.rf, .~log(wk_91_cum_cpue)))$adj.r.squared #0.58
#log-linear
summary(wk91.rf5 <- update(wk91.rf4, .~exp(.)))$adj.r.squared #0.57
#linear
summary(wk91.rf6 <- update(wk91.rf4, exp(.)~exp(.)))$adj.r.squared #0.50
# log-log on cumulative CPUE seems to be best. 

#Summary and some plots
plot(wk91.rf4, labels.id = bs_cn$year, ask = FALSE) #Looks ok

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
wk91.pred <- (cbind(wk91.pred, predict(wk91.rf4, wk91.pred, interval = "pred", level = .75))
              %>% mutate(across(fit:upr, exp))
)

#Plot
ggplot(bs_cn, aes(wk_91_cum_cpue,
                  Somass_term_adult_return)) +
  geom_point() +
  geom_text_repel(aes(label=year)) +
  scale_y_continuous(breaks = c(0,30000,60000,90000,120000,150000)) +
  #coord_cartesian(ylim = c(0,150000)) +
  labs(y = "Somass return", x = "Week 91 cumulative CPUE") +
  #scale_x_continuous(trans = "log") +
  geom_smooth(data = wk91.pred, aes(y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk91.pred %>% filter(year == max(year)), 
                  aes(y=fit,ymin=lwr,ymax = upr), 
                  colour = "red") +
  annotate("text", x = 1, y = 200000, label = "italic(R)^2 == 0.58", parse = TRUE)



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
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, na.rm = TRUE) + 
  scale_y_continuous(trans = "log", 
                     labels = scales::number_format(accuracy = 1, big.mark = ",")) +
  scale_x_continuous(limits = c(0,1.8))


#Linear model
wk91_rf <- lm(Somass_term_adult_return ~ wk_91_cum_cpue + SEAK_adults, bs_cn)
summary(wk91_rf)$adj.r.squared #.74

#Model summary
plot(wk91_rf, labels.id = bs_cn$year, ask = FALSE) #Behaving pretty well

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

# Save cpue val for 2022 (need to filter by it a few times below)
wk91cpue_2022 <- subset(bs_cn, year == max(year))$wk_91_cum_cpue[1]

#New dataframe to house predictions
wk91.mr.pred <- with(bs_cn, expand.grid(
  SEAK_adults = mean(bs_cn$SEAK_adults),
  wk_91_cum_cpue = seq(min(wk_91_cum_cpue), 
                       max(wk_91_cum_cpue), 
                       length.out = 100))) %>% 
  add_row(SEAK_adults = (bs_cn %>% filter(year == max(year)) %>% select(SEAK_adults))[1,1],
          wk_91_cum_cpue = wk91cpue_2022) %>% 
  cbind(., predict(wk91_rf, ., interval = "pred", level = .95))


#Plot
ggplot(bs_cn, aes(wk_91_cum_cpue, Somass_term_adult_return)) +
  #geom_point() +
  geom_label(aes(label=year),hjust=0, vjust=0, alpha = 0.25) +
  labs(y = "Somass return", x = "Week 91 cumulative CPUE") +
  geom_smooth(data = wk91.mr.pred %>% filter(!(wk_91_cum_cpue == wk91cpue_2022)), 
              aes(x = wk_91_cum_cpue,
                  y = fit, ymin = lwr, ymax = upr),
              stat = "identity") +
  geom_pointrange(data = wk91.mr.pred %>% filter(wk_91_cum_cpue == wk91cpue_2022), 
                  aes(x = wk_91_cum_cpue,y=fit,ymin=lwr,ymax = upr),
                  colour = "red") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ",")
                     #,trans = "log"
  )

# List forecast prediction
wk91.mr.pred %>% 
  filter(wk_91_cum_cpue == wk91cpue_2022)




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
  scale_x_continuous(breaks = seq(2002,curr_yr, by = 1)) +
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

