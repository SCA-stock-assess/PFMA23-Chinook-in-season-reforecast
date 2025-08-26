# Packages ----------------------------------------------------------------


pkgs <- c("tidyverse", "here", "readxl", "janitor", "ggpmisc")
#install.packages(pkgs)


library(tidyverse); theme_set(theme_bw(base_size = 14))
library(here)
library(readxl)
library(janitor)
library(ggpmisc)
library(ggrepel)
library(MLmetrics)


curr_year <- 2025


# Load data ---------------------------------------------------------------


# Somass terminal adult return data
bs_cn <- read_xlsx(
  here("01-data_CN_return_predictors_2025_NEW.xlsx"),
  sheet = "CN_return_predictors"
) |> 
  select(year, matches("(?i)Somass_term_"))


# CPUEs from creel data
cpue <- read_excel(
  here("01-data_CN_return_predictors_2025_NEW.xlsx"), 
  sheet = "CREEL Interview Summary"
) |>   
  clean_names() |> 
  filter(
    # Interviews w/these comments are used to calculate CPUE
    asscd_txt %in% c("Adipose fin not chkd", "Complete Form", "Fish not seen for ID"), 
    str_detect(statsub, "^23[[:upper:]]|123(?=(T|R|P))") # 123 T, R, and P are the corridor
  ) |> # Remove offshore areas
  # Combine subareas that were split over the years
  mutate(
    statsub = case_when( 
      # in 2011, 23G was split into 23O and 23P (now 123P)
      statsub %in% c("23G", "23P", "23O", "123P", "23L") ~ "23O+123P", 
      # in 2011, 23H was split into 23Q and 23N (now 123T)
      statsub %in% c("23H", "23Q", "23N", "123T") ~ "23Q+123T", 
      statsub == "23I" ~ "123R", # Changed in 2022
      TRUE ~ statsub
    )
  ) |>  
  summarise(
    .by = c(year, statsub, sw),
    across(c(cn_all_k, rch_cn_k), sum, na.rm = TRUE),
    boat_trips = n()
  ) |> 
  # Add column with adult return data
  left_join(select(.data = bs_cn, year, Somass_term_adult_return)) |> 
  rename("return" = "Somass_term_adult_return") |> 
  # Keep only the mid-Aug to mid-Sept stat weeks
  # filter(sw %in% c(82, 83, 84, 91, 92)) |> 
  pivot_longer(cols = cn_all_k:boat_trips) |> 
  pivot_wider(
    names_from = sw,
    values_from = value
  ) |> 
  rowwise() |> 
  # Sum kept chinook and interviews across stat week periods
  mutate(
    cum7183 = sum(c_across(`71`:`83`), na.rm = TRUE),
    cum7184 = sum(c_across(`71`:`84`), na.rm = TRUE),
    `8183` = sum(c_across(`81`:`83`), na.rm = TRUE),
    `8184` = sum(c_across(`81`:`84`), na.rm = TRUE),
    `8283` = sum(c_across(`82`:`83`), na.rm = TRUE),
    `8284` = sum(c_across(`82`:`84`), na.rm = TRUE),
    cum91 = sum(c_across(`83`:`91`), na.rm = TRUE),
    cum92 = sum(c_across(`83`:`92`), na.rm = TRUE),
    `8384` = sum(c_across(`83`:`84`), na.rm = TRUE),
    `8491` = sum(c_across(`84`:`91`), na.rm = TRUE),
  ) |> 
  ungroup() |> 
  pivot_longer(
    cols = matches("[[:digit:]]{2,}"),
    names_to = "period",
    values_to = "value"
  ) |> 
  pivot_wider(
    names_from = name,
    values_from = value
  )

#check that we have all the period and stat areas:
unique(cpue$period)
unique(cpue$statsub)

# Week 83 model for 2024 and/or earlier-----------------------------------------------------------


# Exploratory analysis (q.v. here("plots")) tells us the best model for 
# Week 83 is the rch loglog from subareas 23J, 23C, and 23E


# Subset to data for wk83 relationship
wk83_data <- cpue |> 
  filter(
    period == "83",
    statsub %in% c("23J", "23C", "23E"),
    !if_any(c(cn_all_k, boat_trips), is.na)
  ) |> 
  summarize(
    .by = c(year, return),
    across(cn_all_k:boat_trips, sum)
  ) |> 
  mutate(
    ttl_cpue = cn_all_k/boat_trips,
    rch_cpue = rch_cn_k/boat_trips
  )|> 
  filter(rch_cpue > 0)# |> # took out 0 values of CPUE. This inflates the R^2. 
  # In general if all subareas had 0 CPUE then it is either an anomoly, or
  # the particular stat areas might not be the best to use. 
  #na.omit() #Take out most recent year so that we can cbind wk83_data to pred_df
  #to calculate model efficiency (MAPE) don't do this line if running model for prediction

# Plot the relationship (should be R^2 of ~0.74)
wk83_data |>  
  ggplot(aes(x = rch_cpue+1e-4, y = return)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq() +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log")


# Percent rank score for current year value
wk83_data |> 
  mutate(rank = percent_rank(rch_cpue)) |> 
  filter(year == curr_year) |> 
  pull(rank)
  

# Fit the model
wk83_mod <- lm(log(return) ~ log(rch_cpue+1e-4), data = wk83_data)

#Beginning of code to evaluated model efficacy. No need to run if doing 
  #in season prediction. Use if doing model evaluation. 
pred_df <- predict(wk83_mod, interval = "prediction") |>
  as.data.frame() |>
  mutate(across(everything(), exp))  # back-transform from log

wk83_df <- cbind(wk83_data, pred_df)

# Calculate MAPE
MAPE(wk83_df$fit, wk83_df$return) 
#data up to 2023 gives a MAPE of 0.2808277 This is worse than the preseason forecast.
#Try to find a better inseason forecast OR use the preseason forecast. 

# End of code to determine model efficacy. 

# Model predictions
wk83_pred <- data.frame(
  rch_cpue = seq(
    min(wk83_data$rch_cpue, na.rm = TRUE), 
    max(wk83_data$rch_cpue, na.rm = TRUE), 
    length.out = 100
  )
) |> 
  bind_rows(
    wk83_data |> 
      #filter(year == curr_year) |> 
      select(year, rch_cpue)
  ) %>%
  cbind(
    .,
    predict(wk83_mod, ., interval = "pred", level = 0.75)
  ) |> 
  mutate(across(fit:upr, exp))


# Plot predictions
(wk83_plot <- wk83_pred |> 
  filter(is.na(year)) |> 
  ggplot(aes(x = rch_cpue, y = fit)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    fill = "blue",
    alpha = 0.3
  ) +
  geom_line(colour = "blue") +
  geom_point(
    data = wk83_data,
    aes(y = return)
  ) +
  geom_pointrange(
    data = filter(wk83_pred, year == curr_year),
    aes(ymin = lwr, ymax = upr),
    colour = "red"
  ) +
  annotate(
    "text",
    label = paste(
      "~italic(R)^2==",
      round(summary(wk83_mod)$adj.r.squared, 2)
    ),
    parse = TRUE,
    x = min(wk83_pred$rch_cpue),
    y = max(wk83_pred$upr),
    hjust = 0,
    vjust = 1,
    size = 6
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, max(wk83_pred$upr, wk83_data$return, na.rm = TRUE)),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "CPUE (Somass-origin Chinook)",
    y = "Somass Chinook return"
  )
)


# Save the plot with predictions
ggsave(
  plot = wk83_plot,
  filename = here(curr_year, "R-PLOT_wk83_cpue_model_prediction_2024model.png"),
  height = 4,
  width = 8,
  units = "in"
)

# New (2025)Week 83 model -----------------------------------------------------------


# Many options were explored. Settled on all areas since it has a higher R^2 from other areas
# and is more consistent from week 83 to week 84. 
# No transformations, since errors appear iid (identically and independently distributed)
# Other high R^2 values with combinations of areas seem spurious
# 
# R^2 of individual areas:
# A:0.31, B:0.04, C:0.79 negatively correlated not many values
# D:0.07, E:0.50 (not many years)
# F: no data points
# J: 0.07, K:0.01, M:<0.01, 
#
# R^2 of combinations of some areas
# Q-T: 0.50 but most folks catch Lingcod in area Q.
#c("23C", "23J", "23E"), # R^2 = 0.55
#c("23C", "23E", "23F", "23M", "23J", "23K", "23Q+123T"), #R^2 = 0.51
#c("23C", "23E", "23F", "23J", "23K", "23Q+123T"), #R^2 = 0.53
#c("23C", "23E", "23J", "23K", "23Q+123T"), #R^2 = 0.49
#c("23C", "23D","23E", "23F", "23M", "23J", "23K", "23Q+123T", "123R"), #R^2 = 0.58
#c("23C", "23D","23E", "23F", "23M", "23J", "23K", "23Q+123T"), #R^2 = 0.63
#c("23C", "23D","23E", "23F", "23M", "23J", "23K"), #R^2 = 0.333 
#c("23C", "23D", "23M", "23J", "23K", "23Q+123T"), #R^2 = 0.63
#c("23D","23E", "23F", "23M", "23J", "23K", "23Q+123T") #R^2 = 0.34
#c("23A", "23D", "23J", "23K",) #R^2 = 0.12




#########
# create local functions

#stat_area =  c("23A") The set of sub areas to use, can use multiple. 
#Use  unique(cpue$statsub) to identify which ones are available to use.
#Note some stat_areas have no data

#stat_week = c("83") The set of stat_week to use. 
#Use  unique(cpue$statsub) to identify which ones are available to use.
#Note some stat_weeks have no data

#cpue_type = #Note currently this is only used to name the plot when saving.
# Use inseason_ttl_cpue() if you want to use ttl_cpue.
# Use inseason_rch_cpue() if you want to use rch_cpue.

#Can either use "total" cpue which is the raw cpue or Robertson Creek Hatchery cpue
#Which uses the percent of rch estimated caught in that stat area(s) in previous years

inseason_ttl_cpue <- function(data = cpue, 
                     stat_week = c("83"),
                     stat_area =  c("23A"),
                     this_year = curr_year, 
                     cpue_type = "ttl_cpue"){
  
# Subset to data for wk83 relationship
statwk_data <- data |> 
  filter(
    period == stat_week, #
    statsub %in% stat_area,
    !if_any(c(cn_all_k, boat_trips), is.na)
  ) |> 
  summarize(
    .by = c(year, return),
    across(cn_all_k:boat_trips, sum)
  ) |> 
  mutate(
    ttl_cpue = cn_all_k/boat_trips,
    rch_cpue = rch_cn_k/boat_trips
  )|> 
  filter(ttl_cpue > 0.05) # |> # took out values of CPUE close to 0. This inflates the R^2. 
# In general if all subareas had 0 CPUE then it is either an anomoly, or
# the particular stat areas might not be the best to use. 

# Fit the model
wk83_mod <- lm(return ~ ttl_cpue, data = statwk_data)

pred_df <- predict(
  wk83_mod,
  newdata = statwk_data,
  interval = "prediction",
  level = 0.75 #Calculates a 75% prediction interval
) |>
  as.data.frame() |>
  mutate(year =     statwk_data$year,
         actual =   statwk_data$return,
         ttl_cpue = statwk_data$ttl_cpue,
         rch_cpue = statwk_data$rch_cpue)

#Here is the forecast, to adjust prediction interval change the level above. 
latest <- pred_df |> filter(year == this_year)
cat(this_year, " Forecast:", round(latest$fit, -3), "\n",
    "Lower 75% PI:",  round(latest$lwr, -3), "\n",
    "Upper 75% PI:",  round(latest$upr, -3), "\n")

#Calculate mean absolute percentage of error
(mape <- MAPE(y_pred = pred_df$fit[!is.na(pred_df$actual)],
              y_true = pred_df$actual[!is.na(pred_df$actual)]))

#pull out r.squared for figure
(r2 <- summary(wk83_mod)$r.squared)

my_plot <- ggplot(pred_df, aes(x = ttl_cpue, y = actual)) +
  geom_point(color = "steelblue", size = 3) + # actual values
  geom_smooth(method = "lm", color = "steelblue", se = FALSE) +  # regression line
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    fill = "steelblue",
    alpha = 0.3
  ) +
  geom_label_repel(aes(label = year),
                   segment.color = 'grey50') +
  geom_point(data = latest, aes(x = ttl_cpue[year == this_year],y = fit),
             color = "darkgreen", size = 4) +                              # 2025 forecast
  geom_errorbar(data = latest, aes(x = ttl_cpue[year == this_year], 
                                   ymin = lwr, ymax = upr),
                color = "darkgreen", width = 0.1) +
  geom_label_repel(data = latest, aes(x = ttl_cpue, y = fit, label = year), 
                   color = "darkgreen") +
  labs(
    x = "Total CPUE",
    y = "Return",
    title = paste("Return vs ", cpue_type, " of stat week ", stat_week, 
                  "sub area ", paste(stat_area, collapse = ", ")),
    subtitle = paste0("R² = ", round(r2, 3), 
                      " | MAPE = ", round(mape, 3),
                      " | Forecast = ", round(latest$fit, -3) , 
                      ", 75% Predictive Interval:(", round(latest$lwr, -3), ", ", round(latest$upr, -3),")")
  ) +
  theme_minimal()

print(my_plot)

## Save the plot with predictions
ggsave(filename = here(paste0(this_year, "/wk", stat_week, "_", paste(stat_area, collapse = "_"), "_", cpue_type, ".png")),
       plot = my_plot, 
       height = 4,
       width = 8,
       units = "in")
}

#Function for rch_cpue
#
#Arguments are much like for inseason_ttl_cpue() defined above with more information
#
#

inseason_rch_cpue <- function(data = cpue, 
                              stat_week = c("83"),
                              stat_area =  c("23A"),
                              this_year = curr_year, 
                              cpue_type = "rch_cpue"){
  
  # Subset to data for wk83 relationship
  statwk_data <- data |> 
    filter(
      period == stat_week, #
      statsub %in% stat_area,
      !if_any(c(cn_all_k, boat_trips), is.na)
    ) |> 
    summarize(
      .by = c(year, return),
      across(cn_all_k:boat_trips, sum)
    ) |> 
    mutate(
      ttl_cpue = cn_all_k/boat_trips,
      rch_cpue = rch_cn_k/boat_trips
    )|> 
    filter(rch_cpue > 0.05) # |> # took out values of CPUE close to 0. This inflates the R^2. 
  # In general if all subareas had 0 CPUE then it is either an anomoly, or
  # the particular stat areas might not be the best to use. 
  
  # Fit the model
  wk83_mod <- lm(return ~ rch_cpue, data = statwk_data)
  
  pred_df <- predict(
    wk83_mod,
    newdata = statwk_data,
    interval = "prediction",
    level = 0.75 #Calculates a 75% prediction interval
  ) |>
    as.data.frame() |>
    mutate(year =     statwk_data$year,
           actual =   statwk_data$return,
           rch_cpue = statwk_data$rch_cpue,
           rch_cpue = statwk_data$rch_cpue)
  
  #Here is the forecast, to adjust prediction interval change the level above. 
  latest <- pred_df |> filter(year == this_year)
  cat(this_year, " Forecast:", round(latest$fit, -3), "\n",
      "Lower 75% PI:",  round(latest$lwr, -3), "\n",
      "Upper 75% PI:",  round(latest$upr, -3), "\n")
  
  #Calculate mean absolute percentage of error
  (mape <- MAPE(y_pred = pred_df$fit[!is.na(pred_df$actual)],
                y_true = pred_df$actual[!is.na(pred_df$actual)]))
  
  #pull out r.squared for figure
  (r2 <- summary(wk83_mod)$r.squared)
  
  my_plot <- ggplot(pred_df, aes(x = rch_cpue, y = actual)) +
      geom_point(color = "steelblue", size = 3) + # actual values
      geom_smooth(method = "lm", color = "steelblue", se = FALSE) +  # regression line
      geom_ribbon(
        aes(ymin = lwr, ymax = upr),
        fill = "steelblue",
        alpha = 0.3
      ) +
      geom_label_repel(aes(label = year),
                       segment.color = 'grey50') +
      geom_point(data = latest, aes(x = rch_cpue[year == this_year],y = fit),
                 color = "darkgreen", size = 4) +                              # 2025 forecast
      geom_errorbar(data = latest, aes(x = rch_cpue[year == this_year], 
                                       ymin = lwr, ymax = upr),
                    color = "darkgreen", width = 0.1) +
      geom_label_repel(data = latest, aes(x = rch_cpue, y = fit, label = year), 
                       color = "darkgreen") +
      labs(
        x = "Total CPUE",
        y = "Return",
        title = paste("Return vs ", cpue_type, " of stat week ", stat_week, 
                      "sub area ", paste(stat_area, collapse = "_")),
        subtitle = paste0("R² = ", round(r2, 3), 
                          " | MAPE = ", round(mape, 3),
                          " | Forecast = ", round(latest$fit, -3) , 
                          ", 75% Predictive Interval:(", round(latest$lwr, -3), ", ", round(latest$upr, -3),")")
      ) +
      theme_minimal()
  print(my_plot)
  
  ## Save the plot with predictions
  ggsave(filename = here(paste0(this_year, "/wk", stat_week, "_", paste(stat_area, collapse = ", "), "_", cpue_type, ".png")),
         plot = my_plot, 
         height = 4,
         width = 8,
         units = "in")
}

all_area <- unique(cpue$statsub)
unique(cpue$period)

inseason_rch_cpue(data = cpue, stat_week = c("83"), stat_area =  c("23E"), this_year = 2025)

inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23A"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23B"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23C"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23D"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23E"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23F"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23J"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23K"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23M"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23R"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("84"), stat_area =  c("23Q+123T"), this_year = 2025)
inseason_rch_cpue(data = cpue, stat_week = c("83"), stat_area =  c("23Q+123T"), this_year = 2025)

#All areas
inseason_ttl_cpue(data = cpue, stat_week = c("8183"), stat_area =  all_area, this_year = 2025) # r2 = 0.55 f: 107
inseason_ttl_cpue(data = cpue, stat_week = c("8184"), stat_area =  all_area, this_year = 2025) # r2 = 0.61 f: 100
inseason_rch_cpue(data = cpue, stat_week = c("8183"), stat_area =  all_area, this_year = 2025) # r2 = 0.19
inseason_rch_cpue(data = cpue, stat_week = c("8184"), stat_area =  all_area, this_year = 2025) # r2 = 0.14

inseason_ttl_cpue(data = cpue, stat_week = c("8283"), stat_area =  all_area, this_year = 2025) # r2 = 0.52 f: 116
inseason_ttl_cpue(data = cpue, stat_week = c("8384"), stat_area =  all_area, this_year = 2025) # r2 = 0.60 f: 126
inseason_rch_cpue(data = cpue, stat_week = c("8283"), stat_area =  all_area, this_year = 2025) # r2 = 0.17
inseason_rch_cpue(data = cpue, stat_week = c("8384"), stat_area =  all_area, this_year = 2025) # r2 = 0.13

#Brads suggested areas
inseason_ttl_cpue(data = cpue, stat_week = c("8283"), stat_area =  c("23D", "23J", "23K"), this_year = 2025) # r2 = 0.07
inseason_ttl_cpue(data = cpue, stat_week = c("8284"), stat_area =  c("23D", "23J", "23K"), this_year = 2025) # r2 = 0.26
inseason_rch_cpue(data = cpue, stat_week = c("8283"), stat_area =  c("23D", "23J", "23K"), this_year = 2025) # r2 = 0.11
inseason_rch_cpue(data = cpue, stat_week = c("8284"), stat_area =  c("23D", "23J", "23K"), this_year = 2025) # r2 = 0.30

#area 23A
inseason_ttl_cpue(data = cpue, stat_week = c("8283"), stat_area =  c("23A"), this_year = 2025) # r2 = 0.40
inseason_ttl_cpue(data = cpue, stat_week = c("8284"), stat_area =  c("23A"), this_year = 2025) # r2 = 0.21
inseason_rch_cpue(data = cpue, stat_week = c("8283"), stat_area =  c("23A"), this_year = 2025) # r2 = 0.40
inseason_rch_cpue(data = cpue, stat_week = c("8284"), stat_area =  c("23A"), this_year = 2025) # r2 = 0.20
