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
  filter(sw %in% c(82, 83, 84, 91, 92)) |> 
  pivot_longer(cols = cn_all_k:boat_trips) |> 
  pivot_wider(
    names_from = sw,
    values_from = value
  ) |> 
  rowwise() |> 
  # Sum kept chinook and interviews across stat week periods
  mutate(
    cum83 = `82` + `83`,
    cum84 = sum(c_across(`82`:`84`), na.rm = TRUE),
    cum91 = sum(c_across(`83`:`91`), na.rm = TRUE),
    cum92 = sum(c_across(`83`:`92`), na.rm = TRUE),
    `8384` = `83` + `84`,
    `8491` = `84` + `91`
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


# Week 83 model -----------------------------------------------------------


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

# 
#pred_df <- pred_df |>
#  mutate(actual = model_data$return)

#Note this figure doesn't work
ggplot(pred_df, aes(x = factor(year))) +
  geom_col(aes(y = return), fill = "steelblue", alpha = 0.6) +
  geom_point(aes(y = fit), color = "darkred", size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "darkred") +
  labs(
    x = "Year",
    y = "Return",
    title = "Actual vs Forecasted Returns",
    subtitle = "Bars = Actual | Points & Lines = Forecast with Prediction Interval"
  ) +
  theme_minimal()

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
  filename = here(curr_year, "R-PLOT_wk83_cpue_model_prediction.png"),
  height = 4,
  width = 8,
  units = "in"
)

# New (2025)Week 83 model -----------------------------------------------------------


# Exploratory analysis (q.v. here("plots")) tells us the best model for 
# Week 83 is the rch loglog from subareas 23J, 23C, and 23E
unique(cpue$statsub)
# R^2 of individual areas:
# A:0.31, B:0.04, C:0.79 negatively correlated not many values
# D:0.07, E:0.50 (not many years)
# F: no data points
# J: 0.07, K:0.01, M:<0.01, 
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
stat_area =  c("23A")
stat_week = c("83")
cpue_type = c("ttl_cpue")
# unique(cpue$period)
# Subset to data for wk83 relationship
wk83_data <- cpue |> 
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
  #na.omit() #took out most recent year so that I can cbind wk83_data to pred_df

# Plot the relationship (should be R^2 of ~0.31)
wk83_data |>  
  ggplot(aes(x = ttl_cpue, y = return)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq() +
  geom_label_repel(aes(label = year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ggtitle(paste(cpue_type, " of stat week ", stat_week, 
                "sub areas ", stat_area[1], stat_area[2])) +
  theme_classic()

# Fit the model
wk83_mod <- lm(return ~ ttl_cpue, data = wk83_data)

#Beginning of code to evaluated model efficacy. 
pred_df <- predict(wk83_mod, interval = "prediction") |>
  as.data.frame() 

wk83_df <- cbind(wk83_data, pred_df)

# Calculate MAPE
MAPE(wk83_df$fit, wk83_df$return) 
#data up to 2023 gives a MAPE of 0.4222564 This is worse than the preseason forecast.
#Try to find a better inseason forecast OR use the preseason forecast. 

# 
pred_df <- pred_df |>
  mutate(actual = model_data$return)

#Note this figure doesn't work
ggplot(pred_df, aes(x = factor(year))) +
  geom_col(aes(y = return), fill = "steelblue", alpha = 0.6) +
  geom_point(aes(y = fit), color = "darkred", size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "darkred") +
  labs(
    x = "Year",
    y = "Return",
    title = "Actual vs Forecasted Returns",
    subtitle = "Bars = Actual | Points & Lines = Forecast with Prediction Interval"
  ) +
  theme_minimal()

# End of code to determine model efficacy. 

# Model predictions
wk83_pred <- data.frame(
    wk83_data |> 
      #filter(year == curr_year) |> 
      select(year, ttl_cpue)
 %>%
  cbind(
    .,
    predict(wk83_mod, ., interval = "pred", level = 0.75)
  ) 


# Plot predictions
(wk83_plot <- wk83_pred |> 
    filter(is.na(year)) |> 
    ggplot(aes(x = ttl_cpue, y = fit)) +
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
    geom_text(aes(label = year),
                     vjust = -1,
                      color = "black") +
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
      x = min(wk83_pred$ttl_cpue),
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
    ggtitle(paste(cpue_type, " of stat week ", stat_week, 
                  "sub area ", stat_area[1])) +
    labs(
      x = "CPUE",
      y = "Somass Chinook return"
    )
)


# Save the plot with predictions
ggsave(
  plot = wk83_plot,
  filename = here(curr_year, "R-PLOT_wk83_cpue_model_prediction.png"),
  height = 4,
  width = 8,
  units = "in"
)




