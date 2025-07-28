# Packages ----------------------------------------------------------------


pkgs <- c("tidyverse", "here", "readxl", "janitor", "ggpmisc")
#install.packages(pkgs)


library(tidyverse); theme_set(theme_bw(base_size = 14))
library(here)
library(readxl)
library(janitor)
library(ggpmisc)


curr_year <- 2025


# Load data ---------------------------------------------------------------


# Somass terminal adult return data
bs_cn <- read_xlsx(
  here("01-data_CN_return_predictors.xlsx"),
  sheet = "CN_return_predictors"
) |> 
  select(year, matches("(?i)Somass_term_"))


# CPUEs from creel data
cpue <- read_excel(
  here("01-data_CN_return_predictors.xlsx"), 
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
    across(c(cn_all_k, rch_cn_k), sum),
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
  )


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
      filter(year == curr_year) |> 
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


