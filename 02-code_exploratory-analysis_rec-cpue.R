# Load packages and functions ------------------------------------------------------

pkgs <- c(
  "tidyverse","readxl","ggridges","here",
  "sf", "broom"
)
#install.packages(pkgs)

library(tidyverse); theme_set(theme_bw(base_size = 16))
library(here)
library(ggridges)
library(sf)
library(readxl)
library(broom)



# Summarize interview data to get CPUE over time & area groupings --------

# Load interview data
interview_summary <- read_xlsx(
  here("01-data_CN_return_predictors.xlsx"),
  sheet = "CREEL Interview Summary"
) |> 
  rename_with(tolower)


# Somass terminal adult return data
bs_cn <- read_xlsx(
  here("01-data_CN_return_predictors.xlsx"),
  sheet = "CN_return_predictors"
) |> 
  select(year, matches("(?i)Somass_term_"))
  

# Group data by strata and calculate CPUEs
strata_sums <-  interview_summary |> 
  filter(
    # Interviews w/these comments are used to calculate CPUE
    asscd_txt %in% c("Adipose fin not chkd", "Complete Form", "Fish not seen for ID"), 
    #!statsub %in% c("23L", "23F"), # Very little data is associated with these areas
    str_detect(statsub, "^23[[:upper:]]")
  ) |> # Remove offshore areas
  # Combine subareas that were split in 2011
  mutate(
    statsub = case_when( 
      statsub %in% c("23G", "23P", "23O") ~ "23O+P", # in 2011, 23G was split into 23O and 23P
      statsub %in% c("23H", "23Q", "23N") ~ "23Q+N", # in 2011, 23H was split into 23Q and 23N
      statsub == "23I" ~ "123R", # Changed in 2022
      statsub == "23L" ~ "23K", # **** GUESS ***** revise once discovered
      TRUE ~ statsub
      )
  ) |>  
  summarise(
    .by = c(year, statsub, sw_2023),
    across(c(cn_all_k, rch_cn_k), sum),
    boat_trips = n()
  ) |> 
  # Add column with adult return data
  left_join(select(.data = bs_cn, year, Somass_term_adult_return)) |> 
  rename("return" = "Somass_term_adult_return") |> 
  # Keep only the mid-Aug to mid-Sept stat weeks
  filter(sw_2023 %in% c(82, 83, 84, 91, 92)) |> 
  pivot_longer(cols = cn_all_k:boat_trips) |> 
  pivot_wider(
    names_from = sw_2023,
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


# Minimalist version of the data
minimal_data <- strata_sums |> 
  filter(!if_any(c(return, cn_all_k, rch_cn_k, boat_trips), is.na))


# Make a list of subarea combinations to group by
combo_n <- seq.int(3,4) # set #s of subarea combos to try 
# Start small (e.g. 3-4) to get script working smoothly
# Will take a long time (45+min) to run for larger groups

subarea_combos <- list(unique(strata_sums$statsub)) |> 
  rep(length(combo_n)) |> 
  map2(
    combo_n, 
    ~ combn(x = .x, m = .y, FUN = list),
    .id = "list_n"
  ) |> 
  unlist(recursive = FALSE)


# Make a nested dataframe with CPUE summarized across different area groups
  nested_data <- subarea_combos |> 
    enframe(name = "subareas") |> 
    mutate(
      subareas = map(
        value, 
        ~ paste(.x, collapse = "_")
      ) |> 
        unlist()
    ) |> 
    expand_grid(period = unique(minimal_data$period)) |> 
    mutate(
      data = map2(
        .x = value,
        .y = period,
        ~ filter(
          .data = minimal_data,
          statsub %in% .x,
          period == .y
        ) |> 
          summarize(
            .by = c(year, return),
            across(c(cn_all_k, rch_cn_k, boat_trips), ~sum(.x, na.rm = TRUE))
          ) |> 
          mutate(cpue = cn_all_k / boat_trips),
        .progress = "Filter and summarize:"
      ),
      rch_data = map(
        .x = data,
        ~ mutate(.x, cpue = rch_cn_k / boat_trips),
        .progress = "Get RCH CPUEs:"
      ),
      across(
        contains("data"),
        ~ map(
          .x,
          ~ select(.x, year, return, cpue),
          .progress = "Trim columns:"
        )
      )
    ) |> 
    pivot_longer(
      cols = contains("data"),
      names_to = "cpue",
      values_to = "data"
    ) |> 
    rowwise() |> 
    mutate(
      cpue = if_else(str_detect(cpue, "rch"), "rch", "ttl"),
      n_obs = nrow(data)
    ) |> 
    ungroup() |> 
    filter(n_obs > 9)
  

# Fit models to the data with various transformations
  nested_models <- nested_data |> 
    select(-value) |> 
    mutate(
      mod_lin = map(
        .x = data,
        ~ lm(
          return ~ cpue,
          data = .x
        ),
        .progress = "Fitting linear models:"
      ),
      mod_log = map(
        .x = data,
        ~ lm(
          log(return) ~ cpue,
          data = .x
        ),
        .progress = "Fitting log models:"
      ),
      mod_loglog = map(
        .x = data,
        ~ lm(
          log(return) ~ log(cpue+0.0001),
          data = .x
        ),
        .progress = "Fitting log-log models:"
      ),
      mod_loglin = map(
        .x = data,
        ~ lm(
          return ~ log(cpue+0.0001),
          data = .x
        ),
        .progress = "Fitting linear-log models:"
      ),
      .keep = "unused" # drop the "data" column
    ) |> 
    pivot_longer(
      cols = contains("mod"),
      names_to = "transformation",
      names_prefix = "mod_",
      values_to = "model"
    ) |> 
    rowwise() |> 
    mutate(r.squared = summary(model)$r.squared) |> 
    ungroup()
  

# Have a look at the distribution of R2 values
nested_models |> 
  ggplot(
    aes(
      y = fct_reorder(period, r.squared),
      x = r.squared
    )
  ) +
  facet_grid(cpue ~ transformation) +
  geom_density_ridges(
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  # Overlay vertical line showing grand median across weeks
  geom_vline(
    data = summarize(
      nested_models,
      .by = c(transformation, cpue),
      r.squared = median(r.squared)
    ),
    aes(xintercept = r.squared),
    colour = "red",
    linewidth = 0.75,
    lty = 2
  )



# Plot best models on a map -----------------------------------------------


# Load Shapefile with CREEL subareas
creel_shp <- sf::st_read(here("Creel_Survey_Areas")) |> 
  filter(STATAREA == 23) # Keep only PFMA 23


# Download high resolution coastline data from GSHHS
coastline <- read_sf(here("GSHHS_shp", "f", "GSHHS_f_L1.shp")) |> 
  st_transform(crs = st_crs(creel_shp))


# Switch off spherical geometry for easier cropping
sf_use_s2(FALSE)


# Stipulate bounding box with small buffer
bbox <- creel_shp |> 
  st_bbox()


# Clip shapefile to desired extent
bs_land <- st_crop(coastline, bbox + c(-0.05, -0.05, 0.05, 0.05))


# Save data to plot r2 values as fill under creel subarea
top_models <- nested_models |> 
  filter(n_obs > 12) |> # A couple of models based on less data had strong relationships
  slice_max(
    order_by = r.squared,
    n = 20
  ) |> 
  distinct(subareas, period, transformation, .keep_all = TRUE) |> 
  separate(
    subareas,
    into = paste0("subarea", 1:4),
    sep = "_",
    remove = FALSE
  ) |> 
  pivot_longer(
    matches("subarea\\d"),
    values_to = "subarea",
    values_drop_na = TRUE
  ) |> 
  select(-name) |> 
  mutate(
    subarea = str_remove_all(subarea, "\\+N|\\+P") |> 
      str_replace_all(c("23I" = "123R")),
    model = paste(cpue, period, transformation, subareas)
  ) |> 
  left_join(
    creel_shp,
    by = join_by(subarea == subareaid)
  )


# Create basemap
(bs_basemap <- bs_land |> 
    expand_grid(model = unique(top_models$model)) |> 
    ggplot(aes(geometry = geometry)) +
    geom_sf(data = st_as_sfc(bbox), fill = "lightblue") +
    #geom_sf(data = creel_shp) +
    geom_sf(fill = "grey50") +
    ggspatial::annotation_scale(location = "br") +
    ggspatial::annotation_north_arrow(
      height = unit(2, "lines"),
      width = unit(2, "lines")
    ) +
    coord_sf(
      xlim = bbox[c(1,3)],
      ylim = bbox[c(2,4)],
      expand = FALSE
    ) +
    scale_y_continuous(breaks = c(48.8, 49.0, 49.2)) +
    scale_x_continuous(breaks = c(-125.0, -125.5)) +
    theme(
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE,
      panel.grid = element_line(linewidth = 0.1)
    )
)  


# Plot data from top models on map
bs_basemap +
  facet_wrap(~model) +
  geom_sf(
    data = creel_shp,
    fill = NA
  ) +
  geom_sf(
    data = top_models,
    aes(fill = r.squared)
  ) +
  scale_fill_viridis_c(option = "rocket") +
  coord_sf(expand = FALSE)


# Plot top models all overlaid on top of each other to show prime areas
(quasi_heatmap <- bs_basemap +
  facet_wrap(~ period) +
  geom_sf(
    data = creel_shp,
    fill = NA
  ) +
  geom_sf(
    data = top_models,
    fill = "red",
    alpha = 0.1
  ) +
  geom_sf(fill = "grey50") +
  geom_sf_label(
    data = top_models,
    aes(label = subarea)
  ) +
  ggspatial::annotation_scale(location = "br") +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL)
)


# Save the plot with top model areas all overlaid on one another
ggsave(
  quasi_heatmap,
  file = here(
    "plots",
    "R-PLOT_quasi_heatmap_of_best_model_subareas.png"
  ),
  height = 4,
  width = 8.5,
  units = "in"
)



# Plot best model predictions versus observed data ------------------------


# Extract the underlying data from each of the top models
top_models_pred <- top_models |> 
  distinct(subareas, period, cpue, transformation) |> 
  left_join(nested_models) |> 
  rowwise() |> 
  mutate(
    glance = list(glance(model)),
    model_frame = list(model.frame(model)),
    varname = colnames(model_frame)[2],
    # Make a dataframe for model predictions
    pred_frame = if_else(
      transformation %in% c("loglog", "linlog"),
      list(
        data.frame(
          cpue = seq(
            min(exp(model_frame[2])), 
            max(exp(model_frame[2])), 
            length.out = 100
          )
        )
      ),
      list(
        data.frame(cpue = seq(min(model_frame[2]), max(model_frame[2]),length.out = 100))
      )
    ),
    # Generate the predictions with lower and upper CIs
    predictions = predict(model, pred_frame, interval = "pred") |> 
      as.data.frame() |> 
      list(),
    # Combine predictions with CPUE values
    pred_frame = list(cbind(pred_frame, predictions)),
    # Back-transform the predictions
    pred_frame = if_else(
      transformation %in% c("log", "loglog"),
      list(mutate(pred_frame, across(fit:upr, exp))),
      list(pred_frame)
    ),
    # Back-transform the observed values
    model_frame = list(rename(model_frame, return = 1, cpue = 2)),
    model_frame = case_when(
      transformation == "lin" ~ list(model_frame),
      transformation == "log" ~ list(mutate(model_frame, return = exp(return))),
      transformation == "loglog" ~ list(mutate(model_frame, across(return:cpue, exp))),
      transformation == "linlog" ~ list(mutate(model_frame, cpue = exp(cpue)))
    ),
    # Names of models
    model_name = paste(period, cpue, transformation, subareas)
  ) |> 
  select(-r.squared, -predictions) |> 
  ungroup() |> 
  unnest(glance)


# Make plots of the predicted versus observed values
list(
  observed_data = pull(top_models_pred, model_frame),
  pred_data = pull(top_models_pred, pred_frame),
  model_name = pull(top_models_pred, model_name),
  r.sq = pull(top_models_pred, adj.r.squared)
) |> 
  pmap(
    ~ggplot(
      data = ..2,
      aes(x = cpue, y = fit)
    ) +
      geom_line(colour = "blue") +
      geom_ribbon(
        aes(ymin = lwr, ymax = upr),
        alpha = 0.3,
        fill = "blue"
      ) +
      geom_point(data = ..1, aes(y = return)) +
      annotate(
        "text",
        label = paste(
          "~italic(R)^2==",
          round(..4, 3)
        ),
        x = min(..2$cpue),
        y = max(..2$upr)*0.9,
        hjust = 0,
        vjust = 1,
        parse = TRUE,
        size = 5
      ) +
      scale_y_continuous(
        limits = c(0, max(..2$upr)),
        expand = expansion(mult = c(0, 0.05)),
        labels = scales::comma,
        oob = scales::squish
      ) +
      labs(
        y = "Chinook return",
        x = "Recreational CPUE",
        tag = paste("Model:", ..3)
      ) +
      theme(
        plot.tag.location = "panel",
        plot.tag.position = "topleft",
        plot.tag = element_text(margin = margin(l = 1, t = 1, unit = "lines"))
      )
  )




# Conclusions -------------------------------------------------------------


# Clear best relationships emerged 
