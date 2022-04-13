library(tidyverse)
library(covidHubUtils)
theme_set(theme_bw())

#' Parse model names
parse_model_names <- function(model) {
  name_segments <- str_split(model, "_")
  return(data.frame(
    case_type = sapply(name_segments, function(x) {
      if (x[1] %in% c("none", "report", "test")) x[1] else NA_character_
      }),
    case_timing = sapply(name_segments, function(x) x[2]),
    smooth_case = sapply(name_segments, function(x) x[5]),
    p = sapply(name_segments, function(x) x[8]),
    d = sapply(name_segments, function(x) x[10]),
    P = sapply(name_segments, function(x) x[12]),
    D = sapply(name_segments, function(x) x[14])
  ))
}

inc_hosp_targets <- paste(0:130, "day ahead inc hosp")

models <- list.dirs(
  "forecasts",
  full.names = FALSE,
  recursive = FALSE)

forecasts <- load_forecasts(
  dates = seq.Date(from = as.Date("2020-12-07"),
                   to = as.Date("2021-06-07"),
                   by = 7),
  date_window_size = 6,
  locations = c("25"),
  types = c("point", "quantile"),
  targets = inc_hosp_targets,
  source = "local_hub_repo",
  hub_repo_path = "forecasts",
  data_processed_subpath = "",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)

hub_forecasts <- load_forecasts(
  models = c("COVIDhub-baseline", "COVIDhub-ensemble"),
  dates = unique(forecasts$forecast_date),
  date_window_size = 6,
  locations = c("25"),
  types = c("point", "quantile"),
  targets = inc_hosp_targets,
  source = "local_hub_repo",
  hub_repo_path = "../covid19-forecast-hub",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)
# combined_forecasts <- forecasts
combined_forecasts <- dplyr::bind_rows(forecasts, hub_forecasts)

combined_forecasts %>%
  dplyr::filter(type == "quantile") %>%
  dplyr::count(model) %>%
  as.data.frame()

# forecasts <- covidHubUtils::align_forecasts(forecasts)

# extract_variation <- function(model_name, var_name) {
#   ind <- regexpr(var_name, model_name) + nchar(var_name) + 1
#   model_substr <- substr(model_name, ind, nchar(model_name))
#   ind <- regexpr("_", model_substr)
#   substr(model_substr, 1, ifelse(ind == -1, nchar(model_substr), ind - 1))
# }

truth_data <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = c("25"),
  hub = "US"
)

truth_data <- truth_data %>%
  dplyr::filter(target_end_date >= "2020-10-01")

scores <- covidHubUtils::score_forecasts(
  combined_forecasts,
  truth_data %>% filter(target_end_date <= "2021-06-12"),
  use_median_as_point = TRUE)

scores <- dplyr::bind_cols(
  scores,
  parse_model_names(scores$model)
)

mean_scores <- scores %>%
  group_by(model) %>%
  summarize(wis = mean(wis),
            mae = mean(abs_error),
            coverage_95 = mean(coverage_95)) %>%
  arrange(wis)

mean_scores <- dplyr::bind_cols(
  mean_scores,
  parse_model_names(mean_scores$model)
) %>%
  as.data.frame()
mean_scores

# plot the forecasts
# models_to_plot <- mean_scores %>%
#   filter(wis < 25) %>%
#   pull(model)

models_to_plot <- c(
  "COVIDhub-ensemble", "COVIDhub-baseline",
  mean_scores %>%
    dplyr::filter(!is.na(case_type)) %>%
    dplyr::group_by(case_type) %>%
    dplyr::slice_min(wis) %>%
    dplyr::pull(model)
)

forecasts_to_plot <- covidHubUtils::get_plot_forecast_data(
  forecast_data = combined_forecasts,
  truth_data = truth_data,
  # models_to_plot = unique(combined_forecasts$model),
  models_to_plot = models_to_plot,
  horizons_to_plot = 28,
  quantiles_to_plot = c(0.025, 0.5, 0.975),
  # quantiles_to_plot = c(0.025, 0.25, 0.5, 0.75, 0.975),
  target_variable_to_plot = "inc hosp",
  hub = "US")

forecasts_to_plot <- forecasts_to_plot %>%
  dplyr::filter(!grepl("Observed Data", model))

# plot of predictive medians and 95% intervals, facetted by model
p <- ggplot() +
  geom_ribbon(
    data = forecasts_to_plot %>%
      dplyr::filter(!is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      ymin = lower,
      ymax = upper,
      alpha = `Prediction Interval`,
      group = paste0(model, forecast_date)
    ),
    fill = "cornflowerblue"
  ) +
  scale_alpha_manual(
    values = c("95%" = 0.5)
  ) +
  geom_line(
    data = forecasts_to_plot %>%
      dplyr::filter(is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      y = point,
      group = paste0(model, forecast_date)
    ),
    color = "blue"
  ) +
  geom_line(
    data = truth_data %>% dplyr::select(-model) %>% dplyr::filter(target_end_date < "2021-07-01"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  # ylim(0, 500) +
  facet_wrap( ~ model, scales = "free_y", ncol = 2)
p


# plot of predictive medians only (no intervals), facetted by forecast date
p <- ggplot() +
  # geom_ribbon(
  #   data = forecasts_to_plot %>%
  #     dplyr::filter(!is.na(`Prediction Interval`)),
  #   mapping = aes(
  #     x = target_end_date,
  #     ymin = lower,
  #     ymax = upper,
  #     alpha = `Prediction Interval`,
  #     group = paste0(model, forecast_date)
  #   ),
  #   fill = "cornflowerblue"
  # ) +
  # scale_alpha_manual(
  #   values = c("95%" = 0.5)
  # ) +
  geom_line(
    data = truth_data %>% dplyr::select(-model) %>% dplyr::filter(target_end_date < "2021-07-01"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  geom_line(
    data = forecasts_to_plot %>%
      dplyr::filter(is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      y = point,
      color = model,
      group = paste0(model, forecast_date)
    ),
    size=1
  ) +
  # ylim(0, 500) +
  facet_wrap( ~ forecast_date, scales = "free_y")
p



# plot of mean WIS by forecast date
mean_scores_by_forecast_date <- scores %>%
  dplyr::filter(model %in% models_to_plot) %>%
  dplyr::group_by(model, forecast_date) %>%
  dplyr::summarize(
    wis = mean(wis),
    mae = mean(abs_error)
  )
p <- ggplot(mean_scores_by_forecast_date) +
  geom_line(mapping = aes(x = forecast_date, y = wis, color = model, linetype = model)) +
  theme_bw()
p

# covidHubUtils::plot_forecasts(
#     forecast_data = stl_forecasts,
#     facet = formula(trend_w~location+trend_deg),
#     hub = "US",
#     truth_source = "HealthData",
#     subtitle = "none",
#     title = "none",
#     show_caption = FALSE,
#     plot = FALSE
#   ) +
#   scale_x_date(
#     breaks = "1 month",
#     date_labels = "%b-%y",
#     limits = as.Date(c(
#       reference_date - (7 * 32), reference_date + 28
#     ), format = "%b-%y")
#   ) +
#   theme_update(
#     legend.position = "bottom",
#     legend.direction = "vertical",
#     legend.text = element_text(size = 8),
#     legend.title = element_text(size = 8),
#     axis.text.x = element_text(angle = 90),
#     axis.title.x = element_blank()
#   ) +
#   ggforce::facet_wrap_paginate(
#     ~ location,
#     scales = "free",
#     ncol = 2,
#     nrow = 3,
#     page = 1
#   )

# n <- n_pages(p)
# pdf(
#   plot_path,
#   paper = 'A4',
#   width = 205 / 25,
#   height = 270 / 25
# )
# for (i in 1:n) {
#   suppressWarnings(print(
#     p + ggforce::facet_wrap_paginate(
#       ~ location,
#       scales = "free",
#       ncol = 2,
#       nrow = 3,
#       page = i
#     )
#   ))
# }
# dev.off()


# final selection of models
sarix_mean_scores <- mean_scores %>%
    dplyr::filter(!is.na(case_type)) %>%
    dplyr::mutate(rank = row_number())

# select best model within each combination of case_type and smooth_case options
sarix_mean_scores %>%
    dplyr::group_by(case_type, smooth_case) %>%
    dplyr::slice_min(wis) %>%
    dplyr::arrange(wis)

# Output is:
# # A tibble: 5 × 12
# # Groups:   case_type, smooth_case [5]
#   model                                                  wis   mae coverage_95 case_type case_timing smooth_case p     d     P     D      rank
#   <chr>                                                <dbl> <dbl>       <dbl> <chr>     <chr>       <chr>       <chr> <chr> <chr> <chr> <int>
# 1 report_final_smooth_case_False_SARIX_p_2_d_0_P_1_D_1  19.0  28.4       0.986 report    final       False       2     0     1     1         1
# 2 report_final_smooth_case_True_SARIX_p_2_d_0_P_0_D_1   19.1  27.4       0.996 report    final       True        2     0     0     1         2
# 3 test_final_smooth_case_False_SARIX_p_2_d_0_P_1_D_1    20.5  31.0       0.984 test      final       False       2     0     1     1        10
# 4 test_final_smooth_case_True_SARIX_p_2_d_0_P_0_D_1     22.1  31.7       0.994 test      final       True        2     0     0     1        24
# 5 none_final_smooth_case_False_SARIX_p_2_d_1_P_2_D_0    23.0  32.6       0.994 none      final       False       2     1     2     0        36
