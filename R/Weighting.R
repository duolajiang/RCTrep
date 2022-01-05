PatternDistribution <- function(data, vars_weighting) {
  #browser()
  if (class(data) == "data.frame") {
    pattern_data <- data %>%
      group_by(across(all_of(vars_weighting)))
    pattern_id_4each_obs <- pattern_data %>% group_indices()
    pattern_density <- pattern_data %>% summarise(n = n())
    pattern_density$density <- pattern_density$n / sum(pattern_density$n)
    pattern_density <- dplyr::select(pattern_density, -n)
    return(list(pattern_id_4each_obs, pattern_density))
  } else {
    var_names_levels <- getVariableNameAndLevels4SummaryStudy(data$univariate_p, vars_weighting)
    pattern <- getPatternDataFrame(var_names_levels)
    density <- getPatternDensityBasedOnUnivariateDistribution(pattern, data, vars_weighting)
    pattern_density <- cbind(pattern, density)
    return(pattern_density)
  }
}

SelectionScoreModeling <- function(data,  vars_weighting, weighting_method) {
  model <- caret::train(
    x = data[, vars_weighting],
    y = data$selection,
    method = weighting_method
  )
  selection_score <- predict(model, type = "prob")[data$selection == 0, c("1")]
  return(selection_score)
}
