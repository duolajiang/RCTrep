#' @export
SEisw <- R6::R6Class(
  "SEisw",
  inherit = SEstimator,
  public = list(
    weighting_estimator = "ISW",
    weighting_method = NA,
    est_ss = NA,
    sampling_formula = NULL,

    initialize = function(target.obj, source.obj,
                          weighting_method="glm",
                          confounders_sampling_name,
                          sampling_formula = NULL,
                          ...){
      super$initialize(target.obj, source.obj, weighting_method,confounders_sampling_name)
      self$sampling_formula <- sampling_formula
      private$trargs <- list(...)
    }
  ),

  private = list(
    trargs = NA,

    get_weight = function(source.data,target.data, vars_weighting){
      #browser()
      source <- select(source.data, vars_weighting)
      target <- select(target.data, vars_weighting)

      source$selection <- 0
      target$selection <- 1
      data <- rbind(source, target)
      data$selection <- as.factor(data$selection)

      #browser()
      samplingscore <- private$fit_est(data, vars_weighting, self$sampling_formula)
      weight <- samplingscore / (1 - samplingscore)

      nss <- dim(source)[1]
      # in this way, sum(weight) == number of observations in source data
      weight <- weight * nss/ sum(weight)

      return(weight)
    },

    fit_est = function(data, vars_weighting, sampling_formula) {
      #browser()
      if (is.null(sampling_formula)) {
        model <- caret::train(
          x = data[, vars_weighting],
          y = data$selection,
          method = self$weighting_method,
          unlist(private$trargs)
        )
      } else {
        model <- caret::train(
          form = sampling_formula,
          data = data,
          method = self$weighting_method,
          unlist(private$trargs)
        )
      }
      selection_score <- predict(model, type = "prob")[data$selection == 0, c("1")]
      return(selection_score)
    }
  )
)
