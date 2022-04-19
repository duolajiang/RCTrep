#' @export
SEisw <- R6::R6Class(
  "SEisw",
  inherit = SEstimator,
  public = list(
    weighting_estimator = "ISW",
    weighting_method = NA,
    est_ss = NA,

    initialize = function(target.obj, source.obj,
                          weighting_method="glm",
                          confounders_sampling_name,
                          ...){
      super$initialize(target.obj, source.obj, weighting_method,confounders_sampling_name)
      #self$weighting_estimator <- weighting_estimator
      self$weighting_method <- weighting_method
      private$trargs <- list(...)
    }
  ),

  private = list(
    trargs = NA,
    get_weight = function(source.data,target.data, vars_weighting, formula_ss=NULL){
      #browser()
      source <- select(source.data, vars_weighting)
      target <- select(target.data, vars_weighting)

      source$selection <- 0
      target$selection <- 1
      data <- rbind(source, target)
      data$selection <- as.factor(data$selection)
      sampling_formula <- self$ss_formula
      samplingscore <- private$fit_est(data, vars_weighting, formula_ss)
      weight <- samplingscore / (1 - samplingscore)
      return(weight)
    },

    fit_est = function(data, vars_weighting, formula_ss) {
      #browser()
      if (is.null(formula_ss)) {
        model <- caret::train(
          x = data[, vars_weighting],
          y = data$selection,
          method = self$weighting_method,
          unlist(private$trargs)
        )
      } else {
        model <- caret::train(
          form = formula_ss,
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
