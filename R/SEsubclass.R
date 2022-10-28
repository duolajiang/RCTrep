#' @export
SEsubclass <- R6::R6Class(
  "SEexact",
  inherit = SEstimator,
  public = list(
    weighting_estimator = "subclass",
    weighting_method = NA,

    initialize = function(target.obj, source.obj,
                          weighting_method="glm",
                          confounders_sampling){
      super$initialize(target.obj, source.obj,weighting_method,confounders_sampling_name)
      self$weighting_method <- weighting_method
    }
  ),
  private = list(
    get_weight = function(source.data,target.data, vars_weighting){
      #browser()
      source <- select(source.data, vars_weighting)
      target <- select(target.data, vars_weighting)
      source$selection <- 0
      target$selection <- 1
      data <- rbind(source, target)
      data$selection <- as.factor(data$selection)
      matching_formula <- formula(paste("selection~", paste(vars_weighting, collapse = "+")))
      matchit.obj <- MatchIt::matchit(matching_formula, method = "subclass", distance = self$weighting_method,
                                      data = data)
      weight <- matchit.obj$weights[1:dim(source)[1]]
      return(weight)
    }
  )
)
