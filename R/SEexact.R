#' @export
SEexact <- R6::R6Class(
  "SEexact",
  inherit = SEstimator,
  public = list(
    weighting_estimator = "Exact",

    initialize = function(target.obj, source.obj,
                          weighting_method=NULL,
                          confounders_sampling_name){
      super$initialize(target.obj, source.obj, weighting_method,confounders_sampling_name)
      #browser()
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
      # default for ATT, means weight for selected 1.
      matchit.obj <- MatchIt::matchit(matching_formula, method = "exact", data = data)
      weight <- matchit.obj$weights[1:dim(source)[1]]

      return(weight)
    }
  )
)
