#' @export
SEstimator_wrapper <- function(Estimator, target.obj, source.obj,
                               confounders_sampling_name,
                               method="glm",
                               sampling_formula = NULL, ...){
  if ("TEstimator_pp" %in% class(source.obj)) {
    if (! Estimator %in% "Exact") {
      stop("for now, we only support Exact matching for TEstimator_pp object. In other words, we only support exact matching when aggregated data is public (data.public=FALSE is indicated in TEstimator_wrapper() ). In the future, we will support other estimators for estimating weight. To be continued... ")
    } else {
      obj <- SEstimator_pp$new(target.obj=target.obj,
                               source.obj=source.obj,
                               confounders_sampling_name=confounders_sampling_name)
    }
  } else if (Estimator=="Exact"){
    obj <- SEexact$new(target.obj=target.obj,
                       source.obj=source.obj,
                       confounders_sampling_name=confounders_sampling_name)
  } else if (Estimator=="ISW"){
    obj <- SEisw$new(target.obj=target.obj,
                     source.obj=source.obj,
                     confounders_sampling_name=confounders_sampling_name,
                     weighting_method = method,
                     sampling_formula = sampling_formula,
                     ...)
  } else if (Estimator=="Subclass"){
    obj <- SEsubclass$new(target.obj=target.obj,
                          source.obj=source.obj,
                          confounders_sampling_name=confounders_sampling_name,
                          weighting_method = method)
  } else {
    message("to be continued...")
  }
  return(obj)
}
