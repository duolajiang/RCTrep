#' @export
SEstimator_wrapper <- function(Estimator, target.obj, source.obj,
                               confounders_sampling_name,
                               method="glm",
                               sampling_formula = NULL, ...){
  if(Estimator=="Exact"){
    obj <- SEexact$new(target.obj=target.obj,
                       source.obj=source.obj,
                       confounders_sampling_name=confounders_sampling_name)
  } else if(Estimator=="ISW"){
    obj <- SEisw$new(target.obj=target.obj,
                     source.obj=source.obj,
                     confounders_sampling_name=confounders_sampling_name,
                     weighting_method = method,
                     sampling_formula = sampling_formula,
                     ...)
  } else if(Estimator=="Subclass"){
    obj <- SEsubclass$new(target.obj=target.obj,
                     source.obj=source.obj,
                     confounders_sampling_name=confounders_sampling_name,
                     ...)
  } else {
    obj <- SEexact_pp$new(target.obj=target.obj,
                              source.obj=source.obj,
                              confounders_sampling_name=confounders_sampling_name)
  }
  return(obj)
}
