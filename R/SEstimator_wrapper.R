#' @export
SEstimator_wrapper <- function(estimator, target.obj, source.obj, confounders_sampling_name, method="glm", ...){
  if(estimator=="Exact"){
    obj <- SEexact$new(target.obj=target.obj,
                       source.obj=source.obj,
                       confounders_sampling_name=confounders_sampling_name)
  } else if(estimator=="ISW"){
    obj <- SEisw$new(target.obj=target.obj,
                     source.obj=source.obj,
                     confounders_sampling_name=confounders_sampling_name,
                     weighting_method = method,
                     ...)
  } else if(estimator=="Subclass"){
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
