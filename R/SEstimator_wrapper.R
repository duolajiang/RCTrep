#' @export
SEstimator_wrapper <- function(Estimator, target.obj, source.obj, confounders_sampling_name, method="glm", ...){
  if(Estimator=="Exact"){
    obj <- SEexact$new(target.obj=target.obj,
                       source.obj=source.obj,
                       confounders_sampling_name=confounders_sampling_name)
  } else if(Estimator=="ISW"){
    obj <- SEisw$new(target.obj=target.obj,
                     source.obj=source.obj,
                     confounders_sampling_name=confounders_sampling_name,
                     weighting_method = method,
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
