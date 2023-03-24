#' @title R6 class which estimates the weighted conditional average treatment effects in \code{source.obj} based on input objects \code{source.obj} and \code{target.obj} of class \code{\link{TEstimator}}.
#'
#' @param Estimator a character specifying an estimator for weight. The allowed estimators are \code{Exact}, \code{ISW}, and \code{Subclass}.
#' @param target.obj,source.obj an instantiated object of class \code{TEstimator}.
#' @param confounders_sampling_name a character vector specifying the names of variables in \code{data} of \code{source.obj} and \code{target.obj}. Weights are estimated based on the variables.
#' @param method an optional character specifying a model for estimating sampling probability once \code{Estimator='ISW'} or \code{Estimator='Subclass'}.
#' @param sampling_formula an object of class \code{formula} specifying a model specification for sampling probability. Default value is NULL.
#' @param ... an optional argument specifying training and tuning for a model of sampling probability. See \url{https://topepo.github.io/caret/model-training-and-tuning.html} for details.
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
    # more allowable methods, see train() function method arguments of model lists in caret package.
    obj <- SEisw$new(target.obj=target.obj,
                     source.obj=source.obj,
                     confounders_sampling_name=confounders_sampling_name,
                     weighting_method = method,
                     sampling_formula = sampling_formula,
                     ...)
  } else if (Estimator=="Subclass"){
    # more allowable methods, see matchit() distance arguments in MatchIt package.
    obj <- SEsubclass$new(target.obj=target.obj,
                          source.obj=source.obj,
                          confounders_sampling_name=confounders_sampling_name,
                          weighting_method = method)
  } else {
    message("to be continued...")
  }
  return(obj)
}
