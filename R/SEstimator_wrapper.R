#' @title Estimating the weighted conditional average treatment effects in \code{source.obj} based on input objects \code{source.obj} and \code{target.obj} of class \code{TEstimator}.
#'
#' @param Estimator a character specifying an estimator for weight. The allowed estimators are \code{"Exact"}, \code{"ISW"}, and \code{"Subclass"}.
#' @param target.obj,source.obj an instantiated object of class \code{TEstimator}.
#' @param confounders_sampling_name a character vector specifying the names of variables in \code{data} of \code{source.obj} and \code{target.obj}. Weights are estimated based on the variables.
#' @param method an optional character specifying a model for estimating sampling probability when \code{Estimator='ISW'} or \code{Estimator='Subclass'}.
#' @param sampling_formula an object of class \code{formula} specifying a model specification for sampling probability. Default value is \code{NULL}.
#' @param ... an optional argument specifying training and tuning for a model of sampling probability. See \url{https://topepo.github.io/caret/model-training-and-tuning.html} for details.
#' @return An object of class \code{SEstimator}
#' @export
#'
#' @examples
#' \dontrun{
#' source.data <- RCTrep::source.data[sample(dim(RCTrep::source.data)[1],500),]
#' target.data <- RCTrep::target.data[sample(dim(RCTrep::target.data)[1],500),]
#'
#' vars_name <- list(confounders_treatment_name = c("x1","x2","x3","x4","x5","x6"),
#'                   treatment_name = c('z'),
#'                   outcome_name = c('y'))
#'
#' target.obj <- TEstimator_wrapper(
#'   Estimator = "Crude",
#'   data = target.data,
#'   vars_name = vars_name,
#'   name = "RCT",
#'   data.public = FALSE,
#'   isTrial = TRUE)
#'
#' source.obj <- TEstimator_wrapper(
#'   Estimator = "G_computation",
#'   data = source.data,
#'   vars_name = vars_name,
#'   outcome_method = "glm",
#'   outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
#'   name = "RWD",
#'   data.public = TRUE)
#'
#' source.rep.obj <- SEstimator_wrapper(Estimator="Exact",
#'                                      target.obj=target.obj,
#'                                      source.obj=source.obj,
#'                                      confounders_sampling_name=c("x2","x6"))
#' source.rep.obj$EstimateRep(stratification = c("x1","x3","x4","x5"),
#'                            stratification_joint = TRUE)
#'
#' }
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
