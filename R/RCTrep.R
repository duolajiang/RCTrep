#' @title Replicate treatment effect estimates obtained from a randomized control trial using real world data
#'
#' @description The function \code{RCTREP} is used to replicate the estimate of treatment effect from a target randomized control trial based on real-world data (RWD). This function estimate the treatment effect of RWD to ensure the internal validity of the estimates within the study population (namely, the observational data) and weight the resulting estimates to the target population (namely, the RCT) to enable external validity. The function currently implement the following types of estimators of treatment effect: G_computation, inverse propensity score weighting (IPW), and augmented propensity score weighting. The function implement the following two types of weighting estimators to generalize the resulting estimates of treatment effect from RWD to the target RCT: exact matching weights, and selection score weights. Since we regard the sample in the RCT as the target population, weights for each individual in RWD is \eqn{p/(1-p)} so that the weighted population of RWD is representative to the target population.
#'
#' @details An R6 object is constructed by a wrapper function \code{\link{TEstimator_wrapper}} and \code{\link{SEstimator_wrapper}} with user's input of data and estimators for treatment effect and weight. \code{TEstimator_wrapper()} returns initialized objects \code{source.obj} and \code{target.obj}. \code{TEstimator_wrapper()} replicates the target RCT estimates of \code{target.obj} via the class method \code{RCTrep()} with input of source object \code{source.obj}, target object \code{target.obj},and estimator of weights \code{SEstimator}.
#'
#'
#' @param TEstimator A character specifying an estimator for average treatment effect. The allowed estimators for \code{TEstimator} are: \code{"G_computation"}, \code{"IPW"}, and \code{"DR"}. The corresponding object will be created by the wrapper function \code{TEstimator_wrapper()}. The default is \code{"G_computation"}, which, along with \code{outcome_method="glm"} model the potential outcomes.
#' @param SEstimator A character specifying an estimator for weight. The allowed estimators are: \code{"Exact"}, \code{"Subclass"}, \code{"ISW"}. The default is \code{"Exact"}, which, implements the exact matching on variables in \code{confounders_sampling_name} to balance the population covariates between \code{source.data} and \code{target.data}.
#' @param source.data A data frame containing variables named in \code{vars_name} and possible other variables. \code{source.obj} is instantiated using \code{source.data}.
#' @param target.data A data frame containing variables named in \code{vars_name} and possible other variables. \code{target.obj} is instantiated using \code{target.data}.
#' @param source.name A character indicating the name of \code{source.obj}.
#' @param target.name A character indicating the name of \code{target.obj}.
#' @param vars_name A list containing four vectors \code{confounders_treatment_name}, \code{treatment_name}, and \code{outcome_name}. \code{confounders_treatment_name} is a character vector containing the adjustment variables, which, along with \code{TEstimator} and the corresponding \code{outcome_method} or \code{treatment_method} to correct for confounding; \code{outcome_name} is a character vector of length one containing the variable name of outcome; \code{treatment_name} is a character vector of length one containing the variable name of treatment.
#' @param confounders_sampling_name a character vector specifying variable names. The weights are estimated using the estimator specified in \code{SEstimator} based on the variables.
#' @param outcome_method,treatment_method,weighting_method A character specifying which model for outcome, treatment, and weight to use. Possible values are found using \code{names(getModelInfo())}. See \url{http://topepo.github.io/caret/train-models-by-tag.html}.
#' @param outcome_formula,treatment_formula,selection_formula An optional object of class \code{formula} describing the outcome model specification, treatment model specification, and selection model specification.
#' @param stratification An optional character vector containing variables to select  subgroups. \code{source.obj} will compute both weighted and unweighted average treatment effects of the subgroups, \code{targe.obj} will calculate the average treatment effects of the subgroups.
#' @param stratification_joint An optional logical indicating if the subgroups are selected based on levels of combined variables in \code{stratification} or levels of individual variable in \code{stratification}.
#' @param strata_cut_source An optional list containing lists. Each component is a list with tag named by a variable in \code{source.data} to discretize, containing \code{break} which is a vector specifying the interval of range of the variable to divide, \code{lable} which is a character vector specifying how to code value in the variable according to which interval they fall. The leftmost interval corresponds to level one, the next leftmost to level two and so on. This parameter is useful in the case we concern the integrated treatment effect conditioning on variables with multiple levels (for instance, continuous variable or ordinal variable with multiple levels). Note that we first model based on these continuous variables, then we discretize these variables according to \code{strata_cut}. The variables in \code{data} of \code{TEstimator} object are discretized, and the weight is calculated based on the discretized variables.
#' @param strata_cut_target An optional list containing lists. Each component is a list with tag named by a variable in \code{target.data} to discretize. The
#' @param two_models An optional logical indicating whether potential outcomes should be modeled separately when \code{TEstimator="DR"}. Default is \code{FALSE}.
#' @param data.public An optional logical indicating whether the \code{data} in the output objects are public. Default is \code{TRUE}.
#' @param ... An optional argument passed to \code{fit()} of each estimator object for model training and tuning. See \url{https://topepo.github.io/caret/model-training-and-tuning.html} for details.
#'
#' @returns A list of length three with three R6 class objects \code{source.obj}, \code{target.obj} and \code{source.rep.obj}
#'
#' @examples
#' output <- RCTREP(TEstimator = "G_computation", SEstimator = "Exact",
#'                  outcome_method = "BART",
#'                  source.data = RCTrep::source.data[sample(dim(RCTrep::source.data)[1],500),],
#'                  target.data = RCTrep::target.data[sample(dim(RCTrep::target.data)[1],500),],
#'                  vars_name = list(confounders_treatment_name =
#'                                     c("x1","x2","x3","x4","x5","x6"),
#'                                  treatment_name = c('z'),
#'                                  outcome_name = c('y')),
#'                  confounders_sampling_name = c("x2","x6"),
#'                  stratification = c("x1","x3","x4","x5"),
#'                  stratification_joint = TRUE)
#' output$target.obj
#' output$source.obj
#' output$source.rep.obj
#'
#' @return
#' @export
#' @import R6
RCTREP <- function(TEstimator = "G_computation", SEstimator = "Exact",
                   source.data = source.data, target.data = target.data,
                   source.name = "RWD", target.name = "RCT",
                   vars_name,
                   confounders_sampling_name,
                   outcome_method = "glm", treatment_method = "glm", weighting_method = "glm",
                   outcome_formula = NULL, treatment_formula = NULL, selection_formula = NULL,
                   stratification = NULL, stratification_joint = FALSE,
                   strata_cut_source = NULL, strata_cut_target = NULL,
                   two_models = FALSE,
                   data.public = TRUE,
                   ...) {

  source.obj <- TEstimator_wrapper(
    Estimator = TEstimator, data = source.data, vars_name = vars_name, name = source.name,
    outcome_method = outcome_method, treatment_method = treatment_method, two_models = two_models,
    outcome_formula = outcome_formula, treatment_formula = treatment_formula,
    data.public = data.public,
    strata_cut = strata_cut_source,
    ...
  )

  target.obj <- TEstimator_wrapper(
    Estimator = "Crude", data = target.data, vars_name = vars_name, name = target.name,
    outcome_method = outcome_method, treatment_method = treatment_method, two_models = two_models,
    outcome_formula = outcome_formula, treatment_formula = treatment_formula,
    data.public = data.public,
    isTrial = TRUE,
    strata_cut = strata_cut_target,
     ...
  )

  source.rep.obj <- SEstimator_wrapper(Estimator=SEstimator,
                                       target.obj=target.obj,
                                       source.obj=source.obj,
                                       confounders_sampling_name=confounders_sampling_name)
  source.rep.obj$EstimateRep(stratification = stratification,
                             stratification_joint = stratification_joint)
  target.obj$estimates$CATE <- target.obj$get_CATE(stratification, stratification_joint)

  return(list(
    source.obj = source.obj,
    target.obj = target.obj,
    source.rep.obj = source.rep.obj
  ))
}
