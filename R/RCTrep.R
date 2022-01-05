#' @title Replicate treatment effect estimates obtained from a randomized control trial using real world data
#'
#' @description The function \code{RCTREP} is used to replicate the estimate of treatment effect from a target randomized control trial based on real-world data (RWD). This function estimate the treatment effect of RWD to ensure the internal validity of the estimates within the study population (namely, the observational data) and weight the resulting estimates to the target population (namely, the RCT) to enable external validity. The function currently implement the following types of estimators of treatment effect: G_computation, inverse propensity score weighting (IPW), and augmented propensity score weighting. The function implement the following two types of weighting estimators to generalize the resulting estimates of treatment effect from RWD to the target RCT: exact matching weights, and selection score weights. Since we regard the sample in the RCT as the target population, weights for each individual in RWD is \eqn{p/(1-p)} so that the weighted population of RWD is representative to the target population.
#'
#' @details An R6 object for both studies are constructed by a wrapper function \code{Estimate()} with user's input of data and estimator for treatment effect. Then \code{Estimate()} return initialized objects \code{source.obj} and \code{target.obj}. \code{source.obj} replicates the target RCT estimate via the class method \code{RCTrep()} with input of target object \code{target.obj} and weighting method.
#'
#' @param Estimator A character specifying an estimator for average treatment effect. The allowed estimators for \code{Estimator} are: \code{"G_computation"}, \code{"IPW"}, and \code{"DR"}. The corresponding object will be created by the wrapper function \code{\link{Estimate()}}. The default is \code{"G_computation"}, which, along with \code{outcome_method="glm"} model the potential outcomes.
#' @param weighting_estimator A character specifying a weighting estimator for generalizing/transporting the estimates of \code{source.obj} (initiated using RWD) to \code{target.obj} (initiated using RCT) as to enable replication/comparison between source study (RWD) and target study(RCT). The allowed estimators are: \code{"balancing"}, and \code{"modeling"}. \code{"balancing"} estimator use exact matching to compute the weight; \code{"modeling"} estimator model the probability of being selected to the target RCT (assuming combing target RCT and the source RWD as a population). The default is \code{"Balancing"}, which, implements the exact matching on variables in \code{vars_name$external_confounder} to balance the population covariates between \code{source.data} and \code{target.data}
#' @param source.data A data frame containing variables named in \code{vars_name} and possible other variables. If not found in \code{source.data}, the function will stop and throw error; vectors of binary treatment and binary outcome should be factor.
#' @param target.data A data frame containing variables named in \code{vars_name} and possible other variables, or a list of four components with four tags \code{ATE_mean}, \code{ATE_se}, \code{CATE_mean_se}, and \code{univariate_p} reference four components with data type numeric, numeric, data.frame, and list respectively. \code{ATE_mean} is a numeric vector of length 1 containing the point estimate of the treatment effect in \code{target.data}, \code{ATE_se} is a numeric vector of length 1 containing the standard error the treatment effect, \code{CATE_mean_se} is a data frame containing five vectors \code{name}, \code{value}, \code{cate}, \code{se}, and \code{size} of length \eqn{N}, where \code{name} is variables that divide the target population into smaller groups, \code{value} is levels of variables in \code{name}, \code{cate} is the provided conditional average treatment effect of a subgroup defined by a variable in \code{name} with the corresponding level in \code{value}, \code{se} is the standard error of the \code{cate}, \code{size} is the group size, \eqn{N} is the number of stratum based on variables in the vector \code{name} and levels in \code{value}. \code{univariate_p} is a list of length equal to the number of variables to divide the population with tags equal to variable names, each component containing a vector, which, each containing the name of a variable, number of levels of the variable, levels of the variable, and the distribution of each level by order.
#' @param vars_name A list containing four vectors \code{confounders_internal}, \code{confounders_external}, \code{treatment_name}, and \code{outcome_name}. \code{confounders_internal} is a character vector containing the adjustment variables, which, along with \code{Estimator} and the corresponding \code{outcome_method} or \code{treatment_method} to correct for confounding; \code{confounders_external} is a character vector containing variables for weighting as to generalize estimates from \code{source.data} to \code{target.data}; \code{outcome_name} is a character vector of length one containing the variable name of outcome; \code{treatment_name} is a character vector of length one containing the variable name of treatment.
#' @param outcome_method,treatment_method,weighting_method A string specifying which model for outcome, treatment, and selection to use. Possible values are found using \code{names(getModelInfo())}. See \url{http://topepo.github.io/caret/train-models-by-tag.html}. A list of functions can also be passed for a custom model function. See \url{http://topepo.github.io/caret/using-your-own-model-in-train.html} for details.
#' @param outcome_formula,treatment_formula,selection_formula An optional object of class \code{formula} describing the outcome model, treatment model, and selection model.
#' @param stratification An optional string vector containing variables to define subgroups. \code{source.obj} will compute both weighted and unweighted conditional average treatment effect, \code{targe.obj} will calculate the conditional average treatment effect based on these variables.
#' @param stratification_joint An optional logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification} when \code{stratification} is specified.
#' @param strata_cut An optional list containing lists. Each component is a list with tag named by a variable to discretize, containing \code{break} which is a vector specifying the interval of range of the variable to divide, \code{lable} which is a string vector specifying how to code value in the variable according to which interval they fall. The leftmost interval corresponds to level one, the next leftmost to level two and so on. This parameter is useful in the case we concern the integrated treatment effect conditioning on variables with multiple levels (for instance, continuous variable or ordinal variable with multiple levels). Note that we first model based on these continuous variables, then we discretize these variables according to \code{strata_cut} by modifying the public variable \code{data} of \code{Estimator} object in wrap function \code{\link{Estimate}}, and calculate the weight for generalization based on the discretized variables.
#' @param two_models An optional logical indicating whether potential outcomes should be modeled separately when \code{Estimator="DR"}. Default is FALSE.
#' @param ... An optional arguments passed to \code{fit()} of each estimator object for model training and tuning. See \url{https://topepo.github.io/caret/model-training-and-tuning.html} for details.
#'
#' @return A list of length two with two R6 class objects \code{source.obj=source.obj} and \code{target.obj=target.obj}.
#'
#' @examples
#' library(RCTrep)
#' source.data <- RCTrep::source.data
#' target.data <- RCT::target.data
#' Estimator <- "IPW"
#' strata <- c("Stage2", "pT")
#' strata_joint <- TRUE
#' vars_name <- list(
#'   confounders_internal = c("Stage2", "age", "pT"),
#'   confounders_external = c("Stage2", "age", "pT"),
#'   treatment_name = c("combined_chemo"),
#'   outcome_name = c("vitstat")
#' )
#' outcome_form <- vitstat ~ Stage2 + age + combined_chemo + pT +
#'   Stage2:combined_chemo + age:combined_chemo + pT:combined_chemo + pT:Stage2:combined_chemo
#' strata_cut <- list(age = list(
#'   breaks = c(
#'     min(data$age),
#'     50, 60, 70, max(data$age)
#'   ),
#'   labels = c(1, 2, 3, 4)
#' ))
#' \dontrun{
#' output <- RCTREP(
#'   Estimator = "G_computation", two_models = FALSE,
#'   source.data = source.data, target.data = target.data,
#'   vars_name = vars_name,
#'   outcome_formula = outcome_form,
#'   stratification = strata, stratification_joint = TRUE, strata_cut = strata_cut
#' )
#' output$source.obj
#' output$target.obj
#' }
#'
#' @return
#' @export
#' @import R6
RCTREP <- function(Estimator = "G_computation", weighting_estimator = "Balancing",
                   source.data = source.data, target.data = target.data,
                   source.name = "source", target.name = "target",
                   vars_name,
                   outcome_method = "glm", treatment_method = "glm", weighting_method = "glm",
                   outcome_formula = NULL, treatment_formula = NULL, selection_formula = NULL,
                   stratification = NULL, stratification_joint = FALSE,
                   strata_cut_source = NULL, strata_cut_target = NULL,
                   two_models = FALSE,
                   data.public = FALSE,
                   ...) {

  #browser()
  source.obj <- Estimator_wrapper(
    Estimator = Estimator, data = source.data, vars_name = vars_name, name = source.name,
    outcome_method = outcome_method, treatment_method = treatment_method, two_models = two_models,
    outcome_formula = outcome_formula, treatment_formula = treatment_formula,
    data.public = data.public,
    ...
  )

  #browser()
  if (class(target.data) == "list") {
    #browser()
    synthetic.data <- GenerateSyntheticData(dim(source.data)[1],
                                            target.data,
                                            unique(target.data$CATE_mean_se$name))
    target.obj <- Estimator$new(df=synthetic.data, vars_name = vars_name, name=target.name)
    target.obj$estimates$ATE$est <- target.data$ATE_mean
    target.obj$estimates$ATE$se <- target.data$ATE_se
    target.obj$estimates$CATE <- target.data$CATE_mean_se
  } else {
    target.obj <- Estimator_wrapper(
      Estimator = Estimator, data = target.data, vars_name = vars_name, name = target.name,
      outcome_method = outcome_method, treatment_method = treatment_method, two_models = two_models,
      outcome_formula = outcome_formula, treatment_formula = treatment_formula,
      data.public = data.public,
      ...
    )
  }

  source.obj$EstimateRep(target.obj,
                         weighting_estimator, weighting_method,
                         stratification, stratification_joint)
  target.obj$estimates$CATE <- target.obj$get_CATE(stratification, stratification_joint)

  return(list(
    source.obj = source.obj,
    target.obj = target.obj
  ))
}
