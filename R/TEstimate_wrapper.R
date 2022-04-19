#' @title Estimator for average treatment effect wrapper function
#'
#' @description The function \code{Estimate} is used to estimate the average treatment effect obtained from \code{data}.
#'
#' @param data A data frame containing variables in \code{vars_name}.
#' @inheritParams RCTREP
#'
#' @return An object of class \code{\link{Estimator}}.
#' @export
TEstimator_wrapper <- function(Estimator, data, vars_name, name="",
                               outcome_method = "glm", treatment_method = "glm", two_models = FALSE,
                               outcome_formula = NULL, treatment_formula = NULL,
                               data.public=TRUE,
                               isTrial = FALSE,
                               #strata_cut = strata_cut,
                               ...) {
  #browser()
  mcall <- match.call()
  if (Estimator == "G_computation") {
    if ((!is.factor(data[, vars_name$outcome_name]))&(length(unique(data[, vars_name$outcome_name]))==2)) {
      message("you are classifiying, but outcome class is numeric, we are converting the outcome to factor!")
      data[,vars_name$outcome_name] <- as.factor(data[,vars_name$outcome_name])
      }
    obj <- G_computation$new(
      df = data,
      name = name,
      vars_name = vars_name,
      gc.method = outcome_method,
      gc.formula = outcome_formula,
      isTrial = isTrial,
      ...
    )
  } else if (Estimator == "IPW") {
    #browser()
    if (is.factor(data[, vars_name$outcome_name])) {
      message("outcome class is factor, we are converting it to numeric")
      data[, vars_name$outcome_name] <- as.numeric(as.character(data[, vars_name$outcome_name]))
    }
    if (!is.factor(data[, vars_name$treatment_name])) {
      message("treatment class is numeric, we are converting it to factor")
      data[, vars_name$treatment_name] <- as.factor(data[, vars_name$treatment_name])
    }
    obj <- IPW$new(
      df = data,
      vars_name = vars_name,
      name = name,
      treatment_method = treatment_method,
      treatment_formula = treatment_formula,
      isTrial = isTrial,
      ...
    )
  } else if (Estimator == "DR") {
    if (!is.factor(data[, vars_name$outcome_name])) {
      message("outcome class is numeric, we are converting it to factor")
      data[, vars_name$outcome_name] <- as.factor(data[, vars_name$outcome_name])
    }
    if (!is.factor(data[, vars_name$treatment_name])) {
      message("treatment class is numeric, we are converting it to factor")
      data[, vars_name$treatment_name] <- as.factor(data[, vars_name$treatment_name])
    }
    obj <- DR$new(
      df = data,
      vars_name = vars_name,
      name = name,
      outcome_method = outcome_method,
      outcome_formula = outcome_formula,
      treatment_method = treatment_method,
      treatment_formula = treatment_formula,
      two_models = two_models,
      isTrial = isTrial,
      ...
    )
  } else if (Estimator == "Crude") {
    if(is.factor(data[, vars_name$outcome_name])) {
      data[,vars_name$outcome_name] <- as.numeric(as.character(data[,vars_name$outcome_name]))
    }
    obj <- Crude$new(
      df = data,
      vars_name = vars_name,
      name = name,
      isTrial = isTrial
    )
  } else {}

  # browser()
  # if (!is.null(strata_cut)) {
  #   for (i in 1:length(strata_cut)) {
  #     strata_cut_var_name <- names(strata_cut[i])
  #     obj$data[, strata_cut_var_name] <- as.character(cut(obj$data[, strata_cut_var_name],
  #                                                         breaks = strata_cut[[i]]$breaks,
  #                                                         labels = strata_cut[[i]]$labels,
  #                                                         include.lowest = TRUE,
  #                                                         ordered_result = TRUE
  #                                                         ))
  #     }
  # }

  #obj$estimates$CATE<- obj$get_CATE(stratification, stratification_joint)

  #browser()
  if(!data.public){
    obj <- TEstimator_pp$new(obj)
  }

  return(obj)
}