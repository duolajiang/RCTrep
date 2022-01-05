#' @title Estimator for average treatment effect wrapper function
#'
#' @description The function \code{Estimate} is used to estimate the average treatment effect obtained from \code{data}.
#'
#' @param data A data frame containing variables in \code{vars_name}.
#' @inheritParams RCTREP
#'
#' @return An object of class \code{\link{Estimator}}.
#' @export
Estimator_wrapper <- function(Estimator, data, vars_name, name,
                              outcome_method = "glm", treatment_method = "glm", two_models = FALSE,
                              outcome_formula = NULL, treatment_formula = NULL,
                              data.public,
                              ...) {
  #browser()
  if (Estimator == "G_computation") {
    obj <- G_computation$new(
      df = data,
      name = name,
      vars_name = vars_name,
      gc.method = outcome_method,
      gc.form = outcome_formula,
      ...
    )
  } else if (Estimator == "IPW") {
    # browser()
    if (is.factor(data[, vars_name$outcome])) stop("outcome class is factor, please convert it to numeric!")
    obj <- IPW$new(
      df = data,
      vars_name = vars_name,
      name = name,
      treatment_method = treatment_method,
      treatment_formula = treatment_formula,
      ...
    )
  } else if (Estimator == "DR") {
    obj <- DR$new(
      df = data,
      vars_name = vars_name,
      name = name,
      outcome_method = outcome_method,
      outcome_formula = outcome_formula,
      treatment_method = treatment_method,
      treatment_formula = treatment_formula,
      two_models = two_models,
      ...
    )
  } else if (Estimator == "Crude") {

  } else {

  }

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


  #
  #   obj$estimates$CATE<- obj$get_CATE(stratification, stratification_joint)
  # }

  if(!data.public){
    obj <- SummaryDecorater$new(obj)
  }

  return(obj)
}
