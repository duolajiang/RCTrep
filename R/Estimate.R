#' @title Estimate average treatment effect
#'
#' @description The function \code{Estimate} is used to estimate the average treatment effect obtained from \code{data}.
#'
#' @param data A data frame containing variables in \code{vars_name}.
#' @inheritParams RCTREP
#'
#' @return An object of class \code{\link{Estimator}}.
#' @export
Estimate <- function(Estimator, data, vars_name,
                     outcome_method="glm", treatment_method="glm", two_models=NULL,
                     outcome_formula, treatment_formula,
                     stratification, stratification_joint, strata_cut,
                     ...){
  #browser()
  if(Estimator=="G_computation"){
    obj <- G_computation$new(df=data,
                             vars_name = vars_name,
                             gc.method = outcome_method,
                             gc.form = outcome_formula,
                             ...)

  } else if (Estimator == "IPW"){
    #browser()
    if(is.factor(data[,vars_name$outcome])) stop("outcome class is factor, please convert it to numeric!")
    obj <- IPW$new(df=data,
                   vars_name = vars_name,
                   treatment_method = treatment_method,
                   treatment_formula = treatment_formula,
                   ...)

  } else if(Estimator == "DR"){
    obj <- DR$new(df=data,
                  vars_name = vars_name,
                  outcome_method=outcome_method,
                  outcome_formula=outcome_formula,
                  treatment_method=treatment_method,
                  treatment_formula=treatment_formula,
                  two_models=two_models,
                  ...)

  } else if(Estimator == "Crude"){

  } else {

  }

  if(!is.null(stratification)){
    #browser()
    if(!is.null(strata_cut)){
      for (i in 1:length(strata_cut)) {
        strata_cut_var_name <- names(strata_cut[i])
        obj$data[,strata_cut_var_name] <- as.character(cut(obj$data[,strata_cut_var_name],
                                                           breaks = strata_cut[[i]]$breaks,
                                                           labels = strata_cut[[i]]$labels,
                                                           include.lowest = TRUE,
                                                           ordered_result = TRUE))
      }
    }

    obj$CATE_mean_se <- obj$get_CATE(stratification,stratification_joint)
  }

  return(obj)
}

