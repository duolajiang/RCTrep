#' @title Estimator for average treatment effect wrapper function
#'
#' @description The function \code{Estimate} is used to estimate the average treatment effect obtained from \code{data}. If TEstimator is G_computation and outcome_method is BART, then make sure that treatment and outcome column is integer/numeric, no matter the outcome is binary or continuous.
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
                               strata_cut = NULL,
                               ...) {
  #browser()
  mcall <- match.call()
  if (Estimator == "G_computation") {
    if(outcome_method %in% c("BART","psBART")){
      class.confounders <- lapply(data[,vars_name$confounders_treatment_name], class)
      if('character' %in% class.confounders) stop("You are using BART, character must be converted to factor!")
      for (variable in vars_name$confounder_treatment_name) {
        empty_level <- data %>% count(across(variable)) %>% summarise(empty_level=any(n==0))
        if(empty_level$empty_level==TRUE) stop(glue::glue("{variable} has empty level! Please drop the empty level!"))
      }
    }

    if ((!is.factor(data[, vars_name$outcome_name]))&(outcome_method != "BART")&(length(unique(data[, vars_name$outcome_name]))==2)) {
      message("you are classifiying, but outcome class is numeric, we are converting the outcome to factor!")
      data[,vars_name$outcome_name] <- as.factor(data[,vars_name$outcome_name])
    }
    if(((outcome_method=="BART")|(outcome_method=="psBART"))&
       (is.factor(data[, vars_name$outcome_name]))&
       (length(unique(data[, vars_name$outcome_name]))==2)) {
      message("you are classifiying using BART, outcome class should be numeric; we are converting the outcome to numeric!")
      data[,vars_name$outcome_name] <- as.numeric(as.character(data[,vars_name$outcome_name]))
    }

    if (outcome_method=="BART"){
      obj <- G_computation_BART$new(
        df = data,
        name = name,
        vars_name = vars_name,
        gc.method = outcome_method,
        gc.formula = outcome_formula,
        isTrial = isTrial,
        ...
      )
    } else if(outcome_method=="psBART"){
      obj <- G_computation_psBART$new(
        df = data,
        name = name,
        vars_name = vars_name,
        gc.method = outcome_method,
        gc.formula = outcome_formula,
        isTrial = isTrial,
        ...
      )
    } else {
      obj <- G_computation$new(
        df = data,
        name = name,
        vars_name = vars_name,
        gc.method = outcome_method,
        gc.formula = outcome_formula,
        isTrial = isTrial,
        ...
      )
    }
  } else if (Estimator == "IPW") {
    #browser()
    if (is.factor(data[, vars_name$outcome_name])) {
      message("outcome class is factor, we are converting it to numeric")
      data[, vars_name$outcome_name] <- as.numeric(as.character(data[, vars_name$outcome_name]))
    }
    if (!is.factor(data[, vars_name$treatment_name]) & (treatment_method!="BART")) {
      message("treatment class is numeric, we are converting it to factor")
      data[, vars_name$treatment_name] <- as.factor(data[, vars_name$treatment_name])
    }
    if(is.factor(data[, vars_name$treatment_name]) & (treatment_method=="BART")) {
      message("treatment class is factor, BART needs numeric value, we are converting it to numeric")
      data[, vars_name$treatment_name] <- is.numeric(is.character(data[, vars_name$treatment_name]))
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
    #browser()
    if (!is.factor(data[, vars_name$outcome_name]) & (outcome_method!='BART') & (length(unique(data[, vars_name$outcome_name]))==2)) {
      message("binary outcome, class is numeric, outcome method is not BART. We are converting it to factor")
      data[, vars_name$outcome_name] <- as.factor(data[, vars_name$outcome_name])
    }
    if (!is.factor(data[, vars_name$treatment_name]) & (treatment_method != 'BART')) {
      message("treatment class is numeric, we are converting it to factor")
      data[, vars_name$treatment_name] <- as.factor(data[, vars_name$treatment_name])
    }
    if(is.factor(data[, vars_name$treatment_name]) & (treatment_method=='BART')) {
      message("your treatment class is factor for BART outcome modeling, we are converting it to numeric")
      data[, vars_name$treatment_name] <- as.numeric(as.character(data[, vars_name$treatment_name]))
    }
    if(is.factor(data[, vars_name$outcome_name]) & (outcome_method=='BART'))
      {
      message("your outcome class is factor for BART outcome modeling, we are converting it to numeric")
      data[, vars_name$outcome_name] <- as.numeric(as.character(data[, vars_name$outcome_name]))
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

  #browser()
  if (!is.null(strata_cut)) {
     for (i in 1:length(strata_cut)) {
       strata_cut_var_name <- names(strata_cut[i])
       obj$data[, strata_cut_var_name] <- as.character(cut(obj$data[, strata_cut_var_name],
                                                           breaks = strata_cut[[i]]$breaks,
                                                           labels = strata_cut[[i]]$labels,
                                                           include.lowest = TRUE,
                                                           ordered_result = TRUE
                                                           ))
     }
    obj$estimates$CATE<- obj$get_CATE(stratification = obj$.__enclos_env__$private$confounders_treatment_name,
                                      stratification_joint = TRUE)
   }


  #browser()
  if(!data.public){
    obj <- TEstimator_pp$new(obj)
  }

  return(obj)
}
