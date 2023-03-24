#' @title Estimator for average treatment effect wrapper function
#' @description The function \code{Estimate} is used to estimate the average treatment effect obtained from \code{data}. If TEstimator is G_computation and outcome_method is BART, then make sure that treatment and outcome column is integer/numeric, no matter the outcome is binary or continuous.
#'
#' @param data A data frame containing variables in \code{vars_name}.
#' @param Estimator A character specifying an estimator for conditional average treatment effect. The allowed estimators for \code{TEstimator} are: \code{"G_computation"}, \code{"IPW"}, and \code{"DR"}. The corresponding object will be created by the wrapper function \code{TEstimator_wrapper()}. The default is \code{"G_computation"}, which, along with \code{outcome_method="glm"} model the potential outcomes.
#' @param data A data frame containing variables named in \code{vars_name} and possible other variables.
#' @param vars_name A list containing four vectors \code{confounders_treatment_name}, \code{treatment_name}, and \code{outcome_name}. \code{confounders_treatment_name} is a character vector containing the adjustment variables, which, along with \code{TEstimator} and the corresponding \code{outcome_method} or \code{treatment_method} to correct for confounding; \code{outcome_name} is a character vector of length one containing the variable name of outcome; \code{treatment_name} is a character vector of length one containing the variable name of treatment.
#' @param name A character indicating the name of the output object
#' @param outcome_method A character specifying a model for outcome. Possible values are found using \code{names(getModelInfo())}. See \url{http://topepo.github.io/caret/train-models-by-tag.html}. Default is "glm".
#' @param treatment_method A character specifying a model for treatment. Possible values are found using \code{names(getModelInfo())}. See \url{http://topepo.github.io/caret/train-models-by-tag.html}. Default is "glm".
#' @param two_models An optional logical indicating whether potential outcomes should be modeled separately when \code{TEstimator="DR"}. Default is \code{FALSE}.
#' @param outcome_formula An optional object of class \code{formula} describing the outcome model specification when \code{Estimator="G_computation"} or \code{Estimator="DR"}.
#' @param treatment_formula An optional object of class \code{formula} describing the treatment model specification when \code{Estimator="IPW"} or \code{Estimator="DR"}
#' @param data.public An optional logical indicating whether individual-level \code{data} is public in the output object. Default is TRUE.
#' @param isTrial An optional logical indicating whether the treatment assignment of \code{data} is random or unknown.
#' @param strata_cut An optional list containing lists. Each component is a list with tag named by a variable in \code{data} to discretize, containing \code{break} which is a vector specifying the interval of range of the variable to divide, \code{lable} which is a character vector specifying how to code value in the variable according to which interval they fall. The leftmost interval corresponds to level one, the next leftmost to level two and so on. This parameter is useful in the case we concern the integrated treatment effect conditioning on variables with multiple levels (for instance, continuous variable or ordinal variable with multiple levels). Note that we first model based on these continuous variables, then we discretize these variables according to \code{strata_cut}. The variables in \code{data} of the output object are discretized.
#' @param ... An optional argument passed to the private function \code{fit()} of each class for model training and tuning. See \url{https://topepo.github.io/caret/model-training-and-tuning.html} for details.
#' @return An object of class \code{TEstimator}.
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
  if ((Estimator == "G_computation") & (outcome_method == "psBART_impute")){
    if(is.factor(data[, vars_name$outcome_name])&(length(levels(data[,vars_name$outcome_name]))==2)) {
      message("you are classifiying using BART, outcome class should be numeric; we are converting the outcome to numeric!")
      data[,vars_name$outcome_name] <- as.numeric(as.character(data[,vars_name$outcome_name]))
    }
    message("you are classifiying using BART, outcome class should be numeric; we are converting the outcome to numeric!")
    data[,vars_name$outcome_name] <- as.numeric(as.character(data[,vars_name$outcome_name]))

    obj <- G_computation_psBART_impute$new(
      df = data,
      name = name,
      vars_name = vars_name,
      gc.method = outcome_method,
      gc.formula = outcome_formula,
      isTrial = isTrial,
      ...
    )} else if (Estimator == "G_computation") {
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
