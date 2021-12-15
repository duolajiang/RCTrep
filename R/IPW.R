#' @title R6 class: IPW class
#' @description A R6 class for IPW estimator for average treatment effect that implements its own fit method.
#' @importFrom caret train
#' @export
IPW <- R6::R6Class(
  "IPW",
  inherit = IPW_base,
  public = list(
    #-------------------------public fields-----------------------------#
    initialize = function(df, vars_name,treatment_method,treatment_formula,...){
      super$initialize(df,vars_name)
      self$model <- private$fit(treatment_formula,treatment_method,...)
      self$ps <- private$est_ps()
      ATE_SE <- private$est_ATE_SE()
      self$ATE_mean <- ATE_SE$est
      self$ATE_se <- ATE_SE$se
    }
  ),

  private = list(
    fit = function(treatment_formula,treatment_method,...){
      if(is.null(treatment_formula)){
        model <- caret::train(x=self$data[,c(self$confounders_internal_name)],
                              y=self$data[,self$treatment_name],
                              method = treatment_method,
                              ...)
      } else {
        model <- caret::train(form=treatment_formula,
                              data=self$data,
                              method = treatment_method,
                              ...)
      }
      return(model)
    },

    est_ps = function(){
      ps <- predict(self$model,newdata=self$data, type="prob")[,2]
      return(ps)
    }

  )
)
