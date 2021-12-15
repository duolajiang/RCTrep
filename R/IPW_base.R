#' @title R6 class: Inverse propensity score weighting estimator base class
#'
#' @description A base R6 class for inverse propensity score weighting estimator of average treatment effect that implements comment methods.
#'
#' @field model: a fitted model for treatment conditioning on covariates
#' @field method: a string specifying the method for fitting the propensity score
#' @field formula: an optional \code{formula} object. The formula is specified as \code{Z ~ X1+X2...}, where \code{Z} represents treatment variable and \code{X1} and \code{X2} are covariates.
#' @field ps: a numeric vector of length n where n is row number of \code{self$data}.
#'
#' @import PSweight
IPW_base <- R6::R6Class(
  "IPW_base",
  inherit = Estimator,
  public = list(
    #-------------------------public fields-----------------------------#
    model = NA,
    method = NA,
    formula = NA,
    ps = NA,

    initialize = function(df,vars_name){
      super$initialize(df,vars_name)
    }
  ),

  private = list(
    est_ATE_SE = function(){
      #browser()
      weight.obj <- PSweight::PSweight(ps.estimate = self$ps,weight = "IPW",
                                       data= self$data,
                                       yname = self$outcome_name, zname=self$treatment_name)
      res.obj <- summary(weight.obj, contrast=NULL, type="DIF", CI="TRUE")
      est <- res.obj$estimates[1]
      se <-  res.obj$estimates[2]
      return(list(est=est, se=se))
    },

    est_CATE_SE = function(index){
      weight.obj <- PSweight::PSweight(ps.estimate = self$ps[index],weight = "IPW",
                                       data= self$data[index,],
                                       yname = self$outcome_name, zname=self$treatment_name)
      res.obj <- summary(weight.obj, contrast=NULL, type="DIF", CI="TRUE")
      est <- res.obj$estimates[1]
      se <-  res.obj$estimates[2]
      return(list(est=est, se=se))
    },

    est_weighted_ATE_SE = function(weight){
      weight.obj <- PSweight.modified(ps.estimate = self$ps,weight = "IPW",
                                      data= self$data,
                                      yname = self$outcome_name, zname=self$treatment_name,
                                      weight.external = weight)
      res.obj <- summary(weight.obj, contrast=NULL, type="DIF", CI="TRUE")
      est <- res.obj$estimates[1]
      se <-  res.obj$estimates[2]
      return(list(est=est, se=se))
    },

    est_weighted_CATE_SE = function(index,weight){
      weight.obj <- PSweight.modified(ps.estimate = self$ps[index],weight = "IPW",
                                      data= self$data[index,],
                                      yname = self$outcome_name, zname=self$treatment_name,
                                      weight.external = weight)
      res.obj <- summary(weight.obj, contrast=NULL, type="DIF", CI="TRUE")
      est <- res.obj$estimates[1]
      se <-  res.obj$estimates[2]
      return(list(est=est, se=se))
    }

  )
)
