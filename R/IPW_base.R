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
