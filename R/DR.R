#' @title R6 class: Doubly robust estimator class
#' @description A R6 class for doubly robust estimator that implements its own fit method.
#' @importFrom caret train
#' @export
DR <- R6::R6Class(
  "DR",
  inherit = DR_base,

  public = list(
    #-------------------------public fields-----------------------------#
    initialize = function(df, vars_name,outcome_method,outcome_formula,treatment_method,treatment_formula,two_models,...){
      super$initialize(df,vars_name)
      self$model_treatment <- private$fit_treatment(treatment_formula,treatment_method,...)
      self$model_outcome <- private$fit_outcome(outcome_formula,outcome_method,two_models,...)
      self$ps.est <- private$est_ps()
      self$po.est <- private$est_potentialOutcomes(two_models)
      self$data[,self$outcome_name] <- as.numeric(levels(self$data[,self$outcome_name])[self$data[,self$outcome_name]])
      ATE_SE <- private$est_ATE_SE()
      self$ATE_mean <- ATE_SE$est
      self$ATE_se <- ATE_SE$se
    }
  ),

  private = list(
    fit_treatment = function(treatment_formula,treatment_method,...){
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

    fit_outcome = function(outcome_formula, outcome_method,two_models,...){
      if(two_models){
        #browser()
        t.level <- unique(self$data[,self$treatment_name])
        level.order <- order(t.level)
        t0 <- t.level[match(1,level.order)]
        t1 <- t.level[match(2,level.order)]
        train.t0.id <- (self$data[,self$treatment_name]==t0)
        train.t1.id <- (self$data[,self$treatment_name]==t1)
        if(is.null(outcome_formula)){
          model.y1 <- caret::train(x = self$data[train.t1.id,self$confounders_internal_name],
                                   y = self$data[train.t1.id,self$outcome_name],
                                   method = outcome_method,
                                   ...)
          model.y0 <- caret::train(x=self$data[train.t0.id,self$confounders_internal_name],
                                   y=self$data[train.t0.id,self$outcome_name],
                                   method = outcome_method,
                                   ...)
        } else {
          model.y1 <- caret::train(form=outcome_formula,
                                   data=self$data[train.t1.id],
                                   method = outcome_method,
                                   ...)
          model.y0 <- caret::train(form=outcome_formula,
                                   data=self$data[train.t0.id],
                                   method = outcome_method,
                                   ...)
        }
        return(model=list(model.y1=model.y1, model.y0=model.y0))

      } else {
        if(is.null(outcome_formula)){
          model <- caret::train(x=self$data[,c(self$confounders_internal_name,self$treatment_name)],
                                y=self$data[,self$outcome_name],
                                method = outcome_method,
                                ...)
        } else {
          model <- caret::train(form=outcome_formula,
                                data=self$data,
                                method = outcome_method,
                                ...)
        }
        return(model)
      }
    },

    est_ps = function(){
      ps <- predict(self$model_treatment,newdata=self$data, type="prob")[,2]
      return(ps)
    },

    est_potentialOutcomes = function(two_models){
      #browser()
      data0 <- data1 <- self$data[,c(self$confounders_internal_name,self$treatment_name)]
      t.level <- unique(self$data[,self$treatment_name])
      level.order <- order(t.level)
      data0[,self$treatment_name] <- t.level[match(1,level.order)]
      data1[,self$treatment_name] <- t.level[match(2,level.order)]

      if(two_models){
        if(class(self$data[,self$outcome_name])=="numeric"){
          y1.hat <- predict(self$model_outcome$model.y1,newdata=data1)
          y0.hat <- predict(self$model_outcome$model.y0,newdata=data0)
        } else {
          y1.hat <- predict(self$model_outcome$model.y1,newdata=data1, type="prob")[,2]
          y0.hat <- predict(self$model_outcome$model.y0,newdata=data0, type="prob")[,2]
        }
      } else {
        if(class(self$data[,self$outcome_name])=="numeric"){
          y1.hat <- predict(self$model_outcome,newdata=data1)
          y0.hat <- predict(self$model_outcome,newdata=data0)
        } else {
          y1.hat <- predict(self$model_outcome,newdata=data1, type="prob")[,2]
          y0.hat <- predict(self$model_outcome,newdata=data0, type="prob")[,2]
        }
      }
      po.est <- data.frame("0" = y0.hat, "1" = y1.hat)
      return(po.est)
    }
  )
)
