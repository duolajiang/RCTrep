#' @title R6 class: Doubly robust estimator base class
#' @description A base R6 class for doubly robust estimator of average treatment effect that implements comment methods.
#' @importfrom PSweight PSweight
#' @export
DR <- R6::R6Class(
  "DR",
  inherit = TEstimator,
  public = list(

    ps.est = NA,

    po.est = data.frame(),

    id = "DR",
    #-------------------------public fields-----------------------------#
    initialize = function(df, vars_name, name,
                          treatment_method, treatment_formula,
                          outcome_method, outcome_formula,two_models,isTrial, ...) {
      super$initialize(df, vars_name, name)
      #browser()
      self$data[, private$outcome_name] <- as.numeric(levels(self$data[, private$outcome_name])[self$data[, private$outcome_name]])
      private$treatment_method <- treatment_method
      private$treatment_formula <- treatment_formula
      private$outcome_method <- outcome_method
      private$outcome_formula <- outcome_formula
      self$model$treatment <- private$fit_treatment(...)
      self$model$outcome <- private$fit_outcome(two_models, ...)
      self$ps.est <- private$est_ps()
      self$po.est <- private$est_potentialOutcomes(two_models)
      private$set_ATE()
      private$set_CATE(private$confounders_treatment_name,TRUE)
      private$isTrial <- isTrial
    }
  ),

  # when use PSweight, outcome data should be numeric!
  private = list(
    treatment_method = NULL,
    outcome_method = NULL,
    treatment_formula = NULL,
    outcome_formula = NULL,

    est_ATE_SE = function(index) {
      ngrp <- length(unique(self$data[index,private$treatment_name]))
      if(ngrp<2){
        warning("no overlap in this group! Variance is unbounded")
        se <- Inf
        y1.hat <- DR.estimator(z=self$data[index,private$treatment_name],
                               y=self$data[index,private$outcome_name],
                               y.hat=self$po.est[index,"y1.hat"],
                               ps=self$ps.est[index],
                               w=rep(1,length(index)),t=1)
        y0.hat <- DR.estimator(z=self$data[index,private$treatment_name],
                               y=self$data[index,private$outcome_name],
                               y.hat=self$po.est[index,"y0.hat"],
                               ps=self$ps.est[index],
                               w=rep(1,length(index)),t=0)
        est <- y1.hat-y0.hat
      } else {
        weight.obj <- PSweight::PSweight(
          ps.estimate = self$ps.est[index], weight = "IPW",
          data = self$data[index, ],
          yname = private$outcome_name,
          augmentation = TRUE, out.estimate = self$po.est[index, ],
          zname = private$treatment_name
        )
        res.obj <- summary(weight.obj, contrast = NULL, type = "DIF", CI = "TRUE")
        est <- res.obj$estimates[1]
        se <- res.obj$estimates[2]
        y1.hat <- weight.obj$muhat[which(weight.obj$group==1)]
        y0.hat <- weight.obj$muhat[which(weight.obj$group==0)]
      }
      return(list(y1.hat = y1.hat, y0.hat = y0.hat, est = est, se = se))
    },

    est_weighted_ATE_SE = function(index, weight) {
      ngrp <- length(unique(self$data[index,private$treatment_name]))
      if(ngrp<2){
        warning("no overlap in this group! Variance is unbounded")
        se <- Inf
        y1.hat <- DR.estimator(z=self$data[index,private$treatment_name],
                               y=self$data[index,private$outcome_name],
                               y.hat=self$po.est[index,"y1.hat"],
                               ps=self$ps.est[index],
                               w=weight,t=1)
        y0.hat <- DR.estimator(z=self$data[index,private$treatment_name],
                               y=self$data[index,private$outcome_name],
                               y.hat=self$po.est[index,"y0.hat"],
                               ps=self$ps.est[index],
                               w=weight,t=0)
        est <- y1.hat-y0.hat
      } else {
        weight.obj <- PSweight.modified(
          ps.estimate = self$ps.est[index], weight = "IPW",
          data = self$data[index, ],
          yname = private$outcome_name, zname = private$treatment_name,
          weight.external = weight,
          out.est = self$po.est[index, ]
        )
        res.obj <- summary(weight.obj, contrast = NULL, type = "DIF", CI = "TRUE")
        est <- res.obj$estimates[1]
        se <- res.obj$estimates[2]
        y1.hat <- weight.obj$muhat[which(weight.obj$group==1)]
        y0.hat <- weight.obj$muhat[which(weight.obj$group==0)]
      }

      return(list(y1.hat = y1.hat, y0.hat = y0.hat, est = est, se = se))
    },

    fit_treatment = function(...) {
      #browser()
      if (is.null(private$treatment_formula)) {
        model <- caret::train(
          x = self$data[, private$confounders_treatment_name],
          y = self$data[, private$treatment_name],
          method = private$treatment_method,
          ...
        )
      } else {
        model <- caret::train(
          form = private$treatment_formula,
          data = self$data,
          method = private$treatment_method,
          ...
        )
      }
      return(model)
    },

    fit_outcome = function(two_models, ...) {
      if (two_models) {
        # browser()
        t.level <- unique(self$data[, private$treatment_name])
        level.order <- order(t.level)
        t0 <- t.level[match(1, level.order)]
        t1 <- t.level[match(2, level.order)]
        train.t0.id <- (self$data[, private$treatment_name] == t0)
        train.t1.id <- (self$data[, private$treatment_name] == t1)
        if (is.null(private$outcome_formula)) {
          model.y1 <- caret::train(
            x = self$data[train.t1.id, private$confounders_treatment_name],
            y = self$data[train.t1.id, private$outcome_name],
            method = private$outcome_method,
            ...
          )
          model.y0 <- caret::train(
            x = self$data[train.t0.id, private$confounders_treatment_name],
            y = self$data[train.t0.id, private$outcome_name],
            method = private$outcome_method,
            ...
          )
        } else {
          model.y1 <- caret::train(
            form = private$outcome_formula,
            data = self$data[train.t1.id],
            method = private$outcome_method,
            ...
          )
          model.y0 <- caret::train(
            form = private$outcome_formula,
            data = self$data[train.t0.id],
            method = private$outcome_method,
            ...
          )
        }
        return(model = list(model.y1 = model.y1, model.y0 = model.y0))
      } else {
        if (is.null(private$outcome_formula)) {
          model <- caret::train(
            x = self$data[, c(private$confounders_treatment_name, private$treatment_name)],
            y = self$data[, private$outcome_name],
            method = private$outcome_method,
            ...
          )
        } else {
          model <- caret::train(
            form = private$outcome_formula,
            data = self$data,
            method = private$outcome_method,
            ...
          )
        }
        return(model)
      }
    },

    est_ps = function() {
      ps <- predict(self$model$treatment, newdata = self$data, type = "prob")[, 2]
      return(ps)
    },

    est_potentialOutcomes = function(two_models) {
      # browser()
      data0 <- data1 <- self$data[, c(private$confounders_treatment_name, private$treatment_name)]
      t.level <- unique(self$data[, private$treatment_name])
      level.order <- order(t.level)
      data0[, private$treatment_name] <- t.level[match(1, level.order)]
      data1[, private$treatment_name] <- t.level[match(2, level.order)]

      if (two_models) {
        if (class(self$data[, private$outcome_name]) == "numeric") {
          y1.hat <- predict(self$model$outcome$model.y1, newdata = data1)
          y0.hat <- predict(self$model$outcome$model.y0, newdata = data0)
        } else {
          y1.hat <- predict(self$model$outcome$model.y1, newdata = data1, type = "prob")[, 2]
          y0.hat <- predict(self$model$outcome$model.y0, newdata = data0, type = "prob")[, 2]
        }
      } else {
        if (class(self$data[, private$outcome_name]) == "numeric") {
          y1.hat <- predict(self$model$outcome, newdata = data1)
          y0.hat <- predict(self$model$outcome, newdata = data0)
        } else {
          y1.hat <- predict(self$model$outcome, newdata = data1, type = "prob")[, 2]
          y0.hat <- predict(self$model$outcome, newdata = data0, type = "prob")[, 2]
        }
      }
      po.est <- data.frame("0"= y0.hat, "1"=y1.hat)
      return(po.est)
    }
  )
)
