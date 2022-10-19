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
      #self$data[, private$outcome_name] <- as.numeric(levels(self$data[, private$outcome_name])[self$data[, private$outcome_name]])
      private$treatment_method <- treatment_method
      private$treatment_formula <- treatment_formula
      private$outcome_method <- outcome_method
      private$outcome_formula <- outcome_formula
      private$confounders_treatment_factor <-
        private$confounders_treatment_name[sapply(self$data[,private$confounders_treatment_name],is.factor)]

      if(outcome_method == "BART"){
        self$model$outcome <- private$fit_outcome_BART(two_models, ...)
        self$po.est <- private$est_potentialOutcomes_BART(two_models)
      } else{
        self$model$outcome <- private$fit_outcome(two_models, ...)
        self$po.est <- private$est_potentialOutcomes(two_models)
      }

      if(treatment_method == "BART"){
        model_ps <- private$fit_treatment_BART(...)
        self$model$treatment <- model_ps$model
        self$ps.est <- model_ps$ps
      } else{
        self$model$treatment <- private$fit_treatment(...)
        self$ps.est <- private$est_ps()
      }

      if(is.factor(self$data[, private$outcome_name])){
        self$data[,private$outcome_name] <- as.numeric(as.character(self$data[,private$outcome_name]))
      }

      if(is.factor(self$data[, private$treatment_name])){
        self$data[,private$treatment_name] <- as.numeric(as.character(self$data[,private$treatment_name]))
      }

      # browser()
      # self$model$treatment <- private$fit_treatment(...)
      # self$model$outcome <- private$fit_outcome(two_models, ...)
      # self$ps.est <- private$est_ps()
      # self$po.est <- private$est_potentialOutcomes(two_models)
      private$set_ATE()
      private$set_CATE(private$confounders_treatment_name,TRUE)
      private$isTrial <- isTrial
      self$id <- paste(self$id, paste(private$treatment_method,private$outcome_method, sep = "_"), sep = "/")
    }
  ),

  # when use PSweight, outcome data should be numeric!
  private = list(
    treatment_method = NULL,
    outcome_method = NULL,
    treatment_formula = NULL,
    outcome_formula = NULL,
    confounders_treatment_factor = NULL,

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

    fit_treatment_BART = function(...) {
      x.train <- self$data[, c(private$confounders_treatment_name)]
      if(length(private$confounders_treatment_factor)>0){
        x.train <- fastDummies::dummy_cols(x.train, select_columns= private$confounders_treatment_factor,
                                           remove_selected_columns = TRUE)
      }
      x.train <- as.matrix(x.train)
      y.train <- as.matrix(self$data[,private$treatment_name])
      if (length(unique(self$data[, private$treatment_name]))>2) {
        message("we don't have the function for more than 2 arms yet")
        model <- BART::wbart(x.train=x.train, y.train = y.train, ...)
      } else {
        model <- BART::pbart(x.train=x.train, y.train = y.train, ...)
        prob.train <- pnorm(model$yhat.train)
        ps <- apply(prob.train,2,mean)
      }
      return(list(model=model,
                  ps = ps))
    },


    fit_outcome = function(two_models, ...) {
      #browser()
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

    fit_outcome_BART = function(two_models, ...) {
      if (two_models) {
        if(is.factor(class(self$data[,private$treatment_name]))){
          # browser()
          t.level <- unique(self$data[, private$treatment_name])
          level.order <- order(t.level)
          t0 <- t.level[match(1, level.order)]
          t1 <- t.level[match(2, level.order)]
          train.t0.id <- (self$data[, private$treatment_name] == t0)
          train.t1.id <- (self$data[, private$treatment_name] == t1)
        } else {
          train.t0.id <- (self$data[, private$treatment_name] == 0)
          train.t1.id <- (self$data[, private$treatment_name] == 1)
        }

        x.train.1 <- self$data[train.t1.id, c(private$confounders_treatment_name)]
        x.train.0 <- self$data[train.t1.id, c(private$confounders_treatment_name)]
        y.train.1 <- self$data[train.t1.id,private$outcome_name]
        y.train.0 <- self$data[train.t0.id,private$outcome_name]

        if(length(private$confounders_treatment_factor)>0){
          x.train.1 <- fastDummies::dummy_cols(x.train.1, select_columns= private$confounders_treatment_factor,
                                             remove_selected_columns = TRUE)
          x.train.0 <- fastDummies::dummy_cols(x.train.0, select_columns= private$confounders_treatment_factor,
                                             remove_selected_columns = TRUE)
        }
        x.train.1 <- as.matrix(x.train.1)
        x.train.0 <- as.matrix(x.train.0)
        y.train.1 <- as.matrix(y.train.1)
        y.train.0 <- as.matrix(y.train.0)

        model.y1 <- BART::pbart(x.train=x.train.1, y.train = y.train.1, ...)
        model.y0 <- BART::pbart(x.train=x.train.1, y.train = y.train.1, ...)

        return(model = list(model.y1 = model.y1, model.y0 = model.y0))

      } else {
        #browser()

        x.train <- self$data[, c(private$confounders_treatment_name,private$treatment_name)]
        x.train[,private$treatment_name] <- as.numeric(as.character(x.train[,private$treatment_name]))
        y.train <- self$data[, private$outcome_name]
        if(length(private$confounders_treatment_factor)>0){
          x.train <- fastDummies::dummy_cols(x.train, select_columns= private$confounders_treatment_factor,
                                             remove_selected_columns = TRUE)
        }
        x.train <- as.matrix(x.train)
        y.train <- as.matrix(y.train)

        model <- BART::pbart(x.train=x.train, y.train = y.train, ...)

        return(model)
      }
    },

    est_ps = function() {
      ps <- predict(self$model$treatment, newdata = self$data, type = "prob")[, 2]
      return(ps)
    },

    est_potentialOutcomes = function(two_models) {
      #browser()
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
    },


    est_potentialOutcomes_BART= function(two_models) {
      #browser()
      if (two_models) {
        data <- self$data[, private$confounders_treatment_name]
        if(length(private$confounders_treatment_factor)>0){
          data <- fastDummies::dummy_cols(data, select_columns= private$confounders_treatment_factor,
                                          remove_selected_columns = TRUE)
        }
        data <- as.matrix(data)

        y1.hat <- apply(predict(self$model$outcome$model.y1, newdata = data)$prob.test,2,mean)
        y0.hat <- apply(predict(self$model$outcome$model.y0, newdata = data)$prob.test,2,mean)
      } else {
        #browser()
        data0 <- data1 <- self$data[, c(private$confounders_treatment_name, private$treatment_name)]
        data0[, private$treatment_name] <- 0
        data1[, private$treatment_name] <- 1
        if(length(private$confounders_treatment_factor)>0){
          data0 <- fastDummies::dummy_cols(data0, select_columns= private$confounders_treatment_factor,
                                           remove_selected_columns = TRUE)
          data1 <- fastDummies::dummy_cols(data1, select_columns= private$confounders_treatment_factor,
                                           remove_selected_columns = TRUE)
        }
        data0 <- as.matrix(data0)
        data1 <- as.matrix(data1)

        y1.hat <- apply(predict(self$model$outcome, newdata = data1)$prob.test,2,mean)
        y0.hat <- apply(predict(self$model$outcome, newdata = data0)$prob.test,2,mean)
      }

      po.est <- data.frame("0"= y0.hat, "1"=y1.hat)
      return(po.est)
    }
  )
)
