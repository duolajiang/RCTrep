#' @title R6 class: Inverse propensity score weighting estimator base class
#'
#' @description A base R6 class for inverse propensity score weighting estimator of average treatment effect that implements comment methods.
#'
#' @field model: a fitted model for treatment conditioning on covariates
#' @field method: a string specifying the method for fitting the propensity score
#' @field formula: an optional \code{formula} object. The formula is specified as \code{Z ~ X1+X2...}, where \code{Z} represents treatment variable and \code{X1} and \code{X2} are covariates.
#' @field ps: a numeric vector of length n where n is row number of \code{self$data}.
#'
#' @importFrom PSweight PSweight
IPW <- R6::R6Class(
  "IPW",
  inherit = TEstimator,
  public = list(
    #-------------------------public fields-----------------------------#
    ps.est = NA,
    id = "IPW",

    initialize = function(df, vars_name, name, treatment_method, treatment_formula, isTrial, ...) {
      #browser()
      super$initialize(df, vars_name, name)
      private$method <- treatment_method
      private$formula <- treatment_formula
      self$model <- private$fit(...)
      self$ps.est <- private$est_ps()
      private$set_ATE()
      private$set_CATE(private$confounders_treatment_name,TRUE)
      private$isTrial <- isTrial

    },

    summary = function(stratification){
      data.ps <- data.frame(treatment=self$data[,private$treatment_name], ps=self$ps.est)
      plot.ps.overall <- ggplot2::ggplot(data=data.ps, aes(x=ps, color=treatment, fill=treatment)) +
        geom_density(alpha=.5)
      plot.ps.subgroup <- private$plot_aggregate_ps(stratification)
      plot.agg <- ggpubr::ggarrange(plot.ps.overall, plot.ps.subgroup, ncol = 3)

      out <- list(est.cate = self$estimates$CATE,
                  plot.agg = plot.agg)
      out
    }

  ),

  private = list(

    method = "glm",
    formula = NULL,

    est_ATE_SE = function(index) {
      ngrp <- length(unique(self$data[index,private$treatment_name]))
      group.name <- unique(self$data[index,private$treatment_name])
      if(ngrp<2){
        warning("no overlap in this group! Variance is unbounded")
        est <- NA
        se <- Inf
        y1.hat <- ifelse(group.name==1,IPW.estimator(z=self$data[index,private$treatment_name],
                                                     y=self$data[index,private$outcome_name],
                                                     ps=self$ps.est[index],
                                                     w=rep(1,length(index)),t=1),NA)
        y0.hat <- ifelse(group.name==0,IPW.estimator(z=self$data[index,private$treatment_name],
                                                     y=self$data[index,private$outcome_name],
                                                     ps=self$ps.est[index],
                                                     w=rep(1,length(index)),t=0),NA)
      } else {
        weight.obj <- PSweight::PSweight(
          ps.estimate = self$ps.est[index], weight = "IPW",
          data = self$data[index, ],
          yname = private$outcome_name, zname = private$treatment_name
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
      group.name <- unique(self$data[index,private$treatment_name])
      if(ngrp<2){
        warning("no overlap in this group! Variance is unbounded")
        est <- NA
        se <- Inf
        y1.hat <- ifelse(group.name==1,IPW.estimator(z=self$data[index,private$treatment_name],
                                                     y=self$data[index,private$outcome_name],
                                                     ps=self$ps.est[index],
                                                     w=weight,t=1),NA)
        y0.hat <- ifelse(group.name==0,IPW.estimator(z=self$data[index,private$treatment_name],
                                                     y=self$data[index,private$outcome_name],
                                                     ps=self$ps.est[index],
                                                     w=weight,t=0),NA)
      } else {
        weight.obj <- PSweight.modified(
          ps.estimate = self$ps.est[index], weight = "IPW",
          data = self$data[index, ],
          yname = private$outcome_name, zname = private$treatment_name,
          weight.external = weight
        )
        res.obj <- summary(weight.obj, contrast = NULL, type = "DIF", CI = "TRUE")
        est <- res.obj$estimates[1]
        se <- sqrt(res.obj$estimates[2])
        y1.hat <- weight.obj$muhat[which(weight.obj$group==1)]
        y0.hat <- weight.obj$muhat[which(weight.obj$group==0)]
      }

      return(list(y1.hat = y1.hat, y0.hat = y0.hat, est = est, se = se))
    },

    fit = function(...) {
      #browser()
      if (is.null(private$formula)) {
        model <- caret::train(
          x = self$data[, private$confounders_treatment_name],
          y = self$data[, private$treatment_name],
          method = private$method,
          ...
        )
      } else {
        model <- caret::train(
          form = private$formula,
          data = self$data,
          method = private$method,
          ...
        )
      }
      return(model)
    },

    est_ps = function() {
      #browser()
      ps.est <- predict(self$model, newdata = self$data, type = "prob")[, 2]
      return(ps.est)
    },

    plot_aggregate_ps = function(stratification){
      #browser()
      data <- data.frame(self$data,ps=self$ps.est)
      group_data <- data %>% group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      group_sample_size <- group_size(group_data)
      DF <- NULL
      var_names <- colnames(group_strata)
      for (i in seq(n_groups)) {
        x <- group_strata[i,]
        strata.i <- paste(var_names, x, sep = "=", collapse = ",")
        size <- rep(group_sample_size[i],each=group_sample_size[i])
        subgroup.id.in.data <- data[group_id == i, "id"]
        #browser()
        df <- data %>%
          slice(subgroup.id.in.data) %>%
          select(ps,private$treatment_name,private$outcome_name) %>%
          mutate(size=size, group=strata.i)

        DF <- rbind(DF,df)
      }

      plot.ps.subgroup <- ggplot2::ggplot(data = DF,aes(x=ps,y=group,
                                                        color=eval(parse(text=private$treatment_name)))) +
        geom_boxplot(alpha=0.4) +
        scale_color_discrete(name = "treatment")

      return(plot.ps.subgroup)

    }
  )
)
