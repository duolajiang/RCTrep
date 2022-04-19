#' @title R6 class: G_computation base class
#' @description A base R6 class for G_computation estimator for average treatment effect
#' @export
#' @import geex
G_computation <- R6::R6Class(
  "G_computation",
  inherit = TEstimator,
  #-------------------------public fields-----------------------------#
  public = list(

    po.est = list(
      y1.hat = NULL,
      y0.hat = NULL
    ),
    po.est.var = list(
      y1.hat.rep = NULL,
      y0.hat.rep = NULL
    ),
    resi = NULL,

    ps.est = NULL,

    id = "G_computation",

    initialize = function(df, vars_name, name,
                          gc.method, gc.formula,
                          var_approach = "Bias_adjusted", isTrial, ...) {
      #browser()
      super$initialize(df, vars_name, name)
      private$gc.method <- gc.method
      private$gc.formula <- gc.formula
      private$var_approach <- var_approach
      self$model <- private$fit(...)
      po.est <- private$est_potentialOutcomes()
      self$data$y1.hat <- po.est$y1.hat
      self$data$y0.hat <- po.est$y0.hat
      self$resi <- private$est_residual()
      private$set_ATE()
      private$set_CATE(private$confounders_treatment_name,TRUE)
      private$isTrial <- isTrial
    },

    summary = function(stratification, stratification_joint=TRUE){
      #browser()
      if(missing(stratification)){
        stratification <- private$confounders_treatment_name
      }

      residuals.overall <- mean(self$resi)
      residuals.subgroups <- private$aggregate_residual(stratification)

      if(test_binary(self$data[,private$outcome_name])){
        colnames.subgroups <- colnames(residuals.subgroups)
        var_names <- colnames.subgroups[!colnames.subgroups %in% c("sample.size","res.mean", "res.se")]
        var_names_data <- residuals.subgroups[,var_names]
        subgroup_name_level <- apply(var_names_data, 1, function(x) paste(var_names, x, sep = "=", collapse = ","))
        subgroup_name_level <- factor(subgroup_name_level, levels = subgroup_name_level, ordered = T)

        df <- residuals.subgroups %>%
          select(res.mean, res.se, sample.size) %>%
          mutate(group=subgroup_name_level,
                 ci_l=res.mean-1.98*res.se,
                 ci_u=res.mean+1.98*res.se)

        plot.res <- ggplot2::ggplot(data = df, aes(x = res.mean, y = group)) +
          geom_point(position = position_dodge(0.5), aes(size=sample.size)) +
          geom_errorbar(aes(xmin = ci_l, xmax = ci_u),
                        width = .3, position = position_dodge(0.5)) +
          geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
          ggtitle("mean(1.98se) deviance")+
          theme(plot.title = element_text(),
                legend.position = "none")

        out <- list(residuals.overall = residuals.overall,
                    residuals.subgroups = residuals.subgroups,
                    plot.res = plot.res)

      } else {
        colnames.subgroups <- colnames(residuals.subgroups)
        var_names <- colnames.subgroups[!colnames.subgroups %in% c("sample.size","res.mean", "res.se","msr","sesr")]
        var_names_data <- residuals.subgroups[,var_names]
        subgroup_name_level <- apply(var_names_data, 1, function(x) paste(var_names, x, sep = "=", collapse = ","))
        subgroup_name_level <- factor(subgroup_name_level, levels = subgroup_name_level, ordered = T)

        df <- residuals.subgroups %>%
          select(res.mean, res.se, sample.size) %>%
          mutate(group=subgroup_name_level,
                 ci_l=res.mean-1.98*res.se,
                 ci_u=res.mean+1.98*res.se)

        plot.res.subgroup <- ggplot2::ggplot(data = df, aes(x = res.mean, y = group)) +
          geom_point(position = position_dodge(0.5), aes(size=sample.size)) +
          geom_errorbar(aes(xmin = ci_l, xmax = ci_u),
                        width = .3, position = position_dodge(0.5)) +
          geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
          ggtitle("mean(1.98se) residual")+
          theme(plot.title = element_text(),
                legend.position = "none")

        df <- residuals.subgroups %>%
          select(msr, sesr, sample.size) %>%
          mutate(group=subgroup_name_level,
                 ci_l=msr-1.98*sesr,
                 ci_u=msr+1.98*sesr)

        plot.msr.subgroup <- ggplot2::ggplot(data = df, aes(x = msr, y = group)) +
          geom_point(position = position_dodge(0.5), aes(size=sample.size)) +
          geom_errorbar(aes(xmin = ci_l, xmax = ci_u),
                        width = .3, position = position_dodge(0.5)) +
          geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
          ggtitle("mean squared error")+
          theme(plot.title = element_text(),
                legend.position = "none")

        df <- data.frame(residual=self$resi)
        plot.res.overall <- ggplot(data = df, aes(x=residual)) + geom_density()

        plot.res.mse <- ggpubr::ggarrange(plot.res.subgroup, plot.res.overall, plot.msr.subgroup, ncol = 3, nrow = 1)

        out <- list(residuals.overall = residuals.overall,
                    residuals.subgroups = residuals.subgroups,
                    plot.res.mse = plot.res.mse)
      }

      out

    }
  ),
  #-------------------------private fields and methods----------------------------#
  private = list(

    gc.method = NULL,
    gc.formula = NULL,
    treatment_method = "glm",
    var_approach = NULL,
    iterations = 1,

    fit = function(...) {
      #browser()
      if (is.null(private$gc.formula)) {
        model <- caret::train(
          x = self$data[, c(private$confounders_treatment_name, private$treatment_name)],
          y = self$data[, private$outcome_name],
          method = private$gc.method,
          ...
        )
      } else {
        print(private$gc.formula)
        model <- caret::train(
          form = private$gc.formula,
          data = self$data,
          method = private$gc.method,
          ...
        )
      }
      return(model)
    },

    est_ATE_SE = function(index) {
      #browser()
      y1.hat <- self$data$y1.hat[index]
      y0.hat <- self$data$y0.hat[index]
      data <- as.data.frame(cbind(y1.hat, y0.hat))
      colnames(data) <- c("y1.hat","y0.hat")
      results <- m_estimate(estFUN = private$gc_estfun,
                            data = data,
                            root_control = setup_root_control(start = c(0,0,0)))
      y1.hat.mu <- results@estimates[1]
      y0.hat.mu <- results@estimates[2]
      est <- results@estimates[3]
      se <- sqrt(results@vcov[3,3])
      return(list(y1.hat = y1.hat.mu, y0.hat = y0.hat.mu, est = est, se = se))
    },

    est_weighted_ATE_SE = function(index, weight) {
      #browser()
      y1.hat <- self$data$y1.hat[index]*weight
      y0.hat <- self$data$y0.hat[index]*weight

      data <- as.data.frame(cbind(y1.hat, y0.hat))
      colnames(data) <- c("y1.hat","y0.hat")
      results <- m_estimate(estFUN = private$gc_estfun,
                            data = data,
                            root_control = setup_root_control(start = c(0,0,0)))
      y1.hat.mu <- results@estimates[1]
      y0.hat.mu <- results@estimates[2]
      est <- results@estimates[3]
      se <- sqrt(results@vcov[3,3])
      return(list(y1.hat = y1.hat.mu, y0.hat = y0.hat.mu, est = est, se = se))
    },

    gc_estfun = function(data){
      #browser()
      Y1 <- data$y1.hat
      Y0 <- data$y0.hat
      function(theta) {
        c(Y1 - theta[1],
          Y0 - theta[2],
          theta[1]-theta[2] - theta[3]
        )
      }
    },

    # compute deviance for continuous outcome/binary outcome
    est_residual = function(index) {
      #browser()
      if (class(self$data[index, private$outcome_name]) == "numeric") {
        resi <- residuals(self$model)
      } else {
        y <- self$data[index, private$outcome_name]
        y <- as.numeric(levels(y))[y]
        y.hat <- predict(self$model, newdata = self$data[index,], type = "prob")
        y.hat.0 <- y.hat[,"0"]
        y.hat.1 <- y.hat[,"1"]
        resi <- -2 * (y * log(y.hat.1) + (1-y)*log(y.hat.0))
      }
      return(resi)
    },

    est_potentialOutcomes = function() {
      #browser()
      data0 <- data1 <- self$data[, c(private$confounders_treatment_name, private$treatment_name)]
      t.level <- unique(self$data[, private$treatment_name])
      level.order <- order(t.level)
      data0[, private$treatment_name] <- t.level[match(1, level.order)]
      data1[, private$treatment_name] <- t.level[match(2, level.order)]

      if (!is.factor(self$data[, private$outcome_name])) {
        y1.hat <- predict(self$model, newdata = data1)
        y0.hat <- predict(self$model, newdata = data0)
      } else {
        y1.hat <- predict(self$model, newdata = data1, type = "prob")[, 2]
        y0.hat <- predict(self$model, newdata = data0, type = "prob")[, 2]
      }
      return(list(y1.hat = y1.hat, y0.hat = y0.hat))
    },

    est_ps = function() {
      ps <- predict(self$model$treatment, newdata = self$data, type = "prob")[, 2]
      return(ps)
    },

    aggregate_residual = function(stratification) {
      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      group_sample_size <- group_size(group_data)
      if(test_binary(self$data[,private$outcome_name])){
        res.mean <- res.se <- sample.size <- NULL
        for (i in seq(n_groups)) {
          # for binary outcome, how to evaluate model fit?
          subgroup.id.in.data <- self$data[group_id == i, "id"]
          res.mean[i] <- sum(self$resi[subgroup.id.in.data])/group_sample_size[i]
          res.se[i] <- sqrt(var(self$resi[subgroup.id.in.data])/group_sample_size[i])
          sample.size[i] <- group_sample_size[i]
        }
        res <- cbind(group_strata, sample.size, res.mean, res.se)
      } else {
        res.mean <- res.se <- sample.size <- msr <- sesr <- NULL
        for (i in seq(n_groups)) {
          subgroup.id.in.data <- self$data[group_id == i, "id"]
          res.mean[i] <- sum(self$resi[subgroup.id.in.data])/group_sample_size[i]
          res.se[i] <- sqrt(var(self$resi[subgroup.id.in.data])/group_sample_size[i])
          # mean of squared residual
          msr[i] <- sum((self$resi[subgroup.id.in.data])^2)/group_sample_size[i]
          sesr[i] <- sqrt(var((self$resi[subgroup.id.in.data])^2)/group_sample_size[i])
          sample.size[i] <- group_sample_size[i]
        }
        res <- cbind(group_strata, sample.size, res.mean, res.se, msr, sesr)
      }
      res <- as.data.frame(res)
      return(res)
    }
  )
)
