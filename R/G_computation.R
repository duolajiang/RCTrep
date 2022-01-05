#' @title R6 class: G_computation base class
#' @description A base R6 class for G_computation estimator for average treatment effect
#' @export
G_computation <- R6::R6Class(
  "G_computation",
  inherit = Estimator,
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

    initialize = function(df, vars_name, name,
                          gc.method, gc.formula,
                          var_approach = "Bias_adjusted",...) {
      # browser()
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
      private$set_CATE(private$confounders_internal_name,TRUE)
    },

    residual_check = function(stratification) {
      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      group_sample_size <- group_size(group_data)
      res.mean <- res.var <- sample.size <- NULL
      for (i in seq(n_groups)) {
        subgroup.id.in.data <- self$data[group_id == i, "id"]
        res.mean[i] <- mean(self$resi[subgroup.id.in.data])
        res.var[i] <- var(self$resi[subgroup.id.in.data])
        sample.size[i] <- group_sample_size[i]
      }
      res <- cbind(group_strata, sample.size, res.mean, res.var)
      res <- as.data.frame(res)
      return(res)
    },

    summary = function(){}
  ),
  #-------------------------private fields and methods----------------------------#
  private = list(

    gc.method = NULL,
    gc.formula = NULL,
    var_approach = NULL,
    iterations = 1,

    fit = function(...) {
      if (is.null(private$gc.formula)) {
        model <- caret::train(
          x = self$data[, c(private$confounders_internal_name, private$treatment_name)],
          y = self$data[, private$outcome_name],
          method = private$gc.method,
          ...
        )
      } else {
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
      n <- length(index)
      y1.hat <- sum(self$data$y1.hat[index])/n
      y0.hat <- sum(self$data$y0.hat[index])/n
      est <- y1.hat - y0.hat

      var.resid <- mean((self$resi[index])^2)
      var.ate <- 2 * (var.resid) / n
      se <- sqrt(var.ate)

      return(list(y1.hat = y1.hat, y0.hat = y0.hat, est = est, se = se))

    },

    est_weighted_ATE_SE = function(index, weight) {
      weight.norm <- weight / sum(weight)
      y1.hat <- sum(self$data$y1.hat[index]*weight.norm)
      y0.hat <- sum(self$data$y0.hat[index]*weight.norm)
      est <- y1.hat - y0.hat

      var.resid <- mean((self$resi[index])^2)
      var.ate <- 2 * (sum(weight^2) * var.resid) / (sum(weight)^2)
      se <- sqrt(var.ate)

      return(list(y1.hat = y1.hat, y0.hat = y0.hat, est = est, se = se))
    },

    est_residual = function() {
      # browser()
      if (class(self$data[, private$outcome_name]) == "numeric") {
        resi <- residuals(self$model)
      } else {
        y <- self$data[, private$outcome_name]
        #browser()
        y.hat <- predict(self$model, newdata = self$data, type = "prob")
        y.class.id <- match(y, colnames(y.hat))
        y.hat.observed <- NULL
        for (i in seq(self$statistics$n)) {
          y.hat.observed <- rbind(y.hat.observed, y.hat[i, y.class.id[i]])
        }
        resi.value <- sqrt(2 * log(1 / y.hat.observed))
        resi.sign <- ifelse(y.class.id == 1, -1, 1)
        resi <- resi.sign * resi.value
      }
      return(resi)
    },

    est_potentialOutcomes = function() {
      # browser()
      data0 <- data1 <- self$data[, c(private$confounders_internal_name, private$treatment_name)]
      t.level <- unique(self$data[, private$treatment_name])
      level.order <- order(t.level)
      data0[, private$treatment_name] <- t.level[match(1, level.order)]
      data1[, private$treatment_name] <- t.level[match(2, level.order)]

      if (class(self$data[, private$outcome_name]) == "numeric") {
        y1.hat <- predict(self$model, newdata = data1)
        y0.hat <- predict(self$model, newdata = data0)
      } else {
        y1.hat <- predict(self$model, newdata = data1, type = "prob")[, 2]
        y0.hat <- predict(self$model, newdata = data0, type = "prob")[, 2]
      }
      return(list(y1.hat = y1.hat, y0.hat = y0.hat))
    },

    est_var_potentialoutcomes = function() {
      # browser()
      d1 <- d0 <- self$data
      d1$z <- 1
      d0$z <- 0
      iterations <- private$iterations
      y1.ind.s <- y0.ind.s <- matrix(data = NA, nrow = self$statistics$n, ncol = iterations)

      if ((private$var_approach == "Simulation") & (self$model$method == "glm")) {
        gc.obj <- self$model
        summary.gc.obj <- summary(gc.obj)
        simul <- MASS::mvrnorm(n = iterations, mu = summary.gc.obj$coefficients[, "Estimate"], Sigma = summary.gc.obj$cov.unscaled)
        for (i in 1:iterations) {
          gc.obj$coefficients <- simul[i, ]
          y1.ind.s[, i] <- predict(gc.obj, newdata = d1)
          y0.ind.s[, i] <- predict(gc.obj, newdata = d0)
        }
      } else if (private$var_approach == "Bootstrapping") {
        for (i in 1:iterations) {
          dboot <- self$data[sample(1:self$n, size = self$n, replace = TRUE), ]
          d.obj <- private$fit(private$gc.formula, private$gc.method, data = dboot)
          y1.ind.s[, i] <- predict(d.obj, newdata = d1)
          y0.ind.s[, i] <- predict(d.obj, newdata = d0)
        }
      }

      return(list(
        y1.hat.rep = y1.ind.s,
        y0.hat.rep = y0.ind.s
      ))
    }
  )
)
