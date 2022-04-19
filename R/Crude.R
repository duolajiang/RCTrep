Crude <- R6::R6Class(
  "Crude",
  inherit = TEstimator,
  #-------------------------public fields-----------------------------#
  public = list(
    id = "Crude",

    initialize = function(df, vars_name, name, isTrial) {
      #browser()
      super$initialize(df, vars_name, name)
      private$set_ATE()
      private$set_CATE(private$confounders_treatment_name,TRUE)
      private$isTrial <- isTrial
    },

    summary = function(...){
      stratification <- list(...)
      if(length(stratification)==0){
        stratification <- private$confounders_external_name
      } else {
        stratification <- unlist(stratification)
      }

      plot.cate <- self$plot_CATE(stratification)
      out <- list(est.cate = self$estimates$CATE,
                  plot.cate = plot.cate)
      out
    }
  ),
  #-------------------------public fields-----------------------------#
  private = list(

    est_ATE_SE = function(index) {
      #browser()
      data <- self$data[index, c(private$treatment_name,private$outcome_name)]
      y1 <- data[data[,private$treatment_name]==1,private$outcome_name]
      y0 <- data[data[,private$treatment_name]==0,private$outcome_name]
      n1 <- length(y1)
      n0 <- length(y0)

      y1.hat.mu <- mean(y1)
      y0.hat.mu <- mean(y0)
      est <- y1.hat.mu - y0.hat.mu
      sigma1 <- var(y1)
      sigma0 <- var(y0)
      se <- sqrt(sigma1/n1 + sigma0/n0)

      # y1.hat.mu <- log(y1.hat.mu/(1-y1.hat.mu))
      # y0.hat.mu <- log(y0.hat.mu/(1-y0.hat.mu))
      # est <- y1.hat.mu - y0.hat.mu

      return(list(y1.hat = y1.hat.mu, y0.hat = y0.hat.mu, est = est, se = se))
    },

    est_weighted_ATE_SE = function(index, weight) {
      #browser()
      data <- self$data[index, c(private$treatment_name,private$outcome_name)]
      y1 <- data[data[,private$treatment_name]==1,private$outcome_name]*weight
      y0 <- data[data[,private$treatment_name]==0,private$outcome_name]*weight
      n1 <- length(y1)
      n0 <- length(y0)

      y1.hat.mu <- mean(y1)
      y0.hat.mu <- mean(y0)
      est <- y1.hat.mu - y0.hat.mu
      sigma1 <- var(y1)
      sigma0 <- var(y0)
      se <- sqrt(sigma1/n1 + sigma0/n0)

      return(list(y1.hat = y1.hat.mu, y0.hat = y0.hat.mu, est = est, se = se))
    }

  )
)
