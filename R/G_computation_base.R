#' @title R6 class: G_computation base class
#' @description A base R6 class for G_computation estimator for average treatment effect
#' @export
G_computation_base <- R6::R6Class(
  "G_computation_base",
  inherit = Estimator,
  #-------------------------public fields-----------------------------#
  public = list(
    po.est = list(y1.hat = NULL,
                  y0.hat = NULL),

    po.est.var = list(y1.hat.rep = NA,
                      y0.hat.rep = NA),

    pred = NA,

    model = NA,
    gc.method = NA,
    gc.formula = NA,
    var_approach = NA,
    iterations = 1,
    resi = NA,

    initialize = function(df,vars_name){
      super$initialize(df,vars_name)
    },

    residual_check = function(stratification){
      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      group_sample_size <- group_size(group_data)
      res.mean <- res.var <- sample.size <- NULL
      for (i in seq(n_groups)){
        subgroup.id.in.data <- self$data[group_id==i,'id']
        res.mean[i] <- mean(self$resi[subgroup.id.in.data])
        res.var[i] <- var(self$resi[subgroup.id.in.data])
        sample.size[i] <- group_sample_size[i]
      }
      res <- cbind(group_strata,sample.size,res.mean,res.var)
      res <- as.data.frame(res)
      return(res)
    }

  ),
  #-------------------------private fields and methods----------------------------#
  private = list(

    est_weighted_ATE_SE = function(weight){
      weight.norm <- weight/sum(weight)
      est <- sum((self$po.est$y1.hat-self$po.est$y0.hat)*weight.norm)

      # assuming Y(1) and Y(0) is independent
      var.resid <- mean((self$resi)^2)
      var.ate <- 2 * (sum(weight^2)*var.resid) / (self$n^2)
      se <- sqrt(var.ate)

      return(list(est=est,se=se))
    },


    est_weighted_CATE_SE = function(index,weight){
      weight.norm <- weight/sum(weight)
      y1.hat <- self$po.est$y1.hat[index]
      y0.hat <- self$po.est$y0.hat[index]
      est <- sum((y1.hat-y0.hat)*weight.norm)

      var.resid <- mean((self$resi[index])^2)
      var.ate <- 2 * (sum(weight^2)*var.resid) / (sum(weight)^2)
      se <- sqrt(var.ate)
      return(list(est=est,se=se))
    },

    est_ATE_SE = function(){
      est <- sum(self$po.est$y1.hat-self$po.est$y0.hat) / self$n

      var.resid <- mean((self$resi)^2)
      var.ate <- 2 * (var.resid) / (self$n)
      se <- sqrt(var.ate)
      return(list(est=est,se=se))
    },

    est_CATE_SE = function(index){
      n <- length(index)
      y1.hat <- self$po.est$y1.hat[index]
      y0.hat <- self$po.est$y0.hat[index]
      est <- sum(y1.hat-y0.hat)/n

      var.resid <- mean((self$resi[index])^2)
      var.ate <- 2 * (var.resid) / n
      se <- sqrt(var.ate)

      return(list(est=est,se=se))
    }
  )
)











