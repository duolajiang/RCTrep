TEstimator_Synthetic <- R6::R6Class(
  "TEstimator_Synthetic",
  inherit = TEstimator,
  public = list(
    initialize = function(data, estimates, vars_name, name, isTrial, data.public=FALSE){
      #browser()
      self$name <- name
      self$estimates$ATE$est <- estimates$ATE_mean
      self$estimates$ATE$se <- estimates$ATE_se
      self$estimates$CATE <- estimates$CATE_mean_se
      private$confounders_treatment_name <- vars_name$confounders_treatment_name
      # self$data should be joint distribution of confounders with sample size
      if(data.public){
        self$data <- data
      } else {
        self$data <- data %>%
          group_by(across(all_of(private$confounders_treatment_name))) %>%
          summarise(size=n())
      }
      self$data$id <- seq(dim(self$data)[1])
      self$id <- "Crude"
      self$statistics <- list(n=dim(data)[1],
                              density_confounders=private$est_joint_denstiy())
      private$isTrial <- isTrial
    }
  ),
  private = list(
    est_joint_denstiy = function(){
      #browser()
      joint_var_internal <-
        self$data %>%
        group_by(across(all_of(c(private$confounders_treatment_name)))) %>%
        summarise(count=n())
      joint_var_internal <- as.data.frame(joint_var_internal)
      return(joint_var_internal)
    }
  )
)
