Crude <- R6::R6Class(
  "Crude",
  inherit = Estimator,
  #-------------------------public fields-----------------------------#
  public = list(
    y1.hat = NA,
    y0.hat = NA,

    initialize = function(df){
      super$initialize(df)
      private$set_ATE()
      private$set_ATE_se()
    }
  ),
  #-------------------------public fields-----------------------------#
  private = list(
    get_ATE = function(){
      return(mean(private$data[private$data$z==1,'y'])-mean(private$data[private$data$z==0,'y']))
    },

    get_ATE_se = function(){
      sigma1 <- var(private$data[private$data$z==1,'y'])
      sigma0 <- var(private$data[private$data$z==0,'y'])
      n1 <- sum(private$data$z==1)
      n0 <- sum(private$data$z==0)
      se <- sqrt(sigma1/n1+sigma0/n0)
      return(se)
    },

    get_CATE_mean = function(index){
      n.sub <- length(index)
      subgroup <- private$data[index,]
      y1 <- subgroup[subgroup$z==1,'y']
      y0 <- subgroup[subgroup$z==0,'y']
      CATE_mean <- mean(y1)-mean(y0)
      return(CATE_mean)
    },

    get_CATE_se = function(index){
      subgroup <- private$data[index,]
      y1 <- subgroup[subgroup$z==1,'y']
      y0 <- subgroup[subgroup$z==0,'y']
      sigma1 <- var(y1); sigma0 <- var(y0)
      n1 <- length(y1); n0 <- length(y0)
      se <- sqrt(sigma1/n1+sigma0/n0)
      return(se)
    }
  )
)
