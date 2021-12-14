#' @export
G_computation <- R6::R6Class(
  "G_computation",
  inherit = G_computation_base,
  #-------------------------public fields-----------------------------#
  public = list(

    initialize = function(df, vars_name, gc.method, gc.formula, var_approach="Bias_adjusted",...){
      #browser()
      super$initialize(df,vars_name)
      self$gc.method <- gc.method
      self$gc.formula <- gc.formula
      self$var_approach <- var_approach
      self$model <- private$fit(gc.formula, gc.method,...)
      self$po.est <- private$est_potentialOutcomes()
      self$resi <- private$est_residual()
      ATE_SE <- private$est_ATE_SE()
      self$ATE_mean <- ATE_SE$est
      self$ATE_se <- ATE_SE$se
    }
  ),

  #-------------------------private fields-----------------------------#
  private = list(

    fit = function(gc.formula, gc.method,...){
      #browser()
      if(is.null(gc.formula)){
        model <- caret::train(x=self$data[,c(self$confounders_internal_name,self$treatment_name)],
                              y=self$data[,self$outcome_name],
                              method = gc.method,
                              ...)
      } else {
        model <- caret::train(form=gc.formula,
                              data=self$data,
                              method = gc.method,
                              ...)
      }
      return(model)
    },

    est_residual = function(){
      #browser()
      if(class(self$data[,self$outcome_name])=="numeric"){
        resi <- residuals(self$model)
      } else {
        y <- self$data[,self$outcome_name]
        y.hat <- predict(self$model,newdata=self$data, type="prob")
        y.class.id <- match(y,colnames(y.hat))
        y.hat.observed <- NULL
        for (i in seq(self$n)) {
          y.hat.observed <- rbind(y.hat.observed, y.hat[i,y.class.id[i]])
        }
        resi.value <- sqrt(2*log(1/y.hat.observed))
        resi.sign <- ifelse(y.class.id==1,-1,1)
        resi <- resi.sign*resi.value
      }
      return(resi)
    },

    est_potentialOutcomes = function(){
      #browser()
      data0 <- data1 <- self$data[,c(self$confounders_internal_name,self$treatment_name)]
      t.level <- unique(self$data[,self$treatment_name])
      level.order <- order(t.level)
      data0[,self$treatment_name] <- t.level[match(1,level.order)]
      data1[,self$treatment_name] <- t.level[match(2,level.order)]

      if(class(self$data[,self$outcome_name])=="numeric"){
        y1.hat <- predict(self$model,newdata=data1)
        y0.hat <- predict(self$model,newdata=data0)
      } else {
        y1.hat <- predict(self$model,newdata=data1, type="prob")[,2]
        y0.hat <- predict(self$model,newdata=data0, type="prob")[,2]
      }
      return(list(y1.hat=y1.hat,y0.hat=y0.hat))
    },

    est_var_potentialoutcomes = function(){
      #browser()
      d1 <- d0 <- self$data
      d1$z <- 1
      d0$z <- 0
      iterations <- self$iterations
      y1.ind.s <- y0.ind.s <- matrix(data=NA,nrow = self$n,ncol = iterations)

      if((self$var_approach=="Simulation")&(self$model$method=="glm")){
        gc.obj <- self$model
        summary.gc.obj <- summary(gc.obj)
        simul <- MASS::mvrnorm(n = iterations, mu=summary.gc.obj$coefficients[,'Estimate'], Sigma=summary.gc.obj$cov.unscaled)
        for(i in 1:iterations) {
          gc.obj$coefficients <- simul[i,]
          y1.ind.s[,i] <- predict(gc.obj, newdata=d1)
          y0.ind.s[,i] <- predict(gc.obj, newdata=d0)
        }
      } else if(self$var_approach=="Bootstrapping"){
        for(i in 1:iterations){
          dboot <- self$data[sample(1:self$n, size=self$n, replace = TRUE),]
          d.obj <- private$fit(self$gc.formula, self$gc.method, data=dboot)
          y1.ind.s[,i] <- predict(d.obj, newdata=d1)
          y0.ind.s[,i] <- predict(d.obj, newdata=d0)
        }
      }

      return(list(y1.hat.rep=y1.ind.s,
                  y0.hat.rep=y0.ind.s))
    }
  )
)
