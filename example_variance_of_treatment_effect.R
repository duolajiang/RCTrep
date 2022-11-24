n_rct <- 5000
var_name <- c("x1","x2","x3","x4","x5","x6")
p_success_rct <- c(0.7,0.9,0.2,0.3,0.2,0.3)
p_success_rwd <- c(0.2,0.2,0.8,0.8,0.7,0.8)
tau <- "6*x2+x6+2"
y0 <- "x1"
log.ps <- "x1*x2+x3*x4+5*x5+x6"
rho1 <- c("x1","x2",0)
rho2 <- c("x2","x3",1)


DGM_2 <- function(trial,n, var_name, p_success,tau, y0, log.ps=NULL, binary=FALSE, noise=1, ...){
  p <- length(var_name)
  mu <- rep(0, p)
  sigma <- diag(x=1,nrow=p,ncol=p)
  args <- list(...)
  n_args <- length(args)
  if(n_args > 0) {
    vars <- var_name
    sigma <- RCTrep:::CovarianceMatrix(sigma,args,vars)
  }
  mu <- rep(0, p)
  X <- MASS::mvrnorm(n,mu,sigma)

  for (i in seq(p)) {
    X[,i] <- ifelse(X[,i]<p_success[i],1,0)
  }

  #X <- sapply(p_success, function(x) rbinom(n,1,x))

  data <- as.data.frame(X)
  colnames(data) <- var_name
  if(trial){
    z <- rbinom(n,1,0.5)
  } else{
    log.OR <- eval(parse(text=log.ps), data)
    mean.log.OR <- mean(log.OR)
    sd.log.OR <- sd(log.OR)
    log.OR.norm <- (log.OR-mean.log.OR)/sd.log.OR*sqrt(3)/pi
    ps <- 1/(1+exp(-log.OR.norm))
    z <- rbinom(n,1,ps)
  }

  data <- cbind(data,z)
  tau <- eval(parse(text=tau),data)
  y0 <- eval(parse(text=y0),data)

  if(binary){
    log.OR <- y0 + tau*z
    mean.log.OR <- mean(log.OR)
    sd.log.OR <- sd(log.OR)
    log.OR.norm <- (log.OR-mean.log.OR)/sd.log.OR*sqrt(3)/pi
    pr <- 1/(1+exp(-log.OR.norm))
    data$y <- as.factor(rbinom(n,1,pr))
  } else {
    data$y <- y0 + tau*z + rnorm(n)
  }
  return(data)
}

data.1 <- DGM_2(trial=TRUE,  n_rct, var_name, p_success_rct, tau, y0, log.ps=0, binary = FALSE, noise=1, rho1, rho2)

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

source.obj <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = data.1,
  name = "RWD",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  data.public = TRUE
)

source.obj$plot_CATE(c("x1","x3","x4","x5"),TRUE)
















