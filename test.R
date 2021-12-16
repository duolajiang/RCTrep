library(testthat)
#library(RCTrep)
library(MASS)
library(dplyr)
library(mvtnorm)

DGM <- function(trial,n, var_name, p_success,tau, y0, binary, ...){
  #browser()
  set.seed(123)
  p <- length(var_name)-1
  mu <- rep(0, p)
  sigma <- diag(x=1,nrow=p,ncol=p)
  args <- list(...)
  n_args <- length(args)
  if(n_args > 0) {
    vars <- var_name
    sigma <- CovarianceMatrix(sigma,args,vars)
  }
  mu <- rep(0, p)
  X <- mvrnorm(n,mu,sigma)

  for (i in seq(p)) {
    X[,i] <- ifelse(X[,i]<p_success[i],1,0)
  }
  X <- cbind(X,rnorm(n,p_success[p+1],1))
  if(trial){
    z <- rbinom(n,1,0.5)
  } else{
    log.OR <- X[,1]*X[,2]+X[,3]*X[,4]+5*X[,5]
    ps <- 1/(1+exp(-log.OR))
    z <- rbinom(n,1,ps)
  }
  data <- cbind(X,z)
  data <- as.data.frame(data)
  colnames(data) <- c(var_name,'z')
  tau <- eval(parse(text=tau),data)
  y0 <- eval(parse(text=y0),data)
  #data$y <- y0 + tau*z + rnorm(n)

  if(binary){
    log.OR <- y0 + tau*z
    pr <- 1/(1+exp(-log.OR))
    data$y <- rbinom(n,1,pr)
  } else {
    data$y <- y0 + tau*z + rnorm(n)
  }
  AdjustATE <- ATEAdjustment(tau,y0,data)
  NoAdjustATE <- mean(data[data$z==1,'y'])-mean(data[data$z==0,'y'])
  TrueATE <- mean(tau)
  colnames.DGM <- c("Crude","Adjusted","True")
  ATE.DGM <- c(NoAdjustATE,
               AdjustATE,
               TrueATE)
  print.ATE <- rbind(colnames.DGM,ATE.DGM)
  print(print.ATE)

  return(data)
}

## adjustment covariate ATE vs no adjustment covariate ATE vs true ATE, see the difference
ATEAdjustment <- function(tau,y0,data){
  formula <- y ~ eval(y0) + z:eval(tau)
  g_compu <- lm(formula, data = data)
  data1 <- data0 <- data
  data1$z <- 1; data0$z <- 0
  po.1 <- predict(g_compu,data1)
  po.0 <- predict(g_compu,data0)
  ATE <- mean(po.1-po.0)
  return(ATE)
}
CovarianceMatrix <- function(sigma,args,vars){
  #
  n_rhos <- length(args)
  for (i in seq(n_rhos)) {
    v_name_1 <- args[[i]][1]
    v_name_2 <- args[[i]][2]
    pair_rho <- as.numeric(args[[i]][3])
    index.1 <- which(v_name_1==vars)
    index.2 <- which(v_name_2==vars)
    sigma[index.1,index.2] <- pair_rho
    sigma[index.2,index.1] <- pair_rho
  }
  return(sigma)
}
######################################
# DGM for simulation
######################################
set.seed(123)
n_rct <- 500; n_rwd <- 500
var_name <- c("x1","x2","x3","x4","x5","x6")
p_success_rct <- c(0.7,0.9,0.2,0.3,0.2,1)
p_success_rwd <- c(0.2,0.2,0.8,0.8,0.7,-1)
tau <- "7*x2+x6+2"
y0 <- "x1"
rho1 <- c("x1","x2",0)
rho2 <- c("x2","x3",0)
# ===================================
# simulating data
RCT <- DGM(trial=TRUE,  n_rct, var_name, p_success_rct, tau, y0, binary = FALSE, rho1, rho2)
RWD <- DGM(trial=FALSE, n_rwd, var_name, p_success_rwd, tau, y0, binary = FALSE, rho1, rho2)
# ===================================
# ===================================
Estimator <- "G_computation"
strata <- c("x1","x2","x6")
strata_joint <- FALSE
vars_name <- list(confounders_internal=c("x1","x2","x3","x4","x5","x6"),
                  treatment=c('z'),
                  outcome=c('y')
                  )
confounders_external <- c("x1","x2","x3","x4","x5","x6")
# ===================================
# ===================================
rwd.obj <- Estimate(Estimator=Estimator, data=RWD, vars_name=vars_name,
                    stratification=strata,stratification_joint=strata_joint,
                    strata_cut=list(x6=list(breaks=c(min(RWD$x6),0,max(RWD$x6)),
                                            labels=c("1","2"))),
                    outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3 + z:x6)
rct.obj <- Estimate(Estimator=Estimator, data=RCT, vars_name=vars_name,
                    stratification=strata,stratification_joint=strata_joint,
                    strata_cut=list(x6=list(breaks=c(min(RCT$x6),0,max(RCT$x6)),
                                            labels=c("1","2"))),
                    outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6)

rwd.obj$RCTrep(target.obj = rct.obj,
               confounders_external_name = confounders_external,
               weighting_estimator="Balancing",
               stratification=strata,stratification_joint=strata_joint)
Plot_estimate(source.obj = rwd.obj, target.obj = rct.obj)

# ===================================
# ===================================
emp.p1 <- mean(RCT$x1==1)
emp.p2 <- mean(RCT$x2==1)
emp.p3 <- mean(RCT$x3==1)
emp.p4 <- mean(RCT$x4==1)
emp.p5 <- mean(RCT$x5==1)
RCT.univariate.p <- list(x1=c("x1",2,0,1,1-emp.p1,emp.p1),
                         x2=c("x2",2,0,1,1-emp.p2,emp.p2),
                         x3=c("x3",2,0,1,1-emp.p3,emp.p3),
                         x4=c("x4",2,0,1,1-emp.p4,emp.p4),
                         x5=c("x5",2,0,1,1-emp.p5,emp.p5))
RCT.summary <- list(ATE_mean = rct.obj$ATE_mean,
                    ATE_se = rct.obj$ATE_se,
                    CATE_mean_se = rct.obj$CATE_mean_se,
                    univariate_p = RCT.univariate.p)

rwd2.obj <- Estimate(Estimator=Estimator, data=RWD,vars_name=vars_name,
                     stratification=strata,stratification_joint=strata_joint)
rct2.obj <- list(data=RCT.summary)
confounders_external <- c("x1","x2","x3")
rwd2.obj$RCTrep(target.obj = rct2.obj,
                confounders_external_name = confounders_external,
                weighting_estimator="Balancing",
                stratification=strata,stratification_joint=strata_joint)
Plot_estimate(source.obj = rwd2.obj, target.obj = rct2.obj)




# ===================================
# ===================================
library(data.table)
source('../../potential_outcomes_subsample/function.R')
source("../../policy_evaluation/main_function.R")
source('../../function/policy_evaluation_function.R')

set.seed(123)

AteRct <- function(year){
  if(year==5){
    Nd1 <- 154; Nc1 <- 1325-773-Nd1; Nt1 <- 1325-Nc1/2; Pd1 <- Nd1/Nt1; ps1 <- (1-98/1522.5)*(1-Pd1); n1 <- Nt1; p1 <- 1-ps1
    Nd0 <- 158; Nc0 <- 1288-735-Nd0; Nt0 <- 1288-Nc0/2; Pd0 <- Nd0/Nt0; ps0 <- (1-129/1517)*(1-Pd0); n0 <- Nt0; p0 <- 1-ps0
    aterct <- p1-p0
  } else{
    Nd1 <- 98;  Nc1 <- 1622-1325-Nd1; Nt1 <- 1622-Nc1/2; Pd1 <- Nd1/Nt1; ps1 <- 1-Pd1; n1 <- Nt1; p1 <- 1-ps1
    Nd0 <- 129; Nc0 <- 1617-1288-Nd0; Nt0 <- 1617-Nc0/2; Pd0 <- Nd0/Nt0; ps0 <- 1-Pd0; n0 <- Nt0; p0 <- 1-ps0
    aterct <- p1-p0
  }
  return(aterct)
}
CateRctByOneStrata <- function(name,value,p1,p0,samplesize1,samplesize0){
  #browser()
  dd <- cbind(p1,samplesize1,p0,samplesize0)
  CATE_mean_se <- data.frame(name=name,
                             value=value,
                             cate= p1-p0,
                             se = apply(dd,1, function(x) sqrt(x[1]*(1-x[1])/x[2]+x[3]*(1-x[3])/x[4])),
                             size = samplesize1+samplesize0,
                             stringsAsFactors = FALSE)
  return(CATE_mean_se)
}

data <- DataImportGeneral(year=20156)
data <- data[!((data$cM==1)|(data$cM=='1A')|(data$cM=='1B')),]
data <- data[(data$Stage==2)|(data$Stage==3),]
data <- data[(data$age_at_diagnosis<87)&(data$age_at_diagnosis>22),]
data <- data[,c("BRAF","RAS","Stage2","age_at_diagnosis","combined_chemo","vitstat","pT","lymph_assessed","male")]
data <- data %>% rename(age=age_at_diagnosis)
data$age <- cut(data$age,breaks = c(min(data$age),50,60,70,max(data$age)),labels=c(1,2,3,4),
                 include.lowest = TRUE,
                 ordered_result = TRUE)

data$combined_chemo <- factor(data$combined_chemo)
data$vitstat <- factor(data$vitstat)
data$weight <- ifelse(data$Stage2==1,1,3)
source.data <- data[sample(1:dim(data)[1],2000),]
target.data <- data[sample(1:dim(data)[1],2000,prob = data$weight),]

CATE_mean_se <- CateRctByOneStrata(name = c("Stage2","Stage2",
                                            "male","male",
                                            "age","age",
                                            "age","age"),
                                   value = c(1,0,1,0,1,2,3,4),
                                   p1 = c(0.161,0.45,0.2,0.17,0.108,0.147,0.196,0.287),
                                   p0 = c(0.185,0.58,0.24,0.211,0.124,0.194,0.254,0.28),
                                   samplesize1 = c(1073,131,1006,616,185,428,678,331),
                                   samplesize0 = c(1073,129,973,644,185,427,673,332))
RCT.univariate.p <- list(Stage=c("Stage2",2,0,1,1-0.91,0.91),
                         male=c("male",2,0,1,1-0.62,0.62),
                         age_at_diagnosis=c("age",4,1,2,3,4,0.11,0.26,0.42,0.21))
target.agg <- list(ATE_mean = AteRct(year=5),
            ATE_se = 0.01,
            CATE_mean_se = CATE_mean_se,
            univariate_p = RCT.univariate.p)
usethis::use_data(source.data,target.data,target.agg,overwrite = TRUE)


# levels of categorical variable should be numeric.
# name and value must be findable in source.
# default: number of strata is smaller than confounders_external.
# confounders_external should no samller than #univariate_p
# levels of categorical variable should be numeric.



confounders_external=c("Stage2","pT","age","BRAF")
source.data %>%
  group_by(across(all_of(confounders_external))) %>% summarise(n=n())
target.data %>%
  group_by(across(all_of(confounders_external))) %>% summarise(n=n())


Estimator <- "IPW"
strata <- c("Stage2","pT")
strata_joint <- TRUE
vars_name <- list(confounders_internal=c("Stage2","age","pT","BRAF"),
                  confounders_external=c("Stage2","age","pT","BRAF"),
                  treatment_name=c('combined_chemo'),
                  outcome_name=c('vitstat')
)
outcome_form <- vitstat~Stage2+age+combined_chemo+pT+
                Stage2:combined_chemo+age:combined_chemo+pT:combined_chemo + pT:Stage2:combined_chemo
strata_cut <- list(age=list(breaks=c(min(data$age),
                                                  50,60,70,max(data$age)),
                                         labels=c(1,2,3,4)),
                   lymph_assessed=list(breaks=c(min(data$lymph_assessed),
                                                7,10,max(data$lymph_assessed))))


fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           ## classProbs = TRUE,
                           ## Evaluate performance using
                           ## the following function
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE)

rpartGrid <- expand.grid(maxdepth=5,cp=0.0001)

output <- RCTREP(Estimator="G_computation",
                 source.data=source.data, target.data=target.data,
                 vars_name=vars_name,
                 stratification=strata,stratification_joint=TRUE)


summary(source.obj = output$source.obj, target.obj = output$target.obj)
p

source.obj <- output$source.obj
source.obj$residual_check(stratification = strata)
source.obj$plot_CATE(stratification=c("Stage2","pT","BRAF"),stratification_joint = TRUE)



library(RCTrep)
library(dplyr)

## data preparation
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

## input
Estimator <- "G_computation"
strata <- c("Stage2","pT")
#strata_joint <- TRUE
vars_name <- list(confounders_internal=c("Stage2","pT","age"),
                  confounders_external=c("Stage2","pT","age"),
                  treatment_name=c('combined_chemo'),
                  outcome_name=c('vitstat')
)

source.data %>%
  group_by(across(all_of(vars_name$confounders_external))) %>% summarise(n=n())
target.data %>%
  group_by(across(all_of(vars_name$confounders_external))) %>% summarise(n=n())

# core function
output <- RCTREP(Estimator=Estimator, outcome_method="gam",
                 source.data=source.data, target.data=target.data,vars_name=vars_name,
                 stratification=strata)
summary(source.obj = output$source.obj, target.obj = output$target.obj)


a <- summary(source.obj = output$source.obj, target.obj = output$target.obj)









to do list:
  dplyr get intersect of two dataframe, get row index of two dataframe,
  when implememt RCTrep(), dataset of source should be larger than target.



