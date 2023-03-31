######################################
# simulating continuous outcome data in use
######################################
library(MASS)
library(mvtnorm)
library(dplyr)

set.seed(123)
# ===================================
n_rct <- 5000; n_rwd <- 5000
var_name <- c("x1","x2","x3","x4","x5","x6")
p_success_rct <- c(0.7,0.9,0.2,0.3,0.2,0.3)
p_success_rwd <- c(0.2,0.2,0.8,0.8,0.7,0.8)
tau <- "6*x2+x6+2"
y0 <- "x1"
log.ps <- "x1*x2+x3*x4+5*x5+x6"
rho1 <- c("x1","x2",0)
rho2 <- c("x2","x3",0)

# simulating data
target.data <- RCTrep::DGM(trial=TRUE,  n_rct,
                           var_name, p_success_rct,
                           tau, y0, log.ps=0,
                           binary = FALSE, noise=1,
                           rho1, rho2)
source.data <- RCTrep::DGM(trial=FALSE, n_rwd,
                           var_name, p_success_rwd,
                           tau, y0, log.ps,
                           binary = FALSE, noise=1,
                           rho1, rho2)

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)
confounders_sampling_name <- vars_name$confounders_treatment_name

#### data preprocess
# select variables confounders_treatment
# order data according to the confounders_treatment
# when some weird error happen, restart R, and rerun.
source.data <- source.data %>%
  select(vars_name$confounders_treatment_name,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment_name))

target.data <- target.data %>%
  select(vars_name$confounders_treatment_name,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment_name))

# step 1: check z-overlap assumption of each data set
# check overlap of probability of receiving treatment given confounders_treatment_name
# filter subgroups with no overlap in treatment/control
source.data <- source.data %>% group_by(across(all_of(vars_name$confounders_treatment_name))) %>%
  mutate(pt=sum(z)/n()) %>%
  ungroup() %>%
  filter((pt!=1)&(pt!=0))
target.data <- target.data %>% group_by(across(all_of(vars_name$confounders_treatment_name))) %>%
  mutate(pt=sum(z)/n()) %>%
  ungroup() %>%
  filter((pt!=1)&(pt!=0))

# step 2: check S-overlap assumptions, namely the probability of being sampled given confounders_sampling
# if two dataset can be combined, then we check p(s=1|x), where x is the confounders_sampling
# if two dataset can't be combined, then we check the overlap of p(x), and select intersect of two dataset
source.data <- semi_join(source.data, target.data, by = confounders_sampling_name)
target.data <- semi_join(target.data, source.data, by = confounders_sampling_name)
source.data %>% group_by(across(all_of(confounders_sampling_name))) %>% summarise(n=n())
target.data %>% group_by(across(all_of(confounders_sampling_name))) %>% summarise(n=n())

source.sum <- source.data %>% group_by(across(all_of(confounders_sampling_name))) %>% summarise(n=n())
target.sum <- target.data %>% group_by(across(all_of(confounders_sampling_name))) %>% summarise(n=n())
a <- semi_join(source.sum, target.sum, by = confounders_sampling_name) %>% select(vars_name$confounders_treatment_name)
b <- semi_join(target.sum, source.sum, by = confounders_sampling_name) %>% select(vars_name$confounders_treatment_name)
identical(a,b)

# for the remaining analysis, data should be data.frame type
source.data <- as.data.frame(source.data)
target.data <- as.data.frame(target.data)

usethis::use_data(source.data,target.data,overwrite = TRUE)


######################################
# simulating binary outcome data
######################################
library(MASS)
library(mvtnorm)
library(dplyr)

set.seed(123)
# ===================================
n_rct <- 5000; n_rwd <- 5000
var_name <- c("x1","x2","x3","x4","x5","x6")
p_success_rct <- c(0.7,0.9,0.2,0.3,0.2,0.3)
p_success_rwd <- c(0.2,0.2,0.8,0.8,0.7,0.8)
tau <- "6*x2+x6+2"
y0 <- "x1"
log.ps <- "x1*x2+x3*x4+5*x5+x6"
rho1 <- c("x1","x2",0)
rho2 <- c("x2","x3",0)

# simulating data
target.binary.data <- RCTrep::DGM(trial=TRUE,  n_rct, var_name, p_success_rct, tau, y0, log.ps=0, binary = TRUE, noise=1, rho1, rho2)
source.binary.data <- RCTrep::DGM(trial=FALSE, n_rwd, var_name, p_success_rwd, tau, y0, log.ps,   binary = TRUE, noise=1, rho1, rho2)

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)
confounders_sampling_name <- vars_name$confounders_treatment_name

#### data preprocess
# select variables confounders_treatment
# order data according to the confounders_treatment
# when some weird error happen, restart R, and rerun.
source.binary.data <- source.binary.data %>%
  select(vars_name$confounders_treatment_name,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment_name))

target.binary.data <- target.binary.data %>%
  select(vars_name$confounders_treatment_name,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment_name))

# step 1: check z-overlap assumption of each data set
# check overlap of probability of receiving treatment given confounders_treatment_name
# filter subgroups with no overlap in treatment/control
source.binary.data <- source.binary.data %>%
  group_by(across(all_of(vars_name$confounders_treatment_name))) %>%
  mutate(pt=sum(z)/n()) %>%
  ungroup() %>%
  filter((pt!=1)&(pt!=0))
target.binary.data <- target.binary.data %>%
  group_by(across(all_of(vars_name$confounders_treatment_name))) %>%
  mutate(pt=sum(z)/n()) %>%
  ungroup() %>%
  filter((pt!=1)&(pt!=0))

# step 2: check S-overlap assumptions, namely the probability of being sampled given confounders_sampling
# if two dataset can be combined, then we check p(s=1|x), where x is the confounders_sampling
# if two dataset can't be combined, then we check the overlap of p(x), and select intersect of two dataset
source.binary.data <- semi_join(source.binary.data, target.binary.data, by = confounders_sampling_name)
target.binary.data <- semi_join(target.binary.data, source.binary.data, by = confounders_sampling_name)
source.binary.data %>% group_by(across(all_of(confounders_sampling_name))) %>% summarise(n=n())
target.binary.data %>% group_by(across(all_of(confounders_sampling_name))) %>% summarise(n=n())

source.sum <- source.binary.data %>%
  group_by(across(all_of(confounders_sampling_name))) %>%
  summarise(n=n())
target.sum <- target.binary.data %>%
  group_by(across(all_of(confounders_sampling_name))) %>%
  summarise(n=n())
a <- semi_join(source.sum, target.sum, by = confounders_sampling_name) %>%
  select(vars_name$confounders_treatment_name)
b <- semi_join(target.sum, source.sum, by = confounders_sampling_name) %>%
  select(vars_name$confounders_treatment_name)
identical(a,b)

# for the remaining analysis, data should be data.frame type
source.binary.data <- as.data.frame(source.binary.data)
target.binary.data <- as.data.frame(target.binary.data)

usethis::use_data(source.binary.data,target.binary.data,overwrite = TRUE)


######################################
# Simulate synthetic QUASAR data
######################################
#' the function compute two-year and five year treatment effect
AteRct <- function(year){
  # if(year==5){
  #   Nd1 <- 154; Nc1 <- 1325-773-Nd1; Nt1 <- 1325-Nc1/2; Pd1 <- Nd1/Nt1; ps1 <- (1-98/1522.5)*(1-Pd1); n1 <- Nt1; p1 <- 1-ps1
  #   Nd0 <- 158; Nc0 <- 1288-735-Nd0; Nt0 <- 1288-Nc0/2; Pd0 <- Nd0/Nt0; ps0 <- (1-129/1517)*(1-Pd0); n0 <- Nt0; p0 <- 1-ps0
  #   aterct <- p1-p0
  # } else{
  #   Nd1 <- 98;  Nc1 <- 1622-1325-Nd1; Nt1 <- 1622-Nc1/2; Pd1 <- Nd1/Nt1; ps1 <- 1-Pd1; n1 <- Nt1; p1 <- 1-ps1
  #   Nd0 <- 129; Nc0 <- 1617-1288-Nd0; Nt0 <- 1617-Nc0/2; Pd0 <- Nd0/Nt0; ps0 <- 1-Pd0; n0 <- Nt0; p0 <- 1-ps0
  #   aterct <- p1-p0
  # }
  if(year==5){
    Nd1 <- 154; Nc1 <- 1325-773-Nd1; Nt1 <- 1325-Nc1/2; Pd1 <- Nd1/Nt1; ps1 <- (1-98/1522.5)*(1-Pd1); n1 <- Nt1; p1 <- ps1
    Nd0 <- 158; Nc0 <- 1288-735-Nd0; Nt0 <- 1288-Nc0/2; Pd0 <- Nd0/Nt0; ps0 <- (1-129/1517)*(1-Pd0); n0 <- Nt0; p0 <- ps0
    aterct <- p1-p0
  } else{
    Nd1 <- 98;  Nc1 <- 1622-1325-Nd1; Nt1 <- 1622-Nc1/2; Pd1 <- Nd1/Nt1; ps1 <- 1-Pd1; n1 <- Nt1; p1 <- ps1
    Nd0 <- 129; Nc0 <- 1617-1288-Nd0; Nt0 <- 1617-Nc0/2; Pd0 <- Nd0/Nt0; ps0 <- 1-Pd0; n0 <- Nt0; p0 <- ps0
    aterct <- p1-p0
  }
  return(aterct)
}

#' the resulting data frame should have the following format:
#' data.frame(name,
#'            value,
#'            y1.hat,
#'            y0.hat,
#'            cate,
#'            se,
#'            size)
#' the same as the TEstimator$CATE
CateRctByOneStrata <- function(name,value,p1,p0,samplesize1,samplesize0){
  #browser()
  dd <- cbind(p1,samplesize1,p0,samplesize0)
  CATE_mean_se <- data.frame(name=name,
                             value=value,
                             y1.hat = p1,
                             y0.hat = p0,
                             cate= p1-p0,
                             se = apply(dd,1, function(x) sqrt(x[1]*(1-x[1])/x[2]+x[3]*(1-x[3])/x[4])),
                             size = samplesize1+samplesize0,
                             stringsAsFactors = FALSE)
  return(CATE_mean_se)
}

CATE_mean_se <- CateRctByOneStrata(name = c("Stage2","Stage2",
                                            "male","male",
                                            "age","age",
                                            "age","age"),
                                   value = c(1,0,1,0,1,2,3,4),
                                   p1 = c(0.161,0.45,0.2,0.17,0.108,0.147,0.196,0.287),
                                   p0 = c(0.185,0.58,0.24,0.211,0.124,0.194,0.254,0.28),
                                   samplesize1 = c(1073,131,1006,616,185,428,678,331),
                                   samplesize0 = c(1073,129,973,644,185,427,673,332))
RCT.univariate.p <- list(Stage2=c("Stage2",2,0,1,1-0.91,0.91),
                         male=c("male",2,0,1,1-0.62,0.62),
                         age=c("age",4,1,2,3,4,0.11,0.26,0.42,0.21))
quasar.agg <- list(ATE_mean = AteRct(year=5),
                   ATE_se = 0.01,
                   CATE_mean_se = CATE_mean_se,
                   univariate_p = RCT.univariate.p,
                   n = 2406)

quasar.synthetic <- RCTrep::GenerateSyntheticData(margin_dis="bernoulli_categorical",
                                                  N=3239,#dim(source.data)[1],
                                                  margin=RCT.univariate.p,
                                                  var_name=c("Stage2","male","age"))

quasar.synthetic$age <- as.factor(quasar.synthetic$age)

quasar.obj <- RCTrep:::TEstimator_Synthetic$new(data = quasar.synthetic,
                                       estimates=quasar.agg,
                                       vars_name = list(confounders_treatment_name = c("Stage2","male","age")),
                                       name = "RCT",
                                       isTrial = TRUE,
                                       data.public = TRUE)

usethis::use_data(quasar.agg,quasar.synthetic,quasar.obj,overwrite = TRUE)

