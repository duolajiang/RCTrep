######################################
# DGM for simulate data in use
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
target.data <- RCTrep::DGM(trial=TRUE,  n_rct, var_name, p_success_rct, tau, y0, log.ps=0, binary = FALSE, noise=1, rho1, rho2)
source.data <- RCTrep::DGM(trial=FALSE, n_rwd, var_name, p_success_rwd, tau, y0, log.ps,   binary = FALSE, noise=1, rho1, rho2)

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
