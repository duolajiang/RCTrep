##########################################################
## example 4: effects of adjustment sets on efficiency
##########################################################
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

source.obj <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  name = "RWD",
  data.public = FALSE
)

target.obj <- TEstimator_wrapper(
  Estimator = "Crude",
  data = target.data,
  vars_name = vars_name,
  name = "RCT",
  data.public = FALSE,
  isTrial = TRUE
)

source.obj.1 <- SEstimator_wrapper(estimator="Exact",target.obj=target.obj, source.obj=source.obj,confounders_sampling_name=c("x2","x6"))
source.obj.1$EstimateRep(stratification = c("x1","x2","x3","x4","x5","x6"), stratification_joint = FALSE)

source.obj.2 <- SEstimator_wrapper(estimator="Exact",target.obj=target.obj, source.obj=source.obj,confounders_sampling_name= c("x1","x2","x3","x4","x5","x6"))
source.obj.2$EstimateRep(stratification = c("x1","x2","x3","x4","x5","x6"), stratification_joint = FALSE)

fusion <- Fusion$new(target.obj,
                     source.obj,
                     source.obj.1,
                     source.obj.2)

destination <- '~lshen/Downloads/plot_exa_4.pdf'
pdf(file=destination, width = 10, height = 7)
fusion$plot()
dev.off()

fusion$evaluate()



# how to distinguish the discrepancy is
# a) due to unmeasured confounders (if we have unmeasured omitted variables, how to test that we miss this variable) or
# b) due to heterogeneity.
# it seems as long as we do not miss important predictive variable, confounding is not a big problem.


##########################################################
## example 5: differ in parameters in DGM
##########################################################
library(dplyr)
source.data <- RCTrep::source.data.2
target.data <- RCTrep::target.data.2

vars_name <- list(confounders_treatment=c("x1","x2","x3","x4","x5","x6"),
                  confounders_sampling=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

# select variables confounders_treatment
# order data according to the confounders_treatment
source.data <- source.data %>%
  select(vars_name$confounders_treatment,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment))

target.data <- target.data %>%
  select(vars_name$confounders_treatment,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment))

# step 1: check z-overlap assumptions of each data set
# check overlap of probability of receiving treatment given confounders_treatment
# filter subgroups with no overlap in treatment/control
source.data <- source.data %>% group_by(across(all_of(vars_name$confounders_treatment))) %>%
  mutate(pt=sum(z)/n()) %>%
  ungroup() %>%
  filter((pt!=1)&(pt!=0))
target.data <- target.data %>% group_by(across(all_of(vars_name$confounders_treatment))) %>%
  mutate(pt=sum(z)/n()) %>%
  ungroup() %>%
  filter((pt!=1)&(pt!=0))

# step 2: check S-overlap assumptions, namely the probability of being sampled given confounders_external
# if two dataset can be combined, then we check p(s=1|x)
# if two dataset can't be combined, then we check the overlap of p(x), and select intersect of two dataset
source.data <- semi_join(source.data, target.data, by = vars_name$confounders_sampling)
target.data <- semi_join(target.data, source.data, by = vars_name$confounders_sampling)
source.data %>% group_by(across(all_of(vars_name$confounders_sampling))) %>% summarise(n=n())
target.data %>% group_by(across(all_of(vars_name$confounders_sampling))) %>% summarise(n=n())

# for the remaining analysis, data should be data.frame type
source.data <- as.data.frame(source.data)
target.data <- as.data.frame(target.data)

Estimator <- "G_computation"
strata <- c("x1","x2","x3","x4","x5","x6")
strata_joint <- TRUE
data.public <- TRUE
vars_name <- list(confounders_treatment=c("x1","x2","x3","x4","x5","x6"),
                  confounders_sampling=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

output <- RCTREP(TEstimator="G_computation", SEstimator = "Exact",
                 outcome_method = "glm",
                 outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
                 source.data=source.data, target.data=target.data, vars_name=vars_name,
                 stratification = strata, stratification_joint = strata_joint,
                 data.public = data.public)
output$source.obj$diagnosis_t_ignorability
summary(target.obj = output$target.obj, source.obj = output$source.obj)

##########################################################
## example 6: binary outcome, is more complex
##########################################################
n_rct <- 10000; n_rwd <- 10000
var_name <- c("x1","x2","x3","x4","x5","x6")
p_success_rct <- c(0.7,0.7,0.2,0.3,0.2,0.3)
p_success_rwd <- c(0.2,0.4,0.8,0.8,0.7,0.8)
tau_rct <- "x2+100*x6+1"
tau_rwd <- "x2+100*x6+1"
y0_rct <- "x1"
y0_rwd <- "x1"
log.ps <- "x2+x6+x1"
rho1 <- c("x1","x2",0)
rho2 <- c("x2","x3",0)
# ===================================
# simulating data
target.data <- RCTrep::DGM(trial=TRUE,  n_rct, var_name, p_success_rct, tau_rct, y0_rct, log.ps=0, binary = TRUE, noise=1, rho1)
source.data <- RCTrep::DGM(trial=FALSE, n_rwd, var_name, p_success_rwd, tau_rwd, y0_rwd, log.ps,   binary = TRUE, noise=1, rho1)
vars_name <- list(confounders_treatment=c("x1","x2","x3","x4","x5","x6"),
                  confounders_sampling=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

library(dplyr)
source.data <- source.data %>%
  select(vars_name$confounders_treatment,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment))

target.data <- target.data %>%
  select(vars_name$confounders_treatment,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment))
source.data <- source.data %>% group_by(across(all_of(vars_name$confounders_treatment))) %>%
  mutate(pt=sum(z)/n()) %>%
  ungroup() %>%
  filter((pt!=1)&(pt!=0))
target.data <- target.data %>% group_by(across(all_of(vars_name$confounders_treatment))) %>%
  mutate(pt=sum(z)/n()) %>%
  ungroup() %>%
  filter((pt!=1)&(pt!=0))
source.data <- semi_join(source.data, target.data, by = vars_name$confounders_sampling)
target.data <- semi_join(target.data, source.data, by = vars_name$confounders_sampling)
source.data %>% group_by(across(all_of(vars_name$confounders_sampling))) %>% summarise(n=n())
target.data %>% group_by(across(all_of(vars_name$confounders_sampling))) %>% summarise(n=n())
source.data %>% group_by(across(all_of(c(vars_name$confounders_treatment,vars_name$treatment_name)))) %>%
  summarise(ybar=mean(y=="1"), obs=n())
target.data %>% group_by(across(all_of(c(vars_name$confounders_treatment,vars_name$treatment_name)))) %>%
  summarise(ybar=mean(y=="1"), obs=n())

# for the remaining analysis, data should be data.frame type
source.data <- as.data.frame(source.data)
target.data <- as.data.frame(target.data)

Estimator <- "G_computation"
strata <- c("x3","x4","x5")
strata_joint <- TRUE
data.public <- TRUE
vars_name <- list(confounders_treatment=c("x1","x2","x3","x4","x5","x6"),
                  confounders_sampling=c("x1","x2","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

output <- RCTREP(Estimator=Estimator, weighting_estimator = "Balancing",
                 #treatment_method = "glm",
                 outcome_method = "glm",
                 #treatment_formula= z ~ x1:x2 + x3:x4+ x5+ x6,
                 outcome_formula = y ~ x1 + z + x2:z + x6:z,
                 source.data=source.data, target.data=target.data, vars_name=vars_name,
                 stratification = strata, stratification_joint = strata_joint,
                 data.public = data.public)
summary(target.obj = output$target.obj, source.obj = output$source.obj)

# remaining_sets <- vars_name$confounders_treatment[!vars_name$confounders_treatment %in% vars_name$confounders_sampling]
# source.data %>% group_by(across(all_of(remaining_sets))) %>% summarise_at(vars_name$confounders_sampling,mean)

##########################################################
## testing: using BART modeling approach
##########################################################
library(RCTrep)
library(ggplot2)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

source.data$y <- ifelse(source.data$y<2,0,1)

source.obj <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  name = "RWD",
  vars_name = vars_name,
  outcome_method = "BART",
  data.public = TRUE,
  ntree = 50
)
source.obj$summary(stratification = c("x1","x2"))
source.obj$diagnosis_t_overlap(stratification = c("x1","x2"))
source.obj$diagnosis_y_overlap(stratification = c("x1","x2"))


##########################################################
## testing: using BART modeling approach for IPW and DR estimator
##########################################################
library(RCTrep)
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
target.data <- RCTrep::DGM(trial=TRUE,  n_rct, var_name, p_success_rct, tau, y0, log.ps=0, binary = TRUE, noise=1, rho1, rho2)

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)



target.obj <- TEstimator_wrapper(
  Estimator = "IPW",
  data = target.data,
  name = "RWD",
  vars_name = vars_name,
  treatment_method = "BART",
  data.public = TRUE,
  strata_cut = list(x1 = list(breaks = c(min(target.data$x1),max(target.data$x1)),
                              labels = c(1)))
)

source.obj <- TEstimator_wrapper(
  Estimator = "DR",
  data = target.data,
  name = "RWD2",
  vars_name = vars_name,
  treatment_method = "BART",
  outcome_method = "logreg",
  data.public = TRUE
  #two_models = TRUE
)

#target.obj$plot_CATE()
target.obj$plot_CATE()
target.obj$diagnosis_t_overlap()
target.obj$diagnosis_y_overlap()
fusion <- Fusion$new(target.obj,
                     source.obj,
                     stratification=c("x1","x2"),
                     stratification_joint = TRUE)
fusion$plot()


