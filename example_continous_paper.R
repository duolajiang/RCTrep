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


##########################################################
## Working example in section 4: a first working example, generate plot_exa.pdf
##########################################################
library(RCTrep)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

source.obj <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  name = "RWD",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  data.public = TRUE
)

target.obj <- TEstimator_wrapper(
  Estimator = "Crude",
  data = target.data,
  name = "RCT",
  vars_name = vars_name,
  data.public = TRUE,
  isTrial = TRUE
)

source.obj.rep <- SEstimator_wrapper(estimator="Exact",
                                target.obj=target.obj,
                                source.obj=source.obj,
                                confounders_sampling_name=c("x1","x2","x3","x4","x5","x6"))
source.obj.rep$EstimateRep(stratification = c("x1","x3","x4","x5"))

destination <- '~lshen/Downloads/plot_wexa_obj_source_model_summary.pdf'
pdf(file=destination, width = 18, height = 5)
source.obj$summary()
dev.off()

destination <- '~lshen/Downloads/plot_wexa_obj_source_t_overlap.pdf'
pdf(file=destination, width = 7, height = 4)
source.obj$diagnosis_t_overlap()
dev.off()

destination <- '~lshen/Downloads/plot_wexa_obj_target_t_overlap.pdf'
pdf(file=destination, width = 7, height = 4)
target.obj$diagnosis_t_overlap()
dev.off()

destination <- '~lshen/Downloads/plot_wexa_obj_sourcerep_s_overlap.pdf'
pdf(file=destination, width = 7, height = 4)
source.obj.rep$diagnosis_s_overlap()
dev.off()

#target.obj$diagnosis_y_overlap(stratification = c("x1","x3"))
#source.obj$diagnosis_y_overlap(stratification = c("x1","x3"))

fusion <- Summary$new(target.obj,
                      source.obj,
                      source.obj.rep)
destination <- '~lshen/Downloads/plot_wexa_compare.pdf'
pdf(file=destination, width = 7, height = 4)
fusion$plot()
dev.off()

fusion$evaluate()

# ==============================================================
# ==============================================================
# ==============================================================
source.obj.gc <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  name = "RWD",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  data.public = TRUE
)

source.obj.ipw <- TEstimator_wrapper(
  Estimator = "IPW",
  data = source.data,
  name = "RWD",
  vars_name = vars_name,
  treatment_method = "glm",
  treatment_formula = z ~ x1 + x2 + x3 + x4 + x5 + x6 + x1:x2 + x3:x4,
  data.public = TRUE
)

source.obj.dr <- TEstimator_wrapper(
  Estimator = "DR",
  data = source.data,
  name = "RWD",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  treatment_method = "glm",
  treatment_formula = z ~ x1 + x2 + x3 + x4 + x5 + x6 + x1:x2 + x3:x4,
  data.public = TRUE
)

target.obj <- TEstimator_wrapper(
  Estimator = "Crude",
  data = target.data,
  name = "RCT",
  vars_name = vars_name,
  data.public = TRUE,
  isTrial = TRUE
)

strata <- c("x1","x4")
confounders_sampling_name <- c("x2","x6")
source.gc.exact <- SEstimator_wrapper(estimator="Exact",target.obj=target.obj, source.obj=source.obj.gc,confounders_sampling_name=confounders_sampling_name)
source.gc.exact$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.gc.isw <- SEstimator_wrapper(estimator="ISW",target.obj=target.obj, source.obj=source.obj.gc,confounders_sampling_name=confounders_sampling_name, method="glm")
source.gc.isw$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.gc.subclass <- SEstimator_wrapper(estimator="Subclass",target.obj=target.obj, source.obj=source.obj.gc,confounders_sampling_name=confounders_sampling_name)
source.gc.subclass$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.ipw.exact <- SEstimator_wrapper(estimator="Exact",target.obj=target.obj, source.obj=source.obj.ipw,confounders_sampling_name=confounders_sampling_name)
source.ipw.exact$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.ipw.isw <- SEstimator_wrapper(estimator="ISW",target.obj=target.obj, source.obj=source.obj.ipw,confounders_sampling_name=confounders_sampling_name, method="glm")
source.ipw.isw$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.ipw.subclass <- SEstimator_wrapper(estimator="Subclass",target.obj=target.obj, source.obj=source.obj.ipw,confounders_sampling_name=confounders_sampling_name)
source.ipw.subclass$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.dr.exact <- SEstimator_wrapper(estimator="Exact",target.obj=target.obj, source.obj=source.obj.dr,confounders_sampling_name=confounders_sampling_name)
source.dr.exact$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.dr.isw <- SEstimator_wrapper(estimator="ISW",target.obj=target.obj, source.obj=source.obj.dr,confounders_sampling_name=confounders_sampling_name, method="glm")
source.dr.isw$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.dr.subclass <- SEstimator_wrapper(estimator="Subclass",target.obj=target.obj, source.obj=source.obj.dr,confounders_sampling_name=confounders_sampling_name)
source.dr.subclass$EstimateRep(stratification = strata, stratification_joint = TRUE)

fusion <- Summary$new(target.obj,
                      #source.obj.gc,
                      source.gc.exact,
                      source.gc.isw,
                      source.gc.subclass,
                      #source.obj.ipw,
                      source.ipw.exact,
                      source.ipw.isw,
                      source.ipw.subclass,
                      #source.obj.dr,
                      source.dr.exact,
                      source.dr.isw,
                      source.dr.subclass)

destination <- '~lshen/Downloads/plot_all_compares.pdf'
pdf(file=destination, width = 14, height = 7)
fusion$plot()
dev.off()

fusion$print()
fusion$evaluate()


######################################################################################
## Example in the introduction: patient-level data are allowed to share, generate plot_exa_1.pdf #######
######################################################################################
library(RCTrep)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

output <- RCTREP(TEstimator="G_computation", SEstimator = "Exact",
                 source.data=source.data, target.data=target.data,
                 vars_name=vars_name,
                 outcome_method = "glm", outcome_formula = y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
                 confounders_sampling_name = c("x1","x2","x3","x4","x5","x6"),
                 stratification = c("x1","x3","x4","x5"), stratification_joint = TRUE)

fusion <- Summary$new(output$target.obj,
                      output$source.obj,
                      output$source.rep.obj)

fusion$evaluate()

destination <- '~lshen/Downloads/plot_exa_intro.pdf'
pdf(file=destination, width = 8, height = 5)
fusion$plot()
dev.off()


######################################################################################
## Working Example 1: patient-level data are allowed to share, generate plot_exa_1.pdf #######
######################################################################################
strata <- c("x1","x4")
confounders_sampling_name <- c("x2","x6")
output <- RCTREP(TEstimator="G_computation", SEstimator = "Exact",
                 outcome_method = "glm",
                 source.name = "RWD", target.name = "RCT",
                 outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
                 source.data=source.data, target.data=target.data,
                 vars_name=vars_name,
                 confounders_sampling_name = confounders_sampling_name,
                 stratification = strata, stratification_joint = TRUE)

fusion <- Summary$new(output$target.obj,
                      output$source.obj,
                      output$source.rep.obj)

destination <- '~lshen/Downloads/plot_exa_1.pdf'
pdf(file=destination, width = 8, height = 5)
fusion$plot()
dev.off()

fusion$print()
fusion$evaluate()

##########################################################################################
## example 2: patient-level data are not allowed to share, generate plot_exa_2.pdf #######
##########################################################################################
library(geex)
library(caret)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

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

class(source.obj)
head(source.obj$data)

strata <- c("x1","x4")
source.rep.obj <- SEstimator_wrapper(estimator="Eexact_pp",
                                     target.obj=target.obj,
                                     source.obj=source.obj,
                                     confounders_sampling_name=c("x2","x6"))
source.rep.obj$EstimateRep(stratification = strata, stratification_joint = FALSE)
fusion <- Summary$new(target.obj,
                      source.obj,
                      source.rep.obj)

destination <- '~lshen/Downloads/plot_exa_2.pdf'
pdf(file=destination, width = 7, height = 4)
fusion$plot()
dev.off()

fusion$print()
fusion$evaluate()

##########################################################
## example 3: evaluation using marginal distribution and estimates of
## average treatment effect marginalizing on the univariate covariate
##########################################################
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

target.obj <- TEstimator_wrapper(
  Estimator = "Crude",
  data = target.data,
  vars_name = vars_name,
  name = "RCT",
  data.public = FALSE,
  isTrial = TRUE
)

emp.p1 <- mean(target.data$x1)
emp.p2 <- mean(target.data$x2)
emp.p3 <- mean(target.data$x3)
emp.p4 <- mean(target.data$x4)
emp.p5 <- mean(target.data$x5)
emp.p6 <- mean(target.data$x6)

#vars_rct <- c("x1","x2","x3")
vars_rct <- c("x1","x2","x3","x4","x5","x6")

t.d <- target.data[,vars_rct]
n <- dim(source.data)[1]
pw.cor <- gdata::upperTriangle(cor(t.d), diag = FALSE, byrow = TRUE)
myCop <- copula::normalCopula(param=pw.cor,
                      dim = 6, dispstr = "un")
myMvd <- copula::mvdc(copula=myCop,
                      margins = c("binom","binom","binom","binom","binom","binom"),
                      paramMargins=list(list(1, emp.p1),
                                        list(1, emp.p2),
                                        list(1, emp.p3),
                                        list(1, emp.p4),
                                        list(1, emp.p5),
                                        list(1, emp.p6)))
synthetic.data <- copula::rMvdc(n, myMvd)
synthetic.data <- data.frame(x1=synthetic.data[,1],
                             x2=synthetic.data[,2],
                             x3=synthetic.data[,3],
                             x4=synthetic.data[,4],
                             x5=synthetic.data[,5],
                             x6=synthetic.data[,6])

apply(synthetic.data, 2, mean)
c(mean(target.data$x1),mean(target.data$x2),mean(target.data$x3),
  mean(target.data$x4),mean(target.data$x5),mean(target.data$x6))

RCT.summary <- list(ATE_mean = target.obj$estimates$ATE$est,
                    ATE_se = target.obj$estimates$ATE$se,
                    CATE_mean_se = target.obj$get_CATE(vars_rct,FALSE))

synthetic.data <- synthetic.data %>%
  arrange(across(vars_rct))

synthetic.data <- semi_join(synthetic.data, source.data, by = vars_rct)
source.data <- semi_join(source.data, synthetic.data, by = vars_rct)
synthetic.data %>% group_by(across(all_of(vars_rct))) %>% summarise(n=n())
source.data %>% group_by(across(all_of(vars_rct))) %>% summarise(n=n())

apply(synthetic.data, 2, mean)
c(emp.p3,emp.p4,emp.p5)

target.obj <- Synthetic_TEstimator$new(df = synthetic.data,
                                       estimates=RCT.summary,
                                       vars_name = vars_rct,
                                       name = "RCT",
                                       isTrial = TRUE,
                                       data.public = FALSE)
target.obj$data

source.obj <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  name = "RWD",
  data.public = FALSE
)

source.rep.obj <- SEstimator_wrapper(estimator="Exact_pp",
                                     target.obj=target.obj,
                                     source.obj=source.obj,
                                     confounders_sampling_name=c("x2","x6"))
source.rep.obj$EstimateRep(stratification = vars_rct, stratification_joint = FALSE)
fusion <- Summary$new(target.obj,
                      source.obj,
                      source.rep.obj)

destination <- '~lshen/Downloads/plot_exa_3.pdf'
pdf(file=destination, width = 7, height = 4)
fusion$plot()
dev.off()

fusion$evaluate()

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

source.obj.1 <- SEstimator_wrapper(estimator="Exact_pp",target.obj=target.obj, source.obj=source.obj,confounders_sampling_name=c("x2","x6"))
source.obj.1$EstimateRep(stratification = c("x1","x2","x3","x4","x5","x6"), stratification_joint = FALSE)

source.obj.2 <- SEstimator_wrapper(estimator="Exact_pp",target.obj=target.obj, source.obj=source.obj,confounders_sampling_name= c("x1","x2","x3","x4","x5","x6"))
source.obj.2$EstimateRep(stratification = c("x1","x2","x3","x4","x5","x6"), stratification_joint = FALSE)

fusion <- Summary$new(target.obj,
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

output <- RCTREP(Estimator="G_computation", weighting_estimator = "Balancing",
                 outcome_method = "glm",
                 outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
                 source.data=source.data, target.data=target.data, vars_name=vars_name,
                 stratification = strata, stratification_joint = strata_joint,
                 data.public = data.public)
output$source.obj$summary()
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

vars_name <- list(confounders_treatment_name=c("x1","x2"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

target.obj <- TEstimator_wrapper(
  Estimator = "IPW",
  data = target.data,
  name = "RWD",
  vars_name = vars_name,
  treatment_method = "BART",
  data.public = TRUE
)

target.obj <- TEstimator_wrapper(
  Estimator = "DR",
  data = target.data,
  name = "RWD",
  vars_name = vars_name,
  treatment_method = "glm",
  outcome_method = "glm",
  data.public = TRUE,
  two_models = TRUE
)

target.obj$plot_CATE()
target.obj$get_CATE()
