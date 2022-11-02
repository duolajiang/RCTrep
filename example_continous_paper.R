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

source.obj.rep <- SEstimator_wrapper(Estimator="Exact",
                                target.obj=target.obj,
                                source.obj=source.obj,
                                confounders_sampling_name=c("x2","x6"))
source.obj.rep$EstimateRep(stratification = c("x1","x3","x4","x5"))

destination <- '~lshen/Downloads/plot_wexa_obj_source_model_summary.pdf'
pdf(file=destination, width = 18, height = 5)
source.obj$diagnosis_t_ignorability()
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

destination <- 'man/figures/plot_wexa_obj_sourcerep_s_ignorability.pdf'
pdf(file=destination, width = 7, height = 4)
source.obj.rep$diagnosis_s_ignorability()
dev.off()


#target.obj$diagnosis_y_overlap(stratification = c("x1","x3"))
#source.obj$diagnosis_y_overlap(stratification = c("x1","x3"))

fusion <- Fusion$new(target.obj,
                      source.obj,
                      source.obj.rep)
destination <- '~lshen/Downloads/plot_wexa_compare.pdf'
pdf(file=destination, width = 7, height = 4)
fusion$plot()
dev.off()

fusion$evaluate()

# ==============================================================
# ==============================================================
# Example 1, validation at scale.
# ==============================================================
library(RCTrep)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

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

source.gc.exact <- SEstimator_wrapper(Estimator="Exact",
                                      target.obj=target.obj,
                                      source.obj=source.obj.gc,
                                      confounders_sampling_name=
                                        confounders_sampling_name)
source.gc.exact$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.gc.isw <- SEstimator_wrapper(Estimator="ISW",
                                    target.obj=target.obj,
                                    source.obj=source.obj.gc,
                                    confounders_sampling_name=
                                      confounders_sampling_name,
                                    method="glm")
source.gc.isw$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.gc.subclass <- SEstimator_wrapper(Estimator="Subclass",
                                         target.obj=target.obj,
                                         source.obj=source.obj.gc,
                                         confounders_sampling_name=
                                           confounders_sampling_name)
source.gc.subclass$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.ipw.exact <- SEstimator_wrapper(Estimator="Exact",
                                       target.obj=target.obj,
                                       source.obj=source.obj.ipw,
                                       confounders_sampling_name=
                                         confounders_sampling_name)
source.ipw.exact$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.ipw.isw <- SEstimator_wrapper(Estimator="ISW",
                                     target.obj=target.obj,
                                     source.obj=source.obj.ipw,
                                     confounders_sampling_name=
                                       confounders_sampling_name,
                                     method="glm")
source.ipw.isw$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.ipw.subclass <- SEstimator_wrapper(Estimator="Subclass",
                                          target.obj=target.obj,
                                          source.obj=source.obj.ipw,
                                          confounders_sampling_name=
                                            confounders_sampling_name)
source.ipw.subclass$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.dr.exact <- SEstimator_wrapper(Estimator="Exact",
                                      target.obj=target.obj,
                                      source.obj=source.obj.dr,
                                      confounders_sampling_name=
                                        confounders_sampling_name)
source.dr.exact$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.dr.isw <- SEstimator_wrapper(Estimator="ISW",
                                    target.obj=target.obj,
                                    source.obj=source.obj.dr,
                                    confounders_sampling_name=
                                      confounders_sampling_name,
                                    method="glm")
source.dr.isw$EstimateRep(stratification = strata, stratification_joint = TRUE)

source.dr.subclass <- SEstimator_wrapper(Estimator="Subclass",
                                         target.obj=target.obj,
                                         source.obj=source.obj.dr,
                                         confounders_sampling_name=
                                           confounders_sampling_name)
source.dr.subclass$EstimateRep(stratification = strata, stratification_joint = TRUE)

fusion <- Fusion$new(target.obj,
                      source.gc.exact,
                      source.gc.isw,
                      source.gc.subclass,
                      source.ipw.exact,
                      source.ipw.isw,
                      source.ipw.subclass,
                      source.dr.exact,
                      source.dr.isw,
                      source.dr.subclass)

destination <- 'man/figures/plot_all_compares.pdf'
pdf(file=destination, width = 14, height = 7)
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
source.rep.obj <- SEstimator_wrapper(Estimator="Exact",
                                     target.obj=target.obj,
                                     source.obj=source.obj,
                                     confounders_sampling_name=c("x2","x6"))
source.rep.obj$EstimateRep(stratification = strata, stratification_joint = FALSE)
fusion <- Fusion$new(target.obj,
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

vars_rct <- c("x1","x2","x3","x4","x5","x6")
RCT.estimates <- list(ATE_mean = target.obj$estimates$ATE$est,
                      ATE_se = target.obj$estimates$ATE$se,
                      CATE_mean_se = target.obj$get_CATE(vars_rct,FALSE))

emp.p1 <- mean(target.data$x1)
emp.p2 <- mean(target.data$x2)
emp.p3 <- mean(target.data$x3)
emp.p4 <- mean(target.data$x4)
emp.p5 <- mean(target.data$x5)
emp.p6 <- mean(target.data$x6)
t.d <- target.data[,vars_rct]
n <- dim(source.data)[1]
pw.cor <- gdata::upperTriangle(cor(t.d), diag = FALSE, byrow = TRUE)
synthetic.data <- RCTrep::GenerateSyntheticData(
  margin_dis="bernoulli",
  N=n,
  margin=list(emp.p1,emp.p2,emp.p3,emp.p4,emp.p5,emp.p6),
  var_name=vars_rct,
  pw.cor=pw.cor)

apply(synthetic.data, 2, mean)
c(emp.p1,emp.p2,emp.p3,emp.p4,emp.p5,emp.p6)


synthetic.data <- synthetic.data %>%
  arrange(across(vars_rct))

synthetic.data <- semi_join(synthetic.data, source.data, by = vars_rct)
source.data <- semi_join(source.data, synthetic.data, by = vars_rct)
synthetic.data %>% group_by(across(all_of(vars_rct))) %>% summarise(n=n())
source.data %>% group_by(across(all_of(vars_rct))) %>% summarise(n=n())

target.obj <- TEstimator_Synthetic$new(df = synthetic.data,
                                       estimates=RCT.summary,
                                       vars_name = list(confounders_treatment_name=vars_rct),
                                       name = "RCT",
                                       isTrial = TRUE,
                                       data.public = TRUE)

source.obj <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  name = "RWD",
  data.public = TRUE
)

source.rep.obj <- SEstimator_wrapper(Estimator="Exact",
                                     target.obj=target.obj,
                                     source.obj=source.obj,
                                     confounders_sampling_name=c("x2","x6"))
source.rep.obj$EstimateRep(stratification = vars_rct, stratification_joint = FALSE)
fusion <- Fusion$new(target.obj,
                     source.obj,
                     source.rep.obj)

destination <- 'man/figures/plot_exa_3.pdf'
pdf(file=destination, width = 7, height = 4)
fusion$plot()
dev.off()

fusion$evaluate()
