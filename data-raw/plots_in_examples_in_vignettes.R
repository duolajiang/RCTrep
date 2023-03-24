##########################################################
## Example in section 1.3. Demonstration of usage
##########################################################
library(RCTrep)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data
output <- RCTREP(TEstimator = "G_computation", SEstimator = "Exact",
                 outcome_method = "BART",
                 source.data = RCTrep::source.data,
                 target.data = RCTrep::target.data,
                 vars_name = list(confounders_treatment_name =
                                    c("x1","x2","x3","x4","x5","x6"),
                                  treatment_name = c('z'),
                                  outcome_name = c('y')),
                 confounders_sampling_name = c("x2","x6"),
                 stratification = c("x1","x3","x4","x5"),
                 stratification_joint = TRUE)

fusion <- Fusion$new(output$target.obj,
                     output$source.obj,
                     output$source.rep.obj)

fusion$plot()

##########################################################
## Example in Section 5. Basic usage
##########################################################
library(RCTrep)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

vars_name <- list(confounders_treatment_name =
                    c("x1","x2","x3","x4","x5","x6"),
                  treatment_name = c('z'),
                  outcome_name = c('y')
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

source.obj.rep <- SEstimator_wrapper(Estimator = "Exact",
                                     target.obj = target.obj,
                                     source.obj = source.obj,
                                     confounders_sampling_name = c("x2","x6"))
source.obj.rep$EstimateRep(stratification = c("x1","x3","x4","x5"))

destination <- '../man/figures/plot_wexa_obj_source_model_summary.pdf'
pdf(file = destination, width = 18, height = 5)
source.obj$diagnosis_t_ignorability()
dev.off()

destination <- '../man/figures/plot_wexa_obj_source_t_overlap.pdf'
pdf(file = destination, width = 7, height = 4)
source.obj$diagnosis_t_overlap()
dev.off()

destination <- '../man/figures/plot_wexa_obj_target_t_overlap.pdf'
pdf(file = destination, width = 7, height = 4)
target.obj$diagnosis_t_overlap()
dev.off()

destination <- '../man/figures/plot_wexa_obj_sourcerep_s_overlap.pdf'
pdf(file = destination, width = 7, height = 4)
source.obj.rep$diagnosis_s_overlap()
dev.off()

destination <- '../man/figures/plot_wexa_obj_sourcerep_s_ignorability.pdf'
pdf(file=destination, width = 7, height = 4)
source.obj.rep$diagnosis_s_ignorability()
dev.off()

fusion <- Fusion$new(target.obj,
                     source.obj,
                     source.obj.rep)
destination <- '../man/figures/plot_wexa_compare.pdf'
pdf(file = destination, width = 7, height = 4)
fusion$plot()
dev.off()

fusion$evaluate()


call_dashboard(source.obj = source.obj,
               target.obj = target.obj,
               source.obj.rep = source.obj.rep)

# ==============================================================
# ==============================================================
# Example in section 6.1. Example 1: Validation at scale.
# ==============================================================
library(RCTrep)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

vars_name <- list(confounders_treatment_name = c("x1","x2","x3","x4","x5","x6"),
                  treatment_name = c('z'),
                  outcome_name = c('y')
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

source.gc.exact <- SEstimator_wrapper(Estimator = "Exact",
                                      target.obj = target.obj,
                                      source.obj = source.obj.gc,
                                      confounders_sampling_name =
                                        confounders_sampling_name)
source.gc.exact$EstimateRep(stratification = strata,
                            stratification_joint = TRUE)

source.gc.isw <- SEstimator_wrapper(Estimator = "ISW",
                                    target.obj = target.obj,
                                    source.obj = source.obj.gc,
                                    confounders_sampling_name =
                                      confounders_sampling_name,
                                    method = "glm")
source.gc.isw$EstimateRep(stratification = strata,
                          stratification_joint = TRUE)

source.gc.subclass <- SEstimator_wrapper(Estimator = "Subclass",
                                         target.obj = target.obj,
                                         source.obj = source.obj.gc,
                                         confounders_sampling_name =
                                           confounders_sampling_name)
source.gc.subclass$EstimateRep(stratification = strata,
                               stratification_joint = TRUE)

source.ipw.exact <- SEstimator_wrapper(Estimator = "Exact",
                                       target.obj = target.obj,
                                       source.obj = source.obj.ipw,
                                       confounders_sampling_name =
                                         confounders_sampling_name)
source.ipw.exact$EstimateRep(stratification = strata,
                             stratification_joint = TRUE)

source.ipw.isw <- SEstimator_wrapper(Estimator = "ISW",
                                     target.obj = target.obj,
                                     source.obj = source.obj.ipw,
                                     confounders_sampling_name =
                                       confounders_sampling_name,
                                     method = "glm")
source.ipw.isw$EstimateRep(stratification = strata,
                           stratification_joint = TRUE)

source.ipw.subclass <- SEstimator_wrapper(Estimator = "Subclass",
                                          target.obj = target.obj,
                                          source.obj = source.obj.ipw,
                                          confounders_sampling_name =
                                            confounders_sampling_name)
source.ipw.subclass$EstimateRep(stratification = strata,
                                stratification_joint = TRUE)

source.dr.exact <- SEstimator_wrapper(Estimator = "Exact",
                                      target.obj = target.obj,
                                      source.obj = source.obj.dr,
                                      confounders_sampling_name =
                                        confounders_sampling_name)
source.dr.exact$EstimateRep(stratification = strata,
                            stratification_joint = TRUE)

source.dr.isw <- SEstimator_wrapper(Estimator = "ISW",
                                    target.obj = target.obj,
                                    source.obj = source.obj.dr,
                                    confounders_sampling_name =
                                      confounders_sampling_name,
                                    method = "glm")
source.dr.isw$EstimateRep(stratification = strata,
                          stratification_joint = TRUE)

source.dr.subclass <- SEstimator_wrapper(Estimator = "Subclass",
                                         target.obj = target.obj,
                                         source.obj = source.obj.dr,
                                         confounders_sampling_name =
                                           confounders_sampling_name)
source.dr.subclass$EstimateRep(stratification = strata,
                               stratification_joint = TRUE)

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

destination <- '../man/figures/plot_all_compares.pdf'
pdf(file = destination, width = 14, height = 7)
fusion$plot()
dev.off()

fusion$print()
fusion$evaluate()

##########################################################################################
# Example in Section 6.2. Example 2: Validation using aggregated data
##########################################################################################
library(geex)
library(caret)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

# Identification
vars_name <- list(confounders_treatment_name = c("x1","x2","x3","x4","x5","x6"),
                  treatment_name = c('z'),
                  outcome_name = c('y')
)
confounders_sampling_name <- c("x2","x6")

# Estimate conditional average treatment effect
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

head(source.obj$data)

# Estimate the weighted conditional average treatment effect of source.obj
strata <- c("x1","x4")
source.rep.obj <- SEstimator_wrapper(Estimator = "Exact",
                                     target.obj = target.obj,
                                     source.obj = source.obj,
                                     confounders_sampling_name =
                                       confounders_sampling_name)
source.rep.obj$EstimateRep(stratification = strata, stratification_joint = TRUE)

# Validate
fusion <- Fusion$new(target.obj,
                     source.obj,
                     source.rep.obj)

destination <- '../man/figures/plot_exa_2.pdf'
pdf(file = destination, width = 7, height = 4)
fusion$plot()
dev.off()

fusion$print()
fusion$evaluate()

##########################################################
# Example in section 6.3. Example 3: Validation using synthetic RCT data
##########################################################
library(dplyr)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

# Identification
vars_name <- list(confounders_treatment_name = c("x1","x2","x3","x4","x5","x6"),
                  treatment_name = c('z'),
                  outcome_name = c('y')
)

# Generate target.obj using full dataset
target.obj <- TEstimator_wrapper(
  Estimator = "Crude",
  data = target.data,
  vars_name = vars_name,
  name = "RCT",
  data.public = FALSE,
  isTrial = TRUE
)

# Get unbiased estimates of conditional average treatment effect
vars_rct <- c("x1","x2","x3","x4","x5","x6")
RCT.estimates <- list(ATE_mean = target.obj$estimates$ATE$est,
                      ATE_se = target.obj$estimates$ATE$se,
                      CATE_mean_se = target.obj$get_CATE(vars_rct,FALSE))

# Simulate synthetic RCT data given marginal distributions
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
  N = n,
  margin = list(emp.p1, emp.p2, emp.p3, emp.p4, emp.p5, emp.p6),
  var_name = vars_rct,
  pw.cor = pw.cor)

# apply(synthetic.data, 2, mean)
# c(emp.p1,emp.p2,emp.p3,emp.p4,emp.p5,emp.p6)

head(synthetic.data)

synthetic.data <- synthetic.data %>%
  arrange(across(vars_rct))

# Initiate target.obj using synthetic data and unbiased estimates
synthetic.data <- semi_join(synthetic.data, source.data, by = vars_rct)
target.obj <- TEstimator_Synthetic$new(data = synthetic.data,
                                       estimates=RCT.estimates,
                                       vars_name = vars_name,
                                       name = "RCT",
                                       isTrial = TRUE,
                                       data.public = TRUE)

# Estimate conditional average treatment effect
source.data <- semi_join(source.data, synthetic.data, by = vars_rct)
source.obj <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  name = "RWD",
  data.public = TRUE
)

# Estimate weighted conditional average treatment effect
source.rep.obj <- SEstimator_wrapper(Estimator="Exact",
                                     target.obj=target.obj,
                                     source.obj=source.obj,
                                     confounders_sampling_name=c("x2","x6"))
source.rep.obj$EstimateRep(stratification = vars_rct,
                           stratification_joint = FALSE)

# Combine objects and validate estimates
fusion <- Fusion$new(target.obj,
                     source.obj,
                     source.rep.obj)

destination <- '../man/figures/plot_exa_3.pdf'
pdf(file=destination, width = 7, height = 4)
fusion$plot()
dev.off()

fusion$evaluate()
