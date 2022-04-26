# ===================================
# ===================================
# ===================================
# ===================================
library(data.table)
library(RCTrep)
library(dplyr)
source('../../potential_outcomes_subsample/function.R')
source("../../policy_evaluation/main_function.R")
source('../../function/policy_evaluation_function.R')

set.seed(123)
data <- DataImportGeneral(year=2)
data <- data[!((data$cM==1)|(data$cM=='1A')|(data$cM=='1B')),]
data <- data[(data$Stage==2)|(data$Stage==3),]
data <- data[(data$age_at_diagnosis<87)&(data$age_at_diagnosis>22),]
data <- data[,c("BRAF","RAS","Stage2","age_at_diagnosis","combined_chemo","vitstat","pT","lymph_assessed","male")]
data <- data %>% rename(age=age_at_diagnosis)
data$age <- cut(data$age,breaks = c(min(data$age),50,60,70,max(data$age)),labels=c(1,2,3,4),
                include.lowest = TRUE,
                ordered_result = TRUE)

#data$combined_chemo <- factor(data$combined_chemo)
#data$vitstat <- factor(data$vitstat)
data$weight <- ifelse(data$Stage2==1,1,3)
ncr <- data[sample(1:dim(data)[1],6000),]
korea <- data[sample(1:dim(data)[1],6000,prob = data$weight),]

vars_name <- list(confounders_treatment_name=c("Stage2","age","pT"),
                  confounders_sampling_name=c("Stage2","age","pT"),
                  treatment_name=c('combined_chemo'),
                  outcome_name=c('vitstat')
)

ncr <- ncr %>%
  select(vars_name$confounders_treatment_name,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment_name))

korea <- korea %>%
  select(vars_name$confounders_treatment_name,
         vars_name$treatment_name,
         vars_name$outcome_name) %>%
  arrange(across(vars_name$confounders_treatment_name))

ncr <- ncr %>% group_by(across(all_of(vars_name$confounders_treatment_name))) %>%
  mutate(pt=sum(eval(parse(text=vars_name$treatment_name)))/n()) %>%
  filter((pt!=1)&(pt!=0)) %>%
  ungroup() %>%
  select(-pt)

korea <- korea %>% group_by(across(all_of(vars_name$confounders_treatment_name))) %>%
  mutate(pt=sum(eval(parse(text=vars_name$treatment_name)))/n()) %>%
  filter((pt!=1)&(pt!=0)) %>%
  ungroup() %>%
  select(-pt)

ncr <- semi_join(ncr, korea, by = vars_name$confounders_treatment_name)
korea <- semi_join(korea, ncr, by = vars_name$confounders_treatment_name)

ncr %>% group_by(across(all_of(vars_name$confounders_sampling_name))) %>% summarise(n=n())
korea %>% group_by(across(all_of(vars_name$confounders_sampling_name))) %>% summarise(n=n())
ncr <- as.data.frame(ncr)
korea <- as.data.frame(korea)
usethis::use_data(ncr,korea, quasar.agg,overwrite = TRUE)


# usethis::use_data(quasar.agg, overwrite = TRUE)
# levels of categorical variable should be numeric.
# name and value must be findable in source.
# default: number of strata is smaller than confounders_external.
# confounders_external should no samller than #univariate_p
# levels of categorical variable should be numeric.
library(RCTrep)
library(dplyr)
## data preparation
ncr <- RCTrep::ncr
korea <- RCTrep::korea
vars_name <- list(confounders_treatment_name=c("Stage2","age","pT"),
                  confounders_sampling_name=c("Stage2","age","pT"),
                  treatment_name=c('combined_chemo'),
                  outcome_name=c('vitstat')
)

data.public <- FALSE
Estimator <- "G_computation"
outcome_formula <- vitstat ~ combined_chemo + Stage2 + age + pT  +
  combined_chemo:Stage2 +combined_chemo:age + combined_chemo:pT + combined_chemo:pT:Stage2

treatment_formula <- combined_chemo ~ Stage2 + age + pT


# NOTE!!!
# data MUST be data.frame, NOT tbl
obj.ncr <- TEstimator_wrapper(
  Estimator = Estimator,
  data = ncr,
  name = "The Netherlands",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = outcome_formula,
  treatment_formula = treatment_formula,
  data.public = data.public
)
#obj.ncr$plot_CATE(stratification = c("Stage2","pT","age"), stratification_joint = TRUE)

obj.korea <- TEstimator_wrapper(
  Estimator = Estimator,
  data = korea,
  name = "SK",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = outcome_formula,
  treatment_formula = treatment_formula,
  data.public = data.public
)

## quasar.obj is generated in generate_synthetic_quasar_data.R
obj.quasar <- RCTrep::quasar.obj

# confounders_sampling_name should be variables in both ncr, korea, and quasar
obj.ncr2quasar <- SEstimator_wrapper(estimator="Exact_pp",
                                  target.obj=obj.quasar,
                                  source.obj=obj.ncr,
                                  confounders_sampling_name=c("Stage2","age"))

obj.korea2quasar <- SEstimator_wrapper(estimator="Exact_pp",
                                    target.obj=obj.quasar,
                                    source.obj=obj.korea,
                                    confounders_sampling_name=c("Stage2","age"))

obj.ncr2quasar$EstimateRep(stratification = c("Stage2","age"), stratification_joint = FALSE)
obj.korea2quasar$EstimateRep(stratification = c("Stage2","age"), stratification_joint = FALSE)


fusion <- Summary$new(obj.ncr,
                      obj.ncr2quasar,
                      obj.korea,
                      obj.korea2quasar,
                      obj.quasar)

fusion$print()
fusion$evaluate()
fusion$plot()


# compare ncr with korea
obj.ncr2korea <- SEstimator_wrapper(estimator="Exact_pp",
                                    target.obj=obj.korea,
                                    source.obj=obj.ncr,
                                    confounders_sampling_name=c("Stage2","pT"))
obj.ncr2korea$EstimateRep(stratification = c("Stage2","pT"), stratification_joint = TRUE)
fusion <- Summary$new(obj.ncr,
                      obj.korea,
                      obj.ncr2korea)
fusion$plot()

obj.ncr2korea$diagnosis_s_overlap(stratification = c("Stage2","pT"), stratification_joint = TRUE)
obj.ncr$diagnosis_t_overlap(stratification = c("Stage2","pT"), TRUE)
obj.ncr$diagnosis_y_overlap(stratification = c("Stage2","pT"), TRUE)
obj.ncr$summary()
obj.korea$diagnosis_t_overlap(stratification = c("Stage2","pT"), TRUE)
obj.korea$diagnosis_y_overlap(stratification = c("Stage2","pT"), TRUE)
obj.korea$summary()




