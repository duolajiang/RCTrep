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
ncr<- RCTrep::ncr
korea <- RCTrep::korea
vars_name <- list(confounders_treatment_name=c("Stage2","age","pT"),
                  confounders_sampling_name=c("Stage2","age","pT"),
                  treatment_name=c('combined_chemo'),
                  outcome_name=c('vitstat')
)

data.public <- TRUE
Estimator <- "IPW"
outcome_formula <- vitstat ~ combined_chemo + Stage2 + age + pT  +
  combined_chemo:Stage2 +combined_chemo:age + combined_chemo:pT + combined_chemo:pT:Stage2

treatment_formula <- combined_chemo ~ Stage2 + age + pT


# NOTE!!!
# data must be data.frame, instead of tbl
ncr.obj <- TEstimator_wrapper(
  Estimator = Estimator,
  data = ncr,
  name = "The Netherlands",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = outcome_formula,
  treatment_formula = treatment_formula,
  data.public = data.public
)
#ncr.obj$plot_CATE(stratification = c("Stage2","pT","age"), stratification_joint = TRUE)

korea.obj <- TEstimator_wrapper(
  Estimator = Estimator,
  data = korea,
  name = "SK",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = outcome_formula,
  treatment_formula = treatment_formula,
  data.public = data.public
)


quasar.obj <- RCTrep::quasar.obj

ncr.rep.obj <- SEstimator_wrapper(estimator="Exact_pp",
                                  target.obj=quasar.obj,
                                  source.obj=ncr.obj,
                                  confounders_sampling_name=c("Stage2","pT","age"))

korea.rep.obj <- SEstimator_wrapper(estimator="Exact_pp",
                                    target.obj=quasar.obj,
                                    source.obj=korea.obj,
                                    confounders_sampling_name=c("Stage2","pT","age"))

ncr.rep.obj$EstimateRep(stratification = c("Stage2","pT","age"), stratification_joint = FALSE)
korea.rep.obj$EstimateRep(stratification = c("Stage2","pT","age"), stratification_joint = FALSE)


fusion <- Summary$new(ncr.obj,
                      ncr.rep.obj,
                      korea.obj,
                      korea.rep.obj,
                      quasar.obj)

fusion$print()
fusion$evaluate()
fusion$plot()

destination <- '~lshen/Downloads/real_example.pdf'
pdf(file=destination, width = 7, height = 4)
fusion$plot()
dev.off()


# do not compare to rct, we compare ncr to korea
# regard ncr as source.obj, korea as target.obj
ncr.rep.korea <- SEstimator_wrapper(estimator="Exact",
                                    target.obj=korea.obj,
                                    source.obj=ncr.obj,
                                    confounders_sampling_name=c("Stage2","pT","age"))
ncr.rep.korea$EstimateRep(stratification = c("Stage2","pT"), stratification_joint = FALSE)

destination <- '~lshen/Downloads/real_s.pdf'
pdf(file=destination, width = 7, height = 4)
ncr.rep.korea$diagnosis_s_overlap(stratification = c("Stage2","pT"), stratification_joint = TRUE)
dev.off()

fusion <- Summary$new(ncr.obj,
                      korea.obj,
                      ncr.rep.korea)
destination <- '~lshen/Downloads/real_example.pdf'
pdf(file=destination, width = 7, height = 4)
fusion$plot()
dev.off()

a <- fusion$plot()
b <- ncr.rep.korea$diagnosis_s_overlap()
library(ggpubr)
destination <- '~lshen/Downloads/real_example.pdf'
pdf(file=destination, width = 20, height = 4)
ggarrange(a,b,nrow = 1)
dev.off()

# need to write summary for
source.obj$diagnosis_t_overlap(stratification = c("Stage2","pT","BRAF"), TRUE)
source.obj$diagnosis_y_overlap(stratification = c("Stage2","pT","BRAF"))
source.obj$summary(stratification = c("Stage2","pT","BRAF"))







