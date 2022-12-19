library(dplyr)
source.data <- RCTrep::source.binary.data
target.data <- RCTrep::target.binary.data

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
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

# step 3 (for binary outcomes): check overlap of probability of outcome given confounders_treatment and treatment
# few overlap might lead to loss of precision of logisitc regression
source.data %>% group_by(across(all_of(c(vars_name$confounders_treatment,vars_name$treatment_name)))) %>%
  summarise(ybar=mean(y=="1"), obs=n())
target.data %>% group_by(across(all_of(c(vars_name$confounders_treatment,vars_name$treatment_name)))) %>%
  summarise(ybar=mean(y=="1"), obs=n())

source.data <- as.data.frame(source.data)
target.data <- as.data.frame(target.data)
#source.data$y <- as.factor(source.data$y)
#source.data$z <- as.factor(source.data$z)

library(RCTrep)
obj.g_com.LReg <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  name = "NKR",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  data.public = TRUE
)

obj.g_com.LReg$plot_CATE(stratification = c("x1","x2"), stratification_joint = TRUE)
obj.g_com.LReg$plot_y1_y0(stratification = c("x1","x2"), stratification_joint = TRUE, seperate = FALSE)
obj.g_com.LReg$diagnosis_t_overlap(stratification = c("x1","x2"), stratification_joint = TRUE)
obj.g_com.LReg$diagnosis_y_overlap(stratification = c("x1","x2"), stratification_joint = TRUE)

# ==================================================
# ==================================================
source.data[1:100,'y'] <- NA
library(RCTrep)
obj.g_com.psBART <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  name = "NKR",
  vars_name = vars_name,
  outcome_method = "psBART_impute",
  data.public = TRUE
)
obj.g_com.psBART$plot_CATE(c("x1",'x2'),TRUE)
obj.g_com.psBART$diagnosis_y_overlap(c('x1','x2'),TRUE)
obj.g_com.psBART$diagnosis_t_ignorability(c('x1','x3'),TRUE)
