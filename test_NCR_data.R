library(dplyr)
library(tidyr)
library(snakecase)

# =====================================
# First, select study population.
# =====================================
cohort <- 'select_target_population_incidence_final'

# =====================================
# Build directories
# =====================================
source(glue::glue("../pr-south-korea/scratches/{cohort}.R"))
concept_sets <- readRDS("../pr-south-korea/scratches/concept_sets_2022-07-13.Rds")
data <- readRDS("../pr-south-korea/scratches/dataframe_2022-07-13.Rds")
data.treatment <- readRDS("../pr-south-korea/scratches/dataframe_treatments_2022-07-13.Rds")
data <- data %>% rename_with(snakecase::to_snake_case)
data.treatment <- data.treatment %>% rename_with(snakecase::to_snake_case)
data.non_censored_counters <- Select_Study_Population(data, data.treatment, concept_sets)
data.non_censored <- data.non_censored_counters$data
# ==========================================
# Reprocess the data.
# Impute all missing data,
#   1. replace NA as "unknown" for categorical data,
#   2. and mean of value for continuous data.
# Convert character to factor:
#   Warning: must be implemented after filter
#            because filter row might generate level(s) of a factor with zero count
# ==========================================
source("../pr-south-korea/scratches/preprocess_data.R")
variables = c(
  'cancer_diagnosis_start_date',
  'age_at_diagnosis',
  'gender_concept_name',
  'morphology',
  'topography',
  'calculated_left_or_right_sided',
  'lymphatic_invasion_value_as_concept_name',
  'lyond_value_as_number',
  'lypos_value_as_number',
  'braf_value_as_concept_name',
  'ras_value_as_concept_name',
  'msi_value_as_concept_name',
  'p_t_concept_name',
  'p_n_concept_name',
  'p_stage_concept_name',
  'grade_concept_name',
  'angio_invasion_extramural_value_as_concept_name',
  'angio_invasion_intramural_value_as_concept_name',
  'colon_perforation_concept_name',
  'asa_concept_name',
  'bmi',
  'treatment',
  'chemo_start_date',
  'chemo_end_date',
  'surgery_start_date',
  'surgery_end_date',
  'death_date',
  'number_of_other_tumor'
)
data.non_censored <- data.non_censored[,variables]
data.non_censored <- preprocess_data(data.non_censored)

# =====================================
# Third, generate TEstimator objects
# TEstimator contains:
# 1. models for outcome/propensity score/both,
#    depends on class of TEstimator
# 2. estimates: potential outcomes and conditional average
#    treatment effect of target population and subgroups
#    stratified by confounders_treatment_name(default)
# 3. public function to diagnosis model assumptions (depends on class of TEstimator),
#    treatment count/proportion in subgroups;
#    outcome count in treatment/control groups in subgroups;
# We use three classes of methods and two classes of modeling approaches;
# in total, leads to 3(G_com) + 2(IPW) + 2X2(DR) => 9 objects of TEstimator
# =====================================
source("../pr-south-korea/scratches/estimate_CATE.R")
source("../pr-south-korea/scratches/censor_data.R")
set.seed(123)

confounders_treatment_name <- c(
  'cancer_diagnosis_start_year',
  'age_at_diagnosis',
  'gender',
  'calculated_left_or_right_sided',
  'lymphatic_invasion',
  'lyond',
  'braf',
  'ras',
  'msi',
  'p_t',
  'p_n',
  'p_stage',
  'grade',
  'angio_invasion_extramural',
  'angio_invasion_intramural',
  'colon_perforation',
  'asa',
  'bmi',
  'number_of_other_tumor'
)

vars_name <- list(confounders_treatment_name = confounders_treatment_name,
                  treatment_name = 'treatment_combined',
                  outcome_name = 'survival')
formula_main <- paste(vars_name$confounders_treatment_name, collapse = "+")
formula_interaction <- paste(vars_name$confounders_treatment_name,
                             rep(vars_name$treatment_name,length(vars_name$confounders_treatment_name)),
                             sep = ":", collapse = "+")
outcome_formula <- as.formula(paste(vars_name$outcome_name, "~",
                                    vars_name$treatment_name, "+",
                                    formula_main, "+",
                                    formula_interaction, sep = ""))
treatment_formula <- paste(vars_name$confounders_treatment_name, collapse = "+")
treatment_formula <- as.formula(paste(vars_name$treatment_name,"~",treatment_formula))

# You might have message: Error in predict.pbart(self$model, newdata = data1)
# The number of columns in newdata must be equal to 74
# because the whole data's cancer_diagnosis_start_year has more levels than
data <- censor_data(data=data.non_censored, cutoff_year=5, keep=TRUE)
data <- data %>% filter(!p_t %in% "T0") %>% filter(!p_n %in% "unknown") %>% droplevels()
data <- as.data.frame(data)

library(RCTrep)
obj.g_com.psBART.impute <- RCTrep::TEstimator_wrapper(
  Estimator = "G_computation",
  data = data,
  name = "NKR",
  vars_name = vars_name,
  outcome_method = "psBART_impute",
  data.public = TRUE
)
obj.g_com.psBART.impute$plot_CATE(c("p_stage","p_t","msi","braf"),TRUE)



