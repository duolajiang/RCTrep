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

objs <- list(target.obj=target.obj,
             source.obj=source.obj,
             source.obj.rep=source.obj.rep)

saveRDS(objs,
        file = "objs.rds")

