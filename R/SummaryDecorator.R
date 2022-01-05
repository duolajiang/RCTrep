SummaryDecorater <- R6::R6Class(
  "SummaryDecorator",
  inherit = Estimator,
  public = list(
    initialize = function(obj){
      #browser()
      self$name = obj$name
      self$statistics = obj$statistics
      self$estimates = obj$estimates
      self$model = obj$model
      self$data = self$estimates$CATE
      self$data$id = seq(dim(self$data)[1])
      private$confounders_name = colnames(self$data)[!colnames(self$data) %in% c("y1.hat","y0.hat","cate","se","size","name","value","id")]
    }
  ),

  private = list(
    confounders_name = NA,

    get_weight = function(source, target){
      #browser()
      source <- source %>% summarise(prop=size/sum(size))
      target <- target %>% summarise(prop=size/sum(size))
      weight <- target$prop/source$prop
      return(weight)
    },

    set_weighted_ATE_SE = function(target.data) {
      #browser()
      weight <- private$get_weight(
        source = self$data,
        target = target.data
      )

      ATE_se_weighted <- private$est_weighted_ATE_SE(self$data$id,weight)
      self$estimates$ATE_weighted$y1.hat <- ATE_se_weighted$y1.hat
      self$estimates$ATE_weighted$y0.hat <- ATE_se_weighted$y0.hat
      self$estimates$ATE_weighted$est <- ATE_se_weighted$est
      self$estimates$ATE_weighted$se <- ATE_se_weighted$se
    },

    est_WeightedCATEestimation4JointStratification = function(target.data,stratification) {
      # browser()
      cate <- se <- size <- y1.hat <- y0.hat <- NULL
      vars_weighting <- private$confounders_name
      vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% stratification]

      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      cate <- se <- NULL
      pattern.type <- sapply(target.data[, stratification], class)

      for (i in seq(n_groups)) {
        pattern <- group_strata[i, ]
        subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern, pattern.type)
        target.subgroup.data <- subset(target.data, eval(parse(text = subgroup_selection_condition)))
        source.subgroup.data <- self$data[group_id == i, ]
        source.subgroup.id.in.data <- self$data[group_id == i, "id"]

        weight <- private$get_weight(
          source = source.subgroup.data,
          target = target.subgroup.data
        )

        cate_y1_y0_se <- private$est_weighted_ATE_SE(source.subgroup.id.in.data, weight)
        y1.hat[i] <- cate_y1_y0_se$y1.hat
        y0.hat[i] <- cate_y1_y0_se$y0.hat
        cate[i] <- cate_y1_y0_se$est
        se[i] <- cate_y1_y0_se$se
        size[i] <- sum(weight)
      }

      CATE_mean_se <- cbind(group_strata, y1.hat, y0.hat, cate, se, size)
      CATE_mean_se <- as.data.frame(CATE_mean_se)
      return(CATE_mean_se)
    },

    est_WeightedCATEestimation4SeperateStratification = function(target.data, stratification) {
      #browser()
      vars_weighting <- private$confounders_name
      group_var <- group_level <- cate <- se <- size <- y1.hat <- y0.hat <- NULL
      i <- 1
      for (var_name in stratification) {
        vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% var_name]
        group_data <- self$data %>% group_by(across(var_name))
        group_strata <- group_data %>% group_keys()
        group_id_4each_obs <- group_data %>% group_indices()
        n_groups <- dim(group_strata)[1]
        pattern.type <- class(target.data[, var_name])

        for (group_id in seq(n_groups)) {
          var_level <- group_strata[group_id, 1]
          source.subgroup.id.in.data <- self$data[group_id_4each_obs == group_id, "id"]
          source.subgroup.data <- self$data[source.subgroup.id.in.data, ]
          subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern = group_strata[group_id, ], pattern.type)
          target.subgroup.data <- subset(target.data, eval(parse(text = subgroup_selection_condition)))

          weight <- private$get_weight(
            source = source.subgroup.data,
            target = target.subgroup.data
          )

          group_var[i] <- var_name
          group_level[i] <- var_level
          cate_y1_y0_se <- private$est_weighted_ATE_SE(source.subgroup.id.in.data, weight)
          y1.hat[i] <- cate_y1_y0_se$y1.hat
          y0.hat[i] <- cate_y1_y0_se$y0.hat
          cate[i] <- cate_y1_y0_se$est
          se[i] <- cate_y1_y0_se$se
          size[i] <- sum(weight)
          i <- i + 1
        }
      }
      # the output element from group_keys() is not a vector/numeric, hence needs to convert to data.frame reshape(4*1)
      group_level <- t(as.data.frame(group_level))
      CATE_mean_se <- data.frame(
        name = group_var,
        value = group_level,
        y1.hat = y1.hat,
        y0.hat = y0.hat,
        cate = cate,
        se = se,
        size = size,
        stringsAsFactors = FALSE
      )

      return(CATE_mean_se)
    },

    est_ATE_SE = function(index) {
      weight.norm <- self$data[index,"size"]/sum(self$data[index,"size"])
      y1.hat <- sum(self$data$y1.hat[index]*weight.norm)
      y0.hat <- sum(self$data$y0.hat[index]*weight.norm)
      est <- y1.hat - y0.hat
      var.ate <- sum(self$data$se[index] * (weight.norm)^2)/sum(weight.norm^2)
      se <- sqrt(var.ate)
      return(list(y1.hat = y1.hat, y0.hat = y0.hat, est = est, se = se))
    },

    est_weighted_ATE_SE = function(index, weight) {
      weight.norm <- weight / sum(weight)
      y1.hat <- sum(self$data$y1.hat[index]*weight.norm)
      y0.hat <- sum(self$data$y0.hat[index]*weight.norm)
      est <- y1.hat - y0.hat
      var.ate <- sum(self$data$se[index] * (weight)^2)/sum(weight^2)
      se <- sqrt(var.ate)
      return(list(y1.hat = y1.hat, y0.hat = y0.hat, est = est, se = se))
    }
  )
)
