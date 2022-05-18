#' @export
SEstimator <- R6::R6Class(
  "SEstimator",
  #-------------------------public fields-----------------------------#
  public = list(
    name = character(),
    id = character(),

    statistics = list(),
    estimates = list(ATE = data.frame(y1.hat=NA,
                                      y0.hat=NA,
                                      est=NA,
                                      se=NA),
                     CATE = data.frame()),
    model = NA,
    confounders_sampling_name = NA,
    weighting_method = character(),

    initialize = function(target.obj, source.obj, weighting_method=NULL,
                          confounders_sampling_name){
      private$target.obj <- target.obj
      private$source.obj <- source.obj
      self$weighting_method <- weighting_method
      self$confounders_sampling_name <- confounders_sampling_name
      private$ispublic <- !c("TEstimator_pp") %in% class(source.obj)
      self$name <- source.obj$name
      self$statistics <- source.obj$statistics
      self$id <- paste(private$source.obj$id,
                       self$weighting_estimator,
                       length(self$confounders_sampling_name),sep = '+')
      private$isTrial <- source.obj$.__enclos_env__$private$isTrial
    },

    EstimateRep = function(stratification=self$confounders_sampling_name, stratification_joint=TRUE) {
      #browser()
      private$set_weighted_ATE_SE()
      private$set_weighted_CATE_SE(stratification = stratification,
                                   stratification_joint = stratification_joint)
    },

    diagnosis_s_overlap = function(stratification=NULL, stratification_joint=TRUE){
      #browser()
      if(missing(stratification)){
        vars_name <- self$confounders_sampling_name
      } else{
        vars_name <- stratification
      }
      if(stratification_joint){
        source.data <- private$source.obj$data %>%
          select(vars_name) %>%
          mutate(study=private$source.obj$name)
        target.data <- private$target.obj$data %>%
          select(vars_name) %>%
          mutate(study=private$target.obj$name)

        data <- rbind(source.data, target.data) %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ",")))

        #data <- bind_rows(source.data, target.data) %>%
        #  mutate(group_name = apply(.[,vars_name], 1, function(x)
        #    paste(vars_name,x,sep = "=",collapse = ",")))

        p.prop <- ggplot(data = data, aes(x=group_name, fill=study)) +
          geom_bar(position = "fill") +
          ylab("proportion") +
          coord_flip() +
          theme(legend.position="none")

        p.count <- ggplot(data = data, aes(x=group_name, fill=study)) +
          geom_bar(stat = "count") +
          coord_flip() +
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.y=element_blank())
      } else{
        stop("stratification_joint must be TRUE for now, the function has not been completed yet.")
      }

      tgrob <- ggpubr::text_grob(c("Sampling overlap within subpopulations"))
      ggpubr::ggarrange(tgrob, NULL, p.prop, p.count, nrow=2, ncol=2, heights = c(1,5))

      # out <- list(aggregate.stats = data,
      #             plot.aggregate = p)
      #
      # out
    }

  ),

  private = list(
    source.obj = NA,
    target.obj = NA,
    ispublic = NA,
    isTrial = NA,

    get_weight = function(){},

    set_weighted_ATE_SE = function() {
      #browser()
      weight <- private$get_weight(
        source = private$source.obj$data,
        target = private$target.obj$data,
        vars_weighting = self$confounders_sampling_name
      )

      ATE_se_weighted <- private$source.obj$.__enclos_env__$private$est_weighted_ATE_SE(private$source.obj$data$id,weight)
      self$estimates$ATE$y1.hat <- ATE_se_weighted$y1.hat
      self$estimates$ATE$y0.hat <- ATE_se_weighted$y0.hat
      self$estimates$ATE$est <- ATE_se_weighted$est
      self$estimates$ATE$se <- ATE_se_weighted$se
    },

    set_weighted_CATE_SE = function(stratification, stratification_joint) {
      if (!is.null(stratification)) {
        if(isTRUE(all.equal(stratification,self$confounders_sampling_name))&stratification_joint){
          self$estimates$CATE <- private$source.obj$get_CATE(stratification = stratification,
                                                             stratification_joint = stratification_joint)
        } else {
          if (stratification_joint) {
            self$estimates$CATE <- private$est_WeightedCATEestimation4JointStratification(stratification)
          } else {
            self$estimates$CATE <- private$est_WeightedCATEestimation4SeperateStratification(stratification)
          }
        }
      }
    },

    est_WeightedCATEestimation4JointStratification = function(stratification) {
      #browser()
      cate <- se <- size <- y1.hat <- y0.hat <- NULL
      vars_weighting <- self$confounders_sampling_name
      vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% stratification]

      group_data <- private$source.obj$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      cate <- se <- y1.hat <- y0.hat <- NULL
      pattern.type <- sapply(private$target.obj$data[, stratification], class)

      for (i in seq(n_groups)) {
        pattern <- group_strata[i, ]
        subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern, pattern.type)
        target.subgroup.data <- subset(private$target.obj$data, eval(parse(text = subgroup_selection_condition)))
        source.subgroup.data <- private$source.obj$data[group_id == i, ]
        source.subgroup.id.in.data <- private$source.obj$data[group_id == i, "id"]

        weight <- private$get_weight(
          source = source.subgroup.data,
          target = target.subgroup.data,
          vars_weighting = vars_weighting_subgroup
        )

        cate_y1_y0_se <- private$source.obj$.__enclos_env__$private$est_weighted_ATE_SE(source.subgroup.id.in.data, weight)
        y1.hat[i] <- cate_y1_y0_se$y1.hat
        y0.hat[i] <- cate_y1_y0_se$y0.hat
        cate[i] <- cate_y1_y0_se$est
        se[i] <- cate_y1_y0_se$se
        size[i] <- ifelse(private$ispublic, dim(source.subgroup.data)[1],
                          sum(source.subgroup.data$size))
        #size[i] <- ifelse(private$ispublic, ((sum(weight))^{2}) / (sum(weight^{2})), sum(weight))
      }
      CATE_mean_se <- cbind(group_strata, y1.hat, y0.hat, cate, se, size)
      CATE_mean_se <- as.data.frame(CATE_mean_se)
      return(CATE_mean_se)
    },

    est_WeightedCATEestimation4SeperateStratification = function(stratification) {
      #browser()
      vars_weighting <- self$confounders_sampling_name
      group_var <- group_level <- cate <- se <- size <- y1.hat <- y0.hat <- NULL
      i <- 1
      for (var_name in stratification) {
        vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% var_name]
        group_data <- private$source.obj$data %>% group_by(across(var_name))
        group_strata <- group_data %>% group_keys()
        group_id_4each_obs <- group_data %>% group_indices()
        n_groups <- dim(group_strata)[1]
        #if(!("Synthetic_TEstimator" %in% class(private$target.obj))){
        pattern.type <- class(private$target.obj$data[, var_name])
        #}

        for (group_id in seq(n_groups)) {
          var_level <- group_strata[group_id, 1]
          source.subgroup.id.in.data <- private$source.obj$data[group_id_4each_obs == group_id, "id"]
          source.subgroup.data <- private$source.obj$data[source.subgroup.id.in.data, ]
          #if(!("Synthetic_TEstimator" %in% class(private$target.obj))){
          subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern = group_strata[group_id, ], pattern.type)
          target.subgroup.data <- subset(private$target.obj$data, eval(parse(text = subgroup_selection_condition)))
          #} else{
          #  target.subgroup.data <- private$target.obj$data[(private$target.obj$data$name==var_name) & (private$target.obj$data$value==as.numeric(var_level)),]
          #}


          weight <- private$get_weight(
            source = source.subgroup.data,
            target = target.subgroup.data,
            vars_weighting = vars_weighting_subgroup
          )

          group_var[i] <- var_name
          group_level[i] <- var_level
          cate_y1_y0_se <- private$source.obj$.__enclos_env__$private$est_weighted_ATE_SE(source.subgroup.id.in.data, weight)
          y1.hat[i] <- cate_y1_y0_se$y1.hat
          y0.hat[i] <- cate_y1_y0_se$y0.hat
          cate[i] <- cate_y1_y0_se$est
          se[i] <- cate_y1_y0_se$se
          size[i] <- ifelse(private$ispublic, dim(source.subgroup.data)[1],
                            sum(source.subgroup.data$size))
          #size[i] <- ifelse(private$ispublic, ((sum(weight))^{2}) / (sum(weight^{2})), sum(weight))
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

    est_statistics = function(){

    }
  )
)
