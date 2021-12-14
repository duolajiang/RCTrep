#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggpubr ggtexttable ggarrange
#' @import dplyr
Estimator <- R6::R6Class(
  "Estimator",
  #-------------------------public fields-----------------------------#
  public = list(
    ATE_mean = NA,
    ATE_se = NA,
    ATE_power = NA,
    CATE_mean_se = NA,

    ATE_weighted = NA,
    ATE_se_weighted = NA,
    CATE_weighted = NA,

    confounders_internal_name = NA,
    confounders_external_name = NA,
    treatment_name = NA,
    outcome_name = NA,
    n = NA,

    density_confounders_external = NA,

    data = NA,
    #-------------------------constructor-----------------------------#
    initialize = function(df,vars_name){
      self$data <- df
      self$confounders_internal_name <- vars_name$confounders_internal
      self$confounders_external_name <- vars_name$confounders_external
      self$treatment_name <- vars_name$treatment_name
      self$outcome_name <- vars_name$outcome_name
      self$n <- dim(df)[1]
      self$data$id <- seq(self$n)
    },

    RCTrep = function(target.obj,
                      confounders_external_name,
                      weighting_estimator,weighting_model,
                      stratification, stratification_joint){
      #browser()
      self$confounders_external_name <- confounders_external_name
      private$set_weighted_ATE_SE(target.obj$data, weighting_estimator,weighting_model)
      if(!is.null(stratification)){
        private$set_weighted_CATE(target=target.obj$data,
                                  stratification = stratification,
                                  stratification_joint = stratification_joint,
                                  weighting_method = weighting_estimator,
                                  weighting_model = weighting_model)
      }
    },

    get_CATE = function(stratification,stratification_joint){
      #browser()
      if(stratification_joint){
        CATE_mean_se <- private$set_CATEestimation4JointStratification(stratification)
      } else {
        CATE_mean_se <- private$set_CATEestimation4SeperateStratification(stratification)
      }
      return(CATE_mean_se)
    },

    plot_CATE = function(stratification,stratification_joint=FALSE){
      #browser()
      CATE <- self$get_CATE(stratification,stratification_joint)
      var_names <- colnames(CATE)[!colnames(CATE) %in% c("cate","se","size")]
      subgroup_name_level <- apply(CATE[,!colnames(CATE) %in% c("cate","se","size")], 1,
                                   function(x) paste(var_names,x,sep = "=",collapse = ","))
      subgroup_name_level <- factor(subgroup_name_level,levels=subgroup_name_level,ordered = T)
      CATE.data <- data.frame(group = subgroup_name_level,
                              effect_size = CATE$cate,
                              ci_l = c(CATE$cate - 1.98*CATE$se),
                              ci_u = c(CATE$cate + 1.98*CATE$se),
                              stringsAsFactors = FALSE)

      p_plot <- ggplot(data=CATE.data, aes(x=effect_size, y=group)) +
                       geom_point(position=position_dodge(0.5))+
                       geom_errorbar(aes(xmin=ci_l, xmax=ci_u), width=.1,position=position_dodge(0.5)) +
                       geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5)

      p_table <- ggpubr::ggtexttable(CATE, rows = NULL)

      p <- ggpubr::ggarrange(p_plot,p_table,nrow = 1,ncol = 2)

      return(p)
    }
  ),
  #-------------------------private fields and methods----------------------------#
  private = list(

    set_weighted_ATE_SE = function(target.data, weighting_estimator,weighting_model){
      #browser()
      weight <- setWeight(source=self$data,
                          target=target.data,
                          weighting_method=weighting_estimator,
                          vars_weighting=self$confounders_external_name,
                          weighting_model=weighting_model)
      ATE_se_weighted <- private$est_weighted_ATE_SE(weight)
      self$ATE_weighted <- ATE_se_weighted$est
      self$ATE_se_weighted <- ATE_se_weighted$se
    },

    set_weighted_CATE = function(target.data,stratification,stratification_joint,weighting_method,weighting_model){
      #browser()
      if(stratification_joint){
        #browser()
        self$CATE_weighted <- private$set_WeightedCATEestimation4JointStratification(target.data,
                                                                                     weighting_method,
                                                                                     weighting_model,
                                                                                     stratification)
      } else {
        self$CATE_weighted <- private$set_WeightedCATEestimation4SeperateStratification(target.data,
                                                                                        weighting_method,
                                                                                        weighting_model,
                                                                                        stratification)
      }
    },

    set_CATEestimation4JointStratification = function(stratification){
      #browser()
      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      group_sample_size <- group_size(group_data)
      cate <- se <- size <- NULL
      for (i in seq(n_groups)){
        subgroup.id.in.data <- self$data[group_id==i,'id']
        cate_se <- private$est_CATE_SE(subgroup.id.in.data)
        cate[i] <- cate_se$est; se[i] <- cate_se$se
        size[i] <- group_sample_size[i]
        #print(i)
      }
      CATE_mean_se <- cbind(group_strata,cate,se,size)
      CATE_mean_se <- as.data.frame(CATE_mean_se)
      #browser()
      #colnames(CATE_mean_se) <- c(colnames(patterns),"cate","se")
      return(CATE_mean_se)
    },

    set_CATEestimation4SeperateStratification = function(stratification){
      #browser()
      group_var <- group_level <- cate <- se <- size <- density <- NULL
      i <- 1
      for (var_name in stratification) {
        group_data <- self$data %>% group_by(across(var_name))
        group_strata <- group_data %>% group_keys()
        group_id_4each_obs <- group_data %>% group_indices()
        n_groups <- dim(group_strata)[1]
        group_sample_size <- group_size(group_data)
        for (group_id in seq(n_groups)) {
          subgroup.id.in.data <- self$data[group_id_4each_obs==group_id,'id']
          group_var[i] <- var_name
          group_level[i] <- group_strata[group_id,1]
          cate_se <- private$est_CATE_SE(subgroup.id.in.data)
          cate[i] <- cate_se$est; se[i] <- cate_se$se
          size[i] <- group_sample_size[group_id]
          i <- i+1
        }
      }
      # the output element from group_keys() is not a vector/numeric, hence needs to convert to data.frame reshape(4*1)
      group_level <- t(as.data.frame(group_level))
      CATE_mean_se <- data.frame(name=group_var,
                                 value=group_level,
                                 cate=cate,
                                 se=se,
                                 size=size,
                                 stringsAsFactors = FALSE)
      return(CATE_mean_se)
    },

    set_WeightedCATEestimation4JointStratification = function(target.data,weighting_method,weighting_model,stratification){
      #browser()
      cate <- se <- size <- NULL
      vars_weighting <- self$confounders_external_name
      vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% stratification]

      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      cate <- se <- NULL

      if(class(target.data)=="data.frame"){
        pattern.type <- sapply(target.data[,stratification], class)
      }

      for (i in seq(n_groups)){
        pattern <- group_strata[i,]
        if(class(target.data)=="data.frame"){
          subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern,pattern.type)
          target.subgroup.data <- subset(target.data,eval(parse(text = subgroup_selection_condition)))
        } else {
          target.subgroup.data <- target.data
        }
        source.subgroup.data <- self$data[group_id==i,]
        source.subgroup.id.in.data <- self$data[group_id==i,'id']

        weight <- setWeight(source=source.subgroup.data,
                            target=target.subgroup.data,
                            weighting_method=weighting_method,
                            weighting_model=weighting_model,
                            vars_weighting=vars_weighting_subgroup)

        cate_se <- private$est_weighted_CATE_SE(source.subgroup.id.in.data,weight)
        cate[i] <- cate_se$est
        se[i] <- cate_se$se
        size[i] <- sum(weight)
      }
      CATE_mean_se <- cbind(group_strata,cate,se,size)
      CATE_mean_se <- as.data.frame(CATE_mean_se)
      #colnames(CATE_mean_se) <- c(colnames(patterns),"cate","se")
      return(CATE_mean_se)
    },

    set_WeightedCATEestimation4SeperateStratification = function(target.data,weighting_method,weighting_model,stratification){
      #browser()
      vars_weighting <- self$confounders_external_name
      group_var <- group_level <- cate <- se <- size <- NULL
      i <- 1
      for (var_name in stratification) {
        vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% var_name]
        group_data <- self$data %>% group_by(across(var_name))
        group_strata <- group_data %>% group_keys()
        group_id_4each_obs <- group_data %>% group_indices()
        n_groups <- dim(group_strata)[1]
        if(class(target.data)=="data.frame"){
          pattern.type <- class(target.data[,var_name])
        }

        for (group_id in seq(n_groups)) {
          var_level <- group_strata[group_id,1]
          source.subgroup.id.in.data <- self$data[group_id_4each_obs==group_id,'id']
          source.subgroup.data <- self$data[source.subgroup.id.in.data,]
          if(class(target.data)=="data.frame"){
            subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern=group_strata[group_id,],pattern.type)
            target.subgroup.data <- subset(target.data,eval(parse(text = subgroup_selection_condition)))
          } else {
            target.subgroup.data <- target.data
          }

          weight <- setWeight(source=source.subgroup.data,
                              target=target.subgroup.data,
                              weighting_method=weighting_method,
                              weighting_model=weighting_model,
                              vars_weighting=vars_weighting_subgroup)

          group_var[i] <- var_name
          group_level[i] <- var_level
          cate_se <- private$est_weighted_CATE_SE(source.subgroup.id.in.data,weight)
          cate[i] <- cate_se$est
          se[i] <- cate_se$se
          size[i] <- sum(weight)
          i <- i+1
        }
      }
      # the output element from group_keys() is not a vector/numeric, hence needs to convert to data.frame reshape(4*1)
      group_level <- t(as.data.frame(group_level))
      CATE_mean_se <- data.frame(name=group_var,
                                 value=group_level,
                                 cate=cate,
                                 se=se,
                                 size=size,
                                 stringsAsFactors = FALSE)

      return(CATE_mean_se)
    }
  )
)



















