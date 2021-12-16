#' @title R6 class: Estimator base class
#' @description A base R6 class for estimator of average treatment effect that implements the common methods, such as \code{\link{RCTrep}}, \code{\link{get_CATE}}, \code{\link{plot_CATE}}, inheritted by \code{\link{G_computation_base}}, \code{\link{IPW_base}}, and \code{\link{DR_base}} class.
#'
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
    #' @description Create a new \code{Estimator} object
    #' @param df A data frame containing variables in \code{vars_name}
    #' @param vars_name vars_name A list containing four vectors \code{confounders_internal}, \code{confounders_external}, \code{treatment_name}, and \code{outcome_name}. \code{confounders_internal} is a character vector containing the adjustment variables, which, along with \code{Estimator} and the corresponding \code{outcome_method} or \code{treatment_method} to correct for confounding; \code{confounders_external} is a character vector containing variables for weighting as to generalize estimates from \code{source.data} to \code{target.data}; \code{outcome_name} is a character vector of length one containing the variable name of outcome; \code{treatment_name} is a character vector of length one containing the variable name of treatment.
    initialize = function(df,vars_name){
      self$data <- df
      self$confounders_internal_name <- vars_name$confounders_internal
      self$confounders_external_name <- vars_name$confounders_external
      self$treatment_name <- vars_name$treatment_name
      self$outcome_name <- vars_name$outcome_name
      self$n <- dim(df)[1]
      self$data$id <- seq(self$n)
    },

    #' @description Replicating the average treatment effect of \code{target.obj}. If \code{stratification} is specified, then replicating the conditional average treatment effect stratified by \code{stratification} and \code{stratification_joint} by weighting based on the residual variables, namely, variables that are specified in \code{confounders_external_name} while not in \code{stratification}.
    #' @param target.obj An object of class \code{Estimator} or list.
    #' @param weighting_estimator A string specifying a weighting estimator for generalizing/transporting the estimates to \code{target.obj}. The allowed estimators are: \code{"balancing"}, and \code{"modeling"}.
    #' @param weighting_method A string specifying which model for selection to use. Possible values are found using \code{names(getModelInfo())}. See \url{http://topepo.github.io/caret/train-models-by-tag.html}.
    #' @param stratification An optional string vector containing variables to define subgroup. If \code{!is.NULL(stratification)}, \code{source.obj} will compute both weighted and unweighted conditional average treatment effect based on these variables, \code{target.obj} will calculate the conditional average treatment effect based on these variables.
    #' @param stratification_joint An optional logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification} when \code{stratification} is specified.
    RCTrep = function(target.obj,
                      weighting_estimator,weighting_method,
                      stratification, stratification_joint){
      #browser()
      private$set_weighted_ATE_SE(target.obj$data, weighting_estimator,weighting_method)
      if(!is.null(stratification)){
        private$set_weighted_CATE(target=target.obj$data,
                                  stratification = stratification,
                                  stratification_joint = stratification_joint,
                                  weighting_estimator = weighting_estimator,
                                  weighting_method = weighting_method)
      }
    },

    #' @description Get conditional average treatment effect of subgroups defined by \code{stratification} and \code{stratification_joint}. If \code{stratification_joint=FALSE}, then the method return conditional average treatment effect of subgroups stratified by each of variables in \code{stratification}.
    #' @param stratification An string vector containing variables to define subgroup.
    #' @param stratification_joint An logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification}.
    #' @return A data frame. If \code{stratification_joint=TRUE}, then the method returns a data frame with N rows and J columns, where N represents the number of subgroups, and J is equal to the sum of number of variables in \code{stratification} and 3 (three additional columns with name \code{cate}, \code{se}, and \code{size}, representing the estimated conditional average treatment effect of this subgroup, standard error of the estimate, and the sample size of the subgroup). If \code{stratification_joint=FALSE}, then the method returns a data frame with N rows and 5 columns, where N represents the number of subgroups stratified by each variable in \code{stratification} and 5 columns with name \code{name}, \code{value}, \code{cate}, \code{se}, and \code{size}, representing the name of a variable used to stratify the population, a level of the variable, the estimated conditional average treatment effect of this subgroup, standard error of the estimate, and the sample size of the subgroup).
    get_CATE = function(stratification,stratification_joint){
      #browser()
      if(stratification_joint){
        CATE_mean_se <- private$set_CATEestimation4JointStratification(stratification)
      } else {
        CATE_mean_se <- private$set_CATEestimation4SeperateStratification(stratification)
      }
      return(CATE_mean_se)
    },

    #' @description Plot the forest plot of conditional average treatment effect of subgroups defined by \code{stratification} and \code{stratification_joint}. The method first call public method \code{get_CATE(stratification,stratification_joint)}, then plot the results.
    #' @param stratification An string vector containing variables to define subgroup.
    #' @param stratification_joint An logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification}.
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

      out <- list(plot=p_plot,est.cate=CATE)
      out
    }
  ),
  #-------------------------private fields and methods----------------------------#
  private = list(

    set_weighted_ATE_SE = function(target.data, weighting_estimator,weighting_method){
      #browser()
      weight <- setWeight(source=self$data,
                          target=target.data,
                          weighting_estimator=weighting_estimator,
                          vars_weighting=self$confounders_external_name,
                          weighting_method=weighting_method)
      ATE_se_weighted <- private$est_weighted_ATE_SE(weight)
      self$ATE_weighted <- ATE_se_weighted$est
      self$ATE_se_weighted <- ATE_se_weighted$se
    },

    set_weighted_CATE = function(target.data,stratification,stratification_joint,weighting_estimator,weighting_method){
      #browser()
      if(stratification_joint){
        #browser()
        self$CATE_weighted <- private$set_WeightedCATEestimation4JointStratification(target.data,
                                                                                     weighting_estimator,
                                                                                     weighting_method,
                                                                                     stratification)
      } else {
        self$CATE_weighted <- private$set_WeightedCATEestimation4SeperateStratification(target.data,
                                                                                        weighting_estimator,
                                                                                        weighting_method,
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

    set_WeightedCATEestimation4JointStratification = function(target.data,weighting_estimator,weighting_method,stratification){
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
                            weighting_estimator=weighting_estimator,
                            weighting_method=weighting_method,
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

    set_WeightedCATEestimation4SeperateStratification = function(target.data,weighting_estimator,weighting_method,stratification){
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
                              weighting_estimator=weighting_estimator,
                              weighting_method=weighting_method,
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



















