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
    name = character(),
    statistics = list(n=numeric(),
                      density_confounders=data.frame()),

    data = NULL,

    estimates = list(ATE=list(est=numeric(),
                              se=numeric(),
                              power=numeric()),
                     ATE_weighted = list(est=numeric(),
                                         se=numeric(),
                                         power=numeric()),
                     CATE = data.frame(),
                     CATE_weighted = data.frame()),
    model = list(),

    #-------------------------constructor-----------------------------#
    #' @description Create a new \code{Estimator} object
    #' @param df A data frame containing variables in \code{vars_name}
    #' @param vars_name vars_name A list containing four vectors \code{confounders_internal}, \code{treatment_name}, and \code{outcome_name}. \code{confounders_internal} is a character vector containing the adjustment variables, which, along with \code{Estimator} and the corresponding \code{outcome_method} or \code{treatment_method} to correct for confounding; \code{outcome_name} is a character vector of length one containing the variable name of outcome; \code{treatment_name} is a character vector of length one containing the variable name of treatment.
    initialize = function(df, vars_name, name) {
      self$name <- name
      self$data <- df
      self$data$id <- seq(dim(df)[1])
      private$confounders_internal_name <- vars_name$confounders_internal
      private$treatment_name <- vars_name$treatment_name
      private$outcome_name <- vars_name$outcome_name
      self$statistics <- list(n=dim(df)[1],
                              density_confounders=private$est_joint_denstiy())
    },

    #' @description Replicating the average treatment effect of \code{target.obj}. If \code{stratification} is specified, then replicating the conditional average treatment effect stratified by \code{stratification} and \code{stratification_joint} by weighting based on the residual variables, namely, variables that are specified in \code{confounders_internal_name} while not in \code{stratification}.
    #' @param target.obj An object of class \code{Estimator} or list.
    #' @param weighting_estimator A string specifying a weighting estimator for generalizing/transporting the estimates to \code{target.obj}. The allowed estimators are: \code{"balancing"}, and \code{"modeling"}.
    #' @param weighting_method A string specifying which model for selection to use. Possible values are found using \code{names(getModelInfo())}. See \url{http://topepo.github.io/caret/train-models-by-tag.html}.
    #' @param stratification An optional string vector containing variables to define subgroup. If \code{!is.NULL(stratification)}, \code{source.obj} will compute both weighted and unweighted conditional average treatment effect based on these variables, \code{target.obj} will calculate the conditional average treatment effect based on these variables.
    #' @param stratification_joint An optional logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification} when \code{stratification} is specified.
    EstimateRep = function(target.obj,
                           weighting_estimator="Balancing", weighting_method="glm",
                           stratification=NULL, stratification_joint=FALSE) {
      private$weighting_estimator <- weighting_estimator
      private$weighting_method <- weighting_method

      #browser()
      private$set_weighted_ATE_SE(target.obj$data)
      if (!is.null(stratification)) {
        self$estimates$CATE <- self$get_CATE(stratification = stratification,
                                             stratification_joint = stratification_joint)
        private$set_weighted_CATE_SE(
          target.data = target.obj$data,
          stratification = stratification,
          stratification_joint = stratification_joint
        )
      }
    },

    #' @description Get conditional average treatment effect of subgroups defined by \code{stratification} and \code{stratification_joint}. If \code{stratification_joint=FALSE}, then the method return conditional average treatment effect of subgroups stratified by each of variables in \code{stratification}.
    #' @param stratification An string vector containing variables to define subgroup.
    #' @param stratification_joint An logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification}.
    #' @return A data frame. If \code{stratification_joint=TRUE}, then the method returns a data frame with N rows and J columns, where N represents the number of subgroups, and J is equal to the sum of number of variables in \code{stratification} and 3 (three additional columns with name \code{cate}, \code{se}, and \code{size}, representing the estimated conditional average treatment effect of this subgroup, standard error of the estimate, and the sample size of the subgroup). If \code{stratification_joint=FALSE}, then the method returns a data frame with N rows and 5 columns, where N represents the number of subgroups stratified by each variable in \code{stratification} and 5 columns with name \code{name}, \code{value}, \code{cate}, \code{se}, and \code{size}, representing the name of a variable used to stratify the population, a level of the variable, the estimated conditional average treatment effect of this subgroup, standard error of the estimate, and the sample size of the subgroup).
    get_CATE = function(stratification, stratification_joint) {
      # browser()
      if (stratification_joint) {
        CATE_mean_se <- private$est_CATEestimation4JointStratification(stratification)
      } else {
        CATE_mean_se <- private$est_CATEestimation4SeperateStratification(stratification)
      }
      return(CATE_mean_se)
    },


    #' @description Plot the forest plot of conditional average treatment effect of subgroups defined by \code{stratification} and \code{stratification_joint}. The method first call public method \code{get_CATE(stratification,stratification_joint)}, then plot the results.
    #' @param stratification An string vector containing variables to define subgroup.
    #' @param stratification_joint An logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification}.
    plot_CATE = function(stratification, stratification_joint = FALSE) {
      # browser()
      CATE <- self$get_CATE(stratification, stratification_joint)
      var_names <- colnames(CATE)[!colnames(CATE) %in% c("y1.hat", "y0.hat", "cate", "se", "size")]
      subgroup_name_level <- apply(
        CATE[, !colnames(CATE) %in% c("y1.hat", "y0.hat", "cate", "se", "size")], 1,
        function(x) paste(var_names, x, sep = "=", collapse = ",")
      )
      subgroup_name_level <- factor(subgroup_name_level, levels = subgroup_name_level, ordered = T)
      CATE.data <- data.frame(
        group = subgroup_name_level,
        effect_size = CATE$cate,
        ci_l = c(CATE$cate - 1.98 * CATE$se),
        ci_u = c(CATE$cate + 1.98 * CATE$se),
        stringsAsFactors = FALSE
      )

      p_plot <- ggplot(data = CATE.data, aes(x = effect_size, y = group)) +
        geom_point(position = position_dodge(0.5)) +
        geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = .1, position = position_dodge(0.5)) +
        geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5)

      out <- list(plot = p_plot, est.cate = CATE)
      out
    }


  ),
  #-------------------------private fields and methods----------------------------#
  private = list(

    weighting_estimator = NA,
    weighting_method = NA,
    confounders_internal_name = NA,
    treatment_name = NA,
    outcome_name = NA,


    set_ATE = function(){
      ATE_SE <- private$est_ATE_SE(self$data$id)
      self$estimates$ATE$est <- ATE_SE$est
      self$estimates$ATE$se <- ATE_SE$se
    },

    set_CATE = function(stratification, stratification_joint){
      self$estimates$CATE <- self$get_CATE(stratification,stratification_joint)
    },

    get_weight = function(source, target, weighting_estimator,weighting_method,vars_weighting){
      source <- select(source,vars_weighting)
      target <- select(target,vars_weighting)
      source$selection <- 0
      target$selection <- 1
      data <- rbind(source, target)
      data$selection <- as.factor(data$selection)
      if (weighting_estimator == "Balancing") {
        matching_formula <- formula(paste("selection~", paste(vars_weighting, collapse = "+")))
        # default for ATT, means weight for selected 1.
        matchit.obj <- MatchIt::matchit(matching_formula, method = "exact", data = data)
        weight <- matchit.obj$weights[1:dim(source)[1]]
      } else {
        samplingscore <- SelectionScoreModeling(data, vars_weighting, weighting_method)
        weight <- samplingscore / (1 - samplingscore)
      }
      return(weight)
    },

    set_weighted_ATE_SE = function(target.data) {
      # browser()
      weight <- private$get_weight(
        source = self$data,
        target = target.data,
        weighting_estimator = private$weighting_estimator,
        weighting_method = private$weighting_method,
        vars_weighting = private$confounders_internal_name
        )

      ATE_se_weighted <- private$est_weighted_ATE_SE(self$data$id,weight)
      self$estimates$ATE_weighted$y1.hat <- ATE_se_weighted$y1.hat
      self$estimates$ATE_weighted$y0.hat <- ATE_se_weighted$y0.hat
      self$estimates$ATE_weighted$est <- ATE_se_weighted$est
      self$estimates$ATE_weighted$se <- ATE_se_weighted$se
    },

    set_weighted_CATE_SE = function(target.data, stratification, stratification_joint) {
      # browser()
      if (stratification_joint) {
        # browser()
        self$estimates$CATE_weighted <- private$est_WeightedCATEestimation4JointStratification(
                                        target.data,
                                        stratification
                                        )
      } else {
        self$estimates$CATE_weighted <- private$est_WeightedCATEestimation4SeperateStratification(
                                        target.data,
                                        stratification
                                        )
      }
    },

    est_joint_denstiy = function(){
      joint_var_internal <-
        self$data %>%
        count(across(all_of(private$confounders_internal_name))) %>%
        mutate(prop = n/sum(n))
      return(joint_var_internal)
    },

    est_CATEestimation4JointStratification = function(stratification) {
      #browser()
      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      group_sample_size <- group_size(group_data)
      cate <- se <- size <- y1.hat <- y0.hat <- NULL
      for (i in seq(n_groups)) {
        subgroup.id.in.data <- self$data[group_id == i, "id"]
        cate_y1_y0_se <- private$est_ATE_SE(subgroup.id.in.data)
        y1.hat[i] <- cate_y1_y0_se$y1.hat
        y0.hat[i] <- cate_y1_y0_se$y0.hat
        cate[i] <- cate_y1_y0_se$est
        se[i] <- cate_y1_y0_se$se
        size[i] <- group_sample_size[i]
        # print(i)
      }
      CATE_mean_se <- cbind(group_strata, y1.hat, y0.hat, cate, se, size)
      CATE_mean_se <- as.data.frame(CATE_mean_se)
      # browser()
      # colnames(CATE_mean_se) <- c(colnames(patterns),"cate","se")
      return(CATE_mean_se)
    },

    est_CATEestimation4SeperateStratification = function(stratification) {
      # browser()
      group_var <- group_level <- cate <- se <- size <- y1.hat <- y0.hat <- density <- NULL
      i <- 1
      for (var_name in stratification) {
        group_data <- self$data %>% group_by(across(var_name))
        group_strata <- group_data %>% group_keys()
        group_id_4each_obs <- group_data %>% group_indices()
        n_groups <- dim(group_strata)[1]
        group_sample_size <- group_size(group_data)
        for (group_id in seq(n_groups)) {
          subgroup.id.in.data <- self$data[group_id_4each_obs == group_id, "id"]
          group_var[i] <- var_name
          group_level[i] <- group_strata[group_id, 1]
          cate_y1_y0_se <- private$est_ATE_SE(subgroup.id.in.data)
          y1.hat[i] <- cate_y1_y0_se$y1.hat
          y0.hat[i] <- cate_y1_y0_se$y0.hat
          cate[i] <- cate_y1_y0_se$est
          se[i] <- cate_y1_y0_se$se
          size[i] <- group_sample_size[i]

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

    est_WeightedCATEestimation4JointStratification = function(target.data,stratification) {
      # browser()
      cate <- se <- size <- y1.hat <- y0.hat <- NULL
      vars_weighting <- private$confounders_internal_name
      vars_weighting_subgroup <- vars_weighting[!vars_weighting %in% stratification]

      group_data <- self$data %>%
        group_by(across(all_of(stratification)))
      group_strata <- group_data %>% group_keys()
      group_id <- group_data %>% group_indices()
      n_groups <- dim(group_strata)[1]
      cate <- se <- y1.hat <- y0.hat <- NULL
      pattern.type <- sapply(target.data[, stratification], class)

      for (i in seq(n_groups)) {
        pattern <- group_strata[i, ]
        subgroup_selection_condition <- .getSubgroupSelectionExpression(pattern, pattern.type)
        target.subgroup.data <- subset(target.data, eval(parse(text = subgroup_selection_condition)))
        source.subgroup.data <- self$data[group_id == i, ]
        source.subgroup.id.in.data <- self$data[group_id == i, "id"]

        weight <- private$get_weight(
          source = source.subgroup.data,
          target = target.subgroup.data,
          weighting_estimator = private$weighting_estimator,
          weighting_method = private$weighting_method,
          vars_weighting = vars_weighting_subgroup
        )

        # print(i)
        #
        # if(i==3){browser()}

        cate_y1_y0_se <- private$est_weighted_ATE_SE(source.subgroup.id.in.data, weight)
        y1.hat[i] <- cate_y1_y0_se$y1.hat
        y0.hat[i] <- cate_y1_y0_se$y0.hat
        cate[i] <- cate_y1_y0_se$est
        se[i] <- cate_y1_y0_se$se
        size[i] <- sum(weight)
      }
      CATE_mean_se <- cbind(group_strata, y1.hat, y0.hat, cate, se, size)
      CATE_mean_se <- as.data.frame(CATE_mean_se)
      # colnames(CATE_mean_se) <- c(colnames(patterns),"cate","se")
      return(CATE_mean_se)
    },

    est_WeightedCATEestimation4SeperateStratification = function(target.data, stratification) {
      # browser()
      vars_weighting <- private$confounders_internal_name
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

          weight <- privateget_weight(
            source = source.subgroup.data,
            target = target.subgroup.data,
            weighting_estimator = private$weighting_estimator,
            weighting_method = private$weighting_method,
            vars_weighting = vars_weighting_subgroup
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

    fit = function(){},

    est_ATE_SE = function(){},

    est_weighted_ATE_SE = function(){}

  )
)
