#' @title R6 class: Estimator base class
#' @description A base R6 class for estimator of average treatment effect that implements the common methods, such as \code{\link{RCTrep}}, \code{\link{get_CATE}}, \code{\link{plot_CATE}}, inheritted by \code{\link{G_computation_base}}, \code{\link{IPW_base}}, and \code{\link{DR_base}} class.
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggpubr ggtexttable ggarrange
#' @import dplyr
TEstimator <- R6::R6Class(
  "TEstimator",
  #-------------------------public fields-----------------------------#
  public = list(
    id = NA,
    name = character(),
    statistics = list(n=numeric(),
                      density_confounders=data.frame()),

    data = NULL,

    estimates = list(ATE=data.frame(y1.hat=NA,
                                    y0.hat=NA,
                                    est=NA,
                                    se=NA),
                     CATE = data.frame()),
    model = list(),

    #-------------------------constructor-----------------------------#
    #' @description Create a new \code{Estimator} object
    #' @param df A data frame containing variables in \code{vars_name}
    #' @param vars_name vars_name A list containing four vectors \code{confounders_internal}, \code{treatment_name}, and \code{outcome_name}. \code{confounders_internal} is a character vector containing the adjustment variables, which, along with \code{Estimator} and the corresponding \code{outcome_method} or \code{treatment_method} to correct for confounding; \code{outcome_name} is a character vector of length one containing the variable name of outcome; \code{treatment_name} is a character vector of length one containing the variable name of treatment.
    initialize = function(df, vars_name, name) {
      self$name <- name
      self$data <- df
      self$data$id <- seq(dim(df)[1])
      private$confounders_treatment_name <- vars_name$confounders_treatment_name
      private$treatment_name <- vars_name$treatment_name
      private$outcome_name <- vars_name$outcome_name
      self$statistics <- list(n=dim(df)[1],
                              density_confounders=private$est_joint_denstiy())
    },

    #' @description Replicating the average treatment effect of \code{target.obj}. If \code{stratification} is specified, then replicating the conditional average treatment effect stratified by \code{stratification} and \code{stratification_joint} by weighting based on the residual variables, namely, variables that are specified in \code{confounders_treatment_name} while not in \code{stratification}.
    #' @param target.obj An object of class \code{Estimator} or list.
    #' @param weighting_estimator A string specifying a weighting estimator for generalizing/transporting the estimates to \code{target.obj}. The allowed estimators are: \code{"balancing"}, and \code{"modeling"}.
    #' @param weighting_method A string specifying which model for selection to use. Possible values are found using \code{names(getModelInfo())}. See \url{http://topepo.github.io/caret/train-models-by-tag.html}.
    #' @param stratification An optional string vector containing variables to define subgroup. If \code{!is.NULL(stratification)}, \code{source.obj} will compute both weighted and unweighted conditional average treatment effect based on these variables, \code{target.obj} will calculate the conditional average treatment effect based on these variables.
    #' @param stratification_joint An optional logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification} when \code{stratification} is specified.
    # EstimateRep = function(target.obj,
    #                        weighting_estimator="Balancing", weighting_method="glm",
    #                        stratification=NULL, stratification_joint=FALSE) {
    #   private$weighting_estimator <- weighting_estimator
    #   private$weighting_method <- weighting_method
    #
    #   #browser()
    #   private$set_weighted_ATE_SE(target.obj$data)
    #   if (!is.null(stratification)) {
    #     if(isTRUE(all.equal(stratification, private$confounders_external_name))&stratification_joint){
    #       self$estimates$CATE <- self$get_CATE(stratification = stratification,
    #                                            stratification_joint = stratification_joint)
    #       self$estimates$CATE_weighted <- self$estimates$CATE
    #     } else {
    #       self$estimates$CATE <- self$get_CATE(stratification = stratification,
    #                                            stratification_joint = stratification_joint)
    #       private$set_weighted_CATE_SE(
    #         target.data = target.obj$data,
    #         stratification = stratification,
    #         stratification_joint = stratification_joint
    #       )
    #     }
    #   }
    # },

    #' @description Get conditional average treatment effect of subgroups defined by \code{stratification} and \code{stratification_joint}. If \code{stratification_joint=FALSE}, then the method return conditional average treatment effect of subgroups stratified by each of variables in \code{stratification}.
    #' @param stratification An string vector containing variables to define subgroup.
    #' @param stratification_joint An logical defining the subgroup based on joint distribution of variables or univariate distribution in \code{stratification}.
    #' @return A data frame. If \code{stratification_joint=TRUE}, then the method returns a data frame with N rows and J columns, where N represents the number of subgroups, and J is equal to the sum of number of variables in \code{stratification} and 3 (three additional columns with name \code{cate}, \code{se}, and \code{size}, representing the estimated conditional average treatment effect of this subgroup, standard error of the estimate, and the sample size of the subgroup). If \code{stratification_joint=FALSE}, then the method returns a data frame with N rows and 5 columns, where N represents the number of subgroups stratified by each variable in \code{stratification} and 5 columns with name \code{name}, \code{value}, \code{cate}, \code{se}, and \code{size}, representing the name of a variable used to stratify the population, a level of the variable, the estimated conditional average treatment effect of this subgroup, standard error of the estimate, and the sample size of the subgroup).
    get_CATE = function(stratification, stratification_joint=TRUE) {
      # browser()
      if("Synthetic_TEstimator" %in% class(self)) {
        return(self$estimates$CATE)
      }

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
    plot_CATE = function(stratification, stratification_joint = TRUE) {
      #browser()
      data.cate <- self$get_CATE(stratification, stratification_joint)
      colnames.subgroups <- colnames(data.cate)
      var_names <- colnames.subgroups[!colnames.subgroups %in% c("y1.hat","y0.hat","cate","se","size")]
      var_names_data <- data.cate[,var_names]
      subgroup_name_level <- apply(var_names_data, 1, function(x) paste(var_names, x, sep = "=", collapse = ","))
      subgroup_name_level <- factor(subgroup_name_level, levels = subgroup_name_level, ordered = T)

      #browser()
      df <- data.cate %>%
        select(cate,se,size) %>%
        mutate(group=subgroup_name_level,
               ci_l=cate-1.98*se,
               ci_u=cate+1.98*se)

      cate_plot <- ggplot2::ggplot(data = df, aes(x = cate, y = group)) +
        geom_point(position = position_dodge(0.5), aes(size=size)) +
        geom_errorbar(aes(xmin = ci_l, xmax = ci_u),
                      width = .3, position = position_dodge(0.5)) +
        geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
        ggtitle("Estimate of conditional average treatment effect.") +
        theme(plot.title = element_text(),
              legend.position = "none")

      return(cate_plot)
    },

    diagnosis_t_overlap = function(stratification, stratification_joint=TRUE){
      #browser()
      if(missing(stratification)){
        vars_name <- private$confounders_treatment_name
      } else{
        vars_name <- stratification
      }

      if(isTRUE(stratification_joint)){
        # position=fill: a percent stacked barplot
        p.prop <- self$data %>%
          select(vars_name, private$treatment_name) %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ","))) %>%
          select(group_name, private$treatment_name) %>%
          ggplot(aes(x=group_name, fill=factor(eval(parse(text = private$treatment_name))))) +
          geom_bar(position = "fill") +
          ylab("proportion") +
          labs(fill = "treatment") +
          coord_flip() +
          theme(legend.position="none")


        p.count <- self$data %>%
          select(vars_name, private$treatment_name) %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ","))) %>%
          select(group_name, private$treatment_name) %>%
          ggplot(aes(x=group_name, fill=factor(eval(parse(text = private$treatment_name))))) +
          geom_bar(stat = "count") +
          labs(fill = "treatment") +
          coord_flip() +
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.y=element_blank())
      } else {
        # apply: X must be matrix or array
        # data then add mutate then group and summarise, can't fist group, summarise and mutate...
        df <- lapply(stratification, function(x) self$data %>%
                         mutate(group_name = apply(as.matrix(.[,x]), 1, function(y) paste(x,y,sep="="))) %>%
                         group_by(group_name, eval(parse(text=private$treatment_name))) %>%
                         summarise(count=n())
                       )
        data <- data.frame()
        for (df.strata in df) {
          data <- bind_rows(data, df.strata)
        }
        colnames(data) <- c("group_name",private$treatment_name,"count")

        # the default behavior of geom_bar() is to count the rows for each x value. It doesn't expect y value, since it's going to
        # count that up itself - in fact, it will flag a warning if you give it one, since it thinks you are confused. How aggregation
        # is to be performed is specified as an argument to geom_bar(), which is stat="count" for default value.
        # stat="identity": you're telling ggplot2 to skip the aggregation and that you'll provide the y values.
        # If you use stat="identity", you need to provide y value.
        p.prop <- ggplot(data = data, aes(x=group_name, y=count, fill=factor(eval(parse(text = private$treatment_name))))) +
          geom_bar(stat = "identity", position = "fill") +
          ylab("proportion") +
          labs(fill = "treatment") +
          coord_flip() +
          theme(legend.position="none")

        p.count <- ggplot(data = data, aes(x=group_name, y=count, fill=factor(eval(parse(text = private$treatment_name))))) +
          geom_bar(stat = "identity") +
          ylab("count") +
          labs(fill = "treatment") +
          coord_flip() +
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.y=element_blank())

      }

      tgrob <- ggpubr::text_grob(c("Treatment overlap within subpopulations"))
      ggpubr::ggarrange(tgrob, NULL, p.prop, p.count, ncol=2, nrow=2, heights = c(1,5))

    },

    diagnosis_y_overlap = function(stratification, stratification_joint=TRUE){
      #browser()

      if(missing(stratification)){
        vars_name <- private$confounders_treatment_name
      } else{
        vars_name <- stratification
      }

      if(test_binary(self$data[,private$outcome_name])){
        p.count.t1 <- self$data %>%
          select(vars_name, private$outcome_name, private$treatment_name) %>%
          filter(eval(parse(text=private$treatment_name)) == "1") %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ","))) %>%
          select(group_name, private$outcome_name) %>%
          ggplot(aes(x=group_name, fill=factor(eval(parse(text = private$outcome_name))))) +
          geom_bar(stat = "count") +
          labs(fill = "outcome") +
          ggtitle("survival count in treatment group")+
          coord_flip() +
          theme(legend.position="none")

        p.count.t0 <- self$data %>%
          select(vars_name, private$outcome_name, private$treatment_name) %>%
          filter(eval(parse(text=private$treatment_name)) == "0") %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ","))) %>%
          select(group_name, private$outcome_name) %>%
          ggplot(aes(x=group_name, fill=factor(eval(parse(text = private$outcome_name))))) +
          geom_bar(stat = "count") +
          labs(fill = "outcome") +
          ggtitle("survival count in control group")+
          coord_flip() +
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.y=element_blank())

        tgrob <- ggpubr::text_grob(c("Outcome overlap within subpopulations"))
        ggpubr::ggarrange(tgrob, NULL, p.count.t1, p.count.t0,ncol=2, nrow=2, heights = c(1,5))

      } else {
        p.dis <- self$data %>%
            select(vars_name, private$outcome_name, private$treatment_name) %>%
            mutate(group_name = apply(.[,vars_name], 1, function(x)
              paste(vars_name,x,sep = "=",collapse = ","))) %>%
            ggplot(aes(x=group_name, y=eval(parse(text = private$outcome_name)), fill=factor(eval(parse(text = private$treatment_name))))) +
            geom_boxplot() +
            ylab("outcome") +
            labs(fill = "treatment") +
            ggtitle("outcome distribution in groups")+
            coord_flip()

        p.dis
      }

    },

    summary = function(){}

  ),
  #-------------------------private fields and methods----------------------------#
  private = list(

    confounders_treatment_name = NA,
    treatment_name = NA,
    outcome_name = NA,
    var_method = "sandwitch",
    isTrial = FALSE,


    set_ATE = function(){
      ATE_SE <- private$est_ATE_SE(self$data$id)
      self$estimates$ATE$y1.hat <- ATE_SE$y1.hat
      self$estimates$ATE$y0.hat <- ATE_SE$y0.hat
      self$estimates$ATE$est <- ATE_SE$est
      self$estimates$ATE$se <- ATE_SE$se
    },

    set_CATE = function(stratification, stratification_joint){
      self$estimates$CATE <- self$get_CATE(stratification,stratification_joint)
    },

    est_joint_denstiy = function(){
      #browser()
      joint_var_internal <-
        self$data %>%
        group_by(across(all_of(c(private$confounders_treatment_name,private$treatment_name, private$outcome_name)))) %>%
        summarise(count=n())
      joint_var_internal <- as.data.frame(joint_var_internal)
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
          size[i] <- group_sample_size[group_id]

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

    # plot_cate = function(){
    #   data.cate <- self$estimates$CATE
    #   colnames.subgroups <- colnames(data.cate)
    #   var_names <- colnames.subgroups[!colnames.subgroups %in% c("y1.hat","y0.hat","cate","se","size")]
    #   var_names_data <- data.cate[,var_names]
    #   subgroup_name_level <- apply(var_names_data, 1, function(x) paste(var_names, x, sep = "=", collapse = ","))
    #   subgroup_name_level <- factor(subgroup_name_level, levels = subgroup_name_level, ordered = T)
    #
    #   #browser()
    #   df <- data.cate %>%
    #     select(cate,se,size) %>%
    #     mutate(group=subgroup_name_level,
    #            ci_l=cate-1.98*se,
    #            ci_u=cate+1.98*se)
    #
    #   cate.plot <- ggplot2::ggplot(data = df, aes(x = cate, y = group)) +
    #     geom_point(position = position_dodge(0.5), aes(size=size)) +
    #     geom_errorbar(aes(xmin = ci_l, xmax = ci_u),
    #                   width = .3, position = position_dodge(0.5)) +
    #     geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
    #     ggtitle("estimate of CATE.") +
    #     theme(plot.title = element_text(),
    #           legend.position = "none")
    #
    #   return(cate.plot)
    # },

    fit = function(){},

    est_ATE_SE = function(){},

    est_weighted_ATE_SE = function(){}

  )
)
