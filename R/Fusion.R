#' @title Validation of estimates of conditional average treatment effects in objects of class \code{TEstimator} and \code{SEstimator}.
#'
#' @param ... objects of class \code{TEstimator} and \code{SEstimator}.
#' @param stratification a character vector specifying variables. The variables are used to select subgroups individually or in combination depending on \code{stratification_joint}. Default value is \code{NULL}.
#' @param stratification_joint a logical indicating if subgroups are selected based on levels of individual variable in \code{stratification} or levels of combined variables in \code{stratifiation}. Default value is NULL.
#' @returns an R6 object
#' @examples
#' \donttest{
#' source.data <- RCTrep::source.data[sample(dim(RCTrep::source.data)[1],500),]
#' target.data <- RCTrep::target.data[sample(dim(RCTrep::target.data)[1],500),]
#'
#' vars_name <- list(confounders_treatment_name = c("x1","x2","x3","x4","x5","x6"),
#'                   treatment_name = c('z'),
#'                   outcome_name = c('y')
#' )
#' confounders_sampling_name <- c("x2","x6")
#'
#' source.obj <- TEstimator_wrapper(
#'  Estimator = "G_computation",
#'  data = source.data,
#'  vars_name = vars_name,
#'  outcome_method = "glm",
#'  outcome_form=y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
#'  name = "RWD",
#'  data.public = FALSE
#' )
#'
#' target.obj <- TEstimator_wrapper(
#'  Estimator = "Crude",
#'  data = target.data,
#'  vars_name = vars_name,
#'  name = "RCT",
#'  data.public = FALSE,
#'  isTrial = TRUE
#' )
#'
#' strata <- c("x1","x4")
#' source.rep.obj <- SEstimator_wrapper(Estimator = "Exact",
#'                                      target.obj = target.obj,
#'                                      source.obj = source.obj,
#'                                      confounders_sampling_name =
#'                                      confounders_sampling_name)
#' source.rep.obj$EstimateRep(stratification = strata, stratification_joint = TRUE)
#'
#' fusion <- Fusion$new(target.obj,
#'                      source.obj,
#'                      source.rep.obj)
#' fusion$plot()
#' fusion$evaluate()
#' }
#'
#' @export
Fusion <- R6::R6Class(
  "Fusion",
  #-------------------------public fields-----------------------------#
  public = list(
    objs.cate.data = data.frame(),
    objs.ate.data = data.frame(),
    stratification = NA,
    stratification_joint = NA,
    # one RCT, and >=1 RWD studies
    RCT.study.name = NA,
    RWD.study.name = NA,

    initialize = function(..., stratification = NULL, stratification_joint = NULL){
      #browser()
      objs <- list(...)
      SEstimator.id <- find_SEstimator_obj(...)
      TEstimator.id <- find_TEstimator_obj(...)
      RCT.id <- find_trial_obj(...)

      # if there is an object of class SEstimator, then we compare all objects
      # on the same level defined in object$estimates$CATE as the object of class SEstimator
      # else, if there is no object of class SEstimator, meaning we are comparing
      # objects of class TEstimator without making covariates balanced across objects.
      # then we are comparing estimates on the level of confounders_treatment_name
      # across objects of class TEstimator, or we specify stratification and stratification_joint
      if(SEstimator.id>0){
        if("name" %in% colnames(objs[[SEstimator.id]]$estimates$CATE)){
          self$stratification <- unique(objs[[SEstimator.id]]$estimates$CATE$name)
          self$stratification_joint <- FALSE
        } else {
          strata.id <- !colnames(objs[[SEstimator.id]]$estimates$CATE) %in% c("y1.hat","y0.hat","cate","se","size","pt","py")
          self$stratification <- colnames(objs[[SEstimator.id]]$estimates$CATE)[strata.id]
          self$stratification_joint <- TRUE
        }
      } else if (TEstimator.id>0) {
        if(is.null(stratification)){
          message("You do not specify variable for group_by, then we specify stratification==confounders_treatment_name")
          self$stratification <- objs[[TEstimator.id]]$.__enclos_env__$private$confounders_treatment_name
          self$stratification_joint <- TRUE
        } else {
          self$stratification <- stratification
          self$stratification_joint <- stratification_joint
        }
      } else{
        stop("please input at least two objects of class either TEstimator or SEstimator for comparison!")
      }

      self$RWD.study.name <- unique(find_RWD_study_name(...))

      if(RCT.id>0){
        self$RCT.study.name <- objs[[RCT.id]]$name
      } else {
        self$RCT.study.name <- NA
      }

      private$aggregate_cate_estimates(...)
      private$aggregate_ate_estimates(...)
    },

    plot = function(){
      n.estimators <- length(unique(self$objs.ate.data$estimator))
      p <- bind_rows(self$objs.ate.data, self$objs.cate.data) %>%
              select(est, ci_l, ci_u, group_name, estimator,size, study) %>%
              ggplot(aes(x = est, y = group_name, color = study, shape = estimator)) +
              geom_point(position = position_dodge(0.7), aes(size=size)) +
              geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = .5, position = position_dodge(0.7)) +
              geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = .5) +
              xlab("treatment effet of populations")
              #xlab("survival difference (95% CI)") +
              ggtitle("Comparison of estimates") +
              theme(axis.text.x = element_text(vjust = -2.5))+
              theme(axis.title.x = element_text(margin = margin(t = 8))) +
              coord_cartesian(clip="off") #+
              # annotate("segment",
              #           x = 0.05, xend = 0.25, y = 0.4, yend = 0.4,
              #           arrow=arrow(length=unit(0.2, "cm"))) +
              # annotate("segment",
              #           x = -0.05, xend = -0.25, y = 0.4, yend = 0.4,
              #           arrow=arrow(length=unit(0.2, "cm"))) +
              # annotate("text", x =  0.2, y = 0.2, label = "surgery better") +
              # annotate("text", x = -0.3, y = 0.2, label = "chemo+surgery better")

      if(n.estimators>6) {p <- p + scale_shape_manual(values = seq(n.estimators))}
      p
    },

    print = function(){
      estimates <- bind_rows(self$objs.ate.data, self$objs.cate.data) %>%
                             select(group_name, study, estimator, est, ci_l, ci_u, size, y1.hat, y0.hat) %>%
                             mutate_if(is.numeric, round, 3) %>%
                             group_by(group_name)
      print(estimates, row.names = FALSE)
    },

    evaluate = function(){
      #browser()
      if(is.na(self$RCT.study.name)) stop("there is no RCT as a gold standard for methods evaluation, hence the function is not executable!")

      objs.data <- bind_rows(self$objs.ate.data,
                             self$objs.cate.data)
      by_groups <- objs.data %>% group_by(group_name)
      n_groups <- dim(by_groups %>% group_keys())[1]
      group_id_4each_row <- by_groups %>% group_indices()
      objs.data <- bind_cols(objs.data, group_id=group_id_4each_row)
      rwd.DF <- NULL
      for (group.id in seq(n_groups)) {
        group_df <- objs.data %>% filter(group_id_4each_row==group.id)
        rct.df <- group_df %>% filter(study==self$RCT.study.name)
        rwd.df <- group_df %>% filter(study %in% self$RWD.study.name)
        rct.est <- rct.df$est
        rct.ci_l <- rct.df$ci_l
        rct.ci_u <- rct.df$ci_u
        rwd.df <- rwd.df %>% mutate(bias=abs(est-rct.est) *100,
                                    mse=(est-rct.est)^2 *100,
                                    len_ci= ci_u-ci_l,
                                    agg.est= (est > rct.ci_l) & (est < rct.ci_u),
                                    agg.reg = ifelse(rct.ci_l>0, ci_l>0,
                                                                ifelse(rct.ci_u<0, ci_u <0, (ci_l<0 & ci_u>0)))
                                    ) %>% mutate_if(is.numeric, round, 3)
        rwd.DF <- bind_rows(rwd.DF, rwd.df)
      }

      evaluation <- rwd.DF %>%
        select(group_name,
               estimator,
               size,
               #bias,
               mse,
               len_ci,
               agg.est,
               agg.reg) %>%
        group_by(group_name) %>%
        arrange(mse, .by_group = TRUE)

      print(evaluation, row.names = FALSE)

    }

  ),

  private = list(

    aggregate_cate_estimates = function(...){
      #browser()
      for(obj in list(...)){
        if("TEstimator" %in% class(obj)){
          obj.data <- obj$get_CATE(self$stratification,self$stratification_joint) %>%
            mutate(estimator=obj$id, study=obj$name) #%>% select(-c("pt","py"))
          if("pt" %in% colnames(obj.data)) obj.data <- obj.data %>% select(-pt)
          if("py" %in% colnames(obj.data)) obj.data <- obj.data %>% select(-py)
        } else if ("SEstimator" %in% class(obj)){
          obj.data <- obj$estimates$CATE %>% mutate(estimator=obj$id, study=obj$name)
          if("pt" %in% colnames(obj.data)) obj.data <- obj.data %>% select(-pt)
          if("py" %in% colnames(obj.data)) obj.data <- obj.data %>% select(-py)
        } else {
          stop("only class TEstimator and SEstimator are allowed as input!")
        }
        self$objs.cate.data <- rbind(self$objs.cate.data, obj.data)
      }

      self$objs.cate.data <- self$objs.cate.data %>% mutate(ci_l=cate-1.98*se, ci_u=cate+1.98*se) %>% rename(est = cate)

      if(self$stratification_joint){
        self$objs.cate.data <- self$objs.cate.data %>%
          mutate(group_name = apply(.[,self$stratification], 1, function(x)
            paste(self$stratification,x,sep = "=",collapse = ",")))
      } else{
        self$objs.cate.data <- self$objs.cate.data %>%
          mutate(group_name = apply(.[,c("name","value")], 1, function(x)
            paste(x[1],x[2],sep = "=")))
      }
    },

    aggregate_ate_estimates = function(...){
      #browser()
      objs.ate.data <- data.frame()
      objs.ate.data <- do.call(rbind,lapply(list(...),
                                            function(x)
                                              bind_rows(objs.ate.data,
                                                        c(x$estimates$ATE,
                                                          estimator = x$id,
                                                          study = x$name,
                                                          size = x$statistics$n
                                                          ))))
      objs.ate.data <- objs.ate.data %>% mutate(group_name = "pop",
                                                ci_l = est - 1.98*se,
                                                ci_u = est + 1.98*se)
      self$objs.ate.data <- objs.ate.data
    }
  )
)
