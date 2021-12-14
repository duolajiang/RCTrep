#' @title Replicate treatment effect estimates obtained from a randomized control trial using real world data
#'
#' @description The function \code{RCTREP} is used to replicate the estimate of treatment effect from a target randomized control trial based on real-world data (RWD). This function estimate the treatment effect of RWD to ensure the internal validity of the estimates within the study population (namely, the observational data) and weight the resulting estimates to the target population (namely, the RCT) to enable external validity. The function currently implement the following types of estimators of treatment effect: G_computation, inverse propensity score weighting (IPW), and augmented propensity score weighting. The function implement the following two types of weighting estimators to generalize the resulting estimates of treatment effect from RWD to the target RCT: exact matching weights, and selection score weights. Since we regard the sample in the RCT as the target population, weights for each individual in RWD is \eqn{p/(1-p)} so that the weighted population of RWD is representative to the target population.
#'
#' @details An R6 object for both studies are constructed by a wrapper function \code{Estimate()} with user's input of data and estimator for treatment effect. Then \code{Estimate()} return initialized objects \code{source.obj} and \code{target.obj}. \code{source.obj} replicates the target RCT estimate via the class method \code{RCTrep()} with input of target object \code{target.obj} and weighting method.
#'
#' @param Estimator a character specifying an estimator for average treatment effect. The allowed estimators for \code{Estimator} are: \code{"G_computation"}, \code{"IPW"}, and \code{"DR"}. The corresponding object will be created by the wrapper function \code{\link{Estimate()}}.
#' @param weighting_estimator a character specifying a weighting estimator for generalizing/transporting the estimates of \code{source.obj} (initiated using RWD) to \code{target.obj} (initiated using RCT) as to enable replication/comparison between source study (RWD) and target study(RCT). The allowed estimators are: \code{"balancing"}, and \code{"modeling"}. \code{"balancing"} estimator use exact matching to compute the weight; \code{"modeling"} estimator model the probability of being selected to the target RCT (assuming combing target RCT and the source RWD as a population).
#' @param source.data a data frame containing variables named in \code{vars_name} and possible other variables. If not found in \code{source.data}, the function will stop and throw error.
#' @param target.data a data frame or a list
#' @example
#' @return
#' @export
#' @import R6
RCTREP <- function(Estimator="G_computation", weighting_estimator="Balancing",
                   source.data=source.data, target.data=target.data,
                   vars_name,
                   outcome_method="glm", treatment_method="glm",weighting_model = "glm",
                   outcome_formula=NULL,treatment_formula=NULL,selection_formula=NULL,
                   stratification = NULL, stratification_joint = FALSE, strata_cut = NULL,
                   two_models=NULL,
                   ...){
  #browser()
  source.obj <- Estimate(Estimator=Estimator, data=source.data, vars_name=vars_name,
                         outcome_method=outcome_method, treatment_method=treatment_method,two_models=two_models,
                         outcome_formula=outcome_formula, treatment_formula=treatment_formula,
                         stratification=stratification, stratification_joint=stratification_joint, strata_cut=strata_cut,
                         ...)

  if (class(target.data)=="list") {
    target.obj <- list(data=target.data)
  } else {
    target.obj <- Estimate(Estimator=Estimator, data=target.data, vars_name=vars_name,
                           outcome_method=outcome_method, treatment_method=treatment_method,two_models=two_models,
                           outcome_formula=outcome_formula, treatment_formula=treatment_formula,
                           stratification=stratification, stratification_joint=stratification_joint, strata_cut=strata_cut,
                           ...)
  }

  source.obj$RCTrep(target.obj=target.obj,
                    confounders_external_name = vars_name$confounders_external,
                    weighting_estimator= weighting_estimator,weighting_model=weighting_model,
                    stratification, stratification_joint)

  return(list(source.obj=source.obj,
              target.obj=target.obj))
}


#' @export
Estimate <- function(Estimator, data, vars_name,
                     outcome_method="glm", treatment_method="glm", two_models=NULL,
                     outcome_formula, treatment_formula,
                     stratification, stratification_joint, strata_cut,
                     ...){
  #browser()
  if(Estimator=="G_computation"){
    obj <- G_computation$new(df=data,
                             vars_name = vars_name,
                             gc.method = outcome_method,
                             gc.form = outcome_formula,
                             ...)

  } else if (Estimator == "IPW"){
    #browser()
    if(is.factor(data[,vars_name$outcome])) stop("outcome class is factor, please convert it to numeric!")
    obj <- IPW$new(df=data,
                   vars_name = vars_name,
                   treatment_method = treatment_method,
                   treatment_formula = treatment_formula,
                   ...)

  } else if(Estimator == "DR"){
    obj <- DR$new(df=data,
                  vars_name = vars_name,
                  outcome_method=outcome_method,
                  outcome_formula=outcome_formula,
                  treatment_method=treatment_method,
                  treatment_formula=treatment_formula,
                  two_models=two_models,
                  ...)

  } else if(Estimator == "Crude"){

  } else {

  }

  if(!is.null(stratification)){
    #browser()
    if(!is.null(strata_cut)){
      for (i in 1:length(strata_cut)) {
        strata_cut_var_name <- names(strata_cut[i])
        obj$data[,strata_cut_var_name] <- as.character(cut(obj$data[,strata_cut_var_name],
                                                           breaks = strata_cut[[i]]$breaks,
                                                           labels = strata_cut[[i]]$labels,
                                                           include.lowest = TRUE,
                                                           ordered_result = TRUE))
      }
    }

    obj$CATE_mean_se <- obj$get_CATE(stratification,stratification_joint)
  }

  return(obj)
}


#' @export
Plot_estimate <- function(source.obj, target.obj){
  #browser()
  if(class(target.obj$data)=="list"){
    target.obj <- target.obj$data
  }

  source.obj$CATE_mean_se <- AlignOrderofPatternInCATE(
    target.obj$CATE_mean_se,
    source.obj$CATE_mean_se)

  source.obj$CATE_weighted <- AlignOrderofPatternInCATE(
    target.obj$CATE_mean_se,
    source.obj$CATE_weighted)

  if(c("name") %in% colnames(source.obj$CATE_mean_se)){
    subgroup_name_level <- source.obj$CATE_mean_se[,!colnames(source.obj$CATE_mean_se) %in% c("cate","se","size")]
    subgroup_name_level <- apply(subgroup_name_level, 1, function(x) paste(x[1],"=",x[2],sep = ""))
    subgroup_name_level <- factor(subgroup_name_level,levels = subgroup_name_level,ordered = T)
  } else {
    var_names <- colnames(source.obj$CATE_mean_se)[!colnames(source.obj$CATE_mean_se) %in% c("cate","se","size")]
    subgroup_name_level <- apply(source.obj$CATE_mean_se[,!colnames(source.obj$CATE_mean_se) %in% c("cate","se","size")],
                                 1, function(x) paste(var_names,x,sep = "=",collapse = ","))
    subgroup_name_level <- factor(subgroup_name_level,levels = subgroup_name_level,ordered = T)
  }

  ATE <- data.frame(study=c("RWD","RCT","RWD.rep"),
                    group=c("ATE","ATE","ATE"),
                    effect_size=c(source.obj$ATE_mean,
                                  target.obj$ATE_mean,
                                  source.obj$ATE_weighted),
                    ci_l = c(source.obj$ATE_mean-1.98*source.obj$ATE_se,
                             target.obj$ATE_mean-1.98*target.obj$ATE_se,
                             source.obj$ATE_weighted-1.98*source.obj$ATE_se_weighted),
                    ci_u = c(source.obj$ATE_mean+1.98*source.obj$ATE_se,
                             target.obj$ATE_mean+1.98*target.obj$ATE_se,
                             source.obj$ATE_weighted+1.98*source.obj$ATE_se_weighted))

  #CATE <- data.frame(study=numeric(),group=character(),effect_size=numeric(),ci_l=numeric(),ci_u=numeric(),stringsAsFactors = FALSE)

  CATE.RWD <- data.frame(group = subgroup_name_level,
                         effect_size = c(source.obj$CATE_mean_se$cate),
                         ci_l = c(source.obj$CATE_mean_se[,'cate'] - 1.98*source.obj$CATE_mean_se[,'se']),
                         ci_u = c(source.obj$CATE_mean_se[,'cate'] + 1.98*source.obj$CATE_mean_se[,'se']),
                         stringsAsFactors = FALSE)
  CATE.RWD$study <- "RWD"

  CATE.RWD.rep <- data.frame(group = subgroup_name_level,
                             effect_size = c(source.obj$CATE_weighted$cate),
                             ci_l = c(source.obj$CATE_weighted[,'cate'] - 1.98*source.obj$CATE_weighted[,'se']),
                             ci_u = c(source.obj$CATE_weighted[,'cate'] + 1.98*source.obj$CATE_weighted[,'se']),
                             stringsAsFactors = FALSE)
  CATE.RWD.rep$study <- "RWD.rep"

  CATE.RCT <- data.frame(group = subgroup_name_level,
                         effect_size = c(target.obj$CATE_mean_se$cate),
                         ci_l = c(target.obj$CATE_mean_se[,'cate'] - 1.98*target.obj$CATE_mean_se[,'se']),
                         ci_u = c(target.obj$CATE_mean_se[,'cate'] + 1.98*target.obj$CATE_mean_se[,'se']),
                         stringsAsFactors = FALSE)
  CATE.RCT$study <- "RCT"

  data <- rbind(CATE.RCT,CATE.RWD,CATE.RWD.rep,ATE)

  p_table <- cbind(source.obj$CATE_mean_se[,!colnames(source.obj$CATE_mean_se) %in% c("cate","se","size")],
                   target.obj$CATE_mean_se$size,source.obj$CATE_mean_se$size,source.obj$CATE_weighted$size)
  colnames(p_table) <- c(colnames(source.obj$CATE_mean_se)[!colnames(source.obj$CATE_mean_se) %in% c("cate","se","size")],"size_rct","size_rwd","size_rwd_rep")
  p_table <- ggpubr::ggtexttable(p_table, rows = NULL)

  library(ggplot2)
  p_plot <- ggplot2::ggplot(data=data, aes(x=effect_size, y=group, color=study, group=study)) +
                   geom_line(orientation = "y",position=position_dodge(0.5),linetype="dotted")+
                   geom_point(position=position_dodge(0.5))+
                   geom_errorbar(aes(xmin=ci_l, xmax=ci_u), width=.3,position=position_dodge(0.5)) +
                   geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5)

  p <- ggpubr::ggarrange(p_plot,p_table,ncol = 1, nrow = 2)

  return(p)
  #adds the CIs
  #geom_errorbarh(height=.1)+
  #sets the scales
  #note that I reverse the y axis to correctly order the effect #sizes based on my index variable
  #scale_x_continuous(limits=c(-1,5), breaks = c(-1:5))
  #scale_y_continuous(name = "", breaks=c(1:7), labels = data$group)
}


