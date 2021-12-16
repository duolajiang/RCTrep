#' @title Summary average treatment effect and possible conditional average treatment
#'
#' @param source.obj An object of class \code{\link{Estimator}} resulting from source data
#' @param target.obj An object of class \code{\link{Estimator}} resulting from target data
#'
#' @return a plot with a forest plot and a table with numeric results
#' @export
summary <- function(source.obj, target.obj){
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

  ATE <- data.frame(study=c("source","target","source.weighted"),
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
  CATE.RWD$study <- "source"

  CATE.RWD.rep <- data.frame(group = subgroup_name_level,
                             effect_size = c(source.obj$CATE_weighted$cate),
                             ci_l = c(source.obj$CATE_weighted[,'cate'] - 1.98*source.obj$CATE_weighted[,'se']),
                             ci_u = c(source.obj$CATE_weighted[,'cate'] + 1.98*source.obj$CATE_weighted[,'se']),
                             stringsAsFactors = FALSE)
  CATE.RWD.rep$study <- "source.weighted"

  CATE.RCT <- data.frame(group = subgroup_name_level,
                         effect_size = c(target.obj$CATE_mean_se$cate),
                         ci_l = c(target.obj$CATE_mean_se[,'cate'] - 1.98*target.obj$CATE_mean_se[,'se']),
                         ci_u = c(target.obj$CATE_mean_se[,'cate'] + 1.98*target.obj$CATE_mean_se[,'se']),
                         stringsAsFactors = FALSE)
  CATE.RCT$study <- "target"

  data <- rbind(CATE.RCT,CATE.RWD,CATE.RWD.rep,ATE)

  p_table <- cbind(source.obj$CATE_mean_se[,!colnames(source.obj$CATE_mean_se) %in% c("cate","se","size")],
                   target.obj$CATE_mean_se$size,source.obj$CATE_mean_se$size,source.obj$CATE_weighted$size)
  colnames(p_table) <- c(colnames(source.obj$CATE_mean_se)[!colnames(source.obj$CATE_mean_se) %in% c("cate","se","size")],"size_target","size_source","size_source_weighted")
  p_table <- ggpubr::ggtexttable(p_table, rows = NULL)

  library(ggplot2)
  p_plot <- ggplot2::ggplot(data=data, aes(x=effect_size, y=group, color=study, group=study)) +
    geom_line(orientation = "y",position=position_dodge(0.5),linetype="dotted")+
    geom_point(position=position_dodge(0.5))+
    geom_errorbar(aes(xmin=ci_l, xmax=ci_u), width=.3,position=position_dodge(0.5)) +
    geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5)

  rownames(target.obj$CATE_mean_se) <- NULL
  rownames(source.obj$CATE_mean_se) <- NULL
  rownames(source.obj$CATE_weighted) <- NULL

  out <- list(summary.plot=p_plot,
              est.target=target.obj$CATE_mean_se,
              est.source=source.obj$CATE_mean_se,
              est.source.weighted=source.obj$CATE_weighted,
              source.model=source.obj$model,
              target.model=target.obj$model)
  out


  #ggpubr::ggarrange(p_plot,p_table,ncol = 1, nrow = 2)

  #adds the CIs
  #geom_errorbarh(height=.1)+
  #sets the scales
  #note that I reverse the y axis to correctly order the effect #sizes based on my index variable
  #scale_x_continuous(limits=c(-1,5), breaks = c(-1:5))
  #scale_y_continuous(name = "", breaks=c(1:7), labels = data$group)
}

