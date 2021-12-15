.GetXVar <- function(vars){
  x_vars <- vars[!vars %in% c('y','z')]
  return(x_vars)
}

.FormulaXGeneration <- function(xvars){
  #browser()
  formula_string <- paste(xvars,collapse = "+")
  return(formula_string)
}

.GetFormulaOutcome <- function(outcome, treatment, xvars){
  #browser()
  formula_outcome <- as.formula(paste(outcome,"~",treatment,"+",.FormulaXGeneration(xvars),sep = ""))
  return(formula_outcome)
}

.getSubgroupSelectionExpression <- function(pattern,var_type){
  #browser()
  selection_expression <- NULL
  vars_name <- colnames(pattern)
  for (i in seq(length(vars_name))) {
    var_level <- ifelse(var_type[i]=="factor", paste("levels(",vars_name[i],")[",pattern[i],"]",sep = ""),pattern[i])
    var_name <- vars_name[i]
    if(i==1){
      selection_expression <- paste(selection_expression,var_name,"==",var_level,sep = "")
    } else{
      selection_expression <- paste(selection_expression,'&',var_name,"==",var_level,sep = "")
    }
  }
  return(selection_expression)
}


getVariableNameAndLevels4FullStudy <- function(data,stratification){
  #browser()
  var_names_levels <- list()

  # var_level should enumerate all realized values

  for (var_name in stratification){
    var_levels <- unique(data[,var_name])
    var_names_levels[[var_name]] <- var_levels
  }
  return(var_names_levels)
}

getPatternDataFrame <- function(var_names_levels){
  #
  pattern_data_frame <- initializePatternDataFrame(var_names_levels)
  pattern_data_frame <- realizationPatternDataFrame(pattern_data_frame,var_names_levels)
  #print(pattern_data_frame)
  return(pattern_data_frame)
}

initializePatternDataFrame <- function(var_names_levels){
  #
  n_patterns <- 1
  n_vars <- length(var_names_levels)
  for (i in seq(n_vars)){
    i_level <- length(var_names_levels[[i]])
    n_patterns <- n_patterns * i_level
  }
  pattern_data_frame <- data.frame(matrix(data=0,nrow=n_patterns,ncol=n_vars))
  colnames(pattern_data_frame) <- names(var_names_levels)
  #print(pattern_data_frame)
  return(pattern_data_frame)
}

realizationPatternDataFrame <- function(pattern_data_frame,var_names_levels){
  #browser()
  #n_levels <- .converFactor2Numeric(var_names_levels[,'level'])
  n_levels <- sapply(var_names_levels, length)
  n_vars <- length(var_names_levels)
  n_patterns <- dim(pattern_data_frame)[1]

  if(n_vars > 1){
    for (i in seq(n_vars-1)){
      each <- prod(n_levels[(i+1):n_vars])
      times <- n_patterns/(each*n_levels[i])
      pattern_data_frame[,i] <- rep(var_names_levels[[i]],each=each,times=times)
    }
  }
  each <- 1; times <- n_patterns/(each*n_levels[n_vars])
  pattern_data_frame[,n_vars] <- rep(var_names_levels[[n_vars]],each=1,times=times)
  return(pattern_data_frame)
}

getPatternDensityBasedOnJointDistribution <- function(data,patterns){
  #browser()
  vars <- colnames(patterns)
  n_vars <- dim(patterns)[2]
  n_obs <- nrow(data)
  data <- dplyr::select(data,vars)
  n_patterns <- dim(patterns)[1]
  density <- numeric(length = n_patterns)
  for (i in seq(n_obs)) {
    pattern.i <- data[i,]
    if(n_vars>1){
      pattern.index <- which(apply(patterns, 1, function(x) return(all(x==pattern.i))))
    } else{
      pattern.index <- which(patterns==pattern.i)
    }
    density[pattern.index] <- density[pattern.index] + 1
  }
  density <- density/n_obs
  #colnames(density) <-
  return(density)
}

FilterPatternNotInTargetAndAlignOrderOfPatternsInSourceTarget <- function(source_pattern_distribution, target_pattern_distribution){
  #browser()
  source_pattern_distribution_new <- data.frame()
  source_pattern_distribution <- as.data.frame(source_pattern_distribution)
  n_patterns_in_target <- dim(target_pattern_distribution)[1]
  vars_name_in_target <- colnames(target_pattern_distribution)[!colnames(target_pattern_distribution) %in% c("density")]
  n_vars <- length(vars_name_in_target)
  patterns_in_source <- source_pattern_distribution[,vars_name_in_target]
  for (i in 1:n_patterns_in_target) {
    pattern_i <- target_pattern_distribution[i,vars_name_in_target]
    if(n_vars>1){
      pattern.id.in.source <- which(apply(patterns_in_source, 1, function(x) return(all(x==pattern_i))))
    } else{
      pattern.id.in.source <- which(patterns_in_source==pattern_i)
    }
    if(length(pattern.id.in.source)!=0){
      source_pattern_distribution_new <- rbind(source_pattern_distribution_new,
                                               cbind(source_pattern_distribution[pattern.id.in.source,],pattern.id.in.source))
    }
  }
  #source_pattern_distribution_new <- as.data.frame(source_pattern_distribution_new)
  colnames(source_pattern_distribution_new) <- c(colnames(source_pattern_distribution),'id')
  return(source_pattern_distribution_new)
}


DensityRatio <- function(target_pattern_distribution,source_pattern_distribution,source.data){
  #browser()
  weight.value <- target_pattern_distribution$density/source_pattern_distribution$density
  n_vars <- ncol(target_pattern_distribution)-1
  vars <- colnames(target_pattern_distribution)[1:n_vars]
  patterns <- target_pattern_distribution[,1:n_vars]
  source.data <- dplyr::select(source.data,vars)
  n_source <- dim(source.data)[1]
  source.weight <- c()
  for (i in seq(n_source)) {
    pattern.i <- source.data[i,]
    if(n_vars>1){
      weight.id <- which(apply(patterns, 1, function(x) return(all(x==pattern.i))))
    } else{
      weight.id <- which(patterns==pattern.i)
    }
    # check if pattern in source.data is in target, if not, then weight is 0.
    if(length(weight.id)){
      source.weight <- c(source.weight,weight.value[weight.id])
    } else{
      source.weight <- c(source.weight,0)
    }
  }
  return(source.weight)
}


getVariableNameAndLevels4SummaryStudy <- function(univariate_p,stratification){
  #browser()
  var_names_levels <- list()
  n_covariates <- length(stratification)
  for (i in seq(n_covariates)){
    var_name <- stratification[i]
    var_id <- which(sapply(univariate_p, function(x) return(x[1]==var_name)))
    var_level <- as.numeric(univariate_p[[var_id]][2])
    var_levels <- as.numeric(univariate_p[[var_id]][3:(2+var_level)])
    var_names_levels[[var_name]] <- var_levels
  }
  return(var_names_levels)
}

getPatternDensityBasedOnUnivariateDistribution <- function(pattern,study,stratification){
  #browser()
  if(length(stratification)>1){
    var_marginal_tables <- VarMarginalTable(study$univariate_p,stratification)
    density <- Copula(pattern,var_marginal_tables)
  } else{
    # convert data.frame to vector
    pattern <- pattern[,colnames(pattern)]
    density <- VarMarginalTable(study$univariate_p,stratification)[[1]]
    density <- density[match(pattern,density[,1]),'density']
  }
  return(density)
}


# Return: list(tables)
# tables[,1]=a level of a covariate
# tables[,2]=density of the level of the covariate
VarMarginalTable <- function(univariate_p,stratification){
  #browser()
  n_vars <- length(stratification)
  tables <- list()
  for (i in seq(n_vars)){
    var_name <- stratification[i]
    var_id <- which(sapply(univariate_p, function(x) return(x[1]==var_name)))
    var_inf_i <- univariate_p[[var_id]]
    i_level <- as.numeric(var_inf_i[2])
    i_table <- data.frame(matrix(NA,nrow = i_level,ncol = 2))
    for (j in seq(i_level)){
      i_table[j,1] <- as.numeric(var_inf_i[2+j])
      i_table[j,2] <- as.numeric(var_inf_i[2+i_level+j])
    }
    colnames(i_table) <- c(var_name,'density')
    tables[[var_name]] <- i_table
  }
  #print(tables)
  return(tables)
}



Copula <- function(patterns,var_marginal_tables){
  #
  #args <- list(...)
  #n_args <- length(args)
  #if(n_args > 0) {
  #  vars <- colnames(pattern_density)[1:n_vars]
  #  Sigma <- CovarianceMatrix(Sigma,args,vars)
  #}
  #
  n_vars <- length(var_marginal_tables)
  Sigma <- diag(x=1,nrow=n_vars,ncol=n_vars)
  mu <- rep(0, n_vars)
  #print(Sigma)

  n_patterns <- dim(patterns)[1]
  density <- rep(0,n_patterns)



  for (i in seq(n_patterns)){
    # select data.frame rows, then the resulting data class is still data.frame
    # however, select data.frame columns, then the resulting data class is numeric..
    pattern <- patterns[i,1:n_vars]
    lower_upper <- LowerUpperNormalCopula(pattern,var_marginal_tables)
    lower <- lower_upper[[1]]
    upper <- lower_upper[[2]]
    density[i] <- mvtnorm::pmvnorm(lower=lower, upper=upper, mean = mu,corr = Sigma)
  }
  return(density)
}


LowerUpperNormalCopula <- function(pattern,var_marginal_tables){
  lower <- upper <- NULL
  #
  for(i in seq(length(pattern))){
    #
    var_name <- colnames(pattern[i])
    var_level <- as.numeric(pattern[i])
    var_level_margin <- var_marginal_tables[[var_name]]
    min_level <- min(var_level_margin[,1])
    max_level <- max(var_level_margin[,1])
    if(var_level==min_level){
      lower <- c(lower,-Inf)
      inver.margin.upper <- qnorm(var_level_margin[var_level_margin[,1]==var_level,'density'])
      upper <- c(upper,inver.margin.upper)
    } else if (var_level==max_level){
      inver.margin.lower <- qnorm(var_level_margin[var_level_margin[,1]==(var_level-1),'density'])
      lower <- c(lower,inver.margin.lower)
      upper <- c(upper,Inf)
    } else{
      inver.margin.lower <- qnorm(var_level_margin[var_level_margin[,1]==(var_level-1),'density'])
      inver.margin.upper <- qnorm(var_level_margin[var_level_margin[,1]==var_level,'density'])
      lower <- c(lower,inver.margin.lower)
      upper <- c(upper,inver.margin.upper)
    }
  }
  return(list(lower,upper))
}


dots <- function(name,value,...){
  args <- list(...)
  names <- names(args)
  if(name %in% names){
    return(args[[name]])
  } else {
    return(value)
  }
}

is.exist <- function(name,...){
  args <- list(...)
  names <- names(args)
  if(name %in% names){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

get.args <- function(name,...){
  args <- list(...)
  return(args[[name]])
}


AlignOrderofPatternInCATE <- function(df1,df2){
  nvars <- sum(!colnames(df1) %in% c("cate","se","size"))
  df2.new <- df2
  for(i in 1:nrow(df1)){
    df2.new[i,1:nvars] <- df1[i,1:nvars]
    subgroup.id.in.df2 <- which(apply(df2[,1:nvars], 1, function(x) return(all(x==df2.new[i,1:nvars]))))
    df2.new[i,'cate'] <- df2[subgroup.id.in.df2,'cate']
    df2.new[i,'se'] <- df2[subgroup.id.in.df2,'se']
    df2.new[i,'size'] <- df2[subgroup.id.in.df2,'size']
  }
  return(df2.new)
}


# PatternDistributionDataFrame:
# data.frame: "x1", "x2", ..., "xk", "density"
#               0    0    ...    0    0.08
#               0    0    ...    1    0.05
#               ...
#               1    1    ...    1    0.01
# nrow = number of total patterns given x
generateSyntheticData <- function(N,PatternDistributionDataFrame_Target){
  syntheticdata <- c()
  nvars <- dim(PatternDistributionDataFrame_Target)[2]-1
  npatterns <- dim(PatternDistributionDataFrame_Target)[1]
  for (i in 1:npatterns) {
    Ni <- floor(PatternDistributionDataFrame_Target[i,'density']* N)
    data.t.sub.i <- data.frame(matrix(as.numeric(rep(PatternDistributionDataFrame_Target[i,1:nvars],each = Ni)),nrow = Ni,byrow = FALSE))
    syntheticdata <- rbind(syntheticdata,data.t.sub.i)
  }

  syntheticdata <- as.data.frame(syntheticdata)
  colnames(syntheticdata) <- c(colnames(PatternDistributionDataFrame_Source)[1:nvars])
  return(syntheticdata)
}


.GetFormulaSelectionScore <- function(xvars){
  formula_selection <- as.formula(paste('selection~',.FormulaXGeneration(xvars),sep = ""))
  return(formula_selection)
}

