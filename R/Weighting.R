setGeneric(name = "setWeight",
           def = function(source,target,weighting_estimator,weighting_method,vars_weighting) standardGeneric("setWeight"))

setMethod(f = "setWeight",
          signature = c(source="data.frame", target="data.frame",
                        weighting_estimator="character",weighting_method="character",vars_weighting="character"),
          definition = function(source, target, weighting_estimator, weighting_method, vars_weighting){
            #browser()
            if(weighting_estimator=="Balancing"){
              source$selection <- 0
              target$selection <- 1
              matching_formula <- formula(paste("selection~",paste(vars_weighting,collapse = "+")))
              # default for ATT, means weight for selected 1.
              matchit.obj <- MatchIt::matchit(matching_formula, method = "exact",data = rbind(source,target))
              weight <- matchit.obj$weights[1:dim(source)[1]]
            } else {
              if(is.null(weighting_method)) weighting_method <- "glm"
              samplingscore <- SelectionScoreModeling(source,target,vars_weighting,weighting_method)
              weight <- samplingscore/(1-samplingscore)
            }
            return(weight)
          })

setMethod(f = "setWeight",
          signature = c(source="data.frame", target="list",
                        weighting_estimator="character",weighting_method="character",vars_weighting="character"),
          definition = function(source, target, weighting_estimator, weighting_method,vars_weighting){
            #browser()
            if(weighting_method=="Balancing"){
              target_pattern_distribution <- PatternDistribution(target,vars_weighting)
              source_pattern <- PatternDistribution(source,vars_weighting)
              source_pattern_id4each_obs <- source_pattern[[1]]
              source_pattern_distribution <- source_pattern[[2]]
              source_pattern_distribution <- FilterPatternNotInTargetAndAlignOrderOfPatternsInSourceTarget(
              source_pattern_distribution,target_pattern_distribution)
              weight4pattern <- data.frame(id = source_pattern_distribution$id,
                                           weight = target_pattern_distribution$density/source_pattern_distribution$density)
              weight <- sapply(source_pattern_id4each_obs, function(x) ifelse(x %in% weight4pattern$id,
                                                                              weight4pattern[weight4pattern$id==x,"weight"],0))
            } else {
              if(is.null(weighting_method)) weighting_method <- "glm"
              samplingscore <- SelectionScoreModeling(source,target,vars_weighting,weighting_method)
              weight <- 1/samplingscore
            }
            return(weight)
          })


PatternDistribution <- function(data,vars_weighting){
  #browser()
  if(class(data)=="data.frame"){
    pattern_data <- data %>%
                    group_by(across(all_of(vars_weighting)))
    pattern_id_4each_obs <- pattern_data %>% group_indices()
    pattern_density <- pattern_data %>% summarise(n=n())
    pattern_density$density <- pattern_density$n/sum(pattern_density$n)
    pattern_density <- dplyr::select(pattern_density,-n)
    return(list(pattern_id_4each_obs,pattern_density))
  } else {
    var_names_levels <- getVariableNameAndLevels4SummaryStudy(data$univariate_p,vars_weighting)
    pattern <- getPatternDataFrame(var_names_levels)
    density <- getPatternDensityBasedOnUnivariateDistribution(pattern,data,vars_weighting)
    pattern_density <- cbind(pattern,density)
    return(pattern_density)
  }
}

SelectionScoreModeling <- function(source, target, vars_weighting,weighting_method){
  #browser()
  if(!is.data.frame(target)){
    target_pattern_distribution <- PatternDistribution(target,vars_weighting)
    source_pattern <- PatternDistribution(source,vars_weighting)
    source_pattern_id4each_obs <- source_pattern[[1]]
    source_pattern_distribution <- source_pattern[[2]]
    source_pattern_distribution <- FilterPatternNotInTargetAndAlignOrderOfPatternsInSourceTarget(
      source_pattern_distribution,target_pattern_distribution)
    N <- dim(source)[1]
    target <- generateSyntheticData(N,source_pattern_distribution, target_pattern_distribution)
  }
  source$selection <- 0
  target$selection <- 1
  data <- rbind(source,target)
  data$selection <- as.factor(data$selection)

  model <- caret::train(x=data[,vars_weighting],
                        y=data$selection,
                        method = weighting_method)

  selection_score <- predict(model,type="prob")[data$selection==0,c("1")]

  return(selection_score)
}
