#' @export
SEstimator_pp <- R6::R6Class(
  "SEstimator_pp",
  inherit = SEstimator,
  public = list(
    weighting_estimator = "Exact",

    initialize = function(target.obj, source.obj,
                          weighting_method=NULL,
                          confounders_sampling_name){
      super$initialize(target.obj, source.obj, weighting_method,confounders_sampling_name)
    },

    diagnosis_s_overlap = function(stratification=NULL, stratification_joint=TRUE){
      #browser()
      if(missing(stratification)){
        vars_name <- self$confounders_sampling_name
      } else{
        vars_name <- stratification
      }

      if(stratification_joint){
        source.data <- private$source.obj$statistics$density_confounders %>%
          select(vars_name,count) %>%
          mutate(study=private$source.obj$name)
        target.data <- private$target.obj$statistics$density_confounders %>%
          select(vars_name,count) %>%
          mutate(study=private$target.obj$name)

        data <- bind_rows(source.data, target.data) %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ","))) %>%
          group_by(study, group_name) %>%
          summarise(count = sum(count))

        p.prop <- ggplot(data = data, aes(x=group_name, y=count, fill=study)) +
          geom_bar(stat =  "identity", position = "fill") +
          xlab("proportion") +
          coord_flip() +
          theme(legend.position="none")

        p.count <- ggplot(data = data, aes(x=group_name, y=count, fill=study)) +
          geom_bar(stat = "identity") +
          xlab("count") +
          coord_flip() +
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.y=element_blank())
      } else{

      }

      ggpubr::ggarrange(p.prop, p.count, nrow=1, ncol=2)

    },


    diagnosis_s_ignorability = function(stratification=NULL, stratification_joint=TRUE){
      #browser()
      if(missing(stratification)){
        vars_name <- self$confounders_sampling_name
      } else{
        vars_name <- stratification
      }

      message("to be continued... This function is to check if
              confounders_sampling are balanced between source and target object
              on population and sub-population levels stratified by
              stratificaiton and stratification_joint.")

      weight <- private$get_weight(
        source = private$source.obj$data,
        target = private$target.obj$data,
        vars_weighting = self$confounders_sampling_name
      )

      p.source <- private$source.obj$data %>%
        bind_cols(weight = weight) %>%
        group_by(across(all_of(self$confounders_sampling_name))) %>%
        summarise(size.agg=sum(weight*size)) %>%
        ungroup() %>%
        mutate(prop=size.agg/sum(size.agg),
               study=private$source.obj$name)

      p.target <- private$target.obj$data %>%
        group_by(across(all_of(self$confounders_sampling_name))) %>%
        summarise(size.agg=sum(size)) %>%
        ungroup() %>%
        mutate(prop=size.agg/sum(size.agg),
               study=private$target.obj$name)

      p.combined <- rbind(p.source, p.target) %>%
        mutate(group_name = apply(.[,vars_name], 1, function(x)
          paste(vars_name,x,sep = "=",collapse = ","))) %>%
        ggplot(aes(x=group_name, y=prop, fill=study)) +
        geom_bar(stat='identity', position='dodge') +
        ylab("proportion") +
        coord_flip() +
        theme(legend.position="none")

      p.combined

    }

  ),
  private = list(

    get_weight = function(source, target, vars_weighting){
      #browser()

      # operations are performed by group
      # hence to compute the distribution of groups, we should first ungroup()

      source.groups <- source %>%
        select(c(vars_weighting,"size")) %>%
        group_by(across(all_of(vars_weighting)))

      target.groups <- target %>%
        select(c(vars_weighting,"size")) %>%
        group_by(across(all_of(vars_weighting)))

      source.sum <- source.groups %>%
        summarise(size.agg=sum(size)) %>%
        ungroup() %>%
        mutate(prop=size.agg/sum(size.agg))

      target.sum <- target.groups %>%
        summarise(size.agg=sum(size)) %>%
        ungroup() %>%
        mutate(prop=size.agg/sum(size.agg))

      weight <- round(target.sum$prop/source.sum$prop,digits = 7)
      weight <- weight[source.groups %>% group_indices()]

      nss <- sum(source[weight>0,"size"])
      # in this way, sum(weight) == number of observations in source data
      weight <- weight * nss/ sum(weight*source[weight>0,"size"])
      #weight <- weight/sum(weight)

      return(weight)
    }

  )

)
