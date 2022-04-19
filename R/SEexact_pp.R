#' @export
SEexact_pp <- R6::R6Class(
  "SEexact_pp",
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
