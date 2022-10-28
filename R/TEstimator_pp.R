TEstimator_pp <- R6::R6Class(
  "TEstimator_pp",
  inherit = TEstimator,
  public = list(
    initialize = function(obj){
      #browser()
      self$id = obj$id
      self$name = obj$name
      self$statistics = obj$statistics
      self$estimates = obj$estimates
      self$model = obj$model
      self$data = self$estimates$CATE
      #private$data =
      self$data$id = seq(dim(self$data)[1])
      private$confounders_treatment_name = obj$.__enclos_env__$private$confounders_treatment_name
      private$treatment_name = obj$.__enclos_env__$private$treatment_name
      private$outcome_name = obj$.__enclos_env__$private$outcome_name
      private$diagnosis_t_ignorability_inherent = obj$diagnosis_t_ignorability(private$confounders_treatment_name)
      private$isTrial = obj$.__enclos_env__$private$isTrial
      # IPW can't use aggregate since it has non-overlap group, need some data cleaning
    },

    diagnosis_t_ignorability = function(){
      private$diagnosis_t_ignorability_inherent
    },

    diagnosis_t_overlap = function(stratification=private$confounders_treatment_name, stratification_joint=TRUE){
      #browser()
      vars_name <- stratification
      # when use group_by(across(all_of(...))), ... should be a vector and
      # each element should be a character/string
      if(isTRUE(stratification_joint)){
        # position=fill: a percent stacked barplot
        p.prop <- self$statistics$density_confounders %>%
          #select(vars_name, private$treatment_name,count) %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ","))) %>%
          group_by(across(all_of(c("group_name", private$treatment_name)))) %>%
          summarise(prop=sum(count)/self$statistics$n) %>%
          ggplot(aes(x=group_name,y=prop,fill=factor(eval(parse(text = private$treatment_name))))) +
          geom_bar(position = "fill",stat = "identity") +
          ylab("proportion") +
          labs(fill = "treatment") +
          coord_flip() +
          theme(legend.position="none")

        p.count <-  self$statistics$density_confounders %>%
          #select(vars_name, private$treatment_name,count) %>%
          mutate(group_name = apply(.[,vars_name], 1, function(x)
            paste(vars_name,x,sep = "=",collapse = ","))) %>%
          group_by(across(all_of(c("group_name", private$treatment_name)))) %>%
          summarise(count=sum(count)) %>%
          ggplot(aes(x=group_name, y=count, fill=factor(eval(parse(text = private$treatment_name))))) +
          geom_bar(stat = "identity") +
          labs(fill = "treatment") +
          coord_flip() +
          theme(axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.y=element_blank())
      } else {
        # apply: X must be matrix or array
        # data then add mutate then group and summarise, can't fist group, summarise and mutate...
        df <- lapply(stratification, function(x) self$statistics$density_confounders %>%
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

      ggarrange(p.prop, p.count, ncol=2, nrow=1)
    },

    diagnosis_y_overlap = function(stratification, stratification_joint){
      #browser()

      message("Sorry, we only support binary outcome for now.")
      if(missing(stratification)){
        vars_name <- private$confounders_treatment_name
      } else{
        vars_name <- stratification
      }

      self$statistics$density_confounders %>%
        mutate(group_name = apply(.[,vars_name], 1, function(x)
          paste(vars_name,x,sep = "=",collapse = ","))) %>%
        group_by(across(all_of(c("group_name", private$outcome_name)))) %>%
        print() %>%
        ggplot(aes(x=group_name, y=count, fill=factor(eval(parse(text = private$outcome_name))))) +
        geom_bar(stat = "count") +
        labs(fill = "outcome") +
        ggtitle("Outcome overlap within subpopulations")+
        coord_flip() +
        facet_wrap(~eval(parse(text=private$treatment_name))) +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.y=element_blank())

      # p.count.t1 <- self$statistics$density_confounders %>%
      #   filter(eval(parse(text=private$treatment_name)) == "1") %>%
      #   mutate(group_name = apply(.[,vars_name], 1, function(x)
      #     paste(vars_name,x,sep = "=",collapse = ","))) %>%
      #   group_by(across(all_of(c("group_name", private$outcome_name)))) %>%
      #   summarise(count=sum(count)) %>%
      #   ggplot(aes(x=group_name, y=count, fill=factor(eval(parse(text = private$outcome_name))))) +
      #   geom_bar(stat = "identity") +
      #   labs(fill = "outcome") +
      #   ggtitle("survival count in treatment group")+
      #   coord_flip() +
      #   theme(legend.position="none")
      #
      # p.count.t0 <- self$statistics$density_confounders %>%
      #   filter(eval(parse(text=private$treatment_name)) == "0") %>%
      #   mutate(group_name = apply(.[,vars_name], 1, function(x)
      #     paste(vars_name,x,sep = "=",collapse = ","))) %>%
      #   group_by(across(all_of(c("group_name", private$outcome_name)))) %>%
      #   summarise(count=sum(count)) %>%
      #   ggplot(aes(x=group_name, y=count, fill=factor(eval(parse(text = private$outcome_name))))) +
      #   geom_bar(stat = "identity") +
      #   labs(fill = "outcome") +
      #   ggtitle("survival count in control group")+
      #   coord_flip() +
      #   theme(axis.text.y=element_blank(),
      #         axis.ticks.y=element_blank(),
      #         axis.title.y=element_blank())
      #
      # ggarrange(p.count.t1, p.count.t0, ncol=2, nrow=1)

    }

  ),

  private = list(
    confounders_treatment_name = NA,
    diagnosis_t_ignorability_inherent = list(),

    est_ATE_SE = function(index) {
      #browser()
      y1.hat <- rep(self$data$y1.hat[index],self$data$size[index])
      y0.hat <- rep(self$data$y0.hat[index],self$data$size[index])
      data <- as.data.frame(cbind(y1.hat,y0.hat))
      colnames(data) <- c("y1.hat","y0.hat")
      results <- m_estimate(estFUN = private$mean_estfun,
                            data = data,
                            root_control = setup_root_control(start = c(0,0,0)))
      y1.hat.mu <- results@estimates[1]
      y0.hat.mu <- results@estimates[2]
      est <- results@estimates[3]
      se <- sqrt(results@vcov[3,3])
      #
      # weight.norm <- self$data[index,"size"]/sum(self$data[index,"size"])
      # y1.hat <- sum(self$data$y1.hat[index]*weight.norm)
      # y0.hat <- sum(self$data$y0.hat[index]*weight.norm)
      # est <- y1.hat - y0.hat
      # var.ate <- sum(self$data$se[index] * (weight.norm)^2)/sum(weight.norm^2)
      # se <- sqrt(var.ate)
      return(list(y1.hat = y1.hat.mu, y0.hat = y0.hat.mu, est = est, se = se))
    },

    est_weighted_ATE_SE = function(index, weight) {
      #browser()
      y1.hat <- rep(self$data$y1.hat[index],self$data$size[index])
      y0.hat <- rep(self$data$y0.hat[index],self$data$size[index])
      weight <- rep(weight,self$data$size[index])
      data <- as.data.frame(cbind(y1.hat,y0.hat,weight))
      colnames(data) <- c("y1.hat","y0.hat","weight")
      results <- m_estimate(estFUN = private$wmean_estfun,
                            data = data,
                            root_control = setup_root_control(start = c(0,0,0)))
      y1.hat.mu <- results@estimates[1]
      y0.hat.mu <- results@estimates[2]
      est <- results@estimates[3]
      se <- sqrt(results@vcov[3,3])

      #browser()
      # standard error in this approach is more conservative, weight is the same as above approach
      # y1.hat <- rep(self$data$y1.hat[index],self$data$size[index])
      # y0.hat <- rep(self$data$y0.hat[index],self$data$size[index])
      # weight <- rep(weight,self$data$size[index])
      # cate.se <- rep(self$data$se[index],self$data$size[index])
      # y1.hat.mu <- sum(y1.hat*weight)/sum(weight)
      # y0.hat.mu <- sum(y0.hat*weight)/sum(weight)
      # est <- y1.hat.mu - y0.hat.mu
      # var.ate <- sum(cate.se^2 * (weight^2) / (sum(weight))^2)
      # se <- sqrt(var.ate)

      # standard error of this appraoch is more conservative, in this case, weight=weight/sum(weight)
      # y1.hat <- self$data$y1.hat[index]
      # y0.hat <- self$data$y0.hat[index]
      # cate.se <- self$data$se[index]
      # y1.hat.mu <- sum(y1.hat*weight)
      # y0.hat.mu <- sum(y0.hat*weight)
      # est <- y1.hat.mu - y0.hat.mu
      # var.ate <- sum(cate.se^2 * (weight^2))
      # se <- sqrt(var.ate)

      return(list(y1.hat = y1.hat.mu, y0.hat = y0.hat.mu, est = est, se = se))
    },

    wmean_estfun = function(data){
      Y1 <- data$y1.hat
      Y0 <- data$y0.hat
      W <-  data$weight
      function(theta) {
        c(Y1*W - theta[1],
          Y0*W - theta[2],
          theta[1]-theta[2] - theta[3]
        )
      }
    },

    mean_estfun = function(data){
      Y1 <- data$y1.hat
      Y0 <- data$y0.hat
      function(theta) {
        c(Y1 - theta[1],
          Y0 - theta[2],
          theta[1]-theta[2] - theta[3]
        )
      }
    }

  )
)
