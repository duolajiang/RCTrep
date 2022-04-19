JSONtoEstimatorAdaptor <- R6::R6Class(
  "JSONtoEstimatorAdaptor",
  inherit = SummaryDecorater,
  public = list(
    initialize = function(obj){
      self$name = fromJSON(obj$name)
      self$statistics = fromJSON(obj$statistics)
      self$estimates = fromJSON(obj$estimates)
      self$data = fromJSON(self$estimates$CATE)
      self$data$id = seq(dim(self$data)[1])
      private$confounders_name = colnames(self$data)[!colnames(self$data) %in% c("y1.hat","y0.hat","cate","se","size","name","value","id")]
    }
  ),
  private = list(
  )
)
