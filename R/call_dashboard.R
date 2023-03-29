#' @title Visualizing validation results according to four steps, namely, set-selection, estimation, diagnosis, and validation
#' @param source.obj an instantiated object of class \code{TEstimator}. The estimates of conditional average treatment effects are compared to those from \code{target.obj}.
#' @param target.obj an instantiated object of class \code{TEstimator}. The estimates of conditional average treatment effects are regarded as unbiased of truth.
#' @param source.obj.rep an instantiated object of class \code{SEstimator}. The estimates of conditional average treatment effects are compared to those from \code{target.obj}.
#' @returns an interactive interface visualizing results of four steps
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
#' call_dashboard(source.obj = source.obj,
#'                target.obj = target.obj,
#'                source.obj.rep = source.obj.rep)
#'
#' }
#' @export
#' @import shiny
#' @import shinydashboard
call_dashboard <- function(source.obj = NULL,
                           target.obj = NULL,
                           source.obj.rep = NULL) {
  shinyApp(
    ui <- dashboardPage(
      title = "ShinyAB",
      dashboardHeader(title = "Validation of estimators for conditional average treatment effect using observational data and RCT data",titleWidth=1000),
      dashboardSidebar(disable =TRUE),
      dashboardBody(
        fluidRow(
          box(
            title = "Identification", width = 12,solidHeader = T, status = "primary",
            fluidRow(
              column(width = 4, box(title = "Confounders X",
                                    solidHeader = T, status = "info",
                                    checkboxGroupInput(
                                      inputId = "diagnosis_t_stratification",
                                      label = "Confounders",
                                      inline = FALSE,
                                      width = NULL
                                    ),
                                    radioButtons(inputId = "diagnosis_t_stratification_joint",
                                                 label = "Stratification levels",
                                                 choices = c("Combination"=TRUE,
                                                             "Individual"=FALSE),
                                                 selected = TRUE),
                                    actionButton(inputId = "diagnosis_t_assumptions_go",label = "Go"))),
              column(width = 4, box(title = "Effect modifiers Xs",
                                    solidHeader = T, status = "info",
                                    checkboxGroupInput(
                                      inputId = "diagnosis_s_stratification",
                                      label = "Effect modifiers",
                                      inline = FALSE,
                                      width = NULL
                                    ),
                                    radioButtons(inputId = "diagnosis_s_stratification_joint",
                                                 label = "Stratification levels",
                                                 choices = c("Combination"=TRUE,
                                                             "Individual"=FALSE),
                                                 selected = TRUE),
                                    actionButton(inputId = "diagnosis_s_assumptions_go",label = "Go"))),
              column(width = 4, box(title = "Stratification Xh",
                                    solidHeader = T, status = "info",
                                    checkboxGroupInput(
                                      inputId = "validation_stratification",
                                      label = "Stratification by",
                                      inline = FALSE,
                                      width = NULL
                                    ),
                                    radioButtons(inputId = "validation_stratification_joint",
                                                 label = "Stratification levels",
                                                 choices = c("Combination"=TRUE,
                                                             "Individual"=FALSE),
                                                 selected = TRUE),
                                    actionButton(inputId = "validation_estimation_stratification_go", label = "Go"))))
          )
        ),
        fluidRow(
          box(
            title = "Estimation", width = 12,solidHeader = T, status = "primary",
            fluidRow(
              box(width = 4, plotOutput(outputId = "estimation_CATE_plot")),
              box(width = 4, plotOutput(outputId = "estimation_y1_y0_plot")),
              box(width = 4, tableOutput(outputId = "CATE_numeric"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Diagnosis", width = 12, solidHeader = T, status = "primary",
            fluidRow(
              box(width = 8,
                  fluidRow(
                    column(
                      width = 12,
                      fluidRow(box(title = "T-overlap",
                                   width = 12,
                                   status="info",
                                   solidHeader = T,
                                   plotOutput(outputId = "diagnosis_t_overlap_plot"))),
                      fluidRow(box(title = "T-ignorability",
                                   width = 12,
                                   status="info",
                                   solidHeader = T,
                                   plotOutput(outputId = "diagnosis_t_ignorability_plot"))))
                  )),
              box(width = 4,
                  fluidRow(
                    column(
                      width = 12,
                      fluidRow(box(title = "S-overlap",
                                   width = 12,
                                   status="info",
                                   solidHeader = T,
                                   plotOutput(outputId = "diagnosis_s_overlap_plot"))),
                      fluidRow(box(title = "S-ignorability",
                                   width = 12,
                                   status="info",
                                   solidHeader = T,
                                   plotOutput(outputId = "diagnosis_s_ignorability_plot"))))
                  ))
            )
          )
        ),
        fluidRow(
          box(
            title = "Validation", id='valiation', width = 12, solidHeader = T, status = "primary",
            fluidRow(
              column(width = 6, height = "33%",plotOutput(outputId = "vadidation_plot")),
              column(width = 6, height = "33%",tableOutput(outputId = "validation_numeric"))
            )
          )
        )
      )
    ),

    server <- function(input, output,session) {
      plot_estimation_CATE_plot <- eventReactive(
        input$validation_estimation_stratification_go,{
          source.obj$plot_CATE(stratification = c(input$validation_stratification),
                               input$validation_stratification_joint)
        }
      )

      plot_estimation_y1_y0_plot <- eventReactive(
        input$validation_estimation_stratification_go,{
          source.obj$plot_y1_y0(stratification = c(input$validation_stratification),
                                input$validation_stratification_joint)
        }
      )

      print_CATE_numeric <- eventReactive(
        input$validation_estimation_stratification_go,{
          source.obj$get_CATE(stratification = c(input$validation_stratification),
                              input$validation_stratification_joint)
        }
      )

      plot_diagnosis_t_overlap <- eventReactive(
        input$diagnosis_t_assumptions_go,
        {source.obj$diagnosis_t_overlap(stratification = c(input$diagnosis_t_stratification),
                                        stratification_joint = TRUE)}
      )

      plot_diagnosis_t_ignorability <- eventReactive(
        input$diagnosis_t_assumptions_go, {
          source.obj$diagnosis_t_ignorability(stratification = c(input$diagnosis_t_stratification),
                                              input$diagnosis_t_stratification_joint)
        }
      )

      plot_diagnosis_s_overlap <- eventReactive(
        input$diagnosis_s_assumptions_go,{
          source.obj.rep$diagnosis_s_overlap(stratification = c(input$diagnosis_s_stratification),
                                             input$diagnosis_s_stratification_joint)
        }
      )

      plot_diagnosis_s_ignorability <- eventReactive(
        input$diagnosis_s_assumptions_go,{
          source.obj.rep$diagnosis_s_ignorability(stratification = c(input$diagnosis_s_stratification),
                                                  input$diagnosis_s_stratification_joint)
        }
      )

      Aggregate_objects <- eventReactive(
        input$validation_estimation_stratification_go,{
          source.obj.rep$EstimateRep(stratification = c(input$validation_stratification),
                                     stratification_joint = input$validation_stratification_joint)
          #print("generate one time")
          #fusion only generated one time for one click "go"
          fusion <- RCTrep::Fusion$new(target.obj,
                                       source.obj,
                                       source.obj.rep)
        })


      plot_fusion <- eventReactive(
        input$validation_estimation_stratification_go,{
          fusion <- Aggregate_objects()
          fusion$plot()
        })

      print_fusion <- eventReactive(
        input$validation_estimation_stratification_go,{
          fusion <- Aggregate_objects()
          fusion$evaluate()
        })

      output$estimation_CATE_plot <- renderPlot({
        plot_estimation_CATE_plot()
      })

      output$estimation_y1_y0_plot <- renderPlot({
        plot_estimation_y1_y0_plot()
      })

      output$CATE_numeric <- renderTable({
        print_CATE_numeric()
      })

      output$diagnosis_t_overlap_plot <- renderPlot({
        plot_diagnosis_t_overlap()
      })

      output$diagnosis_t_ignorability_plot <- renderPlot({
        plot_diagnosis_t_ignorability()
      })

      output$diagnosis_s_overlap_plot <- renderPlot({
        plot_diagnosis_s_overlap()
      })

      output$diagnosis_s_ignorability_plot <- renderPlot({
        plot_diagnosis_s_ignorability()
      })

      output$vadidation_plot <- renderPlot({
        plot_fusion()
      })

      output$validation_numeric <- renderTable({
        print_fusion()
      })

      updateCheckboxGroupInput(
        session, "diagnosis_t_stratification",
        choices = source.obj$.__enclos_env__$private$confounders_treatment_name,
        selected = source.obj$.__enclos_env__$private$confounders_treatment_name
      )

      updateCheckboxGroupInput(
        session, "diagnosis_s_stratification",
        choices = source.obj$.__enclos_env__$private$confounders_treatment_name,
        selected = source.obj.rep$confounders_sampling_name
      )

      updateCheckboxGroupInput(
        session, "validation_stratification",
        choices = source.obj$.__enclos_env__$private$confounders_treatment_name,
        selected = NULL
      )
    }
  )

}
