server <- function(input, output,session) {
  output$CATE_plot <- renderPlot({
    source.obj$plot_CATE(stratification = c(input$cate_stratification), input$cate_stratification_joint)
    })

  output$CATE_numeric <- renderTable({
    source.obj$get_CATE(stratification = c(input$cate_stratification), input$cate_stratification_joint)
  })

  output$diagnosis_t_overlap_plot <- renderPlot({
     source.obj$diagnosis_t_overlap(stratification = c(input$diagnosis_t_stratification),
                                    stratification_joint = TRUE)
   })

  output$diagnosis_t_ignorability_plot <- renderPlot({
    source.obj$diagnosis_t_ignorability(stratification = c(input$diagnosis_t_stratification),
                                        input$diagnosis_t_stratification_joint)
  })

  output$diagnosis_s_overlap_plot <- renderPlot({
    source.obj.rep$diagnosis_s_overlap()
  })

  output$diagnosis_s_ignorability_plot <- renderPlot({
    source.obj.rep$diagnosis_s_ignorability()
  })


  fusionInput <- reactive({
    source.obj.rep$EstimateRep(stratification = c(input$validation_stratification),
                               stratification_joint = input$validation_stratification_joint)
    fusion <- RCTrep::Fusion$new(target.obj,
                                 source.obj,
                                 source.obj.rep)
    return(fusion)
  })

  output$vadidation_plot <- renderPlot({
    fusion <- fusionInput()
    fusion$plot()
    })

  output$validation_numeric <- renderTable({
    fusion <- fusionInput()
    fusion$evaluate()
  })
}
