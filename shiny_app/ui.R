ui <- dashboardPage(
  title = "ShinyAB",
  dashboardHeader(title = "Validation of estimators for conditional average treatment effect using observational data and RCT data",titleWidth=1000),
  dashboardSidebar(disable =TRUE),
  dashboardBody(
    fluidRow(
      box(
        title = "Identification", width = 12,solidHeader = T, status = "primary",
        fluidRow(
          column(width = 4, box(title = "Confounders Xc",
                                solidHeader = T, status = "info",
                                checkboxGroupInput(
                                  inputId = "diagnosis_t_stratification",
                                  label = "Confounders",
                                  choices= c("x1" = "x1","x2" = "x2","x3" = "x3",
                                             "x4" = "x4","x5" = "x5","x6" = "x6"),
                                  selected = c("x1","x2","x3","x4","x5","x6"),
                                  inline = FALSE,
                                  width = NULL
                                  ),
                                radioButtons(inputId = "diagnosis_t_stratification_joint",
                                             label = "Stratification levels",
                                             choices = c("Combination"=TRUE,
                                                         "Individual"=FALSE),
                                             selected = TRUE))),
          column(width = 4, box(title = "Effect modifiers Xs",
                                solidHeader = T, status = "info",
                                checkboxGroupInput(
                                  inputId = "diagnosis_s_stratification",
                                  label = "Effect modifiers",
                                  choices= c("x1" = "x1","x2" = "x2","x3" = "x3",
                                             "x4" = "x4","x5" = "x5","x6" = "x6"),
                                  selected = c("x2","x6"),
                                  inline = FALSE,
                                  width = NULL
                                ),
                                radioButtons(inputId = "diagnosis_s_stratification_joint",
                                             label = "Stratification levels",
                                             choices = c("Combination"=TRUE,
                                                         "Individual"=FALSE),
                                             selected = TRUE))),
          column(width = 4, box(title = "Stratification Xh",
                                solidHeader = T, status = "info",
                                checkboxGroupInput(
                                  inputId = "validation_stratification",
                                  label = "Stratification by",
                                  choices= c("x1" = "x1","x2" = "x2","x3" = "x3",
                                             "x4" = "x4","x5" = "x5","x6" = "x6"),
                                  selected = c("x1","x3","x4","x5"),
                                  inline = FALSE,
                                  width = NULL
                                ),
                                radioButtons(inputId = "validation_stratification_joint",
                                             label = "Stratification levels",
                                             choices = c("Combination"=TRUE,
                                                         "Individual"=FALSE),
                                             selected = TRUE))))
        )
      ),
    fluidRow(
      box(
        title = "Estimation", width = 12,solidHeader = T, status = "primary",
        fluidRow(
        #   box(width = 1,
        #       #height = 200,
        #          checkboxGroupInput(
        #            inputId = "cate_stratification",
        #            label = "Stratifiation by",
        #            choices= c("x1" = "x1","x2" = "x2","x3" = "x3",
        #                       "x4" = "x4","x5" = "x5","x6" = "x6"),
        #            selected = c("x1","x3","x4","x5"),
        #            inline = FALSE,
        #            width = NULL
        #          ),
        #          radioButtons(inputId = "cate_stratification_joint",
        #                       label = "Stratification levels",
        #                       choices = c("Combination"=TRUE,
        #                       "Individual"=FALSE),
        #                       selected = TRUE)
        # ),
        box(width = 4, plotOutput(outputId = "CATE_plot")),
        box(width = 4, plotOutput(outputId = "y1_y0_plot")),
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
              # column(width = 2,
              #        checkboxGroupInput(
              #          inputId = "diagnosis_t_stratification",
              #          label = "Confounders",
              #          choices= c("x1" = "x1","x2" = "x2","x3" = "x3",
              #                     "x4" = "x4","x5" = "x5","x6" = "x6"),
              #          selected = c("x1","x2","x3","x4","x5","x6"),
              #          inline = FALSE,
              #          width = NULL
              #        ),
              #        radioButtons(inputId = "diagnosis_t_stratification_joint",
              #                     label = "Stratification levels",
              #                     choices = c("Combination"=TRUE,
              #                                 "Individual"=FALSE),
              #                     selected = TRUE)),
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
              # column(width = 2,
              #        checkboxGroupInput(
              #          inputId = "diagnosis_s_stratification",
              #          label = "Effect modifiers",
              #          choices= c("x1" = "x1","x2" = "x2","x3" = "x3",
              #                     "x4" = "x4","x5" = "x5","x6" = "x6"),
              #          selected = c("x2","x6"),
              #          inline = FALSE,
              #          width = NULL
              #        ),
              #        radioButtons(inputId = "diagnosis_s_stratification_joint",
              #                     label = "Stratification levels",
              #                     choices = c("Combination"=TRUE,
              #                                 "Individual"=FALSE),
              #                     selected = TRUE)),
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
      title = "Validation", width = 12, solidHeader = T, status = "primary",
      fluidRow(
        # column(width = 2,
        #        checkboxGroupInput(
        #          inputId = "validation_stratification",
        #          label = "Stratification by",
        #          choices= c("x1" = "x1","x2" = "x2","x3" = "x3",
        #                     "x4" = "x4","x5" = "x5","x6" = "x6"),
        #          selected = c("x1","x3","x4","x5"),
        #          inline = FALSE,
        #          width = NULL
        #        ),
        #        radioButtons(inputId = "validation_stratification_joint",
        #                     label = "Stratification levels",
        #                     choices = c("Combination"=TRUE,
        #                                 "Individual"=FALSE),
        #                     selected = TRUE)
        #
        #        ),
        column(width = 6, height = "33%",plotOutput(outputId = "vadidation_plot")),
        column(width = 6, height = "33%",tableOutput(outputId = "validation_numeric"))
      )
    )
  )
)
)


