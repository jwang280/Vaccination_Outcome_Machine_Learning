shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Jiangwei Wang (19364744)"),
  tabsetPanel(
    tabPanel("Data",
             tabsetPanel(type = "pills",
              tabPanel("Summary",
                verbatimTextOutput(outputId = "DataSummary"),
              ),
              tabPanel("DFSummary",
                       tableOutput(outputId = 'DfSummary')
              ),
              tabPanel("Boxplot",
               fluidRow(
                 column(width = 12,
                 checkboxGroupInput(inputId = "VariablesA", label = "Variables to show:",
                                    choices = c('Alcohol', 'Coffee', 'Exercise', 'NumDocVisits', 'ReagentA', 'ReagentB', 'ReagentC', 
                                                'ReagentD', 'ReagentE', 'ReagentF', 'ReagentG', 'ReagentH', 'ReagentI', 'ReagentJ', 
                                                'ReagentK', 'ReagentL', 'ReagentM', 'ReagentN', 'Y'), 
                                    selected = c('Alcohol', 'Coffee', 'Exercise', 'NumDocVisits', 'ReagentA', 'ReagentB', 'ReagentC', 
                                                 'ReagentD', 'ReagentE', 'ReagentF', 'ReagentG', 'ReagentH', 'ReagentI', 'ReagentJ', 
                                                 'ReagentK', 'ReagentL', 'ReagentM', 'ReagentN', 'Y'), inline = TRUE)
                 ),
                 column(width = 4,
                        sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
                 ),
                 column(width = 3,
                        checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
                 )
               ),
               plotOutput(outputId = "BoxPlots")
              ),
              tabPanel("Missing Values",
                plotOutput(outputId = "Missing")
              ),
              tabPanel("Correlation",
                plotOutput(outputId = "Corr")
              ),
              tabPanel("Pairs",
               fluidRow(
                 column(width = 12,
                        checkboxGroupInput(inputId = "VariablesB", label = "Variables to show:",
                                           choices = c('Alcohol', 'Coffee', 'Exercise', 'NumDocVisits', 'ReagentA', 'ReagentB', 'ReagentC', 
                                                       'ReagentD', 'ReagentE', 'ReagentF', 'ReagentG', 'ReagentH', 'ReagentI', 'ReagentJ', 
                                                       'ReagentK', 'ReagentL', 'ReagentM', 'ReagentN', 'TreatmentDate', 'Y'), 
                                           selected = c('ReagentH', 'ReagentJ', 'ReagentK', 'ReagentL', 'ReagentM', 'ReagentN', 'Y'), inline = TRUE)
                 )
              ),
                plotOutput(outputId = "Pairs")
              ),
              tabPanel("Raw Data",
                DT::dataTableOutput(outputId = "Table")
              )
        )
    ),
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available Methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             helpText("The preprocessing steps and their order are important. ", 
                      "See:", a("Documentation", href="https://www.rdocumentation.org/packages/recipes/versions/0.1.13")),
             
             tabsetPanel(type = "pills",
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c()), 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               ),
               
               # maintenance point ------------------------------------------------------------------------------
               # add further tabs (with controls) here
               
               tabPanel("Bayesglm Model",
                        verbatimTextOutput(outputId = "bayesglmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "bayesglmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "bayesglmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "bayesglmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bayesglmMetrics"),
                        hr(),
                        plotOutput(outputId = "bayesglmModelPlots"),
                        verbatimTextOutput(outputId = "bayesglmRecipe"),
                        verbatimTextOutput(outputId = "bayesglmModelSummary2")
               ),
               
               tabPanel("GlmStepAIC Model",
                        verbatimTextOutput(outputId = "glmStepAICModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "glmStepAICPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "glmStepAICGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "glmStepAICGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "glmStepAICMetrics"),
                        hr(),
                        plotOutput(outputId = "glmStepAICModelPlots"),
                        verbatimTextOutput(outputId = "glmStepAICRecipe"),
                        verbatimTextOutput(outputId = "glmStepAICModelSummary2")
               ),
               
               tabPanel("PlsRglm Model",
                        verbatimTextOutput(outputId = "plsRglmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "plsRglmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "plsRglmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "plsRglmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "plsRglmMetrics"),
                        hr(),
                        plotOutput(outputId = "plsRglmModelPlots"),
                        verbatimTextOutput(outputId = "plsRglmRecipe"),
                        verbatimTextOutput(outputId = "plsRglmModelSummary2")
               ),
               
               tabPanel("AvNNet Model",
                        verbatimTextOutput(outputId = "avNNetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "avNNetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "avNNetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "avNNetGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "avNNetMetrics"),
                        hr(),
                        plotOutput(outputId = "avNNetModelPlots"),
                        verbatimTextOutput(outputId = "avNNetRecipe"),
                        verbatimTextOutput(outputId = "avNNetModelSummary2")
               ),
               
               tabPanel("knn Model",
                        verbatimTextOutput(outputId = "knnSModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "knnPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c()), 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "knnGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "knnGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "knnMetrics"),
                        hr(),
                        plotOutput(outputId = "knnModelPlots"),
                        plotOutput(outputId = "knnModelTree"),
                        verbatimTextOutput(outputId = "knnRecipe"),
               ),
               
               tabPanel("RF Model",
                        verbatimTextOutput(outputId = "rfModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rfPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rfGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rfGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rfMetrics"),
                        hr(),
                        plotOutput(outputId = "rfModelPlots"),
                        verbatimTextOutput(outputId = "rfRecipe"),
                        verbatimTextOutput(outputId = "rfModelSummary2")
               ),
               
               tabPanel("Kernelpls Model",
                        verbatimTextOutput(outputId = "kernelplsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "kernelplsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "kernelplsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "kernelplsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "kernelplsMetrics"),
                        hr(),
                        plotOutput(outputId = "kernelplsModelPlots"),
                        verbatimTextOutput(outputId = "kernelplsRecipe"),
                        verbatimTextOutput(outputId = "kernelplsModelSummary2")
               ),
               
               tabPanel("Rlm Model",
                        verbatimTextOutput(outputId = "rlmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rlmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rlmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rlmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rlmMetrics"),
                        hr(),
                        plotOutput(outputId = "rlmModelPlots"),
                        verbatimTextOutput(outputId = "rlmRecipe"),
                        verbatimTextOutput(outputId = "rlmModelSummary2")
               ),
               
               tabPanel("Rqlasso Model",
                        verbatimTextOutput(outputId = "rqlassoModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rqlassoPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rqlassoGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rqlassoGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rqlassoMetrics"),
                        hr(),
                        plotOutput(outputId = "rqlassoModelPlots"),
                        verbatimTextOutput(outputId = "rqlassoRecipe"),
                        verbatimTextOutput(outputId = "rqlassoModelSummary2")
               ),
               
               tabPanel("Cubist Model",
                        verbatimTextOutput(outputId = "cubistModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "cubistPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "cubistGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "cubistGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "cubistMetrics"),
                        hr(),
                        plotOutput(outputId = "cubistModelPlots"),
                        verbatimTextOutput(outputId = "cubistRecipe"),
                        verbatimTextOutput(outputId = "cubistModelSummary2")
               ),
               
               tabPanel("gaussprPoly Model",
                        verbatimTextOutput(outputId = "gaussprPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprPolyPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprPolyModelPlots"),
                        verbatimTextOutput(outputId = "gaussprPolyRecipe"),
                        verbatimTextOutput(outputId = "gaussprPolyModelSummary2")
               ),
               
               tabPanel("GaussprRadial Model",
                        verbatimTextOutput(outputId = "gaussprRadialModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprRadialPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprRadialGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprRadialGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprRadialMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprRadialModelPlots"),
                        verbatimTextOutput(outputId = "gaussprRadialRecipe"),
                        verbatimTextOutput(outputId = "gaussprRadialModelSummary2")
               ),
               
               tabPanel("GaussprLinear Model",
                        verbatimTextOutput(outputId = "gaussprLinearModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprLinearPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprLinearGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprLinearGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprLinearMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprLinearModelPlots"),
                        verbatimTextOutput(outputId = "gaussprLinearRecipe"),
                        verbatimTextOutput(outputId = "gaussprLinearModelSummary2")
               ),
               
               tabPanel("SvmPoly Model",
                        verbatimTextOutput(outputId = "svmPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmPolyPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "svmPolyGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "svmPolyGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "svmPolyModelPlots"),
                        verbatimTextOutput(outputId = "svmPolyRecipe"),
                        verbatimTextOutput(outputId = "svmPolyModelSummary2")
               ),
               
               tabPanel("KrlsPoly Model",
                        verbatimTextOutput(outputId = "krlsPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "krlsPolyPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "krlsPolyGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "krlsPolyGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "krlsPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "krlsPolyModelPlots"),
                        verbatimTextOutput(outputId = "krlsPolyRecipe"),
                        verbatimTextOutput(outputId = "krlsPolyModelSummary2")
               ),
               
               tabPanel("RvmPoly Model",
                        verbatimTextOutput(outputId = "rvmPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rvmPolyPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "rvmPolyGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "rvmPolyGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rvmPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "rvmPolyModelPlots"),
                        verbatimTextOutput(outputId = "rvmPolyRecipe"),
                        verbatimTextOutput(outputId = "rvmPolyModelSummary2")
               ),
               
               tabPanel("Qrf Model",
                        verbatimTextOutput(outputId = "qrfModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "qrfPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "qrfGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "qrfGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "qrfMetrics"),
                        hr(),
                        plotOutput(outputId = "qrfModelPlots"),
                        verbatimTextOutput(outputId = "qrfRecipe"),
                        verbatimTextOutput(outputId = "qrfModelSummary2")
               ),
               
               tabPanel("Ranger Model",
                        verbatimTextOutput(outputId = "rangerModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rangerPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "rangerGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "rangerGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rangerMetrics"),
                        hr(),
                        plotOutput(outputId = "rangerModelPlots"),
                        verbatimTextOutput(outputId = "rangerRecipe"),
                        verbatimTextOutput(outputId = "rangerModelSummary2")
               ),
               
               tabPanel("XgbLinear Model",
                        verbatimTextOutput(outputId = "xgbLinearModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "xgbLinearPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "xgbLinearGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "xgbLinearGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "xgbLinearMetrics"),
                        hr(),
                        plotOutput(outputId = "xgbLinearModelPlots"),
                        verbatimTextOutput(outputId = "xgbLinearRecipe"),
                        verbatimTextOutput(outputId = "xgbLinearModelSummary2")
               ),
               
               tabPanel("Blackboost Model",
                        verbatimTextOutput(outputId = "blackboostModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "blackboostPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "blackboostGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "blackboostGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "blackboostMetrics"),
                        hr(),
                        plotOutput(outputId = "blackboostModelPlots"),
                        verbatimTextOutput(outputId = "blackboostRecipe"),
                        verbatimTextOutput(outputId = "blackboostModelSummary2")
               ),
               
               tabPanel("BagEarthGCV Model",
                        verbatimTextOutput(outputId = "bagEarthGCVModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "bagEarthGCVPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "bagEarthGCVGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "bagEarthGCVGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bagEarthGCVMetrics"),
                        hr(),
                        plotOutput(outputId = "bagEarthGCVModelPlots"),
                        verbatimTextOutput(outputId = "bagEarthGCVRecipe"),
                        verbatimTextOutput(outputId = "bagEarthGCVModelSummary2")
               ),
               
               tabPanel("GaussCubistEnsemble Model",
                        verbatimTextOutput(outputId = "GaussCubistEnsembleModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GaussCubistEnsemblePreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("naomit","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "GaussCubistEnsembleGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "GaussCubistEnsembleGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GaussCubistEnsembleMetrics"),
                        hr(),
                        plotOutput(outputId = "GaussCubistEnsembleModelPlots"),
                        verbatimTextOutput(outputId = "GaussCubistEnsembleRecipe"),
                        verbatimTextOutput(outputId = "GaussCubistEnsembleModelSummary2")
               )
               
               
               # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
               
               
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width=4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width=2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width=2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
