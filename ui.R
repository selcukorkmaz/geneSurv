library("shinythemes")
library("shinyBS")

  shinyUI(
   fluidPage(
    theme = "css/mytheme.css",
     sidebarPanel(width=3,


         ################################# About (start) #####################################

      conditionalPanel(condition="input.tabs1=='About'",
        
        HTML('<center><img src="images/logoSurv.png" width=200 height=200></center>')

       ),

       conditionalPanel(condition="input.tabs1=='Authors'",


            HTML('<center><img src="images/team.png" width=200 height=200></center>')

        ),    

        conditionalPanel(condition="input.tabs1=='Help'",

           # HTML('<img src="images/help.png" width=300 height=200>'),

              conditionalPanel(condition = "input.tabs1 == 'Help'",
                h4(tags$b("Help")), 
                a("Usage of the tool", href = "#usage"), br(),
                a("1. Data upload", href = "#upload"), br(),
                a("2. Analysis", href = "#analysis"), br(),
                a("2.1. Life Table", href = "#lt"), br(),
                a("2.2. Kaplan-Meier", href = "#km"), br(),
                a("2.3. Cox Regression", href = "#cox")
              )

        ),  

         ################################# About (end) #####################################

         ################################# Data Upload (start) #####################################


      conditionalPanel(condition="input.tabs1=='Data Upload'",

          h4("Input data"),
          radioButtons("dataInput", "", list("Upload a file" = 2, "Load example data" = 1), selected=1),

          conditionalPanel(condition="input.dataInput=='1'",
            h5("Load example data:"),
            radioButtons("sampleData", "", list("Example data (Life Table and Kaplan-Meier)"=1, "Example data2 (Cox Regression)"=2), selected=1)
           ),

          conditionalPanel(condition="input.dataInput=='2'",
            h5("Upload a delimited text file: "),

            fileInput("upload", "", multiple = FALSE),

            radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2),

            HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
            HTML('<p>Note: First row must be header.</p>')
           )
        ),
         ################################# Data Upload (end) #####################################

             ########## Analysis (start) #####################################


      conditionalPanel(condition="input.tabs1=='Analysis'",


          selectizeInput(inputId = "selectAnalysis", label = "Select a Method", choices = c("Life Table" = 1, "Kaplan-Meier" = 2, "Cox Regression" = 3, "Cox Regression with Lasso" = 4, "Random Survival Forests" = 5), selected = 2),



          conditionalPanel(condition="input.selectAnalysis=='1'",
            checkboxInput(inputId = "inputs", label = tags$b("Inputs"), value = TRUE),
              conditionalPanel(condition = "input.inputs",

                selectizeInput("survivalTime", "Survival time", choices = NULL, multiple = FALSE),

                selectizeInput("statusVariable", "Select status variable", choices = NULL, multiple = FALSE),

                selectizeInput("status", "Select category for status variable", choices = NULL, multiple = FALSE),

                checkboxInput(inputId = "factorVar", label = "Factor variable", value = TRUE),
                
                  conditionalPanel(condition = "input.factorVar",
              
                    selectizeInput("factor", "Factor", choices = NULL, multiple = FALSE)
              
                   ),


                 fluidRow(column(4,numericInput("from", "From", value = 0)),
                   column(4,numericInput("to", "To", value = 60)),
                    column(4,numericInput("by", "By", value = 12))
                ),

                checkboxInput(inputId = "advancedOptions", label = "Advanced Options", value = FALSE),
                  
                  #column(12, tags$style(type="text/css", '#leftPanel { width:250px; padding:-10px; float:left;}'),

                    conditionalPanel(condition = "input.advancedOptions",

                      selectizeInput("ci", "Confidence interval type", choices = list("Log" = "log", "Log-Log" = "log-log", "Plain" = "plain"), multiple = FALSE),

                      selectizeInput("varianceEstimation", "Variance estimation", choices = list("Greenwood" = "greenwood", "Tsiatis" = "tsiatis"), multiple = FALSE),

                      selectizeInput("comparisonTest", "Comparison test", choices = list("Log-rank" = "logRank", "Gehan-Breslow" = "gehanBreslow", "Tarone-Ware" = "taroneWare", "Peto-Peto" = "petoPeto", "Modified Peto-Peto" = "modPetoPeto", "Flemington-Harrington" = "flemingtonHarnington"), multiple = FALSE),

                      numericInput("confidenceLevel", "Confidence level", value = 95, min = 0, max = 100),

                      radioButtons("refCategory", "Reference category", choices = list("First" = "first", "Last" = "last"))

                     )
                  #)
               ),

            checkboxInput(inputId = "outputs", label = tags$b("Outputs"), value = FALSE),

              #column(12, tags$style(type="text/css", '#leftPanel { width:250px; padding:-10px; float:left;}'),

                conditionalPanel(condition = "input.outputs",


#HTML('<p>  <script src="https://code.highcharts.com/highcharts.js"></script><div id="result"></div><table id="table-sparkline"> <thead><tr><th>State</th><th>Income</th><th>Income per quarter</th><th>Costs</th><th>Costs per quarter</th><th>Result</th><th>Result per quarter</th></tr></thead><tbody id="tbody-sparkline"><tr><th>Alabama</th><td>254</td><td data-sparkline="71, 78, 39, 66 "/><td>296</td><td data-sparkline="68, 52, 80, 96 "/><td>-42</td><td data-sparkline="3, 26, -41, -30 ; column"/></tr></tbody></table></p>'),     


                  checkboxInput(inputId = "caseSummary", label = "Case summary", value = TRUE),
                  checkboxInput(inputId = "lifeTable", label = "Life table", value = TRUE),
                  checkboxInput(inputId = "medianLifeTime", label = "Median life time", value = TRUE),
                  checkboxInput(inputId = "hr", label = "Hazard ratio", value = TRUE),
                  checkboxInput(inputId = "compTest", label = "Comparison test", value = TRUE)

                  ),
                #),

            actionButton(inputId = "run", label = "Run", icon = icon("play", lib = "glyphicon"))

           ),

          conditionalPanel(condition="input.selectAnalysis=='2'",

            checkboxInput(inputId = "inputsKM", label = tags$b("Inputs"), value = TRUE),
              conditionalPanel(condition = "input.inputsKM",

              selectizeInput("survivalTimeKM", "Survival time", choices = NULL, multiple = FALSE),

              selectizeInput("statusVariableKM", "Select status variable", choices = NULL, multiple = FALSE),

              selectizeInput("statusKM", "Select category for status variable", choices = NULL, multiple = FALSE),

              checkboxInput(inputId = "factorVarKM", label = "Factor variable", value = TRUE),
              
              conditionalPanel(condition = "input.factorVarKM",
                
                selectizeInput("factorKM", "Factor", choices = NULL, multiple = FALSE)
                ),

              checkboxInput(inputId = "advancedOptionsKM", label = "Advanced Options", value = FALSE),
                #column(12, tags$style(type="text/css", '#leftPanel { width:250px; padding:-10px; float:left;}'),

                conditionalPanel(condition = "input.advancedOptionsKM",

                  selectizeInput("ciKM", "Confidence interval type", choices = list("Log" = "log", "Log-Log" = "log-log", "Plain" = "plain"), multiple = FALSE),

                  selectizeInput("varianceEstimationKM", "Variance estimation", choices = list("Greenwood" = "greenwood", "Tsiatis" = "tsiatis"), multiple = FALSE),

                  selectizeInput("comparisonTestKM", "Comparison test", choices = list("Log-rank" = "logRank", "Gehan-Breslow" = "gehanBreslow", "Tarone-Ware" = "taroneWare", "Peto-Peto" = "petoPeto", "Modified Peto-Peto" = "modPetoPeto", "Flemington-Harrington" = "flemingtonHarnington"), multiple = FALSE),

                  numericInput("confidenceLevelKM", "Confidence level", value = 95, min = 0, max = 100),

                  radioButtons("refCategoryKM", "Reference category", choices = list("First" = "first", "Last" = "last"))


                  )
                #)

              ),

            checkboxInput(inputId = "outputsKM", label = tags$b("Outputs"), value = FALSE),

              column(12, tags$style(type="text/css", '#leftPanel { width:250px; padding:-10px; float:left;}'),

                conditionalPanel(condition = "input.outputsKM",

                  checkboxInput(inputId = "caseSummaryKM", label = "Case summary", value = TRUE),
                  checkboxInput(inputId = "survivalTable", label = "Survival table", value = TRUE),
                  checkboxInput(inputId = "meanMedianSurvivalTimes", label = "Mean and median life time", value = TRUE),
                  #checkboxInput(inputId = "quartilesOfSurvivalTimes", label = "Quartiles of survival times", value = TRUE),
                  checkboxInput(inputId = "hrKM", label = "Hazard ratio", value = TRUE),
                  checkboxInput(inputId = "compTestKM", label = "Comparison test", value = TRUE)


                 )
              ),

            actionButton(inputId = "runKM", label = "Run", icon = icon("play", lib = "glyphicon"))

           ),

          conditionalPanel(condition="input.selectAnalysis=='3'",

            checkboxInput(inputId = "inputsCox", label = tags$b("Inputs"), value = TRUE),
            
              conditionalPanel(condition = "input.inputsCox",
            

                  selectizeInput("survivalTimeCox", "Survival time", choices = NULL, multiple = FALSE),
              
                  selectizeInput("statusVariableCox", "Select status variable", choices = NULL, multiple = FALSE),
              
                  selectizeInput("statusCox", "Select category for status variable", choices = NULL, multiple = FALSE),
              
                  selectizeInput("categoricalInput", "Categorical variable(s)", choices = NULL, multiple = TRUE),
              
                  selectizeInput("continuousInput", "Continuous variable(s)", choices = NULL, multiple = TRUE),


                      

              
                  checkboxInput(inputId = "advancedOptionsCox", label = "Advanced Options", value = FALSE),


                        conditionalPanel(condition = "input.advancedOptionsCox",
                        checkboxInput(inputId = "multipleID", label = "Multiple ID", value = FALSE),

                          checkboxInput(inputId = "addInteractions", label = "Add interactions", value = FALSE),

                          
                        conditionalPanel(condition = "input.addInteractions",


                                checkboxInput(inputId = "twoWayInteractions", label = "All 2-way interactions", value = FALSE),
                                checkboxInput(inputId = "threeWayInteractions", label = "All 3-way interactions", value = FALSE),
                                checkboxInput(inputId = "customInteractions", label = "Custom interactions", value = FALSE),
                                
                            conditionalPanel(condition = "input.customInteractions",

                              selectizeInput(inputId = "selectCustomInteractions", label = "Select interactions", choices = NULL, multiple = TRUE)

                            )


                        ),

                      checkboxInput(inputId = "addTimeDependentCovariates", label = "Add time dependendent covariates", value = FALSE),
  
                        conditionalPanel(condition = "input.addTimeDependentCovariates",

                                    radioButtons("timeDepTransform", "Transformation", choices = list("None" = "none", "Log" = "log"), selected = "none"),

                                    selectizeInput(inputId = "selectTimeDependentVariables", label = "Select covariates", choices = NULL, multiple = TRUE)

                          
                        ),


                      checkboxInput(inputId = "addStrata", label = "Add strata", value = FALSE),

                            conditionalPanel(condition = "input.addStrata",

                              selectizeInput(inputId = "selectStrataVariable", label = "Select strata variable", choices = NULL, multiple = FALSE)

                            
                        ),

                      checkboxInput(inputId = "moreOptions", label = "More options", value = FALSE),

                          conditionalPanel(condition = "input.moreOptions",

                              radioButtons("modelSelectionCriteria", "Model selection criteria", choices = list("AIC" = "aic", "p value" = "pValue"), selected = "aic"),

                              conditionalPanel(condition = "input.modelSelectionCriteria == 'pValue'",
                        
                              numericInput("alphaToEnter", "Alpha to enter", value = 0.05, min = 0, max = 1, step = 0.05),
                        
                              numericInput("alphaToRemove", "Alpha to remove", value = 0.10, min = 0, max = 1, step = 0.05)
                            
                              ),

                          

                            selectizeInput("modelSelection", "Model selection", choices = list("Enter" = "enter", "Backward" = "backward", "Forward" = "forward", "Stepwise" = "stepwise"), selected = "enter"),
                            numericInput("confidenceLevelCox", "Confidence level", value = 95, min = 0, max = 100),
                            radioButtons("refCategoryCox", "Reference category", choices = list("First" = "first", "Last" = "last")),
                            selectizeInput("ties", "Ties", choices = list("Efron" = "efron", "Breslow" = "breslow", "Exact" = "exact"), selected = "breslow", multiple = FALSE)
                         
                    ))
                  ),
                
            checkboxInput(inputId = "outputsCox", label = tags$b("Outputs"), value = FALSE),
                
              conditionalPanel(condition = "input.outputsCox",
                 #   column(12, tags$style(type="text/css", '#leftPanel { width:250px; padding:-10px; float:left;}'),

                    #checkboxInput(inputId = "displayDescriptives", label = "Descriptives", value = TRUE),
                    checkboxInput(inputId = "displayCoefficientEstimates", label = "Coefficient estimates", value = TRUE),
                    #checkboxInput(inputId = "displayModelFit", label = "Model fit", value = TRUE),
                    checkboxInput(inputId = "hrcox", label = "Hazard ratio", value = TRUE),
                    checkboxInput(inputId = "goodnessOfFitTests", label = "Goodness of fit tests", value = TRUE),
                    checkboxInput(inputId = "analysisOfDeviance", label = "Analysis of deviance", value = FALSE),
                    checkboxInput(inputId = "storePredictions", label = "Predictions", value = FALSE),
                    checkboxInput(inputId = "residuals", label = "Residuals", value = FALSE),
                    checkboxInput(inputId = "martingaleResiduals", label = "Martingale residuals", value = FALSE),
                    checkboxInput(inputId = "schoenfeldResiduals", label = "Schoenfeld residuals", value = FALSE),
                    checkboxInput(inputId = "dfBetas", label = "DfBetas", value = FALSE)
                #)
            ),

            actionButton(inputId = "runCox", label = "Run", icon = icon("play", lib = "glyphicon"))
        
           ),

      conditionalPanel(condition="input.selectAnalysis=='5'",
                 
                 checkboxInput(inputId = "inputsRF", label = tags$b("Inputs"), value = TRUE),
                 
                 conditionalPanel(condition = "input.inputsRF",
                                  
                                  
                                  selectizeInput("survivalTimeRF", "Survival time", choices = NULL, multiple = FALSE),
                                  
                                  selectizeInput("statusVariableRF", "Select status variable", choices = NULL, multiple = FALSE),
                                  
                                  selectizeInput("statusRF", "Select category for status variable", choices = NULL, multiple = FALSE),
                                  
                                  selectizeInput("categoricalInputRF", "Categorical variable(s)", choices = NULL, multiple = TRUE),
                                  
                                  selectizeInput("continuousInputRF", "Continuous variable(s)", choices = NULL, multiple = TRUE),
                                  
                                  
                                  
                                  
                                  
                                  checkboxInput(inputId = "advancedOptionsRF", label = "Advanced Options", value = FALSE),
                                  
                                  
                                  conditionalPanel(condition = "input.advancedOptionsRF",
                                                   checkboxInput(inputId = "multipleIDRF", label = "Multiple ID", value = FALSE),
                                                   
                                                   checkboxInput(inputId = "addInteractionsRF", label = "Add interactions", value = FALSE),
                                                   
                                                   
                                                   conditionalPanel(condition = "input.addInteractionsRF",
                                                                    
                                                                    
                                                                    checkboxInput(inputId = "twoWayInteractionsRF", label = "All 2-way interactions", value = FALSE),
                                                                    checkboxInput(inputId = "threeWayInteractionsRF", label = "All 3-way interactions", value = FALSE),
                                                                    checkboxInput(inputId = "customInteractionsRF", label = "Custom interactions", value = FALSE),
                                                                    
                                                                    conditionalPanel(condition = "input.customInteractionsRF",
                                                                                     
                                                                                     selectizeInput(inputId = "selectCustomInteractionsRF", label = "Select interactions", choices = NULL, multiple = TRUE)
                                                                                     
                                                                    )
                                                                    
                                                                    
                                                   ),
                                                   
                                                   checkboxInput(inputId = "addTimeDependentCovariatesRF", label = "Add time dependendent covariates", value = FALSE),
                                                   
                                                   conditionalPanel(condition = "input.addTimeDependentCovariatesRF",
                                                                    
                                                                    radioButtons("timeDepTransformRF", "Transformation", choices = list("None" = "none", "Log" = "log"), selected = "none"),
                                                                    
                                                                    selectizeInput(inputId = "selectTimeDependentVariablesRF", label = "Select covariates", choices = NULL, multiple = TRUE)
                                                                    
                                                                    
                                                   ),
                                                   
                                                   
                                                   checkboxInput(inputId = "addStrataRF", label = "Add strata", value = FALSE),
                                                   
                                                   conditionalPanel(condition = "input.addStrataRF",
                                                                    
                                                                    selectizeInput(inputId = "selectStrataVariableRF", label = "Select strata variable", choices = NULL, multiple = FALSE)
                                                                    
                                                                    
                                                   ),
                                                   
                                                   checkboxInput(inputId = "moreOptionsRF", label = "More options", value = FALSE),
                                                   
                                                   conditionalPanel(condition = "input.moreOptionsRF",
                                                                    
                                                                    radioButtons("modelSelectionCriteriaRF", "Model selection criteria", choices = list("AIC" = "aic", "p value" = "pValue"), selected = "aic"),
                                                                    
                                                                    conditionalPanel(condition = "input.modelSelectionCriteriaRF == 'pValue'",
                                                                                     
                                                                                     numericInput("alphaToEnterRF", "Alpha to enter", value = 0.05, min = 0, max = 1, step = 0.05),
                                                                                     
                                                                                     numericInput("alphaToRemoveRF", "Alpha to remove", value = 0.10, min = 0, max = 1, step = 0.05)
                                                                                     
                                                                    ),
                                                                    
                                                                    
                                                                    
                                                                    selectizeInput("modelSelectionRF", "Model selection", choices = list("Enter" = "enter", "Backward" = "backward", "Forward" = "forward", "Stepwise" = "stepwise"), selected = "enter"),
                                                                    numericInput("confidenceLevelRF", "Confidence level", value = 95, min = 0, max = 100),
                                                                    radioButtons("refCategoryRF", "Reference category", choices = list("First" = "first", "Last" = "last")),
                                                                    selectizeInput("tiesRF", "Ties", choices = list("Efron" = "efron", "Breslow" = "breslow", "Exact" = "exact"), selected = "breslow", multiple = FALSE)
                                                                    
                                                   ))
                 ),
                 
                 checkboxInput(inputId = "outputsRF", label = tags$b("Outputs"), value = FALSE),
                 
                 conditionalPanel(condition = "input.outputsRF",
                                  #   column(12, tags$style(type="text/css", '#leftPanel { width:250px; padding:-10px; float:left;}'),
                                  
                                  #checkboxInput(inputId = "displayDescriptives", label = "Descriptives", value = TRUE),
                                  checkboxInput(inputId = "displayCoefficientEstimatesRF", label = "Coefficient estimates", value = TRUE),
                                  #checkboxInput(inputId = "displayModelFit", label = "Model fit", value = TRUE),
                                  checkboxInput(inputId = "hrRF", label = "Hazard ratio", value = TRUE),
                                  checkboxInput(inputId = "goodnessOfFitTestsRF", label = "Goodness of fit tests", value = TRUE),
                                  checkboxInput(inputId = "analysisOfDevianceRF", label = "Analysis of deviance", value = FALSE),
                                  checkboxInput(inputId = "storePredictionsRF", label = "Predictions", value = FALSE),
                                  checkboxInput(inputId = "residualsRF", label = "Residuals", value = FALSE),
                                  checkboxInput(inputId = "martingaleResidualsRF", label = "Martingale residuals", value = FALSE),
                                  checkboxInput(inputId = "schoenfeldResidualsRF", label = "Schoenfeld residuals", value = FALSE),
                                  checkboxInput(inputId = "dfBetasRF", label = "DfBetas", value = FALSE)
                                  #)
                 ),
                 
                 actionButton(inputId = "runRF", label = "Run", icon = icon("play", lib = "glyphicon"))
                 
          )

       )
             

             ########## Analysis (end) #####################################

    ),




	mainPanel(

    navbarPage("compSurv: Complete Survival Analysis v.0.5", id="tabs1", inverse = TRUE, collapsible = TRUE, fluid = TRUE, position = "fixed-top", class("navbar navbar-inverse"),

 

         tabPanel("About",

           #br(),
           h4(tags$b('What is survival analysis?')),

             HTML('<p align="justify"> Survival analysis is described as collection of statistical methods for which the response variable of interest
              is <b><i>time until an event occurs.</i></b> In this context, the <b><i>time</i></b> can be days, week, months and years from the beginning of follow-up of an individual until an event occurs,
                or the age of an individual when the event occurs. Moreover, the <b><i>event</i></b> can be death, disease, remission, recovery or any experience of
                interest that may occur to an individual. A more detailed information can be found in <a href="https://books.google.com.tr/books?id=hNDkBwAAQBAJ&printsec=frontcover&dq=survival+analysis+kleinbaum&hl=tr&sa=X&redir_esc=y#v=onepage&q=survival%20analysis%20kleinbaum&f=false" target="_blank"> <b>Kleinbaum</b></a> and <a href="https://books.google.com.tr/books?id=wQlR8UNHZXgC&printsec=frontcover&dq=analysing+survival+data+from+clinical+trials&hl=tr&sa=X&ved=0ahUKEwixoLiIyMrOAhUDOhoKHStQDikQ6AEIKjAA#v=onepage&q=analysing%20survival%20data%20from%20clinical%20trials&f=false" target="_blank"> <b>Marubini and Valsecchi</b></a>.</p>'),


            #br(),

           h4(tags$b('An interactive tool for survival analysis!')),

              HTML('<p align="justify"> Here we developed an easy-to-use, up-to-date, comprehensive and interactive web-based tool for survival analysis. This tool includes 
                analysis procedures for life table, Kaplan-Meier and Cox regression. Each procedure includes following features:</p>'),


           
              HTML('<p align="justify"> <b> Life table:</b> descriptive statistics, life table, median life time, hazard ratios and comparison tests
                including Log-rank, Gehan-Breslow, Tarone-Ware, Peto-Peto, Modified Peto-Peto, Flemington-Harrington.</p>'),

              HTML('<p align="justify"> <b> Kaplan-Meier:</b> descriptive statistics, survival table, mean and median life time, hazard ratios, comparison tests
                including Log-rank, Gehan-Breslow, Tarone-Ware, Peto-Peto, Modified Peto-Peto, Flemington-Harrington, and interactive plots such as Kaplan-Meier curves and hazard plots.</p>'),
 
              HTML('<p align="justify"> <b> Cox regression:</b> coefficient estimates, hazard ratios, goodness of fit test, analysis of deviance, save predictions, save residuals,
                save Martingale residuals, save Schoenfeld residuals, save dfBetas, proportional hazard assumption test, and interactive plots including Schoenfeld residual plot and Log-Minus-Log plot.</p>'),


              HTML('<p align="justify"> All source codes are in <a href="https://github.com/selcukorkmaz/compSurv" target="_blank"><b>GitHub</b></a>. Please see the help page for more detailed information.</p>'),

              HTML('<p><div align = "center"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/kmPlot.jpg" width="300" height="200" ></td><td><img src="images/schoenfeldPlot.jpg" width="300" height="200"></td><td><img src="images/lmlPlot.jpg" width="300" height="200"></td></tr></table></div></p>'),

              HTML('<p><b>License:</b> This web-tool as a whole is distributed under GPL-3 (General Public License Version 3).</p>'),

              HTML('<p><b>Disclaimer:</b> This server is intended for research purposes only, not for clinical or commercial use. It is a non-profit service to the scientific community, provided on an "AS-IS " basis without any warranty, expressed or implied. The authors can not be held liable in any way for the service provided here.</p>')

             #HTML('<center><img src="images/images.jpg" width=800 height=250></center>')



          ),

         tabPanel("Data Upload",

           DT::dataTableOutput('dataUpload')

         ),

         tabPanel("Analysis",


            conditionalPanel(condition="input.selectAnalysis=='1'",


              tabsetPanel(

                  tabPanel('Result',

                      h4(textOutput(outputId = "descriptivesText")),
                      DT::dataTableOutput('descriptives'),

                      h4(textOutput(outputId = "lifeTableText")),
                      DT::dataTableOutput('lifetableResult'),

                      h4(textOutput(outputId = "medianLifeTimeText")),
                      DT::dataTableOutput('medianLifeTimeResult'),

                      h4(textOutput(outputId = "hrText")),
                      DT::dataTableOutput('hazardRatioResult'),

                      h4(textOutput(outputId = "compTestText")),
                      DT::dataTableOutput('comparisonTestResults')

             )

            #tabPanel('Plot',
         
                 #br(),
                 
                 #fluidRow(
                 #  column(4,actionButton(inputId = "createLTPlot", label = "Create Plot", icon = icon("signal", lib = "glyphicon")))
                 #),
                # 
                # plotly::plotlyOutput('LTCurvePlot'),
                # 
                # conditionalPanel(condition = "input.createLTPlot" ,
                #                  
                #                  checkboxInput(inputId = "LTPlotOptions", label = "Edit Plot", value = FALSE)
                #                  
                # ),
                # # LT Options
                # conditionalPanel(condition = "input.LTPlotOptions" ,
                #                
                #                  
                #                  fluidRow(
                #                    column(4,textInput("mainPanelLT", "Main Title", value = "Life Table Survival Plot")),
                #                    column(4,textInput("xlabLT", "X-axis Label", value = "Time")),
                #                    column(4,textInput("ylabLT", "Y-axis Label", value = "Survival Probability"))
                #                  ),
                #                
                #                selectizeInput("ltyLT", "LT curve line type",
                #                                            choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "1",
                #                                                        "\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "2",
                #                                                        "\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "3",
                #                                                        "\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "4",
                #                                                        "\U2500 \U2500 \U2500 \U2500 \U2500" = "5",
                #                                                        "\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "6"),
                #                                            selected = "1")
                                  
          #) # End for LT Options
         
         # Hazard Plot Options
                  # End for Hazard plot options

          #) # End for Plot

                  #)  # End for Tabset Panel  
            )
          ), # End for Condition 1


            conditionalPanel(condition="input.selectAnalysis=='2'",

              tabsetPanel(

                  tabPanel('Result',
                    #verbatimTextOutput("str"),
                    h4(textOutput(outputId = "descriptivesTextKM")),
                    DT::dataTableOutput('descriptivesKM'),

                    h4(textOutput(outputId = "survivalTableTextKM")),

                    
                    DT::dataTableOutput('survivaltableResult'),
                    conditionalPanel(condition = "input.runKM",
                    conditionalPanel(condition = "input.survivalTable",
                      checkboxInput(inputId = "createSurvivalPlot", label = "Create Plot", value = FALSE)
                    )),

                    conditionalPanel(condition = "input.createSurvivalPlot",

                        selectizeInput(inputId = "selectGroup", label = "Select a factor group", choices = NULL, selected = NULL),
                        highcharter::highchartOutput('survivalPlot')
                        
                      ),

                    h4(textOutput(outputId = "meanMedianSurvivalTimesText")),
                    DT::dataTableOutput('meanMedianSurvivalTimesResult'),

                    h4(textOutput(outputId = "hrTextKM")),
                    DT::dataTableOutput('hazardRatioResultKM'),
                    conditionalPanel(condition = "input.runKM",
                    conditionalPanel(condition = "input.hrKM",
                      checkboxInput(inputId = "createHazardPlot", label = "Create Plot", value = FALSE)
                    )),

                    conditionalPanel(condition = "input.createHazardPlot",

                        selectizeInput(inputId = "selectGroupHazard", label = "Select a factor group", choices = NULL, selected = NULL),
                        highcharter::highchartOutput('hazardPlot')
                        
                      ),


                    h4(textOutput(outputId = "compTestTextKM")),
                    DT::dataTableOutput('comparisonTestResultsKM')
                  
                  ),

                  tabPanel('Plot',

                        br(),

                        fluidRow(
                           column(4,selectizeInput(inputId = "selectPlotKM", label = "Select a plot", choices = c("Kaplan-Meier Curve" = 1, "Hazard Plot" = 2, "Log-Minus-Log" = 3), selected = 1)),
                           column(4,actionButton(inputId = "createKmPlot", label = "Create Plot", icon = icon("signal", lib = "glyphicon")))
                        ),

                        #plotly::plotlyOutput('kmCurvePlot'),

                        highcharter::highchartOutput('kmCurvePlot', width = "100%", height = "525px"),


                        conditionalPanel(condition = "input.createKmPlot" ,
                      
                          checkboxInput(inputId = "kmPlotOptions", label = "Edit Plot", value = FALSE)
                      
                        ),
                           # KM Options
                        conditionalPanel(condition = "input.kmPlotOptions && input.selectPlotKM == 1" ,

                          fluidRow(
                                column(4,textInput("mainPanelKM", "Main Title", value = "Kaplan Meier Plot")),
                                column(4,textInput("xlabKM", "X-axis Label", value = "Time")),
                                column(4,textInput("ylabKM", "Y-axis Label", value = "Survival Probability"))
                          ),





                            fluidRow(
                                column(4, checkboxInput("addCens", "Add censored cases", value = TRUE)),

                                column(4,selectizeInput("censShapeKM", "Select censored case shape",
                                  choices = c("\U002B" = "plus", "\U25EF" = "circle", "\U25B3" = "triangle",  "\U25A2" = "square", "\U2573" = "cross"),
                                  selected = "plus")),
                                column(4,shinyjs::colourInput("censColKM", "Choose censored cases color", value = "black"))
                            ),


                            fluidRow(
                              column(4,selectizeInput("ltyKM", "KM curve line type",
                                choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "Solid",
                                 "\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "ShortDash",
                                 "\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "ShortDot",
                                 "\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "ShortDashDot",
                                 "\U2500 \U2500 \U2500 \U2500 \U2500" = "LongDash",
                                 "\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "LongDashShortDash"),
                                 selected = "Solid")
                               ),

                              column(4,checkboxInput(inputId = "addCI", label = "Add Confidence Interval", value = TRUE)),

                              column(4, sliderInput("alpha", "Confidence Interval Opacity", min = 0, max = 1, value = 0.5, step= 0.1))
                            ),

                        
                          
                          fluidRow(
                              column(4, shinyjs::colourInput("backgroundKM", "Plot background color", value = "white")),

                              column(4, selectInput("changeTheme", "Select a theme", choices = c("Default" = "theme0", "Five Thirty Eight" = "theme1", "Economist" = "theme2", 
                                "Financial Times" = "theme3", "Dotabuff" = "theme4", "Flat" = "theme5", "Flat Dark" = "theme6", "Simple" = "theme7", 
                                "Elementary" = "theme8", "Google" = "theme9", "Grid Light" = "theme10", "Sand Signika" = "theme11"), 
                              selected = "theme0"))
                        )

                    ), # End for KM Options
                  



                          # Hazard Plot Options
                        conditionalPanel(condition = "input.kmPlotOptions && input.selectPlotKM == 2" ,
                           fluidRow(
                                column(4,textInput("mainPanelHazard", "Main Title", value = "Hazard Plot")),
                                column(4,textInput("xlabHazard", "X-axis Label", value = "Time")),
                                column(4,textInput("ylabHazard", "Y-axis Label", value = "Cumulative Hazard"))
                          ),

                            fluidRow(
                                column(4, checkboxInput("addCensHazard", "Add censored cases", value = TRUE)),

                                column(4,selectizeInput("censShapeHazard", "Select censored case shape",
                                  choices = c("\U002B" = "plus", "\U25EF" = "circle", "\U25B3" = "triangle",  "\U25A2" = "square", "\U2573" = "cross"),
                                  selected = "plus")),
                                column(4,shinyjs::colourInput("censColHazard", "Choose censored cases color", value = "black"))
                            ),


                            fluidRow(
                              column(4,selectizeInput("ltyHazard", "Hazard curve line type",
                                choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "Solid",
                                 "\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "ShortDash",
                                 "\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "ShortDot",
                                 "\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "ShortDashDot",
                                 "\U2500 \U2500 \U2500 \U2500 \U2500" = "LongDash",
                                 "\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "LongDashShortDash"),
                                 selected = "Solid")
                               ),

                              column(4,checkboxInput(inputId = "addCIHazard", label = "Add Confidence Interval", value = TRUE)),

                              column(4, sliderInput("alphaHazard", "Confidence Interval Opacity", min = 0, max = 1, value = 0.5, step= 0.1))
                            ),

                        
                          
                          fluidRow(
                              column(4, shinyjs::colourInput("backgroundHazard", "Plot background color", value = "white")),

                              column(4, selectInput("changeThemeHazard", "Select a theme", choices = c("Default" = "theme0", "Five Thirty Eight" = "theme1", "Economist" = "theme2", 
                                "Financial Times" = "theme3", "Dotabuff" = "theme4", "Flat" = "theme5", "Flat Dark" = "theme6", "Simple" = "theme7", 
                                "Elementary" = "theme8", "Google" = "theme9", "Grid Light" = "theme10", "Sand Signika" = "theme11"), 
                              selected = "theme0"))
                        )
                        ), 
                          # End for Hazard plot options

                          # Lml Plot Options
                conditionalPanel(condition = "input.kmPlotOptions && input.selectPlotKM == 3" ,
                 fluidRow(
                   column(4,textInput("mainPanelLml", "Main Title", value = "Log-Minus-Log Plot")),
                   column(4,textInput("xlabLml", "X-axis Label", value = "Time")),
                   column(4,textInput("ylabLml", "Y-axis Label", value = "log(-log(survival))"))
                 ),
                 
                 fluidRow(
                   column(4, checkboxInput("addCensLml", "Add censored cases", value = TRUE)),
                   
                   column(4,selectizeInput("censShapeLml", "Select censored case shape",
                                           choices = c("\U002B" = "plus", "\U25EF" = "circle", "\U25B3" = "triangle",  "\U25A2" = "square", "\U2573" = "cross"),
                                           selected = "plus")),
                   column(4,shinyjs::colourInput("censColLml", "Choose censored cases color", value = "black"))
                 ),
                 
                 
                 fluidRow(
                   column(4,selectizeInput("ltyLml", "Lml curve line type",
                                           choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "Solid",
                                                       "\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "ShortDash",
                                                       "\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "ShortDot",
                                                       "\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "ShortDashDot",
                                                       "\U2500 \U2500 \U2500 \U2500 \U2500" = "LongDash",
                                                       "\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "LongDashShortDash"),
                                           selected = "Solid")
                   ),
                   
                   column(4,checkboxInput(inputId = "addCILml", label = "Add Confidence Interval", value = TRUE)),
                   
                   column(4, sliderInput("alphaLml", "Confidence Interval Opacity", min = 0, max = 1, value = 0.5, step= 0.1))
                 ),
                 
                 
                 
                 fluidRow(
                   column(4, shinyjs::colourInput("backgroundLml", "Plot background color", value = "white")),
                   
                   column(4, selectInput("changeThemeLml", "Select a theme", choices = c("Default" = "theme0", "Five Thirty Eight" = "theme1", "Economist" = "theme2", 
                                                                                            "Financial Times" = "theme3", "Dotabuff" = "theme4", "Flat" = "theme5", "Flat Dark" = "theme6", "Simple" = "theme7", 
                                                                                            "Elementary" = "theme8", "Google" = "theme9", "Grid Light" = "theme10", "Sand Signika" = "theme11"), 
                                         selected = "theme0"))
                 )
                ) #End for LML options

                     ) # End for Plot

                  )  # End for Tabset Panel  
            
            ), # End for Condition 2   



        
            conditionalPanel(condition="input.selectAnalysis=='3'",

              tabsetPanel(
            
               tabPanel('Model',

                    #verbatimTextOutput("str"),
                    h4(textOutput(outputId = "displaySummaryCox")),
                    verbatimTextOutput("summaryCox"),

                    h4(textOutput(outputId = "displayCoefficientEstimatesCox")),
                    DT::dataTableOutput('displayCoefficientEstimatesResult'),

                    h4(textOutput(outputId = "hazardRatioCox")),
                    DT::dataTableOutput('hazardRatioResultCox'),
                    conditionalPanel(condition = "input.runCox",
                      conditionalPanel(condition = "input.hrcox",
                      checkboxInput(inputId = "createHazardCoxPlot", label = "Create Plot", value = FALSE)
                      
                    )),

                    conditionalPanel(condition = "input.createHazardCoxPlot",

                        highcharter::highchartOutput("hazardErrorbar")

                      ),


                    #fluidRow(
                    #  column(5, DT::dataTableOutput('hazardRatioResultCox')),
                    #  column(7, plotOutput("hazardErrorbar"))
                    #),

                    h4(textOutput(outputId = "goodnessOfFitTestsText")),
                    DT::dataTableOutput('goodnessOfFitTestsRes'),

                    h4(textOutput(outputId = "analysisOfDevianceCox")),
                    DT::dataTableOutput('analysisOfDevianceRes'),

                    h4(textOutput(outputId = "storePredictionsCox")),
                    DT::dataTableOutput('predictionsCox'),

                    h4(textOutput(outputId = "residualsCoxText")),
                    DT::dataTableOutput('residualsCox'),

                    h4(textOutput(outputId = "martingaleResidualsCoxText")),
                    DT::dataTableOutput('martingaleResidualsCox'),

                    h4(textOutput(outputId = "schoenfeldResidualsCoxText")),
                    DT::dataTableOutput('schoenfeldResidualsCox'),

                    h4(textOutput(outputId = "dfBetasCoxText")),
                    DT::dataTableOutput('dfBetasCox')

                  ),

                tabPanel('Proportional Hazard Assumption',

                  h4(textOutput(outputId = "phAssumptionCoxText")),
                  DT::dataTableOutput('phAssumptionCox'),

                  br(),

                  conditionalPanel(condition = "input.runCox",

                      fluidRow(
                            column(4, selectizeInput(inputId = "selectPlotCox", label = "Select a plot", choices = c("Schoenfeld" = 1, "Log-Minus-Log" = 2), selected = NULL)),
                            column(4, selectizeInput(inputId = "selectVariablesForSchoenfeld", label = "Select a variable ", choices = NULL, selected = NULL)),
                            column(4, actionButton(inputId = "createSchoenfeldPlot", label = "Create plot", icon = icon("signal", lib = "glyphicon")))
                      )   
                  ),

                  conditionalPanel(condition = "input.createSchoenfeldPlot" ,
 
                          h4(textOutput(outputId = "phPlotsText")),
                          highcharter::highchartOutput('phPlots', width = "100%", height = "525px"),


                          conditionalPanel(condition = "input.createSchoenfeldPlot" ,
                      
                          checkboxInput(inputId = "phPlotOptions", label = "Edit Plot", value = FALSE)
                      
                        ),
                        # KM Options
                        conditionalPanel(condition = "input.phPlotOptions && input.selectPlotCox == 2" ,
                           
                          fluidRow(
                              column(4,textInput("mainPanelLmlSchoenfeld", "Main Title", value = "Log-Minus-Log Plot")),
                              column(4,textInput("xlabLmlSchoenfeld", "X-axis Label", value = "Time")),
                              column(4,textInput("ylabLmlSchoenfeld", "Y-axis Label", value = "log(-log(survival))"))
                            ),

                            #fluidRow(
                            #  column(4, checkboxInput("addCensLmlSchoenfeld", "Add censored cases", value = TRUE)),
                            #  
                            #  column(4,selectizeInput("censShapeLmlSchoenfeld", "Select censored case shape",
                            #                          choices = c("\U002B" = "plus", "\U25EF" = "circle", "\U25B3" = "triangle",  "\U25A2" = "square", "\U2573" = "cross"),
                            #                          selected = "plus")),
                            #  column(4,shinyjs::colourInput("censColLmlSchoenfeld", "Choose censored cases color", value = "black"))
                            #),


                            fluidRow(
                              column(4,selectizeInput("ltyLmlSchoenfeld", "LmlSchoenfeld curve line type",
                                                      choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "Solid",
                                                                  "\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "ShortDash",
                                                                  "\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "ShortDot",
                                                                  "\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "ShortDashDot",
                                                                  "\U2500 \U2500 \U2500 \U2500 \U2500" = "LongDash",
                                                                  "\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "LongDashShortDash"),
                                                      selected = "Solid")
                              ),
                              
                              column(4,checkboxInput(inputId = "addCILmlSchoenfeld", label = "Add Confidence Interval", value = FALSE)),
                              
                              column(4, sliderInput("alphaLmlSchoenfeld", "Confidence Interval Opacity", min = 0, max = 1, value = 0.5, step= 0.1))
                            ),



                            fluidRow(
                              column(4, shinyjs::colourInput("backgroundSchoenfeldLml", "Plot background color", value = "white")),
                              
                              column(4, selectInput("changeThemeSchoenfeldLml", "Select a theme", choices = c("Default" = "theme0", "Five Thirty Eight" = "theme1", "Economist" = "theme2", 
                                                                                                    "Financial Times" = "theme3", "Dotabuff" = "theme4", "Flat" = "theme5", "Flat Dark" = "theme6", "Simple" = "theme7", 
                                                                                                    "Elementary" = "theme8", "Google" = "theme9", "Grid Light" = "theme10", "Sand Signika" = "theme11"), 
                                                    selected = "theme0"))
                            )

                          ), # End for KM Options
                  
                          # Hazard Plot Options
                        conditionalPanel(condition = "input.phPlotOptions && input.selectPlotCox == 1" ,
                            fluidRow(
                              

                              column(4, checkboxInput(inputId = "addCIschoenfeld", label = "Add Confidence Interval", value = FALSE)),

                              column(4, checkboxInput(inputId = "addResidual", label = "Add Residuals", value = TRUE)),


                              column(4,selectizeInput("ltySchoenfeld", "Line type",
                                  choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "Solid",
                                                       "\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "ShortDash",
                                                       "\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "ShortDot",
                                                       "\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "ShortDashDot",
                                                       "\U2500 \U2500 \U2500 \U2500 \U2500" = "LongDash",
                                                       "\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "LongDashShortDash"),
                                           selected = "Solid"))

                            ),



                            fluidRow( 
                              
                              column(4,shinyjs::colourInput("curveColSchoenfeld", "Choose line color", value = "#204BD9")),
                              column(4,shinyjs::colourInput("colSchoenfeldResiduals", "Choose color for residuals", value = "#000000")),
                              column(4,shinyjs::colourInput("colSchoenfeldCI", "Choose color for CI", value = "#59A819"))
                            ),

                            fluidRow(
                              column(4,textInput("mainPanelSchoenfeld", "Main Title", value = "Schoenfeld Plot")),
                              column(4,textInput("xlabSchoenfeld", "X-axis Label", value = "Time")),
                              column(4,textInput("ylabSchoenfeld", "Y-axis Label", value = "Scaled Schoenfeld residuals for "))
                            ),

                            fluidRow(
                              column(4, sliderInput("dfSchoenfeld", "Degrees of freedom for the spline", min = 2, max = 10, value = 4, step= 1)),
                              column(4, numericInput("nsmoPH", "Number of points for the lines", value = 40)),
                              column(4, sliderInput("alphaSchoenfeld", "Confidence Interval Opacity", min = 0, max = 1, value = 0.4, step= 0.1))
                              
                            ),

                            fluidRow(
                              column(4,shinyjs::colourInput("backgroundSchoenfeld", "Choose background color", value = "#FFFFFF")),
                              column(4, selectInput("changeThemeSchoenfeld", "Select a theme", choices = c("Default" = "theme0", "Five Thirty Eight" = "theme1", "Economist" = "theme2", 
                                "Financial Times" = "theme3", "Dotabuff" = "theme4", "Flat" = "theme5", "Flat Dark" = "theme6", "Simple" = "theme7", 
                                "Elementary" = "theme8", "Google" = "theme9", "Grid Light" = "theme10", "Sand Signika" = "theme11"), 
                              selected = "theme0"))
                              
                            )                            
                    ) 
                  )
                )
              )
            ),# End for Condition 3



            conditionalPanel(condition="input.selectAnalysis=='5'",

              tabsetPanel(
            
               tabPanel('Model',

                    DT::dataTableOutput('individualSurvivalPredictions'),
                    verbatimTextOutput('rf')
                )
              )
            ) # End for Condition 5


          ),

         tabPanel("Help",

           h4(tags$b('Usage of the tool:'), id = "usage"),
           h5(tags$b('1. Data upload'), id = "upload"),
           HTML('<p> Load your data set in *.txt file format using this tab.</p>'),

          HTML('<p>  
                  <ul>
                    <li>Rows must represent the observations and each column must represent the variables.</li>
                    <li>First row must be a header which indicates the variable names.</li>
                    </ul></p>'),

          HTML('<p><div align = "justify"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/dataUpload.jpg" width="400" height="300" border = "1000" alt="" ></td><td><img src="images/dataUpload2.jpg" width="550" height="300" border = "1000" alt=""></td></tr></table></div></p>'),

          br(),
           h5(tags$b('2. Analysis'), id = "analysis"),
            h5(tags$b('2.1. Life Table'), id = "lt"),
            HTML('<p>In order to perform Life Table analysis,</p>'),

            HTML('<p>  
                  <ol>
                    <li>Select the analysis method as <b>Life Table</b>.</li>
                    <li>Select suitable variables for the analysis, such as <b>survival time, status variable, category value for status variable and factor variable, if exists.</b></li>
                    <li>Define an appropriate <b>time interval</b> from beginning to end of study by a specific step.</li>
                    <li>In advanced options, one can change <b>confidence interval type</b>, as log, log-log or plain, <b>variance estimation method</b>, as Greenwood or Tsiatis, <b>comparison test type</b>, as Log-rank, Gehan-Breslow, Tarone-Ware, Peto-Peto, Modifi Peto-Peto or Flemington-Harrington, <b>confidence level</b> and <b>reference category</b>, as first or last.</li>
                    <li>Desired outputs can be selected by clicking <b>Outputs</b> checkbox. Available outputs are <b>case summary, life table, median life time, hazard ratio and comparison test</b>.</li>
                    <li>Click <b>Run</b> button to run the analysis.</li>

                  </ol></p>'),


            HTML('<p><div align = "justify"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/lifeTable.jpg" width="400" height="300" border = "1000" alt="" ></td><td><img src="images/lifeTable2.jpg" width="550" height="300" border = "1000" alt=""></td></tr></table></div></p>'),
          
          h5(tags$b('A short tutorial for life table analysis.')),
            HTML('<p>
               <video class="centre" width="620" height="440" controls>
                  <source src="images/lifeTable.mp4" type="video/mp4">
                  Your browser does not support the video tag.
                </video> 
                </p>'),

          br(),
            h5(tags$b('2.2. Kaplan-Meier'), id = "km"),

               HTML('<p>  
                  <ol>
                    <li>Select the analysis method as <b>Kaplan-Meier</b>.</li>
                    <li>Select suitable variables for the analysis, such as <b>survival time, status variable, category value for status variable and factor variable, if exists.</b></li>
                    <li>In advanced options, one can change <b>confidence interval type</b>, as log, log-log or plain, <b>variance estimation method</b>, as Greenwood or Tsiatis, <b>comparison test type</b>, as Log-rank, Gehan-Breslow, Tarone-Ware, Peto-Peto, Modifi Peto-Peto or Flemington-Harrington, <b>confidence level</b> and <b>reference category</b>, as first or last.</li>
                    <li>Desired outputs can be selected by clicking <b>Outputs</b> checkbox. Available outputs are <b>case summary, survival table, mean and median life time, hazard ratio and comparison test</b>.</li>
                    <li>Click <b>Run</b> button to run the analysis.</li>
                  </ol></p>'),

            HTML('<p><div align = "justify"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/km.jpg" width="500" height="300" border = "1000" alt="" ></td><td><img src="images/kmPlots2.jpg" width="450" height="300" border = "1000" alt=""></td></tr></table></div></p>'),
            
            HTML('<p>  
                  <ul>
                    <li>Two interactive plots can be created under <b>Plot</b> subtab: <b>Kaplan-Meier plot and Hazard plot</b> </li>
                    <li>A number of options available for plot editing.</li>
                  </ul></p>'),


            HTML('<p><div align = "justify"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/kmPlots.jpg" width="500" height="300" border = "1000" alt="" ></td><td><img src="images/kmPlots3.jpg" width="450" height="300" border = "1000" alt=""></td></tr></table></div></p>'),

            h5(tags$b('A short tutorial for Kaplan-Meier analysis.')),
            HTML('<p>
               <video class="centre" width="620" height="440" controls>
                  <source src="images/kaplanMeier.mp4" type="video/mp4">
                  Your browser does not support the video tag.
                </video> 
                </p>'),

         
          br(),
            h5(tags$b('2.3. Cox Regression'), id = "cox"),

                HTML('<p>  
                  <ol>
                    <li>Select the analysis method as <b>Cox Regression</b>.</li>
                    <li>Select suitable variables for the analysis, such as <b>survival time, status variable, category value for status variable, and categorical and continuous predictors for the model.</b></li>
                    <li>In advanced options, interaction terms, strata terms and time dependent covariates can be added to the model. Furthermore, once can choose model selection criteria, as AIC or p-value, model selection method, as backward, forward or stepwise, reference category, as first or last, and ties method, as Efron, Breslow or exact.</li>
                    <li>Desired outputs can be selected by clicking <b>Outputs</b> checkbox. Available outputs are <b>coefficient estimates, hazard ratio, goodness of fit tests, analysis of deviance, predictions, residuals, Martingale residuals, Schoenfeld residuals and DfBetas</b>.</li>
                    <li>Click <b>Run</b> button to run the analysis.</li>
                  </ol></p>'), 

            HTML('<p><div align = "justify"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/coxReg.jpg" width="500" height="300" border = "1000" alt="" ></td><td><img src="images/coxReg2.jpg" width="450" height="300" border = "1000" alt=""></td></tr></table></div></p>'),
                HTML('<p>  
                  <ul>
                    <li>In order to test proportional hazard assumption: </li>

                    </ul></p>'),
              HTML('<p>  
                  <ol>
                    <li>A global test can be run</li>
                    <li>interactive plots, such as Schoenfeld residual plot and Log-Minus-Log plot can be created.</li>

                  </ol></p>'),

            HTML('<p><div align = "justify"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/coxReg3.jpg" width="450" height="300" border = "1000" alt="" ></td><td><img src="images/coxReg4.jpg" width="500" height="300" border = "1000" alt=""></td></tr></table></div></p>'),


            h5(tags$b('A short tutorial for Cox regression analysis.')),
            HTML('<p>
               <video class="centre" width="620" height="440" controls>
                  <source src="images/coxRegression.mp4" type="video/mp4">
                  Your browser does not support the video tag.
                </video> 
                </p>')

         ),
             
         tabPanel("Authors",
#br(),
                HTML('<p><a href="http://yunus.hacettepe.edu.tr/~selcuk.korkmaz/" target="_blank"> <b>Selcuk Korkmaz</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:selcukorkmaz@gmail.com" target="_blank">selcukorkmaz@gmail.com</a><p>'),
                HTML('<p><a href="http://yunus.hacettepe.edu.tr/~dincer.goksuluk" target="_blank"> <b>Dincer Goksuluk</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:dincer.goksuluk@gmail.com" target="_blank">dincer.goksuluk@gmail.com</a><p>'),
                #HTML('<br>'),
                #h4("Contributors"),
                #h5("Gokmen Zararsiz"),
                HTML('<p><a href="http://gokmenzararsiz.simplesite.com" target="_blank"> <b>Gokmen Zararsiz</b></a><p>'),
                HTML('<p>Erciyes University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:gokmenzararsiz@hotmail.com" target="_blank">gokmenzararsiz@hotmail.com</a><p>')          
         )

    
     ),


    tags$head(
  
   tags$style("body {padding-top: 58px};")

    ),

    tags$head(
       
        tags$link(rel = "shortcut icon", href = "images/favicon.ico")
     
     )

   )
  )
)#)




