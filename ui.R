library("shinythemes")
library("shinyBS")
library("highcharter")
library("knitr")


#rmdfiles <- c("help.rmd")
#sapply(rmdfiles, knit, quiet = T)

shinyUI(
  fluidPage(
    theme =  "css/mytheme.css",
    sidebarPanel(width=3,


                 ################################# About (start) #####################################

                 conditionalPanel(condition="input.tabs1=='About'",

                                  HTML('<center><img src="images/logoSurv.png" width=200 height=200></center>')

                 ),

                 conditionalPanel(condition="input.tabs1=='Authors'",

                                  HTML('<center><img src="images/team.png" width=200 height=200></center>')

                 ),
                 
                 conditionalPanel(condition="input.tabs1=='Citation'",
                                  
                                  HTML('<center><img src="images/cite.png" width=200 height=200></center>')
                                  
                 ),

                 #conditionalPanel(condition="input.tabs1=='Help'"

                 # HTML('<img src="images/help.png" width=300 height=200>'),



                 #),

                 ################################# About (end) #####################################
                 ################################# Data Upload (start) #####################################


                 conditionalPanel(condition="input.tabs1=='Data Upload'",

                                  h4("Input data"),
                                  radioButtons("dataInput", "", list("Upload a file" = 2, "Load example data" = 1), selected=2),

                                  conditionalPanel(condition="input.dataInput=='1'",
                                                   h5("Example dataset:"),
                                                   radioButtons("sampleData", "", list("GSE2034"=1), selected=1)
                                  ),

                                  conditionalPanel(condition="input.dataInput=='2'",
                                                   h5("Upload a delimited text file: "),

                                                   fileInput("upload", "", multiple = FALSE),

                                                   radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2),

                                                   HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
                                                   HTML('<p>Note: First row must be header.</p>')
                                  ),

                                  br(),

                                  h4("Data Pre-processing"),



                                  checkboxInput("preProcess", "Pre-processing",value = FALSE),


                                  conditionalPanel(condition="input.preProcess",
                                  selectizeInput("survivalTimeProcess", "Survival time", choices = NULL, multiple = FALSE),

                                  selectizeInput("statusVariableProcess", "Status variable", choices = NULL, multiple = FALSE),

                                  selectizeInput("excludeVarsProcess", "Exclude variable(s)", choices = NULL, multiple = TRUE),


                                    checkboxInput("nearZero", "Near zero filtering",value = FALSE),

                                    checkboxInput("center", "Centering", value = FALSE),
                                    checkboxInput("scale", "Scaling", value = FALSE),
                                    checkboxInput("transform", "Log-transformation", value = FALSE),

                                  actionButton(inputId = "runProcess", label = "Run pre-process", icon = icon("play", lib = "glyphicon"))

                                    # checkboxInput("varFilter", "Variance filtering",value = FALSE)
                                    #
                                    # conditionalPanel(condition = "input.varFilter",
                                    #                  numericInput(inputId = "maxVar", label = "Number of genes with maximum variance", value = "")
                                    # )
                                    #
                                  ),

                                  br(),

                                  actionButton(inputId = "helpDataUpload", label = "Help", icon = icon("question-sign", lib = "glyphicon"),
                                               onclick ="window.open('help/dataUpload.html', '_blank')")



                 ),
                 ################################# Data Upload (end) #####################################

                 ########## Analysis (start) #####################################


                 conditionalPanel(condition="input.tabs1=='Analysis'",


                                  selectizeInput(inputId = "selectAnalysis", label = "Select an Analysis Method", choices = c("Kaplan-Meier" = 2, "Cox Regression" = 3, "Penalized Cox Regression" = 4, "Random Survival Forests" = 5, "Optimal Cutoff" = 6), selected = 1),

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

                                                                                     #selectizeInput("comparisonTest", "Comparison test", choices = list("Log-rank" = "logRank", "Gehan-Breslow" = "gehanBreslow", "Tarone-Ware" = "taroneWare", "Peto-Peto" = "petoPeto", "Modified Peto-Peto" = "modPetoPeto", "Flemington-Harrington" = "flemingtonHarnington"), multiple = FALSE),

                                                                                     h5(tags$b("Flemington-Harrington Weights")),
                                                                                     fluidRow(column(4,numericInput("pLT", "p", value = 1)),
                                                                                              column(4,numericInput("qLT", "q", value = 1))
                                                                                     ),

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
                                                   #tags$head(
                                                   #  tags$style(HTML('#run{background-color:green}'))
                                                   #),

                                                   actionButton(inputId = "run",  label = "Run", icon = icon("play", lib = "glyphicon"))

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

                                                                                     #selectizeInput("comparisonTestKM", "Comparison test", choices = list("Log-rank" = "logRank", "Gehan-Breslow" = "gehanBreslow", "Tarone-Ware" = "taroneWare", "Peto-Peto" = "petoPeto", "Modified Peto-Peto" = "modPetoPeto", "Flemington-Harrington" = "flemingtonHarnington"), multiple = FALSE),

                                                                                     h5(tags$b("Flemington-Harrington Weights")),
                                                                                     fluidRow(column(6,numericInput("pKM", "p", value = 1)),
                                                                                              column(6,numericInput("qKM", "q", value = 1))
                                                                                     ),

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


                                                   fluidRow(column(4,actionButton(inputId = "helpKaplanMeier", label = "Help", icon = icon("question-sign", lib = "glyphicon"),
                                                                                  onclick ="window.open('help/kaplanMeier.html', '_blank')")),
                                                            column(4,actionButton(inputId = "runKM", label = "Run", icon = icon("play", lib = "glyphicon"))))






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


                                                   fluidRow(column(4,actionButton(inputId = "helpCoxRegression", label = "Help", icon = icon("question-sign", lib = "glyphicon"),
                                                                                  onclick ="window.open('help/coxRegression.html', '_blank')")),
                                                            column(4,actionButton(inputId = "runCox", label = "Run", icon = icon("play", lib = "glyphicon"))))

                                  ),

                                  conditionalPanel(condition="input.selectAnalysis=='4'",

                                                   checkboxInput(inputId = "inputsrCox", label = tags$b("Inputs"), value = TRUE),

                                                   conditionalPanel(condition = "input.inputsrCox",


                                                                    selectizeInput("survivalTimerCox", "Survival time", choices = NULL, multiple = FALSE),

                                                                    selectizeInput("statusVariablerCox", "Select status variable", choices = NULL, multiple = FALSE),

                                                                    selectizeInput("statusrCox", "Select category for status variable", choices = NULL, multiple = FALSE),

                                                                    h5(tags$b("Variables for selection")),
                                                                    checkboxInput("selectAllVarsrCox", "Include all variables", value = TRUE),

                                                                    conditionalPanel(condition="!input.selectAllVarsrCox",

                                                                                     selectizeInput("categoricalVariablerCox", "Select categorical variables", choices = NULL, multiple = TRUE),
                                                                                     selectizeInput("numericalVariablerCox", "Select continuous variables", choices = NULL, multiple = TRUE)

                                                                    ),

                                                                    sliderInput("rAlpha", "Penalty term", value = 1, min = 0, max = 1, step = 0.1),
                                                                    bsTooltip(id = "rAlpha", title = "0 for ridge penalty, 1 for lasso penalty, (0,1) for elastic net.", placement = "bottom", trigger = "hover"),

                                                                    numericInput("nFold", "Number of folds for cross-validation", value = 10, min = 3, step = 1),


                                                                    fluidRow(column(4,actionButton(inputId = "helpCoxRegression", label = "Help", icon = icon("question-sign", lib = "glyphicon"),
                                                                                                   onclick ="window.open('help/penalizedCoxRegression.html', '_blank')")),
                                                                             column(4,actionButton(inputId = "runRegularized", label = "Run", icon = icon("play", lib = "glyphicon"))))



                                                   )
                                  ),

                                  conditionalPanel(condition="input.selectAnalysis=='5'",

                                                   checkboxInput(inputId = "inputsRF", label = tags$b("Inputs"), value = TRUE),

                                                   conditionalPanel(condition = "input.inputsRF",


                                                                    selectizeInput("survivalTimeRF", "Survival time", choices = NULL, multiple = FALSE),

                                                                    selectizeInput("statusVariableRF", "Select status variable", choices = NULL, multiple = FALSE),

                                                                    selectizeInput("statusRF", "Select category for status variable", choices = NULL, multiple = FALSE),

                                                                    checkboxInput("selectAllVarsRF", "Include all variables", value = TRUE),

                                                                    conditionalPanel(condition="!input.selectAllVarsRF",

                                                                        selectizeInput("categoricalInputRF", "Categorical variable(s)", choices = NULL, multiple = TRUE),

                                                                        selectizeInput("continuousInputRF", "Continuous variable(s)", choices = NULL, multiple = TRUE)

                                                                    ),




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

                                                                                     checkboxInput(inputId = "moreOptionsRF", label = "RSF options", value = FALSE),

                                                                                     conditionalPanel(condition = "input.moreOptionsRF",

                                                                                                      numericInput("ntree", "Number of tree", value = 100),
                                                                                                      selectInput("bootstrap", "Bootstrap method", choices = list("Root" = "by.root", "Node" = "by.node"), selected = "by.root"),
                                                                                                      numericInput("mtry", "Randomly selected number of variables", value = 5),
                                                                                                      numericInput("nodesize", "Minimum number of cases in terminal node", value = 3),
                                                                                                      numericInput("nodedepth", "Maximum depth for a tree", value = NULL),
                                                                                                      selectInput("splitrule", "Splitting rule", choices = list("Log-rank" = "logrank", "Log-rank score" = "logrankscore"), selected = "logrank"),
                                                                                                      numericInput("nsplit", "Number of split", value = 1),
                                                                                                      selectInput("naAction", "Missing values", choices = list("Remove" = "na.omit", "Impute" = "na.impute"), selected = "na.omit"),
                                                                                                      numericInput("nimpute", "Number of iterations of the missing data algorithm", value = 1),
                                                                                                      selectInput("proximity", "Proximity of cases", choices = list("Inbag" = "inbag", "OOB" = "oob", "All" = "all", "None" = FALSE), selected = "inbag"),
                                                                                                      numericInput("sampsize", "Size of bootstrap", value = NA),
                                                                                                      selectInput("samptype", "Type of bootstrap", choices = list("With replacement" = "swr", "Without replacement" = "swor"), selected = "swr"),


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
                                                                    checkboxInput(inputId = "survivalResultRF", label = "Survival", value = TRUE),
                                                                    checkboxInput(inputId = "survivalResultOobRF", label = "Survival OOB", value = TRUE),
                                                                    checkboxInput(inputId = "chRF", label = "Cumulative Hazard", value = TRUE),
                                                                    checkboxInput(inputId = "chOobRF", label = "Cumulative Hazard OOB", value = FALSE),
                                                                    checkboxInput(inputId = "errorRateRF", label = "Error rate", value = FALSE),
                                                                    checkboxInput(inputId = "varImp", label = "Feature Selection", value = FALSE),

                                                                    conditionalPanel(condition = "input.varImp",

                                                                                     numericInput("dropOutRate", "Drop rate", value = 0.2, min = 0, max = 1)
                                                                    )

                                                   ),

                                                   fluidRow(column(4,actionButton(inputId = "helpCoxRegression", label = "Help", icon = icon("question-sign", lib = "glyphicon"),
                                                                                  onclick ="window.open('help/randomForestSurvival.html', '_blank')")),
                                                            column(4,actionButton(inputId = "runRF", label = "Run", icon = icon("play", lib = "glyphicon"))))


                                  ),



                                  conditionalPanel(condition="input.selectAnalysis=='6'",
                                                   selectizeInput("selectMarkers", "Select marker(s)", choices = NULL, multiple = TRUE),
                                                   #checkboxInput(inputId = "higherValues", label = "Higher values indicate risks", value = TRUE),
                                                   selectizeInput("survivalTimeCutoff", "Survival time", choices = NULL, multiple = FALSE),
                                                   selectizeInput("statusVariableCutoff", "Select status variable", choices = NULL, multiple = FALSE),
                                                   selectizeInput("statusCutoff", "Select category for status variable", choices = NULL, multiple = FALSE),
                                                   selectizeInput("selectTestCutoff", "Select a test for cutoff", choices = c("Log-rank" = "logRank", "Gehan-Breslow" = "gehanBreslow",
                                                                                                                              "Tarone-Ware" = "taroneWare", "Peto-Peto" = "petoPeto", "Modified Peto-Peto" = "modifiedPetoPeto", "Flemington-Harrington" = "flemingtonHarrington"), multiple = FALSE),

                                                   conditionalPanel(condition = "input.selectTestCutoff == 'flemingtonHarrington'",

                                                                    h5(tags$b("Flemington-Harrington Weights")),
                                                                    fluidRow(column(4,numericInput("pLTCutoff", "p", value = 1)),
                                                                             column(4,numericInput("qLTCutoff", "q", value = 1))

                                                                    )
                                                   ),



                                                   checkboxInput(inputId = "advancedForCutoff", label = "Advanced options", value = FALSE),

                                                   conditionalPanel(condition = "input.advancedForCutoff",


                                                                    selectizeInput("ciForCutoff", "Confidence interval type", choices = list("Log" = "log", "Log-Log" = "log-log", "Plain" = "plain"), multiple = FALSE),

                                                                    selectizeInput("varianceEstimationForCutoff", "Variance estimation", choices = list("Greenwood" = "greenwood", "Tsiatis" = "tsiatis"), multiple = FALSE),

                                                                    numericInput("CLcutoff",label = "Confidence level",value = 95, min=1, max=100)

                                                   ),

                                                   fluidRow(column(4,actionButton(inputId = "helpCoxRegression", label = "Help", icon = icon("question-sign", lib = "glyphicon"),
                                                                                  onclick ="window.open('help/cutoffValue.html', '_blank')")),
                                                            column(4,actionButton(inputId = "runCutoff", label = "Run", icon = icon("play", lib = "glyphicon"))))


                                  )


                 )


                 ########## Analysis (end) #####################################

    ),




    mainPanel(

      navbarPage("geneSurv: Survival Analysis for Genomics", id="tabs1", inverse = TRUE, collapsible = TRUE, fluid = TRUE, position = "fixed-top", class("navbar navbar-inverse"),



                 tabPanel("About",

                          #br(),
                          h4(tags$b('An interactive tool for survival analysis in genomics research')),

                          HTML('<p align="justify">Survival analysis is often used in cancer studies. It has been shown that combination of clinical data with genomics increases predictive performance of survival analysis methods.
                               This tool provides a wide range of survival analysis methods for genomic research, especially in cancer studies. The tool includes analysis methods including Kaplan-Meier, Cox regression, Penalized Cox regression and Random Survival Forests. It also offers methods for optimal cutoff point determination for continuous markers. </p>'),

                          HTML('<p align="justify">Each procedure includes following features:</p>'),

                          HTML('<p align="justify"> <b> Kaplan-Meier:</b> descriptive statistics, survival table, mean and median life time, hazard ratios, comparison tests
                               including Log-rank, Gehan-Breslow, Tarone-Ware, Peto-Peto, Modified Peto-Peto, Flemington-Harrington, and interactive plots such as Kaplan-Meier curves and hazard plots.</p>'),

                          HTML('<p align="justify"> <b> Cox regression:</b> coefficient estimates, hazard ratios, goodness of fit test, analysis of deviance, save predictions, save residuals,
                               save Martingale residuals, save Schoenfeld residuals, save dfBetas, proportional hazard assumption test, and interactive plots including Schoenfeld residual plot and Log-Minus-Log plot.</p>'),

                          HTML('<p align="justify"> <b> Penalized Cox regression:</b> feature selection using ridge, elastic net and lasso penalization. A cross-validation to investigate the relationship between partial likelihood devaince and lambda values.</p>'),

                          HTML('<p align="justify"> <b> Random survival forests:</b> individual survival predictions, individual cumulative hazard predictions, error rate, variable importance, and interactive plots including random survival plot, cumulative hazard plot, error rate plot, Cox vs RSF plot</p>'),

                          HTML('<p align="justify"> <b> Optimal cutoff:</b> determination of optimal cutoff value by maxmizing test statistics, including log-rank, Gehan-Breslow, Tarone-Ware, Peto-Peto, modified Peto-Peto, Flemington-Harrington.</p>'),


                          HTML('<p align="justify"> All source codes are in <a href="https://github.com/selcukorkmaz/compSurv" target="_blank"><b>GitHub</b></a>. Please see the <a href="help/help.html" target="_blank"> <b>help page</b></a> for more detailed information.</p>'),

                          HTML('<p><div align = "center"><table cellpadding="0" cellspacing="0"><tr><td><img src="images/kmPlot.jpg" width="300" height="200" ></td><td><img src="images/schoenfeldPlot.jpg" width="300" height="200"></td><td><img src="images/lmlPlot.jpg" width="300" height="200"></td></tr></table></div></p>'),

                          HTML('<p><b>License:</b> This web-tool as a whole is distributed under GPL-3 (General Public License Version 3).</p>'),

                          HTML('<p><b>Disclaimer:</b> This server is intended for research purposes only, not for clinical or commercial use. It is a non-profit service to the scientific community, provided on an "AS-IS " basis without any warranty, expressed or implied. The authors can not be held liable in any way for the service provided here.</p>')

                          #HTML('<center><img src="images/images.jpg" width=800 height=250></center>')



                          ),

                 tabPanel("Data Upload",

                          DT::dataTableOutput('dataUpload'),
                          br(),
                          h4(textOutput(outputId = "newDataset")),
                          DT::dataTableOutput("nearZeroFeaturesResult")


                 ),

                 tabPanel("Analysis",


                          conditionalPanel(condition="input.selectAnalysis=='1'",


                                           tabsetPanel(

                                             tabPanel('Life Table Results',

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

                                             tabPanel('Kaplan-Meier Results',
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

                                                            conditionalPanel(condition = "input.factorVarKM",
                                                                       selectizeInput(inputId = "selectGroup", label = "Select a factor group", choices = NULL, selected = NULL)
                                                            ),
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

                                                                     conditionalPanel(condition = "input.factorVarKM",

                                                                         selectizeInput(inputId = "selectGroupHazard", label = "Select a factor group", choices = NULL, selected = NULL)

                                                                    ),
                                                                       highcharter::highchartOutput('hazardPlot')

                                                      ),


                                                      h4(textOutput(outputId = "compTestTextKM")),
                                                      DT::dataTableOutput('comparisonTestResultsKM')

                                             ),

                                             tabPanel('Plots',

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

                                                                         column(4, selectInput("changeTheme", "Select a theme", choices = c("Default" = "theme0", "Theme1" = "theme1", "Theme2" = "theme2",
                                                                                                                                            "Theme3" = "theme3", "Theme4" = "theme4", "Theme5" = "theme5", "Theme6" = "theme6", "Theme7" = "theme7",
                                                                                                                                            "Theme8" = "theme8", "Theme9" = "theme9", "Theme10" = "theme10", "Theme11" = "theme11"),
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

                                                                         column(4, selectInput("changeThemeHazard", "Select a theme", choices = c("Default" = "theme0", "Theme1" = "theme1", "Theme2" = "theme2",
                                                                                                                                                  "Theme3" = "theme3", "Theme4" = "theme4", "Theme5" = "theme5", "Theme6" = "theme6", "Theme7" = "theme7",
                                                                                                                                                  "Theme8" = "theme8", "Theme9" = "theme9", "Theme10" = "theme10", "Theme11" = "theme11"),
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

                                                                         column(4, selectInput("changeThemeLml", "Select a theme", choices = c("Default" = "theme0", "Theme1" = "theme1", "Theme2" = "theme2",
                                                                                                                                               "Theme3" = "theme3", "Theme4" = "theme4", "Theme5" = "theme5", "Theme6" = "theme6", "Theme7" = "theme7",
                                                                                                                                               "Theme8" = "theme8", "Theme9" = "theme9", "Theme10" = "theme10", "Theme11" = "theme11"),
                                                                                               selected = "theme0"))
                                                                       )
                                                      ) #End for LML options

                                             ) # End for Plot

                                           )  # End for Tabset Panel

                          ), # End for Condition 2




                          conditionalPanel(condition="input.selectAnalysis=='3'",

                                           tabsetPanel(

                                             tabPanel('Cox Regression Results',

                                                      #verbatimTextOutput("str"),
                                                      h4(textOutput(outputId = "displaySummaryCox")),
                                                      # verbatimTextOutput("summaryCox"),

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

                                                                                          column(4, selectInput("changeThemeSchoenfeldLml", "Select a theme", choices = c("Default" = "theme0", "Theme1" = "theme1", "Theme2" = "theme2",
                                                                                                                                                                          "Theme3" = "theme3", "Theme4" = "theme4", "Theme5" = "theme5", "Theme6" = "theme6", "Theme7" = "theme7",
                                                                                                                                                                          "Theme8" = "theme8", "Theme9" = "theme9", "Theme10" = "theme10", "Theme11" = "theme11"),
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
                                                                                          column(4, selectInput("changeThemeSchoenfeld", "Select a theme", choices = c("Default" = "theme0", "Theme1" = "theme1", "Theme2" = "theme2",
                                                                                                                                                                       "Theme3" = "theme3", "Theme4" = "theme4", "Theme5" = "theme5", "Theme6" = "theme6", "Theme7" = "theme7",
                                                                                                                                                                       "Theme8" = "theme8", "Theme9" = "theme9", "Theme10" = "theme10", "Theme11" = "theme11"),
                                                                                                                selected = "theme0"))

                                                                                        )
                                                                       )
                                                      )
                                             )
                                           )
                          ),# End for Condition 3


                          conditionalPanel(condition="input.selectAnalysis=='4'",

                                           tabsetPanel(

                                             tabPanel('Penalized Cox Regression Results',

                                                      h4(textOutput(outputId = "varInModelText")),
                                                      DT::dataTableOutput('regularCox'),
                                                      #verbatimTextOutput('regularCoxResult')
                                                      h4(textOutput(outputId = "crossValCurvePlot")),
                                                      highchartOutput('regularizedPlot'),
                                                      h4(textOutput(outputId = "varNotInModelText")),
                                                      DT::dataTableOutput('regularNoVariableCox')

                                             ))

                          ), ## End for condition 4

                          conditionalPanel(condition="input.selectAnalysis=='5'",

                                           tabsetPanel(

                                             tabPanel('Random Survival Forests Results',


                                                      #verbatimTextOutput("rf"),

                                                      h4(textOutput(outputId = "indSurvPreds")),
                                                      DT::dataTableOutput('survival'),

                                                      h4(textOutput(outputId = "indSurvPredsOOB")),
                                                      DT::dataTableOutput('survivalOOB'),

                                                      h4(textOutput(outputId = "indChfPreds")),
                                                      DT::dataTableOutput('chf'),

                                                      h4(textOutput(outputId = "indChfPredsOOB")),

                                                      DT::dataTableOutput('chfOOB'),

                                                      h4(textOutput(outputId = "errorRateText")),
                                                      DT::dataTableOutput('errorRate'),

                                                      h4(textOutput(outputId = "varImpText")),

                                                      DT::dataTableOutput('variableImportance'),
                                                      highchartOutput('variableImportancePlot')

                                             ),

                                             tabPanel('Plots',


                                                      selectInput("selectRFPlot", "Select a plot", choices = list("Overall Survival" = 7, "Individual Survival" = 1, "Individual Survival OOB" = 2, "Individual Cumulative Hazard" = 3, "Individual Cumulative Hazard OOB" = 4, "Error rate" = 5, "Cox vs RSF" = 6), selected = 7),

                                                      conditionalPanel(condition = "input.selectRFPlot == 1" ,
                                                                       selectInput("selectObs", "Select cases for survival curves", choices = list("All" = 1, "Range" = 2, "Custom" = 3), selected = 1),

                                                                       conditionalPanel(condition = "input.selectObs == 2" ,
                                                                                        fluidRow(column(6,numericInput("fromRSF", "From", value = 1)),
                                                                                                 column(6,numericInput("toRSF", "To", value = 2)))

                                                                       ),

                                                                       conditionalPanel(condition = "input.selectObs == 3" ,

                                                                                        selectizeInput("customSelect", "Select cases", choices = NULL, multiple = TRUE)

                                                                       )
                                                      ),

                                                      conditionalPanel(condition = "input.selectRFPlot == 2" ,
                                                                       selectInput("selectObsSurvOOB", "Select cases for survival curves", choices = list("All" = 1, "Range" = 2, "Custom" = 3), selected = 1),

                                                                       conditionalPanel(condition = "input.selectObsSurvOOB == 2" ,
                                                                                        fluidRow(column(6,numericInput("fromRSFOOB", "From", value = 1)),
                                                                                                 column(6,numericInput("toRSFOOB", "To", value = 2)))

                                                                       ),

                                                                       conditionalPanel(condition = "input.selectObsSurvOOB == 3" ,

                                                                                        selectizeInput("customSelectOOB", "Select cases", choices = NULL, multiple = TRUE)

                                                                       )
                                                      ),

                                                      conditionalPanel(condition = "input.selectRFPlot == 3" ,
                                                                       selectInput("selectObsHazard", "Select cases for hazard curves", choices = list("All" = 1, "Range" = 2, "Custom" = 3), selected = 1),

                                                                       conditionalPanel(condition = "input.selectObsHazard == 2" ,
                                                                                        fluidRow(column(6,numericInput("fromHazard", "From", value = 1)),
                                                                                                 column(6,numericInput("toHazard", "To", value = 2)))

                                                                       ),

                                                                       conditionalPanel(condition = "input.selectObsHazard == 3" ,

                                                                                        selectizeInput("customSelectHazard", "Select cases", choices = NULL, multiple = TRUE)

                                                                       )
                                                      ),

                                                      conditionalPanel(condition = "input.selectRFPlot == 4" ,
                                                                       selectInput("selectObsHazardOOB", "Select cases for hazard curves", choices = list("All" = 1, "Range" = 2, "Custom" = 3), selected = 1),

                                                                       conditionalPanel(condition = "input.selectObsHazardOOB == 2" ,
                                                                                        fluidRow(column(6,numericInput("fromHazardOOB", "From", value = 1)),
                                                                                                 column(6,numericInput("toHazardOOB", "To", value = 2)))

                                                                       ),

                                                                       conditionalPanel(condition = "input.selectObsHazardOOB == 3" ,

                                                                                        selectizeInput("customSelectHazardOOB", "Select cases", choices = NULL, multiple = TRUE)

                                                                       )
                                                      ),

                                                      actionButton("createRSFplot", "Create plot"),

                                                      highcharter::highchartOutput('rsfPlot')

                                             )
                                           )
                          ), # End for Condition 5

                          conditionalPanel(condition="input.selectAnalysis=='6'",

                                           tabsetPanel(

                                             tabPanel('Optimal Cutoff Results',

                                                      DT::dataTableOutput('optimalCutoffResult')


                                             ),

                                             tabPanel('KM Curves',

                                                      br(),

                                                      fluidRow(
                                                        column(4,selectizeInput(inputId = "selectMarkerForCutoff", label = "Select a marker", choices = NULL, selected = NULL)),
                                                        column(4,actionButton(inputId = "createKmCutoffPlot", label = "Create Plot", icon = icon("signal", lib = "glyphicon"))),


                                                        br(),

                                                        highcharter::highchartOutput('kmCurveCutoffPlot'),
                                                        # verbatimTextOutput("printData"),

                                                        br(),
                                                        br(),
                                                        checkboxInput(inputId = "kmPlotOptionsForCutoffs", label = "Edit Plot", value = FALSE),

                                                        conditionalPanel(condition = "input.kmPlotOptionsForCutoffs" ,

                                                                         fluidRow(
                                                                           column(4,textInput("mainPanelKMForCutoff", "Main Title", value = "Kaplan Meier Plot")),
                                                                           column(4,textInput("xlabKMForCutoff", "X-axis Label", value = "Time")),
                                                                           column(4,textInput("ylabKMForCutoff", "Y-axis Label", value = "Survival Probability"))
                                                                         ),


                                                                         # fluidRow(
                                                                         #   column(4, checkboxInput("addCensForCutoff", "Add censored cases", value = TRUE)),
                                                                         #
                                                                         #   column(4,selectizeInput("censShapeKMForCutoff", "Select censored case shape",
                                                                         #                           choices = c("\U002B" = "plus", "\U25EF" = "circle", "\U25B3" = "triangle",  "\U25A2" = "square", "\U2573" = "cross"),
                                                                         #                           selected = "plus")),
                                                                         #   column(4,shinyjs::colourInput("censColKMForCutoff", "Choose censored cases color", value = "black"))
                                                                         # ),
                                                                         #

                                                                         fluidRow(
                                                                           column(4,selectizeInput("ltyKMForCutoff", "KM curve line type",
                                                                                                   choices = c("\U2500\U2500\U2500\U2500\U2500\U2500\U2500" = "Solid",
                                                                                                               "\U2574 \U2574 \U2574 \U2574 \U2574 \U2574" = "ShortDash",
                                                                                                               "\U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7 \U00B7" = "ShortDot",
                                                                                                               "\U2574 \U00B7 \U2574 \U00B7 \U2574 \U00B7 \U2574" = "ShortDashDot",
                                                                                                               "\U2500 \U2500 \U2500 \U2500 \U2500" = "LongDash",
                                                                                                               "\U2500 \U2574 \U2500 \U2574 \U2500 \U2574" = "LongDashShortDash"),
                                                                                                   selected = "Solid")
                                                                           ),

                                                                           column(4,checkboxInput(inputId = "addCIForCutoff", label = "Add Confidence Interval", value = FALSE)),

                                                                           column(4, sliderInput("alphaForCutoff", "Confidence Interval Opacity", min = 0, max = 1, value = 0.5, step= 0.1))
                                                                         ),



                                                                         fluidRow(
                                                                           column(4, shinyjs::colourInput("backgroundKMForCutoff", "Plot background color", value = "white")),

                                                                           column(4, selectInput("changeThemeForCutoffs", "Select a theme", choices = c("Default" = "theme0", "Theme1" = "theme1", "Theme2" = "theme2",
                                                                                                                                                        "Theme3" = "theme3", "Theme4" = "theme4", "Theme5" = "theme5", "Theme6" = "theme6", "Theme7" = "theme7",
                                                                                                                                                        "Theme8" = "theme8", "Theme9" = "theme9", "Theme10" = "theme10", "Theme11" = "theme11"),
                                                                                                 selected = "theme0"))
                                                                         )

                                                        )

                                                      ))

                                           )

                          )


                 ),

                 #tabPanel("Help",

                 #shiny::includeMarkdown("help.md"),
                 #   HTML('<p>Please click here to reach the <a href="help.html" target="_blank"> <b>help page</b></a> </p>')

                 #uiOutput('help'),

                 #),

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
                 ),
                 
                 tabPanel("Citation",
                          #br(),
                          HTML('<p><b>To cite geneSurv in publications use:</b><p>'),
                          HTML('<p><b>Korkmaz S, Göksülük D, Zararsiz G, Karahan S (2017) "geneSurv: An Interactive Web-Based Tool for Survival Analysis in Genomics Research" Computers in Biology and Medicine (Accepted/In press).</b><p>')
                          
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




