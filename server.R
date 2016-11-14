shinyServer(function(input, output, session) {

 library("glmnet")
 library("DT")
 library("survival")
    library("KMsurv")
    library("survMisc")
    source("lifeTables.R")
    source("kaplanMeier.R")
    source("coxRegression.R")
    source("getDescriptiveResultsCoxRegression.R")
    source("stepwise.R")
    source("plotLT.R")
    require("ggplot2")
    source("ggsurv.R")
    source("ggsurv2.R")
    source("plotSchoenfeld.R")
    library("magrittr")
    library("dplyr")
    library("survminer")
    library("highcharter")
    library("randomForestSRC")
    library("pec")





   dataM <- reactive({  ## Data input.
       if(input$dataInput==1){  ## Load example data.
           
           if(input$sampleData == 1){
           
                data <- read.table("hmohiv.txt", header=TRUE, sep = "\t")
          
                data$time <- as.numeric(data$time)
           }
           
           else if(input$sampleData == 2){
               
               data <- read.table("Rossi.txt", header=TRUE, sep = "\t")

           }
       }
       
       else if(input$dataInput==2){  ## Upload data.
           
           inFile <- input$upload
           
           mySep <- switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="")
           
           if (is.null(input$upload))  {return(NULL)}
           
           if (file.info(inFile$datapath)$size <= 10485800){
               data <- read.table(inFile$datapath, sep=mySep, header=TRUE, fill=TRUE, na.strings = c("", "NA","."))
           }
           
           else print("File is bigger than 10MB and will not be uploaded.")
           
       }

       ind = complete.cases(data)
       return(data[ind,])

       #else {
       #    data[,dim(data)[2]] = as.factor(data[,dim(data)[2]])
       #    ind = complete.cases(data)
       #    return(data[ind,])
       #}
   })
   
   
   output$dataUpload <- DT::renderDataTable({
       
       datatable(dataM(), extensions = 'ColReorder', options = list(colReorder = TRUE))
       
   })
   
   
   
   ################################ Observe Life Table (start) ###############################
   
   # observe({
   #    updateSelectInput(session, "statusVariable", choices = colnames(dataM()), selected = colnames(dataM())[4])
   #})
   
   observe({
       updateSelectizeInput(session, "survivalTime", choices = colnames(dataM()), selected = colnames(dataM())[1])
   })
   
   observe({
       
       if(input$factorVar){
          updateSelectizeInput(session, "factor", choices = colnames(dataM()), selected = colnames(dataM())[3])
       }else{
           
          updateSelectizeInput(session, "factor", choices = NULL, selected = NULL)
       }
   })
   
   
   statusVarLT <- reactive({return(input$statusVariable)})
   
   
   observe({
       data_tmp <- dataM()
       if (!is.null(data_tmp)){
           updateSelectizeInput(session = session, inputId = "statusVariable",
           choices = colnames(data_tmp), selected = colnames(data_tmp)[4])
       } else {
           updateSelectizeInput(session = session, inputId = "statusVariable",
           choices = "", selected = "")
       }
   })
   
   observe({
       data_tmp <- dataM()
       if (!is.null(data_tmp)){
           idx <- which(colnames(data_tmp) %in% statusVarLT())
           categories <- levels(as.factor(as.character(data_tmp[ ,idx])))
           
           updateSelectizeInput(session = session, inputId = "status", choices = categories,
           selected = categories[2])
       } else {
           updateSelectizeInput(session = session, inputId = "status", choices = "",
           selected = "")
       }
   })
   

   
 ################################ Observe Life Table (end) ###############################
 #########################################################################################
 ################################ Observe Kaplan-Meier (start) ###########################
 observe({
     updateSelectizeInput(session, "statusVariableKM", choices = colnames(dataM()), selected = colnames(dataM())[4])
 })
 
 observe({
     updateSelectizeInput(session, "survivalTimeKM", choices = colnames(dataM()), selected = colnames(dataM())[1])
 })
 
 observe({
     
     if(input$factorVarKM){
         updateSelectizeInput(session, "factorKM", choices = colnames(dataM()), selected = colnames(dataM())[3])
     }else{
         
         updateSelectizeInput(session, "factoKMr", choices = NULL, selected = NULL)
     }
 })
 
 statusVarKM <- reactive({return(input$statusVariableKM)})
 
 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         updateSelectizeInput(session = session, inputId = "statusVariableKM", choices = colnames(data_tmp), selected = colnames(data_tmp)[4])
     } else {
         updateSelectizeInput(session = session, inputId = "statusVariableKM", choices = "", selected = "")
     }
 })
 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         idx <- which(colnames(data_tmp) %in% statusVarKM())
         categories <- levels(as.factor(as.character(data_tmp[ ,idx])))
         
         updateSelectizeInput(session = session, inputId = "statusKM", choices = categories, selected = categories[2])
     } else {
         updateSelectizeInput(session = session, inputId = "statusKM", choices = "", selected = "")
     }
 })

 #observe({
     
 #    if (input$selectPlotKM == 1){
 #    updateCheckboxInput(session, "kmPlotOptions", label = "Edit Plot", value = NULL)
 #    }
 #})
 

 ################################ Observe Kaplan-Meier (end) ###########################
 #########################################################################################
 #########################################################################################
 ################################ Observe Cox Regression (start) ###########################

 observe({
     updateSelectizeInput(session, "survivalTimeCox", choices = colnames(dataM()), selected = colnames(dataM())[1])
 })

 #############################################################
 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         updateSelectizeInput(session = session, inputId = "statusVariableCox", choices = colnames(data_tmp), selected = colnames(data_tmp)[4])
     } else {
         updateSelectizeInput(session = session, inputId = "statusVariableCox", choices = "", selected = "")
     }
 })

  statusVarCox <- reactive({return(input$statusVariableCox)})

 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         idx <- which(colnames(data_tmp) %in% statusVarCox())
         categories <- levels(as.factor(as.character(data_tmp[ ,idx])))
         
         updateSelectizeInput(session = session, inputId = "statusCox", choices = categories, selected = categories[2])
     } else {
         updateSelectizeInput(session = session, inputId = "statusCox", choices = "", selected = "")
     }
 })

#############################################################

observe({
    updateSelectizeInput(session, "categoricalInput", choices = colnames(dataM()), selected = colnames(dataM())[3])
})

observe({
    updateSelectizeInput(session, "continuousInput", choices = colnames(dataM()), selected = colnames(dataM())[2])
})

observe({

      if(input$twoWayInteractions || input$threeWayInteractions){
    
       updateCheckboxInput(session, "customInteractions", value = FALSE)

    }

  if(input$customInteractions){
    
       updateCheckboxInput(session, "twoWayInteractions", value = FALSE)
       updateCheckboxInput(session, "threeWayInteractions", value = FALSE)
       updateCheckboxInput(session, "customInteractions", value = TRUE)

    }
})

observe({

      if(input$customInteractions){

        updateSelectizeInput(session, "selectCustomInteractions", choices = cInteractions(), selected = NULL)
      }

  })


observe({

      if(input$addTimeDependentCovariates){

        updateSelectizeInput(session, "selectTimeDependentVariables", choices = colnames(dataM()), selected = NULL)
      }

  })


strataVarNames <- reactive({

      colnames(dataM())

  })


observe({

      if(input$addStrata){

        updateSelectizeInput(session, "selectStrataVariable", choices = colnames(dataM()), selected = NULL)
      }

  })


 ################################ Observe Cox Regression (end) #########################
 #######################################################################################

 #########################################################################################
 ################################ Observe Regularized Cox Regression (start) ###########################
 #########################################################################################

 observe({
     updateSelectizeInput(session, "survivalTimerCox", choices = colnames(dataM()), selected = colnames(dataM())[1])
 })

 
 observe({
     data_tmp_rCox <- dataM()
     if (!is.null(data_tmp_rCox)){
         updateSelectizeInput(session = session, inputId = "statusVariablerCox", choices = colnames(data_tmp_rCox), selected = colnames(data_tmp_rCox)[4])
     } else {
         updateSelectizeInput(session = session, inputId = "statusVariablerCox", choices = "", selected = "")
     }
 })

  statusVarrCox <- reactive({return(input$statusVariablerCox)})

 
 observe({
     data_tmp_rCox <- dataM()
     if (!is.null(data_tmp_rCox)){
         idx_rCox <- which(colnames(data_tmp_rCox) %in% statusVarrCox())
         categories_rCox <- levels(as.factor(as.character(data_tmp_rCox[ ,idx_rCox])))
         
         updateSelectizeInput(session = session, inputId = "statusrCox", choices = categories_rCox, selected = categories_rCox[2])
     } else {
         updateSelectizeInput(session = session, inputId = "statusrCox", choices = "", selected = "")
     }
 })


observe({
    updateSelectizeInput(session, "selectVariablerCox", choices = colnames(dataM()), selected = colnames(dataM())[3])
})



 ################################ Observe Regularized Cox Regression (end) #########################
 #######################################################################################
################################ Observe Random Survival Forest (start) ###########################

 observe({
     updateSelectizeInput(session, "survivalTimeRF", choices = colnames(dataM()), selected = colnames(dataM())[1])
 })

 #############################################################
 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         updateSelectizeInput(session = session, inputId = "statusVariableRF", choices = colnames(data_tmp), selected = colnames(data_tmp)[4])
     } else {
         updateSelectizeInput(session = session, inputId = "statusVariableRF", choices = "", selected = "")
     }
 })

  statusVarRF <- reactive({return(input$statusVariableRF)})

 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         idx <- which(colnames(data_tmp) %in% statusVarRF())
         categories <- levels(as.factor(as.character(data_tmp[ ,idx])))
         
         updateSelectizeInput(session = session, inputId = "statusRF", choices = categories, selected = categories[2])
     } else {
         updateSelectizeInput(session = session, inputId = "statusRF", choices = "", selected = "")
     }
 })


observe({
    updateSelectizeInput(session, "categoricalInputRF", choices = colnames(dataM()), selected = colnames(dataM())[3])
})

observe({
    updateSelectizeInput(session, "continuousInputRF", choices = colnames(dataM()), selected = colnames(dataM())[2])
})

observe({

      if(input$twoWayInteractionsRF || input$threeWayInteractionsRF){
    
       updateCheckboxInput(session, "customInteractionsRF", value = FALSE)

    }

  if(input$customInteractionsRF){
    
       updateCheckboxInput(session, "twoWayInteractionsRF", value = FALSE)
       updateCheckboxInput(session, "threeWayInteractionsRF", value = FALSE)
       updateCheckboxInput(session, "customInteractionsRF", value = TRUE)

    }
})

observe({

      if(input$customInteractionsRF){

        updateSelectizeInput(session, "selectCustomInteractionsRF", choices = cInteractions(), selected = NULL)
      }

  })


observe({

      if(input$addTimeDependentCovariatesRF){

        updateSelectizeInput(session, "selectTimeDependentVariablesRF", choices = colnames(dataM()), selected = NULL)
      }

  })


strataVarNamesRF <- reactive({

      colnames(dataM())

  })


observe({

      if(input$addStrataRF){

        updateSelectizeInput(session, "selectStrataVariableRF", choices = colnames(dataM()), selected = NULL)
      }

  })


 ################################ Observe Random Survival Forest (end) #########################





 ###################### Life Table (start) #############################################
 
 output$descriptivesText <- renderText({
     if (input$run && input$caseSummary){
         'Table 1: Descriptives'
     }
 })
 
 output$lifeTableText <- renderText({
     if (input$run && input$lifeTable){
         'Table 2: Life Table'
     }
 })
 
 
 output$medianLifeTimeText <- renderText({
     if (input$run && input$medianLifeTime){
         'Table 3: Median Life Time'
     }
 })
 
 
 output$hrText <- renderText({
     if (input$run && input$hr){
         'Table 4: Hazard Ratio'
     }
 })
 
 output$compTestText <- renderText({
     if (input$run && input$compTest && input$factorVar){
         'Table 5: Comparison Test'
     }
 })
 
 
 result <- reactive({
 
  if(input$run || input$createLTPlot){
    dataSet = dataM()
    
    
        if(input$factorVar){
            
            fctr = input$factor
        }else{fctr = NULL}
        
 
        lt = lifeTables(survivalTime = input$survivalTime, statusVariable = input$statusVariable, status = input$status, factors= fctr, fromTime = input$from, toTime = input$to, by = input$by, lifeTable = input$lifeTable, descriptives = input$caseSummary, hr = input$hr, medianLifeTime = input$medianLifeTime, ci = input$ci,
            varianceEstimation = input$varianceEstimation, compare = input$compTest, comparisonTest = input$comparisonTest, confidenceLevel = input$confidenceLevel,
        referenceCategory = input$refCategory, typeOfTest = "asymptotic", data = dataSet)
    
    
    lt
    }
 
 })
 
 
 descriptivesReactive <- reactive({
 
 if(input$run){
     
     desc = result()$tableResult$caseSummary
     
     if(!is.null(desc)){
         if(input$factorVar){
             descs = do.call(rbind.data.frame, desc)
             
         }else{
             
             descs = desc
             
         }
     }else{descs = NULL}
     
     descs
     
 }else{descs = NULL}

 
 
 })
 
 
 output$descriptives <- DT::renderDataTable({
  
  datatable(descriptivesReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
  ))

 })
 
 
 lifetableReactive <- reactive({
 
 if(input$run){
     ltResult = result()$testResult$lifeTable
     
     if(!is.null(ltResult)){
         if(input$factorVar){
             ltResults = do.call(rbind.data.frame, ltResult)
             
         }else{
             
             ltResults = ltResult
             
         }
     }else{ltResults = NULL}
     
     ltResults
 }else{ltResults = NULL}



 })
 
 
 
 output$lifetableResult <- DT::renderDataTable({
     
     datatable(lifetableReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
     ))
     
 })
 
 medianLifeTimeReactive <- reactive({
 
 if(input$run){
     mlt = result()$tableResult$medianLifeTime
     
     if(!is.null(mlt)){
         if(input$factorVar){
             mltResults = do.call(rbind.data.frame, mlt)
             
         }else{
             
             mltResults = mlt
             
         }
     }else{mltResults = NULL}
     
     mltResults
 }else{mltResults = NULL}
 
 
 
 })
 
 
 output$medianLifeTimeResult <- DT::renderDataTable({
     
     datatable(medianLifeTimeReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
     ))
     
 })
 
 
 hazardRatioReactive <- reactive({
  
 if(input$run){
     
     hrResult = result()$testResult$hazardRatio
     
     if(!is.null(hrResult)){
         if(input$factorVar){
             hrResults = do.call(rbind.data.frame, hrResult)
             
         }else{
             
             hrResults = hrResult
             
         }
     }else{hrResults = NULL}
     
     hrResults
 }else{hrResults = NULL}
 
 })
 
 
 output$hazardRatioResult <- DT::renderDataTable({
    
    datatable(hazardRatioReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
 
 })
 
 
 comparisonTestReactive <- reactive({
 
 if(input$factorVar && input$compTest && input$run){
     
     compTestResult = result()$testResult$testResults
     
 }else{
     
     compTestResult = NULL
     
 }
 
 
 })
 
 output$comparisonTestResults <- DT::renderDataTable({
     
     datatable(comparisonTestReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
     ))
   
 })
 
 ###################### Life Table (end) #######################################################
 ###############################################################################################
 ###################### Kaplan-Meier (start) ###################################################


output$descriptivesTextKM <- renderText({
    if (input$runKM && input$caseSummaryKM){
        'Table 1: Descriptives'
    }
})

output$survivalTableTextKM <- renderText({
    if (input$runKM && input$survivalTable){
        'Table 2: Survival Table'
    }
})


output$meanMedianSurvivalTimesText <- renderText({
    if (input$runKM && input$meanMedianSurvivalTimes){
        'Table 3: Mean and Median Life Time'
    }
})

#output$quartilesOfSurvivalTimesText <- renderText({
#    if (input$runKM && input$quartilesOfSurvivalTimes){
#        'Table 4: Quartiles of Survival Times'
#    }
#})



output$hrTextKM <- renderText({
    if (input$runKM && input$hrKM){
        'Table 4: Hazard Ratio'
    }
})

output$compTestTextKM <- renderText({
    if (input$runKM && input$compTestKM && input$factorVarKM){
        'Table 5: Comparison Test'
    }
})


resultKM <- reactive({
    
    if(input$runKM){
        dataSet = dataM()
        
         if(input$factorVarKM){
             
             fctr = input$factorKM
         }else{fctr = NULL}
         
            
            km = kaplanMeier (survivalTime = input$survivalTimeKM, statusVariable  = input$statusVariableKM, status = input$statusKM, factors = fctr, survivalTable = TRUE, caseSummary = input$caseSummaryKM, hr=input$hrKM,
            meanMedianSurvivalTimes = input$meanMedianSurvivalTimes, quartilesOfSurvivalTimes = FALSE, ci = input$ciKM,
            varianceEstimation = input$varianceEstimationKM, comparisonTest = input$comparisonTestKM, confidenceLevel = input$confidenceLevelKM,
            referenceCategory = input$refCategoryKM, typeOfTest = "asymptotic", kmCurve = TRUE, data = dataSet)

        
        km
    }
    
})


descriptivesReactiveKM <- reactive({
    
    if(input$runKM && input$caseSummaryKM){
        
        desc = resultKM()$tableResult$caseSummary
        
        if(!is.null(desc)){
            if(input$factorVarKM){
                descs = do.call(rbind.data.frame, desc)
                
            }else{
                
                descs = desc
                
            }
        }else{descs = NULL}
        
        descs
        
    }else{descs = NULL}
    
    
    
})


output$descriptivesKM <- DT::renderDataTable({
    
    datatable(descriptivesReactiveKM(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


survivalTableReactive <- reactive({
    
    if(input$runKM && input$survivalTable){
        stResult = resultKM()$testResult$survivalTable
        
        if(!is.null(stResult)){
            if(input$factorVarKM){
                stResults = do.call(rbind.data.frame, stResult)
                
            }else{
                
                stResults = stResult
                
            }
        }else{stResults = NULL}
        
        stResults
    }else{stResults = NULL}
    
    
    
})



output$survivaltableResult <- DT::renderDataTable({
    
    datatable(survivalTableReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


factorGroup <- reactive({

      names(resultKM()$testResult$survivalTable)

  })                


observe({

      if(input$createSurvivalPlot){

        updateSelectizeInput(session, "selectGroup", choices =  names(resultKM()$testResult$survivalTable), selected = NULL)
      }

  })

output$survivalPlot <- highcharter::renderHighchart({


    surv = resultKM()$testResult$survivalTable[[input$selectGroup]]

    highchart() %>% hc_exporting(enabled = TRUE, filename = "survivalPlot") %>% 
      hc_add_series(name = "Survival", type = "line", data = surv$`Cumulative probability of surviving`, showInLegend = FALSE, zIndex = 1, marker = list(lineColor = "black", lineWidth = 1), lineWidth = 0, id = "survival") %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(surv$`Lower limit`, surv$`Upper limit`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "survival") %>%
      hc_chart(zoomType = "xy", inverted = TRUE) %>%
      hc_xAxis(categories = as.character(surv$Time), title = list(text = "Time")) %>%
      hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Survival")) %>%
      #hc_plotOptions(tooltip = list(headerFormat = "<b>Time: </b>{point.x}")) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())


  })


meanMedianSurvivalTimesReactive <- reactive({
    
    if(input$runKM){
        mst = resultKM()$tableResult$meanMedianSurvivalTimes
        
        if(!is.null(mst)){
            
                mstResults = mst
                
        }else{mstResults = NULL}
        
        rownames(mstResults) = mstResults$Factor
        mstResults[-1]
        
    }else{mstResults = NULL}
    
    
    
})


output$meanMedianSurvivalTimesResult <- DT::renderDataTable({
    
    datatable(meanMedianSurvivalTimesReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


hazardRatioReactiveKM <- reactive({
    
    if(input$runKM){
        
        hrResult = resultKM()$testResult$hazardRatio
        
        if(!is.null(hrResult)){
            if(input$factorVarKM){
                hrResults = do.call(rbind.data.frame, hrResult)
                
            }else{
                
                hrResults = hrResult
                
            }
        }else{hrResults = NULL}
        
        hrResults
    }else{hrResults = NULL}
    
})


output$hazardRatioResultKM <- DT::renderDataTable({
    
    datatable(hazardRatioReactiveKM(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})



observe({

      if(input$createHazardPlot){

        updateSelectizeInput(session, "selectGroupHazard", choices =  names(resultKM()$testResult$hazardRatio), selected = NULL)
      }

  })

output$hazardPlot <- highcharter::renderHighchart({


    hazard = resultKM()$testResult$hazardRatio[[input$selectGroupHazard]]

    highchart() %>% hc_exporting(enabled = TRUE, filename = "hazardPlot") %>% 
      hc_add_series(name = "Hazard", type = "line", data = hazard$Hazard.Ratio, showInLegend = FALSE, zIndex = 1, marker = list(lineColor = "black", lineWidth = 1), lineWidth = 0, id = "hazard") %>%
      hc_add_series(name = "CI", data = as.matrix(cbind(hazard$Lower, hazard$Upper)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "hazard") %>%
      hc_chart(zoomType = "xy", inverted = TRUE) %>%
      hc_xAxis(categories = as.character(hazard$Time), title = list(text = "Time")) %>%
      hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Hazard Ratio")) %>%
      #hc_plotOptions(tooltip = list(headerFormat = "<b>Time: </b>{point.x}")) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>") %>%
      hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                     errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
      hc_add_theme(hc_theme_google())


  })







comparisonTestReactiveKM <- reactive({
    
    if(input$factorVarKM && input$compTestKM && input$runKM){
        
        compTestResult = resultKM()$testResult$testResults
        
    }else{
        
        compTestResult = NULL
        
    }
    
    
})

output$comparisonTestResultsKM <- DT::renderDataTable({
    
    datatable(comparisonTestReactiveKM(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

###################### Kaplan-Meier (end) ###################################################
#############################################################################################
###################### Cox Regression (start) #################################################

output$displaySummaryCox <- renderText({
  if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox){
        'Model Summary'
    }

  }  
})


output$displayCoefficientEstimatesCox <- renderText({
  if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$displayCoefficientEstimates){
        'Table 1: Coefficient Estimates'
    }

  }  
})

output$hazardRatioCox <- renderText({

  if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$hrcox){
        'Table 2: Hazard Ratio'
    }

  }  
})


output$goodnessOfFitTestsText <- renderText({

   if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$goodnessOfFitTests){
        'Table 3: Goodness of Fit Tests'
    }
  }
})

output$analysisOfDevianceCox <- renderText({

  if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$analysisOfDeviance){
        'Table 4: Analysis of Deviance'
    }

  }
})


output$storePredictionsCox <- renderText({

   if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$storePredictions){
        'Table 5: Predictions'
    }
  }
})

output$residualsCoxText <- renderText({

  if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$residuals){
        'Table 6: Residuals'
    }

  }
})

output$martingaleResidualsCoxText <- renderText({

  if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$martingaleResiduals){
        'Table 7: Martingale Residuals'
    }
  }
})

output$schoenfeldResidualsCoxText <- renderText({

  if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$schoenfeldResiduals){
        'Table 8: Schoenfeld Residuals'
    }

  }
})


output$dfBetasCoxText <- renderText({
  if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox && input$dfBetas){
        'Table 9: DfBetas'
    }

  }
})


output$phAssumptionCoxText <- renderText({

    if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox){
        'Table 1: Proportional Hazard Test'
    }

  }
})

output$phPlotsText <- renderText({

    if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

    if (input$runCox){
        'Figure 1: Proportional Hazard Plots'
    }

  }
})


resultCox <- reactive({
    
    #if(input$runKM){
        dataSet = dataM()
        
        #if(input$factorVarKM){
            
        #    fctr = input$factorKM
        #}else{fctr = NULL}
        

 if(is.null(input$categoricalInput) && is.null(input$continuousInput)){     

      cox = NULL

 }
 else if(input$runCox){  


      #Sys.sleep(1)
         
       cox = coxRegression(survivalTime = input$survivalTimeCox, categoricalInput = input$categoricalInput, 
                           continuousInput = input$continuousInput, statusVariable = input$statusVariableCox, 
                           status = input$statusCox, addInteractions = input$addInteractions, twoWayinteractions = input$twoWayInteractions, threeWayinteractions = input$threeWayInteractions, 
                           customInteractions =input$customInteractions, selectCustomInteractionTerms = input$selectCustomInteractions, timeDependetCovariate = input$addTimeDependentCovariates, selectTimeDependentCovariate = input$selectTimeDependentVariables,
                           timeDependentVariableTransformation = input$timeDepTransform, strata = input$addStrata, strataVariable = input$selectStrataVariable, displayDescriptives = TRUE, displayCoefficientEstimates = input$displayCoefficientEstimates, displayModelFit = TRUE,
                           hazardRatio = input$hrcox, goodnessOfFitTests = input$goodnessOfFitTests, 
                           analysisOfDeviance = input$analysisOfDeviance,
                           ties = input$ties, confidenceLevel = input$confidenceLevelCox, 
                           alternativeHypothesis = "equalToTestValue",
                           modelSelectionCriteria = input$modelSelectionCriteria, 
                           modelSelection = input$modelSelection, alphaEnter = input$alphaToEnter, 
                           referenceCategory = input$refCategoryCox,alphaRemove = input$alphaToRemove, 
                           storePredictions = input$storePredictions, storeResiduals = input$residuals,
                           storeMartingaleResiduals = input$martingaleResiduals, 
                           storeSchoenfeldResiduals = input$schoenfeldResiduals, storeDfBetas = input$dfBetas, multipleID = input$multipleID,
                           data = dataSet)        #,
 
 }
        #
        #
        #
        #  data = dataSet)
        
        
        cox
        #}


})


#descriptivesReactiveCox <- reactive({
    
#    if(input$runCox && input$caseSummaryCox){
        
#        desc = resultKM()$tableResult$caseSummary
        
#        if(!is.null(desc)){
#            if(input$factorVarKM){
#                descs = do.call(rbind.data.frame, desc)
#
#            }else{
                
#               descs = desc
#
#            }
#        }else{descs = NULL}
        
#        descs
        
#    }else{descs = NULL}
    
    
    
#})


#output$descriptivesKM <- DT::renderDataTable({
    
#    datatable(descriptivesReactiveKM(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
#    dom = 'Bfrtip',
#    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
#    ))
    
#})


output$summaryCox <- renderPrint({

      if(input$runCox){
        summary <- resultCox()$testResult$ModelSummaryList
        summary
      }else{summary = NULL}
  })


displayCoefficientEstimatesReactive <- reactive({
    

    if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

        coeffResults = NULL
    }else{

      if(input$runCox && input$displayCoefficientEstimates){
      
          coeffResults = resultCox()$testResult$displayCoefficientEstimatesResults
          
         }else{coeffResults = NULL}
      
    }
    #coeffResults
    
})



output$displayCoefficientEstimatesResult <- DT::renderDataTable({
    
    datatable(displayCoefficientEstimatesReactive(), rownames = FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})





hazardRatioReactiveCox <- reactive({
    
    if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

          hrResults = NULL

    }else{  
    
        if(input$runCox && input$hrcox){
            
            hrResult = resultCox()$testResult$hazardRatioResults
            
    
        }else{hrResults = NULL}
    }
  # hrResult
  
})


output$hazardRatioResultCox <- DT::renderDataTable({
    
    datatable(hazardRatioReactiveCox(), rownames= FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


hazardError <- reactive({

    if(input$createHazardCoxPlot){
    
          cox = resultCox()$testResult$hazardRatioResults

          if(nrow(cox)>1){
    
            cox = cbind.data.frame("Variable" = levels(cox$Variable), apply(cox[,-1],2, as.numeric))
    
           }else{

            cox = cbind.data.frame("Variable" = levels(cox$Variable), as.data.frame(t(apply(cox[,-1],2, as.numeric))))

           }   

         highchart() %>% hc_exporting(enabled = TRUE, filename = "hazardPlot") %>% 
          hc_add_series(name = "Hazard", type = "line", data = cox$`Hazard ratio`, showInLegend = FALSE, zIndex = 1, marker = list(lineColor = "black", lineWidth = 1), lineWidth = 0, id = "hazard") %>%
          hc_add_series(name = "CI", data = as.matrix(cbind(cox$`Lower limit (95%)`, cox$`Upper limit (95%)`)),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "hazard") %>%
          hc_chart(zoomType = "xy", inverted = TRUE) %>%
          hc_xAxis(categories = matrix(cox$Variable, ncol = 1)) %>%
          hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Hazard Ratio"), plotLines = list(list(value = 1, width = 2, color = "green", dashStyle = "Dash"))) %>%
          #hc_plotOptions(tooltip = list(headerFormat = "<b>Time: </b>{point.x}")) %>%
          hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Variable: </b>{point.x} <br>") %>%
          hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                         errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
          hc_add_theme(hc_theme_google())
    
    }

  })

output$hazardErrorbar <- highcharter::renderHighchart({

      if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

          p = NULL

    }else{  

    if(input$runCox && input$hrcox){
    
    hazardError()


    }else{p = NULL}
  }
    
})



goodnessOfFitTestsResultsReactiveCox <- reactive({
    
  if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

    gof = NULL

  }else{
  
      if(input$runCox && input$goodnessOfFitTests){
          
              gof = resultCox()$testResult$goodnessOfFitTestsResults
              
      }else{gof = NULL}
  }
  gof
})

output$goodnessOfFitTestsRes <- DT::renderDataTable({
    
    datatable(goodnessOfFitTestsResultsReactiveCox(), rownames=FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


analysisOfDevianceResultsReactiveCox <- reactive({


  if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

      aod = NULL
  }else{
  
      if(input$runCox && input$analysisOfDeviance){
      
          aod = resultCox()$testResult$analysisOfDevianceResults
      
      }else{aod = NULL}
  }
    aod
})

output$analysisOfDevianceRes <- DT::renderDataTable({
    
    datatable(analysisOfDevianceResultsReactiveCox(), rownames=FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

predictionsReactiveCox <- reactive({

  if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

      preds = NULL

   }else{ 
       if(input$runCox && input$storePredictions){
       
           preds = resultCox()$testResult$Store$Predictions
       
       }else{preds = NULL}
    }
    preds
})

output$predictionsCox <- DT::renderDataTable({
    
    datatable(predictionsReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


residualsReactiveCox <- reactive({

  if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

    residuals = NULL
  }else{  
    if(input$runCox && input$residuals){
        
        residuals = resultCox()$testResult$Store$Residuals
    
    }else{residuals = NULL}
   } 
    residuals
})

output$residualsCox <- DT::renderDataTable({
    
    datatable(residualsReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

martingaleResidualsReactiveCox <- reactive({
    
  if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

    martingale = NULL
  }else{ 
    if(input$runCox && input$martingaleResiduals){
        
        martingale = resultCox()$testResult$Store$MartingaleResiduals
    
    }else{martingale = NULL}
   } 
    martingale
})

output$martingaleResidualsCox <- DT::renderDataTable({
    
    datatable(martingaleResidualsReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

schoenfeldResidualsReactiveCox <- reactive({

  if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

    schoenfeld = NULL
  }else{ 
    
    if(input$runCox && input$schoenfeldResiduals){
        
        schoenfeld = resultCox()$testResult$Store$SchoenfeldResiduals
    
    }else{schoenfeld = NULL}
   } 
    schoenfeld
})

output$schoenfeldResidualsCox <- DT::renderDataTable({
    
    datatable(schoenfeldResidualsReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})



dfBetasReactiveCox <- reactive({
    
  if(is.null(input$categoricalInput) && is.null(input$continuousInput)){ 

    dfbetas = NULL
  }else{ 
    if(input$runCox && input$dfBetas){
    
            dfbetas = resultCox()$testResult$Store$DfBetas
        
    }else{dfbetas = NULL}
  }
    dfbetas
})

output$dfBetasCox <- DT::renderDataTable({
    
    datatable(dfBetasReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})




phAssumptionReactiveCox <- reactive({
    
    if(input$runCox){
        
        ph = resultCox()$testResult$displayCoxPh
        
    }else{ph = NULL}
    
    ph
})

output$phAssumptionCox <- DT::renderDataTable({
    
    datatable(phAssumptionReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


output$phPlot <- renderPlot({
    #if(input$runCox){
        #n_row = nrow(resultCox()$testResult$displayCoxPh)
        #par(mfrow=c(n_row, 1))
        #lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
        resultCox()$plotResult
    #}
})


###################### Cox Regression (end) ###################################################


kmCurves <- reactive({

    data = dataM()


    survivalTime = input$survivalTimeKM
    statusVariable  = input$statusVariableKM
    status = input$statusKM

    if(input$factorVarKM){
        fctr = input$factorKM
    }else{fctr = NULL}

    ci = input$ciKM
    varianceEstimation = input$varianceEstimationKM
    confidenceLevel = input$confidenceLevelKM
    factors = fctr


    if(!is.null(survivalTime)){
        survivalTime = as.matrix(data[, survivalTime, drop = FALSE])
    }

    if(!is.null(factors)){
        factors = as.factor(data[, factors])
    }

    if(!is.null(factors)){
        factorsName = data[, factors, drop = FALSE]
    }

    if(!is.null(statusVariable)){
        statusVariable = data[, statusVariable]
        
    }

    if(!is.null(status)){
        if(is.numeric(status)){status = as.numeric(status)}else{status = as.factor(status)}
    }

    if(!is.null(factors)){
        newData = data.frame(id =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime,
        statusVar=statusVariable,factor = factors)
        newData = newData[complete.cases(newData),]
        colnames(newData) = c("id","time","statusVar", "factor")
        
    }else{
        
        newData = data.frame(id =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime,
        statusVar=statusVariable)
        newData = newData[complete.cases(newData),]
        colnames(newData) = c("id", "time", "statusVar")
        
    }

        newData$statusVar = newData$statusVar%in%status


    #data[,input$survivalTimeKM] = as.numeric(data[,input$survivalTimeKM])
    #data[,input$factorVarKM] = as.factor(data[,input$factorVarKM])


    if(!is.null(fctr)){
        compareCurves <- survfit(Surv(time, statusVar == TRUE) ~ factors, data = newData, conf.type = ci, error = varianceEstimation, conf.int = confidenceLevel/100)
    

        for(i in 1:length(names(compareCurves$strata))) {
          
            names(compareCurves$strata)[i] = gsub("factors", input$factorKM, names(compareCurves$strata)[i])
          
        }




    }else{
        compareCurves <- survfit(Surv(time, statusVar == TRUE) ~ 1, data = newData, conf.type = ci, error = varianceEstimation, conf.int = confidenceLevel/100)
    }




      return(compareCurves)


  })


theme <- reactive({

    if(input$changeTheme == "theme1"){

      hc_theme_538()

    }

    else if(input$changeTheme == "theme2"){

      hc_theme_economist()
      
    }
    else if(input$changeTheme == "theme3"){

      hc_theme_ft()
      
    }

    else if(input$changeTheme == "theme4"){

      hc_theme_db()
      
    }
    else if(input$changeTheme == "theme5"){

      hc_theme_flat()
      
    }
    else if(input$changeTheme == "theme6"){

      hc_theme_flatdark()
      
    }

    else if(input$changeTheme == "theme7"){

      hc_theme_smpl()
      
    }
   else if(input$changeTheme == "theme8"){

      hc_theme_elementary()
      
    }
   else if(input$changeTheme == "theme9"){

      hc_theme_google()
      
    }

   else if(input$changeTheme == "theme10"){

      hc_theme_gridlight()
      
    }
   else if(input$changeTheme == "theme11"){

      hc_theme_sandsignika()
      
    }





  })



    selectBackgroundColor <- reactive({

        if(input$changeTheme == "theme0" || input$changeTheme == "theme7" || input$changeTheme == "theme8" || input$changeTheme == "theme9" || input$changeTheme == "theme10" || input$changeTheme == "theme11"){

          "#FFFFFF"

        }else{


            theme()$chart$backgroundColor
        }

    })



    observe({

        shinyjs::updateColourInput(session, "backgroundKM", value = selectBackgroundColor())

      }, priority = 100)







kmPlot <- reactive({

    if(input$factorVarKM){
        fctr = input$factorKM
        enabledLegend = TRUE
    }else{
      fctr = NULL
      enabledLegend = FALSE
    }


  

      is.even <- function(x) x %% 2 == 0

      ranges = input$addCI

      p = hchart(kmCurves(), ranges = ranges, type = "line", markTimes = input$addCens, symbol = input$censShapeKM, markerColor = input$censColKM, 
                 animation = TRUE, rangesOpacity = input$alpha)# %>% hc_colors(cols)#, xAxis = list(crosshair = list(width = 1, color = "#000")))


      if(ranges){
        if(!is.null(fctr)){

          for(i in 1:length(p$x$hc_opts$series)){

            if(is.even(i)){
              p$x$hc_opts$series[[i]]$name = "CI (95%)"

            }

          }
        }else{

            p$x$hc_opts$series[[2]]$name = "CI (95%)"


          }
      }



    if(input$changeTheme == "theme0"){

        p2 = p %>% hc_exporting(enabled = TRUE, filename = "plot") %>% 
          hc_title(text = input$mainPanelKM) %>%  
          hc_xAxis(title = list(text = input$xlabKM), tickInterval=NULL, tickLength = 5, lineWidth = 1)  %>%  
          hc_yAxis(title = list(text = input$ylabKM), lineWidth = 1, tickLength = 5, tickWidth= 1, labels = list(format = "{value:.2f}")) %>% 
          #hc_colors("#440154") %>%
          hc_chart(backgroundColor = input$backgroundKM, zoomType = "xy") %>% 
          hc_legend(enabled = enabledLegend) %>% 
          hc_plotOptions(line = list(dashStyle = input$ltyKM), area = list(zIndex = 15), series = list(enableMouseTracking = TRUE)) %>% 
          hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 3, followTouchMove = FALSE, headerFormat = "<b>Time</b>: {point.key} <br>")#, pointFormat = "{series.name}: {point.y}")


    }else{

        p2 = p %>% hc_exporting(enabled = TRUE, filename = "plot") %>% 
          hc_title(text = input$mainPanelKM) %>%  
          hc_xAxis(title = list(text = input$xlabKM), tickInterval=NULL, tickLength = 5, lineWidth = 1)  %>%  
          hc_yAxis(title = list(text = input$ylabKM), lineWidth = 1, tickLength = 5, tickWidth= 1, labels = list(format = "{value:.2f}")) %>% 
          #hc_colors("#440154") %>%
          hc_add_theme(theme()) %>%
          hc_chart(backgroundColor = input$backgroundKM, zoomType = "xy") %>% 
          hc_legend(enabled = enabledLegend) %>% 
          hc_plotOptions(line = list(dashStyle = input$ltyKM), area = list(zIndex = 15), series = list(enableMouseTracking = TRUE)) %>% 
          hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 3, followTouchMove = FALSE, headerFormat = "<b>Time</b>: {point.key} <br>")#, pointFormat = "{series.name}: {point.y}")
    }
        p2

})



themeHazard <- reactive({

    if(input$changeThemeHazard == "theme1"){

      hc_theme_538()

    }

    else if(input$changeThemeHazard == "theme2"){

      hc_theme_economist()
      
    }
    else if(input$changeThemeHazard == "theme3"){

      hc_theme_ft()
      
    }

    else if(input$changeThemeHazard == "theme4"){

      hc_theme_db()
      
    }
    else if(input$changeThemeHazard == "theme5"){

      hc_theme_flat()
      
    }
    else if(input$changeThemeHazard == "theme6"){

      hc_theme_flatdark()
      
    }

    else if(input$changeThemeHazard == "theme7"){

      hc_theme_smpl()
      
    }
   else if(input$changeThemeHazard == "theme8"){

      hc_theme_elementary()
      
    }
   else if(input$changeThemeHazard == "theme9"){

      hc_theme_google()
      
    }

   else if(input$changeThemeHazard == "theme10"){

      hc_theme_gridlight()
      
    }
   else if(input$changeThemeHazard == "theme11"){

      hc_theme_sandsignika()
      
    }





  })


    selectBackgroundColorHazard <- reactive({

        if(input$changeThemeHazard == "theme0" || input$changeThemeHazard == "theme7" || input$changeThemeHazard == "theme8" || input$changeThemeHazard == "theme9" || input$changeThemeHazard == "theme10" || input$changeThemeHazard == "theme11"){

          "#FFFFFF"

        }else{


            themeHazard()$chart$backgroundColor
        }

    })


  observe({

    shinyjs::updateColourInput(session, "backgroundHazard", value = selectBackgroundColorHazard())

  })




hazardPlot <- reactive({
    
    if(input$factorVarKM){
        fctr = input$factorKM
        enabledLegend = TRUE
    }else{
      fctr = NULL
      enabledLegend = FALSE
    }


  

      is.even <- function(x) x %% 2 == 0

      ranges = input$addCIHazard

      p = hchart(kmCurves(), fun = "cumhaz", ranges = ranges, type = "line", markTimes = input$addCensHazard, symbol = input$censShapeHazard, markerColor = input$censColHazard, 
                 animation = TRUE, rangesOpacity = input$alphaHazard)# %>% hc_colors(cols)#, xAxis = list(crosshair = list(width = 1, color = "#000")))


      if(ranges){
        if(!is.null(fctr)){

          for(i in 1:length(p$x$hc_opts$series)){

            if(is.even(i)){
              p$x$hc_opts$series[[i]]$name = "CI (95%)"

            }

          }
        }else{

            p$x$hc_opts$series[[2]]$name = "CI (95%)"


          }
      }

    if(input$changeThemeHazard == "theme0"){

      p2 = p %>% hc_exporting(enabled = TRUE, filename = "plot") %>% 
        hc_title(text = input$mainPanelHazard) %>%  
        hc_xAxis(title = list(text = input$xlabHazard), tickInterval=NULL, tickLength = 5, lineWidth = 1)  %>%  
        hc_yAxis(title = list(text = input$ylabHazard), lineWidth = 1, tickLength = 5, tickWidth= 1, labels = list(format = "{value:.2f}")) %>% 
        #hc_colors("#440154") %>%
        hc_chart(backgroundColor = input$backgroundHazard, zoomType = "xy") %>% 
        hc_legend(enabled = enabledLegend) %>% 
        hc_plotOptions(line = list(dashStyle = input$ltyHazard), area = list(zIndex = 15), series = list(enableMouseTracking = TRUE)) %>% 
        hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 3, followTouchMove = FALSE, headerFormat = "<b>Time</b>: {point.key} <br>")#, pointFormat = "{series.name}: {point.y}")


   }else{

      p2 = p %>% hc_exporting(enabled = TRUE, filename = "plot") %>% 
        hc_title(text = input$mainPanelHazard) %>%  
        hc_xAxis(title = list(text = input$xlabHazard), tickInterval=NULL, tickLength = 5, lineWidth = 1)  %>%  
        hc_yAxis(title = list(text = input$ylabHazard), lineWidth = 1, tickLength = 5, tickWidth= 1, labels = list(format = "{value:.2f}")) %>% 
        #hc_colors("#440154") %>%
        hc_add_theme(themeHazard()) %>%
        hc_chart(backgroundColor = input$backgroundHazard, zoomType = "xy") %>% 
        hc_legend(enabled = enabledLegend) %>% 
        hc_plotOptions(line = list(dashStyle = input$ltyHazard), area = list(zIndex = 15), series = list(enableMouseTracking = TRUE)) %>% 
        hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 3, followTouchMove = FALSE, headerFormat = "<b>Time</b>: {point.key} <br>")#, pointFormat = "{series.name}: {point.y}")
    }
      p2
    
    
})




themeLml <- reactive({
  
  if(input$changeThemeLml == "theme1"){
    
    hc_theme_538()
    
  }
  
  else if(input$changeThemeLml == "theme2"){
    
    hc_theme_economist()
    
  }
  else if(input$changeThemeLml == "theme3"){
    
    hc_theme_ft()
    
  }
  
  else if(input$changeThemeLml == "theme4"){
    
    hc_theme_db()
    
  }
  else if(input$changeThemeLml == "theme5"){
    
    hc_theme_flat()
    
  }
  else if(input$changeThemeLml == "theme6"){
    
    hc_theme_flatdark()
    
  }
  
  else if(input$changeThemeLml == "theme7"){
    
    hc_theme_smpl()
    
  }
  else if(input$changeThemeLml == "theme8"){
    
    hc_theme_elementary()
    
  }
  else if(input$changeThemeLml == "theme9"){
    
    hc_theme_google()
    
  }
  
  else if(input$changeThemeLml == "theme10"){
    
    hc_theme_gridlight()
    
  }
  else if(input$changeThemeLml == "theme11"){
    
    hc_theme_sandsignika()
    
  }
  
  
  
  
  
})

selectBackgroundColorLml <- reactive({
  
  if(input$changeThemeLml == "theme0" || input$changeThemeLml == "theme7" || input$changeThemeLml == "theme8" || input$changeThemeLml == "theme9" || input$changeThemeLml == "theme10" || input$changeThemeLml == "theme11"){
    
    "#FFFFFF"
    
  }else{
    
    
    themeLml()$chart$backgroundColor
  }
  
})

observe({
  
  shinyjs::updateColourInput(session, "backgroundLml", value = selectBackgroundColorLml())
  
})


LmlPlot <- reactive({
  
  if(input$factorVarKM){
    fctr = input$factorKM
    enabledLegend = TRUE
  }else{
    fctr = NULL
    enabledLegend = FALSE
  }
  
  
  
  
  is.even <- function(x) x %% 2 == 0
  
  ranges = input$addCILml
  
  p = hchart(kmCurves(), fun = "cloglog", ranges = ranges, type = "line", markTimes = input$addCensLml, symbol = input$censShapeLml, markerColor = input$censColLml, 
             animation = TRUE, rangesOpacity = input$alphaLml)# %>% hc_colors(cols)#, xAxis = list(crosshair = list(width = 1, color = "#000")))
  
  
  if(ranges){
    if(!is.null(fctr)){
      
      for(i in 1:length(p$x$hc_opts$series)){
        
        if(is.even(i)){
          p$x$hc_opts$series[[i]]$name = "CI (95%)"
          
        }
        
      }
    }else{
      
      p$x$hc_opts$series[[2]]$name = "CI (95%)"
      
      
    }
  }
  
  if(input$changeThemeLml == "theme0"){
    
    p2 = p %>% hc_exporting(enabled = TRUE, filename = "plot") %>% 
      hc_title(text = input$mainPanelLml) %>%  
      hc_xAxis(title = list(text = input$xlabLml), tickInterval=NULL, tickLength = 5, lineWidth = 1)  %>%  
      hc_yAxis(title = list(text = input$ylabLml), lineWidth = 1, tickLength = 5, tickWidth= 1, labels = list(format = "{value:.2f}")) %>% 
      #hc_colors("#440154") %>%
      hc_chart(backgroundColor = input$backgroundLml, zoomType = "xy") %>% 
      hc_legend(enabled = enabledLegend) %>% 
      hc_plotOptions(line = list(dashStyle = input$ltyLml), area = list(zIndex = 15), series = list(enableMouseTracking = TRUE)) %>% 
      hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 3, followTouchMove = FALSE, headerFormat = "<b>Time</b>: {point.key} <br>")#, pointFormat = "{series.name}: {point.y}")
    
    
  }else{
    
    p2 = p %>% hc_exporting(enabled = TRUE, filename = "plot") %>% 
      hc_title(text = input$mainPanelLml) %>%  
      hc_xAxis(title = list(text = input$xlabLml), tickInterval=NULL, tickLength = 5, lineWidth = 1)  %>%  
      hc_yAxis(title = list(text = input$ylabLml), lineWidth = 1, tickLength = 5, tickWidth= 1, labels = list(format = "{value:.2f}")) %>% 
      #hc_colors("#440154") %>%
      hc_add_theme(themeLml()) %>%
      hc_chart(backgroundColor = input$backgroundLml, zoomType = "xy") %>% 
      hc_legend(enabled = enabledLegend) %>% 
      hc_plotOptions(line = list(dashStyle = input$ltyLml), area = list(zIndex = 15), series = list(enableMouseTracking = TRUE)) %>% 
      hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 3, followTouchMove = FALSE, headerFormat = "<b>Time</b>: {point.key} <br>")#, pointFormat = "{series.name}: {point.y}")
  }
  p2
  
  
})




 output$LTCurvePlot <- plotly::renderPlotly({
     
     if(input$createLTPlot){
         
         plotLT(result(), lty.est = input$ltyLT, xlab = input$xlabLT,
                 ylab = input$ylabLT, main = input$mainPanelLT)
         
     }
     
   
 })

  
schoenfeldPlotReactive <- reactive({

    if(is.null(input$categoricalInput) && is.null(input$continuousInput)){
        cph = NULL
    }else{
        compareCurves = resultCox()$model
        cph <- cox.zph(compareCurves, transform = 'rank')

    }
    cph
  })


# output$kmCurvePlot <- plotly::renderPlotly({
     
 #    if(input$createKmPlot  && input$selectPlotKM == '1'){
 #        
 #        kmPlot()
 #        
 #    }
 #    
 #   else if(input$createKmPlot && input$selectPlotKM == '2'){
 #        
 #        hazardPlot()
 #        
 #    }
 #    
 #})


 output$kmCurvePlot <- highcharter::renderHighchart({
     
     if(input$createKmPlot && input$selectPlotKM == 1){
         
         kmPlot()
         
     }


     else if(input$createKmPlot && input$selectPlotKM == 2){
         
         hazardPlot()
         
     }

     else if(input$createKmPlot && input$selectPlotKM == 3){
         
         LmlPlot()
         
     }
    
     
 })



cInteractions <- reactive({

  fNames <- c(input$categoricalInput, input$continuousInput)


                 if(input$customInteractions && length(fNames) > 2){
                         
                            ctwoWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 2)), paste, collapse = ":"))
                            names(ctwoWayInteractionTerms) <- NULL
                            
                            cthreeWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 3)), paste, collapse = ":"))
                            names(cthreeWayInteractionTerms) <- NULL
                            
                            
                            customInteractionTerms = c(ctwoWayInteractionTerms, cthreeWayInteractionTerms)
                            names(customInteractionTerms) = NULL


                        
                        }else{customInteractionTerms = NULL}

                        customInteractionTerms
  })




 #output$schoenfeldPlot <- renderPlot({
    
 #   if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){

  #    plotSchoenfeld(schoenfeldPlotReactive(), resid="input$addResidual", se=TRUE, df=4, nsmo=40, ltyest=2, ltyci = 3, col=1, lwd=1)

   # }   

#})


observe({

if(!is.null(input$categoricalInput) || !is.null(input$continuousInput)){


  if(input$runCox){
 
  nRow = nrow(schoenfeldPlotReactive()$table)
 
  if(nRow > 1){
  
        svars = rownames(schoenfeldPlotReactive()$table)[-nRow]  

  }else{

       svars = rownames(schoenfeldPlotReactive()$table)
  }

       svars

  }else{

       svars = NULL
  }

  

    if(input$runCox){

       updateSelectizeInput(session, "selectVariablesForSchoenfeld", choices = svars, selected = NULL)

     }else{

       updateSelectizeInput(session = session, inputId = "selectVariablesForSchoenfeld", choices = "", selected = "")

   }

  } 
})


themeSchoenfeld <- reactive({
  
  if(input$changeThemeSchoenfeld == "theme1"){
    
    hc_theme_538()
    
  }
  
  else if(input$changeThemeSchoenfeld == "theme2"){
    
    hc_theme_economist()
    
  }
  else if(input$changeThemeSchoenfeld == "theme3"){
    
    hc_theme_ft()
    
  }
  
  else if(input$changeThemeSchoenfeld == "theme4"){
    
    hc_theme_db()
    
  }
  else if(input$changeThemeSchoenfeld == "theme5"){
    
    hc_theme_flat()
    
  }
  else if(input$changeThemeSchoenfeld == "theme6"){
    
    hc_theme_flatdark()
    
  }
  
  else if(input$changeThemeSchoenfeld == "theme7"){
    
    hc_theme_smpl()
    
  }
  else if(input$changeThemeSchoenfeld == "theme8"){
    
    hc_theme_elementary()
    
  }
  else if(input$changeThemeSchoenfeld == "theme9"){
    
    hc_theme_google()
    
  }
  
  else if(input$changeThemeSchoenfeld == "theme10"){
    
    hc_theme_gridlight()
    
  }
  else if(input$changeThemeSchoenfeld == "theme11"){
    
    hc_theme_sandsignika()
    
  }
   
  
})

observe({
  
  shinyjs::updateColourInput(session, "backgroundSchoenfeld", value = selectBackgroundColorSchoenfeld())
  
})


selectBackgroundColorSchoenfeld <- reactive({
  
  if(input$changeThemeSchoenfeld == "theme0" || input$changeThemeSchoenfeld == "theme7" || input$changeThemeSchoenfeld == "theme8" || input$changeThemeSchoenfeld == "theme9" || input$changeThemeSchoenfeld == "theme10" || input$changeThemeSchoenfeld == "theme11"){
    
    "#FFFFFF"
    
  }else{
    
    
    themeSchoenfeld()$chart$backgroundColor
  }
  
})


 schoenfeldPlot <- reactive({

    if(input$selectPlotCox == 1 && input$runCox){
          compareCurves = resultCox()$model
          x <- cox.zph(compareCurves, transform = 'rank')
    
          resid = input$addResidual
          se = input$addCIschoenfeld
          df = input$dfSchoenfeld
          nsmo = input$nsmoPH
          ltyest = input$ltySchoenfeld 
          ltyci = input$ltySchoenfeldCI
          col = 1
          colorLine = input$curveColSchoenfeld
          colorLineCI = input$colSchoenfeldCI

          lwd=1

          lty = ltyest
          xx <- x$x
          yy <- x$y
          d <- nrow(yy)
          df <- max(df)     
          nvar <- ncol(yy)
          pred.x <- seq(from=min(xx), to=max(xx), length=nsmo)
          temp <- c(pred.x, xx)
          lmat <- splines::ns(temp, df=df, intercept=TRUE)
          pmat <- lmat[1:nsmo,]       
          xmat <- lmat[-(1:nsmo),]
          qmat <- qr(xmat)
          yvar = x$y
          var = ncol(yvar)
    
          if (qmat$rank < df) {stop("Spline fit is singular, try a smaller degrees of freedom")}
    
          if (se) {
            bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
            xtx <- bk %*% t(bk)
            seval <- d*((pmat%*% xtx) *pmat) %*% rep(1, df)
          }
    
          if (missing(var)) {var <- 1:nvar}else {
            if (is.character(var)) {var <- match(var, dimnames(yy)[[2]])}
            if  (any(is.na(var)) || max(var)>nvar || min(var) <1) {stop("Invalid variable requested")}
          }
    
          if (x$transform == 'log') {
            xx <- exp(xx)
            pred.x <- exp(pred.x)
          }
          if(x$transform != 'identity') {
            xtime <- as.numeric(dimnames(yy)[[1]])
            indx <- !duplicated(xx) 
            apr1  <- approx(xx[indx], xtime[indx], 
                            seq(min(xx), max(xx), length=17)[2*(1:8)])
            temp <- signif(apr1$y,2)
            apr2  <- approx(xtime[indx], xx[indx], temp)
            xaxisval <- apr2$y
            xaxislab <- rep("",8)
            for (i in 1:8) {xaxislab[i] <- format(temp[i])}
          }
    
          col <- rep(col, length=2)
          lwd <- rep(lwd, length=2)
          lty <- rep(lty, length=2)
    
    
          svar = input$selectVariablesForSchoenfeld
    
    
          for (i in 1:var) {
            y <- yy[,svar]
            yhat <- pmat %*% qr.coef(qmat, y)
            if (resid) {yr <-range(yhat, y)}else{yr <-range(yhat)}
            if (se) {
              temp <- 2* sqrt(x$var[i,i]*seval)
              yup <- yhat + temp
              ylow<- yhat - temp
              yr <- range(yr, yup, ylow)
              newData2 = cbind.data.frame(pred.x,yhat, yup, ylow) 

            }
            
            newData = cbind.data.frame(xx, y)
            newData3 = cbind.data.frame(pred.x, yhat)

            
           
           fn =  paste0("function() {\n if (this.value == ", xaxisval[1], ") {return ",xaxislab[1], "}\n  
                    if (this.value == ", xaxisval[2], ") {return ",xaxislab[2], "}\n
                    if (this.value == ", xaxisval[3], ") {return ",xaxislab[3], "}\n
                    if (this.value == ", xaxisval[4], ") {return ",xaxislab[4], "}\n
                    if (this.value == ", xaxisval[5], ") {return ",xaxislab[5], "}\n
                    if (this.value == ", xaxisval[6], ") {return ",xaxislab[6], "}\n
                    if (this.value == ", xaxisval[7], ") {return ",xaxislab[7], "}\n
                    if (this.value == ", xaxisval[8], ") {return ",xaxislab[8], "}\n
                   ", "}"
                  )
            
      
         ylabel =  paste0(input$ylabSchoenfeld, svar)
  
         if(input$changeThemeSchoenfeld == "theme0"){

              sp = highchart() %>% 
                hc_add_series(name = "Curve", data = as.matrix(newData3), type = "line",  enabled = FALSE, color = input$curveColSchoenfeld, marker = list(enabled = FALSE), id = "schoenLine")%>%
                hc_xAxis(tickInterval=NULL, tickLength = 5, lineWidth = 1, tickPositions = xaxisval, labels = list(formatter = JS(fn), format = "{value:.2f}"), title = list(text = input$xlabSchoenfeld))%>%
                hc_yAxis(tickInterval=NULL, tickLength = 5, lineWidth = 1, title = list(text = ylabel), labels = list(format = "{value:.2f}"))%>% 
                hc_title(text = input$mainPanelSchoenfeld) %>%
                hc_plotOptions(line = list(dashStyle = input$ltySchoenfeld)) %>%
                hc_chart(backgroundColor = input$backgroundSchoenfeld, zoomType = "xy") %>%
                hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 2, followTouchMove = FALSE)

          }else{

              sp = highchart() %>% 
                hc_add_series(name = "Curve", data = as.matrix(newData3), type = "line",  enabled = FALSE, color = input$curveColSchoenfeld, marker = list(enabled = FALSE), id = "schoenLine")%>%
                hc_xAxis(tickInterval=NULL, tickLength = 5, lineWidth = 1, tickPositions = xaxisval, labels = list(formatter = JS(fn), format = "{value:.2f}"), title = list(text = input$xlabSchoenfeld))%>%
                hc_yAxis(tickInterval=NULL, tickLength = 5, lineWidth = 1, title = list(text = ylabel), labels = list(format = "{value:.2f}"))%>% 
                hc_title(text = input$mainPanelSchoenfeld) %>%
                hc_add_theme(themeSchoenfeld()) %>%
                hc_plotOptions(line = list(dashStyle = input$ltySchoenfeld)) %>%
                hc_chart(backgroundColor = input$backgroundSchoenfeld, zoomType = "xy") %>%
                hc_tooltip(shared = TRUE, crosshairs = FALSE, valueDecimals = 2, followTouchMove = FALSE)

          }
              
              if (resid){
                sp = sp %>%  hc_add_series(name = "Residuals", data = as.matrix(newData), type="scatter",  labels = list(format = "{value:.2f}"), color = input$colSchoenfeldResiduals, zIndex = 10, marker = list(symbol = "circle", radius = 4))
              }
              
              if (se){
                
                sp = sp %>% hc_add_series(name = "CI", data = as.matrix(cbind(newData2$pred.x, newData2$ylow, newData2$yup)),
                                     type = "arearange", fillOpacity = input$alphaSchoenfeld, showInLegend = FALSE, linkedTo = "schoenLine", color = input$colSchoenfeldCI)
                
              }
              
              sp = sp %>% hc_exporting(enabled = TRUE, filename = "schoenfeldplot") 
          }
    
        }else{

            sp = NULL      
        } 

    return(sp)   #schoenfeldPlotReactive()
})



 kmCurvesCoxReactive <- reactive({


    if(input$selectPlotCox == 2){
    data = dataM()


    survivalTime = input$survivalTimeCox
    statusVariable  = input$statusVariableCox
    status = input$statusCox


    rnames = colnames(data)
    fctr = input$selectVariablesForSchoenfeld

    fctr = rnames[which.max(RecordLinkage::levenshteinSim(fctr,rnames))]

    #ci = input$ciKM
    #varianceEstimation = input$varianceEstimationKM
    #confidenceLevel = input$confidenceLevelKM
    factors = fctr


    if(!is.null(survivalTime)){
        survivalTime = as.matrix(data[, survivalTime, drop = FALSE])
    }

    if(!is.null(factors)){
        factors = as.factor(data[, factors])
    }

   # if(!is.null(factors)){
    #    factorsName = data[, factors, drop = FALSE]
    #}

    if(!is.null(statusVariable)){
        statusVariable = data[, statusVariable]   
    }

    if(!is.null(status)){
        if(is.numeric(status)){status = as.factor(status)}else{status = as.factor(status)}
    }

    if(!is.null(factors)){
        newData = data.frame(id =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime,
        statusVar=statusVariable,factor = factors)
        newData = newData[complete.cases(newData),]
        colnames(newData) = c("id", "time", "statusVar", "factor")

    }

    #data[,input$survivalTimeKM] = as.numeric(data[,input$survivalTimeKM])
    #data[,input$factorVarKM] = as.factor(data[,input$factorVarKM])

    newData$statusVar = newData$statusVar%in%status

    if(!is.null(fctr)){

        compareCurves <- survfit(Surv(time, statusVar == TRUE) ~ factors, data = newData)



        for(i in 1:length(names(compareCurves$strata))) {
          
            names(compareCurves$strata)[i] = gsub("factors", fctr, names(compareCurves$strata)[i])
          
        }


    }

     # compareCurves$surv = log(-log(compareCurves$surv))



    }else{

      compareCurves = NULL
    }


    return(compareCurves)


  })


themeSchoenfeldLml <- reactive({
  
  if(input$changeThemeSchoenfeldLml == "theme1"){
    
    hc_theme_538()
    
  }
  
  else if(input$changeThemeSchoenfeldLml == "theme2"){
    
    hc_theme_economist()
    
  }
  else if(input$changeThemeSchoenfeldLml == "theme3"){
    
    hc_theme_ft()
    
  }
  
  else if(input$changeThemeSchoenfeldLml == "theme4"){
    
    hc_theme_db()
    
  }
  else if(input$changeThemeSchoenfeldLml == "theme5"){
    
    hc_theme_flat()
    
  }
  else if(input$changeThemeSchoenfeldLml == "theme6"){
    
    hc_theme_flatdark()
    
  }
  
  else if(input$changeThemeSchoenfeldLml == "theme7"){
    
    hc_theme_smpl()
    
  }
  else if(input$changeThemeSchoenfeldLml == "theme8"){
    
    hc_theme_elementary()
    
  }
  else if(input$changeThemeSchoenfeldLml == "theme9"){
    
    hc_theme_google()
    
  }
  
  else if(input$changeThemeSchoenfeldLml == "theme10"){
    
    hc_theme_gridlight()
    
  }
  else if(input$changeThemeSchoenfeldLml == "theme11"){
    
    hc_theme_sandsignika()
    
  }
  
  
  
  
  
})


selectBackgroundColorSchoenfeldLml <- reactive({
  
  if(input$changeThemeSchoenfeldLml == "theme0" || input$changeThemeSchoenfeldLml == "theme7" || input$changeThemeSchoenfeldLml == "theme8" || input$changeThemeSchoenfeldLml == "theme9" || input$changeThemeSchoenfeldLml == "theme10" || input$changeThemeSchoenfeldLml == "theme11"){
    
    "#FFFFFF"
    
  }else{
    
    
    themeSchoenfeldLml()$chart$backgroundColor
  }
  
})



observe({
  
  shinyjs::updateColourInput(session, "backgroundSchoenfeldLml", value = selectBackgroundColorSchoenfeldLml())
  
})


kmPlotCoxReactive <- reactive({


 #if(!is.null(kmCurvesCoxReactive())){

      #p <-  hchart(kmCurvesCoxReactive(), fun = "cloglog") %>% 


      is.even <- function(x) x %% 2 == 0
      
      ranges = input$addCILmlSchoenfeld
      
      p = hchart(kmCurvesCoxReactive(), fun = "cloglog", ranges = ranges, type = "line", animation = TRUE, rangesOpacity = input$alphaLmlSchoenfeld)
      
      
      if(ranges){
          
          for(i in 1:length(p$x$hc_opts$series)){
            
            if(is.even(i)){
              p$x$hc_opts$series[[i]]$name = "CI (95%)"
          
        }
        
      }
    
  }
  
  if(input$changeThemeSchoenfeldLml == "theme0"){
    
    p2 = p %>% hc_exporting(enabled = TRUE, filename = "plot") %>% 
      hc_title(text = input$mainPanelLmlSchoenfeld) %>%  
      hc_xAxis(title = list(text = input$xlabLmlSchoenfeld), tickInterval=NULL, tickLength = 5, lineWidth = 1)  %>%  
      hc_yAxis(title = list(text = input$ylabLmlSchoenfeld), lineWidth = 1, tickLength = 5, tickWidth= 1, labels = list(format = "{value:.2f}")) %>% 
      #hc_colors("#440154") %>%
      hc_chart(backgroundColor = input$backgroundSchoenfeldLml, zoomType = "xy") %>% 
      hc_legend(enabled = TRUE) %>% 
      hc_plotOptions(line = list(dashStyle = input$ltyLmlSchoenfeld), area = list(zIndex = 15), series = list(enableMouseTracking = TRUE)) %>% 
      hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 3, followTouchMove = FALSE, headerFormat = "<b>Time</b>: {point.key} <br>")#, pointFormat = "{series.name}: {point.y}")
    
    
  }else{
    
    p2 = p %>% hc_exporting(enabled = TRUE, filename = "plot") %>% 
      hc_title(text = input$mainPanelLmlSchoenfeld) %>%  
      hc_xAxis(title = list(text = input$xlabLmlSchoenfeld), tickInterval=NULL, tickLength = 5, lineWidth = 1)  %>%  
      hc_yAxis(title = list(text = input$ylabLmlSchoenfeld), lineWidth = 1, tickLength = 5, tickWidth= 1, labels = list(format = "{value:.2f}")) %>% 
      #hc_colors("#440154") %>%
      hc_add_theme(themeSchoenfeldLml()) %>%
      hc_chart(backgroundColor = input$backgroundSchoenfeldLml, zoomType = "xy") %>% 
      hc_legend(enabled = TRUE) %>% 
      hc_plotOptions(line = list(dashStyle = input$ltyLmlSchoenfeld), area = list(zIndex = 15), series = list(enableMouseTracking = TRUE)) %>% 
      hc_tooltip(shared = TRUE, crosshairs = TRUE, valueDecimals = 3, followTouchMove = FALSE, headerFormat = "<b>Time</b>: {point.key} <br>")#, pointFormat = "{series.name}: {point.y}")
  }
  p2
  




      





 
      #  }

      })


         output$phPlots <- renderHighchart({

                if(input$selectPlotCox == 1 && input$runCox){
                    schoenfeldPlot()
                }

                else if(input$selectPlotCox == 2 && input$runCox){

                    kmPlotCoxReactive()
              }

          })

          output$str <- renderPrint({
            categoriesCox()

          })


##############################################################################################
#################### Random Survival Forest (start) ##########################################
##############################################################################################

output$indSurvPreds <- renderText({

   if(!is.null(input$categoricalInputRF) || !is.null(input$categoricalInputRF)){

    if (input$runRF && input$survivalResultRF){
        'Table 1: Individual Survival Predictions'
    }
  }
})


output$indSurvPredsOOB <- renderText({

   if(!is.null(input$categoricalInputRF) || !is.null(input$categoricalInputRF)){

    if (input$runRF && input$survivalResultOobRF){
        'Table 2: Individual Survival Predictions OOB'
    }
  }
})

output$indChfPreds <- renderText({

   if(!is.null(input$categoricalInputRF) || !is.null(input$categoricalInputRF)){

    if (input$runRF && input$chRF){
        'Table 4: Individual Cumulative Hazard Predictions'
    }
  }
})


output$indChfPredsOOB <- renderText({

   if(!is.null(input$categoricalInputRF) || !is.null(input$categoricalInputRF)){

    if (input$runRF && input$chOobRF){
        "Table 5: Individual Cumulative Hazard Predictions OOB"
    }
  }
})

output$errorRateText <- renderText({

   if(!is.null(input$categoricalInputRF) || !is.null(input$categoricalInputRF)){

    if (input$runRF && input$errorRateRF){
        "Table 6: Error Rate"
    }
  }
})


output$varImpText <- renderText({

   if(!is.null(input$categoricalInputRF) || !is.null(input$categoricalInputRF)){

    if (input$runRF && input$varImp){
        "Table 7: Variable Importance"
    }
  }
})




randomForestSurvival <- reactive({
  
  if(input$runRF){
  
      survivalTime = input$survivalTimeRF
      categoricalInput =input$categoricalInputRF
      continuousInput = input$continuousInputRF 
      statusVariable = input$statusVariableRF 
      status = input$statusRF
      addInteractions = input$addInteractionsRF
      twoWayinteractions = input$twoWayInteractionsRF
      threeWayinteractions = input$threeWayInteractionsRF
      customInteractions = input$customInteractionsRF
      selectCustomInteractionTerms = input$selectCustomInteractionsRF
      timeDependetCovariate = input$addTimeDependentCovariatesRF
      timeDependentVariableTransformation = input$timeDepTransformRF
      selectTimeDependentCovariate = input$selectTimeDependentVariablesRF
      strata = input$addStrataRF
      strataVariable = input$selectStrataVariableRF
      referenceCategory = input$refCategoryRF
      multipleID = input$multipleIDRF
      
      
      #if(input$runKM){
      data = dataM()
      
      
      if(!is.null(survivalTime)){
        survivalTime = as.matrix(data[, survivalTime, drop = FALSE])
        survivalTime = apply(survivalTime, 2, as.numeric)
      }
      
      if(!is.null(categoricalInput)){
        categoricalInput = as.data.frame(data[, categoricalInput, drop = FALSE])
        categoricalInput = apply(categoricalInput, 2, as.factor)
        categoricalInput = as.data.frame(categoricalInput)
        
        
      }
      
      if(!is.null(continuousInput)){
        continuousInput = as.data.frame(data[, continuousInput, drop = FALSE])
        continuousInput = apply(continuousInput, 2, as.numeric)
        continuousInput = as.data.frame(continuousInput)
      }
      
      if(!is.null(statusVariable)){
        statusVariable = as.factor(data[, statusVariable])
        
        
      }
      
      if(!is.null(status)){
        if(is.numeric(status)){status = as.factor(status)}else{status = as.factor(status)}
        
      }
      
      if(!is.null(categoricalInput) && !is.null(continuousInput)){
        newData = data.frame(id2 =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime[,1], 
                             statusVar=statusVariable, categoricalInput, continuousInput)
        newData = newData[complete.cases(newData),]
        
      }else if(!is.null(categoricalInput) && is.null(continuousInput)){
        newData = data.frame(id2 =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime[,1], 
                             statusVar=statusVariable, categoricalInput)
        newData = newData[complete.cases(newData),]
        
      }else if(is.null(categoricalInput) && !is.null(continuousInput)){
        newData = data.frame(id2 =seq(1,dim(survivalTime)[1], 1), survivalTime = survivalTime[,1], 
                             statusVar=statusVariable, continuousInput)
        newData = newData[complete.cases(newData),]
        
      }
      
      
      
      
      if(referenceCategory != "first"){
        for(l in 1:dim(categoricalInput)[2]){
          newData[, names(categoricalInput)[l]] <- relevel(categoricalInput[,l], ref = levels(categoricalInput[,l])[length(levels(categoricalInput[,l]))])
        }
      }
      
      
      if(addInteractions){
        
        if(!is.null(categoricalInput) || !is.null(continuousInput)){
          
          fNames <- names(c(categoricalInput, continuousInput))
          
        }  
        
        if(twoWayinteractions && length(fNames) >1){
          
          twoWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 2)), paste, collapse = ":"))
          names(twoWayInteractionTerms) <- NULL
          
        }else{twoWayInteractionTerms = NULL}
        
        if(threeWayinteractions && length(fNames) >2){
          
          threeWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 3)), paste, collapse = ":"))
          names(threeWayInteractionTerms) <- NULL
          
        }else{threeWayInteractionTerms = NULL}  
        
        #if(customInteractions && length(fNames) >2){
        
        #   ctwoWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 2)), paste, collapse = ":"))
        #   names(twoWayInteractionTerms) <- NULL
        
        #   cthreeWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 3)), paste, collapse = ":"))
        #  names(threeWayInteractionTerms) <- NULL
        
        
        #  customInteractionTerms = c(ctwoWayInteractionTerms, cthreeWayInteractionTerms)
        # names(customInteractionTerms) = NULL
        
        
        
        #}else{customInteractionTerms = NULL}
        
        if(customInteractions){
          
          interactions = selectCustomInteractionTerms
          
        }else{
          
          interactions = c(twoWayInteractionTerms, threeWayInteractionTerms)
          
        } 
        
      }else{
        interactions = NULL
        customInteractionTerms= NULL
      }
      
      
      if(strata){
        
        strataVar = strataVariable
        newData = cbind.data.frame(newData, data[, strataVar])
        names(newData)[dim(newData)[2]] = strataVar
        
      }
      
      
      newData = cbind.data.frame(newData, data[colnames(data)[!(colnames(data) %in% colnames(newData))]])
      
      # if(multipleID != TRUE){  
      #   newData$statusVar = newData$statusVar%in%status
      #   
      #   newData$id <- 1:nrow(newData)
      #   cut.points <- unique(newData$survivalTime[newData$statusVar == TRUE])
      #   newData <- survSplit(formula = Surv(survivalTime, statusVar)~., data = newData, cut = cut.points, end = "stop",
      #                        start = "start", event = "statusVar")
      # }
      newData$statusVar = as.factor(newData$statusVar)%in%status
      
      if(timeDependetCovariate && !is.null(selectTimeDependentCovariate)){
        
        timeDependentCovariateNames = list()
        for(i in 1:length(selectTimeDependentCovariate)){
          
          if(timeDependentVariableTransformation == "log"){
            
            newData = cbind.data.frame(newData, tmpNames = newData[,selectTimeDependentCovariate[i]]*log(newData[, "survivalTime"]))
            
          }else{
            
            newData = cbind.data.frame(newData, tmpNames = as.numeric(newData[,selectTimeDependentCovariate[i]])*newData[, "survivalTime"])
            
          }
          
          names(newData)[dim(newData)[2]] = timeDependentCovariateNames[[i]] = paste0("time_", selectTimeDependentCovariate[i])
          
        }
        
        timeDependentNames = unlist(timeDependentCovariateNames)    
        
      }
      
      # if(multipleID){  
      #   names(newData)[which(names(newData) == "start")] = "start2"
      #   
      #   newData <- newData %>%
      #     group_by(id) %>%
      #     mutate(start = 0:(n() - 1))
      #   
      #   newData = as.data.frame(newData)
      #   
      #   ind = row.names(newData[newData$start !=0,])
      #   
      #   newData$start[as.numeric(ind)] =  newData$survivalTime[as.numeric(ind)-1]
      # }
      
      if(!is.null(categoricalInput) || !is.null(continuousInput)){
        
        predictors = paste0(names(c(categoricalInput, continuousInput)), collapse = "+")
        
        if(!is.null(interactions)){
          
          
          if(length(interactions) > 1){
            interactions2 = paste(interactions, collapse = "+")
            predictors2 = paste(predictors, interactions2, sep = "+", collapse = "+")
          }    
          
          if(length(interactions) == 1){
            predictors2 = paste(predictors, interactions, sep = "+")
          }
          predictors = predictors2
        }
        
        if(timeDependetCovariate && !is.null(selectTimeDependentCovariate)){
          
          
          if(length(timeDependentNames) > 1){
            timeDependents = paste(timeDependentNames, collapse = "+")
          }else{
            
            timeDependents =  timeDependentNames
          }
          predictors = paste(predictors, timeDependents, sep = "+", collapse = "+")
        }
        
        if(strata && !is.null(strataVariable)){
          
          strataVars = paste0("strata(",strataVar,")")
          predictors = paste(predictors, strataVars, sep = "+", collapse = "+")
          
        } 
        
        }else{predictors = 1}
      
        formula = as.formula(paste0("Surv(survivalTime, statusVar ==  TRUE) ~ ", predictors))
            
        rf = rfsrc(formula = formula, data = newData, ntree = input$ntree, bootstrap = input$bootstrap, tree.err=TRUE, 
                     importance = TRUE, membership = TRUE, statistics = TRUE, do.trace = TRUE,
                     mtry = input$mtry, nodesize = input$nodesize, nodedepth = input$nodedepth, splitrule = input$splitrule, nsplit = input$nsplit,
                     split.null = FALSE, na.action = input$naAction, nimpute = input$nimpute, proximity = input$proximity, sampsize = NULL,
                     samptype = input$samptype, case.wt = NULL, xvar.wt = NULL, forest = TRUE, var.used = FALSE, 
                     split.depth = FALSE, seed = 1234, coerce.factor = NULL)
      
      
      attr(rf, "class") = "list"
      return(rf)  
    }
})




 output$survival <- DT::renderDataTable({


if(input$survivalResultRF && input$runRF){
    survival = data.frame(randomForestSurvival()$survival)

    survival = apply(survival, 2, FUN = function(x){
  
          as.numeric(formatC(x, digits = 3))
  
    })

    survival = as.data.frame(survival)
    colnames(survival) = as.character(randomForestSurvival()$time.interest)
  


      datatable(survival, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
      ))

      }

 })


 output$survivalOOB <- DT::renderDataTable({

    if(input$survivalResultOobRF  && input$runRF){


      survivalOOB = data.frame(randomForestSurvival()$survival.oob)

      survivalOOB = apply(survivalOOB, 2, FUN = function(x){
    
            as.numeric(formatC(x, digits = 3))
    
      })

      survivalOOB = as.data.frame(survivalOOB)
      colnames(survivalOOB) = as.character(randomForestSurvival()$time.interest)
    


        datatable(survivalOOB, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
        ))
      }
 })


  output$errorRate <- DT::renderDataTable({

            if(input$errorRateRF  && input$runRF){

                errorRates = data.frame(randomForestSurvival()$err.rate)

                errorRates = apply(errorRates, 2, FUN = function(x){
              
                      as.numeric(formatC(x, digits = 3))
                  }
                )

                errorRates = data.frame(1:length(errorRates), errorRates)

                colnames(errorRates) = c("Number of tree", "Error rate")

                  datatable(errorRates, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
                  ))
           }   
         })


  output$chf <- DT::renderDataTable({

          if(input$chRF  && input$runRF){

            chfRes = data.frame(randomForestSurvival()$chf)

            chfRes = apply(chfRes, 2, FUN = function(x){
          
                  as.numeric(formatC(x, digits = 3))
          
            })

            chfRes = as.data.frame(chfRes)
            colnames(chfRes) = as.character(randomForestSurvival()$time.interest)
          


              datatable(chfRes, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
              ))
            }
         })


         output$chfOOB <- DT::renderDataTable({

            if(input$chOobRF  && input$runRF){

              chfOOBres = data.frame(randomForestSurvival()$chf.oob)

              chfOOBres = apply(chfOOBres, 2, FUN = function(x){
            
                    as.numeric(formatC(x, digits = 3))
            
              })

              chfOOBres = as.data.frame(chfOOBres)
              colnames(chfOOBres) = as.character(randomForestSurvival()$time.interest)
            


                datatable(chfOOBres, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
                ))

              }

 })


  output$variableImportance <- DT::renderDataTable({

    if(input$varImp  && input$runRF){

      variableImp = data.frame(t(randomForestSurvival()$importance))

      variableImp = apply(variableImp, 2, FUN = function(x){
    
            as.numeric(formatC(x, digits = 3))
    
      })

      variableImp = as.data.frame(variableImp)

        datatable(variableImp, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
        ))

      }

 })



output$variableImportancePlot <- renderHighchart({

 if(input$varImp  && input$runRF){
      importance = as.numeric(formatC(randomForestSurvival()$importance, digits = 3, format = "f"))


      highchart() %>% 
        hc_chart(type = "column", inverted = TRUE) %>% 
        hc_xAxis(categories = names(randomForestSurvival()$importance)) %>% 
        hc_add_series(data = importance, name = "Importance") %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>")%>%
        hc_add_theme(hc_theme_google())
    }
})




 observe({
  if(input$runRF && input$createRSFplot){
       updateSelectInput(session, "customSelect", choices = c(1:nrow(dataM())), selected = NULL)
   }
   })

  observe({
    if(input$runRF && input$createRSFplot){
       updateSelectInput(session, "customSelectOOB", choices = c(1:nrow(dataM())), selected = NULL)
   }
   })

  observe({
    if(input$runRF && input$createRSFplot){
       updateSelectInput(session, "customSelectHazard", choices = c(1:nrow(dataM())), selected = NULL)
     }
   })

    observe({
      if(input$runRF && input$createRSFplot){
       updateSelectInput(session, "customSelectHazardOOB", choices = c(1:nrow(dataM())), selected = NULL)
     }
   })




 output$rsfPlot <- renderHighchart({
          
    if(input$createRSFplot  && input$runRF){        
        
        if(input$selectRFPlot == 1){

              surv = data.frame(t(randomForestSurvival()$survival))

              survList = list()
              
              for(i in 1:ncol(surv)){
                
                survList[[i]] = list(data = as.matrix(cbind(randomForestSurvival()$time.interest, surv[,i])), name = paste0("Survival (obs", i, ")"), type = "line")
                
              }
              names(survList) = NULL
              
              if(input$selectObs == 1){
                  indx = 1:ncol(surv)
              }

              if(input$selectObs == 2){

                indx = c(input$fromRSF:input$toRSF)

               }     

              if(input$selectObs == 3){

                  indx = c(as.numeric(input$customSelect))

               }     

              survList =  survList[indx]

              legend = if(input$selectObs == 1){FALSE}else{TRUE}

              highchart() %>% hc_exporting(enabled = TRUE, filename = "survivalRFPlot") %>% 
                hc_add_series_list(lst =survList) %>%
                hc_chart(zoomType = "xy", inverted = FALSE) %>%
                hc_xAxis(categories = NULL, title = list(text = "Time")) %>%
                hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Survival"))%>%
                hc_legend(enabled = legend) %>%
                hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>") %>%
                hc_plotOptions(line = list(tooltip = list(pointFormat = "<b> {series.name}: </b>{point.y:.3f} <br>")), 
                               errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
                hc_add_theme(hc_theme_google())

                

          }



          else if(input$selectRFPlot == 2){

              survOob = data.frame(t(randomForestSurvival()$survival.oob))

              survListOob = list()
              
              for(i in 1:ncol(survOob)){
                
                survListOob[[i]] = list(data = as.matrix(cbind(randomForestSurvival()$time.interest, survOob[,i])), name = paste0("Survival (obs", i, ")"), type = "line")
                
              }
              names(survListOob) = NULL
              
              if(input$selectObsSurvOOB == 1){
                  indx = 1:ncol(survOob)
              }

              if(input$selectObsSurvOOB == 2){

                indx = c(input$fromRSFOOB:input$toRSFOOB)

               }     

              if(input$selectObsSurvOOB == 3){

                  indx = c(as.numeric(input$customSelectHazard))

               }     

              survListOob =  survListOob[indx]

              legend = if(input$selectObsSurvOOB == 1){FALSE}else{TRUE}

              highchart() %>% hc_exporting(enabled = TRUE, filename = "survivalRFOOBPlot") %>% 
                hc_add_series_list(lst =survListOob) %>%
                hc_chart(zoomType = "xy", inverted = FALSE) %>%
                hc_xAxis(categories = NULL, title = list(text = "Time")) %>%
                hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Survival OOB"))%>%
                hc_legend(enabled = legend) %>%
                hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>") %>%
                hc_plotOptions(line = list(tooltip = list(pointFormat = "<b> {series.name}: </b>{point.y:.3f} <br>")), 
                               errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
                hc_add_theme(hc_theme_google())

                

          }


          else if(input$selectRFPlot == 3){

              hazardRF = data.frame(t(randomForestSurvival()$chf))

              hazardList = list()
              
              for(i in 1:ncol(hazardRF)){
                
                hazardList[[i]] = list(data = as.matrix(cbind(randomForestSurvival()$time.interest, hazardRF[,i])), name = paste0("Survival (obs", i, ")"), type = "line")
                
              }
              names(hazardList) = NULL
              
              if(input$selectObsHazard == 1){
                  indx = 1:ncol(hazardRF)
              }

              if(input$selectObsHazard == 2){

                indx = c(input$fromHazard:input$toHazard)

               }     

              if(input$selectObsHazard == 3){

                  indx = c(as.numeric(input$customSelectHazard))

               }     

              hazardList =  hazardList[indx]

              legend = if(input$selectObsHazard == 1){FALSE}else{TRUE}

              highchart() %>% hc_exporting(enabled = TRUE, filename = "survivalRFPlot") %>% 
                hc_add_series_list(lst =hazardList) %>%
                hc_chart(zoomType = "xy", inverted = FALSE) %>%
                hc_xAxis(categories = NULL, title = list(text = "Time")) %>%
                hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Survival"))%>%
                hc_legend(enabled = legend) %>%
                hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>") %>%
                hc_plotOptions(line = list(tooltip = list(pointFormat = "<b> {series.name}: </b>{point.y:.3f} <br>")), 
                               errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
                hc_add_theme(hc_theme_google())

                

          }


          else 

          if(input$selectRFPlot == 4){

              hazardRFOOB = data.frame(t(randomForestSurvival()$chf.oob))

              hazardListOOB = list()
              
              for(i in 1:ncol(hazardRFOOB)){
                
                hazardListOOB[[i]] = list(data = as.matrix(cbind(randomForestSurvival()$time.interest, hazardRFOOB[,i])), name = paste0("Survival (obs", i, ")"), type = "line")
                
              }
              names(hazardListOOB) = NULL
              
              if(input$selectObsHazardOOB == 1){
                  indx = 1:ncol(hazardRFOOB)
              }

              if(input$selectObsHazardOOB == 2){

                indx = c(input$fromHazardOOB:input$toHazardOOB)

               }     

              if(input$selectObsHazardOOB == 3){

                  indx = c(as.numeric(input$customSelectHazardOOB))

               }     

              hazardListOOB =  hazardListOOB[indx]

              legend = if(input$selectObsHazardOOB == 1){FALSE}else{TRUE}

              highchart() %>% hc_exporting(enabled = TRUE, filename = "survivalRFPlot") %>% 
                hc_add_series_list(lst =hazardListOOB) %>%
                hc_chart(zoomType = "xy", inverted = FALSE) %>%
                hc_xAxis(categories = NULL, title = list(text = "Time")) %>%
                hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Survival"))%>%
                hc_legend(enabled = legend) %>%
                hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>") %>%
                hc_plotOptions(line = list(tooltip = list(pointFormat = "<b> {series.name}: </b>{point.y:.3f} <br>")), 
                               errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
                hc_add_theme(hc_theme_google())

                

          }

          else if(input$selectRFPlot == 4){

              survOob = data.frame(t(randomForestSurvival()$survival.oob))

              survListOob = list()
              
              for(i in 1:ncol(survOob)){
                
                survListOob[[i]] = list(data = as.matrix(cbind(randomForestSurvival()$time.interest, survOob[,i])), name = paste0("Survival (obs", i, ")"), type = "line")
                
              }
              names(survListOob) = NULL
              
              if(input$selectObsSurvOOB == 1){
                  indx = 1:ncol(survOob)
              }

              if(input$selectObsSurvOOB == 2){

                indx = c(input$fromRSFOOB:input$toRSFOOB)

               }     

              if(input$selectObsSurvOOB == 3){

                  indx = c(as.numeric(input$customSelectOOB))

               }     

              survListOob =  survListOob[indx]

              legend = if(input$selectObsSurvOOB == 1){FALSE}else{TRUE}

              highchart() %>% hc_exporting(enabled = TRUE, filename = "survivalRFOOBPlot") %>% 
                hc_add_series_list(lst =survListOob) %>%
                hc_chart(zoomType = "xy", inverted = FALSE) %>%
                hc_xAxis(categories = NULL, title = list(text = "Time")) %>%
                hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Survival OOB"))%>%
                hc_legend(enabled = legend) %>%
                hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>") %>%
                hc_plotOptions(line = list(tooltip = list(pointFormat = "<b> {series.name}: </b>{point.y:.3f} <br>")), 
                               errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
                hc_add_theme(hc_theme_google())

                

          }


          else if(input$selectRFPlot == 5){

              highchart() %>% hc_exporting(enabled = TRUE, filename = "errorPlot") %>% 
              hc_add_series(data = randomForestSurvival()$err.rate, type = "line", name = "Error") %>%
              hc_chart(zoomType = "xy", inverted = FALSE) %>%
              hc_xAxis(categories = NULL, title = list(text = "Number of tree")) %>%
              hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Error rate"))%>%
              hc_legend(enabled = TRUE) %>%
              hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Tree: </b>{point.x} <br>") %>%
              hc_plotOptions(line = list(tooltip = list(pointFormat = "<b> {series.name}: </b>{point.y:.3f} <br>")), 
                             errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
              hc_add_theme(hc_theme_google())

          }

          else if(input$selectRFPlot == 6){

              survivalTime = input$survivalTimeRF
              categoricalInput =input$categoricalInputRF
              continuousInput = input$continuousInputRF 
              statusVariable = input$statusVariableRF 
              status = input$statusRF
              addInteractions = input$addInteractionsRF
              twoWayinteractions = input$twoWayInteractionsRF
              threeWayinteractions = input$threeWayInteractionsRF
              customInteractions = input$customInteractionsRF
              selectCustomInteractionTerms = input$selectCustomInteractionsRF
              timeDependetCovariate = input$addTimeDependentCovariatesRF
              timeDependentVariableTransformation = input$timeDepTransformRF
              selectTimeDependentCovariate = input$selectTimeDependentVariablesRF
              strata = input$addStrataRF
              strataVariable = input$selectStrataVariableRF
              referenceCategory = input$refCategoryRF
              multipleID = input$multipleIDRF
              
              
              #if(input$runKM){
              data = dataM()
              
              
              if(!is.null(survivalTime)){
                survivalTime = as.matrix(data[, survivalTime, drop = FALSE])
                survivalTime = apply(survivalTime, 2, as.numeric)
              }
              
              if(!is.null(categoricalInput)){
                categoricalInput = as.data.frame(data[, categoricalInput, drop = FALSE])
                categoricalInput = apply(categoricalInput, 2, as.factor)
                categoricalInput = as.data.frame(categoricalInput)
                
                
              }
              
              if(!is.null(continuousInput)){
                continuousInput = as.data.frame(data[, continuousInput, drop = FALSE])
                continuousInput = apply(continuousInput, 2, as.numeric)
                continuousInput = as.data.frame(continuousInput)
              }
              
              if(!is.null(statusVariable)){
                statusVariable = as.factor(data[, statusVariable])
                
                
              }
              
              if(!is.null(status)){
                if(is.numeric(status)){status = as.factor(status)}else{status = as.factor(status)}
                
              }
              
              if(!is.null(categoricalInput) && !is.null(continuousInput)){
                newData = data.frame(id2 =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime[,1], 
                                     statusVar=statusVariable, categoricalInput, continuousInput)
                newData = newData[complete.cases(newData),]
                
              }else if(!is.null(categoricalInput) && is.null(continuousInput)){
                newData = data.frame(id2 =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime[,1], 
                                     statusVar=statusVariable, categoricalInput)
                newData = newData[complete.cases(newData),]
                
              }else if(is.null(categoricalInput) && !is.null(continuousInput)){
                newData = data.frame(id2 =seq(1,dim(survivalTime)[1], 1), survivalTime = survivalTime[,1], 
                                     statusVar=statusVariable, continuousInput)
                newData = newData[complete.cases(newData),]
                
              }
              
              
              
              
              if(referenceCategory != "first"){
                for(l in 1:dim(categoricalInput)[2]){
                  newData[, names(categoricalInput)[l]] <- relevel(categoricalInput[,l], ref = levels(categoricalInput[,l])[length(levels(categoricalInput[,l]))])
                }
              }
              
              
              if(addInteractions){
                
                if(!is.null(categoricalInput) || !is.null(continuousInput)){
                  
                  fNames <- names(c(categoricalInput, continuousInput))
                  
                }  
                
                if(twoWayinteractions && length(fNames) >1){
                  
                  twoWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 2)), paste, collapse = ":"))
                  names(twoWayInteractionTerms) <- NULL
                  
                }else{twoWayInteractionTerms = NULL}
                
                if(threeWayinteractions && length(fNames) >2){
                  
                  threeWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 3)), paste, collapse = ":"))
                  names(threeWayInteractionTerms) <- NULL
                  
                }else{threeWayInteractionTerms = NULL}  
                
                #if(customInteractions && length(fNames) >2){
                
                #   ctwoWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 2)), paste, collapse = ":"))
                #   names(twoWayInteractionTerms) <- NULL
                
                #   cthreeWayInteractionTerms <- sort(sapply(data.frame(combn(fNames, 3)), paste, collapse = ":"))
                #  names(threeWayInteractionTerms) <- NULL
                
                
                #  customInteractionTerms = c(ctwoWayInteractionTerms, cthreeWayInteractionTerms)
                # names(customInteractionTerms) = NULL
                
                
                
                #}else{customInteractionTerms = NULL}
                
                if(customInteractions){
                  
                  interactions = selectCustomInteractionTerms
                  
                }else{
                  
                  interactions = c(twoWayInteractionTerms, threeWayInteractionTerms)
                  
                } 
                
              }else{
                interactions = NULL
                customInteractionTerms= NULL
              }
              
              
              if(strata){
                
                strataVar = strataVariable
                newData = cbind.data.frame(newData, data[, strataVar])
                names(newData)[dim(newData)[2]] = strataVar
                
              }
              
              
              newData = cbind.data.frame(newData, data[colnames(data)[!(colnames(data) %in% colnames(newData))]])
              
              # if(multipleID != TRUE){  
              #   newData$statusVar = newData$statusVar%in%status
              #   
              #   newData$id <- 1:nrow(newData)
              #   cut.points <- unique(newData$survivalTime[newData$statusVar == TRUE])
              #   newData <- survSplit(formula = Surv(survivalTime, statusVar)~., data = newData, cut = cut.points, end = "stop",
              #                        start = "start", event = "statusVar")
              # }
              newData$statusVar = as.factor(newData$statusVar)%in%status
              
              if(timeDependetCovariate && !is.null(selectTimeDependentCovariate)){
                
                timeDependentCovariateNames = list()
                for(i in 1:length(selectTimeDependentCovariate)){
                  
                  if(timeDependentVariableTransformation == "log"){
                    
                    newData = cbind.data.frame(newData, tmpNames = newData[,selectTimeDependentCovariate[i]]*log(newData[, "survivalTime"]))
                    
                  }else{
                    
                    newData = cbind.data.frame(newData, tmpNames = as.numeric(newData[,selectTimeDependentCovariate[i]])*newData[, "survivalTime"])
                    
                  }
                  
                  names(newData)[dim(newData)[2]] = timeDependentCovariateNames[[i]] = paste0("time_", selectTimeDependentCovariate[i])
                  
                }
                
                timeDependentNames = unlist(timeDependentCovariateNames)    
                
              }
              
              # if(multipleID){  
              #   names(newData)[which(names(newData) == "start")] = "start2"
              #   
              #   newData <- newData %>%
              #     group_by(id) %>%
              #     mutate(start = 0:(n() - 1))
              #   
              #   newData = as.data.frame(newData)
              #   
              #   ind = row.names(newData[newData$start !=0,])
              #   
              #   newData$start[as.numeric(ind)] =  newData$survivalTime[as.numeric(ind)-1]
              # }
              
              if(!is.null(categoricalInput) || !is.null(continuousInput)){
                
                predictors = paste0(names(c(categoricalInput, continuousInput)), collapse = "+")
                
                if(!is.null(interactions)){
                  
                  
                  if(length(interactions) > 1){
                    interactions2 = paste(interactions, collapse = "+")
                    predictors2 = paste(predictors, interactions2, sep = "+", collapse = "+")
                  }    
                  
                  if(length(interactions) == 1){
                    predictors2 = paste(predictors, interactions, sep = "+")
                  }
                  predictors = predictors2
                }
                
                if(timeDependetCovariate && !is.null(selectTimeDependentCovariate)){
                  
                  
                  if(length(timeDependentNames) > 1){
                    timeDependents = paste(timeDependentNames, collapse = "+")
                  }else{
                    
                    timeDependents =  timeDependentNames
                  }
                  predictors = paste(predictors, timeDependents, sep = "+", collapse = "+")
                }
                
                if(strata && !is.null(strataVariable)){
                  
                  strataVars = paste0("strata(",strataVar,")")
                  predictors = paste(predictors, strataVars, sep = "+", collapse = "+")
                  
                } 
                
                }else{predictors = 1}
              
                formula = as.formula(paste0("Surv(survivalTime, statusVar ==  TRUE) ~ ", predictors))


                Models <- list("Cox model"=coxph(formula = formula, data=newData, y=TRUE),
                                "RSF model"=rfsrc(formula = formula, data = newData, ntree = input$ntree, bootstrap = input$bootstrap, tree.err=TRUE, 
                     importance = TRUE, membership = TRUE, statistics = TRUE, do.trace = TRUE,
                     mtry = input$mtry, nodesize = input$nodesize, nodedepth = input$nodedepth, splitrule = input$splitrule, nsplit = input$nsplit,
                     split.null = FALSE, na.action = input$naAction, nimpute = input$nimpute, proximity = input$proximity, sampsize = NULL,
                     samptype = input$samptype, case.wt = NULL, xvar.wt = NULL, forest = TRUE, var.used = FALSE, 
                     split.depth = FALSE, seed = 1234, coerce.factor = NULL, y=TRUE))

                # compute the apparent prediction error 
                PredError <- pec(object=Models,
                                 formula = formula,
                                 data=newData,
                                 exact=TRUE,
                                 cens.model="marginal",
                                 splitMethod="none",
                                 B=0,
                                 verbose=TRUE)


                modelErrors = data.frame(PredError$AppErr)

                modelErrorsList = list()

                for(i in 1:ncol(modelErrors)){
                  
                  modelErrorsList[[i]] = list(data = as.matrix(cbind(PredError$time, modelErrors[,i])), name = colnames(modelErrors[i]), type = "line")
                  
                }
                names(modelErrorsList) = NULL



                highchart() %>% hc_exporting(enabled = TRUE, filename = "modelErrorPlot") %>% 
                  hc_add_series_list(lst =modelErrorsList) %>%
                  hc_chart(zoomType = "xy", inverted = FALSE) %>%
                  hc_xAxis(categories = NULL, title = list(text = "Time")) %>%
                  hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Prediction error"))%>%
                  hc_legend(enabled = TRUE) %>%
                  hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Time: </b>{point.x} <br>") %>%
                  hc_plotOptions(line = list(tooltip = list(pointFormat = "<b> {series.name}: </b>{point.y:.3f} <br>")), 
                                 errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
                  hc_add_theme(hc_theme_google())

          }
        }
})
 
##############################################################################################
#################### Random Survival Forest (end) ############################################
##############################################################################################


##############################################################################################
#################### Regularized Cox Regression (start) ############################################
##############################################################################################



regularCoxResult <- reactive({

      survivalTimerCox = input$survivalTimerCox
      survivalStatusrCox = input$statusVariablerCox

      alpharCox = input$rAlpha

      regCoxList = list()

      if(input$selectAllVarsrCox){
      
            indx = !(colnames(dataM()) %in% c(input$survivalTimerCox, survivalStatusrCox))

      }else{


            indx = (colnames(dataM()) %in% input$selectVariablerCox)

      }


      x = data.matrix(dataM()[,indx, drop = FALSE])

      y= Surv(dataM()[,survivalTimerCox], dataM()[,survivalStatusrCox])

      set.seed(1234)
      cvFit = cv.glmnet(x, y, family = "cox", alpha = alpharCox)
      coefficients = as.data.frame(as.matrix(coef(cvFit, s = cvFit$lambda.min)))
      coefficients$`1` = as.numeric(formatC(coefficients$`1`, digits = 3, format = "f"))

      coefficients2 = data.frame(rownames(coefficients), coefficients[,1])
      coefficients3 = coefficients2[coefficients2[2] != 0,]
      colnames(coefficients3) = c("Variable", "Coefficient estimate")



      varsNotInTheModel = coefficients2[coefficients2[2] == 0,]

      if(nrow(varsNotInTheModel) > 0){
        varsNotInTheModel$coefficients...1. = formatC(varsNotInTheModel$coefficients...1., digits = 3, format = "f")
        colnames(varsNotInTheModel) = c("Variable", "Coefficient estimate")
      }else{

        varsNotInTheModel = NULL
      }

      regCoxList = list(coefficients3, varsNotInTheModel)

      return(regCoxList)


  })



output$regularCox <- DT::renderDataTable({

  if(input$runRegularized){
       datatable(regularCoxResult()[[1]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
           dom = 'Bfrtip',
           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
           )) 

    }  
 })


output$regularNoVariableCox <- DT::renderDataTable({

  if(input$runRegularized){
       datatable(regularCoxResult()[[2]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
           dom = 'Bfrtip',
           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
           )) 

    }
 })


output$regularizedPlot <- renderHighchart({

  if(input$runRegularized){
     survivalTimerCox = input$survivalTimerCox
      survivalStatusrCox = input$statusVariablerCox

      alpharCox = input$rAlpha

      regCoxList = list()

      if(input$selectAllVarsrCox){
      
            indx = !(colnames(dataM()) %in% c(input$survivalTimerCox, survivalStatusrCox))

      }else{


            indx = (colnames(dataM()) %in% input$selectVariablerCox)

      }


      x = data.matrix(dataM()[,indx, drop = FALSE])

      y= Surv(dataM()[,survivalTimerCox], dataM()[,survivalStatusrCox])

      set.seed(1234)
      cvFit = cv.glmnet(x, y, family = "cox", alpha = alpharCox, type.measure = "deviance", nfolds = as.numeric(input$nFold))


      highchart() %>% hc_exporting(enabled = TRUE, filename = "lambdaPlot") %>% 
        hc_add_series(name = "CI", type = "line", data = sort(cvFit$cvm), showInLegend = FALSE, zIndex = 1, marker = list(lineColor = "black", lineWidth = 1), lineWidth = 0, id = "survival") %>%
        hc_add_series(name = "CI", data = as.matrix(cbind(sort(cvFit$cvlo), sort(cvFit$cvup))),type = "errorbar", names = "Limits", showInLegend = FALSE, zIndex = 0, lineWidth = 1.5, linkedTo = "survival") %>%
        hc_chart(zoomType = "xy", inverted = FALSE) %>%
        hc_xAxis(categories = sort(round(log(cvFit$lambda), 1)), title = list(text = "log(Lambda)")) %>%
        hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = "Partial Likelihood Deviance")) %>%
        #hc_plotOptions(tooltip = list(headerFormat = "<b>Time: </b>{point.x}")) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>Partial Likelihood Deviance: </b>{point.x} <br>") %>%
        hc_plotOptions(line = list(tooltip = list(pointFormat = "<b>{series.name}: </b>{point.y:.3f} ")), 
                       errorbar = list(tooltip = list(pointFormat = "({point.low} - {point.high})"))) %>%
        hc_add_theme(hc_theme_google())

      }

  })

 output$varInModelText <- renderText({
     if (input$runRegularized){
         'Table 1: Variables in the model'
     }
 })

  output$crossValCurvePlot <- renderText({
     if (input$runRegularized){
         'Cross-validation curve'
     }
 })

 output$varNotInModelText <- renderText({
     if (input$runRegularized && !is.null(regularCoxResult()[[2]])){
         'Table 2: Variables NOT in the model'
     }
 })


##############################################################################################
#################### Regularized Cox Regression (end) ############################################
##############################################################################################

 
})





