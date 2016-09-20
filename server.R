shinyServer(function(input, output, session) {

library("DT")
library("survival")
library("KMsurv")
library("survMisc")
source("lifeTables.R")
source("kaplanMeier.R")
source("coxRegression.R")
source("getDescriptiveResultsCoxRegression.R")
source("stepwise.R")
require("ggplot2")
source("ggsurv.R")
source("ggsurv2.R")
source("plotSchoenfeld.R")
library("magrittr")
library("dplyr")




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
         updateSelectizeInput(session = session, inputId = "statusVariableCox", choices = colnames(data_tmp), selected = colnames(data_tmp)[2])
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
    updateSelectizeInput(session, "categoricalInput", choices = colnames(dataM()), selected = colnames(dataM())[6])
})

observe({
    updateSelectizeInput(session, "continuousInput", choices = colnames(dataM()), selected = colnames(dataM())[4])
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
 
  if(input$run){
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
    }else{
        compareCurves <- survfit(Surv(time, statusVar == TRUE) ~ 1, data = newData, conf.type = ci, error = varianceEstimation, conf.int = confidenceLevel/100)
    }


      return(compareCurves)


  })


kmPlot <- reactive({

    if(input$factorVarKM){
        fctr = input$factorKM
    }else{fctr = NULL}


  if(!is.null(fctr)){

    if(input$addCI == "Ribbon"){
        
        p <- ggsurv(kmCurves(), CI = "NULL", plot.cens = T, surv.col = 'gg.def',
        cens.col = input$censColKM, lty.est = input$ltyKM, lty.ci = input$ltyKMCI,
        cens.shape = input$censShapeKM, back.white = T, xlab = input$xlabKM,
        ylab = input$ylabKM, main = input$mainPanelKM) + theme_bw()+geom_ribbon(aes(ymin=low, ymax=up, fill=group),alpha= input$alpha, show.legend = FALSE)+guides(fill=guide_legend(input$factorKM))#+ 
        #scale_x_continuous(breaks=seq(input$xAxisLowerKM, input$xAxisUpperKM, by=round((input$xAxisUpperKM-input$xAxisLowerKM)/10,0)))+
        #scale_y_continuous(breaks=seq(min(compareCurves$surv), max(compareCurves$surv), by=((max(compareCurves$surv)-min(compareCurves$surv))/10)))

        
    }
    
    if(input$addCI == "Line"){
        
        p <- ggsurv(kmCurves(), CI = T, plot.cens = T, surv.col = 'gg.def',
        cens.col = input$censColKM, lty.est = input$ltyKM, lty.ci = input$ltyKMCI,
        cens.shape = input$censShapeKM, back.white = T, xlab = input$xlabKM,
        ylab = input$ylabKM, main = input$mainPanelKM)+guides(fill=guide_legend(input$factorKM))#+ 
        #scale_x_continuous(breaks=seq(min(compareCurves$time), max(compareCurves$time), by=round((max(compareCurves$time)-min(compareCurves$time))/10,0)))+
        #scale_y_continuous(breaks=seq(min(compareCurves$surv), max(compareCurves$surv), by=((max(compareCurves$surv)-min(compareCurves$surv))/10)))

        
    }
    
    if(input$addCI == "None"){
        
        p <- ggsurv(kmCurves(), CI = 'def', plot.cens = T, surv.col = 'gg.def',
        cens.col = input$censColKM, lty.est = input$ltyKM, lty.ci = input$ltyKMCI,
        cens.shape = input$censShapeKM, back.white = F, xlab = input$xlabKM,
        ylab = input$ylabKM, main = input$mainPanelKM) + theme_bw()#+ 
        #scale_x_continuous(breaks=seq(min(compareCurves$time), max(compareCurves$time), by=round((max(compareCurves$time)-min(compareCurves$time))/10,0)))+
        #scale_y_continuous(breaks=seq(min(compareCurves$surv), max(compareCurves$surv), by=((max(compareCurves$surv)-min(compareCurves$surv))/10)))

        
    }
    
  }else{
    
    if(input$addCI == "Ribbon"){
        
        p <- ggsurv(kmCurves(), CI = "NULL", plot.cens = T, surv.col = input$curveColKM,
        cens.col = input$censColKM, lty.est = input$ltyKM, lty.ci = input$ltyKMCI,
        cens.shape = input$censShapeKM, back.white = T, xlab = input$xlabKM,
        ylab = input$ylabKM, main = input$mainPanelKM) + theme_bw()+geom_ribbon(aes(ymin=low, ymax=up, fill= "red"),alpha= input$alpha, show.legend = FALSE)
       # + scale_x_continuous(breaks=seq(min(compareCurves$time), max(compareCurves$time), by=round((max(compareCurves$time)-min(compareCurves$time))/10,0)))+
       # scale_y_continuous(breaks=seq(min(compareCurves$surv), max(compareCurves$surv), by=((max(compareCurves$surv)-min(compareCurves$surv))/10)))

    }
    
    if(input$addCI == "Line"){
        
        p <- ggsurv(kmCurves(), CI = T, plot.cens = T, surv.col = input$curveColKM,
        cens.col = input$censColKM, lty.est = input$ltyKM, lty.ci = input$ltyKMCI,
        cens.shape = input$censShapeKM, back.white = T, xlab = input$xlabKM,
        ylab = input$ylabKM, main = input$mainPanelKM)#+ 
        #scale_x_continuous(breaks=seq(min(compareCurves$time), max(compareCurves$time), by=round((max(compareCurves$time)-min(compareCurves$time))/10,0)))+
        #scale_y_continuous(breaks=seq(min(compareCurves$surv), max(compareCurves$surv), by=((max(compareCurves$surv)-min(compareCurves$surv))/10)))

        
    }
    
    if(input$addCI == "None"){
        
        p <- ggsurv(kmCurves(), CI = "NULL", plot.cens = T, surv.col = input$curveColKM,
        cens.col = input$censColKM, lty.est = input$ltyKM, lty.ci = input$ltyKMCI,
        cens.shape = input$censShapeKM, back.white = F, xlab = input$xlabKM,
        ylab = input$ylabKM, main = input$mainPanelKM) + theme_bw()#+ 
        #scale_x_continuous(breaks=seq(min(compareCurves$time), max(compareCurves$time), by=round((max(compareCurves$time)-min(compareCurves$time))/10,0)))+
        #scale_y_continuous(breaks=seq(min(compareCurves$surv), max(compareCurves$surv), by=((max(compareCurves$surv)-min(compareCurves$surv))/10)))

        
        
    }
}


plotly::ggplotly(p)


})



hazardPlot <- reactive({
    

    if(input$factorVarKM){
        fctr = input$factorKM
    }else{fctr = NULL}

    
    hcompareCurves = kmCurves()
    
    hcompareCurves$surv = -log(hcompareCurves$surv)
    hcompareCurves$lower = -log(hcompareCurves$lower)
    hcompareCurves$upper = -log(hcompareCurves$upper)
    
    
    if(!is.null(fctr)){
        
        if(input$addCIhazard == "Ribbon"){
            
            ph <- ggsurv2(hcompareCurves, CI = "NULL", plot.cens = T, surv.col = 'gg.def',
            cens.col = input$censColHazard, lty.est = input$ltyHazard, lty.ci = input$ltyHazardCI,
            cens.shape = input$censShapeHazard, back.white = T, xlab = input$xlabHazard,
            ylab = input$ylabHazard, main = input$mainPanelHazard) + theme_bw()+geom_ribbon(aes(ymin=low, ymax=up, fill=group),alpha= input$alphaHazard, show.legend = FALSE)+guides(fill=guide_legend(input$factorKM))
            #+ scale_x_continuous(breaks=seq(min(hcompareCurves$time), max(hcompareCurves$time), by=round((max(hcompareCurves$time)-min(hcompareCurves$time))/10,0)))+
            #scale_y_continuous(breaks=seq(min(hcompareCurves$surv), max(hcompareCurves$surv), by=((max(hcompareCurves$surv)-min(hcompareCurves$surv))/10)))

        }
        
        if(input$addCIhazard == "Line"){
            
            ph <- ggsurv2(hcompareCurves, CI = T, plot.cens = T, surv.col = 'gg.def',
            cens.col = input$censColHazard, lty.est = input$ltyHazard, lty.ci = input$ltyHazardCI,
            cens.shape = input$censShapeHazard, back.white = T, xlab = input$xlabHazard,
            ylab = input$ylabHazard, main = input$mainPanelHazard)+guides(fill=guide_legend(input$factorKM))
            #  + scale_x_continuous(breaks=seq(min(hcompareCurves$time), max(hcompareCurves$time), by=round((max(hcompareCurves$time)-min(hcompareCurves$time))/10,0)))+
           # scale_y_continuous(breaks=seq(min(hcompareCurves$surv), max(hcompareCurves$surv), by=((max(hcompareCurves$surv)-min(hcompareCurves$surv))/10)))

            
        }
        
        if(input$addCIhazard == "None"){
            
            ph <- ggsurv2(hcompareCurves, CI = 'def', plot.cens = T, surv.col = 'gg.def',
            cens.col = input$censColHazard, lty.est = input$ltyHazard, lty.ci = input$ltyHazardCI,
            cens.shape = input$censShapeHazard, back.white = F, xlab = input$xlabHazard,
            ylab = input$ylabHazard, main = input$mainPanelHazard) + theme_bw()+guides(fill=guide_legend(input$factorKM))
            #  + scale_x_continuous(breaks=seq(min(hcompareCurves$time), max(hcompareCurves$time), by=round((max(hcompareCurves$time)-min(hcompareCurves$time))/10,0)))+
            #scale_y_continuous(breaks=seq(min(hcompareCurves$surv), max(hcompareCurves$surv), by=((max(hcompareCurves$surv)-min(hcompareCurves$surv))/10)))

            
        }
        
    }else{
        
        if(input$addCIhazard == "Ribbon"){
            
            ph <- ggsurv2(hcompareCurves, CI = "NULL", plot.cens = T, surv.col = input$curveColHazard,
            cens.col = input$censColHazard, lty.est = input$ltyHazard, lty.ci = input$ltyHazardCI,
            cens.shape = input$censShapeHazard, back.white = T, xlab = input$xlabHazard,
            ylab = input$ylabHazard, main = input$mainPanelHazard) + theme_bw()+geom_ribbon(aes(ymin=low, ymax=up, fill= "red"),alpha= input$alphaHazard, show.legend = FALSE)
            #  + scale_x_continuous(breaks=seq(min(hcompareCurves$time), max(hcompareCurves$time), by=round((max(hcompareCurves$time)-min(hcompareCurves$time))/10,0)))+
            #scale_y_continuous(breaks=seq(min(hcompareCurves$surv), max(hcompareCurves$surv), by=((max(hcompareCurves$surv)-min(hcompareCurves$surv))/10)))

            
        }
        
        if(input$addCIhazard == "Line"){
            
            ph <- ggsurv2(hcompareCurves, CI = T, plot.cens = T, surv.col = input$curveColHazard,
            cens.col = input$censColHazard, lty.est = input$ltyHazard, lty.ci = input$ltyHazardCI,
            cens.shape = input$censShapeHazard, back.white = T, xlab = input$xlabHazard,
            ylab = input$ylabHazard, main = input$mainPanelHazard)
            #  + scale_x_continuous(breaks=seq(min(hcompareCurves$time), max(hcompareCurves$time), by=round((max(hcompareCurves$time)-min(hcompareCurves$time))/10,0)))+
            #scale_y_continuous(breaks=seq(min(hcompareCurves$surv), max(hcompareCurves$surv), by=((max(hcompareCurves$surv)-min(hcompareCurves$surv))/10)))

            
        }
        
        if(input$addCIhazard == "None"){
            
            ph <- ggsurv2(hcompareCurves, CI = "NULL", plot.cens = T, surv.col = input$curveColHazard,
            cens.col = input$censColHazard, lty.est = input$ltyHazard, lty.ci = input$ltyHazardCI,
            cens.shape = input$censShapeHazard, back.white = F, xlab = input$xlabHazard,
            ylab = input$ylabHazard, main = input$mainPanelHazard) + theme_bw()
            #  + scale_x_continuous(breaks=seq(min(hcompareCurves$time), max(hcompareCurves$time), by=round((max(hcompareCurves$time)-min(hcompareCurves$time))/10,0)))+
            #scale_y_continuous(breaks=seq(min(hcompareCurves$surv), max(hcompareCurves$surv), by=((max(hcompareCurves$surv)-min(hcompareCurves$surv))/10)))

            
            
        }
    }
    
    
    plotly::ggplotly(ph)
    
    
    
    
})



 output$kmCurvePlot <- plotly::renderPlotly({
     
     if(input$createKmPlot  && input$selectPlotKM == '1'){
         
         kmPlot()
         
     }
     
    else if(input$createKmPlot && input$selectPlotKM == '2'){
         
         hazardPlot()
         
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

            
            #print(plotly::ggplotly(
              sp = ggplot2::ggplot(newData, aes(x=xx, y=y)) +
                   geom_line(data=newData3, aes(x=pred.x, y=yhat), lty = ltyest, color = colorLine) +   
                   scale_x_continuous(breaks = xaxisval, labels = xaxislab)
                      #))
            
    
            if (resid){
              sp = sp+geom_point(shape=1)
            }
            
           if (se){
              
                sp = sp + geom_line(data=newData2, aes(x=pred.x, y=yup), lty = ltyci, color = colorLineCI) +
                          geom_line(data=newData2, aes(x=pred.x, y=ylow), lty = ltyci, color = colorLineCI) 
              
            }
            
             ylabel =  paste0(input$ylabSchoenfeld, svar)
    
            sp2 = sp + xlab(input$xlabSchoenfeld) + ylab(ylabel) + ggtitle(input$mainPanelSchoenfeld) + theme_bw()
            
            
            #xlab = "Time"
            #ylab = paste0("Scaled Schoenfeld residuals for ", colnames(yy)[i])            
            
           #ply <- plotly::ggplotly(sp)
           #print(ply)
           
            #plotly::ggplotly(sp)
              
          }
    
    }else{

        sp2 = NULL      
    } 

    return(sp2)   #schoenfeldPlotReactive()
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

    }

      compareCurves$surv = log(-log(compareCurves$surv))



    }else{

      compareCurves = NULL
    }


    return(compareCurves)


  })


kmPlotCoxReactive <- reactive({


    if(!is.null(kmCurvesCoxReactive())){

        s = kmCurvesCoxReactive()
            #scale_x_continuous(breaks=seq(min(compareCurves$time), max(compareCurves$time), by=round((max(compareCurves$time)-min(compareCurves$time))/10,0)))+
            #scale_y_continuous(breaks=seq(min(compareCurves$surv), max(compareCurves$surv), by=((max(compareCurves$surv)-min(compareCurves$surv))/10)))

            
        cens.col = input$censColKMph
        lty.est = input$ltyKMph
        cens.shape = input$censShapeKMph
        back.white = input$themeKmCox
        xlab = input$xlabKMph
        ylab = input$ylabKMph
        main = input$mainPanelKMph
        surv.col = 'gg.def'
        CI = 'def'


        strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
        stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
        stopifnot(length(lty.est) == 1 | length(lty.est) == strata)


        n <- s$strata

        groups <- factor(unlist(strsplit(names
                                         (s$strata), '='))[seq(2, 2*strata, by = 2)])
        gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
        gr.df <- vector('list', strata)
        ind <- vector('list', strata)
        n.ind <- c(0,n); n.ind <- cumsum(n.ind)
        for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]

        for(i in 1:strata){
          gr.df[[i]] <- data.frame(
            time = c(s$time[ind[[i]]]),
            surv = c(s$surv[ind[[i]]]),
            up = c(s$upper[ind[[i]]]),
            low = c(s$lower[ind[[i]] ]),
            cens = c(s$n.censor[ind[[i]]]),
            group = rep(groups[i],n[i]))
        }

        dat <- do.call(rbind, gr.df)
        dat.cens <- subset(dat, cens != 0)

        pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
          xlab(xlab) + ylab(ylab) + ggtitle(main) +
          geom_step(aes(col = group, lty = group))

        col <- if(length(surv.col == 1)){
          scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
        } else{
          scale_colour_manual(name = gr.name, values = surv.col)
        }

        pl <- if(surv.col[1] != 'gg.def'){
          pl + col
        } else {pl + scale_colour_discrete(name = gr.name)}

        line <- if(length(lty.est) == 1){
          scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
        } else {scale_linetype_manual(name = gr.name, values = lty.est)}

        pl <- pl + line

        pl <- if(CI == T) {
          if(length(surv.col) > 1 && length(lty.est) > 1){
            stop('Either surv.col or lty.est should be of length 1 in order
                 to plot 95% CI with multiple strata')
          }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
            pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
              geom_step(aes(y = low, color = group), lty = lty.ci)
          } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
              geom_step(aes(y = low,lty = group), col = surv.col)}
        } else {pl}


        #pl <- if(plot.cens == T & length(dat.cens) > 0){
        #  pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
        #                  col = cens.col)
        #} else if (plot.cens == T & length(dat.cens) == 0){
        #  stop ('There are no censored observations')
        #} else(pl)

        pl <- if(back.white == T) {pl + theme_bw()
        } else (pl)
        pl

              plotly::ggplotly(pl)
            }

        })


         output$phPlots <- plotly::renderPlotly({

                if(input$selectPlotCox == 1 && input$runCox){
                    plotly::ggplotly(schoenfeldPlot())
                }

                else if(input$selectPlotCox == 2 && input$runCox){

                    kmPlotCoxReactive()
              }

          })

          output$str <- renderPrint({
            categoriesCox()

          })





 
})





