kaplanMeier <- function(survivalTime, statusVariable, status, factors, survivalTable = TRUE, caseSummary = TRUE, hr=TRUE, 
                        meanMedianSurvivalTimes = TRUE, quartilesOfSurvivalTimes = FALSE, ci = "log", 
                        varianceEstimation = "greenwood", comparisonTest = "logRank", confidenceLevel = 95,
                        referenceCategory = "first", typeOfTest = "asymptotic", kmCurve = TRUE, p= 1, q = 1, data = dataSet){
  
fname = factors
if(!is.null(survivalTime)){
  survivalTime = as.matrix(data[, survivalTime, drop = FALSE])
  survivalTime = apply(survivalTime, 2, as.numeric)
  
}

if(!is.null(factors)){
  factors = as.factor(data[, factors])
}

if(!is.null(factors)){
  factorsName = data[, factors, drop = FALSE]
}

if(referenceCategory != "first" && !is.null(factors)){
  factors <- factor(factors, levels=rev(levels(factors)))
}


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
  #newData$survivalTime = as.numeric(newData$survivalTime)
  colnames(newData) = c("id", "time", "statusVar", "factor")

  
}else{
  
  newData = data.frame(id =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime, 
                       statusVar=statusVariable)
  newData = newData[complete.cases(newData),]
  # newData$survivalTime = as.numeric(newData$survivalTime)
  
  colnames(newData) = c("id", "time", "statusVar")

  
}

newData$statusVar = newData$statusVar%in%status


if(!is.null(factors)){
  
  
  if(caseSummary){
    
    splitFactor = split(newData, newData$factor)
    
    caseSummary <- lapply(splitFactor, FUN = function(x){
      n = nrow(x)
      nOfEvent =  as.numeric(table(x$statusVar)[as.factor(names(table(x$statusVar))) %in% TRUE][[1]])
      percentOfEvent = as.numeric(formatC((nOfEvent/n)*100, digits = 3, format = "f"))

      if(percentOfEvent == 100){
          nOfCensor = as.numeric(formatC(0, digits = 3, format = "f"))
          percentOfCensor = as.numeric(formatC(0, digits = 3, format = "f"))
      }else{
        
        nOfCensor =  as.numeric(table(x$statusVar)[(!(names(table(x$statusVar))) %in% TRUE)][[1]])
        percentOfCensor = as.numeric(formatC((nOfCensor/n)*100, digits = 3, format = "f"))
        
      }
      caseSummary = data.frame(n,nOfEvent, percentOfEvent, nOfCensor, percentOfCensor)
      colnames(caseSummary) = c("n", "n of event", "% of event", "n of censor", "% of censor")
      
      return(caseSummary)
      
    })
    
  }
  
  assign("newData", newData, envir=.GlobalEnv)  # put the dat in the global env
  compareCurves <- survfit(Surv(time, statusVar) ~ factor, data = newData, conf.type = ci, error = varianceEstimation, conf.int = confidenceLevel/100)
  
  
  
  summary = summary(compareCurves, rmean = "individual")
  
  if(survivalTable){
    survivalTableResult = data.frame(summary[c(2:5,7,8,10,9)])
    survivalTableResult$surv = as.numeric(formatC(survivalTableResult$surv, digits = 3, format = "f"))
    survivalTableResult$std.err = as.numeric(formatC(survivalTableResult$std.err, digits = 3, format = "f"))
    survivalTableResult$upper = as.numeric(formatC(survivalTableResult$upper, digits = 3, format = "f"))
    survivalTableResult$lower = as.numeric(formatC(survivalTableResult$lower, digits = 3, format = "f"))
    
    survivalTableResult = split(survivalTableResult[-5], survivalTableResult$strata)
    names(survivalTableResult) = levels(factors)
    
    survivalTableLastResult = lapply(survivalTableResult, function(x)
    {
      colnames(x) = c("Time", "Number at risk", "Number of event", "Cumulative probability of surviving", "S.E.", "Lower limit", "Upper limit")
      return(x)
    }
    )
    
  }else{
    survivalTableLastResult = NULL
  }
  
  if(hr){
    
    if(!is.null(survivalTableResult)){
      hazardRatio = lapply(survivalTableResult, function(x)
      {
        survHat = x$surv
        lowerLimit = x$upper
        upperLimit = x$lower
        hazard = data.frame(Time = x$time, "Hazard Ratio" = as.numeric(formatC(-log(survHat), digits = 3, format = "f")), Lower = as.numeric(formatC(-log(lowerLimit), digits = 3, format = "f")), Upper = as.numeric(formatC(-log(upperLimit), digits = 3, format = "f")))
        
        return(hazard)
        
      })
    }else{hazardRatio = NULL}
    
  }else{hazardRatio = NULL}
  
  
  if(meanMedianSurvivalTimes){
    
    meanMedian = as.data.frame(summary$table[,-c(2:4)])
    meanMedian$meanLL = meanMedian$`*rmean` - qnorm(1-((1-confidenceLevel/100)/2))*meanMedian$`*se(rmean)`
    meanMedian$meanUL = meanMedian$`*rmean` + qnorm(1-((1-confidenceLevel/100)/2))*meanMedian$`*se(rmean)`
    meanMedian$factor = levels(factors)
    row.names(meanMedian) = NULL
    meanMedian2 = meanMedian[c(9,1,2:3,7:8,4:6)]
    colnames(meanMedian2) = c("Factor", "n", "Mean", "S.E.(mean)", "Lower (mean)", "Upper (mean)", "Median", "Lower (median)", "Upper (median)")
    meanMedian2$Mean = as.numeric(formatC(meanMedian2$Mean, digits = 3, format ="f"))
    meanMedian2$`S.E.(mean)` = as.numeric(formatC(meanMedian2$`S.E.(mean)`, digits = 3, format ="f"))
    meanMedian2$`Lower (mean)` = as.numeric(formatC(meanMedian2$`Lower (mean)`, digits = 3, format ="f"))
    meanMedian2$`Upper (mean)` = as.numeric(formatC(meanMedian2$`Upper (mean)`, digits = 3, format ="f"))
    meanMedian2$Median = as.numeric(formatC(meanMedian2$Median, digits = 3, format ="f"))
    meanMedian2$`Lower (median)` = as.numeric(formatC(meanMedian2$`Lower (median)`, digits = 3, format ="f"))
    meanMedian2$`Upper (median)` = as.numeric(formatC(meanMedian2$`Upper (median)`, digits = 3, format ="f"))
    
  }else{meanMedian2 = NULL}
  
  if(quartilesOfSurvivalTimes){
    
    quan = as.data.frame(quantile(compareCurves)$quantile)
    names(quan) = c("25%", "50% (median)", "75%")
    quan2 = cbind(Factor = levels(factors), quan)
    row.names(quan2) = NULL
    
  }else{quan2 = NULL}    
  
  comps = ten(compareCurves)
  comp(comps, p = p, q = q)
  comparisonTests = as.data.frame(attr(comps, "lrt"))

  
  
  #$tests$lrTests
  
  #if(comparisonTest == "logRank"){
    lr = data.frame(cbind(Test = "Log-rank", Chi_square= as.numeric(formatC(comparisonTests[1,6], digits = 3, format = "f")), DF = comparisonTests[1,7], p_value= as.numeric(formatC(comparisonTests[1,8], digits = 3, format = "f")))) 
  #}
  
  #if(comparisonTest == "gehanBreslow"){
    gb = data.frame(cbind(Test = "Gehan-Breslow", Chi_square= as.numeric(formatC(comparisonTests[2,6], digits = 3, format = "f")), DF = comparisonTests[2,7], p_value= as.numeric(formatC(comparisonTests[2,8], digits = 3, format = "f")))) 
  #}
  
  #if(comparisonTest == "taroneWare"){
    tw = data.frame(cbind(Test = "Tarone-Ware", Chi_square= as.numeric(formatC(comparisonTests[3,6], digits = 3, format = "f")), DF = comparisonTests[3,7], p_value= as.numeric(formatC(comparisonTests[3,8], digits = 3, format = "f")))) 
  #}
  
  #if(comparisonTest == "petoPeto"){
    pp = data.frame(cbind(Test = "Peto-Peto", Chi_square= as.numeric(formatC(comparisonTests[4,6], digits = 3, format = "f")), DF = comparisonTests[4,7], p_value= as.numeric(formatC(comparisonTests[4,8], digits = 3, format = "f")))) 
  #}
  
  #if(comparisonTest == "modPetoPeto"){
    mpp = data.frame(cbind(Test = "Modified Peto-Peto", Chi_square= as.numeric(formatC(comparisonTests[5,6], digits = 3, format = "f")), DF = comparisonTests[5,7], p_value= as.numeric(formatC(comparisonTests[5,8], digits = 3, format = "f")))) 
  #}
  
  #if(comparisonTest == "flemingtonHarnington"){
    fh = data.frame(cbind(Test = paste0("Flemington-Harnington p = ",p, " q = ", q), Chi_square= as.numeric(formatC(comparisonTests[6,6], digits = 3, format = "f")), DF = comparisonTests[6,7], p_value= as.numeric(formatC(comparisonTests[6,8], digits = 3, format = "f")))) 
  #}
    testResults = rbind.data.frame(lr, gb, tw, pp, mpp, fh)

    colnames(testResults) = c("Test", "Chi square", "df", "p value")

}else{
  
  if(caseSummary){
    
    n = nrow(newData)
    nOfEvent =  as.numeric(table(newData$statusVar)[as.factor(names(table(newData$statusVar))) %in% TRUE][[1]])
    percentOfEvent = as.numeric(formatC((nOfEvent/n)*100, digits = 3, format = "f"))
    nOfCensor =  as.numeric(table(newData$statusVar)[(!(names(table(newData$statusVar))) %in% TRUE)][[1]])
    percentOfCensor = as.numeric(formatC((nOfCensor/n)*100, digits = 3, format = "f"))
    caseSummary = data.frame(n,nOfEvent, percentOfEvent, nOfCensor, percentOfCensor)
    colnames(caseSummary) = c("n", "n of event", "% of event", "n of censor", "% of censor")
    
  }
  
  
  assign("newData", newData, envir=.GlobalEnv)  # put the dat in the global env
  compareCurves <- survfit(Surv(time, statusVar == TRUE) ~ 1, data = newData, conf.type = ci, error = varianceEstimation, conf.int = confidenceLevel/100)
  
    summary = summary(compareCurves, rmean = "individual")
  
  if(survivalTable){
    survivalTableResult = data.frame(summary[c(2:5,7,8,10,9)])
    survivalTableResult$surv = as.numeric(formatC(survivalTableResult$surv, digits = 3, format = "f"))
    survivalTableResult$std.err = as.numeric(formatC(survivalTableResult$std.err, digits = 3, format = "f"))
    survivalTableResult$upper = as.numeric(formatC(survivalTableResult$upper, digits = 3, format = "f"))
    survivalTableResult$lower = as.numeric(formatC(survivalTableResult$lower, digits = 3, format = "f"))
    
    survivalTableLastResult = survivalTableResult[-8]
    colnames(survivalTableLastResult) = c("Time", "Number at risk", "Number of event", "Cumulative probability of surviving", "S.E.", "Upper limit", "Lower limit")      
  }else{
    survivalTableLastResult = NULL
  }
  
  
  if(hr){
    
    if(!is.null(survivalTableResult)){
      
      survHat = survivalTableResult$surv
      lowerLimit = survivalTableResult$upper
      upperLimit = survivalTableResult$lower
      
      
      hazardRatio = data.frame(Time = survivalTableResult$time, Hazard = as.numeric(formatC(-log(survHat), digits = 3, format = "f")))
      
      
    }else{hazardRatio = NULL}
    
  }else{hazardRatio = NULL}
  
  if(meanMedianSurvivalTimes){
    
    meanMedian = as.data.frame(t(summary$table))
    meanMedian$meanLL = meanMedian$`*rmean` - qnorm(1-((1-confidenceLevel/100)/2))*meanMedian$`*se(rmean)`
    meanMedian$meanUL = meanMedian$`*rmean` + qnorm(1-((1-confidenceLevel/100)/2))*meanMedian$`*se(rmean)`
    row.names(meanMedian) = NULL
    meanMedian2 = meanMedian[c(1,5:6,10:11,7:9)]
    colnames(meanMedian2) = c("n", "Mean", "S.E.(mean)", "Lower (mean)", "Upper (mean)", "Median", "Lower (median)", "Upper (median)")
    meanMedian2$Mean = as.numeric(formatC(meanMedian2$Mean, digits = 3, format ="f"))
    meanMedian2$`S.E.(mean)` = as.numeric(formatC(meanMedian2$`S.E.(mean)`, digits = 3, format ="f"))
    meanMedian2$`Lower (mean)` = as.numeric(formatC(meanMedian2$`Lower (mean)`, digits = 3, format ="f"))
    meanMedian2$`Upper (mean)` = as.numeric(formatC(meanMedian2$`Upper (mean)`, digits = 3, format ="f"))
    meanMedian2$Median = as.numeric(formatC(meanMedian2$Median, digits = 3, format ="f"))
    meanMedian2$`Lower (median)` = as.numeric(formatC(meanMedian2$`Lower (median)`, digits = 3, format ="f"))
    meanMedian2$`Upper (median)` = as.numeric(formatC(meanMedian2$`Upper (median)`, digits = 3, format ="f"))
    
  }else{meanMedian2 = NULL}
  
  
  if(quartilesOfSurvivalTimes){
    
    quan2 = as.data.frame(t(quantile(compareCurves)$quantile))
    names(quan2) = c("25%", "50% (median)", "75%")
    row.names(quan2) = NULL
    
  }else{quan2 = NULL}    
  
  testResults = NULL
}


result = list(tableResult = list(caseSummary = caseSummary, meanMedianSurvivalTimes = meanMedian2, quartilesOfSurvivalTimes = quan2), testResult = list(testResults = testResults, survivalTable = survivalTableLastResult, hazardRatio = hazardRatio))
return(result)
}

