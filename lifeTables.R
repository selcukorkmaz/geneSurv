

lifeTables <- function(survivalTime, statusVariable, status, factors, fromTime = 0, toTime = 60, by = 12,
                      lifeTable = TRUE, descriptives = TRUE, hr=TRUE, medianLifeTime = FALSE, ci = "log",
                      varianceEstimation = "greenwood", compare = TRUE, comparisonTest = "logRank", confidenceLevel = 95,
                      referenceCategory = "first", typeOfTest = "asymptotic", q = 1, p = 1, data = dataSet){
  
  
  
  if(!is.null(survivalTime)){
    survivalTime = as.matrix(data[, survivalTime, drop = FALSE])
    survivalTime = apply(survivalTime, 2, as.numeric)

  }
  
  if(!is.null(factors)){
    factors = as.factor(data[, factors])
  }
  
  if(referenceCategory != "first"){
    factors <- factor(factors, levels=rev(levels(factors)))
  }
  
  if(!is.null(statusVariable)){
    statusVariable = data[, statusVariable]
    
  }
  
  if(!is.null(status)){
    if(is.numeric(status)){status = as.numeric(status)}else{status = as.factor(status)}
  }
  
  
  if(!is.null(factors)){
    newData = cbind.data.frame(id =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime[,1], 
                         statusVar=statusVariable,factor = factors)
    newData = newData[complete.cases(newData),]
    
  }else{
    
    newData = data.frame(id =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime[,1], 
                         statusVar=statusVariable)
    newData = newData[complete.cases(newData),]
    
  }
  
  newData$statusVar = newData$statusVar%in%status


  if(is.null(factors)){
    if(lifeTable){  
    tis <- seq(fromTime, toTime, by)
    
    nevent = 0
    nlost = 0
    nsubs = dim(newData)[1]
    
    lifeTableFormat = list()
    
    for(i in 1:(length(tis)-1)){
      
      subData = newData[newData$survivalTime>=tis[i] & newData$survivalTime<tis[i+1],]
      time = paste(tis[i], tis[i+1], sep = "_<")
      nsubs = nsubs-nevent-nlost
      nevent = sum(subData$statusVar) 
      nlost = nrow(subData)-nevent 
      lifeTableFormat [[i]] = data.frame(time, nsubs, nlost, nevent)
      
    }
    
    lifeTableResult <- do.call(rbind.data.frame, lifeTableFormat)
    lifeTableResult$nrisk = as.numeric(formatC(lifeTableResult$nsubs - (lifeTableResult$nlost/2), digits = 3, format = "f"))
    lifeTableResult$qt = formatC(lifeTableResult$nevent/lifeTableResult$nrisk, digits = 3, format = "f")
    lifeTableResult$pt = formatC(1-as.numeric(lifeTableResult$qt), digits = 3, format = "f")
    lifeTableResult$Pt[1] = 1
    
    for(j in  2:nrow(lifeTableResult)){
      lifeTableResult$Pt[j] =  formatC(as.numeric(lifeTableResult$pt[j-1])*as.numeric(lifeTableResult$Pt[j-1]), digits = 3, format = "f")
    }
    
    lifeTableRes = lifeTableResult[c(1:3,5,4,6:8)]
    colnames(lifeTableRes) = c("interval time", "n of subjects", "n of withdraws", "n of subjects at risk", "n of events", "prob. of terminating during the time interval", "prob. of surviving in the time interval", "cum. prob. of surviving at the begining of the time interval")
    
    
    }else{lifeTableRes = NULL}
    
    if(hr){
      surv = as.data.frame(KMsurv::lifetab(tis, lifeTableResult$nsubs[1], lifeTableResult$nlost, lifeTableResult$nevent))
      hazardRatio = surv[c(7,10)]
      hazardRatio$lowerLimit = as.numeric(formatC(hazardRatio$hazard - qnorm(1-(1-confidenceLevel/100)/2)*hazardRatio$se.hazard, digits = 3, format = "f"))
      hazardRatio$upperLimit = as.numeric(formatC(hazardRatio$hazard + qnorm(1-(1-confidenceLevel/100)/2)*hazardRatio$se.hazard, digits = 3, format = "f"))
      hazardRatio$hazard = as.numeric(formatC(hazardRatio$hazard, digits = 3, format = "f"))
      hazardRatio$se.hazard = as.numeric(formatC(hazardRatio$se.hazard, digits = 3, format = "f"))
      row.names(hazardRatio) = NULL
      hazardRatio = data.frame(time = lifeTableResult$time, hazardRatio)
      colnames(hazardRatio) = c("Interval time", "HR", "S.E.", "Lower limit", "Upper limit")
    }else{hazardRatio=NULL}
    
    if(medianLifeTime){
      surv$time = rownames(surv)
      rownames(surv) = NULL
      subSurv = surv[surv$surv > 0.5,]
      subSurv2 = subSurv[dim(subSurv)[1],]
      rowNumber = as.numeric(rownames(subSurv2))
      subSurv3 = surv[rowNumber:(rowNumber+1),]
      tj = as.numeric(strsplit(subSurv3$time[1], "-")[[1]][1]) 
      tj1 = as.numeric(strsplit(subSurv3$time[1], "-")[[1]][2])    
      stj = subSurv3$surv[1]
      stj1 = subSurv3$surv[2]
      wj = tj1-tj
      medianLifeTimeResult = as.data.frame(formatC(tj+((stj - 0.5)*wj)/(stj-stj1), digits = 3 , format = "f"))
      colnames(medianLifeTimeResult) = "Median life time"
    }else{
      
      medianLifeTimeResult = NULL
    }
    
  }else{
    splitDF = split(newData, newData$factor)
    
    lifeTableResult <- lapply(splitDF, FUN = function(x){
      tis <- seq(fromTime, toTime, by)
      nevent = 0
      nlost = 0
      nsubs = dim(x)[1]
      lifeTableFormat = list()
      
      for(i in 1:(length(tis)-1)){
        subData = x[x$survivalTime>=tis[i] & x$survivalTime<tis[i+1],]
        time = paste(tis[i], tis[i+1], sep = "_<")
        nsubs = nsubs-nevent-nlost
        nevent = sum(subData$statusVar) 
        nlost = nrow(subData)-nevent 
        lifeTableFormat[[i]] = data.frame(time, nsubs, nlost, nevent)
      }
      
      lifeTableResult <- do.call(rbind.data.frame, lifeTableFormat)
      lifeTableResult$nrisk = as.numeric(formatC(lifeTableResult$nsubs - (lifeTableResult$nlost/2), digits = 3, format = "f"))
      lifeTableResult$qt = formatC(lifeTableResult$nevent/lifeTableResult$nrisk, digits = 3, format = "f")
      lifeTableResult$pt = formatC(1-as.numeric(lifeTableResult$qt), digits = 3, format = "f")
      lifeTableResult$Pt[1] = 1
      
      for(j in  2:nrow(lifeTableResult)){
        lifeTableResult$Pt[j] =  formatC(as.numeric(lifeTableResult$pt[j-1])*as.numeric(lifeTableResult$Pt[j-1]), digits = 3, format = "f")
      }
      return(lifeTableResult)
    })
    
    
    if(hr){
      hazardRatio <- lapply(lifeTableResult, FUN = function(y){
        tis <- seq(fromTime, toTime, by)
        surv = as.data.frame(KMsurv::lifetab(tis, y$nsubs[1], y$nlost, y$nevent))
        hazardRatio = surv[c(7,10)]
        hazardRatio$lowerLimit = as.numeric(formatC(hazardRatio$hazard - qnorm(1-(1-confidenceLevel/100)/2)*hazardRatio$se.hazard, digits = 3, format = "f"))
        hazardRatio$upperLimit = as.numeric(formatC(hazardRatio$hazard + qnorm(1-(1-confidenceLevel/100)/2)*hazardRatio$se.hazard, digits = 3, format = "f"))
        hazardRatio$hazard = as.numeric(formatC(hazardRatio$hazard, digits = 3, format = "f"))
        hazardRatio$se.hazard = as.numeric(formatC(hazardRatio$se.hazard, digits = 3, format = "f"))
        row.names(hazardRatio) = NULL
        hazardRatio = data.frame(time = y$time, hazardRatio)
        colnames(hazardRatio) = c("Interval time", "HR", "S.E.", "Lower limit", "Upper limit")
        return(hazardRatio)
      })
    }else{hazardRatio = NULL}
    
    if(medianLifeTime){
      medianLifeTimeResult <- lapply(lifeTableResult, FUN = function(y){
        tis <- seq(fromTime, toTime, by)
        surv = as.data.frame(KMsurv::lifetab(tis, y$nsubs[1], y$nlost, y$nevent))
        surv$time = rownames(surv)
        rownames(surv) = NULL
        subSurv = surv[surv$surv > 0.5,]
        subSurv2 = subSurv[dim(subSurv)[1],]
        rowNumber = as.numeric(rownames(subSurv2))
        subSurv3 = surv[rowNumber:(rowNumber+1),]
        tj = as.numeric(strsplit(subSurv3$time[1], "-")[[1]][1]) 
        tj1 = as.numeric(strsplit(subSurv3$time[1], "-")[[1]][2])    
        stj = subSurv3$surv[1]
        stj1 = subSurv3$surv[2]
        wj = tj1-tj
        medianLifeTimeResult = as.data.frame(formatC(tj+((stj - 0.5)*wj)/(stj-stj1), digits = 3 , format = "f"))
        colnames(medianLifeTimeResult) = "Median life time"
        return(medianLifeTimeResult)
      })
    }else{medianLifeTimeResult = NULL}
    
    
    if(lifeTable){
      lifeTableRes <- lapply(lifeTableResult, FUN = function(z){
      
      lifeTable = z[c(1:3,5,4,6:8)]
      colnames(lifeTable) = c("interval time", "n of subjects", "n of withdraws", "n of subjects at risk", "n of events", "prob. of terminating during the time interval", "prob. of surviving in the time interval", "cum. prob. of surviving at the begining of the time interval")
      
      return(lifeTable)
      
    })
    }else{lifeTableRes = NULL}
  }
  
  if(descriptives){
  
  if(is.null(factors)){
    n = nrow(newData)
    nOfEvent =  as.numeric(table(newData$statusVar)[as.factor(names(table(newData$statusVar))) %in% TRUE][[1]])
    percentOfEvent = as.numeric(formatC((nOfEvent/n)*100, digits = 3, format = "f"))
    nOfCensor =  as.numeric(table(newData$statusVar)[(!(names(table(newData$statusVar))) %in% TRUE)][[1]])
    percentOfCensor = as.numeric(formatC((nOfCensor/n)*100, digits = 3, format = "f"))
    caseSummary = data.frame(n,nOfEvent, percentOfEvent, nOfCensor, percentOfCensor)
    colnames(caseSummary) = c("n", "n of event", "% of event", "n of censor", "% of censor")
    
  }else{
    
    splitFactor = split(newData, newData$factor)
    
    caseSummary <- lapply(splitFactor, FUN = function(x){
      n = nrow(x)
      nOfEvent =  as.numeric(table(x$statusVar)[as.factor(names(table(x$statusVar))) %in% TRUE][[1]])
      percentOfEvent = as.numeric(formatC((nOfEvent/n)*100, digits = 3, format = "f"))
      nOfCensor =  as.numeric(table(x$statusVar)[(!(names(table(x$statusVar))) %in% TRUE)][[1]])
      percentOfCensor = as.numeric(formatC((nOfCensor/n)*100, digits = 3, format = "f"))
      caseSummary = data.frame(n,nOfEvent, percentOfEvent, nOfCensor, percentOfCensor)
      colnames(caseSummary) = c("n", "n of event", "% of event", "n of censor", "% of censor")
      
      return(caseSummary)
      
    })
    
  }}else{caseSummary = NULL}
  
  if(!is.null(factors)){
    
    assign("newData", newData, envir=.GlobalEnv)  # put the dat in the global env
    compareCurves <- survfit(Surv(survivalTime, statusVar == TRUE) ~ factor, data = newData, conf.type = ci, error = varianceEstimation, conf.int = confidenceLevel/100)
    # remove(newData,envir=.GlobalEnv) # delete dat again from global env
    
comps = ten(compareCurves)
  comp(comps, p = p, q = q)
  comparisonTests = as.data.frame(attr(comps, "lrt"))

  if(compare){
    
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
    testResults = NULL
  }}else{testResults = NULL}
  
  
  result = list(tableResult = list(caseSummary = caseSummary, medianLifeTime = medianLifeTimeResult), testResult = list(testResults = testResults, lifeTable = lifeTableRes, hazardRatio = hazardRatio))
  
  return(result)
  
  
}













