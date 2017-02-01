cuttOffForSurvival <- function(markers, survivalTime, statusVariable, status, compTest = "logRank", method = "significance", p= 1, q = 1, nmin=1, 
                               confidenceLevel=95, higher = TRUE, data = data){
  # cat(paste("Optimizing cutoff using method survival", method, sep="_"))
  
  
  
  markerData = data[,markers, drop = F]
  survivalTimeData = data[,survivalTime, drop = F]
  statusVariableData = data[, statusVariable, drop = F]
  survivalTimeName <- names(survivalTimeData)
  endtime = max(survivalTimeData[,1])
  optimalCutoffs <- matrix(nrow= length(markers), ncol=8)
  colnames(optimalCutoffs) <- c("Optimal cut-off", "HR", "HR (lower)", "HR (upper)", "Mean survival time (factor = 0)", "Mean survival time (factor = 1)",  "testStatistic", "p value")
  optimalCutoffList = list()
  
  for(m in 1:ncol(markerData)){
    
    markerName <- names(markerData[m])
    
    newData = cbind.data.frame(markerData[,m], survivalTimeData, statusVariableData)
    colnames(newData)[1] = markerName
    newData = newData[complete.cases(newData),]
    npatients <- nrow(newData)
    
    marker = newData[1]
    time = newData[2]
    event = newData[3]
    
    index <- intersect(which(!is.na(marker)), intersect(which(!is.na(time)), which(!is.na(event))))
    
    marker <- marker[index,]
    time <- time[index,]
    event <- event[index,]
    n <- length(marker)
    nevent <- length(which(event == status))
    nmarker <- length(unique(marker))
    if (nmarker < 3) stop("insufficient data")
    index <- order(marker)
    marker <- marker[index]
    time <- time[index]
    event <- event[index]
    event = event == status
    y <- Surv(time, event) 
    q <- 1-(1-confidenceLevel/100)/2
    z <- qnorm(q)
    nlow <- nmin:(n-nmin)
    Y <- matrix(nrow=length(nlow), ncol=8)
    rownames(Y) <- nlow
    for (i in nlow) {
      j <- i-nmin+1   
      if (marker[i] != marker[i+1]) {
        if(higher){
        x <- c(rep(0, i), rep(1, n-i))
        }else{
          
          x <- c(rep(1, i), rep(0, n-i))
          
        }
        model <- summary(coxph(Surv(time, event) ~ x))
        coef <- model$coefficients
        Y[j, 1] <- (marker[i] + marker[i+1]) / 2
        Y[j, 2] <- coef[2]
        Y[j, 3] <- exp(coef[1] - z * coef[3])
        Y[j, 4] <- exp(coef[1] + z * coef[3])
        
        
        if(compTest == "logRank"){
          
          newData2 = cbind.data.frame("x2" = x,"time2" = time, "event2" = event)
          test <- coxph(Surv(time2, event2) ~ x2, data = newData2)
          comps <- ten(test)
          comp(comps)
          
          comparisonTests = as.data.frame(attr(comps, "lrt"))
          
          Y[j, 7] <- comparisonTests[1,6]
          Y[j, 8] <- comparisonTests[1,8]
          
          fit <- summary(survfit(y ~ x), rmean=endtime)
          tab <- fit$table
          index.low <- grep(0, rownames(tab))
          Y[j, 5] <- tab[index.low, "*rmean"]
          # Y[j, 6] <- tab[index.low, "*se(rmean)"]
          index.high <- grep(1, rownames(tab)) 
          Y[j, 6] <- tab[index.high, "*rmean"]
          # Y[j, 8] <- tab[index.high, "*se(rmean)"]
          
          
        }
        
        if(compTest == "gehanBreslow"){
          
          newData2 = cbind.data.frame("x2" = x,"time2" = time, "event2" = event)
          test <- coxph(Surv(time2, event2) ~ x2, data = newData2)
          comps <- ten(test)
          comp(comps)
          
          comparisonTests = as.data.frame(attr(comps, "lrt"))
          
          Y[j, 7] <- comparisonTests[2,6]
          Y[j, 8] <- comparisonTests[2,8]
          
          fit <- summary(survfit(y ~ x), rmean=endtime)
          tab <- fit$table
          index.low <- grep(0, rownames(tab))
          Y[j, 5] <- tab[index.low, "*rmean"]
          # Y[j, 6] <- tab[index.low, "*se(rmean)"]
          index.high <- grep(1, rownames(tab)) 
          Y[j, 6] <- tab[index.high, "*rmean"]
          # Y[j, 8] <- tab[index.high, "*se(rmean)"]
          
        }
        
        if(compTest == "taroneWare"){
          
          newData2 = cbind.data.frame("x2" = x,"time2" = time, "event2" = event)
          test <- coxph(Surv(time2, event2) ~ x2, data = newData2)
          comps <- ten(test)
          comp(comps)
          
          comparisonTests = as.data.frame(attr(comps, "lrt"))
          
          Y[j, 7] <- comparisonTests[3,6]
          Y[j, 8] <- comparisonTests[3,8]
          
          fit <- summary(survfit(y ~ x), rmean=endtime)
          tab <- fit$table
          index.low <- grep(0, rownames(tab))
          Y[j, 5] <- tab[index.low, "*rmean"]
          # Y[j, 6] <- tab[index.low, "*se(rmean)"]
          index.high <- grep(1, rownames(tab)) 
          Y[j, 6] <- tab[index.high, "*rmean"]
          # Y[j, 8] <- tab[index.high, "*se(rmean)"]
          
          
        }
        
        
        if(compTest == "petoPeto"){
          
          newData2 = cbind.data.frame("x2" = x,"time2" = time, "event2" = event)
          test <- coxph(Surv(time2, event2) ~ x2, data = newData2)
          comps <- ten(test)
          comp(comps)
          
          comparisonTests = as.data.frame(attr(comps, "lrt"))
          
          Y[j, 7] <- comparisonTests[4,6]
          Y[j, 8] <- comparisonTests[4,8]
          
          fit <- summary(survfit(y ~ x), rmean=endtime)
          tab <- fit$table
          index.low <- grep(0, rownames(tab))
          Y[j, 5] <- tab[index.low, "*rmean"]
          # Y[j, 6] <- tab[index.low, "*se(rmean)"]
          index.high <- grep(1, rownames(tab)) 
          Y[j, 6] <- tab[index.high, "*rmean"]
          # Y[j, 8] <- tab[index.high, "*se(rmean)"]
          
          
        }
        
        if(compTest == "modifiedPetoPeto"){
          
          newData2 = cbind.data.frame("x2" = x,"time2" = time, "event2" = event)
          test <- coxph(Surv(time2, event2) ~ x2, data = newData2)
          comps <- ten(test)
          comp(comps)
          
          comparisonTests = as.data.frame(attr(comps, "lrt"))
          
          Y[j, 7] <- comparisonTests[5,6]
          Y[j, 8] <- comparisonTests[5,8]
          
          fit <- summary(survfit(y ~ x), rmean=endtime)
          tab <- fit$table
          index.low <- grep(0, rownames(tab))
          Y[j, 5] <- tab[index.low, "*rmean"]
          # Y[j, 6] <- tab[index.low, "*se(rmean)"]
          index.high <- grep(1, rownames(tab)) 
          Y[j, 6] <- tab[index.high, "*rmean"]
          # Y[j, 8] <- tab[index.high, "*se(rmean)"]
          
          
        }
        
        if(compTest == "flemingtonHarrington"){
          
          newData2 = cbind.data.frame("x2" = x,"time2" = time, "event2" = event)
          test <- coxph(Surv(time2, event2) ~ x2, data = newData2)
          comps <- ten(test)
          comp(comps, p = p, q = q)
          
          comparisonTests = as.data.frame(attr(comps, "lrt"))
          
          Y[j, 7] <- comparisonTests[1,6]
          Y[j, 8] <- comparisonTests[1,8]
          
          fit <- summary(survfit(y ~ x), rmean=endtime)
          tab <- fit$table
          index.low <- grep(0, rownames(tab))
          Y[j, 5] <- tab[index.low, "*rmean"]
          # Y[j, 6] <- tab[index.low, "*se(rmean)"]
          index.high <- grep(1, rownames(tab)) 
          Y[j, 6] <- tab[index.high, "*rmean"]
          # Y[j, 8] <- tab[index.high, "*se(rmean)"]
          
        }
      }
      else Y[j, 8] <- 2
    }
    if (method == "significance") {
      # index.optimal <- which.min(as.numeric(Y[, "p"]))
      index.optimal <- which.max(as.numeric(Y[, 7]))
      
      rownames(Y)[index.optimal] <- "optimal"
    }
    index.p <- which(colnames(Y) == "p")
    colnames(Y)[index.p] <- "p_value"
    
    optimalCutoffs[m,] <- formatC(Y[rownames(Y) == "optimal",], digits = 3, format = "f")
    indx = which(rownames(Y) == "optimal")
    
    if(higher){
    newData2$x2 <- c(rep(0, indx), rep(1, n-indx))
    }else{
      newData2$x2 <- c(rep(1, indx), rep(0, n-indx))
    }
    optimalCutoffList[[m]] = newData2
  }
  
  names(optimalCutoffList) = markers
  cutOffResults = as.data.frame(optimalCutoffs)[-7]
  rownames(cutOffResults) = markers
  result = list(cutOff = cutOffResults, cutOffData = optimalCutoffList)
  
  return(result)
}
