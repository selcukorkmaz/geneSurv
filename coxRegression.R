
coxRegression <- function(survivalTime, categoricalInput, continuousInput, statusVariable, status, addInteractions = FALSE, twoWayinteractions = FALSE, threeWayinteractions = FALSE, customInteractions =FALSE,
                          selectCustomInteractionTerms = NULL, timeDependetCovariate = FALSE, selectTimeDependentCovariate = NULL, timeDependentVariableTransformation = "none", strata = FALSE, strataVariable = NULL, displayDescriptives = TRUE, displayCoefficientEstimates = TRUE, displayModelFit = TRUE, hazardRatio = TRUE, goodnessOfFitTests = TRUE, analysisOfDeviance = TRUE,
                          ties = "efron", confidenceLevel = 95, alternativeHypothesis = "equalToTestValue", modelSelectionCriteria = "aic",  modelSelection = "enter",
                          alphaEnter = 0.05, alphaRemove = 0.10, referenceCategory = "first", storePredictions = FALSE, storeResiduals = FALSE, storeMartingaleResiduals = FALSE,
                          storeSchoenfeldResiduals = FALSE, storeDfBetas = FALSE, CoxPH = TRUE, multipleID = FALSE, data = dataSet){
  
  
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
  
  if(multipleID != TRUE){  
       newData$statusVar = newData$statusVar%in%status
       
       newData$id <- 1:nrow(newData)
       cut.points <- unique(newData$survivalTime[newData$statusVar == TRUE])
       newData <- survSplit(formula = Surv(survivalTime, statusVar)~., data = newData, cut = cut.points, end = "stop",
                            start = "start", event = "statusVar")
   }
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
  
  if(multipleID){  
      names(newData)[which(names(newData) == "start")] = "start2"
      
      newData <- newData %>%
        group_by(id) %>%
        mutate(start = 0:(n() - 1))
      
      newData = as.data.frame(newData)
      
      ind = row.names(newData[newData$start !=0,])
      
      newData$start[as.numeric(ind)] =  newData$survivalTime[as.numeric(ind)-1]
  }
  
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
  
  if(modelSelection == "enter"){
    formula = as.formula(paste0("Surv(start, survivalTime, statusVar ==  TRUE) ~ ", predictors))
    current = coxph(formula, data=newData, ties = ties)
  }
  
  if(modelSelection == "forward"){
    formula = as.formula(paste0("Surv(start, survivalTime, statusVar ==  TRUE) ~ ", 1))
    fullFormula = as.formula(paste0("Surv(start, survivalTime, statusVar ==  TRUE) ~ ", predictors))
    scope = paste0("~ ", predictors)
    current = coxph(formula = formula, data=newData, ties = ties)
    full = coxph(formula = fullFormula, data=newData, ties = ties)
    if(modelSelectionCriteria == "aic"){
      stepForward <- function(formula, data, ties, scope) {
        model.initial <- do.call("coxph", list(formula, data =data, ties = ties))
        model.stepwise1 <- step(model.initial, direction = "forward")
        model.stepwise2 <- step(model.stepwise1, scope)
        list(modInitial = model.initial, modStep1 = model.stepwise1, modStep2 = model.stepwise2)
      }
      current = stepForward(formula = formula, data = newData, ties = ties, scope = scope)
      #current = step(current, data = df2, direction = "forward", scope = scopes)
    }
    else if(modelSelectionCriteria == "pValue"){
      while (TRUE) {
        formulaFull = as.formula(paste0("Surv(survivalTime, statusVar == TRUE) ~ ", predictors))
        #current = glm(formula = formulaFull, data = df2, family = binomial(link="logit"))
        a <- tryCatch(add1(current, scope=full, test="Chisq"), error=function(e) NULL)
        if (is.null(a)) {
          break;  # there are no unused variables (or something went splat), so we bail out
        }
        pmin <- min(a[-1,4])
        if (pmin < alphaEnter) {
          # we have a candidate for addition to the model
          var <- rownames(a)[a[,4] == pmin];  # name of variable to add
          if (length(var) > 1) {
            # same issue with ties, intercept as above
            var <- var[2];
          }
          f <- formula(current);  # current formula
          f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" + ")));  # modify the formula to add the chosen variable
          current <- coxph(formula = f, data=newData, ties = ties)
          next
        }
        break
      }
    }
  }
  
  if(modelSelection == "backward"){
    formula = as.formula(paste0("Surv(start, survivalTime, statusVar ==  TRUE) ~ ", predictors))
    current = coxph(formula, data=newData, ties = ties)
    coeffs = length(current$coefficients)
    if(modelSelectionCriteria == "aic"){
      current = step(current, direction = "backward")
    }
    else if(modelSelectionCriteria == "pValue"){
      while (TRUE) {
        if (coeffs > 1) { 
          d <- drop1(current, test="Chisq")
          pmax <- max(d[-1,4])   
          if (pmax > alphaRemove) {
            # we have a candidate for deletion
            var <- rownames(d)[d[,4] == pmax];  # name of variable to delete
            if (length(var) > 1) {
              # if an intercept is present, it will be the first name in the list
              # there also could be ties for worst p-value
              # taking the second entry if there is more than one is a safe solution to both issues
              var <- var[2];
            }
            f <- formula(current);  # current formula
            f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" - ")));  # modify the formula to drop the chosen variable (by subtracting it)
            current <- coxph(formula = f, data=newData, ties = ties)
            # fit the modified model
            next;  # return to the top of the loop
          }
        }
        break;
      }
    }
  }
  
  if(modelSelection == "stepwise"){
    formulaFull = as.formula(paste0("Surv(start, survivalTime, statusVar ==  TRUE) ~ ", predictors))
    current = coxph(formula = formulaFull, data=newData, ties = ties)
    formula = paste0("Surv(survivalTime, statusVar == TRUE) ~ ", 1)
    coeffs = length(current$coefficients)
    if(modelSelectionCriteria == "aic"){
      stepStepwise <- function(formula, data, ties) {
        model.initial <- do.call("coxph", list(formula, data =data, ties = ties))
        model.stepwise1 <- step(model.initial, direction = "both")
        list(modInitial = model.initial, modStep1 = model.stepwise1)
      }
      current = stepStepwise(formula = formulaFull, data = newData, ties = ties)
    }
    else if(modelSelectionCriteria == "pValue"){
      while (TRUE) {  # process each model until we break out of the loop
        if (coeffs > 1) { 
          d <- drop1(current, test="Chisq")
          pmax <- max(d[-1,4])   
          if (pmax > alphaRemove) {
            # we have a candidate for deletion
            var <- rownames(d)[d[,4] == pmax];  # name of variable to delete
            if (length(var) > 1) {
              # if an intercept is present, it will be the first name in the list
              # there also could be ties for worst p-value
              # taking the second entry if there is more than one is a safe solution to both issues
              var <- var[2];
            }
            f <- formula(current);  # current formula
            f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" - ")));  # modify the formula to drop the chosen variable (by subtracting it)
            current = coxph(formula = f, data=newData, ties = ties)# fit the modified model
            next;  # return to the top of the loop
          }
        }
        # if we get here, we failed to drop a term; try adding one
        # note: add1 throws an error if nothing can be added (current == full), which we trap with tryCatch
        a <- tryCatch(add1(current, scope = formulaFull, test="Chisq"), error=function(e) NULL)
        if (is.null(a)) {
          break;  # there are no unused variables (or something went splat), so we bail out
        }
        pmin <- min(a[-1,4])
        if (pmin < alphaEnter) {
          # we have a candidate for addition to the model
          var <- rownames(a)[a[,4] == pmin];  # name of variable to add
          if (length(var) > 1) {
            # same issue with ties, intercept as above
            var <- var[2];
          }
          f <- formula(current);  # current formula
          f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" + ")));  # modify the formula to add the chosen variable
          current <- coxph(formula = f, data=newData, ties = ties)
          
          next
        }
        break
      } 
    }
  }
  
  if(modelSelection == "forward" && modelSelectionCriteria == "aic"){
    
    coeffs = as.data.frame(current$modStep2$coefficients)
    summary = summary(current$modStep2)
    concordance = as.data.frame(formatC(t(summary$concordance), digits = 3, format = "f"))
    names(concordance) = c("Concordance", "S.E.")
    aic = as.data.frame(formatC(AIC(current$modStep2), digits = 3, format = "f"))
    colnames(aic) = "AIC"
    rsq = as.data.frame(formatC(t(summary$rsq), digits = 3, format = "f"))
    colnames(rsq) = c("R square", "Maximum R square")
    betas = as.data.frame(formatC(t(current$modStep2$coefficients), digits = 3, format = "f"))
    
    if(length(names(current$modStep2$coefficients)) > 0){
      
      if(analysisOfDeviance){
        dev = anova(current$modStep2, test="Chisq")
        dev2 = as.data.frame(dev[-1,-1])
        if(length(dev2$Df) > 1){
          dev3 = as.data.frame(apply(dev2, 2, cbind))
        }else{
          dev3 = as.data.frame(t(apply(dev2, 2, cbind)))
        }
        dev3$Variable = rownames(dev)[-1]
        deviance = dev3[c(4,2,1,3)]
        deviance[,3] = formatC(deviance[,3], digits = 3, format = "f")
        deviance[,4] = formatC(deviance[,4], digits = 3, format = "f")
        names(deviance) = c("Variable", "df", "Chisquare","p value")
      }else{deviance = NULL}
      
      if(goodnessOfFitTests){
        
        gof = summary(current$modStep2)
        
        likelihoodRatio = as.data.frame(t(gof$logtest))
        likelihoodRatio$Test = "Likelihood ratio"
        
        waldTest = as.data.frame(t(gof$waldtest))
        waldTest$Test = "Wald"
        
        scoreTest = as.data.frame(t(gof$sctest))
        scoreTest$Test = "Score (logrank)"
        
        gofRes = rbind(likelihoodRatio,waldTest,scoreTest)
        gofResult = gofRes[c(4,1:3)]
        gofResult$test = formatC(gofResult$test, digits = 3, format = "f")
        gofResult$pvalue = formatC(gofResult$pvalue, digits = 3, format = "f")
        names(gofResult) = c("Test", "Statistic", "df", "p value")
      }else{gofResult = NULL}  
      
      
      if(hazardRatio){  
        
        hr = as.data.frame(formatC(summary(current$modStep2, conf.int = confidenceLevel/100)$conf.int, digits = 3, format = "f"))[-2]
        
        #         if(!is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && !is.null(names(current$modStep2$xlevels))){
        #           varNames = c(rownames(hr)[rownames(hr) %in% names(continuousInput)],names(current$modStep2$xlevels))
        #         }
        #         
        #         if(is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && !is.null(names(current$modStep2$xlevels))){
        #           varNames = names(current$modStep2$xlevels)
        #         }
        #         
        #         if(!is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && is.null(names(current$modStep2$xlevels))){
        #           varNames = rownames(hr)
        #         }
        
        hr2 = cbind(Variable = rownames(hr), hr)
        rownames(hr2) =NULL
        colnames(hr2) = c("Variable", "Hazard ratio", paste0("Lower limit (", confidenceLevel,"%)"), paste0("Upper limit (", confidenceLevel,"%)"))
        
      }else{hr2 = NULL}
      
      if(displayCoefficientEstimates){  
        
        coefficients = as.data.frame(formatC(summary(current$modStep2)$coefficients, digits = 3, format = "f"))[-2] 
        #         
        #         if(!is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && !is.null(names(current$modStep2$xlevels))){
        #           varNames = c(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)],names(current$modStep2$xlevels))
        #         }
        #         
        #         if(is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && !is.null(names(current$modStep2$xlevels))){
        #           varNames = names(current$modStep2$xlevels)
        #         }
        #         
        #         if(!is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && is.null(names(current$modStep2$xlevels))){
        #           varNames = rownames(coefficients)
        #         }
        
        coefficients2 = cbind(Variable = rownames(coefficients), coefficients)
        rownames(coefficients2) =NULL
        colnames(coefficients2) = c("Variable", "Coefficient estimate", "Standard error", "z statistic", "p value")
        
      }else{coefficients2 = NULL}  
      
      if(displayModelFit){
        
        coeffs = current$modStep2$coefficients
        
        eq = paste(formatC(coeffs, digits = 3, format = "f"), names(coeffs), sep="*", collapse = " + ")
        eq2 = gsub("\\+ \\-", "- ", eq)
        #         eq3 = paste(formatC(coeffs, digits = 3, format = "f"), eq2, sep = " + ")
        #         eq4 = gsub("\\+ \\-", "- ", eq3)
        coxModel = paste0("h(t) = h(0)t*exp(", eq2,")")   
        
      }else{coxModel = NULL}
      
      
      
      if(CoxPH){
        
        res = cox.zph(current$modStep2)
        
        
        
        coxPh = apply(res$table,2, function(x){
          
          formatC(x, digits = 3, format = "f")
          
        })
        
        if(nrow(res$table) == 1){
          
          coxPh = as.data.frame(t(coxPh))
          row.names(coxPh) = rownames(res$table)
          
          
        }else{
          
          coxPh = as.data.frame(coxPh)
          
        }
        
        colnames(coxPh) = c("Rho", "Chi-square statistic", "p value")
        
      }else{coxPh = NULL}
      
      
    }else{
      deviance = NULL
    }
  }
  else if(modelSelection == "stepwise" && modelSelectionCriteria == "aic"){
    
    coeffs = as.data.frame(current$modStep1$coefficients)
    summary = summary(current$modStep1)
    concordance = as.data.frame(formatC(t(summary$concordance), digits = 3, format = "f"))
    names(concordance) = c("Concordance", "S.E.")
    aic = as.data.frame(formatC(AIC(current$modStep1), digits = 3, format = "f"))
    colnames(aic) = "AIC"
    rsq = as.data.frame(formatC(t(summary$rsq), digits = 3, format = "f"))
    colnames(rsq) = c("R square", "Maximum R square")
    betas = as.data.frame(formatC(t(current$modStep1$coefficients), digits = 3, format = "f"))
    
    if(length(names(current$modStep1$coefficients)) > 0){
      
      if(analysisOfDeviance){
        dev = anova(current$modStep1, test="Chisq")
        dev2 = as.data.frame(dev[-1,-1])
        if(length(dev2$Df) > 1){
          dev3 = as.data.frame(apply(dev2, 2, cbind))
        }else{
          dev3 = as.data.frame(t(apply(dev2, 2, cbind)))
        }
        dev3$Variable = rownames(dev)[-1]
        deviance = dev3[c(4,2,1,3)]
        deviance[,3] = formatC(deviance[,3], digits = 3, format = "f")
        deviance[,4] = formatC(deviance[,4], digits = 3, format = "f")
        names(deviance) = c("Variable", "df", "Chisquare","p value")
      }else{deviance = NULL}
      
      if(goodnessOfFitTests){
        
        gof = summary(current$modStep1)
        
        likelihoodRatio = as.data.frame(t(gof$logtest))
        likelihoodRatio$Test = "Likelihood ratio"
        
        waldTest = as.data.frame(t(gof$waldtest))
        waldTest$Test = "Wald"
        
        scoreTest = as.data.frame(t(gof$sctest))
        scoreTest$Test = "Score (logrank)"
        
        gofRes = rbind(likelihoodRatio,waldTest,scoreTest)
        gofResult = gofRes[c(4,1:3)]
        gofResult$test = formatC(gofResult$test, digits = 3, format = "f")
        gofResult$pvalue = formatC(gofResult$pvalue, digits = 3, format = "f")
        names(gofResult) = c("Test", "Statistic", "df", "p value")
      }else{gofResult = NULL}  
      
      
      if(hazardRatio){  
        
        hr = as.data.frame(formatC(summary(current$modStep1, conf.int = confidenceLevel/100)$conf.int, digits = 3, format = "f"))[-2]
        
        #         if(!is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && !is.null(names(current$modStep1$xlevels))){
        #           varNames = c(rownames(hr)[rownames(hr) %in% names(continuousInput)],names(current$modStep1$xlevels))
        #         }
        #         
        #         if(is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && !is.null(names(current$modStep1$xlevels))){
        #           varNames = names(current$modStep1$xlevels)
        #         }
        #         
        #         if(!is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && is.null(names(current$modStep1$xlevels))){
        #           varNames = rownames(hr)
        #         }
        
        hr2 = cbind(Variable = rownames(hr), hr)
        rownames(hr2) =NULL
        colnames(hr2) = c("Variable", "Hazard ratio", paste0("Lower limit (", confidenceLevel,"%)"), paste0("Upper limit (", confidenceLevel,"%)"))
        
      }else{hr2 = NULL}
      
      if(displayCoefficientEstimates){  
        
        coefficients = as.data.frame(formatC(summary(current$modStep1)$coefficients, digits = 3, format = "f"))[-2] 
        #         
        #         if(!is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && !is.null(names(current$modStep1$xlevels))){
        #           varNames = c(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)],names(current$modStep1$xlevels))
        #         }
        #         
        #         if(is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && !is.null(names(current$modStep1$xlevels))){
        #           varNames = names(current$modStep1$xlevels)
        #         }
        #         
        #         if(!is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && is.null(names(current$modStep1$xlevels))){
        #           varNames = rownames(coefficients)
        #         }
        
        coefficients2 = cbind(Variable = rownames(coefficients), coefficients)
        rownames(coefficients2) =NULL
        colnames(coefficients2) = c("Variable", "Coefficient estimate", "Standard error", "z statistic", "p value")
        
      }else{coefficients2 = NULL}  
      
      if(displayModelFit){
        
        coeffs = current$modStep1$coefficients
        
        eq = paste(formatC(coeffs, digits = 3, format = "f"), names(coeffs), sep="*", collapse = " + ")
        eq2 = gsub("\\+ \\-", "- ", eq)
        #         eq3 = paste(formatC(coeffs, digits = 3, format = "f"), eq2, sep = " + ")
        #         eq4 = gsub("\\+ \\-", "- ", eq3)
        coxModel = paste0("h(t) = h(0)t*exp(", eq2,")")   
        
      }else{coxModel = NULL}
      
      
      if(CoxPH){
        
        res = cox.zph(current$modStep1)
        
        coxPh = apply(res$table,2, function(x){
          
          formatC(x, digits = 3, format = "f")
          
        })
        
        if(nrow(res$table) == 1){
          
          coxPh = as.data.frame(t(coxPh))
          row.names(coxPh) = rownames(res$table)
          
          
        }else{
          
          coxPh = as.data.frame(coxPh)
          
        }
        
        colnames(coxPh) = c("Rho", "Chi-square statistic", "p value")
        
        
      }else{coxPh = NULL}
      
      
      
    }else{
      deviance = NULL
      
    }
    
  }else{
    
    coeffs = as.data.frame(current$coefficients)
    summary = summary(current)
    concordance = as.data.frame(formatC(t(summary$concordance), digits = 3, format = "f"))
    names(concordance) = c("Concordance", "S.E.")
    aic = as.data.frame(formatC(AIC(current), digits = 3, format = "f"))
    colnames(aic) = "AIC"
    rsq = as.data.frame(formatC(t(summary$rsq), digits = 3, format = "f"))
    colnames(rsq) = c("R square", "Maximum R square")
    betas = as.data.frame(formatC(t(current$coefficients), digits = 3, format = "f"))
    
    
    if(length(names(current$coefficients)) > 0){
      
      if(analysisOfDeviance){
        dev = anova(current, test="Chisq")
        dev2 = as.data.frame(dev[-1,-1])
        if(length(dev2$Df) > 1){
          dev3 = as.data.frame(apply(dev2, 2, cbind))
        }else{
          dev3 = as.data.frame(t(apply(dev2, 2, cbind)))
        }
        dev3$Variable = rownames(dev)[-1]
        deviance = dev3[c(4,2,1,3)]
        deviance[,3] = formatC(deviance[,3], digits = 3, format = "f")
        deviance[,4] = formatC(deviance[,4], digits = 3, format = "f")
        names(deviance) = c("Variable", "df", "Chisquare","p value")
      }else{deviance = NULL}
      
      if(goodnessOfFitTests){
        
        gof = summary(current)
        
        likelihoodRatio = as.data.frame(t(gof$logtest))
        likelihoodRatio$Test = "Likelihood ratio"
        
        waldTest = as.data.frame(t(gof$waldtest))
        waldTest$Test = "Wald"
        
        scoreTest = as.data.frame(t(gof$sctest))
        scoreTest$Test = "Score (logrank)"
        
        gofRes = rbind(likelihoodRatio,waldTest,scoreTest)
        gofResult = gofRes[c(4,1:3)]
        gofResult$test = formatC(gofResult$test, digits = 3, format = "f")
        gofResult$pvalue = formatC(gofResult$pvalue, digits = 3, format = "f")
        names(gofResult) = c("Test", "Statistic", "df", "p value")
      }else{gofResult = NULL}  
      
      if(hazardRatio){  
        
        hr = as.data.frame(formatC(summary(current, conf.int = confidenceLevel/100)$conf.int, digits = 3, format = "f"))[-2]
        
        #         if(!is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && !is.null(names(current$xlevels))){
        #           varNames = c(rownames(hr)[rownames(hr) %in% names(continuousInput)],names(current$xlevels))
        #         }
        #         
        #         if(is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && !is.null(names(current$xlevels))){
        #           varNames = names(current$xlevels)
        #         }
        #         
        #         if(!is.null(rownames(hr)[rownames(hr) %in% names(continuousInput)]) && is.null(names(current$xlevels))){
        #           varNames = rownames(hr)
        #         }
        
        hr2 = cbind(Variable = rownames(hr), hr)
        rownames(hr2) =NULL
        colnames(hr2) = c("Variable", "Hazard ratio", paste0("Lower limit (", confidenceLevel,"%)"), paste0("Upper limit (", confidenceLevel,"%)"))
        
      }else{hr2 = NULL}
      
      if(displayCoefficientEstimates){  
        
        coefficients = as.data.frame(formatC(summary(current)$coefficients, digits = 3, format = "f"))[-2] 
        #         
        #         if(!is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && !is.null(names(current$xlevels))){
        #           varNames = c(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)],names(current$xlevels))
        #         }
        #         
        #         if(is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && !is.null(names(current$xlevels))){
        #           varNames = names(current$xlevels)
        #         }
        #         
        #         if(!is.null(rownames(coefficients)[rownames(coefficients) %in% names(continuousInput)]) && is.null(names(current$xlevels))){
        #           varNames = rownames(coefficients)
        #         }
        
        coefficients2 = cbind(Variable = rownames(coefficients), coefficients)
        rownames(coefficients2) =NULL
        colnames(coefficients2) = c("Variable", "Coefficient estimate", "Standard error", "z statistic", "p value")
        
      }else{coefficients2 = NULL}  
      
      if(displayModelFit){
        
        coeffs = current$coefficients
        
        eq = paste(formatC(coeffs, digits = 3, format = "f"), names(coeffs), sep="*", collapse = " + ")
        eq2 = gsub("\\+ \\-", "- ", eq)
        #         eq3 = paste(formatC(coeffs, digits = 3, format = "f"), eq2, sep = " + ")
        #         eq4 = gsub("\\+ \\-", "- ", eq3)
        coxModel = paste0("h(t) = h(0)t*exp(", eq2,")")   
        
      }else{coxModel = NULL}
      
      
      if(CoxPH){
        
        res = cox.zph(current)
        
        coxPh = apply(res$table,2, function(x){
          
          formatC(x, digits = 3, format = "f")
          
        })
        
        if(nrow(res$table) == 1){
          
          coxPh = as.data.frame(t(coxPh))
          row.names(coxPh) = rownames(res$table)
          
          
        }else{
          
          coxPh = as.data.frame(coxPh)
          
        }
        
        colnames(coxPh) = c("Rho", "Chi-square statistic", "p value")
        
        
      }else{coxPh = NULL}
      
      
      
    }else{
      deviance = NULL
      
    }
    
    
  }
  
  if(displayDescriptives){
    
    if(!is.null(continuousInput)){
      descriptivesForContinuous = apply(X = continuousInput, 2, function(x)
      {
        n = length(x)
        Mean = as.numeric(formatC(mean(x), digits = 3, format = "f"))
        Std.Dev. = as.numeric(formatC(sd(x), digits = 3, format = "f"))
        S.E. = as.numeric(formatC(sd(x)/sqrt(length(x)), digits = 3, format = "f"))
        Lower = as.numeric(formatC(mean(x) - qnorm(1-(1-confidenceLevel/100)/2)*(sd(x)/sqrt(length(x))), digits = 3, format = "f")) 
        Upper = as.numeric(formatC(mean(x) + qnorm(1-(1-confidenceLevel/100)/2)*(sd(x)/sqrt(length(x))), digits = 3, format = "f"))         
        descForContinuous = data.frame(n, Mean, Std.Dev., S.E., Lower, Upper)
        return(descForContinuous)
      }
      )
      
      descriptivesForContinuousDataFrame = do.call(rbind.data.frame, descriptivesForContinuous)
      
      descriptivesForContinuousDataFrame = as.data.frame(cbind(Variable = rownames(descriptivesForContinuousDataFrame), descriptivesForContinuousDataFrame))
      rownames(descriptivesForContinuousDataFrame) = NULL
    }else(descriptivesForContinuousDataFrame  = NULL)
    
    if(!is.null(categoricalInput)){
      
      descriptivesForCategorical = apply(X = categoricalInput, 2, function(x){
        tbl = as.data.frame(table(x))
        tbl$Percentage = as.numeric(formatC(tbl$Freq/sum(tbl$Freq), digits = 3, format = "f"))
        colnames(tbl) = c("Category", "Count", "Percentage")
        return(tbl)
      })
      
    }else{descriptivesForCategorical = NULL}
    
  }else{
    
    descriptivesForContinuousDataFrame = NULL
    descriptivesForCategorical = NULL
  }
  
  if(modelSelection == "forward" && modelSelectionCriteria == "aic"){
    
    if(storePredictions){
      predictions = as.data.frame(formatC(predict(current$modStep2), digits = 3, format = "f"))
      colnames(predictions) = "Predictions"
    }else{predictions = NULL}  
    
    if(storeResiduals){
      residuals = as.data.frame(formatC(residuals(current$modStep2, type = "deviance"), digits = 3, format = "f"))
      colnames(residuals) = "Residuals"
    }else{residuals = NULL}
    
    if(storeMartingaleResiduals){
      residualsMartingale = as.data.frame(formatC(residuals(current$modStep2, type = "martingale"), digits = 3, format = "f"))
      colnames(residualsMartingale) = "Martingale Residuals"
    }else{residualsMartingale = NULL}
    
    if(storeSchoenfeldResiduals){
      
      residualsSchoenfeld = as.data.frame(residuals(current$modStep2, type = "schoenfeld"))
      
      residualsSchoenfeld = apply(X = residualsSchoenfeld, 2, function(x){
        
        as.numeric(formatC(x, digits = 3, format = "f"))
      })
      
      if(length(current$modStep2$coefficients) == 1){
        
        residualsSchoenfeld = as.data.frame(residualsSchoenfeld)
        colnames(residualsSchoenfeld) = names(current$modStep2$coefficients)
      }
      
      residualsSchoenfeld = as.data.frame(residualsSchoenfeld)
      
    }else{residualsSchoenfeld = NULL}
    
    if(storeDfBetas){
      residualsDfBetas = as.data.frame(formatC(residuals(current$modStep2, type = "dfbetas"), digits = 3, format = "f"))
      colnames(residualsDfBetas) = names(current$modStep2$coefficients)
    }else{residualsDfBetas = NULL}
    
    storeList = list(Predictions = predictions, Residuals = residuals, MartingaleResiduals = residualsMartingale, SchoenfeldResiduals = residualsSchoenfeld, DfBetas = residualsDfBetas)
  }
  
  else if(modelSelection == "stepwise" && modelSelectionCriteria == "aic"){
    
    if(storePredictions){
      predictions = as.data.frame(formatC(predict(current$modStep1), digits = 3, format = "f"))
      colnames(predictions) = "Predictions"
    }else{predictions = NULL} 
    
    if(storeResiduals){
      residuals = as.data.frame(formatC(residuals(current$modStep1, type = "deviance"), digits = 3, format = "f"))
      colnames(residuals) = "Residuals"
    }else{residuals = NULL}
    
    if(storeMartingaleResiduals){
      residualsMartingale = as.data.frame(formatC(residuals(current$modStep1, type = "martingale"), digits = 3, format = "f"))
      colnames(residualsMartingale) = "Martingale Residuals"
    }else{residualsMartingale = NULL}
    
    if(storeSchoenfeldResiduals){
      
      residualsSchoenfeld = as.data.frame(residuals(current$modStep1, type = "schoenfeld"))
      
      
      
      residualsSchoenfeld = apply(X = residualsSchoenfeld, 2, function(x){
        as.numeric(formatC(x, digits = 3, format = "f"))
      })
      
      if(length(current$modStep2$coefficients) == 1){
        
        residualsSchoenfeld = as.data.frame(residualsSchoenfeld)
        colnames(residualsSchoenfeld) = names(current$modStep1$coefficients)
      }
      
      residualsSchoenfeld = as.data.frame(residualsSchoenfeld)
      
      
      
    }else{residualsSchoenfeld = NULL}
    
    if(storeDfBetas){
      residualsDfBetas = as.data.frame(formatC(residuals(current$modStep1, type = "dfbetas"), digits = 3, format = "f"))
      colnames(residualsDfBetas) = names(current$modStep1$coefficients)
    }else{residualsDfBetas = NULL}
    
    storeList = list(Predictions = predictions, Residuals = residuals, MartingaleResiduals = residualsMartingale, SchoenfeldResiduals = residualsSchoenfeld, DfBetas = residualsDfBetas)
    
  }else{
    
    if(storePredictions){
      predictions = as.data.frame(formatC(predict(current), digits = 3, format = "f"))
      colnames(predictions) = "Predictions"
    }else{predictions = NULL}
    
    if(storeResiduals){
      residuals = as.data.frame(formatC(residuals(current, type = "deviance"), digits = 3, format = "f"))
      colnames(residuals) = "Residuals"
    }else{residuals = NULL}
    
    if(storeMartingaleResiduals){
      residualsMartingale = as.data.frame(formatC(residuals(current, type = "martingale"), digits = 3, format = "f"))
      colnames(residualsMartingale) = "Martingale Residuals"
    }else{residualsMartingale = NULL}
    
    if(storeSchoenfeldResiduals){
      residualsSchoenfeld = residuals(current, type = "schoenfeld")
      
      residualsSchoenfeld = apply(X = residualsSchoenfeld, 2, function(x){
        as.numeric(formatC(x, digits = 3, format = "f"))
      })
      
      if(length(current$modStep2$coefficients) == 1){
        
        residualsSchoenfeld = as.data.frame(residualsSchoenfeld)
        colnames(residualsSchoenfeld) = names(current$coefficients)
      }
      
      residualsSchoenfeld = as.data.frame(residualsSchoenfeld)
      
    }else{residualsSchoenfeld = NULL}
    
    if(storeDfBetas){
      residualsDfBetas = as.data.frame(formatC(residuals(current, type = "dfbetas"), digits = 3, format = "f"))
      colnames(residualsDfBetas) = names(current$coefficients)
      
    }else{residualsDfBetas = NULL}
    
    storeList = list(Predictions = predictions, Residuals = residuals, MartingaleResiduals = residualsMartingale, SchoenfeldResiduals = residualsSchoenfeld, DfBetas = residualsDfBetas)
    
  }
  
  
  if(modelSelection == "stepwise" && modelSelectionCriteria == "aic"){
    
    modelResult = current$modStep1
  }
  
  else if(modelSelection == "forward" && modelSelectionCriteria == "aic"){
    
    modelResult = current$modStep2
    
  }else{modelResult = current}
  
  
  result = list(tableResult = list(Descriptives = list(Continuous = descriptivesForContinuousDataFrame, 
                                                       Categorical = descriptivesForCategorical)), 
                testResult = list(ModelSummaryList = list(ModelFit = coxModel, Concordance = concordance,
                                                          AIC = aic, Rsquare = rsq, Betas = betas), 
                                  displayCoefficientEstimatesResults = coefficients2, 
                                  displayModelFitResults = coxModel, hazardRatioResults = hr2, 
                                  goodnessOfFitTestsResults = gofResult, displayCoxPh = coxPh,
                                  analysisOfDevianceResults = deviance, Store = storeList), 
                model = modelResult)
  
  return(result)
}

