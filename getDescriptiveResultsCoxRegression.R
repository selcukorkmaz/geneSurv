
getDescriptiveResultsCoxRegression <- function(obj, variable, group.var = NULL,
                                                confidence.level = 0.95,
                                                number.of.decimals = 3){
  # This function returns the descriptive statistics for One-Way ANOVA.
  # Mean, Std.Dev., SE.Mean and the CI of the true mean.
  # Args:
  #   obj: a data.frame or matrix (data)
  #   variable: a character string indicating the name of response variable.
  #   group.var: a character string indicating the name of grouping variable.
  #   confidence.level: a numeric value in the interval [0-1] indicating the level of confidence interval
  #                     for true population median.
  #   NOTE: Confidence intervals are estimated using the function "wilcox.test(...)". 
  #         Given intervals are asymptotic. Another alternative is to use ci.median(...) function
  #         from package "asbio". It gives the confidence interval for true median along with the 
  #         true coverage of the interval.



  if (!is.factor(obj[ ,group.var]) && !is.null(group.var)){
    return("Warning: Only categorical variables are acceptable as a grouping variable.")
  }
  
  obj <- obj[complete.cases(obj[ ,c(variable, group.var)]), ]
  n <- nrow(obj)   ## number of observations in pairs.
  
  group.categories <- levels(obj[ ,group.var])    ## all categories of given group variable.
  
  # Group response variable by group.var
  if(!is.null(group.var)){
      obj_grp <- eval(substitute(dplyr::group_by(obj, group.var), list(group.var = as.name(group.var))))
  }else{
      obj_grp <- obj

  }
  res <- eval(substitute(dplyr::summarise(obj_grp,
                   n = n(),
                   Mean = mean(variable),
                   'Std.Dev' = sd(variable),
                   'SE.Mean' = sd(variable) / sqrt(n())),
                   list(variable = as.name(variable))))
  
  res <- eval(substitute(dplyr::mutate(res, Lower = Mean - qnorm((1 - confidence.level)/2, lower.tail = FALSE)*SE.Mean,
                                Upper = Mean + qnorm((1 - confidence.level)/2, lower.tail = FALSE)*SE.Mean),
                         list(confidence.level = as.numeric(confidence.level))))
  #attr(res, "class") <- "data.frame"

  res[ ,-1] <- round(res[ ,-1], number.of.decimals)
  return(res)
}
