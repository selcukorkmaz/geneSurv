stepwise <- function(full.model, initial.model, alpha.to.enter, alpha.to.leave) {
  # full.model is the model containing all possible terms
  # initial.model is the first model to consider
  # alpha.to.enter is the significance level above which a variable may enter the model
  # alpha.to.leave is the significance level below which a variable may be deleted from the model
  # (Useful things for someone to add: specification of a data frame; a list of variables that must be included)
  formula = paste0(dependent, " ~ ", predictors)
  full = glm(formula = formula, data = df2, family = binomial(link="logit"))
  
  #full <- glm(full.model);  # fit the full model
  msef <- (summary(full)$sigma)^2;  # MSE of full model
  n <- length(full$residuals);  # sample size
  allvars <- attr(full$terms, "predvars");  # this gets a list of all predictor variables
  current <- lm(initial.model);  # this is the current model
  while (TRUE) {  # process each model until we break out of the loop
    temp <- summary(current);  # summary output for the current model
    rnames <- rownames(temp$coefficients);  # list of terms in the current model
    print(temp$coefficients);  # write the model description
    p <- dim(temp$coefficients)[1];  # current model's size
    mse <- (temp$sigma)^2;  # MSE for current model
    cp <- (n-p)*mse/msef - (n-2*p);  # Mallow's cp
    fit <- sprintf("\nS = %f, R-sq = %f, R-sq(adj) = %f, C-p = %f",
                   temp$sigma, temp$r.squared, temp$adj.r.squared, cp);
    write(fit, file="");  # show the fit
    write("=====", file="");  # print a separator
    if (p > 1) {  # don't try to drop a term if only one is left
      d <- drop1(current, test="F");  # looks for significance of terms based on F tests
      pmax <- max(d[-1,6]);  # maximum p-value of any term (have to skip the intercept to avoid an NA value)
      if (pmax > alpha.to.leave) {
        # we have a candidate for deletion
        var <- rownames(d)[d[,6] == pmax];  # name of variable to delete
        if (length(var) > 1) {
          # if an intercept is present, it will be the first name in the list
          # there also could be ties for worst p-value
          # taking the second entry if there is more than one is a safe solution to both issues
          var <- var[2];
        }
        write(paste("--- Dropping", var, "\n"), file="");  # print out the variable to be dropped
        f <- formula(current);  # current formula
        f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" - ")));  # modify the formula to drop the chosen variable (by subtracting it)
        current <- lm(f);  # fit the modified model
        next;  # return to the top of the loop
      }
    }
    # if we get here, we failed to drop a term; try adding one
    # note: add1 throws an error if nothing can be added (current == full), which we trap with tryCatch
    a <- tryCatch(add1(current, scope=full, test="F"), error=function(e) NULL);  # looks for significance of possible additions based on F tests
    if (is.null(a)) {
      break;  # there are no unused variables (or something went splat), so we bail out
    }
    pmin <- min(a[-1,6]);  # minimum p-value of any term (skipping the intercept again)
    if (pmin < alpha.to.enter) {
      # we have a candidate for addition to the model
      var <- rownames(a)[a[,6] == pmin];  # name of variable to add
      if (length(var) > 1) {
        # same issue with ties, intercept as above
        var <- var[2];
      }
      write(paste("+++ Adding", var, "\n"), file="");  # print the variable being added
      f <- formula(current);  # current formula
      f <- as.formula(paste(f[2], "~", paste(f[3], var, sep=" + ")));  # modify the formula to add the chosen variable
      current <- lm(f);  # fit the modified model
      next;  # return to the top of the loop
    }
    # if we get here, we failed to make any changes to the model; time to punt
    break;
  } 
}