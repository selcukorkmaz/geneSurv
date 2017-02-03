# geneSurv: an interactive web-tool for survival analysis for genetic research

Survival analysis is often used in cancer studies. It has been shown that combination of clinical data with genomics increases predictive performance of survival analysis methods. This tool provides a wide range of survival analysis methods for genomic research, especially in cancer studies. The tool includes analysis methods including Kaplan-Meier, Cox regression, Penalized Cox regression and Random Survival Forests. It also offers methods for optimal cutoff point determination for continuous markers.

Each procedure includes following features:

Kaplan-Meier: descriptive statistics, survival table, mean and median life time, hazard ratios, comparison tests including Log-rank, Gehan-Breslow, Tarone-Ware, Peto-Peto, Modified Peto-Peto, Flemington-Harrington, and interactive plots such as Kaplan-Meier curves and hazard plots.

Cox regression: coefficient estimates, hazard ratios, goodness of fit test, analysis of deviance, save predictions, save residuals, save Martingale residuals, save Schoenfeld residuals, save dfBetas, proportional hazard assumption test, and interactive plots including Schoenfeld residual plot and Log-Minus-Log plot.

Penalized Cox regression: feature selection using ridge, elastic net and lasso penalization. A cross-validation to investigate the relationship between partial likelihood devaince and lambda values.

Random survival forests: individual survival predictions, individual cumulative hazard predictions, error rate, variable importance, and interactive plots including random survival plot, cumulative hazard plot, error rate plot, Cox vs RSF plot

Optimal cutoff: determination of optimal cutoff value by maxmizing test statistics, including log-rank, Gehan-Breslow, Tarone-Ware, Peto-Peto, modified Peto-Peto, Flemington-Harrington.

![My image](www/screenShots/ss1.jpg?raw=true "About")
