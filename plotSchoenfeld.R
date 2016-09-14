plotSchoenfeld <- function(x, resid=FALSE, se=TRUE, df=4, nsmo=40, ltyest=1, ltyci = 3, col=1, lwd=1) 
{
  
  ## adapted from survival::plot.cox.zph
  
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


for (i in 1:var) {
  y <- yy[,i]
  yhat <- pmat %*% qr.coef(qmat, y)
  if (resid) {yr <-range(yhat, y)}else{yr <-range(yhat)}
  if (se) {
    temp <- 2* sqrt(x$var[i,i]*seval)
    yup <- yhat + temp
    ylow<- yhat - temp
    yr <- range(yr, yup, ylow)
    newData2 = cbind.data.frame(pred.x,yhat, yup, ylow) 

  }
  
  newData = cbind.data.frame(xx, y, pred.x, yhat)
  
  sp =  ggplot2::ggplot(newData, aes(x=xx, y=y)) +
    geom_line(data=newData, aes(x=pred.x, y=yhat), lty = ltyest) +   
    scale_x_continuous(breaks = xaxisval, labels = xaxislab)
  
  if (resid){
    sp = sp+geom_point(shape=1)
  }
  
  if (se){
    
    sp = sp + geom_line(data=newData2, aes(x=pred.x, y=yup), lty = ltyci) +
      geom_line(data=newData2, aes(x=pred.x, y=ylow), lty = ltyci) 
    
  }
  
  colnames



  sp2 = sp + xlab("Time") + ylab(paste0("Scaled Schoenfeld residuals for ", colnames(yy)[i]))
  
  
  #xlab = "Time"
  #ylab = paste0("Scaled Schoenfeld residuals for ", colnames(yy)[i])            
  
  print(sp2)
  #ggplotly(sp)

  
  
  
  
}
}
