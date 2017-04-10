library(randomForestSRC)
data = read.table("~/Dropbox/GSD/Studies/Web-Tools(Devel)/geneSurv/www/data/GSE2034.txt", header = T, sep = "\t")

time = "dmfs_time"
status = "dmfs_event"
dropRate = 0.1

rsf<- rfsrc(Surv(dmfs_time, dmfs_event) ~ ., data = data, ntree = 1000, tree.err=TRUE, importance = TRUE)
varImp = cbind.data.frame(var = names(rsfInitial$importance), importance = rsfInitial$importance)
varImpInitial = cbind.data.frame(variable = names(rsfInitial$importance), importance = rsfInitial$importance)

excludeVarNumber = 0
errorRateList = list()
varImpList = list()
i=0
excludeVarNumber = 1

while(((ncol(data)-2) > 2) && excludeVarNumber !=0){
  i=i+1
  varImp = varImp[order(varImp$importance,decreasing = TRUE),]
  errorRate = mean(rsf$err.rate)
  excludeVarNumber = round(nrow(varImp)*dropRate)
  varsInModel = varImp[1:(nrow(varImp) - excludeVarNumber),]
  
  errorRateList[[i]] = errorRate
  varImpList[[i]] = varImp
  
  data = data[,c(time, status, rownames(varsInModel))]
  varImp = varImp[rownames(varsInModel),]
  
  rsf <- rfsrc(Surv(dmfs_time, dmfs_event) ~ ., data = data, ntree = 1000, tree.err=TRUE, importance = FALSE)
  
  print(i)
  print(ncol(data))
  
}
CIsd = 1
oobError = do.call(rbind.data.frame,errorRateList)
colnames(oobError) = "OOB Error"

minOOB = oobError[which.min(oobError$`OOB Error`),]
sdMinOOB <- sqrt(minOOB * (1 -minOOB) * (1/nrow(data)))

minOOBci <- minOOB + CIsd * sdMinOOB

indx = which(oobError$`OOB Error`< minOOBci)

candiateFeatures = cbind.data.frame(variable = indx,lenght = do.call(rbind.data.frame, lapply(varImpList[indx], nrow)))
colnames(candiateFeatures) = c("variable", "lenght")

bestFeatures <- varImpList[[candiateFeatures$var[which.min(candiateFeatures$lenght)]]]

