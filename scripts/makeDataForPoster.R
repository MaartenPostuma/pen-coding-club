rm(list=ls())
predictionsData<-read.csv("C:/Users/postu003/Dropbox/PhD/IPM_old/IPM_Alpine_Lyrata/results/globalChanges.csv")
oldData<-read.table("C:/Users/postu003/Dropbox/PhD/IPM_old/IPM_Alpine_Lyrata/results/bootLamPop.tsv")
newData<-read.table("C:/Users/postu003/Dropbox/PhD/IPM_new/results/bootLamPop.tsv")


oldSub<-data.frame(Pop=paste0("NO",1:8),lam=colMeans(oldData[,10:17],na.rm=T),sds=apply(oldData[,10:17],2,sd,na.rm=T))
newSub<-data.frame(Pop=paste0("NO",1:8),lam=colMeans(newData,na.rm=T),sds=apply(newData,2,sd,na.rm=T))

library(ggplot2)
library(reshape2)


head(predictionsData)
head(oldData)
head(newData)

predSub<-predictionsData[predictionsData$method=="All Change"&predictionsData$Country=="NO",c("Pop","lamDiff")]
predSub$lam<-oldSub$lam+predSub$lamDiff
predSub$source<-"prediction"
predSub$sds<-NA
oldSub$source<-"old"
newSub$source<-"new"

allData<-rbind(predSub[,c("Pop","lam","sds","source")],oldSub,newSub)
allData$Country<-substr(allData$Pop,0,2)
norwayData<-allData[allData$Country=="NO",]


norwayData<-allData[grep("NO",allData$Pop),]

popInfo<-read.csv("C:/Users/postu003/Dropbox/PhD/IPM_old/IPM_Alpine_Lyrata/data/popInfo.csv")


plotData<-merge(norwayData,popInfo,by="Pop")

write.csv(plotData,"Poster/posterPlot.csv",row.names = F)


