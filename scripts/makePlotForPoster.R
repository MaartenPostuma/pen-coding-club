rm(list=ls())
library(ggplot2)
library(ggpubr)
library("ggpattern")
colorBlindBlack8<-c("#000000","#E69F00","#56B4E9","#009E73","#0072B2","#D55E00","#CC79A7","#F0E442")

plotData<-read.csv("Poster/posterPlot.csv")

plotData$AltitudeNum
ggplot(plotData,aes(x=Pop,y=lam,col=Pop,fill=Pop,shape=source,))+
  geom_point(size=4)+
  theme_pubclean()+
  scale_colour_manual("",values=colorBlindBlack8)+
  scale_fill_manual("",values=colorBlindBlack8)

plotData$intGrowth<-log(plotData$lam)
plotData$source[plotData$source=="prediction"]<-substr(plotData$source[plotData$source=="prediction"],0,4)

plotData$source<-factor(plotData$source,c("new","pred","old"),labels=c("2022-2023","prediction","2005-2007"))

ggplot(plotData,aes(y=source,x=intGrowth,fill=Pop,shape=source,alpha=source))+
  geom_col(col="black")+
  theme_pubclean()+
  scale_fill_manual("",values=colorBlindBlack8)+
  scale_alpha_manual("",values=c(0.3,1,0.6))+
  facet_grid(Pop~.)+
  geom_vline(xintercept=0)+
  theme(axis.text.x = element_text(angle = 45),
        axis.ticks = element_blank(),
        panel.grid.major.y= element_blank(),
        panel.grid.major.x = element_line(color = "grey",
                                          size = 0.5,
                                          linetype = 2),
        legend.position="none")+
  ylab("")+
  xlab("Intrinsic growth rate [r]")



plotData$lamUp<-log(plotData$lam+plotData$sds)
plotData$lamDown<-log(plotData$lam-plotData$sds)


ggplot(plotData,aes(y=source,x=intGrowth,col=Pop,fill=Pop,shape=source))+
  geom_point(size=4)+
  geom_segment(aes(y=source,x=lamUp,xend=lamDown,yend=source))+
  theme_pubclean()+
  scale_colour_manual("",values=colorBlindBlack8)+
  scale_fill_manual("",values=colorBlindBlack8)+
  scale_shape_manual("",values=c(23,25,22))+
  facet_grid(Pop~.)+
  geom_vline(xintercept=0)+
  theme(axis.text.x = element_text(angle = 45),
        axis.ticks = element_blank(),
        panel.grid.major.y= element_blank(),
        panel.grid.major.x = element_line(color = "grey",
                                          size = 0.5,
                                          linetype = 2),
        legend.position="none")+
  ylab("")+
  xlab("Intrinsic growth rate [r]")



ggsave("C:/Users/postu003/Dropbox/PhD/Conferences/BES/Poster/lambdas.pdf",unit="cm",height=34,width=24)
