bee<-data.frame(fields=c("field1","field1","field2","field2"),transect=c(1,2,1,2),bees=c(7,9,110,150),round=c(1,2,1,2))
flower<-data.frame(fields=c("field1","field2"),flowers=c(5,20),round=c(1,2))

bee$fieldRound<-paste(bee$fields,bee$round)
flower$fieldRound<-paste(flower$fields,flower$round)

merge(bee,flower,by=c('fields','round'))
merge(bee,flower,by='fieldRound')
