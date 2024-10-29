#####Start tutorial######
##### Barrier island spatial visualization#######
#Aim is to visualize the barrier islands of the Netherlands and habitat types on Terschelling

#Loading the required packages:


###### Spatial packages #######
#To work with vector data
library(sf)
#to work with raster data
library("terra")
###############################


######    Plotting     #######
library(ggplot2)
#To plot a north arrow and scale bar
library("ggsn")
#To arrange several maps
library(gridExtra)

###############################


###### Data processing #######
library("tidyverse")


#https://epsg.io/7415
#Define coordinate reference system:
#Amersfoort with NAP and in meters
CoordinateReferenceSystem = st_crs("EPSG:7415")

#Read the Netherlands as a country shapefile:

TheNetherlands = read_sf(list.files("data/spatialData/vectorData/",pattern="shp",full.names = T)[1])
TerschellingHabitats = read_sf(list.files("data/spatialData/vectorData/",pattern="shp",full.names = T)[2])


#Read Terschelling raster file:
# 
# download.file("https://www.dropbox.com/s/gv9pp38ej0pv88h/DEM_Terschelling.tif?dl=1","data/spatialData/rasterData/DEM_Terschelling.tif")
TerschellingRaster = rast("data/spatialData/rasterData/DEM_Terschelling.tif")
#Transform coordinates to our coordinate reference system: 
TheNetherlands = st_transform(TheNetherlands,CoordinateReferenceSystem)
TerschellingHabitats = st_transform(TerschellingHabitats,CoordinateReferenceSystem)

TerschellingRaster = project(TerschellingRaster, "EPSG:7415")

#Convert to dataframe for plotting
TerschellingRaster_df = as.data.frame(TerschellingRaster, xy = TRUE)
names(TerschellingRaster_df) = c("x","y","height [m]")

#Create first plot to see what data we have:
plot(TheNetherlands,max.plot = 1,main="The Netherlands")
plot(TerschellingHabitats,max.plot = 1,main="Terschelling")
plot(TerschellingRaster,max.plot = 1,main="Terschelling")


#How does this data look like?
TheNetherlands


#We want to select our barrier islands
OurIslands = TheNetherlands$NAME_2=="Terschelling"|
  TheNetherlands$NAME_2=="Ameland"|
  TheNetherlands$NAME_2=="Schiermonnikoog"|
  TheNetherlands$NAME_2=="Vlieland"|
  TheNetherlands$NAME_2=="Texel"

#Create a new spatial object with only barrier islands:
BarrierIslands = TheNetherlands[OurIslands,]
#Create an object for only Terschelling
Terschelling = TheNetherlands[TheNetherlands$NAME_2=="Terschelling",]


####Explain how to plot maps with ggplot here first: 




#Plot first only Barrier islands
ggplot(BarrierIslands) +
  geom_sf(aes(fill = NAME_2))+
  geom_sf_label(aes(label = NAME_2))+
  ylab("Latitude") + xlab("Longitude")+
  ggtitle("Barrier islands of the Netherlands")+
  theme_bw()+ theme(legend.position = "none")+
  scalebar(BarrierIslands, dist = 10, dist_unit = "km",
           transform = FALSE, model = "WGS84")+
  north(BarrierIslands,location="topleft")

#This looks strange without the mainland...

#Change NAME_2 so that everything that isn't barrier island becomes mainland:
TheNetherlands$NAME_2[TheNetherlands$NAME_2!="Terschelling"&
                 TheNetherlands$NAME_2!="Ameland"&
                 TheNetherlands$NAME_2!="Schiermonnikoog"&
                 TheNetherlands$NAME_2!="Vlieland"&
                 TheNetherlands$NAME_2!="Texel"] <- "Mainland"


#Some tidyverse pipeline magic: 
#Basically creating a new shapefile based on NAME_2 and unifying all little polygons to a continuous one
BarrierIslandsMainland <- TheNetherlands %>%   group_by(NAME_2) %>%   summarise(geometry = st_union(geometry))



p1 = ggplot(BarrierIslandsMainland) +
  geom_sf(aes(fill = NAME_2))+
  geom_sf_label(aes(label = NAME_2))+
  ylab("Latitude") + xlab("Longitude")+
  ggtitle("Barrier islands of the Netherlands")+
  theme_bw()+ theme(legend.position = "none")+
  scalebar(BarrierIslandsMainland, dist = 10, dist_unit = "km",
           transform = FALSE, model = "WGS84")+
  north(BarrierIslandsMainland,location="topleft")

#plotting
p1
#This map is not exactly the focus that we would like


#Return limits of barrierIslands:
Limits = st_bbox(BarrierIslands)


p2 = ggplot(BarrierIslandsMainland) +
  geom_sf(aes(fill = NAME_2))+
  geom_sf_label(aes(label = NAME_2))+
  ylab("Latitude") + xlab("Longitude")+
  ggtitle("Zoomed map")+
  theme_bw()+ theme(legend.position = "none")+
  scalebar(BarrierIslands, dist = 10, dist_unit = "km",
           transform = FALSE, model = "WGS84")+
  north(BarrierIslands,location="topleft")+
  xlim(Limits["xmin"],Limits["xmax"])+
  ylim(Limits["ymin"],Limits["ymax"])

p2
#Better!

p3 = ggplot(Terschelling) +
  geom_sf(aes(fill = NAME_2))+
  geom_sf_label(aes(label = NAME_2))+
  ylab("Latitude") + xlab("Longitude")+
  ggtitle("Terschelling")+
  theme_bw()+ theme(legend.position = "none")+
  scalebar(Terschelling, dist = 4, dist_unit = "km",
           transform = FALSE, model = "WGS84")+
  north(Terschelling,location="topleft")
p3



p4 = ggplot(TerschellingRaster_df) + 
  geom_raster(aes(x, y, fill=`height [m]`)) +
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
  ylab("Latitude") + xlab("Longitude")+
  ggtitle("Terschelling")+
  theme_bw()+
  geom_sf(Terschelling,mapping=aes(),fill=NA,col="red")+ 
  scalebar(Terschelling, dist = 4, dist_unit = "km",
           transform = FALSE, model = "WGS84")+
  north(Terschelling,location="topleft")


p4
#Arrange all of the maps together:
lay <- rbind(c(1,1,2),
             c(1,1,3))

grid.arrange(p1,p2,p3,layout_matrix = lay)

#



#Combine the point data from Terschelling and our extracted Terschelling polygon
ggplot(Terschelling)+geom_sf(mapping=aes())+
  geom_sf(TerschellingHabitats,mapping=aes(col=Habitat))+
  facet_wrap(~YrsDvdd)+
  ggtitle("Terschelling Habitat Types")+
  theme_bw()+
  north(Terschelling,location="topleft")


#combine raster and point data
TerschellingHabitats_df = as.data.frame(TerschellingHabitats)
#Use terra to extract heights from digital elevation model:
TerschellingHabitats_df$Heights =  terra::extract(TerschellingRaster,vect(TerschellingHabitats))[,2]

ggplot(TerschellingHabitats_df,aes(x=Habitat,y=Heights,fill=Habitat))+
  geom_boxplot()+
  ggtitle("Habitats on Terschelling in function of elevation")+
  ylab("Height [m above NAP]")+
  xlab("Habitats")+
  ylim(c(-1,10))





##########################

#Reading Terschelling habitat data derived from synbiosys (https://www.synbiosys.alterra.nl/LVD2/#Kaart)
TerschellingHabitats = read.table("data/spatialData/Habitats/TerschellingHabitats.txt")
Terschelling = st_transform(Terschelling,crs = 4326)

#Take a look at the data:
glimpse(TerschellingHabitats)




#Finally, convert the TerschellingHabitats data to a shapefile:

TerschellingHabitatsSpatial <- st_as_sf(x = TerschellingHabitats,                         
                                        coords = c("lon", "lat"),
                                        crs = 28992)

#save the shape file in the current directory: 
st_write(TerschellingHabitatsSpatial,"HabitatsTerschelling.shp")


NewHeights = TerschellingRaster + 100 
mean(values(TerschellingRaster,na.rm=TRUE))
