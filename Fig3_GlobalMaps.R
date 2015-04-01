
library(rgeos)
library(raster)
library(rgdal)
library(dismo)
library(SDMTools)
library(rasterVis) 
library(ncdf) 
library(ncdf4) 
library(ggplot2) 
library(plyr) 

# Made a meta-data chunk ala Sean Anderson 
# Created by:    Emily S. darling
# Created:       15 March 2015
# Last modified: 1 April 2015
# Purpose:       global plots of climate variables and histograms of their distributions across reef pixels 

#Emily's Dropbox
setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/")

#use open.ncdf to open a .nc file and list stack of raster variables
nc <- open.ncdf("stats_ERSSTv4_updated.nc")
names(nc$var) 

#not sure why all.var.reef.nc doesn't have raster stack in it
#can remake later
#nc <- open.ncdf("all.var.reef.nc")
#names(nc$var) 

# TO DO
# 1. read in predicted bleaching layer
bl_pred <- raster("TopmodelPredmapMask.tif")
print(bl_pred)
histogram(bl_pred)   
plot(bl_pred)

#convert raster to points for ggplot
bl_pred.p <- rasterToPoints(bl_pred) 
  
#Make the points a dataframe for ggplot
df <- data.frame(bl_pred.p)  

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")

#recenter longitude to match pacific-centred world map 
#note center needs to be set the same here and in 0-global-base-map.R
center <- 115
df$long.recenter <-  ifelse(df$Longitude<center-180, df$Longitude+360, df$Longitude)
head(df)   
hist(df$long.recenter)    
     

# 2. Read in predictor variables cropped to reef pixels
# code below isn't work yet but placeholder for top variables from dredge models, AIC < 4
#autocorr<-raster(wv2[1], varname="autocorr")  
#diff_events_all<-raster(wv[1], varname="diff_events_all") 
#diff_events_periodmax<-raster(wv[1], varname="diff_events_periodmax")  
#events_pos_all<-raster(wv[1], varname="events_pos_all")     
#NSE<-raster(wv[1], varname="NSE")    

#read in Pacific-centred base layer map
#holy smokes source is cool (thanks Sean Anderson!)
setwd("/Users/emilydarling/Documents/Work/GitHub/PaperZero")  
source("0-global-base-map.R")

#add raster to base map layer in ggplot  
#change colours and fills in geom_polygon
# change fill of land, country lines, colour of background 
dev.new(width = 5, height = 3)
base <- ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), 
		size = 0.2, fill="white", colour = "grey95", data=worldmap) +
  ylim(-60, 90) +
  coord_equal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_raster(aes(long.recenter, Latitude, fill = MAP), data = df) +
  scale_fill_gradient("Bleaching probability", limits = c(0,1), low = "blue", high = "red")       
base 

        





# ================
# = test raster  =
# ================   
#load raster 
print(blmodel) ## from dredgemodel code

#convert raster to points for ggplot
blmodel.p <- rasterToPoints(blmodel) 
  
#Make the points a dataframe for ggplot
df <- data.frame(blmodel.p)  

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")

#try recentering long
center <- 115
df$long.recenter <-  ifelse(df$Longitude<center-180, df$Longitude+360, df$Longitude)
head(df)   
hist(df$long.recenter)  



dev.new(width = 8.9, height = 8)
base <- ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), size = 0.2, fill="white", colour = "grey95", data=worldmap.cp) +
  ylim(-60, 90) +
  coord_equal() + 
  geom_raster(aes(long.recenter, Latitude, fill = MAP), data = df) +
  scale_fill_gradient("Bleaching probability", limits = c(0,1), low = "blue", high = "red")  
base






       