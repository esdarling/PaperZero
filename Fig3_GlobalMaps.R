rm(list=ls())
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


setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/")

wv2<-list.files(pattern='nc')   
names(wv2)[1]       


ncin <- open.ncdf("all.var.DHM.reef_15Mar2015.nc")
print(ncin)

#read in top predictors from models
autocorr<-raster(wv2[1], varname="autocorr")  
diff_events_all<-raster(wv[1], varname="diff_events_all") 
diff_events_periodmax<-raster(wv[1], varname="diff_events_periodmax")  
events_pos_all<-raster(wv[1], varname="events_pos_all")     
NSE<-raster(wv[1], varname="NSE")    


# =================================
# = ggplot Pacific recentred map  =
# =================================  
#http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot
center <- 115 # positive values only - US centered view is 260
# shift coordinates to recenter worldmap
worldmap <- map_data ("world")
worldmap$long.recenter <-  ifelse(worldmap$long  < center - 180 , worldmap$long + 360, worldmap$long)
 
### Function to regroup split lines and polygons
# takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}
 
### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}
 
# now regroup
worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")
 
# close polys
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var

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






       