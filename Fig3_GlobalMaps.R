# Made a meta-data chunk ala Sean Anderson 
# Created by:    Emily S. Darling
# Created:       15 March 2015
# Last modified: 1 April 2015
# Purpose:       global plots of climate variables and histograms of their distributions across reef pixels 

#Emily's Dropbox
setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/")

# ==============================================
# = Predicted probability of severe bleaching  =
# ==============================================

# 1. read in predicted bleaching layer
bl_pred <- raster("topmodel_pred_maps.tif")
print(bl_pred)
hist(bl_pred)   
plot(bl_pred)

#convert raster to points for ggplot
bl_pred.p <- rasterToPoints(bl_pred) 
  
#Make the points a dataframe for ggplot
df <- data.frame(bl_pred.p)  

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
head(df)

#recenter longitude to match pacific-centred world map 
#note center needs to be set the same here and in 0-global-base-map.R
center <- 163
df$long.recenter <-  ifelse(df$Longitude<center-180, df$Longitude+360, df$Longitude)
head(df)   
#hist(df$long.recenter)     

#read in Pacific-centred base layer map
#holy smokes source is cool (thanks Sean Anderson!)
setwd("/Users/emilydarling/Documents/Work/GitHub/PaperZero")  
source("0-global-base-map.R")

#add raster to base map layer in ggplot  
#change colours and fills in geom_polygon
# change fill of land, country lines, colour of background 
base <- ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), 
		size = 0.2, fill="white", colour = "grey95", data=worldmap) +
  ylim(-60, 90) +
  coord_equal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_raster(aes(long.recenter, Latitude, fill = MAP), data = df) +
  scale_fill_gradient("Bleaching probability", limits = c(0,1), low = "yellow", high = "red")       
base 

head(df)  
quartz()
# df$MAP are the predicted bleaching probabilities
quartz()
hist(df$MAP) 
min(df$MAP) ; max(df$MAP) 
mean(df$MAP) 

#ggplot histogram of bleaching responses
ggplot() +
  	geom_density(aes(x = MAP), data = df, fill = "yellow", alpha = 0.65) +
	scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
	scale_y_continuous(expand = c(0,0), limits = c(0,2)) 


#find top 5% of data	
quantile(df$MAP, probs = c(0.05, 0.1, 0.25,0.5,0.75,0.9,0.95,1))
qqnorm(df$MAP)           

# Refugia sensitivity analysis -----------------------------

# find 5% lowest predictions 
quantile(df$MAP, 0.05)
low5 <- df[df$MAP < quantile(df$MAP, 0.05), ]
nrow(low5)

# find 10% lowest predictions, plot 
quantile(df$MAP, 0.1)
low10 <- df[df$MAP < quantile(df$MAP, 0.1), ]
nrow(low10)

# find 25% lowest predictions, plot 
quantile(df$MAP, 0.25)
low25 <- df[df$MAP < quantile(df$MAP, 0.25), ]
nrow(low25)

# find 50% lowest predictions, plot 
quantile(df$MAP, 0.5)
low50 <- df[df$MAP < quantile(df$MAP, 0.5), ]
nrow(low50)

# global map plot , refugia
base <- ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), 
		size = 0.2, fill="white", colour = "grey95", data=worldmap) +
  ylim(-60, 90) +
  coord_equal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_raster(aes(long.recenter, Latitude), fill = "orange", data = low5) + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) +
  annotate("text", label = "5th percentile refugia", 
           x = 15, y = 50, hjust = 0, colour = "orange", size = 3)
base

# Stressed reefs sensitivity analysis -----------------------------

# find 5% highest predictions 
quantile(df$MAP, 0.95)
high5 <- df[df$MAP > quantile(df$MAP, 0.95), ]
nrow(high5)

# find 10% highest predictions 
quantile(df$MAP, 0.90)
high10 <- df[df$MAP > quantile(df$MAP, 0.90), ]
nrow(high10)

# find 25% highest predictions 
quantile(df$MAP, 0.75)
high25 <- df[df$MAP > quantile(df$MAP, 0.75), ]
nrow(high25)

# global map plot, stressed
base <- ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), 
               size = 0.2, fill="white", colour = "grey95", data=worldmap) +
  ylim(-60, 90) +
  coord_equal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_raster(aes(long.recenter, Latitude), fill = "red", data = high25) + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) +
  annotate("text", label = "25th percentile stressed", 
           x = 15, y = 50, hjust = 0, colour = "red", size = 3)
base









# ============================
# = TOP PREDICTOR VARIABLES  =
# ============================
# 2. Read in predictor variables cropped to reef pixels
# code below isn't working yet but placeholder for top variables from dredge models, AIC < 4

#Add code to read in all.var.reef
#Then pull variables from top model results
                                           
#running all.var.reef in memory from dredgemodel code 
#all.var.reef is NCDF file with predictors cropped to reef pixels 
#all.var.reef is a raster brick with 33 layers (or bands)
#A RasterBrick is a multi-layer raster object.
   
all.var.reef  
#print(c(hasValues(all.var.reef), inMemory(all.var.reef)))  
nlayers(all.var.reef) 
names(all.var.reef) 

#instead of slicing, coerce the whole thing to a points data.frame              
#want to slice, extract individual laters from raster brick and then plot, et                    
#plots all variables - there are data stored in memory
#levelplot(all.var.reef)  
#plot just layer 26 = autocorr to show there are values stored in memory
plot(all.var.reef, 26)

#coerce test to points and to data.frame 
test <- rasterToPoints(all.var.reef) 
head(test)
nrow(test)    
test <- as.data.frame(test)   
is.data.frame(test)   

#autocorr<-raster(all.var.reef [[26]])  
#diff_events_all<-raster(wv[1], varname="diff_events_all") 
#diff_events_periodmax<-raster(wv[1], varname="diff_events_periodmax")  
#events_pos_all<-raster(wv[1], varname="events_pos_all")     
#NSE<-raster(wv[1], varname="NSE")     
  
#Make appropriate column headings
names(test)[1:2] <- c("Longitude","Latitude")
head(test)

#recenter longitude to match pacific-centred world map 
#note center needs to be set the same here and in 0-global-base-map.R
center <- 163
test$long.recenter <-  ifelse(test$Longitude<center-180, test$Longitude+360, test$Longitude)
head(test)   
hist(test$long.recenter)      

#read in Pacific-centred base layer map
#holy smokes source is cool (thanks Sean Anderson!)
setwd("/Users/emilydarling/Documents/Work/GitHub/PaperZero")  
source("0-global-base-map.R")  

#autocorr
base <- ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), 
		size = 0.2, fill="white", colour = "grey95", data=worldmap) +
  ylim(-60, 90) +
  coord_equal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_raster(aes(long.recenter, Latitude, fill = autocorr), data = test) +
  scale_fill_gradient("Autocorr", limits = c(0,1), low = "yellow", high = "red")       
base

densityplot(test$autocorr)

#diff_events_all
base <- ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), 
		size = 0.2, fill="white", colour = "grey95", data=worldmap) +
  ylim(-60, 90) +
  coord_equal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_raster(aes(long.recenter, Latitude, fill = diff_events_all), data = test) +
  scale_fill_gradient("Difference events_all", low = "yellow", high = "red")       
base 

densityplot(test$diff_events_all) 
     
test$diff_events_all




       