library(dplyr)
library(reshape2)
library(car)
library(gdata)
library(stringr)
library(reports)
library(ggplot2)
library(ggmap) 

## code to add Keith provinces to each ReefBase observation in updated Donner datasets
# complete through 2010 bleaching observations

# load Donner dataset to add Keith provinces
setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/GLOBAL CORAL PAPERS/Paper0 - thermal refuges/PaperzeroReloaded")    
test <- read.csv("Bleaching_by_Region_w_DHW.csv", header = TRUE, 
                 stringsAsFactors = FALSE, strip.white = TRUE) 
head(test)
names(test)


#clip to relevant columns for province, then add back in 
d <- test %>% 
  select(ID, COUNTRY,LOCATION,SEVERITY_C,coords.x1,coords.x2)
head(d)

unique(d$SEVERITY_C)

