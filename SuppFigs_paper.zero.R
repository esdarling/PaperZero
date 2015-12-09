
# Created by:    Emily S. Darling
# Created:       5 June 2015
# Last modified: 5 June 2015
# Purpose:       supplementary figures for paper zero


# correlation plots of variables
# load corr plot data 
setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/")
c <- read.csv("cor_all.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)   
head(c)
nrow(c) 
names(c)

c2 <- 
for (i in (2:38)) {
  values <- strsplit(c[,2], split = "\\*")
  values2 <- sapply(values, function(x) x[1])
  c2[,i] <- values2
  
}


values <- strsplit(c$period_max, split = "\\*")     
head(values2)
c2 <- 
  
# Get some data
data(mtcars)
# Get the correlation matrix
corr.mtcars <- cor(mtcars)
head(corr.mtcars)
# Change the column and row names for clarity
colnames(corr.mtcars) = c('Miles/gallon', 'Number of cylinders', 'Displacement', 'Horsepower', 'Rear axle ratio', 'Weight', '1/4 mile time', 'V/S', 'Transmission type', 'Number of gears', 'Number of carburetors')
rownames(corr.mtcars) = colnames(corr.mtcars)