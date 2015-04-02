rm(list=ls())
library(rgeos)
library(raster)
library(rgdal)
library(dismo)
library(SDMTools)

# Created by:    Joseph Maina
# Created:       25 March 2015
# Last modified: 1 April 2015
# Purpose:       analysis of multiband climate wavelets from ERSSTv4 data        

#Emily's dropbox
setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/")   

#Maina's dropbox     
setwd("~/Dropbox/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/")

bl<-read.csv('CoralBleaching.csv')
head(bl)


##lh<-read.csv('data.csv')
wv<-list.files(pattern=".updated.nc$")
##[2015-03-14, 10:22:23 PM] Maina: wv<-list.files(pattern='nc')
##[2015-03-14, 10:22:34 PM] Maina: that just lists all the files with nc extension
##then the file we want is position 9  
##but THIS POSITION CAN CHANGE, check [#] - which changes as we add more nc files     
###load the netcdf file as a multiband image with wavelet variables   
##[2015-03-14, 10:19:34 PM] Maina: it doesn't like the nc file to be called dirctly
##[2015-03-14, 10:20:37 PM] Maina: so we have to use for example wv[9] to call the wavelet rasterstack
names(wv)                                                                
  
names<-stack(wv) ##get names
period_max<-raster(wv[1], varname="period_max")
pct_signif_periodmax<-raster(wv[1], varname="pct_signif_periodmax")
pct_coi_periodmax<-raster(wv[1], varname="pct_coi_periodmax")
pct_ispos_signif<-raster(wv[1], varname="pct_ispos_signif")
events_pos_periodmax<-raster(wv[1], varname="events_pos_periodmax")
events_neg_periodmax<-raster(wv[1], varname="events_neg_periodmax")
events_pos_all<-raster(wv[1], varname="events_pos_all")
events_neg_all<-raster(wv[1], varname="events_neg_all")
maxpower_pos<-raster(wv[1], varname="maxpower_pos")
maxpower_neg<-raster(wv[1], varname="maxpower_neg")
maxpower_events_pos<-raster(wv[1], varname="maxpower_events_pos")
maxpower_events_neg<-raster(wv[1], varname="maxpower_events_neg")
maxpower_pos_norm<-raster(wv[1], varname="maxpower_pos_norm")
maxpower_neg_norm<-raster(wv[1], varname="maxpower_neg_norm")
maxpower_events_pos_norm<-raster(wv[1], varname="maxpower_events_pos_norm")
maxpower_events_neg_norm<-raster(wv[1], varname="maxpower_events_neg_norm")
period_events_pos<-raster(wv[1], varname="period_events_pos")
period_events_neg<-raster(wv[1], varname="period_events_neg")
duration_events_pos<-raster(wv[1], varname="duration_events_pos")
duration_events_neg<-raster(wv[1], varname="duration_events_neg")
meanSST<-raster(wv[1], varname="meanSST")
varSST<-raster(wv[1], varname="varSST")
trendSST<-raster(wv[1], varname="trendSST")
var_seasonal<-raster(wv[1], varname="var_seasonal")
pct_seasonal<-raster(wv[1], varname="pct_seasonal")
autocorr<-raster(wv[1], varname="autocorr")
NSE<-raster(wv[1], varname="NSE")
diff_events_periodmax<-raster(wv[1], varname="diff_events_periodmax")
diff_events_all<-raster(wv[1], varname="diff_events_all")
NSE_trend<-raster(wv[1], varname="NSE_trend")
autocorr_trend<-raster(wv[1], varname="autocorr_trend")
events_pos_all_trend<-raster(wv[1], varname="events_pos_all_trend")
maxpower_events_pos_trend<-raster(wv[1], varname="maxpower_events_pos_trend")

##this creates a rasterstack of all the images loaded above.the names between the parethesis should match the names of rasters loaded above
##all.var below is a rasterstack of wavelet variables  
all.var<-stack(period_max, pct_signif_periodmax, pct_coi_periodmax, pct_ispos_signif, events_pos_periodmax, events_neg_periodmax, events_pos_all, events_neg_all, maxpower_pos, maxpower_neg, maxpower_events_pos, maxpower_events_neg, maxpower_pos_norm, maxpower_neg_norm, maxpower_events_pos_norm, maxpower_events_neg_norm, period_events_pos, period_events_neg, duration_events_pos, duration_events_neg, meanSST, varSST, trendSST, var_seasonal, pct_seasonal, autocorr, NSE, diff_events_periodmax, diff_events_all, NSE_trend, autocorr_trend, events_pos_all_trend, maxpower_events_pos_trend)

##the following sections will calculate DHM and add to the rasterstack above    
##Accumulate DHM (1910-1990) based on climatology from 1910-1940
ersst<-list.files(pattern="nc")

#"climatology_ERSSTv4_1960to1990.nc" = monthly climatology
#"ERSST_v4_1910to1990.nc" #sst data
#"climatology_ERSSTv4_1910to1939.nc" = monthly climatology
##mmm is calculated as a maximum of monthly climatology 

#load the ERSST files 
mmm_a<-max(stack("climatology_ERSSTv4_1910to1939.nc" ))
mmm_b<-max(stack("climatology_ERSSTv4_1960to1990.nc" ))
sstts<-stack("ERSST_v4_1910to1990.nc")

##Calculate MMM based anomalies       
#anom_a<-sstts - (mmm_a + 1) #anom based on 1910-1939 clim
#anom_b<-sstts - (mmm_b + 1)#anom based on 1960-1990 clim
anom_a<-sstts - (mmm_a) #anom based on 1910-1939 clim
anom_b<-sstts - (mmm_b)#anom based on 1960-1990 cli


##anom_c is based on 1985-2014 sst and on on 1960-1990 clim
sstts_c<-stack("ERSST_v4_1985to2014.nc")
#anom_c<-sstts_c - (mmm_b + 1) #anom based on 1960-1990 clim
anom_c<-sstts_c - (mmm_b) #anom based on 1960-1990 clim
##accumulate retrospective DHM
anom_retro_cumm<-calc( anom_b , function(x) { x[ x < 0 ] <- 0; return(x) } )
CummDHM_retro <- calc(anom_retro_cumm, sum) 

###dhm with counts greater than 1
anom_retro_count<-calc( anom_retro_cumm , function(x) { x[ x > 0 ] <- 1; return(x) } )
DHM_retro_cnt<-calc(anom_retro_count, sum)

##accumulate satellite period DHM
anom_sat_cumm<-calc( anom_c , function(x) { x[ x < 0 ] <- 0; return(x) } )
CummDHM_sat <- calc(anom_sat_cumm, sum) 

###satellite period DHM with counts greater than 1
anom_sat_count<-calc( anom_sat_cumm , function(x) { x[ x > 0 ] <- 1; return(x) } )
DHM_sat_cnt<-calc(anom_sat_count, sum)

###rasters not of equal extents therefore rotate using raster package
names(CummDHM_retro)<-"CummDHM_retro"
CummDHM_retro<-rotate(CummDHM_retro)
names(DHM_retro_cnt)<-"DHM_retro_cnt"
DHM_retro_cnt<-rotate(DHM_retro_cnt)
names(CummDHM_sat)<-"CummDHM_sat"
CummDHM_sat<-rotate(CummDHM_sat)
names(DHM_sat_cnt)<-"DHM_sat_cnt"
DHM_sat_cnt<-rotate(DHM_sat_cnt)

#add layers to the stack
all.var<-addLayer(all.var,CummDHM_retro,DHM_retro_cnt, CummDHM_sat,DHM_sat_cnt)


#correlation
all.var<-all.var*1
layerStats(all.var,'pearson')


##extract all layers
CoralBleach_Extr<-extract(all.var, bl[,7:6])
##lh_Extr<-extract(all.var, lh[,11:10], na.rm=T, Buffer=50000, fun=mean)

##bind extracted with bleaching data
CoralBleach_Extr<-cbind(bl,CoralBleach_Extr)
write.csv(CoralBleach_Extr,"CoralBleach_Extr.csv" )

reef.layer<-raster("reef_1x1.nc")

reef.layer<-rotate(reef.layer)
all.var.r <- resample(all.var, reef.layer, method='bilinear')

all.var.reef<-mask(all.var.r, reef.layer)

# write to netcdf 
if (require(ncdf)) {	
 maskedvar <- writeRaster(all.var.reef, filename='all.var.reef.nc', format="CDF", overwrite=TRUE)   
}

###the above file is now written to file 
##to read it call the respectve libraries
#load the file
my.masked.variables<-stack('all.var.reef.nc')

#to see the names of the variables
names(my.masked.variables)

#select one variable from the stack 
NSE<-raster(my.masked.variables, "NSE")
##to semect a few variables
var.subset<-stack(my.masked.variables, c("NSE","autocorr"))



##if starting from reading extracted values start here

CoralBleach_Extr<-read.csv("CoralBleach_Extr.csv")
CoralBleach_Extr1<-subset(CoralBleach_Extr,SEVERITY_CODE>-1)#remove -1 code

##write.csv(CoralBleach_Extr,"lh_Extr.csv" )
##write.csv(lh_and_wavelets,"lh_and_wavelets.csv" )

###comparing wavelet metrics with bleaching databaseCoralBleach_Extr<-read.csv("~/Dropbox/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/lh_Extr.csv")

##CoralBleach_Extr<-read.csv("CoralBleach_Extr.csv")

##1998
CoralBleach_Extr2<-subset(CoralBleach_Extr1,YEAR==c(1998))
c<-cbind(CoralBleach_Extr2[9], stack(CoralBleach_Extr2[12:48]))
d<-subset(c, SEVERITY_CODE==c(0,3))
fig1<-ggplot(data=as.data.frame(d), aes(factor(SEVERITY_CODE), values )) + geom_boxplot(aes(fill = factor(SEVERITY_CODE)),alpha = .6,size = 1) +
scale_fill_brewer(palette = "Set2") + facet_wrap(~ind, scales="free_y") + stat_summary(fun.y = "mean", geom = "text", label="----", size= 10, color= "white") + stat_summary(fun.y=mean, colour="red", geom="text", size= 4.5, show_guide = FALSE, vjust=-0.7, aes( label=round(..y.., digits=2))) + theme(legend.position="bottom") + theme(legend.title=element_blank()) +  xlab("Bleaching severity") + ylab("SST attribute") + ggtitle("1998 bleaching")

###stats


##1998,2005,2006
CoralBleach_Extr2<-subset(CoralBleach_Extr1,YEAR==c(1998,2005, 2006))
c<-cbind(CoralBleach_Extr2[10], stack(CoralBleach_Extr2[12:48]))
d<-subset(c, SEVERITY_CODE==c(0,3))
fig2<-ggplot(data=as.data.frame(d), aes(factor(SEVERITY_CODE), values )) + geom_boxplot(aes(fill = factor(SEVERITY_CODE)),alpha = .6,size = 1) +
scale_fill_brewer(palette = "Set2") + facet_wrap(~ind, scales="free_y") + stat_summary(fun.y = "mean", geom = "text", label="----", size= 10, color= "white") + stat_summary(fun.y=mean, colour="red", geom="text", size= 4.5, show_guide = FALSE, vjust=-0.7, aes( label=round(..y.., digits=2))) + theme(legend.position="bottom") + theme(legend.title=element_blank()) +  xlab("Bleaching severity") + ylab("SST attribute") + ggtitle("98,05&06 bleaching")



##2005,2006
CoralBleach_Extr2<-subset(CoralBleach_Extr1,YEAR==c(2005, 2006))
c<-cbind(CoralBleach_Extr2[10], stack(CoralBleach_Extr2[12:48]))
d<-subset(c, SEVERITY_CODE==c(0,3))
fig3<-ggplot(data=as.data.frame(d), aes(factor(SEVERITY_CODE), values )) + geom_boxplot(aes(fill = factor(SEVERITY_CODE)),alpha = .6,size = 1) +
scale_fill_brewer(palette = "Set2") + facet_wrap(~ind, scales="free_y") + stat_summary(fun.y = "mean", geom = "text", label="----", size= 10, color= "white") + stat_summary(fun.y=mean, colour="red", geom="text", size= 4.5, show_guide = FALSE, vjust=-0.7, aes( label=round(..y.., digits=2))) + theme(legend.position="bottom") + theme(legend.title=element_blank()) +  xlab("Bleaching severity") + ylab("SST attribute") + ggtitle("05&06 bleaching")


##all data
c<-cbind(CoralBleach_Extr1[10], stack(CoralBleach_Extr1[12:47]))
d<-subset(c, SEVERITY_CODE==c(0,3))
fig4<-ggplot(data=as.data.frame(d), aes(factor(SEVERITY_CODE), values )) + geom_boxplot(aes(fill = factor(SEVERITY_CODE)),alpha = .6,size = 1) +
scale_fill_brewer(palette = "Set2") + facet_wrap(~ind, scales="free_y") + stat_summary(fun.y = "mean", geom = "text", label="----", size= 10, color= "white") + stat_summary(fun.y=mean, colour="red", geom="text", size= 4.5, show_guide = FALSE, vjust=-0.7, aes( label=round(..y.., digits=2))) + theme(legend.position="bottom") + theme(legend.title=element_blank()) +  xlab("Bleaching severity") + ylab("SST attribute") + ggtitle("Bleaching_all years")



####
library(corrplot)
e<-CoralBleach_Extr1[12:47]
cor.all <- cor(e, use = "na.or.complete")
corrplot(cor.p, method="number",shade.col=NA, tl.col="black", tl.srt=45)
corrplot.mixed(cor.all, lower = "number", upper = "shade")

###Regression Trees
sf<-subset(CoralBleach_Extr1, YEAR==1998)
f<-sf[c(9,12:33)]
f<-subset(f, SEVERITY_CODE==c(0,3))
fit <- rpart(factor(SEVERITY_CODE) ~ maxpower_pos + period_max, data = f)
plot(fit)
text(fit)


lhw<-lh_and_wavelets[c(13:18,69:90)]
lhw1<-cbind(lh_and_wavelets[13:18], stack(lh_and_wavelets[69:90]))

##stats
setwd("~/Dropbox/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/")
CoralBleach_Extr<-read.csv("CoralBleach_Extr.csv")
CoralBleach_Extr1<-subset(CoralBleach_Extr,SEVERITY_CODE>-1)
CoralBleach_pa<-subset(CoralBleach_Extr1, SEVERITY_CODE==c(0,3))
CoralBleach_pa$SEVERITY_CODE[CoralBleach_pa$SEVERITY_CODE>2] <- 1
levels(as.factor(CoralBleach_pa$SEVERITY_CODE))
CoralBleach_pa<-na.exclude(CoralBleach_pa)
colnames(CoralBleach.pa)
_____________________________________________________________________________________________
###Cool correlation package that prodices a table
corstarsl <- function(x){ 
require(Hmisc) 
x <- as.matrix(x) 
R <- rcorr(x)$r 
p <- rcorr(x)$P 

## define notions for significance levels; spacing is important.
mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))

## trunctuate the matrix that holds the correlations to two decimal
R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 

## build a new matrix that includes the correlations with their apropriate stars 
Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
diag(Rnew) <- paste(diag(R), " ", sep="") 
rownames(Rnew) <- colnames(x) 
colnames(Rnew) <- paste(colnames(x), "", sep="") 

## remove upper triangle
Rnew <- as.matrix(Rnew)
Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
Rnew <- as.data.frame(Rnew) 

## remove last column and return the matrix (which is now a data frame)
Rnew <- cbind(Rnew[1:length(Rnew)-1])
return(Rnew) 
} 

cor.all<-corstarsl(e)

write.csv(cor.all,"cor_all.csv" )
If one employs the xtable package that produces LaTeX tables from within R, xtable(corstarsl(swiss[,1:4])) produces this:
_____________________________________________________________________________________________________
library(glmulti)
library(MASS)
library(mgcv)
library(sjPlot)

BleachWavelet<-glmulti(y="SEVERITY_CODE", xr=c("period_max","events_pos_all","maxpower_pos","diff_events_periodmax","duration_events_pos","pct_signif_periodmax","events_neg_all","maxpower_neg","diff_events_all","duration_events_neg","pct_coi_periodmax","maxpower_events_pos","pct_ispos_signif","maxpower_events_neg","events_pos_periodmax","events_neg_periodmax","NSE","meanSST","trendSST","pct_seasonal","autocorr" ), name="BleachWavelet",fitfunction=glm,family=binomial(link=logit), data=CoralBleach.pa, crit=AIC,maxsize =2, level=1)

write(BleachWavelet, file = "BleachWavelet_2level.csv")

BleachWavelet@formulas[3] 
importances <- summary(BleachWavelet)$termweights

mod1<-glm(SEVERITY_CODE ~ events_pos_all, family=binomial(link=logit), data=CoralBleach.pa)

mod2<-glm(SEVERITY_CODE ~ NSE, family=binomial(link=logit), data=CoralBleach.pa)
sjp.glm(mod1)


###############
GLM to extract AIC
###############

rm(list=ls())
library(TSA)
library(mgcv)
library(plyr)


##################
## running GLM##
##################
setwd("~/Dropbox/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/")
CoralBleach_Extr<-read.csv("CoralBleach_Extr.csv", header=TRUE, stringsAsFactors=FALSE)
CoralBleach_Extr1<-subset(CoralBleach_Extr,SEVERITY_CODE>-1)
CoralBleach_pa<-subset(CoralBleach_Extr1, SEVERITY_CODE==c(0,3))
CoralBleach_pa$SEVERITY_CODE[CoralBleach_pa$SEVERITY_CODE>2] <- 1
levels(as.factor(CoralBleach_pa$SEVERITY_CODE))
CoralBleach.pa<-na.exclude(CoralBleach_pa) ##all dataset
#CoralBleach_.pa.98<-subset(CoralBleach_pa,YEAR==c(1998))##only1998
#CoralBleach_pa.980506<-subset(CoralBleach_pa,YEAR==c(1998,2005, 2006))##


xvals<-c("period_max","events_pos_all","maxpower_pos","diff_events_periodmax","duration_events_pos","pct_signif_periodmax","events_neg_all","maxpower_neg","diff_events_all","duration_events_neg","pct_coi_periodmax","maxpower_events_pos","pct_ispos_signif","maxpower_events_neg","events_pos_periodmax","events_neg_periodmax","NSE","meanSST","trendSST","pct_seasonal","autocorr" )

outputs <- c()


for(i in xvals){
    models <- glm(all.data[,"SEVERITY_CODE"] ~ all.data[,i], family=binomial(link=logit))

    tmp <- summary(models)[["coefficients"]][2,c("Estimate", "Std. Error", "z value", "Pr(>|z|)")]
    intercept<-summary(models)[["coefficients"]][1,"Estimate"]
    tmp <- c(intercept, tmp, "AIC" = AIC(models))
    outputs <- rbind(outputs, tmp)
}

rownames(outputs) <- xvals
colnames(outputs)<-c("Intercept","Estimate","Std.Error","zvalue","Pr","AIC")
write.csv(outputs, "GLM_outputs.csv")
outputs<-as.data.frame(outputs)

##calculate predicted estimates
##http://stats.stackexchange.com/questions/20835/find-the-equation-from-generalized-linear-model-output
library(boot)
ys <- c() ###added
for(i in 1:nrow(outputs)){
    xl <- rownames(outputs)[i]
    x <- seq(min(all.data[,xl]), max(all.data[,xl]),length=100)
    ##calculate probability curve
    #r.fn <- function(x) exp(outputs[i,"Estimate"]*x + outputs[i,"Intercept"])
    r.fn <- function(x) inv.logit(outputs[i,"Intercept"] + outputs[i,"Estimate"]*x )
    y <-r.fn(x)
    ##confidence interval
    #upper<- y + inv.logit(outputs[i,"Std.Error"])
    #lower<- y - inv.logit(outputs[i,"Std.Error"])
    ys <- cbind(ys,xl,x, y) 
}

write.csv(ys, "probability.csv")

##colnames(ys) <- rownames(outputs) ##added

probability.stack<-read.csv("probability_stack.csv")
ggplot(probability.stack, aes(x,y)) + geom_line() + facet_wrap(~xl, scale="free_x") + theme_bw() +xlab("Predictor scale") + ylab("Pedicted probability - bleaching") + ggsave("xyz.png")



##+ geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2)





##### regression trees

install.packages("tree")
library(tree)
library(rpart)
tree.data<-CoralBleach.pa[c("SEVERITY_CODE" ,"NSE","events_pos_all", "diff_events_all" )]
tree.data1<-CoralBleach.pa[c("SEVERITY_CODE" ,"NSE","events_pos_all", "diff_events_all" )]

model<-rpart(SEVERITY_CODE~.,data=tree.data)
plot(model)
text(model)  

########
#using dredge package with correlated variables removed
#load the data above
###################

###################
library(MASS)
library(mgcv)
library(MuMIn)


all.data<-data.frame(CoralBleach_pa[,c(10, 12:31,33,35:40,45:48 )])
predictors<-all.data[,c(2:32)]
predictors<-predictors[, order(colnames(predictors))]


is.correlated <- function(i, j, data, conf.level = .95, cutoff = 0.2, ...) {
 	if(j >= i) return(NA)
 	ct <- cor.test(data[, i], data[, j], conf.level = conf.level, ...)
 	ct$p.value > (1 - conf.level) || abs(ct$estimate) <= cutoff
 }

 # Need vectorized function to use with 'outer'
 vCorrelated <- Vectorize(is.correlated, c("i", "j"))

 # Create logical matrix
 smat <- outer(1:31, 1:31, vCorrelated, data = predictors)

 nm <- colnames(predictors[c(1:31)])

 dimnames(smat) <- list(nm, nm)
write.csv(smat,"smat.csv")
sum(!is.na(smat[!lower.tri(smat)]))

###############################################
 ### A simpler case: exclude only pairs of variables having cor. coefficient
### r > 0.5
smat <- abs(cor(predictors)) <= .2
smat[!lower.tri(smat)] <- NA
##############################################
##set global model #removed mean sst and trend sst
forml<-glm(SEVERITY_CODE ~(autocorr+CummDHM_retro + CummDHM_sat + DHM_retro_cnt + DHM_sat_cnt + diff_events_all + diff_events_periodmax + duration_events_neg + duration_events_pos + events_neg_all + events_neg_periodmax + events_pos_all + events_pos_periodmax + maxpower_events_neg + maxpower_events_neg_norm + maxpower_events_pos + maxpower_events_pos_norm + maxpower_neg + maxpower_neg_norm + maxpower_pos + maxpower_pos_norm + NSE + pct_coi_periodmax + pct_ispos_signif + pct_seasonal + pct_signif_periodmax + period_events_neg + period_events_pos + period_max + var_seasonal + varSST ), family=binomial(link=logit),  data=all.data)

options(na.action = "na.fail") 

system.time(modelmix <- dredge(forml, subset = smat, trace=2, evaluate=FALSE, extra = c("R^2", F = function(x)
    summary(x)$fstatistic[[1]]) ))

write.csv(modelmix,"modelmix.csv")


##subset the modelmix to obtaon the top models
# Get the top model:
m10<-get.models(modelmix, subset =1)[[1]]

##Get top-most models
topmostmodels<- get.models(modelmix, subset = delta < 4)

#Get all models
allmodels<-get.models(modelmix, subset=TRUE)

models.ave=model.avg(topmostmodels)
##run the top models manually
##model average the top models
##apply the model average to maps below

##All models
allmodels_ave=model.avg(modelmix, fit=TRUE)
topmodels_ave=model.avg(topmostmodels)

topmodel.pred.maps=predict(object=all.var, model=topmodels_ave, type='response',factors=list(c("period_max" , "pct_signif_periodmax" , "pct_coi_periodmax" , "pct_ispos_signif", "events_pos_periodmax","events_neg_periodmax","events_pos_all", "events_neg_all", "maxpower_pos" ,  "maxpower_neg", "maxpower_events_pos" ,  "maxpower_events_neg"  ,
"period_events_pos","period_events_neg" , "NSE","duration_events_pos" ,  "duration_events_neg" , "meanSST", "trendSST" ,      "pct_seasonal"  , "autocorr" ,    "diff_events_periodmax" ,"diff_events_all  ")), progress="text", format="GTiff")

 allmodel.pred.maps=predict(object=all.var, model=allmodels_ave, type='response',factors=list(c("period_max" , "pct_signif_periodmax" , "pct_coi_periodmax" , "pct_ispos_signif", "events_pos_periodmax","events_neg_periodmax","events_pos_all", "events_neg_all", "maxpower_pos" ,  "maxpower_neg", "maxpower_events_pos" ,  "maxpower_events_neg"  ,
 "period_events_pos","period_events_neg" , "NSE","duration_events_pos" ,  "duration_events_neg" , "meanSST", "trendSST" ,      "pct_seasonal"  , "autocorr" ,    "diff_events_periodmax" ,"diff_events_all  ")), progress="text", format="GTiff")

writeRaster(topmodel.pred.maps, filename="topmodel_pred_maps.tif", format="GTiff", overwrite=TRUE)
writeRaster(allmodel.pred.maps, filename="allmodel_pred_maps.tif", format="GTiff", overwrite=TRUE)
plot(allmodel.pred.maps,zlim=c(0,1), axes=FALSE, box=FALSE)

##############
predictions_march
##################
all.var<stack(CummDHM_retro,CummDHM_sat,NSE,autocorr,diff_events_all,diff_events_periodmax,duration_events_neg,duration_events_pos,events_neg_all,events_neg_periodmax,events_pos_all,events_pos_periodmax,maxpower_events_neg,maxpower_events_pos,maxpower_neg,maxpower_pos,meanSST,pct_coi_periodmax,pct_ispos_signif,pct_seasonal,pct_signif_periodmax,period_events_neg,period_events_pos,period_max,trendSST,varSST,var_seasonal)  

load("/Users/josephmaina/Dropbox/PaperZero/Analysis/world/SSTanom_from1910_ERSSTv4/rstudio_export/DredgeModelWavelet.RData")


topmostmodels<- get.models(DredgeModelWavelet, subset = delta < 10)
topmodels_ave=model.avg(topmostmodels, fit=TRUE)

topmodel.pred.map=predict(object=all.var, model=topmodels_ave, type='response',factors=list(c( "CummDHM_retro"  , "CummDHM_sat" ,"NSE" ,"autocorr" , "diff_events_all","diff_events_periodmax", "duration_events_neg" ,"duration_events_pos" ,  "events_neg_all","events_neg_periodmax" , "events_pos_all" ,"events_pos_periodmax" , "maxpower_events_neg" ,  "maxpower_events_pos",  "maxpower_neg","maxpower_pos"  ,  "meanSST","pct_coi_periodmax" ,  "pct_ispos_signif"  , "pct_seasonal","pct_signif_periodmax" , "period_events_neg","period_events_pos" ,  "period_max"  , "trendSST", "varSST","var_seasonal")), progress="text", format="GTiff")

##mask with reefdata
reef.layer<-raster("reef_1x1.nc")
reef.layer<-rotate(reef.layer)
resample_topmodel.pred.map <- resample(topmodel.pred.map, reef.layer, method='bilinear')
topmodel.pred.map_fin<-mask(resample_topmodel.pred.map, reef.layer)
writeRaster(topmodel.pred.map_fin, filename="TopmodelPredmapMask.tif", format="GTiff", overwrite=TRUE)




