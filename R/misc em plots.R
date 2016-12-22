#Misc plots for paper 0
#Maina et al. 

#load in 2649 records used in models
setwd ("/Users/emilydarling/Dropbox/1-On the go/Coral Database/GLOBAL CORAL PAPERS/Paper0 - thermal refuges/Submission/ScienceAdvances/ReefBase Bleaching Data")  
d <- read.csv("UniquePresenceAbsence_usedInModels.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)
names(d)
nrow(d)

unique(d$Bleach_lev.1)
table(d$Bleach_lev.1)

#89 countries
unique(d$Region)  
unique(d$Country)  
unique(d$Year) 

min(d$Year) 
max(d$Year) 

str(d)

#what major years? 
severe <- subset(d, Bleach_lev.1 == "HIGH")
hist(severe$Year)

table(severe$Year)



#histogram of year
ggplot(data = d, aes(x = Year)) +
  geom_histogram(binwidth = 2) + 
  theme_bw(base_size = 18) + 
  xlab("Year") +
  ylab("No. of observations") +
  scale_x_continuous(limits = c(1979,2012), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01))
ggsave("hist of 2649 observations.pdf", width = 8, height = 3)

#facet by type of observation
head(d)
label_names <- c('No Bleaching' = 'No bleaching',
                 'HIGH' = 'Severe bleaching')

ggplot(data = d, aes(x = Year)) +
  geom_histogram(binwidth = 2, aes(fill = Bleach_lev.1), colour = "black") + 
  theme_bw(base_size = 18) + 
  xlab("Year") +
  ylab("No. of observations") +
  scale_x_continuous(limits = c(1979,2012), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) + 
  facet_wrap(~Bleach_lev.1, labeller = as_labeller(label_names)) + 
  scale_fill_manual("Bleaching severity", labels = c("No bleaching", "Severe bleaching"),
                    values = c("steelblue", "white")) +
  theme(legend.position = "none")
ggsave("hist_by bleaching cat.pdf", width = 8, height = 4)
