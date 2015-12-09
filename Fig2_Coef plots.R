# coefficient estimates figure 
library(ggplot2)

coeftable<-read.csv("~/Dropbox/GLOBALCORALPAPERS/Paper0 - thermal refuges/tables/coefdataplot.csv")

##category colours 
cols <- c("Intensity" = "#CC79A7","Frequency" = "#E69F00","Biogeography" = "#56B4E9", 
          "Duration" = "#009E73", "Threshold model"="#F0E442", "Warming"="#0072B2",
          "Predictability"="#D55E00")

plot ggplot(data = coeftable, aes(Variable, Estimate, colour=Category)) + 
  geom_point(aes(shape=varAIC), size = 4) + coord_flip() + 
  geom_errorbar(limits, width=0.2) + 
  theme_bw() + 
  geom_hline(yintercept=0, colour="black", linetype = "longdash") + 
  theme(legend.background = element_rect(colour ="#999999"),
        panel.border = element_rect(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0, size=13, face="bold"), 
        text = element_text(size=13),
        axis.title.y = element_blank(), 
        axis.title.x=element_text(size=14), 
        axis.text=element_text(size=13), 
        legend.position=c(0.805, 0.815), 
        legend.key = element_blank()) + 
  ylab("Standardized average coefficient") + 
  geom_text(aes(label=label),hjust=-0.18, vjust=-0.12, size=5, colour="black") + 
  scale_shape_manual(values=c(1,19), guide = FALSE) +
  scale_x_discrete (limits=c("maxpower_pos", "maxpower_neg", "maxpower_events_neg",
                             "period_events_pos", "meanSST", "period_events_neg", 
                             "events_pos_periodmax", "duration_events_neg", "DHM_retro_cnt",
                             "autocorr", "pct_ispos_signif", "diff_events_all", 
                             "trendSST", "NSE")) +
  scale_colour_manual(values = cols,name="Category")

#myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) #sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))
