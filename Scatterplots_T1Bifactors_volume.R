############################################
#### SCATTERPLOTS FOR T1 BIFACTOR STUDY ####
############################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)
library(visreg)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

#Divide total gray matter volume by 1000 to change the units from cubic millimeters (mm3) to cubic centimeters (cc3); 1 cc3 = 1,000 mm3
data.NMF$mprage_antsCT_vol_GrayMatter <- data.NMF$mprage_antsCT_vol_GrayMatter/1000

#Run gam models
NmfModel <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, method="REML", data = data.NMF)

model <- summary(NmfModel)

################################
#### PLOT OVERALL BY VOLUME ####
################################

plotdata <- visreg(NmfModel,'overall_psychopathology_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$overall_psychopathology_4factorv2,
                       y=plotdata$res$visregRes)

#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#4775d8"
lineColor<- "#325194"
p_text <- "p < 0.001"
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Overall Psych (z)", y = bquote('Total GM Volume'~(cm^3))) +
  theme(axis.title=element_text(size=30), axis.text=element_text(size=20))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_totalVol_overall.png")


#############################
#### PLOT MOOD BY VOLUME ####
#############################

plotdata <- visreg(NmfModel,'mood_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$mood_4factorv2,
                       y=plotdata$res$visregRes)

#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#f59611"
lineColor<- "#ce2d14"
p_text <- "p < 0.001"
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Anxious-Misery (z)", y = bquote('Total GM Volume'~(cm^3))) +
  theme(axis.title=element_text(size=30), axis.text=element_text(size=20))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_totalVol_mood.png")
