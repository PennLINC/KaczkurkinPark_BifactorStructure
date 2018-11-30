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

#Get variable names
nmfComponents <- names(data.NMF)[grep("Ct_Nmf18",names(data.NMF))]

#Run gam models (GAM without TBV)
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

#########################
#### PLOT FEAR BY CT ####
#########################

#Scatterplots for select NMF components

##################################
### Temporal Parietal Junction ###
##################################
plotdata <- visreg(NmfModels[[16]],'phobias_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$phobias_4factorv2,
                       y=plotdata$res$visregRes)

##To make data points have color gradients, use colpal and colkey
##Colors are defined as follows: colpal[1]=Purples, colpal[2]=Blues, colpal[3]=Oranges, colpal[4]=Reds
#colpal <- c("Purples","Blues","Oranges","Reds")
#colkey <- brewer.pal(8,colpal[3])[c(3:8)]

##Or for solid color data points, use this command:
#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#4775d8"
lineColor<- "#325194"
p_text <- "p[fdr] < 0.001"
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Fear (z)", y = "CT of Network 16 (a.u.)") +
  theme(axis.title=element_text(size=30), axis.text=element_text(size=20))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_ctTPJ_fear.png")

#####################
### Subgenual ACC ###
#####################
plotdata <- visreg(NmfModels[[13]],'phobias_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$phobias_4factorv2,
                       y=plotdata$res$visregRes)

##To make data points have color gradients, use colpal and colkey
##Colors are defined as follows: colpal[1]=Purples, colpal[2]=Blues, colpal[3]=Oranges, colpal[4]=Reds
#colpal <- c("Purples","Blues","Oranges","Reds")
#colkey <- brewer.pal(8,colpal[3])[c(3:8)]

##Or for solid color data points, use this command:
#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#4775d8"
lineColor<- "#325194"
p_text <- "p[fdr] == 0.002"
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Fear (z)", y = "CT of Network 13 (a.u.)") +
  theme(axis.title=element_text(size=30), axis.text=element_text(size=20))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_ctSubgenACC_fear.png")
