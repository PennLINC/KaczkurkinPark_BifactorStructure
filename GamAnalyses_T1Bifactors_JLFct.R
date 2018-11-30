###############################################
#### SENSITIVITY ANALYSES WITH JLF CT ROIs ####
###############################################

#Load library
library(mgcv)
library(dplyr)

#Load data
data.JLF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load JLF labels and corresponding index numbers
#These are taken from https://github.com/PennBBL/jlfVisualizer/blob/master/data/jlf_lookupWithWM.csv
JLF.labels <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/jlf_lookupWithWM.csv", header=TRUE)

#Get JLF variable names
jlfComponents <- names(data.JLF)[grep("mprage_jlf_ct",names(data.JLF))]

#Run gam models
JlfModels <- lapply(jlfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.JLF)
})

#Look at model summaries
models <- lapply(JlfModels, summary)

################################
#### FEAR (PHOBIAS) RESULTS ####
################################

################
### P VALUES ###
################

#Pull p-values
p_fear <- sapply(JlfModels, function(v) summary(v)$p.table[7,4])

#Convert to data frame
p_fear <- as.data.frame(p_fear)

#Add row names
rownames(p_fear) <- jlfComponents

#Print original p-values to three decimal places
p_fear_round <- round(p_fear,3)

#FDR correct p-values
p_fear_fdr <- p.adjust(p_fear[,1],method="fdr")

#Convert to data frame
p_fear_fdr <- as.data.frame(p_fear_fdr)

#To print fdr-corrected p-values to three decimal places
p_fear_fdr_round <- round(p_fear_fdr,3)

#Add row names
rownames(p_fear_fdr_round) <- jlfComponents

#Keep only the p-values that survive FDR correction
p_fear_fdr_round_signif <- p_fear_fdr_round[p_fear_fdr<0.05]

#Convert to data frame
p_fear_fdr_round_signif <- as.data.frame(p_fear_fdr_round_signif)

#################
### ROI NAMES ###
#################

#List the JLF components that survive FDR correction
ROIs_fear <- row.names(p_fear_fdr_round)[p_fear_fdr<0.05]

#Convert to data frame
ROIs_fear <- as.data.frame(ROIs_fear)

#Keep only region names
ROIs_fear[] <- lapply(ROIs_fear, function(x) gsub("mprage_jlf_ct_", "", x))

################
### T VALUES ###
################

#Pull t-values
t_fear <- sapply(JlfModels, function(x) summary(x)$p.table[7,3])

#Print to two decimal places and keep only significant values
t_fear_round <- round(t_fear,2)[p_fear_fdr<0.05]

#Convert to data frame
t_fear_round <- as.data.frame(t_fear_round)

######################
### COMBINE VALUES ###
######################

#Combine ROI names and t values into one dataframe
combined <- cbind(ROIs_fear,t_fear_round)

#Rename variables
combined <- rename(combined, t = t_fear_round, ROI_NAME = ROIs_fear)

#Merge to add index numbers that correspond to the significant ROIs
dataMerge <-merge(combined,JLF.labels, by="ROI_NAME", all=FALSE)

#Remove the ROI names, leaving only the index numbers and t values for input into BrainNet Viewer
dataMerge$ROI_NAME <- NULL

#Save as a .csv
write.table(dataMerge, file="/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/JLFct_signifROIs_fear.csv", row.names=FALSE, col.names=FALSE, sep=",")
