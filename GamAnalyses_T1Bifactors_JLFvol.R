###################################################
#### SENSITIVITY ANALYSES WITH JLF VOLUME ROIs ####
###################################################

#Load library
library(mgcv)
library(dplyr)

#Load data
data.JLF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#The right accumbens is named "mprage_jlf_vol_R_Accumbens_Area" in the JLF dataset but "R_Accumbens" in the JLF labels csv. Remove "Area" from names.
names(data.JLF)[names(data.JLF) == 'mprage_jlf_vol_R_Accumbens_Area'] <- 'mprage_jlf_vol_R_Accumbens'
names(data.JLF)[names(data.JLF) == 'mprage_jlf_vol_L_Accumbens_Area'] <- 'mprage_jlf_vol_L_Accumbens'

#Load JLF labels and corresponding index numbers
#These are taken from https://github.com/PennBBL/jlfVisualizer/blob/master/data/jlf_lookupWithWM.csv
JLF.labels <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/jlf_lookupWithWM.csv", header=TRUE)

#Get JLF variable names
jlfAllComponents <- data.JLF[c(grep("mprage_jlf_vol",names(data.JLF)))]
jlfComponents_short <- jlfAllComponents[,-grep("Vent|Brain_Stem|Cerebell|Cerebral_White_Matter|CSF|Lobe_WM",names(jlfAllComponents))]
jlfComponents <- names(jlfComponents_short)

#Run gam models
JlfModels <- lapply(jlfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.JLF)
})

#Look at model summaries
models <- lapply(JlfModels, summary)

#######################################
#### ANXIOUS-MISERY (MOOD) RESULTS ####
#######################################

################
### P VALUES ###
################

#Pull p-values
p_mood <- sapply(JlfModels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_mood <- as.data.frame(p_mood)

#Add row names
rownames(p_mood) <- jlfComponents

#Print original p-values to three decimal places
p_mood_round <- round(p_mood,3)

#FDR correct p-values
p_mood_fdr <- p.adjust(p_mood[,1],method="fdr")

#Convert to data frame
p_mood_fdr <- as.data.frame(p_mood_fdr)

#To print fdr-corrected p-values to three decimal places
p_mood_fdr_round <- round(p_mood_fdr,3)

#Add row names
rownames(p_mood_fdr_round) <- jlfComponents

#Keep only the p-values that survive FDR correction
p_mood_fdr_round_signif <- p_mood_fdr_round[p_mood_fdr<0.05]

#Convert to data frame
p_mood_fdr_round_signif <- as.data.frame(p_mood_fdr_round_signif)

#################
### ROI NAMES ###
#################

#List the JLF components that survive FDR correction
ROIs_mood <- row.names(p_mood_fdr_round)[p_mood_fdr<0.05]

#Convert to data frame
ROIs_mood <- as.data.frame(ROIs_mood)

#Keep only region names
ROIs_mood[] <- lapply(ROIs_mood, function(x) gsub("mprage_jlf_vol_", "", x))

################
### T VALUES ###
################

#Pull t-values
t_mood <- sapply(JlfModels, function(x) summary(x)$p.table[4,3])

#Print to two decimal places and keep only significant values
t_mood_round <- round(t_mood,2)[p_mood_fdr<0.05]

#Convert to data frame
t_mood_round <- as.data.frame(t_mood_round)

######################
### COMBINE VALUES ###
######################

#Combine ROI names and t values into one dataframe
ROI_t_mood <- cbind(ROIs_mood,t_mood_round)

#Rename variables
ROI_t_mood_renamed <- rename(ROI_t_mood, t = t_mood_round, ROI_NAME = ROIs_mood)

#Merge to add index numbers that correspond to the significant ROIs
dataMood <-merge(ROI_t_mood_renamed,JLF.labels, by="ROI_NAME", all=FALSE)

#Remove the ROI names, leaving only the index numbers and t values for input into BrainNet Viewer
dataMood$ROI_NAME <- NULL

#Save as a .csv
write.table(dataMood, file="/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/JLFvol_signifROIs_mood.csv", row.names=FALSE, col.names=FALSE, sep=",")


#########################################
#### OVERALL PSYCHOPATHOLOGY RESULTS ####
#########################################

################
### P VALUES ###
################

#Pull p-values
p_overall <- sapply(JlfModels, function(v) summary(v)$p.table[8,4])

#Convert to data frame
p_overall <- as.data.frame(p_overall)

#Add row names
rownames(p_overall) <- jlfComponents

#Print original p-values to three decimal places
p_overall_round <- round(p_overall,3)

#FDR correct p-values
p_overall_fdr <- p.adjust(p_overall[,1],method="fdr")

#Convert to data frame
p_overall_fdr <- as.data.frame(p_overall_fdr)

#To print fdr-corrected p-values to three decimal places
p_overall_fdr_round <- round(p_overall_fdr,3)

#Add row names
rownames(p_overall_fdr_round) <- jlfComponents

#Keep only the p-values that survive FDR correction
p_overall_fdr_round_signif <- p_overall_fdr_round[p_overall_fdr<0.05]

#Convert to data frame
p_overall_fdr_round_signif <- as.data.frame(p_overall_fdr_round_signif)

#################
### ROI NAMES ###
#################

#List the JLF components that survive FDR correction
ROIs_overall <- row.names(p_overall_fdr_round)[p_overall_fdr<0.05]

#Convert to data frame
ROIs_overall <- as.data.frame(ROIs_overall)

#Keep only region names
ROIs_overall[] <- lapply(ROIs_overall, function(x) gsub("mprage_jlf_vol_", "", x))

################
### T VALUES ###
################

#Pull t-values
t_overall <- sapply(JlfModels, function(x) summary(x)$p.table[8,3])

#Print to two decimal places and keep only significant values
t_overall_round <- round(t_overall,2)[p_overall_fdr<0.05]

#Convert to data frame
t_overall_round <- as.data.frame(t_overall_round)

######################
### COMBINE VALUES ###
######################

#Combine ROI names and t values into one dataframe
ROI_t_overall <- cbind(ROIs_overall,t_overall_round)

#Rename variables
ROI_t_overall_renamed <- rename(ROI_t_overall, t = t_overall_round, ROI_NAME = ROIs_overall)

#Merge to add index numbers that correspond to the significant ROIs
dataOverall <-merge(ROI_t_overall_renamed,JLF.labels, by="ROI_NAME", all=FALSE)

#Remove the ROI names, leaving only the index numbers and t values for input into BrainNet Viewer
dataOverall$ROI_NAME <- NULL

#Save as a .csv
write.table(dataOverall, file="/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/JLFvol_signifROIs_overall.csv", row.names=FALSE, col.names=FALSE, sep=",")
