##############################
#### PARTIAL CORRELATIONS ####
##############################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(ppcor)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("newJacobian_Nmf18",names(data.NMF))]

#Makes sex numeric (necessary for pcor)
data.NMF$sex <- as.numeric(data.NMF$sex)


#Mood correlations controlling for covariates
NmfCorrs_mood <- lapply(nmfComponents, function(z) {
  pcor.test(substitute(data.NMF$i, list(i = as.name(z))), data.NMF$mood_4factorv2, data.NMF[,c("age","ageSq","sex","averageManualRating","phobias_4factorv2","psychosis_4factorv2","externalizing_4factorv2","overall_psychopathology_4factorv2")], method = "pearson")
})

##Pull the r values
Corrs_r_mood <- sapply(NmfCorrs_mood, function(x) (x)$estimate)

#Convert to data frame
Corrs_r_mood <- as.data.frame(Corrs_r_mood)

#To print the r values to three decimal places
Corrs_r_mood_round <- round(Corrs_r_mood,3)


#Overall Psychopathology correlations controlling for covariates
NmfCorrs_overall <- lapply(nmfComponents, function(z) {
  pcor.test(substitute(data.NMF$i, list(i = as.name(z))), data.NMF$overall_psychopathology_4factorv2, data.NMF[,c("age","ageSq","sex","averageManualRating","mood_4factorv2","phobias_4factorv2","psychosis_4factorv2","externalizing_4factorv2")], method = "pearson")
})

##Pull the r values
Corrs_r_overall <- sapply(NmfCorrs_overall, function(x) (x)$estimate)

#Convert to data frame
Corrs_r_overall <- as.data.frame(Corrs_r_overall)

#To print the r values to three decimal places
Corrs_r_overall_round <- round(Corrs_r_overall,3)
