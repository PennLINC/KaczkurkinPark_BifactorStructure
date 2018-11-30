#################################################################################################################
#### SENSITIVITY ANALYSES FOR OVERALL PSYCHOPATHOLOGY AND ANXIOUS-MISERY RESULTS WITH MEDU AND NO PSYCH MEDS ####
#################################################################################################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1239_T1_subjData_NoPsychMeds.rds")

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("newJacobian_Nmf18",names(data.NMF))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + medu1 + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

######################
#### MOOD RESULTS ####
######################

#Pull p-values
p_mood <- sapply(NmfModels, function(v) summary(v)$p.table[5,4])

#Convert to data frame
p_mood <- as.data.frame(p_mood)

#Print original p-values to three decimal places
p_mood_round <- round(p_mood,3)

#FDR correct p-values
p_mood_fdr <- p.adjust(p_mood[,1],method="fdr")

#Convert to data frame
p_mood_fdr <- as.data.frame(p_mood_fdr)

#To print fdr-corrected p-values to three decimal places
p_mood_fdr_round <- round(p_mood_fdr,3)

#List the NMF components that survive FDR correction
Nmf_mood_fdr <- row.names(p_mood_fdr)[p_mood_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_mood_fdr_names <- nmfComponents[as.numeric(Nmf_mood_fdr)]

#To check direction of coefficient estimates
mood_coeff <- models[as.numeric(Nmf_mood_fdr)]

#########################################
#### OVERALL PSYCHOPATHOLOGY RESULTS ####
#########################################

#Pull p-values
p_overall <- sapply(NmfModels, function(v) summary(v)$p.table[9,4])

#Convert to data frame
p_overall <- as.data.frame(p_overall)

#Print original p-values to three decimal places
p_overall_round <- round(p_overall,3)

#FDR correct p-values
p_overall_fdr <- p.adjust(p_overall[,1],method="fdr")

#Convert to data frame
p_overall_fdr <- as.data.frame(p_overall_fdr)

#To print fdr-corrected p-values to three decimal places
p_overall_fdr_round <- round(p_overall_fdr,3)

#List the NMF components that survive FDR correction
Nmf_overall_fdr <- row.names(p_overall_fdr)[p_overall_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_overall_fdr_names <- nmfComponents[as.numeric(Nmf_overall_fdr)]

#To check direction of coefficient estimates
overall_coeff <- models[as.numeric(Nmf_overall_fdr)]

###########################
#### VALUES FOR TABLES ####
###########################

#Look at the coefficients
mood_coeffs <- sapply(NmfModels, function(v) summary(v)$p.table[5,])
overall_coeffs <- sapply(NmfModels, function(v) summary(v)$p.table[9,])


##Anxious-Misery (Mood)
#Pull B(coeff)
B_mood <- as.data.frame(mood_coeffs[1,])

#Print to two decimal places
B_mood_round <- round(B_mood,2)

#Pull SE
SE_mood <- as.data.frame(mood_coeffs[2,])

#Print to two decimal places
SE_mood_round <- round(SE_mood,2)

#Pull t-values
t_mood <- as.data.frame(mood_coeffs[3,])

#Print to two decimal places
t_mood_round <- round(t_mood,2)


##Overall Psychopathology
#Pull B(coeff)
B_overall <- as.data.frame(overall_coeffs[1,])

#Print to two decimal places
B_overall_round <- round(B_overall,2)

#Pull SE
SE_overall <- as.data.frame(overall_coeffs[2,])

#Print to two decimal places
SE_overall_round <- round(SE_overall,2)

#Pull t-values
t_overall <- as.data.frame(overall_coeffs[3,])

#Print to two decimal places
t_overall_round <- round(t_overall,2)


##Adjusted R-squared for all models
#Pull adjusted R-squared
Rsq <- sapply(NmfModels, function(v) summary(v)$r.sq)

#Convert to data frame
Rsq <- as.data.frame(Rsq)

#Print to two decimal places
Rsq_round <- round(Rsq,2)
