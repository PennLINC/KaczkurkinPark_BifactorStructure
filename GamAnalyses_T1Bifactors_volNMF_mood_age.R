###########################################
#### ANXIOUS-MISERY (MOOD) INTERACTION ####
###########################################

#Load data
data.JAC <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Get variable names
jacComponents <- names(data.JAC)[grep("newJacobian_Nmf18",names(data.JAC))]

#Run gam models
JacModels <- lapply(jacComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2 + mood_4factorv2*age , list(i = as.name(x))), method="REML", data = data.JAC)
})

#Look at model summaries
models <- lapply(JacModels, summary)

#############################
#### INTERACTION RESULTS ####
#############################

#Pull p-values
p_age <- sapply(JacModels, function(v) summary(v)$p.table[10,4])

#Convert to data frame
p_age <- as.data.frame(p_age)

#Print original p-values to three decimal places
p_age_round <- round(p_age,3)

#FDR correct p-values
p_age_fdr <- p.adjust(p_age[,1],method="fdr")

#Convert to data frame
p_age_fdr <- as.data.frame(p_age_fdr)

#To print fdr-corrected p-values to three decimal places
p_age_fdr_round <- round(p_age_fdr,3)

#List the components that survive FDR correction
Jac_age_fdr <- row.names(p_age_fdr)[p_age_fdr<0.05]

#Name of the components that survive FDR correction
Jac_age_fdr_names <- jacComponents[as.numeric(Jac_age_fdr)]

#To check direction of coefficient estimates
age_coeff <- models[as.numeric(Jac_age_fdr)]
