########################################
#### FEAR BY SEX BY AGE INTERACTION ####
########################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Ct_Nmf18",names(data.NMF))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2 + phobias_4factorv2*sex + phobias_4factorv2*age + sex*age + phobias_4factorv2*sex*age, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

#############################
#### INTERACTION RESULTS ####
#############################

#Pull p-values
p_sex_age <- sapply(NmfModels, function(v) summary(v)$p.table[13,4])

#Convert to data frame
p_sex_age <- as.data.frame(p_sex_age)

#Print original p-values to three decimal places
p_sex_age_round <- round(p_sex_age,3)

#FDR correct p-values
p_sex_age_fdr <- p.adjust(p_sex_age[,1],method="fdr")

#Convert to data frame
p_sex_age_fdr <- as.data.frame(p_sex_age_fdr)

#To print fdr-corrected p-values to three decimal places
p_sex_age_fdr_round <- round(p_sex_age_fdr,3)

#List the NMF components that survive FDR correction
Nmf_sex_age_fdr <- row.names(p_sex_age_fdr)[p_sex_age_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_sex_age_fdr_names <- nmfComponents[as.numeric(Nmf_sex_age_fdr)]

#To check direction of coefficient estimates
sex_age_coeff <- models[as.numeric(Nmf_sex_age_fdr)]
