###########################################################################
#### SENSITIVITY ANALYSES FOR FEAR RESULTS WITH MEDU AND NO PSYCH MEDS ####
###########################################################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1239_T1_subjData_NoPsychMeds.rds")

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Ct_Nmf18",names(data.NMF))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + medu1 + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

################################
#### FEAR (PHOBIAS) RESULTS ####
################################

#Pull p-values
p_fear <- sapply(NmfModels, function(v) summary(v)$p.table[8,4])

#Convert to data frame
p_fear <- as.data.frame(p_fear)

#Print original p-values to three decimal places
p_fear_round <- round(p_fear,3)

#FDR correct p-values
p_fear_fdr <- p.adjust(p_fear[,1],method="fdr")

#Convert to data frame
p_fear_fdr <- as.data.frame(p_fear_fdr)

#To print fdr-corrected p-values to three decimal places
p_fear_fdr_round <- round(p_fear_fdr,3)

#List the NMF components that survive FDR correction
Nmf_fear_fdr <- row.names(p_fear_fdr)[p_fear_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_fear_fdr_names <- nmfComponents[as.numeric(Nmf_fear_fdr)]

#To check direction of coefficient estimates
fear_coeff <- models[as.numeric(Nmf_fear_fdr)]

#################################################
#### Pulling out values for the result table ####
#################################################

#Look at coefficients for Fear
fear_coeffs <- sapply(NmfModels, function(v) summary(v)$p.table[8,])

#Pull B(coeff)
B_fear <- as.data.frame(fear_coeffs[1,])

#Print to two decimal places
B_fear_round <- round(B_fear,2)

#Pull SE
SE_fear <- as.data.frame(fear_coeffs[2,])

#Print to two decimal places
SE_fear_round <- round(SE_fear,2)

#Pull t-values
t_fear <- as.data.frame(fear_coeffs[3,])

#Print to two decimal places
t_fear_round <- round(t_fear,2)


##Adjusted R-squared for all models
#Pull adjusted R-squared
Rsq <- sapply(NmfModels, function(v) summary(v)$r.sq)

#Convert to data frame
Rsq <- as.data.frame(Rsq)

#Print to two decimal places
Rsq_round <- round(Rsq,2)
