################################
#### AGE BY SEX INTERACTION ####
################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Make sex an ordered variable for spline interactions
data.NMF$sex<-ordered(data.NMF$sex)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Ct_Nmf18",names(data.NMF))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + s(age,by=sex) + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

#############################
#### INTERACTION RESULTS ####
#############################

#Pull p-values
p_ageSex <- sapply(NmfModels, function(v) summary(v)$s.table[2,4])

#Convert to data frame
p_ageSex <- as.data.frame(p_ageSex)

#Print original p-values to three decimal places
p_ageSex_round <- round(p_ageSex,3)

#FDR correct p-values
p_ageSex_fdr <- p.adjust(p_ageSex[,1],method="fdr")

#Convert to data frame
p_ageSex_fdr <- as.data.frame(p_ageSex_fdr)

#To print fdr-corrected p-values to three decimal places
p_ageSex_fdr_round <- round(p_ageSex_fdr,3)

#List the NMF components that survive FDR correction
Nmf_ageSex_fdr <- row.names(p_ageSex_fdr)[p_ageSex_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_ageSex_fdr_names <- nmfComponents[as.numeric(Nmf_ageSex_fdr)]

#To check direction of coefficient estimates
ageSex_coeff <- models[as.numeric(Nmf_ageSex_fdr)]
