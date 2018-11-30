################################
#### AGE BY SEX INTERACTION ####
################################

#Load data
data.JAC <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Make sex an ordered variable for spline interactions
data.JAC$sex<-ordered(data.JAC$sex)

#Get variable names
jacComponents <- names(data.JAC)[grep("newJacobian_Nmf18",names(data.JAC))]

#Run gam models
JacModels <- lapply(jacComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + s(age,by=sex) + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.JAC)
})

#Look at model summaries
models <- lapply(JacModels, summary)

#############################
#### INTERACTION RESULTS ####
#############################

#Pull p-values
p_ageSex <- sapply(JacModels, function(v) summary(v)$s.table[2,4])

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

#List the components that survive FDR correction
Jac_ageSex_fdr <- row.names(p_ageSex_fdr)[p_ageSex_fdr<0.05]

#Name of the components that survive FDR correction
Jac_ageSex_fdr_names <- jacComponents[as.numeric(Jac_ageSex_fdr)]

#To check direction of coefficient estimates
ageSex_coeff <- models[as.numeric(Jac_ageSex_fdr)]

