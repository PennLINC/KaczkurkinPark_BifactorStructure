#################################################
#### GAM MODELS FOR EACH DIAGNOSTIC CATEGORY ####
#################################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("newJacobian_Nmf18",names(data.NMF))]

#Make the diagnostic categories into factors
data.NMF$Add <- as.factor(data.NMF$Add)
data.NMF$Agr <- as.factor(data.NMF$Agr)
data.NMF$Con <- as.factor(data.NMF$Con)
data.NMF$Gad <- as.factor(data.NMF$Gad)
data.NMF$Mdd <- as.factor(data.NMF$Mdd)
data.NMF$Ocd <- as.factor(data.NMF$Ocd)
data.NMF$Odd <- as.factor(data.NMF$Odd)
data.NMF$Ps <- as.factor(data.NMF$Ps)
data.NMF$Ptd <- as.factor(data.NMF$Ptd)
data.NMF$Sep <- as.factor(data.NMF$Sep)
data.NMF$Soc <- as.factor(data.NMF$Soc)
data.NMF$Sph <- as.factor(data.NMF$Sph)

#Divide total gray matter volume by 1000 to change the units from cubic millimeters (mm3) to cubic centimeters (cc3); 1 cc3 = 1,000 mm3
data.NMF$mprage_antsCT_vol_GrayMatter <- data.NMF$mprage_antsCT_vol_GrayMatter/1000

###########
### ADD ###
###########

#Global model
AddGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Add, method="REML", data = data.NMF)

AddGlobalSumm <- summary(AddGlobal)

#Individual components
AddModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Add, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
AddModelSumm <- lapply(AddModel, summary)

#Pull p-values
p_Add <- sapply(AddModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Add <- as.data.frame(p_Add)

#Print original p-values to three decimal places
p_Add_round <- round(p_Add,3)

#FDR correct p-values
p_Add_fdr <- p.adjust(p_Add[,1],method="fdr")

#Convert to data frame
p_Add_fdr <- as.data.frame(p_Add_fdr)

#To print fdr-corrected p-values to three decimal places
p_Add_fdr_round <- round(p_Add_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Add_fdr <- row.names(p_Add_fdr)[p_Add_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Add_fdr_names <- nmfComponents[as.numeric(Nmf_Add_fdr)]

#To check direction of coefficient estimates
Add_coeff <- AddModelSumm[as.numeric(Nmf_Add_fdr)]


###########
### AGR ###
###########

#Global model
AgrGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Agr, method="REML", data = data.NMF)

AgrGlobalSumm <- summary(AgrGlobal)

#Individual components
AgrModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Agr, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
AgrModelSumm <- lapply(AgrModel, summary)

#Pull p-values
p_Agr <- sapply(AgrModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Agr <- as.data.frame(p_Agr)

#Print original p-values to three decimal places
p_Agr_round <- round(p_Agr,3)

#FDR correct p-values
p_Agr_fdr <- p.adjust(p_Agr[,1],method="fdr")

#Convert to data frame
p_Agr_fdr <- as.data.frame(p_Agr_fdr)

#To print fdr-corrected p-values to three decimal places
p_Agr_fdr_round <- round(p_Agr_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Agr_fdr <- row.names(p_Agr_fdr)[p_Agr_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Agr_fdr_names <- nmfComponents[as.numeric(Nmf_Agr_fdr)]

#To check direction of coefficient estimates
Agr_coeff <- AgrModelSumm[as.numeric(Nmf_Agr_fdr)]


###########
### CON ###
###########

#Global model
ConGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Con, method="REML", data = data.NMF)

ConGlobalSumm <- summary(ConGlobal)

#Individual components
ConModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Con, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
ConModelSumm <- lapply(ConModel, summary)

#Pull p-values
p_Con <- sapply(ConModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Con <- as.data.frame(p_Con)

#Print original p-values to three decimal places
p_Con_round <- round(p_Con,3)

#FDR correct p-values
p_Con_fdr <- p.adjust(p_Con[,1],method="fdr")

#Convert to data frame
p_Con_fdr <- as.data.frame(p_Con_fdr)

#To print fdr-corrected p-values to three decimal places
p_Con_fdr_round <- round(p_Con_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Con_fdr <- row.names(p_Con_fdr)[p_Con_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Con_fdr_names <- nmfComponents[as.numeric(Nmf_Con_fdr)]

#To check direction of coefficient estimates
Con_coeff <- ConModelSumm[as.numeric(Nmf_Con_fdr)]


###########
### GAD ###
###########

#Global model
GadGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Gad, method="REML", data = data.NMF)

GadGlobalSumm <- summary(GadGlobal)

#Individual components
GadModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Gad, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
GadModelSumm <- lapply(GadModel, summary)

#Pull p-values
p_Gad <- sapply(GadModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Gad <- as.data.frame(p_Gad)

#Print original p-values to three decimal places
p_Gad_round <- round(p_Gad,3)

#FDR correct p-values
p_Gad_fdr <- p.adjust(p_Gad[,1],method="fdr")

#Convert to data frame
p_Gad_fdr <- as.data.frame(p_Gad_fdr)

#To print fdr-corrected p-values to three decimal places
p_Gad_fdr_round <- round(p_Gad_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Gad_fdr <- row.names(p_Gad_fdr)[p_Gad_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Gad_fdr_names <- nmfComponents[as.numeric(Nmf_Gad_fdr)]

#To check direction of coefficient estimates
Gad_coeff <- GadModelSumm[as.numeric(Nmf_Gad_fdr)]


###########
### MDD ###
###########

#Global model
MddGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Mdd, method="REML", data = data.NMF)

MddGlobalSumm <- summary(MddGlobal)

#Individual components
MddModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Mdd, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
MddModelSumm <- lapply(MddModel, summary)

#Pull p-values
p_Mdd <- sapply(MddModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Mdd <- as.data.frame(p_Mdd)

#Print original p-values to three decimal places
p_Mdd_round <- round(p_Mdd,3)

#FDR correct p-values
p_Mdd_fdr <- p.adjust(p_Mdd[,1],method="fdr")

#Convert to data frame
p_Mdd_fdr <- as.data.frame(p_Mdd_fdr)

#To print fdr-corrected p-values to three decimal places
p_Mdd_fdr_round <- round(p_Mdd_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Mdd_fdr <- row.names(p_Mdd_fdr)[p_Mdd_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Mdd_fdr_names <- nmfComponents[as.numeric(Nmf_Mdd_fdr)]

#To check direction of coefficient estimates
Mdd_coeff <- MddModelSumm[as.numeric(Nmf_Mdd_fdr)]


###########
### OCD ###
###########

#Global model
OcdGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Ocd, method="REML", data = data.NMF)

OcdGlobalSumm <- summary(OcdGlobal)

#Individual components
OcdModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Ocd, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
OcdModelSumm <- lapply(OcdModel, summary)

#Pull p-values
p_Ocd <- sapply(OcdModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Ocd <- as.data.frame(p_Ocd)

#Print original p-values to three decimal places
p_Ocd_round <- round(p_Ocd,3)

#FDR correct p-values
p_Ocd_fdr <- p.adjust(p_Ocd[,1],method="fdr")

#Convert to data frame
p_Ocd_fdr <- as.data.frame(p_Ocd_fdr)

#To print fdr-corrected p-values to three decimal places
p_Ocd_fdr_round <- round(p_Ocd_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Ocd_fdr <- row.names(p_Ocd_fdr)[p_Ocd_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Ocd_fdr_names <- nmfComponents[as.numeric(Nmf_Ocd_fdr)]

#To check direction of coefficient estimates
Ocd_coeff <- OcdModelSumm[as.numeric(Nmf_Ocd_fdr)]


###########
### ODD ###
###########

#Global model
OddGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Odd, method="REML", data = data.NMF)

OddGlobalSumm <- summary(OddGlobal)

#Individual components
OddModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Odd, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
OddModelSumm <- lapply(OddModel, summary)

#Pull p-values
p_Odd <- sapply(OddModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Odd <- as.data.frame(p_Odd)

#Print original p-values to three decimal places
p_Odd_round <- round(p_Odd,3)

#FDR correct p-values
p_Odd_fdr <- p.adjust(p_Odd[,1],method="fdr")

#Convert to data frame
p_Odd_fdr <- as.data.frame(p_Odd_fdr)

#To print fdr-corrected p-values to three decimal places
p_Odd_fdr_round <- round(p_Odd_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Odd_fdr <- row.names(p_Odd_fdr)[p_Odd_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Odd_fdr_names <- nmfComponents[as.numeric(Nmf_Odd_fdr)]

#To check direction of coefficient estimates
Odd_coeff <- OddModelSumm[as.numeric(Nmf_Odd_fdr)]


##########
### Ps ###
##########

#Global model
PsGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Ps, method="REML", data = data.NMF)

PsGlobalSumm <- summary(PsGlobal)

#Individual components
PsModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Ps, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
PsModelSumm <- lapply(PsModel, summary)

#Pull p-values
p_Ps <- sapply(PsModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Ps <- as.data.frame(p_Ps)

#Print original p-values to three decimal places
p_Ps_round <- round(p_Ps,3)

#FDR correct p-values
p_Ps_fdr <- p.adjust(p_Ps[,1],method="fdr")

#Convert to data frame
p_Ps_fdr <- as.data.frame(p_Ps_fdr)

#To print fdr-corrected p-values to three decimal places
p_Ps_fdr_round <- round(p_Ps_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Ps_fdr <- row.names(p_Ps_fdr)[p_Ps_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Ps_fdr_names <- nmfComponents[as.numeric(Nmf_Ps_fdr)]

#To check direction of coefficient estimates
Ps_coeff <- PsModelSumm[as.numeric(Nmf_Ps_fdr)]


###########
### PTD ###
###########

#Global model
PtdGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Ptd, method="REML", data = data.NMF)

PtdGlobalSumm <- summary(PtdGlobal)

#Individual components
PtdModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Ptd, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
PtdModelSumm <- lapply(PtdModel, summary)

#Pull p-values
p_Ptd <- sapply(PtdModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Ptd <- as.data.frame(p_Ptd)

#Print original p-values to three decimal places
p_Ptd_round <- round(p_Ptd,3)

#FDR correct p-values
p_Ptd_fdr <- p.adjust(p_Ptd[,1],method="fdr")

#Convert to data frame
p_Ptd_fdr <- as.data.frame(p_Ptd_fdr)

#To print fdr-corrected p-values to three decimal places
p_Ptd_fdr_round <- round(p_Ptd_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Ptd_fdr <- row.names(p_Ptd_fdr)[p_Ptd_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Ptd_fdr_names <- nmfComponents[as.numeric(Nmf_Ptd_fdr)]

#To check direction of coefficient estimates
Ptd_coeff <- PtdModelSumm[as.numeric(Nmf_Ptd_fdr)]


###########
### SEP ###
###########

#Global model
SepGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Sep, method="REML", data = data.NMF)

SepGlobalSumm <- summary(SepGlobal)

#Individual components
SepModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Sep, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
SepModelSumm <- lapply(SepModel, summary)

#Pull p-values
p_Sep <- sapply(SepModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Sep <- as.data.frame(p_Sep)

#Print original p-values to three decimal places
p_Sep_round <- round(p_Sep,3)

#FDR correct p-values
p_Sep_fdr <- p.adjust(p_Sep[,1],method="fdr")

#Convert to data frame
p_Sep_fdr <- as.data.frame(p_Sep_fdr)

#To print fdr-corrected p-values to three decimal places
p_Sep_fdr_round <- round(p_Sep_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Sep_fdr <- row.names(p_Sep_fdr)[p_Sep_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Sep_fdr_names <- nmfComponents[as.numeric(Nmf_Sep_fdr)]

#To check direction of coefficient estimates
Sep_coeff <- SepModelSumm[as.numeric(Nmf_Sep_fdr)]


###########
### SOC ###
###########

#Global model
SocGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Soc, method="REML", data = data.NMF)

SocGlobalSumm <- summary(SocGlobal)

#Individual components
SocModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Soc, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
SocModelSumm <- lapply(SocModel, summary)

#Pull p-values
p_Soc <- sapply(SocModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Soc <- as.data.frame(p_Soc)

#Print original p-values to three decimal places
p_Soc_round <- round(p_Soc,3)

#FDR correct p-values
p_Soc_fdr <- p.adjust(p_Soc[,1],method="fdr")

#Convert to data frame
p_Soc_fdr <- as.data.frame(p_Soc_fdr)

#To print fdr-corrected p-values to three decimal places
p_Soc_fdr_round <- round(p_Soc_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Soc_fdr <- row.names(p_Soc_fdr)[p_Soc_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Soc_fdr_names <- nmfComponents[as.numeric(Nmf_Soc_fdr)]

#To check direction of coefficient estimates
Soc_coeff <- SocModelSumm[as.numeric(Nmf_Soc_fdr)]


###########
### SPH ###
###########

#Global model
SphGlobal <- gam(mprage_antsCT_vol_GrayMatter ~ s(age) + sex + averageManualRating + Sph, method="REML", data = data.NMF)

SphGlobalSumm <- summary(SphGlobal)

#Individual components
SphModel <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + Sph, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
SphModelSumm <- lapply(SphModel, summary)

#Pull p-values
p_Sph <- sapply(SphModel, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_Sph <- as.data.frame(p_Sph)

#Print original p-values to three decimal places
p_Sph_round <- round(p_Sph,3)

#FDR correct p-values
p_Sph_fdr <- p.adjust(p_Sph[,1],method="fdr")

#Convert to data frame
p_Sph_fdr <- as.data.frame(p_Sph_fdr)

#To print fdr-corrected p-values to three decimal places
p_Sph_fdr_round <- round(p_Sph_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Sph_fdr <- row.names(p_Sph_fdr)[p_Sph_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_Sph_fdr_names <- nmfComponents[as.numeric(Nmf_Sph_fdr)]

#To check direction of coefficient estimates
Sph_coeff <- SphModelSumm[as.numeric(Nmf_Sph_fdr)]
