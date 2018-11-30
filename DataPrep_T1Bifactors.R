#################
### LOAD DATA ###
#################

##Demographic data (n=1629)
data.demo <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv", header=TRUE, na.strings="") 

##Clinical data
#Screening diagnoses (n=1601) (no missing values)
data.diag <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_psych_summary_vars_20131014.csv", header=TRUE)

#Psychosis clinical group (n=1601)
data.psychosis <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_diagnosis_dxpmr_20170509.csv", header=TRUE, na.strings="")

#Bifactors (n=1601)
data.bifactors <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_itemwise_bifactor_scores_20161219.csv", header=TRUE, na.strings="")

#Correlated traits (n=1601)
data.corrTraits <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_itemwise_corrtraits_scores_20161219.csv", header=TRUE, na.strings="")

##Cognitive data
#Summary factor scores (n=1601)
data.cogFactors <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/cnb/n1601_cnb_factor_scores_tymoore_20151006.csv", header=TRUE, na.strings="")

#z-scores for the 14 subtests (n=1601)
data.cogZscores <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/cnb/n1601_cnb_zscores_all_fr_20161215.csv", header=TRUE, na.strings="")

##Exclusion data
#Health exclusion (use the new healthExcludev2 variable) (n=1601; no missing values)
data.healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)

#T1 QA exclusion (n=1601)
data.t1QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header=TRUE, na.strings="NA")

##Brain data
#18 CT NMF components (n=1396) (no missing values)
data.CtNMF <- read.csv("/data/jux/BBL/projects/pncNmf/subjectData/n1396_Nmf18Bases_CT_bblids.csv", header=TRUE)

#18 CT components applied to the new Jacobian volume images (n=1396) (no missing values)
data.jacobian <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/cmpWeightedAverageNumBases_18_newJacobian.csv", header=FALSE)

#JLF T1 ROIs (n=1601; no missing values)
data.ct <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv", header=TRUE)
data.vol <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv", header=TRUE)

#JLF total brain volume (TBV) (n=1601; no missing values)
data.tbv <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_ctVol20170412.csv", header=TRUE)

#################
### DATA PREP ###
#################

#Transform the age variable from months to years
data.demo$age <- (data.demo$ageAtScan1)/12

#Define age squared (de-mean age)
data.demo$ageSq <- I(scale(data.demo$age, scale=FALSE, center=TRUE)^2)

#Recode male as 0 and female as 1 (0=male, 1=female)
data.demo$sex[which(data.demo$sex==1)] <- 0
data.demo$sex[which(data.demo$sex==2)] <- 1

#Make sex a factor
data.demo$sex <- as.factor(data.demo$sex)

#Define white vs nonwhite (white=1, non-white=0)
data.demo$white <- 0
data.demo$white[which(data.demo$race==1)] <- 1

#Make white a factor
data.demo$white <- as.factor(data.demo$white)

#Make an average CT variable
data.CtNMF$averageCT <- (data.CtNMF$Ct_Nmf18C1 + data.CtNMF$Ct_Nmf18C2 + data.CtNMF$Ct_Nmf18C3 + data.CtNMF$Ct_Nmf18C4 + data.CtNMF$Ct_Nmf18C5 + data.CtNMF$Ct_Nmf18C6 + data.CtNMF$Ct_Nmf18C7 + data.CtNMF$Ct_Nmf18C8 + data.CtNMF$Ct_Nmf18C9 + data.CtNMF$Ct_Nmf18C10 + data.CtNMF$Ct_Nmf18C11 + data.CtNMF$Ct_Nmf18C12 + data.CtNMF$Ct_Nmf18C13 + data.CtNMF$Ct_Nmf18C14 + data.CtNMF$Ct_Nmf18C15 + data.CtNMF$Ct_Nmf18C16 + data.CtNMF$Ct_Nmf18C17 + data.CtNMF$Ct_Nmf18C18)/18

#############################
### PREPARE JACOBIAN DATA ###
#############################

#Remove path to get scan ID only
data.jacobian[] <- lapply(data.jacobian, function(x) gsub("/cbica/projects/pncNmf/n1396_t1NMF/images/NewJacobians/", "", x))
data.jacobian[] <- lapply(data.jacobian, function(x) gsub("_logJacDet.nii.gz", "", x))

#Rename variables
colnames(data.jacobian) <- c("scanid","newJacobian_Nmf18C1","newJacobian_Nmf18C2","newJacobian_Nmf18C3","newJacobian_Nmf18C4","newJacobian_Nmf18C5","newJacobian_Nmf18C6","newJacobian_Nmf18C7","newJacobian_Nmf18C8","newJacobian_Nmf18C9","newJacobian_Nmf18C10","newJacobian_Nmf18C11","newJacobian_Nmf18C12","newJacobian_Nmf18C13","newJacobian_Nmf18C14","newJacobian_Nmf18C15","newJacobian_Nmf18C16","newJacobian_Nmf18C17","newJacobian_Nmf18C18")

#Make Jacobian variables numeric
data.jacobian <- data.frame(lapply(data.jacobian, function(x) as.numeric(as.character(x))))

################
### Z SCORES ###
################

#z-score the ct and volume measures because they are on different scales
data.CtNMF[c(3:20)] <- lapply(data.CtNMF[c(3:20)], function(x) c(scale(x)))
data.jacobian[c(2:19)] <- lapply(data.jacobian[c(2:19)], function(x) c(scale(x)))

##################
### MERGE DATA ###
##################
dataMerge1 <-merge(data.demo,data.diag, by=c("bblid","scanid"), all=TRUE) 
dataMerge2 <-merge(dataMerge1,data.psychosis, by=c("bblid","scanid"), all=TRUE) 
dataMerge3 <-merge(dataMerge2,data.bifactors, by=c("bblid","scanid"), all=TRUE)
dataMerge4 <-merge(dataMerge3,data.corrTraits, by=c("bblid","scanid"), all=TRUE)
dataMerge5 <-merge(dataMerge4,data.cogFactors, by=c("bblid","scanid"), all=TRUE)
dataMerge6 <-merge(dataMerge5,data.cogZscores, by=c("bblid","scanid"), all=TRUE)
dataMerge7 <-merge(dataMerge6,data.healthExclude, by=c("bblid","scanid"), all=TRUE)
dataMerge8 <-merge(dataMerge7,data.t1QA, by=c("bblid","scanid"), all=TRUE)
dataMerge9 <- merge(dataMerge8,data.CtNMF, by=c("bblid","scanid"), all=TRUE)
dataMerge10 <- merge(dataMerge9,data.ct, by=c("bblid","scanid"), all=TRUE)
dataMerge11 <- merge(dataMerge10,data.vol, by=c("bblid","scanid"), all=TRUE)
dataMerge12 <- merge(dataMerge11,data.tbv, by=c("bblid","scanid"), all=TRUE)
dataMerge13 <- merge(dataMerge12,data.jacobian, by="scanid", all=TRUE)

#Retain only the 1601 bblids (demographics has 1629)
data.n1601 <- dataMerge13[match(data.t1QA$bblid, dataMerge13$bblid, nomatch=0),] 

#Put bblids in ascending order
data.ordered <- data.n1601[order(data.n1601$bblid),]

#Count the number of subjects (should be 1601)
n <- nrow(data.ordered)

########################
### APPLY EXCLUSIONS ### 
########################
##Count the total number excluded for healthExcludev2=1 (1=Excludes those with medical rating 3/4, major incidental findings that distort anatomy, psychoactive medical medications)
#Included: n=1447; Excluded: n=154, but medical.exclude (n=81) + incidental.exclude (n=20) + medicalMed.exclude (n=64) = 165, so 11 people were excluded on the basis of two or more of these criteria
data.final <- data.ordered
data.final$ACROSS.INCLUDE.health <- 1
data.final$ACROSS.INCLUDE.health[data.final$healthExcludev2==1] <- 0
health.include<-sum(data.final$ACROSS.INCLUDE.health)
health.exclude<-1601-health.include

#Count the number excluded just medical rating 3/4 (GOAssess Medial History and CHOP EMR were used to define one summary rating for overall medical problems) (n=81)
data.final$ACROSS.INCLUDE.medical <- 1
data.final$ACROSS.INCLUDE.medical[data.final$medicalratingExclude==1] <- 0
medical.include<-sum(data.final$ACROSS.INCLUDE.medical)
medical.exclude<-1601-medical.include

#Count the number excluded for just major incidental findings that distort anatomy (n=20)
data.final$ACROSS.INCLUDE.incidental <- 1
data.final$ACROSS.INCLUDE.incidental[data.final$incidentalFindingExclude==1] <- 0
incidental.include<-sum(data.final$ACROSS.INCLUDE.incidental)
incidental.exclude<-1601-incidental.include

#Count the number excluded for just psychoactive medical medications (n=64)
data.final$ACROSS.INCLUDE.medicalMed <- 1
data.final$ACROSS.INCLUDE.medicalMed[data.final$psychoactiveMedMedicalv2==1] <- 0
medicalMed.include<-sum(data.final$ACROSS.INCLUDE.medicalMed)
medicalMed.exclude<-1601-medicalMed.include

#Subset the data to just those who pass healthExcludev2 (n=1447)
data.subset <-data.final[which(data.final$ACROSS.INCLUDE.health == 1), ]
n_health <- nrow(data.subset)

##Count the number excluded for failing to meet structural image quality assurance protocols
#Included: n=1396; Excluded: n=51
data.subset$ACROSS.INCLUDE.t1QA <- 1
data.subset$ACROSS.INCLUDE.t1QA[data.subset$t1Exclude==1] <- 0
t1QA.include<-sum(data.subset$ACROSS.INCLUDE.t1QA)
t1QA.exclude<-1447-t1QA.include

###Exclude those with ALL problems (health problems and problems with their t1 data) (included n=1396)
data.exclude <- data.subset[which(data.subset$healthExcludev2==0 & data.subset$t1Exclude == 0 ),]
n_health_t1 <- nrow(data.exclude)

##Count the number missing clinical data
#Included: n=1394; Excluded: n=2
data.exclude$ACROSS.INCLUDE.clinical <- 1
data.exclude$ACROSS.INCLUDE.clinical[is.na(data.exclude$overall_psychopathology_4factorv2)] <- 0
clinical.include<-sum(data.exclude$ACROSS.INCLUDE.clinical)
clinical.exclude<-1396-clinical.include

#Exclude those missing clinical data
subjData <- data.exclude[!is.na(data.exclude$overall_psychopathology_4factorv2),]

#Check that number of subjects = 1394
n_final <- nrow(subjData)

##Count the number taking psychotropic psychiatric medications
#Included: n=1239; Excluded for meds: n=155
subjData$ACROSS.INCLUDE.psychMeds <- 1
subjData$ACROSS.INCLUDE.psychMeds[subjData$psychoactiveMedPsychv2==1] <- 0
psychMeds.include<-sum(subjData$ACROSS.INCLUDE.psychMeds)
psychMeds.exclude<-1394-psychMeds.include


##################################################
### DEFINE PSYCHOPATHOLOGY SCREENING DIAGNOSES ###
##################################################

##Make variables where 1 = diagnosis

#ADHD
subjData$Add <- NA
subjData$Add[which(subjData$goassessSmryAdd==4)] <- 1

#Agoraphobia
subjData$Agr <- NA
subjData$Agr[which(subjData$goassessSmryAgr==4)] <- 1

#Anorexia
subjData$Ano <- NA
subjData$Ano[which(subjData$goassessSmryAno==4)] <- 1

#Bulimia
subjData$Bul <- NA
subjData$Bul[which(subjData$goassessSmryBul==4)] <- 1

#Conduct Disorder
subjData$Con <- NA
subjData$Con[which(subjData$goassessSmryCon==4)] <- 1

#Generalized Anxiety Disorder
subjData$Gad <- NA
subjData$Gad[which(subjData$goassessSmryGad==4)] <- 1

#Mania
subjData$Man <- NA
subjData$Man[which(subjData$goassessSmryMan==4)] <- 1

#Major Depressive Disorder
subjData$Mdd <- NA
subjData$Mdd[which(subjData$goassessSmryDep==4)] <- 1

#OCD
subjData$Ocd <- NA
subjData$Ocd[which(subjData$goassessSmryOcd==4)] <- 1

#Oppositional Defiant Disorder
subjData$Odd <- NA
subjData$Odd[which(subjData$goassessSmryOdd==4)] <- 1

#Panic Disorder
subjData$Pan <- NA
subjData$Pan[which(subjData$goassessSmryPan==4)] <- 1

#Psychosis
subjData$Ps <- NA
subjData$Ps[which(subjData$goassessDxpmr4=="4PS")] <- 1

#Posttraumatic Stress Disorder
subjData$Ptd <- NA
subjData$Ptd[which(subjData$goassessSmryPtd==4)] <- 1

#Separation Anxiety Disorder
subjData$Sep <- NA
subjData$Sep[which(subjData$goassessSmrySep==4)] <- 1

#Social Anxiety Disorder
subjData$Soc <- NA
subjData$Soc[which(subjData$goassessSmrySoc==4)] <- 1

#Specific Phobia
subjData$Sph <- NA
subjData$Sph[which(subjData$goassessSmryPhb==4)] <- 1

#Typically Developing
dxNames <- c("bblid","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf <- data.matrix(subjData[,dxNames])
subjData$totDx <- rowSums(dxDf[,2:17], na.rm=TRUE) #This is how many people have how many diagnoses: sum(subjData$totDx==0): 428, sum(subjData$totDx==1): 321, sum(subjData$totDx>=2): 647.
subjData$Td <- 0
subjData$Td[which(subjData$totDx==0)] <- 1

#####################################
#### MAKE TD THE REFERENCE GROUP ####
#####################################

subjData$Add[which(subjData$Td==1)] <- 0
subjData$Agr[which(subjData$Td==1)] <- 0
subjData$Ano[which(subjData$Td==1)] <- 0
subjData$Bul[which(subjData$Td==1)] <- 0
subjData$Con[which(subjData$Td==1)] <- 0
subjData$Gad[which(subjData$Td==1)] <- 0
subjData$Man[which(subjData$Td==1)] <- 0
subjData$Mdd[which(subjData$Td==1)] <- 0
subjData$Ocd[which(subjData$Td==1)] <- 0
subjData$Odd[which(subjData$Td==1)] <- 0
subjData$Pan[which(subjData$Td==1)] <- 0
subjData$Ps[which(subjData$Td==1)] <- 0
subjData$Ptd[which(subjData$Td==1)] <- 0
subjData$Sep[which(subjData$Td==1)] <- 0
subjData$Soc[which(subjData$Td==1)] <- 0
subjData$Sph[which(subjData$Td==1)] <- 0

#######################################################
### CREATE SUMMARY DIAGNOSTIC VARIABLES FOR FIGURES ###
#######################################################

#Anxious-misery disorders
subjData$GadMdd <- NA
subjData$GadMdd[which(subjData$Gad==1 | subjData$Mdd==1)] <- 1
subjData$GadMdd[which(subjData$Td==1)] <- 0

#Psychotic disorders
subjData$Psychosis <- subjData$Ps

#Behavioral disorders
subjData$AddConOdd <- NA
subjData$AddConOdd[which(subjData$Add==1 | subjData$Con==1 | subjData$Odd==1)] <- 1
subjData$AddConOdd[which(subjData$Td==1)] <- 0

#Fear disorders
subjData$AgrPtdSepSocSph <- NA
subjData$AgrPtdSepSocSph[which(subjData$Agr==1 | subjData$Ptd==1 | subjData$Sep==1 |subjData$Soc==1 | subjData$Sph==1)] <- 1
subjData$AgrPtdSepSocSph[which(subjData$Td==1)] <- 0

#All disorders
subjData$AllDiag <- 1
subjData$AllDiag[which(subjData$Td==1)] <- 0

#################
### SAVE DATA ###
#################

saveRDS(subjData,"/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

############################
### SENSITIVITY ANALYSES ###
############################

#Exclude those who were on psychiatric medications (excluded n=155)
data.sensitivity <- subjData[which(subjData$ACROSS.INCLUDE.psychMeds==1),]

#Total sample size after excluding those on psychiatric meds (n=1239)
n_NoPsychMeds <- nrow(data.sensitivity)

#Save sensitivity dataset
saveRDS(data.sensitivity,"/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1239_T1_subjData_NoPsychMeds.rds")

#Count the number missing medu1 from sensitivity dataset.
#Included: n=1226; Excluded: n=13
data.sensitivity$ACROSS.INCLUDE.medu <- 1
data.sensitivity$ACROSS.INCLUDE.medu[is.na(data.sensitivity$medu1)] <- 0
medu.include<-sum(data.sensitivity$ACROSS.INCLUDE.medu)
medu.exclude<-1239-medu.include
