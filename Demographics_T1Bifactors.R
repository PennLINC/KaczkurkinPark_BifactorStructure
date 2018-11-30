###############################
#### Table 1: Demographics ####
###############################

###############################
### Load data and libraries ###
###############################

subjData <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load libraries
library(plyr)
library(varhandle)

#################################
### Total sample demographics ###
#################################

#Total sample means
meanAge_total <- mean(subjData$age)

#Total sample sd
sdAge_total <- sd(subjData$age)

#Total age range
rangeAge_total <- range(subjData$age)

#Total number of males and females (0=male, 1=female)
sexTable_total <- table(subjData$sex)

#Percentage of females
subjData$sex <- unfactor(subjData$sex)
percentFemale <- mean(subjData$sex)

#Total number of whites (0=non-white, 1=white)
whiteTable_total <- table(subjData$white)

#Percentage of White
subjData$white <- unfactor(subjData$white)
percentWhite <- mean(subjData$white)

#Maternal Education Summary table
medu1_total<-table(subjData$medu1)

#Maternal education: 12 years or less
medu1_12orLess <-length(which(subjData$medu1<=12))

#Maternal education: greater than 12 years
medu1_13andUp <- length(which(subjData$medu1>12))

#Maternal education: missing
medu1_missing <- length(which(is.na(subjData$medu)))

#Percentages for maternal education
percent12orLess <- medu1_12orLess/1394
percent13andUp <- medu1_13andUp/1394
percentMissing <- medu1_missing/1394

#########################
#### Psychopathology ####
#########################

#Typically Developing (N and percent)
Td_total <- sum(subjData$Td,na.rm=TRUE)
Td_percent <- Td_total/1394

#ADHD Diagnosis
Add_total <-sum(subjData$Add,na.rm=TRUE)
Add_percent <- Add_total/1394

#Agoraphobia Diagnosis
Agr_total <-sum(subjData$Agr,na.rm=TRUE)
Agr_percent <- Agr_total/1394

#Anorexia Diagnosis
Ano_total <-sum(subjData$Ano,na.rm=TRUE)
Ano_percent <- Ano_total/1394

#Bulimia Diagnosis
Bul_total <-sum(subjData$Bul,na.rm=TRUE)
Bul_percent <- Bul_total/1394

#Conduct Disorder Diagnosis
Con_total <-sum(subjData$Con,na.rm=TRUE)
Con_percent <- Con_total/1394

#Generalized Anxiety Disorder Diagnosis
Gad_total <-sum(subjData$Gad,na.rm=TRUE)
Gad_percent <- Gad_total/1394

#Major Depression Diagnosis
Mdd_total <-sum(subjData$Mdd,na.rm=TRUE)
Mdd_percent <- Mdd_total/1394

#Mania Diagnosis
Man_total <-sum(subjData$Man,na.rm=TRUE)
Man_percent <- Man_total/1394

#OCD Diagnosis
Ocd_total <-sum(subjData$Ocd,na.rm=TRUE)
Ocd_percent <- Ocd_total/1394

#ODD Diagnosis
Odd_total <-sum(subjData$Odd,na.rm=TRUE)
Odd_percent <- Odd_total/1394

#Panic Diagnosis
Pan_total <-sum(subjData$Pan,na.rm=TRUE)
Pan_percent <- Pan_total/1394

#Psychosis spectrum Diagnosis
Ps_total <-sum(subjData$Ps,na.rm=TRUE)
Ps_percent <- Ps_total/1394

#PTSD Diagnosis
Ptd_total <-sum(subjData$Ptd,na.rm=TRUE)
Ptd_percent <- Ptd_total/1394

#Seperation Anxiety Diagnosis
Sep_total <-sum(subjData$Sep,na.rm=TRUE)
Sep_percent <- Sep_total/1394

#Social Phobia Diagnosis
Soc_total <-sum(subjData$Soc,na.rm=TRUE)
Soc_percent <- Soc_total/1394

#Specific Phobia Diagnosis
Sph_total <-sum(subjData$Sph,na.rm=TRUE)
Sph_percent <- Sph_total/1394

#########################################
#### Percentages on psychiatric meds ####
#########################################

antiPsy <- mean(subjData$medclass_Antipsychotic)
antiCon <- mean(subjData$medclass_Anticonvulsant)
antiDep <- mean(subjData$medclass_Antidepressant)
benzo <- mean(subjData$medclass_Benzodiazepine)
stim <- mean(subjData$medclass_Stimulant)
nonStimADHD <- mean(subjData$medclass_NonstimulantADHDmed)
Lithium <- mean(subjData$medclass_Lithium)
Other <- mean(subjData$medclass_Other)

