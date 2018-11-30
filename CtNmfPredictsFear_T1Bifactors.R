###################################
#### NMF AS PREDICTORS OF FEAR ####
###################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Remove NAs (three subjects are missing cognitive data on all three cog factors)
data.NMF <- data.NMF[!is.na(data.NMF$F1_Exec_Comp_Res_Accuracy),]
n <- nrow(data.NMF)

#Load libraries
library(ggplot2)

#CT components predicting Fear
NmfModel <- lm(phobias_4factorv2 ~ age + sex + F1_Exec_Comp_Res_Accuracy + F2_Social_Cog_Accuracy + F3_Memory_Accuracy + 
	    	     Ct_Nmf18C1 + Ct_Nmf18C2 + Ct_Nmf18C3 + Ct_Nmf18C4 + Ct_Nmf18C5 + Ct_Nmf18C6 + Ct_Nmf18C7 + Ct_Nmf18C8 + Ct_Nmf18C9 + 
                     Ct_Nmf18C10 + Ct_Nmf18C11 + Ct_Nmf18C12 + Ct_Nmf18C13 + Ct_Nmf18C14 + Ct_Nmf18C15 + 
                     Ct_Nmf18C16 + Ct_Nmf18C17 + Ct_Nmf18C18, data = data.NMF)

NullModel <- lm(phobias_4factorv2 ~ age + sex + F1_Exec_Comp_Res_Accuracy + F2_Social_Cog_Accuracy + F3_Memory_Accuracy, data = data.NMF)

#Look at model summaries
NmfSummary <- summary(NmfModel)
NullSummary <- summary(NullModel)

#Correlation between actual fear scores and predicted fear scores in a null model
nullCor <- cor.test(predict(NullModel), data.NMF$phobias_4factorv2)

#Correlation between actual fear scores and predicted fear scores in a model with age, sex, cognition, and the 18 NMF networks
nmfCor <- cor.test(predict(NmfModel), data.NMF$phobias_4factorv2)

#Compare the null model to the Nmf model with an F test
Ftest <- anova(NmfModel,NullModel)

#Plot predicted fear score vs actual fear scores.
png(file='/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Predicted_Actual_ct_Fear.png',width=2000,height=2000,res=300)
par(mar=c(5,5,2,2), cex.axis=2, bty="l")
plot(predict(NmfModel), data.NMF$phobias_4factorv2, ylab="Fear (z)", xlab="Predicted Fear", cex.lab=2, pch=19, col="#4775d8")
abline(a=0,b=1, col = "#325194", lwd = 4)
dev.off()

