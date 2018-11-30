######################################################
#### NMF AS PREDICTORS OF OVERALL_PSYCHOPATHOLOGY ####
######################################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Remove NAs (three subjects are missing cognitive data on all three cog factors)
data.NMF <- data.NMF[!is.na(data.NMF$F1_Exec_Comp_Res_Accuracy),]
n <- nrow(data.NMF)

#Load libraries
library(ggplot2)

#Volume components predicting Overall
NmfModel <- lm(overall_psychopathology_4factorv2 ~ age + sex + F1_Exec_Comp_Res_Accuracy + F2_Social_Cog_Accuracy + F3_Memory_Accuracy + 
	    	     newJacobian_Nmf18C1 + newJacobian_Nmf18C2 + newJacobian_Nmf18C3 + newJacobian_Nmf18C4 + newJacobian_Nmf18C5 + 
		     newJacobian_Nmf18C6 + newJacobian_Nmf18C7 + newJacobian_Nmf18C8 + newJacobian_Nmf18C9 + newJacobian_Nmf18C10 + 
		     newJacobian_Nmf18C11 + newJacobian_Nmf18C12 + newJacobian_Nmf18C13 + newJacobian_Nmf18C14 + newJacobian_Nmf18C15 + 
                     newJacobian_Nmf18C16 + newJacobian_Nmf18C17 + newJacobian_Nmf18C18, data = data.NMF)

NullModel <- lm(overall_psychopathology_4factorv2 ~ age + sex + F1_Exec_Comp_Res_Accuracy + F2_Social_Cog_Accuracy + F3_Memory_Accuracy, data = data.NMF)

#Look at model summaries
NmfSummary <- summary(NmfModel)
NullSummary <- summary(NullModel)

#Correlation between actual overall scores and predicted overall scores in a null model
nullCor <- cor.test(predict(NullModel), data.NMF$overall_psychopathology_4factorv2)

#Correlation between actual overall scores and predicted overall scores in a model with age, sex, cognition, and the 18 NMF networks
nmfCor <- cor.test(predict(NmfModel), data.NMF$overall_psychopathology_4factorv2)

#Compare the null model to the Nmf model with an F test
Ftest <- anova(NmfModel,NullModel)

#Plot predicted vs actual overall scores.
png(file='/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Predicted_Actual_vol_Overall.png',width=2000,height=2000,res=300)
par(mar=c(5,5,2,2), cex.axis=2, bty="l")
plot(predict(NmfModel), data.NMF$overall_psychopathology_4factorv2, ylab="Overall (z)", xlab="Predicted Overall", cex.lab=2, pch=19, col="#4775d8")
abline(a=0,b=1, col = "#325194", lwd = 4)
dev.off()

