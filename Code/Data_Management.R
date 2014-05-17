 # Data Managing file

#Data_from_Excel <- read.xlsx("Data/Summary_Data.xlsx", sheetName="Summary_Data")
Data_from_Excel <- read.xlsx("Data/Summary_Data-tdaedited-9.5.14.xlsx", sheetName="Summary_Data")

DF_All <- Data_from_Excel

DF_A01 <- Data_from_Excel[which(Data_from_Excel$A01==1),]
DF_A02 <- Data_from_Excel[which(Data_from_Excel$A02==1),]
DF_A03 <- Data_from_Excel[which(Data_from_Excel$A03==1),]
DF_A04 <- Data_from_Excel[which(Data_from_Excel$A04==1),]
DF_A05 <- Data_from_Excel[which(Data_from_Excel$A05==1),]
DF_A06 <- Data_from_Excel[which(Data_from_Excel$A06==1),]
DF_A07 <- Data_from_Excel[which(Data_from_Excel$A07==1),]
DF_A08 <- Data_from_Excel[which(Data_from_Excel$A08==1),]
DF_A09 <- Data_from_Excel[which(Data_from_Excel$A09==1),]
DF_A10 <- Data_from_Excel[which(Data_from_Excel$A10==1),]

correction <- 0.5

Diagnostics <- vector("list", 2)
names(Diagnostics) <- c("Uncorrected", "Corrected")

# For each of these sets of numbers, calculate:
# sensitivity
# specificity
# diagnostic odds ratio
attach(Data_from_Excel)
Diagnostics[["Uncorrected"]]$Sensitivity <- Calc_Sensitivity(tp=TP, tn=TN, fp=FP, fn=FN)
Diagnostics[["Uncorrected"]]$Specificity <- Calc_Specificity(tp=TP, tn=TN, fp=FP, fn=FN)
Diagnostics[["Uncorrected"]]$NPV <- Calc_NPV(tp=TP, tn=TN, fp=FP, fn=FN)
Diagnostics[["Uncorrected"]]$PPV <- Calc_PPV(tp=TP, tn=TN, fp=FP, fn=FN)

Diagnostics[["Corrected"]]$Sensitivity <- Calc_Sensitivity(
    tp=TP+correction, 
    tn=TN+correction, 
    fp=FP+correction, 
    fn=FN+correction)

Diagnostics[["Corrected"]]$Specificity <- Calc_Specificity(
    tp=TP+correction, 
    tn=TN+correction, 
    fp=FP+correction, 
    fn=FN+correction)

Diagnostics[["Corrected"]]$NPV <- Calc_NPV(
    tp=TP+correction, 
    tn=TN+correction, 
    fp=FP+correction, 
    fn=FN+correction)

Diagnostics[["Corrected"]]$PPV <- Calc_PPV(
    tp=TP+correction, 
    tn=TN+correction, 
    fp=FP+correction, 
    fn=FN+correction)


detach(Data_from_Excel)


Data_Amended <- data.frame(
    Data_from_Excel,
    uncor.Sens=Diagnostics$Uncorrected$Sensitivity,
    uncor.Spec=Diagnostics$Uncorrected$Specificity,
    uncor.PPV =Diagnostics$Uncorrected$PPV,
    uncor.NPV =Diagnostics$Uncorrected$NPV,
    cor.Sens  =Diagnostics$Corrected$Sensitivity,
    cor.Spec  =Diagnostics$Corrected$Specificity,
    cor.PPV   =Diagnostics$Corrected$PPV,
    cor.NPV   =Diagnostics$Corrected$NPV
)
