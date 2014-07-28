# Simple descriptive statistics and visualisations of diagnostic data sent
# by Tony 2 May 2014


rm(list=ls())

source("Code/Prerequisites.R")
source("Code/Data_Management.R")




# Models 
model.a01 <- reitsma(DF_A01)
model.a02 <- reitsma(DF_A02)
model.a03 <- reitsma(DF_A03)
model.a04 <- reitsma(DF_A04)
model.a05 <- reitsma(DF_A05)
model.a06 <- reitsma(DF_A06)
model.a07 <- reitsma(DF_A07)
model.a08 <- reitsma(DF_A08)
model.a09 <- reitsma(DF_A09)
#model.a10 <- reitsma(DF_A10)



# SROC plots

Make_SROC(DF_A01, model.a01, "Outputs/Figures/SROC_A01.png", xlab_="False Positive Rate (1 - Specificity)")
Make_SROC(DF_A02, model.a02, "Outputs/Figures/SROC_A02.png", xlab_="False Positive Rate (1 - Specificity)")
Make_SROC(DF_A03, model.a03, "Outputs/Figures/SROC_A03.png", xlab_="False Positive Rate (1 - Specificity)")
Make_SROC(DF_A04, model.a04, "Outputs/Figures/SROC_A04.png", xlab_="False Positive Rate (1 - Specificity)")
Make_SROC(DF_A05, model.a05, "Outputs/Figures/SROC_A05.png", xlab_="False Positive Rate (1 - Specificity)")
Make_SROC(DF_A06, model.a06, "Outputs/Figures/SROC_A06.png", xlab_="False Positive Rate (1 - Specificity)")
Make_SROC(DF_A07, model.a07, "Outputs/Figures/SROC_A07.png", xlab_="False Positive Rate (1 - Specificity)")
Make_SROC(DF_A08, model.a08, "Outputs/Figures/SROC_A08.png", xlab_="False Positive Rate (1 - Specificity)")
Make_SROC(DF_A09, model.a09, "Outputs/Figures/SROC_A09.png", xlab_="False Positive Rate (1 - Specificity)")
#Make_SROC(DF_A10, model.a10, "Outputs/Figures/SROC_A10.png", xlab_="False Positive Rate (1 - Specificity)")


# Diagnostics

diags.all <- madad(DF_All)

diags.a01 <- madad(DF_A01)
diags.a02 <- madad(DF_A02)
diags.a03 <- madad(DF_A03)
diags.a04 <- madad(DF_A04)
diags.a05 <- madad(DF_A05) 
diags.a06 <- madad(DF_A06)
diags.a07 <- madad(DF_A07)
diags.a08 <- madad(DF_A08)
diags.a09 <- madad(DF_A09)
#diags.a10 <- madad(DF_A10)

# 
Model_Fit_DF <- data.frame(
    Analysis=c(
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9
        ),
    X_Squared_Sensitivity=c(
        diags.a01$sens.htest$statistic,
        diags.a02$sens.htest$statistic,
        diags.a03$sens.htest$statistic,
        diags.a04$sens.htest$statistic,
        diags.a05$sens.htest$statistic,
        diags.a06$sens.htest$statistic,
        diags.a07$sens.htest$statistic,
        diags.a08$sens.htest$statistic,
        diags.a09$sens.htest$statistic
#        diags.a10$sens.htest$statistic
        ),
    I_Squared_Sensitivity=c(
        Calc_I2(diags.a01$sens.htest$statistic, diags.a01$sens.htest$parameter),
        Calc_I2(diags.a02$sens.htest$statistic, diags.a02$sens.htest$parameter),
        Calc_I2(diags.a03$sens.htest$statistic, diags.a03$sens.htest$parameter),
        Calc_I2(diags.a04$sens.htest$statistic, diags.a04$sens.htest$parameter),
        Calc_I2(diags.a05$sens.htest$statistic, diags.a05$sens.htest$parameter),
        Calc_I2(diags.a06$sens.htest$statistic, diags.a06$sens.htest$parameter),
        Calc_I2(diags.a07$sens.htest$statistic, diags.a07$sens.htest$parameter),
        Calc_I2(diags.a08$sens.htest$statistic, diags.a08$sens.htest$parameter),
        Calc_I2(diags.a09$sens.htest$statistic, diags.a09$sens.htest$parameter)
        
#        Calc_I2(diags.a10$sens.htest$statistic, diags.a10$sens.htest$parameter)
        
        ),
    p_value_Sensitivity=c(
        diags.a01$sens.htest$p.value,
        diags.a02$sens.htest$p.value,
        diags.a03$sens.htest$p.value,
        diags.a04$sens.htest$p.value,
        diags.a05$sens.htest$p.value,
        diags.a06$sens.htest$p.value,
        diags.a07$sens.htest$p.value,
        diags.a08$sens.htest$p.value,
        diags.a09$sens.htest$p.value
#        diags.a10$sens.htest$p.value
        ),
    X_Squared_Specificity=c(
        diags.a01$spec.htest$statistic,
        diags.a02$spec.htest$statistic,
        diags.a03$spec.htest$statistic,
        diags.a04$spec.htest$statistic,
        diags.a05$spec.htest$statistic,
        diags.a06$spec.htest$statistic,
        diags.a07$spec.htest$statistic,
        diags.a08$spec.htest$statistic,
        diags.a09$spec.htest$statistic
        
#        diags.a10$spec.htest$statistic
        ),
    I_Squared_Specificity=c(
        Calc_I2(diags.a01$spec.htest$statistic, diags.a01$spec.htest$parameter),
        Calc_I2(diags.a02$spec.htest$statistic, diags.a02$spec.htest$parameter),
        Calc_I2(diags.a03$spec.htest$statistic, diags.a03$spec.htest$parameter),
        Calc_I2(diags.a04$spec.htest$statistic, diags.a04$spec.htest$parameter),
        Calc_I2(diags.a05$spec.htest$statistic, diags.a05$spec.htest$parameter),
        Calc_I2(diags.a06$spec.htest$statistic, diags.a06$spec.htest$parameter),
        Calc_I2(diags.a07$spec.htest$statistic, diags.a07$spec.htest$parameter),
        Calc_I2(diags.a08$spec.htest$statistic, diags.a08$spec.htest$parameter),
        Calc_I2(diags.a09$spec.htest$statistic, diags.a09$spec.htest$parameter)        
#        Calc_I2(diags.a10$spec.htest$statistic, diags.a10$spec.htest$parameter)
    ),
    p_value_Specificity=c(
        diags.a01$spec.htest$p.value,
        diags.a02$spec.htest$p.value,
        diags.a03$spec.htest$p.value,
        diags.a04$spec.htest$p.value,
        diags.a05$spec.htest$p.value,
        diags.a06$spec.htest$p.value,
        diags.a07$spec.htest$p.value,
        diags.a08$spec.htest$p.value,
        diags.a09$spec.htest$p.value
        
#        diags.a10$spec.htest$p.value
    ),
    AUC=c(
        summary(model.a01)$AUC$AUC,
        summary(model.a02)$AUC$AUC,
        summary(model.a03)$AUC$AUC,
        summary(model.a04)$AUC$AUC,
        summary(model.a05)$AUC$AUC,
        summary(model.a06)$AUC$AUC,
        summary(model.a07)$AUC$AUC,
        summary(model.a08)$AUC$AUC,        
        summary(model.a09)$AUC$AUC        
        
#        summary(model.a10)$AUC$AUC
        )
)

diags.all <- madad(DF_All)

DF_Results <- data.frame(
    Sens_mean=diags.all$sens$sens,
    Sens_lower=diags.all$sens$sens.ci[,1],
    Sens_higher=diags.all$sens$sens.ci[,2],
    Spec_mean=diags.all$spec$spec,
    Spec_lower=diags.all$spec$spec.ci[,1],
    Spec_higher=diags.all$spec$spec.ci[,2],
    FPR_mean=diags.all$fpr$fpr,
    FPR_lower=diags.all$fpr$fpr.ci[,1],
    FPR_higher=diags.all$fpr$fpr.ci[,2],
    posLR_mean=diags.all$posLR$posLR,
    posLR_lower=diags.all$posLR$posLR.ci[,1],
    posLR_higher=diags.all$posLR$posLR.ci[,2],
    negLR_mean=diags.all$negLR$negLR,
    negLR_lower=diags.all$negLR$negLR.ci[,1],
    negLR_higher=diags.all$negLR$negLR.ci[,2],
    DOR_mean=diags.all$DOR$DOR,
    DOR_lower=diags.all$DOR$DOR.ci[,1],
    DOR_higher=diags.all$DOR$DOR.ci[,2]
)

Data_Amended <- data.frame(Data_from_Excel, DF_Results)
write.xlsx(Data_Amended, file="Data/Output_Data.xlsx", sheetName="Output_Data", append=F)

write.xlsx(Model_Fit_DF, file="Data/Output_Data.xlsx", sheetName="Model_Summaries", append=T)


## Forest Plots
# Note : each of these needs to be saved
# manually after being created
BivarForest(
    DF=DF_A01,
    diags=diags.a01,
    model=model.a01
)

BivarForest(
    DF=DF_A02,
    diags=diags.a02,
    model=model.a02
)

BivarForest(
    DF=DF_A03,
    diags=diags.a03,
    model=model.a03
)

BivarForest(
    DF=DF_A04,
    diags=diags.a04,
    model=model.a04
)

BivarForest(
    DF=DF_A05,
    diags=diags.a05,
    model=model.a05
)

BivarForest(
    DF=DF_A06,
    diags=diags.a06,
    model=model.a06
)

BivarForest(
    DF=DF_A07,
    diags=diags.a07,
    model=model.a07
)

BivarForest(
    DF=DF_A08,
    diags=diags.a08,
    model=model.a08
)

BivarForest(
    DF=DF_A09,
    diags=diags.a09,
    model=model.a09
)

# BivarForest(
#     DF=DF_A10,
#     diags=diags.a10,
#     model=model.a10
# )


