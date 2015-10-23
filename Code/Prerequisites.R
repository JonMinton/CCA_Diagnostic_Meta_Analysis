# Prerequisites and functions file

# Prerequisites
require("xlsx")
require("MCMCpack")
require("ggplot2")
require("metamisc")
require("mada")
require("xtable")

# Mark Strong's bivariate forest plot function

source("Code/forestbivar_function2.r")

# Functions
Calc_Sensitivity <- function(tp, tn, fp, fn){
    output <- tp / ( tp + fn)
    return(output)
}

Calc_Specificity <- function(tp, tn, fp, fn){
    output <- tn / ( fp + tn)    
    return(output)
}

Calc_PPV <- function(tp, tn, fp, fn){
    output <- tp/(tp + fp)
    return(output)
}

Calc_NPV <- function(tp, tn, fp, fn){
    output <- tn/(tn + fn)
    return(output)
}

Calc_I2 <- function(Q, df){
    out <- 100* (Q-df)/Q
}

Logit <- function(x){
    out <- 1/(1 + exp(-x))
}


# Working out for bivariate forest plot 
BivarForest <- function(
    DF,
    diags,
    model
    ){
    studlab <- as.character(DF$Study_ID)
      TP <- DF$TP
      TN <- DF$TN
      FP <- DF$FP
      FN <- DF$FN
      sdsens <- (Logit(diags$sens$sens.ci[,2]) - Logit(diags$sens$sens))/2
      sdspec <- (Logit(diags$spec$spec.ci[,2]) - Logit(diags$spec$spec))/2
      sensPE <- diags$sens$sens
      specPE <- diags$spec$spec
      sensCI <- diags$sens$sens.ci
      specCI <- diags$spec$spec.ci
      pooledSensPE <- Logit(model$coefficients[1,1])
      pooledSpecPE <- 1 - Logit(model$coefficients[1,2])
      
      samples <- mvrnorm(10000, mu=model$coefficients, Sigma=model$vcov)
      pooledSensCI <- quantile(Logit(samples[,1]), c(0.025, 0.975))
      pooledSpecCI <- quantile(I(1 - Logit(samples[,2])), c(0.025, 0.975))
      
      forestbivar(
          studlab=studlab,
          TP=TP,
          FP=FP,
          FN=FN,
          TN=TN,
          sdSens=sdsens,
          sdSpec=sdspec,
          sensPE=sensPE,
          sensCI=sensCI,
          specPE=specPE,
          specCI=specCI,
          pooledSensPE=pooledSensPE,
          pooledSensCI=pooledSensCI,
          pooledSpecPE=pooledSpecPE,
          pooledSpecCI=pooledSpecCI
      )
}

# GGPLOT image function
Make_GGplot_Image <- function(
    DF,
    xvar, yvar,
    filename,
    width=600, height=480
    ){
        png(filename,
            width=width, height=height)
        p <- ggplot(DF, aes(x=xvar, y=yvar))
        p <- p + xlim(0,1) + ylim(0,1)
        p <- p + geom_point()
        p <- p + xlab("Specificity")
        p <- p + ylab("Sensitivity")
        p <- p + geom_point(
            aes(
                colour=factor(Study_ID), 
                size=(TP+TN+FP+FN)
            )
        )
        p <- p + scale_size(name="Study Size")
        p <- p + scale_colour_hue("Study")
        p <- p + coord_fixed()
        p
        dev.off()    
}


# Function for producing Summary ROC curves with extrapolation, study estimates, global
# ellipses

Make_SROC <- function(
    DF,
    model,
    filename,
    width_=15,
    height_=15,
    xlab_="False Positive Rate",
    ylab_="Sensitivity",
    units_ = "cm",
    res_ = 300
    ){
    
    #Create graphics device connection
    png(
        filename,
        width=width_,
        height=height_,
        units = units_,
        res = res_
        )
    
    # Plot SROC with extrapolation (dashed line)
    rsSROC(
        DF,
        xlim=c(0,1),
        ylim=c(0,1),
        extrapolate=T,
        lty="dashed",
        xlab=xlab_,
        ylab=ylab_
        )
    
    # Plot SROC curve for interpolated region only (solid line)
    rsSROC(
        DF,
        extrapolate=F,
        lty="solid",
        lwd=3,
        add=T
        )
    
    # Add confidence ellipses for each study individually
    ROCellipse(
        DF,
        add=T
        )
    
    # Add global (model based) confidence ellipse
    ROCellipse(
        model,
        add=T,
        pch=19,
        cex=2,
        lwd=2
        )
    
    # Add AUC
    
    auc <- AUC(model)$AUC
    auc <- round(auc, 2)
    N.studies <- dim(DF)[1]
    
    legend(
        "bottomright", 
        legend=c(
            paste("AUC:", auc) 
            )
        )
    
    dev.off()
}

# Function for producing Summary ROC curves with extrapolation, study estimates, global
# ellipses
# Changing method of calculating SROC curve from mslSROC to rsSROC
Make_SROC.old <- function(
    DF,
    model,
    filename,
    width_=15,
    height_=15,
    xlab_="False Positive Rate",
    ylab_="Sensitivity",
    res_ = 300,
    units_ = "cm"
){
    
    #Create graphics device connection
    png(
        filename,
        width=width_,
        height=height_,
        res = res_,
        units = units_
    )
    
    # Plot SROC with extrapolation (dashed line)
    mslSROC(
        DF,
        xlim=c(0,1),
        ylim=c(0,1),
        extrapolate=T,
        lty="dashed",
        xlab=xlab_,
        ylab=ylab_
    )
    
    # Plot SROC curve for interpolated region only (solid line)
    mslSROC(
        DF,
        extrapolate=F,
        lty="solid",
        lwd=3,
        add=T
    )
    
    # Add confidence ellipses for each study individually
    ROCellipse(
        DF,
        add=T
    )
    
    # Add global (model based) confidence ellipse
    ROCellipse(
        model,
        add=T,
        pch=19,
        cex=2,
        lwd=2
    )
    
    dev.off()
}


