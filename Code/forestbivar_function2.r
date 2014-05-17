#based on forestplot function in rmeta

forestbivar<-function(studlab,TP,FP,FN,TN,sdSens,sdSpec,
                      sensPE,sensCI,specPE,specCI,
                      pooledSensPE,pooledSensCI,pooledSpecPE,pooledSpecCI,
                      blocksizescaleSens=1,blocksizescaleSpec=1,sensScale=c(0,1),specScale=c(0,1),...)
{
    
    rd<-function(x) format(round(x,2),nsmall=2)
    
    #PLOTTING FUNCTION
    byvarplotter<-function (labeltext, mean1, lower1, upper1, mean2, lower2, upper2, align = NULL, 
                            is.summary = FALSE, clip = c(-Inf, Inf), xlab = "", zero = 0, graphwidth = unit(2,"inches"), 
                            col = meta.colors(), xlog = FALSE, xticks = NULL, 
                            boxsizeSens = NULL,boxsizeSpec= NULL, SumBox=1,...) 
    {
        require("grid") || stop("`grid' package not found")
        
        drawNormalCI <- function(LL, OR, UL, size) {
            size = 0.75 * size
            clipupper <- convertX(unit(UL, "native"), "npc", valueOnly = TRUE) > 
                1
            cliplower <- convertX(unit(LL, "native"), "npc", valueOnly = TRUE) < 
                0
            box <- convertX(unit(OR, "native"), "npc", valueOnly = TRUE)
            clipbox <- box < 0 || box > 1
            if (clipupper || cliplower) {
                ends <- "both"
                lims <- unit(c(0, 1), c("npc", "npc"))
                if (!clipupper) {
                    ends <- "first"
                    lims <- unit(c(0, UL), c("npc", "native"))
                }
                if (!cliplower) {
                    ends <- "last"
                    lims <- unit(c(LL, 1), c("native", "npc"))
                }
                grid.lines(x = lims, y = 0.5, arrow = arrow(ends = ends, 
                                                            length = unit(0.05, "inches")), gp = gpar(col = col$lines))
                if (!clipbox)
                    grid.rect(x = unit(OR, "native"), width = unit(size, 
                                                                   "snpc"), height = unit(size, "snpc"), gp = gpar(fill = col$box, 
                                                                                                                   col = col$box))
            }
            else {
                grid.lines(x = unit(c(LL, UL), "native"), y = 0.5, 
                           gp = gpar(col = col$lines))
                grid.rect(x = unit(OR, "native"), width = unit(size, 
                                                               "snpc"), height = unit(size, "snpc"), gp = gpar(fill = col$box, 
                                                                                                               col = col$box))
                if ((convertX(unit(OR, "native") + unit(0.5 * size, 
                                                        "lines"), "native", valueOnly = TRUE) > UL) && 
                        (convertX(unit(OR, "native") - unit(0.5 * size, 
                                                            "lines"), "native", valueOnly = TRUE) < LL)) 
                    grid.lines(x = unit(c(LL, UL), "native"), y = 0.5, 
                               gp = gpar(col = col$lines))
            }
        }
        drawSummaryCI <- function(LL, OR, UL, size) {
            grid.polygon(x = unit(c(LL, OR, UL, OR), "native"), y = unit(0.5 + 
                                                                             c(0, 0.5 * size, 0, -0.5 * size), "npc"), gp = gpar(fill = col$summary, 
                                                                                                                                 col = col$summary))
        }
        plot.new()
        widthcolumn <- !apply(is.na(labeltext), 1, any)
        nc <- NCOL(labeltext)
        labels <- vector("list", nc)
        if (is.null(align)) 
            align <- c("l", rep("r", nc - 1))
        else align <- rep(align, length = nc)
        nr <- NROW(labeltext)
        is.summary <- rep(is.summary, length = nr)
        for (j in 1:nc) {
            labels[[j]] <- vector("list", nr)
            for (i in 1:nr) {
                if (is.na(labeltext[i, j])) 
                    next
                x <- switch(align[j], l = 0, r = 1, c = 0.5)
                just <- switch(align[j], l = "left", r = "right", 
                               c = "center")
                labels[[j]][[i]] <- textGrob(labeltext[i, j], x = x, 
                                             just = just, gp = gpar(fontface = if (is.summary[i]) 
                                                 "bold"
                                                 else "plain", col = rep(col$text, length = nr)[i]))
            }
        }
        colgap <- unit(4, "mm")
        colwidths <- unit.c(max(unit(rep(1, sum(widthcolumn)), "grobwidth", 
                                     labels[[1]][widthcolumn])), colgap)
        
        if (nc > 1) {
            for (i in 2:nc) colwidths <- unit.c(colwidths, max(unit(rep(1, 
                                                                        sum(widthcolumn)), "grobwidth", labels[[i]][widthcolumn])), 
                                                colgap)
        }
        colwidths <- unit.c(colwidths, graphwidth,unit(5,"mm"),graphwidth)
        
        pushViewport(viewport(layout = grid.layout(nr + 1, nc * 2 + 
                                                       3, widths = colwidths, heights = unit(c(rep(1, nr), 0.5),  
                                                                                             "lines"))))
        cwidth <- (upper1 - lower1)
        xrange<-c(0,1)    
        
        info <- 1/cwidth
        info <- info/max(info[!is.summary], na.rm = TRUE)
        info[is.summary] <- 1
        info1<-boxsizeSens
        info2<-boxsizeSpec
        for (j in 1:nc) {
            for (i in 1:nr) {
                if (!is.null(labels[[j]][[i]])) {
                    pushViewport(viewport(layout.pos.row = i, layout.pos.col = 2 * 
                                              j - 1))
                    grid.draw(labels[[j]][[i]])
                    popViewport()
                }
            }
        }
        pushViewport(viewport(layout.pos.col = 2 * nc + 1, xscale = sensScale))
        
        if (xlog) {
            if (is.null(xticks)) {
                ticks <- pretty(exp(xrange))
                ticks <- ticks[ticks > 0]
            }
            else {
                ticks <- xticks
            }
            if (length(ticks)) {
                if (min(lower1, na.rm = TRUE) < clip[1]) 
                    ticks <- c(exp(clip[1]), ticks)
                if (max(upper1, na.rm = TRUE) > clip[2]) 
                    ticks <- c(ticks, exp(clip[2]))
                xax <- xaxisGrob(gp = gpar(cex = 0.6, col = col$axes), 
                                 at = log(ticks), name = "xax")
                xax1 <- editGrob(xax, gPath("labels"), label = format(ticks, 
                                                                      digits = 2))
                grid.draw(xax1)
            }
        }
        else {
            if (is.null(xticks)) {
                grid.xaxis(gp = gpar(cex = 0.6, col = col$axes))
            }
            else if (length(xticks)) {
                grid.xaxis(at = xticks, gp = gpar(cex = 0.6, col = col$axes))
            }
        }
        grid.text(xlab, y = unit(-2, "lines"), gp = gpar(col = col$axes))
        popViewport()
        for (i in 1:nr) {
            if (is.na(mean1[i])) 
                next
            pushViewport(viewport(layout.pos.row = i, layout.pos.col = 2 * 
                                      nc + 1, xscale = sensScale))
            if (is.summary[i]) 
                drawSummaryCI(lower1[i], mean1[i], upper1[i], SumBox)
            else drawNormalCI(lower1[i], mean1[i], upper1[i], info1[i])
            popViewport()
        }
        
        pushViewport(viewport(layout.pos.col = 2 * nc + 3, xscale = specScale)) 
        
        if (xlog) {
            if (is.null(xticks)) {
                ticks <- pretty(exp(xrange))
                ticks <- ticks[ticks > 0]
            }
            else {
                ticks <- xticks
            }
            if (length(ticks)) {
                if (min(lower2, na.rm = TRUE) < clip[1]) 
                    ticks <- c(exp(clip[1]), ticks)
                if (max(upper2, na.rm = TRUE) > clip[2]) 
                    ticks <- c(ticks, exp(clip[2]))
                xax <- xaxisGrob(gp = gpar(cex = 0.6, col = col$axes), 
                                 at = log(ticks), name = "xax")
                xax1 <- editGrob(xax, gPath("labels"), label = format(ticks, 
                                                                      digits = 2))
                grid.draw(xax1)
            }
        }
        else {
            if (is.null(xticks)) {
                grid.xaxis(gp = gpar(cex = 0.6, col = col$axes))
            }
            else if (length(xticks)) {
                grid.xaxis(at = xticks, gp = gpar(cex = 0.6, col = col$axes))
            }
        }
        grid.text(xlab, y = unit(-2, "lines"), gp = gpar(col = col$axes))
        popViewport()
        
        for (i in 1:nr) {
            if (is.na(mean2[i])) 
                next
            pushViewport(viewport(layout.pos.row = i, layout.pos.col = 2 * 
                                      nc + 3, xscale = specScale))
            if (is.summary[i]) 
                drawSummaryCI(lower2[i], mean2[i], upper2[i], SumBox)
            else drawNormalCI(lower2[i], mean2[i], upper2[i], info2[i])
            popViewport()
        }
        popViewport()
    }
    
    meta.colors<-function (all.elements, box = "black", lines = "gray", summary = "black", 
                           zero = "lightgray", mirror = "lightblue", text = "black", 
                           axes = "black", background = NA) 
    {
        if (missing(all.elements)) {
            return(list(box = box, lines = lines, summary = summary, 
                        zero = zero, mirror = mirror, text = text, axes = axes, 
                        background = background))
        }
        if (is.null(all.elements)) 
            all.elements <- par("fg")
        return(list(box = all.elements, lines = all.elements, summary = all.elements, 
                    zero = all.elements, mirror = all.elements, text = all.elements, 
                    axes = all.elements, background = NA))
    }
    
    
    
    tabletext<-cbind(c("Study",NA,studlab,NA,"Pooled effect"),
                     c("TP",NA,TP,NA,NA),#you can change the order of these
                     c("FP",NA,FP,NA,NA),
                     c("FN",NA,FN,NA,NA),
                     c("TN",NA,TN,NA,NA),
                     c("Sensitivity",NA,
                       paste(rd(sensPE),
                             " [",rd(sensCI[,1]),
                             ", ",rd(sensCI[,2]),"]",sep=""),
                       NA,
                       paste(rd(pooledSensPE),
                             " [",rd(pooledSensCI[1]),
                             ", ",rd(pooledSensCI[2]),"]",sep="")),
                     c("Specificity",NA,
                       paste(rd(specPE),
                             " [",rd(specCI[,1]),
                             ", ",rd(specCI[,2]),"]",sep=""),
                       NA,
                       paste(rd(pooledSpecPE),
                             " [",rd(pooledSpecCI[1]),
                             ", ",rd(pooledSpecCI[2]),"]",sep=""))
    )
    
    m.sens<- c(NA,NA,sensPE,NA,pooledSensPE)
    l.sens<- c(NA,NA,sensCI[,1],NA,pooledSensCI[1])
    u.sens<- c(NA,NA,sensCI[,2],NA,pooledSensCI[2])
    
    m.spec<- c(NA,NA,specPE,NA,pooledSpecPE)
    l.spec<- c(NA,NA,specCI[,1],NA,pooledSpecCI[1])
    u.spec<- c(NA,NA,specCI[,2],NA,pooledSpecCI[2])
    
    nstud<-length(studlab)
    
    iwtSens<-1/sdSens
    iwtSpec<-1/sdSpec
    iwtSens<-iwtSens/max(iwtSens)*.8*blocksizescaleSens
    iwtSpec<-iwtSpec/max(iwtSpec)*.8*blocksizescaleSpec
    
    byvarplotter(tabletext,m.sens,l.sens,u.sens,m.spec,l.spec,u.spec,
                 zero=0,is.summary=c(TRUE,FALSE,rep(FALSE,nstud),FALSE,TRUE),
                 clip=c((0),(1)), xlog=F,xlim=c(0,1),boxsizeSens = c(NA,NA,iwtSens,NA),boxsizeSpec = c(NA,NA,iwtSpec,NA),graphwidth = unit(1.5,"inches"),
                 col=meta.colors(box="black",line="black", summary="black"),SumBox=0.6)
    
}
