library(ggplot2)
library(openxlsx)

# stype: Airplane-High, Airplane-Low, Train-High, Train-Low
stype <- "Airplane-High" 

timeNum <- read.xlsx("AR~CotravelTime_Wuhan.xlsx", stype)

AR.ErrorbarForTime <- function(ar, stitle="", xtitle=NULL, barWidthV=0.2, barWidthH=0.1) {
  if(is.null(xtitle)) xtitle <- names(ar)[1]
  
  g <- ggplot(ar, aes_string(x=names(ar)[1], y="risk", weight="close_contact")) + 
    geom_errorbar(aes(ymin=CI95Lower, ymax=CI95Upper), color="#FDAF91FF", width=barWidthV, size = 0.5) + 
    geom_errorbar(aes(xmin=cotravel_time_mean-cotravel_time_sd, xmax=cotravel_time_mean+cotravel_time_sd), color="gray60", width=barWidthH, size = 0.5) # deepskyblue
  g <- g + geom_point(color="tomato1", size = 1.5) +
    xlab(xtitle) +
    ylab("Attack rate (%)") +
    labs(title = stitle) +
    scale_x_continuous(breaks = unique(round(ar[,1])), minor_breaks = unique(round(ar[,1]))) +
    theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10, face="bold"),
      legend.text = element_text(size=10),
      legend.title = element_text(size=10, face="bold"),
      legend.position = "bottom" #c(0.85, 0.15)
    )
  
  return(g)
}

if(stype %in% c("Airplane-High","Airplane-Low")) {
  p <- AR.ErrorbarForTime(timeNum, xtitle = "Co-travel time (hours)", barWidthH = 0.2) + 
    scale_x_continuous(breaks = seq(1,4,by=1), minor_breaks = NULL) + 
    scale_y_continuous(limits = c(0,3.4), minor_breaks = NULL)
}

if(stype %in% c("Train-High","Train-Low")) {
  p <- AR.ErrorbarForTime(timeNum, xtitle = "Co-travel time (hours)", barWidthV = 0.2) + 
    scale_x_continuous(breaks = seq(1,9,by=1), minor_breaks = NULL) + 
    scale_y_continuous(limits = c(0,2), minor_breaks = NULL) # max(ceiling(timeNum$CI95Upper * 10)/10)
}

p

