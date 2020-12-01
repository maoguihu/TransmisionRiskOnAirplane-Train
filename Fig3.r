library(ggplot2)
library(openxlsx)

LoadAR <- function(sheetName, shiftpos = FALSE, alldiff=FALSE) {
  ar.AvgA1 <<- read.xlsx("AR~Seat.xlsx", paste0("AirplaneHigh_",sheetName))
  ar.AvgA2 <<- read.xlsx("AR~Seat.xlsx", paste0("AirplaneLow_",sheetName))
  ar.AvgT1 <<- read.xlsx("AR~Seat.xlsx", paste0("TrainHigh_",sheetName))
  ar.AvgT2 <<- read.xlsx("AR~Seat.xlsx", paste0("TrainLow_",sheetName))
  
  ar.AvgA1$type <- "Airplane (high transmission setting)"
  ar.AvgA2$type <- "Airplane (low transmission setting)"
  ar.AvgT1$type <- "Train (high transmission setting)"
  ar.AvgT2$type <- "Train (low transmission setting)"
  
  ar.AvgA1 <- ar.AvgA1[ar.AvgA1[,1] <= 6, ]
  ar.AvgA2 <- ar.AvgA2[ar.AvgA2[,1] <= 6, ]
  ar.AvgT1 <- ar.AvgT1[ar.AvgT1[,1] <= 6, ]
  ar.AvgT2 <- ar.AvgT2[ar.AvgT2[,1] <= 6, ]
  
  if(shiftpos && sheetName %in% c("SeatRow","SeatCol")) {
    if (alldiff) {
      ar.AvgA1[,1] <- ar.AvgA1[,1] - 0.2
      ar.AvgA2[,1] <- ar.AvgA2[,1] - 0.1
      ar.AvgT1[,1] <- ar.AvgT1[,1] + 0.1
      ar.AvgT2[,1] <- ar.AvgT2[,1] + 0.2
    } else {
      ar.AvgA1[,1] <- ar.AvgA1[,1] - 0.1
      ar.AvgA2[,1] <- ar.AvgA2[,1] - 0.1
      ar.AvgT1[,1] <- ar.AvgT1[,1] + 0.1
      ar.AvgT2[,1] <- ar.AvgT2[,1] + 0.1 
    }
  }
  ar.Avg1 <<- rbind(ar.AvgA1, ar.AvgT1)
  ar.Avg2 <<- rbind(ar.AvgA2, ar.AvgT2)
  ar.AvgALL <<- rbind(ar.AvgA1, ar.AvgA2, ar.AvgT1, ar.AvgT2)
}

AR.Errorbar <- function(ar, stitle="", xtitle=NULL, withSmooth=T, fit.f="y~x", hideErrbar=F, barwidth=0.2, RightLegend=F) {
  if(is.null(xtitle)) xtitle <- names(ar)[1]
  
  g <- ggplot(ar, aes_string(x=names(ar)[1], y="risk", weight="close_contact", color="type"))
  if(withSmooth) g <- g + geom_smooth(formula = fit.f, method = "lm", fill = "gray80", alpha = 0.4, size = 1)
  if(!hideErrbar) g <- g + geom_errorbar(aes(ymin=CI95Lower, ymax=CI95Upper), width=barwidth)
  g <- g + geom_point(size = 2) +
    xlab(xtitle) +
    ylab("Attack rate (%)") +
    labs(title = stitle, color="Transport") +
    scale_x_continuous(breaks = unique(round(ar[,1])), minor_breaks = unique(round(ar[,1]))) +
    theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10, face="bold"),
      legend.text = element_text(size=10),
      legend.title = element_text(size=10, face="bold"),
      legend.position = ifelse(RightLegend,"right","bottom") #c(0.85, 0.15)
    )
  
  return(g)
}

## Fig3-a ----
LoadAR("SeatRow", shiftpos = TRUE, alldiff = TRUE)
p <- AR.Errorbar(ar.AvgALL, xtitle = "Number of rows apart", withSmooth = F, barwidth = 0.1, RightLegend = T) + 
  scale_y_continuous(limits = c(0,4.5)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.text=element_text(size=9), legend.title = element_text(size=9, face="bold"),
        legend.position = c(0.64, 0.8))
p

## Fig3-b ----
LoadAR("SeatCol", shiftpos = TRUE, alldiff = TRUE)
p <- AR.Errorbar(ar.AvgALL, xtitle = "Number of columns apart", withSmooth = F, barwidth = 0.1, RightLegend = T) + 
  scale_y_continuous(limits = c(0,2.8)) +
  scale_color_brewer(palette = "Set1") + 
  theme(legend.text=element_text(size=9), 
        legend.title = element_text(size=9, face="bold"),
        legend.background = element_blank(),
        legend.position = c(0.73, 0.8))
p
