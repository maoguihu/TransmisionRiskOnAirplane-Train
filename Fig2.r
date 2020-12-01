library(ggplot2)
library(openxlsx)
library(rayshader)

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

AvgScatter3D <- function(dAR, topPanel = TRUE) {
  pTimePoint <- ggplot(dAR[dAR$col_dist <= 6,], aes(x=col_dist, y=row_dist, fill = risk, 
                                                label=infected_case)) +
    geom_raster() +
    # geom_tile(aes(width=w, height=w)) +
    coord_equal() +
    scale_fill_gradientn(colours=RColorBrewer::brewer.pal(9,"YlOrRd")) +
    scale_x_continuous(breaks = 0:6, expand = c(0,0), position = ifelse(!topPanel,"top","bottom")) +
    scale_y_continuous(breaks = 0:3, expand = c(0,0)) +
    facet_wrap(~type, ncol=2, strip.position = ifelse(topPanel,"top","bottom")) +
    xlab("Number of columns apart") +
    ylab("Number of rows apart") +
    labs(fill = "Attack rate (%)", size = "Close contact") +
    theme(
      panel.background = element_blank(), 
      panel.grid.minor = element_blank(),
      strip.text = element_text(hjust=0.5, color = "black", size = 6, margin = margin()),
      strip.background = element_rect(fill="#EEEEEE"),
      axis.text = element_text(size=6),
      axis.title = element_text(size=8,face="bold"),
      legend.title.align = 1,
      legend.text=element_text(size=6),
      legend.title=element_text(size=8, face="bold"),
      legend.key.height = unit(0.2, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.position = "top"
    )
  
  rayshader::plot_gg(pTimePoint, multicore=TRUE, scale=100, zoom=0.67, solid=FALSE,  
                     lambert = TRUE, width=4, height=3)
}

LoadAR("Seat")
AvgScatter3D(ar.AvgALL, topPanel = F)
