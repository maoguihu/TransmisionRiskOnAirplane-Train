library(ggplot2)
library(openxlsx)

dailyContacts <- read.xlsx("TravelTimeSummary_China.xlsx", "Close contacts travel date", detectDates=TRUE)

gpTrain <- ggplot(subset(dailyContacts, Type == "Train"), aes(x=Day, y=Count)) + 
  geom_bar(fill="tomato1", stat="identity") +
  geom_segment(aes(x=as.Date("2020-1-23"),xend=as.Date("2020-1-23"),y=0,yend=4942), linetype=2, color="black", size=0.5) +
  geom_segment(aes(x=as.Date("2020-1-25"),xend=as.Date("2020-1-25"),y=0,yend=4942), linetype=2, color="blue", size=0.5) +
  geom_segment(aes(x=as.Date("2020-1-29"),xend=as.Date("2020-1-29"),y=0,yend=4942), linetype=2, color="green", size=0.5) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "10 days", limits = range(dailyContacts$Day)) +
  xlab("Travel date") +
  ylab("Close contact num.") +
  labs(title = "Train") +
  theme_light() +
  theme(
    axis.text = element_text(size=9),
    axis.text.x = element_text(angle = -30, vjust=0.2),
    axis.title = element_text(size=10,face="bold"),
    plot.title = element_text(size=10, hjust=0.5)
  )

gpAir <- ggplot(subset(dailyContacts, Type == "Airplane"), aes(x=Day, y=Count)) + 
  geom_bar(fill="tomato1", stat="identity") +
  geom_segment(aes(x=as.Date("2020-1-23"),xend=as.Date("2020-1-23"),y=0,yend=1992), linetype=2, color="black", size=0.5) +
  geom_segment(aes(x=as.Date("2020-1-25"),xend=as.Date("2020-1-25"),y=0,yend=1992), linetype=2, color="blue", size=0.5) +
  geom_segment(aes(x=as.Date("2020-1-29"),xend=as.Date("2020-1-29"),y=0,yend=1992), linetype=2, color="green", size=0.5) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "10 days", limits = range(dailyContacts$Day)) +
  xlab("Travel date") +
  ylab("Close contact num.") +
  labs(title = "Airplane") +
  theme_light() +
  theme(
    axis.text = element_text(size=9),
    axis.text.x = element_text(angle = -30, vjust=0.2),
    axis.title = element_text(size=10,face="bold"),
    plot.title = element_text(size=10, hjust=0.5)
  )

gp <- ggpubr::ggarrange(gpAir, 
                        gpTrain + theme(axis.title.y = element_blank(), plot.margin = margin(5,5,5,17,unit="pt")), 
                        nrow = 1)
gp
