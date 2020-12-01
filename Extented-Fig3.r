library(ggplot2)

m <- data.frame(Transport=c("Airplane","Airplane","Train","Train"),
                X = c(0.9,1.1,1.9,2.1),
                Scenario=c("High transmission setting","Low transmission setting",
                           "High transmission setting","Low transmission setting"),
                AR=c(0.68,0.37,0.33,0.19),CI95Lower=c(0.53,0.26,0.27,0.14),CI95Upper=c(0.87,0.52,0.40,0.24))
g <- ggplot(m, aes(x=X, y=AR, color=Scenario)) + 
  geom_errorbar(aes(ymin=CI95Lower, ymax=CI95Upper), width=0.1, size = 0.5) +
  geom_point(size = 1.5) +
  xlab("Tranport type") +
  ylab("Attack rate (%)") +
  scale_x_continuous(breaks = c(1,2), minor_breaks = c(1,2), labels = c("Airplane","Train"), limits = c(0.5,2.5)) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face="bold"),
    legend.text = element_text(size=10),
    legend.title = element_text(size=10, face="bold"),
    legend.position = "right" #c(0.85, 0.15)
  )
g
