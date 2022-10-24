library(readxl)
data <- read_excel("C:/Users/cju1004/OneDrive - University of New Hampshire/Desktop/PLFA_final_data_and_plots/PLFA_class_NMDS.xlsx")

library(vegan)
library(ggplot2)

library(ggpubr)

#install.packages("devtools")
#install.packages("remotes")

#remotes::install_github("gavinsimpson/ggvegan")

library(ggvegan)

data_1 <- data[,5:9]
data_2 <- data[,1]
data_3 <- data[,3:4]

my_data.hel <- decostand(data_1, method="hellinger")
?decostand
nmds1 <- metaMDS(my_data.hel, distance ="bray", k = 2, autotransform = F)

ordiplot(nmds1)
ordiplot(nmds1,type="t")

autoplot(nmds1)

fort <- fortify(nmds1)

summary(data_2)
adonis(data_1~`Sample`,data=data_2)



p3 <- ggplot() +
  geom_point(data = subset(fort, Score == 'sites'),
             mapping = aes(x = NMDS1, y = NMDS2, colour= data_3$`HRT`, shape = data_3$Salinity, size=4),
             alpha=1)+
  geom_segment(data = subset(fort,Score == 'species'),
               mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.015, "npc"),
                             type="closed"), colour="black", size = 0.8, lty = 2)+
  geom_text(data = subset(fort, Score == 'species'),
            mapping = aes(label = Label, x = NMDS1 * 1.1, y = NMDS2 * 1.1))+
  geom_abline(intercept=0, slope = 0, linetype ="dashed", size=0.8, colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill= "NA"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.line = element_line(colour="black"))+ 
  scale_size(guide="none")+
  guides(shape = guide_legend(override.aes = list(size = 5)))+
  scale_colour_manual(values = c("blue", "red", "orange", "purple")) +   
  scale_shape_manual(values = c(15, 17, 16, 18))+
  theme(legend.title=element_blank(),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_blank(),
      legend.position="none")


p3

