
#take the tables with the condition to test
WDR62 <- read.csv("../SpidleOrientation_siLA_siWDR62_R.csv")
KATNB1 <- read.csv("../spindleorientation_siLA_siKATNB1_R.csv")
HeLa <- read.csv("../SpindleOrientation_HeLa_R.csv")

#read head of table
head(WDR62)
head(KATNB1)
head(HeLa)

#split condition and cell number - WDR62
cond_char <- as.character(WDR62$condition)
WDR62$cond2 <- sapply(cond_char, function(x){strsplit(x, "_")[[1]][1]})
WDR62$cell_nb <- sapply(cond_char, function(x){strsplit(x, "_")[[1]][2]})
  WDR62[,c("condition", "cond2")]

#split condition and cell number - KATNB1
cond_char <- as.character(KATNB1$condition)
KATNB1$cond2 <- sapply(cond_char, function(x){strsplit(x, "_")[[1]][1]})
KATNB1$cell_nb <- sapply(cond_char, function(x){strsplit(x, "_")[[1]][2]})
  KATNB1[,c("condition", "cond2")]

#split condition and cell number - HeLa
cond_char <- as.character(HeLa$condition)
HeLa$cond2 <- sapply(cond_char, function(x){strsplit(x, "_")[[1]][1]})
HeLa$cell_nb <- sapply(cond_char, function(x){strsplit(x, "_")[[1]][2]})
  HeLa[,c("condition", "cond2")]  
  
###### graphs of WDR62 of density
library(ggplot2)

WDR62_Distance_density <- ggplot(data=WDR62, mapping = aes(distance, color=cond2)) + geom_density(lwd=1)+
  theme_bw() + scale_color_manual(values = c("gray", "darkorange1")) + 
  labs(color="condition") + 
  scale_x_continuous(breaks=seq(0,30,5), labels = as.character(seq(0,30,5)), limits = c(0,30))+
  scale_y_continuous(breaks=seq(0,0.40,0.05), labels = as.character(seq(0,0.40,0.05)),limits = c(0,0.40))
WDR62_Distance_density
ggsave(filename = "density_distanceWDR62.svg", plot = WDR62_Distance_density, width = 8, height = 6)

WDR62_Angle_density <- ggplot(data=WDR62, mapping = aes(angle, color=cond2)) + geom_density(lwd=1)+
  theme_bw() + scale_color_manual(values = c("gray", "darkorange1")) + 
  labs(color="condition") + 
  scale_x_continuous(breaks=seq(0,40,5), labels = as.character(seq(0,40,5)),limits = c(0,40))+ 
  scale_y_continuous(limits = c(0,0.120))
WDR62_Angle_density
ggsave(filename = "density_angleWDR62.svg", plot = WDR62_Angle_density, width = 8, height = 6)

###### graphs of KATNB1 of density
KATNB1_Distance_density <- ggplot(data=KATNB1, mapping = aes(distance, color=cond2)) + geom_density(lwd=1)+
  theme_bw() + scale_color_manual(values = c("blue", "gray")) + 
  labs(color="condition") +
  scale_x_continuous(breaks=seq(0,30,5), labels = as.character(seq(0,30,5)), limits = c(0,30))+
  scale_y_continuous(breaks=seq(0,0.40,0.05), labels = as.character(seq(0,0.40,0.05)),limits = c(0,0.40))
KATNB1_Distance_density
ggsave(filename = "density_distanceKATNB1.svg", plot = KATNB1_Distance_density, width = 8, height = 6)


KATNB1_Angle_density <- ggplot(data=KATNB1, mapping = aes(angle, color=cond2)) + geom_density(lwd=1)+
  theme_bw() + scale_color_manual(values = c("blue", "gray")) + 
  labs(color="condition") + 
  scale_x_continuous(breaks=seq(0,40,5), labels = as.character(seq(0,40,5)),limits = c(0,40))+ 
  scale_y_continuous(limits = c(0,0.120))
KATNB1_Angle_density
ggsave(filename = "density_angleKATNB1.svg", plot = KATNB1_Angle_density, width = 8, height = 6)

###### graphs of HeLa of density
library(ggplot2)

HeLa_Distance_density <- ggplot(data=HeLa, mapping = aes(distance, color=cond2)) + geom_density(lwd=1)+
  theme_bw() + scale_color_manual(values = c("gray", "darkorange1")) + 
  labs(color="condition") + 
  scale_x_continuous(breaks=seq(0,30,5), labels = as.character(seq(0,30,5)), limits = c(0,30))+
  scale_y_continuous(breaks=seq(0,0.40,0.05), labels = as.character(seq(0,0.40,0.05)),limits = c(0,0.40))
HeLa_Distance_density
ggsave(filename = "density_distanceHeLa.svg", plot = HeLa_Distance_density, width = 8, height = 6)

HeLa_Angle_density <- ggplot(data=HeLa, mapping = aes(angle, color=cond2)) + geom_density(lwd=1)+
  theme_bw() + scale_color_manual(values = c("gray", "darkorange1")) + 
  labs(color="condition") + 
  scale_x_continuous(breaks=seq(0,40,5), labels = as.character(seq(0,40,5)),limits = c(0,40))+ 
  scale_y_continuous(limits = c(0,0.120))
HeLa_Angle_density
ggsave(filename = "density_angleHeLa.svg", plot = HeLa_Angle_density, width = 8, height = 6)



#### graphs of WDR62 of distance:angle function
WDR62_DistanceAngle <-ggplot(data=WDR62, mapping = aes(distance, y=angle, color=cond2)) + 
  geom_point(alpha=0.5, size=2)+
  theme_bw() + 
  scale_color_manual(values = c("gray", "darkorange1"))+
  labs(color="condition")+
  scale_x_continuous(breaks=seq(0,30,5), labels = as.character(seq(0,30,5)),limits = c(0,30))+
  scale_y_continuous(breaks=seq(0,40,2.5), labels = as.character(seq(0,40,2.5)), limits = c(0,40))
  
WDR62_DistanceAngle
ggsave(filename="WDR62_DistanceAngle.svg", plot = WDR62_DistanceAngle, width = 8, height = 6)

#### graphs of KATNB1 of distance:angle function
KATNB1_DistanceAngle <-ggplot(data=KATNB1, mapping = aes(distance, y=angle, color=cond2)) + 
  geom_point(alpha=0.5, size=2)+
  theme_bw() + 
  scale_color_manual(values = c("blue", "gray"))+
  labs(color="condition")+
  scale_x_continuous(breaks=seq(0,30,5), labels = as.character(seq(0,30,5)),limits = c(0,30))+
  scale_y_continuous(breaks=seq(0,40,2.5), labels = as.character(seq(0,40,2.5)), limits = c(0,40))

KATNB1_DistanceAngle
ggsave(filename="KATNB1_DistanceAngle.svg", plot = KATNB1_DistanceAngle, width = 8, height = 6)


#test
#a <- strsplit(x = as.character(WDR62$condition[1:5]), split = "_")
#a[[1]][2]
# sapply(X = as.character(WDR62$condition[1:5]), FUN = function(x){strsplit(x, "_")[[1]][1]})
  
  
