require("RColorBrewer")
library(ggplot2)
library(stringr)

data3 <- read.csv("/Users/xiaolai/Desktop/Data/Chironomid/July 25, 2019 chironomid development data/Gnotobiotic Chironomids median 07252019.csv", header=TRUE)
sub_trt4 <- subset(data3, Treatment=="Conventional"|Treatment=="Axenic")
str(sub_trt)
ggplot(data = data3,
       aes(x=factor(Treatment, levels=c("Conventional+ENV390","ENV390","Conventional+HC-16","HC-16","Drosophila","Mosquito","Chironomid","Axenic","Conventional")), y=Day2))+
  #facet_grid(.~Stage,switch = "x")+
  geom_bar(aes(fill=factor(Stage, levels=c("Adults","Pupae","L4","L3","L2","L1"))),stat="identity",width=0.5)+
  #geom_bar(aes(fill=Condition))+
  #scale_color_brewer(palette="Set1")+
  #scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand = c(0, 0),limits = c(0,42),minor_breaks = seq(0, 42, 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 0))+
  geom_text(aes(label=factor(Stage, levels=c("L1","L2","L3","L4","Pupae","Adults"))),hjust=0, vjust=-4, angle=45, position = position_stack(vjust = 0.8),size=4)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.4),text = element_text(size=30),
        plot.margin = margin(3, 6, 3, 6),
        #axis.line=element_blank(),
        #axis.line.y=element_blank(),
        axis.text.y = element_text(color="black"),
        #axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=0, hjust=1,color="black"),
        legend.position = ("none"),
        legend.title=element_blank(),
        axis.line = element_line(size = 1),
        panel.grid.major.x = element_line(colour = "gray",size=0.5),
        panel.grid.minor.x=element_line(colour = "gray",size=0.5),
        panel.grid.major.y = element_blank())+
  labs(y="Development Time (Days)",x=NULL)+
  coord_flip()

ggsave("Axenic vs Conventional development 9treatment", plot = last_plot(), device = "pdf", path = "/Users/xiaolai/Desktop/Data/Chironomid/July 25, 2019 chironomid development data",
       scale = 1, width = 30, height = 30, units = c("cm"),
       dpi = 300)

str(data3)
data3$Treatment <- factor(data3$Treatment)
data3$Treatment <- factor(Treatment, levels=c("Conventional+ENV390","ENV390","Conventional+HC-16","HC-16","Drosophila","Mosquito","Chironomid","Axenic","Conventional"))
sub_trt5 <- subset(data3, Treatment=="Conventional+ENV390"|Treatment=="ENV390"|Treatment=="Conventional+HC-16"|Treatment=="HC-16"|Treatment=="Axenic"|Treatment=="Conventional")
ggplot(data = data3,
       aes(x=factor(Stage, levels=c("L1","L2","L3","L4","Pupae","Adults")), y=Survival,shape=Treatment,color=Treatment))+
  #facet_grid(.~Treatment)+
  geom_line(aes(group=Treatment),size=1)+
  geom_point(size=5)+
  scale_shape_manual(values = 0:9)+
  #geom_bar(aes(fill=Condition))+
  #scale_color_brewer(palette="Set1")+
  #scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.1))+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 0))+
  #geom_text(aes(label=factor(Stage, levels=c("L1","L2","L3","L4","Pupae","Adults"))),hjust=0, vjust=-4, angle=45, position = position_stack(vjust = 0.8),size=4)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.4),text = element_text(size=25),
        plot.margin = margin(3, 6, 3, 6),
        #axis.line=element_blank(),
        #axis.line.y=element_blank(),
        axis.text.y = element_text(color="black"),
        #axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=0, hjust=1,color="black"),
        #legend.position = ("none"),
        #legend.title=element_blank(),
        axis.line = element_line(size = 1),
        panel.grid.major.x = element_line(colour = "gray",size=0.5),
        panel.grid.minor.x=element_line(colour = "gray",size=0.5),
        panel.grid.major.y = element_blank())+
  labs(x="Stage",y="Survival")

ggsave("Axenic vs Conventional development survival 9Treatment", plot = last_plot(), device = "pdf", path = "/Users/xiaolai/Desktop/Data/Chironomid/July 25, 2019 chironomid development data",
       scale = 1, width = 30, height = 20, units = c("cm"),
       dpi = 300)
rm(list=ls())
