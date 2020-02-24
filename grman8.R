library(Rmisc)
library(readr)
library(tidyverse)
grman8 <- read_csv("U:/Green Manure ghse exp/trial 8 AprMayJun 19/grman8-3.csv", 
                   na = "NA")
#grman8$FvInoc <- factor(grman8$FvInoc, levels=c("none","sp500","sp2000"))
grman8$GrMan <- factor(grman8$GrMan, levels=c("control","oats","alfalfa","rye"))
str(grman8)
table(grman8$FvInoc)
grman8$FvInoc[grman8$FvInoc=="sp500"] <- "500sp/g.soil"
grman8$FvInoc[grman8$FvInoc=="sp2000"] <- "2000sp/g.soil"
grman8$FvInoc[grman8$FvInoc=="none"] <- "non-inoc"

grman8$FvInoc <- factor(grman8$FvInoc, levels=c("non-inoc","500sp/g.soil","2000sp/g.soil"))

grman8 <- grman8 %>%
  mutate(HC=HLcir/90*HLlen, 
         unif_l=((unif_l1+unif_l2)/2))
grman8$HC

grman8v1 <- subset(grman8, sampling=="v1")
grman8v3 <- subset(grman8, sampling=="v3")

grman8inoc <- subset(grman8, FvInoc=="500sp/g.soil"|FvInoc=="2000sp/g.soil")


#Root rot02

lbldefault <- c("1","2","3","4","5","6","7","8","9","10","11","12",
                "13","14","15","16","17","18","19","20","21","22","23","24")

lblrrot02 <- c("c","c","c","c","a","bc","c","c","a","c","b","b",
               "c","bc","bc","bc","a","bc","bc","bc","a","bc","b","b")
lblrrot02inoc <- c("a","bc","c","c","a","c","b","b",
               "a","bc","bc","bc","a","bc","b","b")
lblrrot02inocv3 <- c("a","bc","bc","bc","a","bc","b","b")

# lblrrot02 <- c("c","c","a","a","a","a","c","bc","b","bc","c","bc",
#                "c","bc","bc","bc","b","b","c","bc","c","bc","b","b")

grman8.rrot02.summ <- summarySE(grman8, measurevar="Rrot02", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
grman8.rrot02.summ.inoc <- subset(grman8.rrot02.summ, FvInoc != "non-inoc")
grman8.rrot02.summ.inoc

grman8.rrot02.summ.inoc2 <- summarySE(subset(grman8, FvInoc != "non-inoc"), measurevar="Rrot02", groupvars=c("GrMan", "sampling"), na.rm=TRUE)
grman8.rrot02.summ.inoc2
grman8.rrot02.summ.inoc2 <- grman8.rrot02.summ.inoc2 %>% arrange(sampling)


#with jitter distribution data points
#edited 9/15/19 removed the non inoc bars
ggplot(grman8.rrot02.summ.inoc, aes(x=GrMan, y=Rrot02, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=Rrot02-se, ymax=Rrot02+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=subset(grman8, FvInoc!="non-inoc"),
              aes(x=GrMan, y=Rrot02,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Root rot % area (hypocotyl 0-2 cm)")+
  ggtitle("Root rot 0-2 cm, \nn=8", subtitle="Green manure exp 2")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=0.25, size=4, label=lblgm8v1, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")+
  scale_fill_discrete(name = "crop \namendment")+
  theme_bw()

grman8rrot.lm<- lm(Rrot02~GrMan*FvInoc + block , data=subset(grman8, FvInoc!="none" & sampling=="v3")) #just change v1 and v3 as needed
summary(aov(grman8rrot.lm))
LSD.test(grman8rrot.lm, c("GrMan","FvInoc"), console=TRUE)


lblgm8v1 <- c("a","bc","c","c","a","c","c","b", "a","b","b","b","a","b","b","b")




#spore suspensions 500 and 2000 combined
grman8.rrot02.inoc.summ <- summarySE(grman8inoc, measurevar="Rrot02", groupvars=c("GrMan", "sampling"), na.rm=TRUE)
lblrrot02inoccomb <- c("a","b","b","b","a","b","b","b")
ggplot(grman8.rrot02.inoc.summ, aes(x=GrMan, y=Rrot02, fill=GrMan)) +
  facet_grid(sampling~.)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=Rrot02-se, ymax=Rrot02+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  #geom_jitter(data=grman8, aes(x=GrMan, y=Rrot02,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Root rot % area (hypocotyl 0-2 cm)")+
  ggtitle("Root rot 0-2 cm, \ncombined spore suspension 500 and 2000 sp/g soil, \nn=16, LSD of v3=19.60", 
          subtitle="Green manure exp 2")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=-.5, size=4,aes(fontface=2), 
            label=lblrrot02inoccomb)+
  theme(text=element_text(size=11), legend.position = "")

#Root rot 2-5cm
grman8.rrot25.summ <- summarySE(grman8, measurevar="Rrot25", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
lblrrot25 <- c("d","cd","cd","cd","b","cd","bcd","bcd","a","cd","bc","bcd",
                "d","d","d","d","ab","d","cd","cd","a","bcd","abc","cd")
ggplot(grman8.rrot25.summ, aes(x=GrMan, y=Rrot25, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=Rrot25-se, ymax=Rrot25+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=Rrot25,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Root rot % area (hypocotyl 2-5 cm)")+
  ggtitle("Root rot 2-5 cm, n=8")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=-.5, size=4, label=lblrrot25, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")

#Root rot 5+
grman8.rrot5.summ <- summarySE(grman8, measurevar="Rrot5", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
lblrrot5 <- c("bcde","e","de","cde","b","bcde","bcd","bc","b","b","a","bc",
              "c","bc","c","c","a","bc","bc","bc","a","bc","b","bc")
ggplot(grman8.rrot5.summ, aes(x=GrMan, y=Rrot5, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=Rrot5-se, ymax=Rrot5+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=Rrot5,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Root rot % area (hypocotyl 5+ cm)")+
  ggtitle("Root rot 5+ cm, n=8")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=-.5, size=4, label=lblrrot5, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")


#Root rot overall
grman8.rrotall.summ <- summarySE(grman8, measurevar="Rrotall", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
lblrrotall <- c("1","2","3","4","5","6","7","8","9","10","11","12",
                "c","c","bc","c","a","bc","bc","bc","a","bc","b","bc")
ggplot(grman8.rrotall.summ, aes(x=GrMan, y=Rrotall, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=Rrotall-se, ymax=Rrotall+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=Rrotall,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Root rot % area (overall)")+
  ggtitle("Root rot overall cm, n=4")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=-.5, size=4, label=lblrrotall, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")

#Hypocotyl HC
grman8.HC.summ <- summarySE(grman8, measurevar="HC", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
lblHC <- c("c","c","c","bc","a","bc","bc","c","a","bc","b","bc",
            "c","bc","bc","bc","a","bc","bc","bc","a","bc","b","bc")
ggplot(grman8.HC.summ, aes(x=GrMan, y=HC, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=HC-se, ymax=HC+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=HC,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Hypocotyl rot circumference*legnth ")+
  ggtitle("Hypocotyl rot circumference*length, n=8")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=-.5, size=4, label=lblHC, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")

#unif_l
grman8.unifl.summ <- summarySE(grman8, measurevar="unif_l", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
ggplot(grman8.unifl.summ, aes(x=GrMan, y=unif_l, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=unif_l-se, ymax=unif_l+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=unif_l,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("unifoliate length (cm) ")+
  ggtitle("unifoliate length, n=8")+
  #geom_text(position = position_dodge(0.9),vjust = -2,hjust=-.5, size=4, label=lblrrot02, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")

#Plant weight
grman8.Pwgt.summ <- summarySE(grman8, measurevar="Pwgt", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
ggplot(grman8.Pwgt.summ, aes(x=GrMan, y=Pwgt, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=Pwgt-se, ymax=Pwgt+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=Pwgt,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Plant weight (g) ")+
  ggtitle("Plant weight, n=8", subtitle="exp 2")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=0.5, size=4, label=lblpwgt, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")+
  theme_bw()

pwgt.lm <- lm(Pwgt~GrMan + FvInoc + block, data=subset(grman8, sampling=="v1"))
lsdpwgt <- LSD.test(pwgt.lm, c("GrMan","FvInoc"), console=TRUE)
#lblpwgtv3 <- lsdpwgt[[5]]$groups
lblpwgt <- c("abc","ab","ab","a","abcd","abc","cd","bcd","d","bcd","cd","bcd",
             "a","a","a","a","c","a","bc","a","bc","ab","ab","a")
lblpwgt

#foliar SDS
grman8.SDS19p.summ <- summarySE(grman8, measurevar="SDS19p", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
ggplot(grman8.SDS19p.summ, aes(x=GrMan, y=SDS19p, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=SDS19p-se, ymax=SDS19p+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=SDS19p,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Foliar severity area (%) ")+
  ggtitle("SDS foliar severity, 19DAP, n=8")+
  #geom_text(position = position_dodge(0.9),vjust = -2,hjust=-.5, size=4, label=lblrrot02, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")

grman8.SDS29p.summ <- summarySE(grman8, measurevar="SDS29p", groupvars=c("GrMan","FvInoc", "sampling"), na.rm=TRUE)
ggplot(grman8.SDS29p.summ, aes(x=GrMan, y=SDS29p, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=SDS29p-se, ymax=SDS29p+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=SDS29p,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Foliar severity area (%) ")+
  ggtitle("SDS foliar severity, 29DAP, n=8")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=-.5, size=4, 
            label=lblrrot02, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")

grman8.SDS29p.summ.v3 <- subset(grman8.SDS29p.summ, FvInoc!="none" & sampling=="v3")
lblsds29p.gm8 <- c("a","b","ab","b","ab","b","ab","b")
ggplot(grman8.SDS29p.summ.v3, aes(x=GrMan, y=SDS29p, fill=GrMan)) +
  facet_grid(sampling~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=SDS29p-se, ymax=SDS29p+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=subset(grman8, FvInoc!="none" & sampling=="v3"), aes(x=GrMan, y=SDS29p,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Foliar severity area (%) ")+
  expand_limits(y=100)+
  ggtitle("SDS foliar severity, 29DAP, n=8", subtitle="Green manure exp 2")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=0.25, size=4, 
            label=lblsds29p.gm8, aes(fontface=2))+
  theme(text=element_text(size=14), legend.position="")+
  scale_fill_discrete(name = "crop \namendment")+
  theme_bw()

#if not done in facet grid...use color for data points
library(RColorBrewer)
library(viridis)
dotdist <- c("#FF0000", "#006600", "#0000FF")

ggplot(grman8v1.rrot02.summ, aes(x=GrMan, y=Rrot02, fill=FvInoc)) +
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=Rrot02-se, ymax=Rrot02+se), width=.2, position=position_dodge(.9))+
  scale_color_manual(values=dotdist)+
  geom_jitter(data=grman8, aes(x=GrMan, y=Rrot02, color=FvInoc, stroke=1.5), alpha=0.3)+
  #geom_text(data=grman8, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Root rot % area (hypocotyl 0-2 cm) V1")+
  #geom_text(position = position_dodge(0.9),vjust = -2.5,hjust=-.5, size=4)+
  theme(text=element_text(size=14))

grman8$FvInoc
grman8inoc$FvInoc

library(agricolae)
mod.rrot02v3<- lm(Rrot02 ~ GrMan + FvInoc , data=subset(grman8inoc, sampling=="v3"))
summary(mod.rrot02v3)
LSD.test(mod.rrot02v3,c("GrMan","FvInoc"), console=TRUE)

mod.SDS29p.grman8 <- lm(SDS29p ~ GrMan + FvInoc , data=subset(grman8inoc, sampling=="v3"))
summary(mod.SDS29p.grman8)
LSD.test(mod.SDS29p.grman8,c("GrMan","FvInoc"), console=TRUE)

#correlation Pwgt and Rrot02
mod.rrot02v3<- lm(Pwgt~Rrot02, data=subset(grman8, sampling=="v3"))
summary(mod.rrot02v3)


ggplot(subset(grman8, sampling=="v3"), aes(x=Pwgt, y=Rrot02))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  ggtitle("Root rot 0-2 cm vs Plant weight, V3 only")



# detach(package:Rmisc)
# detach(package:plyr)

#without summary SE function to show the distribution of the data points 
# grman8.Rrot02 <- grman8 %>% 
#   drop_na() %>%
#   group_by(GrMan, FvInoc) %>%
#   summarise(mean_rrot02=mean(Rrot02),
#             sd_rrot02=sd(Rrot02),
#             n_rrot02=n(),
#             se=sd_rrot02/sqrt(n_rrot02)
#             )

#errorbar dataframe diff from the bar's
  

#sig_label_rootrotpct <- c("d","d","d","d","a","a","a","b","a","a","c","a")
