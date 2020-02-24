library(tidyverse)
getwd()
library(readr)

#load dataset ####
globgrman <- read_csv("grmanFHK.csv")

#convert green manure to factor, blk to character 
head(globgrman)
unique(globgrman$GrMan)
globgrman$GrMan <- factor(globgrman$GrMan, levels=c("none","oat","alfalfa","rye","clover","oats+clover","corn","soybean"))
globgrman$GrMan
globgrman$GrManpart[is.na(globgrman$GrManpart)] <- "mix"
globgrman$blk <- as.character(globgrman$blk)




#Root rot 0-2 cm only (Exp 6, 8, 11 or F, H, K) ####

globgrman.fhk <- globgrman %>%
  dplyr::filter(FvInoc=="Fv" & GrMan %in% c("none","oat","alfalfa","rye","clover","corn","soybean")) %>%
  dplyr::filter(FvConc=="Fv500"|FvConc=="Fv2000") %>%
  dplyr::filter(GrManform %in% c("fresh","none")) %>%
  dplyr::filter(GrManpart== "mix" ) %>%
  dplyr::filter(exp %in% c("F","H","K"))
View(globgrman.fhk)

globgrman.fhk.compact <- globgrman.fhk %>%
  select(exp, blk, glob_blk, plot, GrMan, GrManform, GrManpart, FvInoc, FvConc, GrowStg, seed_emerg, Rrot02, Swgt, Rwgt, Pwgt, DSperc)
write.csv(globgrman.fhk.compact, "grmanFHK.csv")


globgrman.fhk.summ <- globgrman.fhk %>%
  group_by(GrMan, FvInoc, FvConc, exp) %>%
  summarise(mRrot = mean(Rrot02, na.rm=T),
            sd = sd(Rrot02, na.rm=T),
            n=sum(!is.na(Rrot02)) ) %>%
  mutate(se=sd/sqrt(n))

View(globgrman.fhk.summ)

ggplot(globgrman.fhk.summ, aes(x=GrMan, y=mRrot, fill=GrMan)) +
  facet_grid(FvConc~exp )+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=mRrot-se, ymax=mRrot+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=globgrman.fhk, aes(x=GrMan, y=Rrot02,  stroke=.5), alpha=0.3)+
  geom_text(aes(label=n, y=-3),position = position_dodge(0.9),size=4, colour="black")+
  #geom_text(data=grman10, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Root rot % area")+
  ggtitle("%Root rot at 0-2cm", subtitle="exp 6,8,11, n=varies")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=0.25, size=4, label="", aes(fontface=1))+
  theme(text=element_text(size=14), legend.position="")+
  scale_fill_discrete(name = "crop \namendment")+
  theme_bw()

#root rot diff, exp as an exp unit ####
globgrman.expblka <- globgrman %>%
  dplyr::filter(FvInoc=="Fv" & GrMan %in% c("none","oat","alfalfa","rye","clover","corn","soybean")) %>%
  group_by(GrMan,exp) %>%
  summarise(mRrot = mean(Rrot, na.rm=T))

View(globgrman.expblka)

globgrman.rrot02 <- globgrman %>%
  dplyr::filter(Rrot02!="NA")

a<- spread(globgrman.expblka, GrMan, mRrot)
a
b<- a %>% dplyr::filter(!exp %in% c("C","F","I","J","L"))

b

c<- b %>% mutate(moat=none-oat, 
                 malfalfa=none-alfalfa,
                 mrye=none-rye,
                 mclover=none-clover)
c

d<- gather(c, GrMan, mean, moat, malfalfa, mrye, mclover) %>% select(-c(none,oat,alfalfa,rye,clover))
d

rrot.summ.summ <- d %>% 
  group_by(GrMan) %>%
  summarise(mRrot = mean(mean, na.rm=T),
            sd = sd(mean, na.rm=T),
            n=sum(!is.na(mean)) ) %>%
  mutate(se=sd/sqrt(n))

d
rrot.summ.summ

ggplot(rrot.summ.summ, aes(x=GrMan, y=mRrot, fill=GrMan)) +
  facet_grid(.~.)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=mRrot-se, ymax=mRrot+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=d, aes(x=GrMan, y=mean,  stroke=.5), alpha=0.3)+
  #geom_text(data=grman10, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Root rot % area")+
  ggtitle("%Root rot suppression against no-amendment\n
          exp 1,2,4,5,8, n=5(x5)")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=0.25, size=4, label="", aes(fontface=1))+
  theme(text=element_text(size=14), legend.position="")+
  scale_fill_discrete(name = "crop \namendment")+
  theme_bw()



#seed emergence, compare inoc vs non-inoc, green manure #### 
#mapping
globgrman
table(globgrman$seed_emerg)
globgrman$seed_emerg[globgrman$seed_emerg==3]
globgrman$seed_emerg

table(globgrman$FvConc)
globgrman$FvConc[globgrman$FvConc==0] <- "non-inoc"

unique(globgrman$GrManpart)
globgrman$GrManpart[is.na(globgrman$GrManpart)] <- "mix"
globgrman

k<- subset(globgrman, exp=="K")
View(k)


#for seed emergence, we use numbers from expts a, b, e, f, h, k
globgrman.abefhk<- globgrman %>%
  dplyr::filter(exp %in% c("A","B","E","F","H","K")) %>%
  dplyr::filter(GrMan %in% c("none","oat","alfalfa","rye","clover","corn","soybean")) %>%
  dplyr::filter(GrManform %in% c("fresh","none")) %>%
  dplyr::filter(FvConc %in% c("non-inoc","Fv500","Fv525","Fv1000","Fv2000")) %>%
  dplyr::filter(GrManpart == "mix") %>%
  dplyr::filter(seed_emerg != "NA")
globgrman.abefhk  
table(globgrman.abefhk$exp)
table(globgrman.abefhk$FvInoc)
globgrman.abefhk$seed_emerg

names(globgrman.abefhk)

#get the seed emergence rate
globgrman.abefhk.seed <- globgrman.abefhk %>%
  group_by(exp, GrMan, FvInoc) %>%
  summarise(germd = sum(seed_emerg),
            nseed=n()*2) %>%
  mutate(germrate=germd/nseed)
View(globgrman.abefhk.seed)

#summary mean of the seed emergence
globgrman.abefhk.seed.summ <- globgrman.abefhk.seed %>%
  group_by(GrMan, FvInoc) %>%
  summarise(mgermrate= mean(germrate, na.rm=T),
            sumseed= sum(germd, na.rm=T),
            sd= sd(germrate, na.rm=T),
            n=sum(!is.na(germrate))) %>%
  mutate(se=sd/sqrt(n))

View(globgrman.abefhk.seed.summ)

ggplot(globgrman.abefhk.seed.summ, aes(x=GrMan, y=mgermrate, fill=GrMan)) +
  facet_grid(.~FvInoc)+
  geom_bar(position="dodge", stat="identity", color="#000000")+
  geom_errorbar(aes(ymin=mgermrate-se, ymax=mgermrate+se), width=.2, position=position_dodge(.9))+
  #scale_color_manual(values=dotdist)+
  geom_jitter(data=globgrman.abefhk.seed, aes(x=GrMan, y=germrate,  stroke=.5), alpha=0.3)+
  geom_text(aes(label=n, y=-.05),position = position_dodge(0.9),size=4, colour="black")+
  geom_text(aes(label=sumseed, y=-.1),position = position_dodge(0.9),size=4, colour="black")+
  #geom_text(data=grman10, aes(label=cup), position=position_jitter(width=.2,height=1), size=2, colour="black")+
  xlab("Green Manure")+
  ylab("Germination rate (%)")+
  ggtitle("Germination rate", subtitle="exp 1,2,5,6,8,11, n=varies")+
  geom_text(position = position_dodge(0.9),vjust = -2,hjust=0.25, size=4, label="", aes(fontface=1))+
  theme(text=element_text(size=14), legend.position="")+
  scale_fill_discrete(name = "crop \namendment")+
  theme_bw()





#statistics #### 
str(globgrman.fhk)
library(lme4)
library(emmeans)
fhk.lmm <- lmer(Rrot02~GrMan + blk + GrMan:blk + (1|exp), data=globgrman.fhk ) #fitting blk as random factor returns "singular fit"
fhk.lmm <- lmer(Rrot02~GrMan + blk + (1|exp), data=globgrman.fhk ) 
fhk.lmm <- lm(Rrot02~GrMan + blk + exp + blk:exp + GrMan:blk + GrMan:exp+ GrMan:blk:exp, data=globgrman.fhk ) 
fhk.lmm <- lm(Rrot02~GrMan + blk + exp + blk:exp + GrMan:blk+  GrMan:blk:exp, data=globgrman.fhk ) 

fhk.lmm
anova(fhk.lmm)

fhk.emm <- emmeans(fhk.lmm, "GrMan")
fhk.emm

joint_tests(fhk.emm)
test(pairs(emmeans(fhk.lmm, "GrMan")), joint=T)




#miscelleneaous
#mapping ####
globgrman.map <- globgrman %>% 
  group_by(exp, FvInoc, GrMan, GrManform, GrManpart) %>%
  summarise(mRrot = mean(Rrot, na.rm=T))
View(globgrman.map)
