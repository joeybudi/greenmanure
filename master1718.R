library(tidyverse)
library(dplyr)
install.packages("agricolae")
library(agricolae)

masterGH19 <- read_csv("U:/Green Manure ghse exp/global oats compiled exp 1-5/MASTER171819 w NA.csv", 
                       col_types = cols(DSperc = col_double(), 
                                        LArea = col_double(), Rlen = col_double(), 
                                        Rwgt = col_double(), Slen = col_double(), 
                                        blk = col_skip(), 
                                        plot = col_character(), unif_l = col_double()), na = "NA")
head(masterGH15)
str(masterGH15)
View(masterGH15)
table(masterGH15$FvInoc)
table(masterGH15$glob_blk)
table(masterGH15$GrMan)

masterGH15_sel_con <- masterGH15 %>% 
  select(plot,exp,glob_blk, GrMan, FvInoc, Rrot)%>% 
  filter(GrMan==c("control"))
View(masterGH15_sel_con)

masterGH15_sel_oat <- masterGH15 %>% 
  select(plot,exp,glob_blk, GrMan, FvInoc, Rrot)%>% 
  filter(GrMan==c("oats"))
View(masterGH15_sel_oat)

masterGH15_sel_rye <- masterGH15 %>% 
  select(plot,exp,glob_blk, GrMan, FvInoc, Rrot)%>% 
  filter(GrMan==c("rye"))
View(masterGH15_sel_rye)

masterGH15_sel_alf <- masterGH15 %>% 
  select(plot,exp,glob_blk, GrMan, FvInoc, Rrot)%>% 
  filter(GrMan==c("alfalfa"))
View(masterGH15_sel_rye)

masterGH15sel <- bind_rows(masterGH15_sel_con,masterGH15_sel_oat,masterGH15_sel_rye,masterGH15_sel_alf)
str(masterGH15sel)
table(masterGH15sel$GrMan) #how many exp unit are there on each green manure treatment?

str(masterGH15_sel)
View(masterGH15_sel)


#NEED TO MIX MODEL USING LMER AND SUCH
mod.Rrot<-lm(Rrot~GrMan  + FvInoc + GrMan:FvInoc + glob_blk + exp, data=masterGH15sel)
anova(mod.Rrot)

summary(mod.Rrot)

LSD.test(mod.Rrot, c("GrMan","FvInoc"), console=TRUE)

library(emmeans)
class(masterGH15_sel)
masterGH15_sel.df <- as.data.frame(masterGH15_sel)

GHsel.emm <- emmeans(mod.Rrot, c('GrMan','FvInoc'))
GHsel.emm
joint_tests(GHsel.emm)
plot(GHsel.emm, horizontal=F, ylab='Root rot%')
CLD(GHsel.emm)


collard.emm <- emmeans(collard.lm, c('clarify.f', 'size.f'))
joint_tests(collard.emm)
plot(collard.emm)









ggplot(masterGH15_sel,aes(GrMan,Rrot))+
  geom_boxplot()

#try chi-squared test comparing proportions relative to control?