## Sediment core data. 
### stuff to know! ### 
rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

# DATA
SR_GS <- read.csv("Sediment core data/SR_GS.csv")
SR_combine <- read.csv("Sediment core data/SR_combine.csv")

# PACKAGES
library(ggplot2)
library(plotly)
library(dplyr)
library(plyr)
library(tidyr)
library(lme4)
library(nlme)
library(glmmTMB)
library(TMB)
library(Matrix)
library(car)

# SUMMARIZE DATA #

SR_GS_sum <- SR_GS %>%
  select(reef, in.out, position, top.bot, grain, percent) %>%
  group_by(reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

SR_GS_sum

#doesn't work!
SR_GS_sum <- SR_GS %>%
  select(reef, in.out, position, top.bot, sand) %>%
  group_by(reef, in.out, position, top.bot) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5)) %>%
  select(reef, in.out, position, top.bot, silt) %>%
  group_by(reef, in.out, position, top.bot) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5)) %>%
  select(reef, in.out, position, top.bot, clay) %>%
  group_by(reef, in.out, position, top.bot) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

SR_GS_sum

#new, doesn't work!
SR_GS_sum2 <- SR_combine %>%
  select(reef, in.out, position, top.bot, sand, silt, clay) %>%
  group_by(reef, in.out, position, top.bot) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

SR_GS_sum

#from OM code, factoring

SR_GS_sum$in.out <- factor(SR_GS_sum$in.out, c("IN", "OUT"))
SR_GS_sum$position <- factor(SR_GS_sum$position, c("0", "12", "19", "27"))
SR_GS_sum$grain <- factor(SR_GS_sum$grain, c("sand", "silt", "clay"))
SR_GS_sum$reef <- factor(SR_GS_sum$reef, c("1", "2"))
SR_GS_sum$top.bot <- factor(SR_GS_sum$top.bot, c("Top", "Bot"))

#### Select reef, compare all ####

# Reef 1, Top v Bot & IN v OUT #

TNCGS1 <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$reef=="1",], aes(x=position, y=mean, fill=grain)) #can do color or fill

label_INOUT <- c("IN"="With Reef", "OUT"="Without Reef")
label_TOPBOT <- c("Top"="Top (0-2cm)", "Bot"="Bottom (2-8cm)")

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Reef 1
With Reef v Without Reef & Top v Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out, labeller = labeller(in.out=label_INOUT, top.bot=label_TOPBOT)) #if I switch to grid, need to get rid of category break at top of code
#
# Reef 2, Top v Bot & IN v OUT #
#
TNCGS1 <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$reef=="2",], aes(x=position, y=mean, fill=grain)) #can do color or fill

label_INOUT <- c("IN"="With Reef", "OUT"="Without Reef")
label_TOPBOT <- c("Top"="Top (0-2cm)", "Bot"="Bottom (2-8cm)")

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Reef 2
With Reef v Without Reef & Top v Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out, labeller = labeller(in.out=label_INOUT, top.bot=label_TOPBOT)) #if I switch to grid, need to get rid of category break at top of code

#### Both reefs, compare all ####

SR_GS_sum <- SR_GS %>%
  select(in.out, position, top.bot, grain, percent) %>%
  group_by(in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

#from OM code, factoring

SR_GS_sum$in.out <- factor(SR_GS_sum$in.out, c("IN", "OUT"))
SR_GS_sum$position <- factor(SR_GS_sum$position, c("0", "12", "19", "27"))
SR_GS_sum$grain <- factor(SR_GS_sum$grain, c("sand", "silt", "clay"))
SR_GS_sum$top.bot <- factor(SR_GS_sum$top.bot, c("Top", "Bot"))

TNCGS1 <- 
  ggplot(data=SR_GS_sum, aes(x=position, y=mean, fill=grain)) #can do color or fill

label_INOUT <- c("IN"="With Reef", "OUT"="Without Reef")
label_TOPBOT <- c("Top"="Top (0-2cm)", "Bot"="Bottom (2-8cm)")

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Reefs 1&2
With Reef v Without Reef & Top v Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out, labeller = labeller(in.out=label_INOUT, top.bot=label_TOPBOT)) #if I switch to grid, need to get rid of category break at top of code

#### Reef 1&2, IN, Top v Bot####

SR_GS_sum <- SR_GS %>%
  select(reef, in.out, position, top.bot, grain, percent) %>%
  group_by(reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

#from OM code, factoring

SR_GS_sum$in.out <- factor(SR_GS_sum$in.out, c("IN", "OUT"))
SR_GS_sum$position <- factor(SR_GS_sum$position, c("0", "12", "19", "27"))
SR_GS_sum$grain <- factor(SR_GS_sum$grain, c("sand", "silt", "clay"))
SR_GS_sum$reef <- factor(SR_GS_sum$reef, c("1", "2"))
SR_GS_sum$top.bot <- factor(SR_GS_sum$top.bot, c("Top", "Bot"))

TNCGS1 <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$in.out=="IN",], aes(x=position, y=mean, fill=grain)) #can do color or fill

labeldf <- c("1"="Reef 1", "2"="Reef 2")

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Both Reefs, IN, Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", breaks=seq(0, 70, 20)) +
  facet_grid(top.bot~reef, labeller = labeller(reef=labeldf)) #if I switch to grid, need to get rid of category break at top of code

#facet_grid(top.bot~year, labeller = labeller(year=labeldf))
#facet_wrap(~year, nrow=1, labeller = as_labeller(labeldf))
#try viridis instead of scale_color_manual

#### Reef 1&2, OUT, Top v Bot ####

TNCGS1 <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$in.out=="OUT",], aes(x=position, y=mean, fill=grain)) #can do color or fill

labeldf <- c("1"="Reef 1", "2"="Reef 2")

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Both Reefs, OUT, Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", breaks=seq(0, 70, 20)) +
  facet_grid(top.bot~reef, labeller = labeller(reef=labeldf)) #if I switch to grid, need to get rid of category break at top of code
#
#### Select Reef & Position ####

# Reef 1, IN, Top v Bot #

TNCGS1 <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$in.out=="IN"&SR_GS_sum$reef=="1",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Reef 1, IN, Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", breaks=seq(0, 70, 20)) +
  facet_grid(top.bot~.) #if I switch to grid, need to get rid of category break at top of code

# Reef 1, OUT, Top v Bot #

TNCGS1 <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$in.out=="OUT"&SR_GS_sum$reef=="1",], aes(x=position, y=mean, fill=grain)) #can do color or fill

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Reef 1, OUT, Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", breaks=seq(0, 70, 20)) +
  facet_grid(top.bot~.) #if I switch to grid, need to get rid of category break at top of code

# Reef 2, IN, Top v Bot #

TNCGS1 <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$in.out=="IN"&SR_GS_sum$reef=="2",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Reef 2, IN, Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", breaks=seq(0, 70, 20)) +
  facet_grid(top.bot~.) #if I switch to grid, need to get rid of category break at top of code

# Reef 2, OUT, Top v Bot #

TNCGS1 <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$in.out=="OUT"&SR_GS_sum$reef=="2",], aes(x=position, y=mean, fill=grain)) #can do color or fill

TNCGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="SR, Reef 2, OUT, Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", breaks=seq(0, 70, 20)) +
  facet_grid(top.bot~.) #if I switch to grid, need to get rid of category break at top of code

#### PERS/CAERS ####

SR_GS <- read.csv("Sediment core data/SR_GS.csv")

SR_GS_sum <- SR_GS %>%
  select(reef, in.out, position, top.bot, grain, percent) %>%
  mutate(percent=replace(percent, percent<0, 0)) %>%
  group_by(reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

SR_GS_sum$in.out <- factor(SR_GS_sum$in.out, c("IN", "OUT"))
SR_GS_sum$position <- factor(SR_GS_sum$position, c("-1", "1", "8", "16"))
SR_GS_sum$grain <- factor(SR_GS_sum$grain, c("sand", "silt", "clay"))
SR_GS_sum$reef <- factor(SR_GS_sum$reef, c("1", "2"))
SR_GS_sum$top.bot <- factor(SR_GS_sum$top.bot, c("Top", "Bot"))

TNCGSnew <- 
  ggplot(data=SR_GS_sum, aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

topbotlabel <- c("Top" = "Top Layer (0-2cm)", "Bot" = "Bottom Layer (2-8cm)")

inoutlabel <- c("IN" = "With Reef Influence", "OUT" = "Without Reef Influence")

TNCGSnew +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(0.7), shape = 1, size = 1) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey70"),
        plot.background = element_rect(color = "white")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Sand") +
  facet_grid(top.bot~in.out, labeller = labeller(top.bot=topbotlabel, in.out=inoutlabel)) #if I switch to grid, need to get rid of category break at top of code

#### Bar Plots - Both Reefs - With Reef/Without Reef & Top/Bottom ####

rm(list=ls())

SR_GS <- read.csv("Sediment core data/SR_GS.csv") #read "GM_GS" in file Sediment core data, make it file "GM_GS"

SR_GS_sum <- SR_GS %>%
  select(in.out, position, top.bot, grain, percent) %>%
  group_by(in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

#from OM code, factoring

SR_GS_sum$in.out <- factor(SR_GS_sum$in.out, c("IN", "OUT"))
SR_GS_sum$position <- factor(SR_GS_sum$position, c("0", "12", "19", "27"))
SR_GS_sum$grain <- factor(SR_GS_sum$grain, c("sand", "silt", "clay"))
SR_GS_sum$top.bot <- factor(SR_GS_sum$top.bot, c("Top", "Bot"))

SRGSnew <- 
  ggplot(data=SR_GS_sum, aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

inoutlabel <- c("IN"="With Reef", "OUT"="Without Reef")

topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

SRGSnew +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="San Rafael, Reef 1 & 2
With Reef v. Without Reef - Top v. Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out, labeller = labeller(in.out=inoutlabel, top.bot=topbotlabel))

#### Box Plots - Both Reefs - With Reef/Without Reef & Top/Bottom ####

rm(list=ls())

SR_GS <- read.csv("Sediment core data/SR_GS.csv") #read "GM_GS" in file Sediment core data, make it file "GM_GS"

SR_GS_sum <- SR_GS %>%
  select(reef, in.out, position, top.bot, grain, percent) %>%
  group_by(reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

#from OM code, factoring

SR_GS_sum$in.out <- factor(SR_GS_sum$in.out, c("IN", "OUT"))
SR_GS_sum$position <- factor(SR_GS_sum$position, c("0", "12", "19", "27"))
SR_GS_sum$grain <- factor(SR_GS_sum$grain, c("sand", "silt", "clay"))
SR_GS_sum$top.bot <- factor(SR_GS_sum$top.bot, c("Top", "Bot"))
SR_GS_sum$reef <- factor(SR_GS_sum$reef, c("1", "2"))

SRGSnew <- 
  ggplot(data=SR_GS_sum[SR_GS_sum$reef=="1"&SR_GS_sum$grain!="sand",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

inoutlabel <- c("IN"="With Reef", "OUT"="Without Reef")

topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

SRGSnew +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(0.7), shape = 1, size = 1) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="San Rafael, Reef 1
With Reef v. Without Reef - Top v. Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out, labeller = labeller(in.out=inoutlabel, top.bot=topbotlabel))

