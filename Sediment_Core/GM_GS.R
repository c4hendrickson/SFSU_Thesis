## Sediment core data. 
### stuff to know! ### 
rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

# DATA
GM_GS <- read.csv("Sediment core data/GM_GS.csv") 
GM_combine <- read.csv("Sediment core data/GM_combine.csv")

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

# SUMMARIZE DATA

# WORKS
GM_GS_sum <- GM_GS %>%
  select(year, reef, in.out, position, top.bot, grain, percent) %>%
  group_by(year, reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

# COMBINE - not used #
GM_combine_sum <- GM_combine %>%
  select(year, reef, in.out, position, top.bot, sand) %>%
  group_by(year, reef, in.out, position, top.bot) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

GM_GS_sum

# doesn't work - select individually, pipe all together

GM_GS_sum <- GM_GS %>%
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

GM_GS_sum

#new, doesn't work - select each one
GM_GS_sum <- GM_combine %>%
  select(reef, in.out, position, top.bot, sand, silt, clay) %>%
  group_by(reef, in.out, position, top.bot) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

GM_GS_sum

# FACTOR DATA

GM_combine$in.out <- factor(GM_combine$in.out, c("IN", "OUT"))
GM_combine$position <- factor(GM_combine$position, c("0", "12", "19", "27"))
GM_combine$position1 <- (as.numeric(as.character(GM_combine$position))-6)
GM_combine$grain <- factor(GM_combine$grain, c("sand", "silt", "clay"))
GM_combine$year <- factor(GM_combine$year, c("1", "2"))
GM_combine$reef <- factor(GM_combine$reef, c("1", "2", "3"))
GM_combine$top.bot <- factor(GM_combine$top.bot, c("Top", "Bot"))

# FACTOR SUMMARY DATA

GM_GS_sum$in.out <- factor(GM_GS_sum$in.out, c("IN", "OUT"))
GM_GS_sum$position <- factor(GM_GS_sum$position, c("0", "12", "19", "27"))
GM_GS_sum$position1 <- (as.numeric(as.character(GM_GS_sum$position))-6)
GM_GS_sum$grain <- factor(GM_GS_sum$grain, c("sand", "silt", "clay"))
GM_GS_sum$year <- factor(GM_GS_sum$year, c("1", "2"))
GM_GS_sum$reef <- factor(GM_GS_sum$reef, c("1", "2", "3"))
GM_GS_sum$top.bot <- factor(GM_GS_sum$top.bot, c("Top", "Bot"))

#### ALL 3 REEFS ####
#### WITH or WITHOUT REEF - 2019 v. 2020 & Top v. Bottom ####
#### Box Plots ####

# With Reef Influence #

GMGS1 <- 
  ggplot(data=GM_combine[GM_combine$in.out=="IN",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")
topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

GMGS1 +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(0.75), shape = 1, size = 1) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("goldenrod1", "darkorange", "yellow")) +
  labs(title="Giant Marsh, All Reefs, With Reef Influence
2019 v. 2020 - Top v. Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Sand") +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel))

# Without Reef Influence #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="OUT",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")
topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

GMGS1 +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(0.75), shape = 1, size = 1) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("goldenrod1", "darkorange", "yellow")) +
  labs(title="Giant Marsh, All Reefs, Without Reef Influence
2019 v. 2020 - Top v. Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel))

#### mean +/- se ####
# se = sd/sqrt(5)

# With Reef Influence #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="IN",], aes(x=position, y=mean, color=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")
topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

GMGS1 +
  geom_point(position = position_dodge(0.7), size = 2) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.7), width=0.3) +
  scale_color_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="Giant Marsh, All Reefs, With Reef Influence
2019 v. 2020 & Top v. Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", limits=c(0,60)) +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel))

# Without Reef Influence #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="OUT",], aes(x=position, y=mean, color=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")
topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

GMGS1 +
  geom_point(position = position_dodge(0.7), size = 2) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.7), width=0.3) +
  scale_color_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="Giant Marsh, Without Reef Influence
2019 v. 2020 & Top v. Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", limits=c(0,60)) +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel))

#### Reef 1, With Reef, 2019 v 2020, Top v Bot ####

# IN #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="IN"&GM_GS_sum$reef=="1",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")

topbotlabel <- c("Top" = "Top", "Bot" = "Bottom")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, Reef 1, With Reef, Top v Bot, 2019 v 2020") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel)) #if I switch to grid, need to get rid of category break at top of code

# OUT #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="OUT"&GM_GS_sum$reef=="1",], aes(x=position, y=mean, fill=grain)) #can do color or fill
# &GM_GS_sum$in.out=="IN"
# &GM_GS_sum$position=="1"
# &GM_GS_sum$reef=="1"
# &GM_GS_sum$top.bot=="Top"

labeldf <- c("1"="2019", "2"="2020")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, Reef 1, Without Reef, Top v Bot, 2019 v 2020") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=labeldf)) #if I switch to grid, need to get rid of category break at top of code

  #facet_grid(top.bot~year, labeller = labeller(year=labeldf))
  #facet_wrap(~year, nrow=1, labeller = as_labeller(labeldf))
#try viridis instead of scale_color_manual

#### 2019 v 2020, Reef 2, Top v Bot ####

# IN #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="IN"&GM_GS_sum$reef=="2",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

# &GM_GS_sum$in.out=="IN"
# &GM_GS_sum$position=="1"
# &GM_GS_sum$reef=="1"
# &GM_GS_sum$top.bot=="Top"

labeldf <- c("1"="2019", "2"="2020")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, Reef 2, With Reef, Top v Bot, 2019 v 2020") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=labeldf)) #if I switch to grid, need to get rid of category break at top of code

# OUT #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="OUT"&GM_GS_sum$reef=="2",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

# &GM_GS_sum$in.out=="IN"
# &GM_GS_sum$position=="1"
# &GM_GS_sum$reef=="1"
# &GM_GS_sum$top.bot=="Top"

labeldf <- c("1"="2019", "2"="2020")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, Reef 2, Without Reef, Top v Bot, 2019 v 2020") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=labeldf)) #if I switch to grid, need to get rid of category break at top of code

#### 2019 v 2020, Reef 3, Top v Bot ####

# IN #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="IN"&GM_GS_sum$reef=="3",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

# &GM_GS_sum$in.out=="IN"
# &GM_GS_sum$position=="1"
# &GM_GS_sum$reef=="1"
# &GM_GS_sum$top.bot=="Top"

labeldf <- c("1"="2019", "2"="2020")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, Reef 3, With Reef, Top v Bot, 2019 v 2020") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=labeldf)) #if I switch to grid, need to get rid of category break at top of code

# OUT #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="OUT"&GM_GS_sum$reef=="3",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

# &GM_GS_sum$in.out=="IN"
# &GM_GS_sum$position=="1"
# &GM_GS_sum$reef=="1"
# &GM_GS_sum$top.bot=="Top"

labeldf <- c("1"="2019", "2"="2020")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, Reef 3, Without Reef, Top v Bot, 2019 v 2020") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=labeldf)) #if I switch to grid, need to get rid of category break at top of code
#
#### Select Reef & Year ####

# 2019, Reef 1 #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$reef=="1"&GM_GS_sum$year=="1",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, 2019, Reef 1, IN v OUT & Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out) #if I switch to grid, need to get rid of category break at top of code

# 2019, Reef 2 #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$reef=="2"&GM_GS_sum$year=="1",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, 2019, Reef 2, IN v OUT & Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out) #if I switch to grid, need to get rid of category break at top of code

# 2019, Reef 3 #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$reef=="3"&GM_GS_sum$year=="1",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, 2019, Reef 3, IN v OUT & Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out) #if I switch to grid, need to get rid of category break at top of code

# 2020, Reef 1 #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$reef=="1"&GM_GS_sum$year=="2",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, 2020, Reef 1, IN v OUT & Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out) #if I switch to grid, need to get rid of category break at top of code

# 2020, Reef 2 #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$reef=="2"&GM_GS_sum$year=="2",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, 2020, Reef 2, IN v OUT & Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out) #if I switch to grid, need to get rid of category break at top of code

# 2020, Reef 3 #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$reef=="3"&GM_GS_sum$year=="2",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="GM, 2020, Reef 3, IN v OUT & Top v Bot") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~in.out) #if I switch to grid, need to get rid of category break at top of code
