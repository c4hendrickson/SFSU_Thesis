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

GM_GS_sum <- GM_GS %>%
  select(year, reef, in.out, position, top.bot, grain, percent) %>%
  group_by(year, reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

GM_GS_sum

# FACTORING

GM_GS_sum$in.out <- factor(GM_GS_sum$in.out, c("IN", "OUT"))
GM_GS_sum$position <- factor(GM_GS_sum$position, c("0", "12", "19", "27"))
GM_GS_sum$grain <- factor(GM_GS_sum$grain, c("sand", "silt", "clay"))
GM_GS_sum$year <- factor(GM_GS_sum$year, c("1", "2"))
GM_GS_sum$reef <- factor(GM_GS_sum$reef, c("1", "2", "3"))
GM_GS_sum$top.bot <- factor(GM_GS_sum$top.bot, c("Top", "Bot"))

#### 2019 v 2020, Reef 1, Top v Bot ####

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

#### EXTRAS ####
##makes a colored plot, probably don't use##

#(https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html)
position <- GM_GS$position
grain <- GM_GS$grain
percent <- GM_GS$percent
data <- data.frame(position,grain,percent)

ggplot(data, aes(fill=grain, y=percent, x=position)) + 
  geom_bar(position="dodge", stat="identity")

# make summary table, include stats in there, then graph from summary table to get error bars etc.

#better one from Margot
momsumm <- momdata %>% 
  select(-mound_id) %>% 
  filter(treatment != "FT") %>% #put notes here
  mutate(mom_mg = mom_mass * 1000) %>% 
  group_by(season, treatment) %>% 
  summarise(mean_mg = mean(mom_mg, na.rm = TRUE),
            sd_mg = sd(mom_mg, na.rm = TRUE))

#my attempt
GM_GS_sum <- GM_GS %>%  #take GM_GS and name is GM_GS_sum
  select(year, reef, in.out, position, top.bot, grain, percent) %>%  #select "thing,thing" use "-" to ENTIRE COLUIMN
  #filter(treatment != "FT") %>% #pick the thing in treatment that you want "==" pick this, "!=" remove this WITHIN COLUMN
  #mutate(mom_mg = mom_mass * 1000) %>%  #thing you want to do "mom_mg" becomes new column
  group_by(year, reef, in.out, position, top.bot, grain) %>%  #select things to filter data by and group them in summarise
  summarise(mean = mean(percent, na.rm = TRUE), 
            sd = sd(percent, na.rm = TRUE))  #select "percent" column to do all this to

GM_GS_sum <- GM_GS %>%
  select(year, reef, in.out, position, top.bot, grain, percent) %>%
  group_by(year, reef, in.out, position, top.bot, grain) %>%
  summarise_all(list(mean = mean, sd = sd))

GM_GS_sum

#Margot LSP report notes

facetlabel <- c("oldname" = "newname", "oldname2" = "newname2", etc.) 

facet_grid(top.bot~year, labeller = labeller(top.bot = as_labeller(facetlabel)))

#### LSP report ####

rm(list=ls())

GM_GS <- read.csv("Sediment core data/GM_GS.csv")

#Old Data Summary
GM_GS_sum <- GM_GS %>%
  select(year, reef, in.out, position, top.bot, grain, percent) %>%
  group_by(year, reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

# Version Evan.0
GM_GS_sum <- GM_GS %>%
  select(year, in.out, position, top.bot, grain, percent) %>%
  group_by(year, in.out, position, top.bot, grain) %>%
  na.omit() %>%
  summarize_all(list(mean = mean, sd = sd, n=length)) %>% 
  mutate(se = sd/sqrt(n))

#New Data Summary (v.1)
GM_GS_sum1 <- GM_GS %>%
  select(year, in.out, position, top.bot, grain, percent) %>%
  group_by(year, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(15))
# leaves out 2019 data entirely

#New Data Summary (v.2)
GM_GS_sum <- GM_GS %>%
  select(year, in.out, position, top.bot, grain, percent) %>%
  group_by(year, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd, n = length(.)), na.rm=TRUE) %>%
  mutate(se = sd/sqrt(n))
# Error: expecting a one sided formula, a function, or a function name.

#New Data Summary (v.3)
GM_GS_sum <- GM_GS %>%
  select(year, in.out, position, top.bot, grain, percent) %>%
  group_by(year, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd, n = sum(!is.na(.)))) %>%
  mutate(se = sd/sqrt(n))
# Error: expecting a one sided formula, a function, or a function name.

#Factoring
GM_GS_sum$in.out <- factor(GM_GS_sum$in.out, c("IN", "OUT"))
GM_GS_sum$position <- factor(GM_GS_sum$position, c("-1", "1", "8", "16"))
GM_GS_sum$grain <- factor(GM_GS_sum$grain, c("sand", "silt", "clay"))
GM_GS_sum$year <- factor(GM_GS_sum$year, c("1", "2"))
GM_GS_sum$reef <- factor(GM_GS_sum$reef, c("1", "2", "3"))
GM_GS_sum$top.bot <- factor(GM_GS_sum$top.bot, c("Top", "Bot"))

#ORIGINAL#

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="IN",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")

topbotlabel <- c("Top" = "Top Layer (0-2cm)", "Bot" = "Bottom Layer (2-8cm)")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel)) #if I switch to grid, need to get rid of category break at top of code

# CHEATING - works in report #

rm(list=ls())

GM_GSx <- read.csv("Sediment core data/GM_GSx.csv")

GM_GSx$in.out <- factor(GM_GSx$in.out, c("IN", "OUT"))
GM_GSx$position <- factor(GM_GSx$position, c("-1", "1", "8", "16"))
GM_GSx$grain <- factor(GM_GSx$grain, c("sand"))
GM_GSx$year <- factor(GM_GSx$year, c("1", "2"))
GM_GSx$reef <- factor(GM_GSx$reef, c("1", "2", "3"))
GM_GSx$top.bot <- factor(GM_GSx$top.bot, c("Top", "Bot"))

GMGS1 <- 
  ggplot(data=GM_GSx[GM_GSx$in.out=="IN",], aes(x=position, y=percent, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")

topbotlabel <- c("Top" = "Top Layer (0-2cm)", "Bot" = "Bottom Layer (2-8cm)")

GMGS1 +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(0.7), shape = 1, size = 1) +
  scale_fill_manual(name="Grain", breaks=c("sand"), labels=c("sand"), values=c("gold")) +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey70"),
        plot.background = element_rect(color = "white")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Sand") +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel)) #if I switch to grid, need to get rid of category break at top of code



#### Box Plots, All Reefs Inside - 2019 v 2020 & top v bot ####

rm(list=ls())

GM_GS <- read.csv("Sediment core data/GM_GS.csv") #read "GM_GS" in file Sediment core data, make it file "GM_GS"

library(ggplot2)
library(plotly)
library(dplyr)
library(plyr)
library(tidyr)

# summarize data #

GM_GS_sum <- GM_GS %>%
  select(year, reef, in.out, position, top.bot, grain, percent) %>%
  group_by(year, reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

#from OM code, factoring

GM_GS_sum$in.out <- factor(GM_GS_sum$in.out, c("IN", "OUT"))
GM_GS_sum$position <- factor(GM_GS_sum$position, c("-1", "1", "8", "16"))
GM_GS_sum$grain <- factor(GM_GS_sum$grain, c("sand", "silt", "clay"))
GM_GS_sum$year <- factor(GM_GS_sum$year, c("1", "2"))
GM_GS_sum$reef <- factor(GM_GS_sum$reef, c("1", "2", "3"))
GM_GS_sum$top.bot <- factor(GM_GS_sum$top.bot, c("Top", "Bot"))

# With Reef Influence #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="IN"&GM_GS_sum$grain=="sand",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")

topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

GMGS1 +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(0.75), shape = 1, size = 1) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("goldenrod1", "orange", "yellow")) +
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
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="Giant Marsh, All Reefs, Without Reef Influence
2019 v. 2020 - Top v. Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes") +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel))

#### Bar Plots, All Reefs, Inside - 2019 v 2020 & top v bot ####

rm(list=ls())

GM_GS <- read.csv("Sediment core data/GM_GS.csv") #read "GM_GS" in file Sediment core data, make it file "GM_GS"

# Version Evan.0
GM_GS_sum <- GM_GS %>%
  select(year, in.out, position, top.bot, grain, percent) %>%
  group_by(year, in.out, position, top.bot, grain) %>%
  na.omit() %>%
  summarize_all(list(mean = mean, sd = sd, n=length)) %>% 
  mutate(se = sd/sqrt(n))

#from OM code, factoring

GM_GS_sum$in.out <- factor(GM_GS_sum$in.out, c("IN", "OUT"))
GM_GS_sum$position <- factor(GM_GS_sum$position, c("-1", "1", "8", "16"))
GM_GS_sum$grain <- factor(GM_GS_sum$grain, c("sand", "silt", "clay"))
GM_GS_sum$year <- factor(GM_GS_sum$year, c("1", "2"))
GM_GS_sum$top.bot <- factor(GM_GS_sum$top.bot, c("Top", "Bot"))

# With Reef Influence #

GMGS1 <- 
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="IN",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")

topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
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
  ggplot(data=GM_GS_sum[GM_GS_sum$in.out=="OUT",], aes(x=position, y=mean, fill=grain)) #can do color or fill. If I just want Top/Bot data, add into the brackets above

yearlabel <- c("1"="2019", "2"="2020")

topbotlabel <- c("Top" = "Top (0-2cm)", "Bot" = "Bottom (2-8cm)")

GMGS1 +
  geom_bar(position=position_dodge(0.9), stat="identity", color="grey") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_manual(name="Grain", breaks=c("sand", "silt", "clay"), labels=c("sand", "silt", "clay"), values=c("red", "orange", "yellow")) +
  labs(title="Giant Marsh, Without Reef Influence
2019 v. 2020 & Top v. Bottom") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="Percent Grain Sizes", limits=c(0,60)) +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel))

#### ANALYSIS ####

GM_GS <- read.csv("Sediment core data/GM_GS.csv")

hist(GM_GS$sand)
#data is normal
#gaussian

# splitting grain parts

# SAND GM_GS[1:480,]
# SILT GM_GS[481:960,]
# CLAY GM_GS[961:1440,]

GM_GS_grain=GM_GS[1:480,]

glm1 <- glm(percent ~ position * in.out * top.bot * year, data = GM_GS_grain, family = "gaussian")
# + = main effects
# : = interactive effects
# * = main AND interactive effects
plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#for % data, logit transform - logit(df$colname)

#glmm
library(lme4)
install.packages("scales")
library(scales)
install.packages("reshape2")
library(reshape2)

logit(GM_GS_grain$sand)

glmm1 <- lmer(percent ~ position * in.out * top.bot * year + (1|reef), data = GM_GS_grain)
summary(glmm1)
plot(glmm1)
anova(glmm1)

# Ed Connor analysis - glmmTMB #

freef=as.factor(GM_GS_grain$reef)
fcore=as.factor(GM_GS_grain$core)

newdf=data.frame(GM_GS_grain, freef, fcore)
head(newdf)

newdf=newdf[161:320,]
dim(newdf)

library(car)

#random intercepts model
model1 = glmmTMB(percent ~ in.out * position * top.bot + (1|freef) + (1|freef:fcore), data = newdf, family=beta_family(link="logit"))

model1

anova.model1 = Anova(model1)

anova.model1

#removing core
#random intercepts model - best model
model2 = glmmTMB(X.om/100 ~ in.out * position * top.bot + (1|freef), data = newdf, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#compare 1 & 2

anova(model1,model2)

#random intercepts model
model3 = glmmTMB(X.om/100 ~ in.out * position * top.bot + (1|freef:fcore), data = newdf, family=beta_family(link="logit"))

model3

anova.model3 = Anova(model3)

anova.model3

anova(model1, model2, model3)
