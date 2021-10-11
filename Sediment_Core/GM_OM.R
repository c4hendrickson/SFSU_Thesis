## Sediment core data. 
### stuff to know! ### 
rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

# DATA
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

# SUMMARIZE DATA #
GM_combine_sum <- GM_combine %>%
  select(reef, in.out, position, top.bot, grain, percent) %>%
  group_by(reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

GM_combine_sum

#FACTORS

GM_combine$year <- factor(GM_combine$year, c("1", "2"))
GM_combine$reef <- factor(GM_combine$reef, c("1", "2", "3"))
GM_combine$in.out <- factor(GM_combine$in.out, c("IN", "OUT")) 
GM_combine$top.bot <- factor(GM_combine$top.bot, c("Top", "Bot"))

#position

#factor as CATEGORIES with evenly spaced positions
GM_combine$position <- factor(GM_combine$position, c("0", "12", "19", "27"))

#AND

#factor as NUMERIC with middle of the reef as "0m"
GM_combine$position1 <- (as.numeric(as.character(GM_combine$position))-6)
#GM_combine$position1 <- (as.numeric(as.character(GM_combine$position)))

#### ALL REEFS - 2019 ####

#### Top, split by IN/OUT #

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="1"&GM_combine$top.bot=="Top",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, All Reefs, Top, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.4, 3.4)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#### Bottom, split by IN/OUT #

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="1"&GM_combine$top.bot=="Bot",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, All Reefs, Bot, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.8, 4.3, 0.5), limits=c(1.8, 4.2)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#### IN, split by Top/Bot #

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="1"&GM_combine$in.out=="IN",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("Top"="Top", "Bot"="Bottom")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, All Reefs, IN, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.4, 3.6)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### OUT, split by Top/Bot #

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="1"&GM_combine$in.out=="OUT",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("Top"="Top", "Bot"="Bottom")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, All Reefs, OUT, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 4.2, 0.5), limits=c(1.3, 4.2)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### ALL REEFS - 2020 ####

#### Top, split by IN/OUT #

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="2"&GM_combine$top.bot=="Top",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, All Reefs, Top, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.0, 0.5), limits=c(1.4, 3.0)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#### Bottom, split by IN/OUT #

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="2"&GM_combine$top.bot=="Bot",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, All Reefs, Bot, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.5, 0.5), limits=c(1.4, 3.5)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#### IN, split by Top/Bot #

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="2"&GM_combine$in.out=="IN",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("Top"="Top", "Bot"="Bottom")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, All Reefs, IN, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.5, 0.5), limits=c(1.5, 3.5)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### OUT, split by Top/Bot #

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="2"&GM_combine$in.out=="OUT",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("Top"="Top", "Bot"="Bottom")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, All Reefs, OUT, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.5, 0.5), limits=c(1.4, 3.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### Select year & reef, split by in/out ####
#
#
#2019, reef 1
#
GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="1"&GM_combine$reef=="1",], aes(x=position, y=X.om, color=in.out))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, Reef 1, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.2, 3.6)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(topbotlabel))
#
#2019, reef 2
#
GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="1"&GM_combine$reef=="2",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, Reef 2, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.2, 3.7)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#2019, reef 3
#
GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="1"&GM_combine$reef=="3",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, Reef 3, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.6, 4.2, 0.5), limits=c(1.6, 4.2)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#2020, reef 1
#
GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="2"&GM_combine$reef=="1",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, Reef 1, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.1, 0.5), limits=c(1.4, 3.1)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#2020, reef 2
#
GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="2"&GM_combine$reef=="2",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, Reef 2, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.2, 0.5), limits=c(1.4, 3.2)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#2020, reef 3
#
GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="2"&GM_combine$reef=="3",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, Reef 3, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.5, 3.5, 0.5), limits=c(1.5, 3.5)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
####Select Top, split IN/OUT####
#
#
GM1b <- ggplot(data=GM_combine[GM_combine$top.bot=="Top"&GM_combine$year=="1",], aes(x=position, y=X.om, color=in.out))
#[]inside square brackets always write row,column
#
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef/No Reef", breaks=c("IN", "OUT"), labels=c("Reef", "No Reef"), values=c("green2", "darkorange4")) +
  labs(title="2019 % Organic Matter, All Reefs, Top (0-2cm)") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.3))
#
####Select Bot, split IN/OUT####
#
GM1b <- ggplot(data=GM_combine[GM_combine$top.bot=="Bot"&GM_combine$year=="1",], aes(x=position, y=X.om, color=in.out))
#[]inside square brackets always write row,column

GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef/No Reef", breaks=c("IN", "OUT"), labels=c("Reef", "No Reef"), values=c("green2", "darkorange4")) +
  labs(title="2019 % Organic Matter, All Reefs, Bottom (2-8cm)") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 4.2, 0.3))
#
####Select IN, split Top/Bot####
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="IN"&GM_combine$year=="1",], aes(x=position, y=X.om, color=top.bot))

GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("deepskyblue", "navy")) +
  labs(title="2019 % OM, Inside All Reefs - GM") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m) (m)") +
  scale_y_continuous(name="OM (%)", breaks=seq(1.4, 3.7, 0.3))
#
####Select OUT, split Top/Bot####
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="OUT"&GM_combine$year=="1",], aes(x=position, y=X.om, color=top.bot))

GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("deepskyblue", "navy")) +
  labs(title="2019 % OM, Outside All Reefs - GM") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m) (m)") +
  scale_y_continuous(name="OM (%)", breaks=seq(1.4, 4.2, 0.3))
#
####Select only IN/OUT####
#
#2019
#
#Reef 1 IN
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="IN"&GM_combine$reef=="1"&GM_combine$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 1", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 1 OUT
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="OUT"&GM_combine$reef=="1"&GM_combine$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 1", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 2 IN
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="IN"&GM_combine$reef=="2"&GM_combine$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 2", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 2 OUT
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="OUT"&GM_combine$reef=="2"&GM_combine$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 2", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 3 IN
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="IN"&GM_combine$reef=="3"&GM_combine$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 3", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 3 OUT
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="OUT"&GM_combine$reef=="3"&GM_combine$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 3", x="Mudflat Position (m)", y="OM (%)")
#
#2020#
#
#Reef 1 IN
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="IN"&GM_combine$reef=="1"&GM_combine$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 1", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 1 OUT
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="OUT"&GM_combine$reef=="1"&GM_combine$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 1", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 2 IN
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="IN"&GM_combine$reef=="2"&GM_combine$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 2", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 2 OUT
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="OUT"&GM_combine$reef=="2"&GM_combine$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 2", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 3 IN
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="IN"&GM_combine$reef=="3"&GM_combine$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 3", x="Mudflat Position (m)", y="OM (%)")
#
#Reef 3 OUT
#
GM1b <- ggplot(data=GM_combine[GM_combine$in.out=="OUT"&GM_combine$reef=="3"&GM_combine$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 3", x="Mudflat Position (m)", y="OM (%)")

#### LSP report ####

GM_combine <- read.csv("Sediment core data/GM_combine.csv") #read "GM_combine" in file Sediment core data, make it file "GM_combine"
library(ggplot2)
#
GM_combine$year <- factor(GM_combine$year,c("1", "2"))
GM_combine$reef <- factor(GM_combine$reef, c("1", "2", "3"))
GM_combine$in.out <- factor(GM_combine$in.out,c("IN", "OUT")) 
GM_combine$top.bot <- factor(GM_combine$top.bot,c("Top", "Bot")) 
GM_combine$position <- factor(GM_combine$position,c("-1", "1", "8", "16"))

#ORIGINAL#

GMOM1 <- ggplot(data=GM_combine[GM_combine$year=="2",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey70"),
        plot.background = element_rect(color = "white")) +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.4, 3.5, 0.5), limits=c(1.4, 3.5)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#NEW#

GMOM2 <- ggplot(data=GM_combine[GM_combine$year=="2",], aes(x=position, y=X.om, color=in.out))

labeldf <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")

GMOM2+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 1) +
  scale_color_manual(name="Reef Influence", breaks=c("IN", "OUT"), labels=c("With Reef", "Without Reef"), values=c("navy", "deepskyblue")) +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey70"),
        plot.background = element_rect(color = "white")) +
  scale_x_discrete(name="Mudflat Position (m) (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.4, 3.5, 0.5), limits=c(1.4, 3.5)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### NEW STUFF ####

#### all reefs combined, looking at 2019 v 2020 and top v bottom

GM_combine$in.out <- factor(GM_combine$in.out, c("OUT", "IN")) 

GMOM <- ggplot(data=GM_combine, aes(x=position, y=X.om, color=in.out))

topbotlabel <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")
yearlabel <- c("1"="2019", "2"="2020")

GMOM+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 0.5) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  labs(title="Giant Marsh, All Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m) (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.4, 4.25, 0.5), limits=c(1.4, 4.25)) +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel))

#### all reefs combined, 2019 only and looking at top v bottom -----------

GM_combine$in.out <- factor(GM_combine$in.out, c("OUT", "IN")) 

GMOM <- ggplot(data=GM_combine[GM_combine$year=="1",], aes(x=position, y=X.om, color=in.out))

topbotlabel <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")
reeflabel <- c("1"="reef 1 (north)", "2"="reef 2 (middle)", "3"="reef 3 (south)")

GMOM+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 0.5) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  labs(title="Giant Marsh, All Reefs, 2019") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.4, 4.4, 0.5), limits=c(1.4, 4.4)) +
  facet_grid(top.bot~reef, labeller = labeller(reef=reeflabel, top.bot=topbotlabel))

#### all reefs combined, 2020 only and looking at top v bottom -----------

GM_combine$in.out <- factor(GM_combine$in.out, c("OUT", "IN")) 

GMOM <- ggplot(data=GM_combine[GM_combine$year=="2",], aes(x=position, y=X.om, color=in.out))

topbotlabel <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")
reeflabel <- c("1"="reef 1 (north)", "2"="reef 2 (middle)", "3"="reef 3 (south)")

GMOM+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 0.5) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  labs(title="Giant Marsh, All Reefs, 2020") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.4, 3.9, 0.5), limits=c(1.4, 3.9)) +
  facet_grid(top.bot~reef, labeller = labeller(reef=reeflabel, top.bot=topbotlabel))

#### top only, looking at each reef, 2019 v 2020

GM_combine$in.out <- factor(GM_combine$in.out, c("OUT", "IN")) 

GMOM <- ggplot(data=GM_combine[GM_combine$top.bot=="Top",], aes(x=position, y=X.om, color=in.out))

yearlabel <- c("1"="2019", "2"="2020")
reeflabel <- c("1"="reef 1 (north)", "2"="reef 2 (middle)", "3"="reef 3 (south)")

GMOM+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 0.5) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  labs(title="Giant Marsh, All Reefs, Top Only") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m) (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.4, 3.4, 0.5), limits=c(1.4, 3.4)) +
  facet_grid(year~reef, labeller = labeller(year=yearlabel, reef=reeflabel))

#### bot only, looking at each reef, 2019 v 2020

GM_combine$in.out <- factor(GM_combine$in.out, c("OUT", "IN")) 

GMOM <- ggplot(data=GM_combine[GM_combine$top.bot=="Bot",], aes(x=position, y=X.om, color=in.out))

yearlabel <- c("1"="2019", "2"="2020")
reeflabel <- c("1"="reef 1 (north)", "2"="reef 2 (middle)", "3"="reef 3 (south)")

GMOM+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 0.5) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  labs(title="Giant Marsh, All Reefs, Top Only") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m) (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.5, 4.2, 0.5), limits=c(1.5, 4.2)) +
  facet_grid(year~reef, labeller = labeller(year=yearlabel, reef=reeflabel))
