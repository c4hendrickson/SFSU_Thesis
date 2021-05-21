## Sediment core data. 
### stuff to know! ### 
rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

GM_OM <- read.csv("Sediment core data/GM_OM.csv") #read "GM_OM" in file Sediment core data, make it file "GM_OM"
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
#
GM_OM$year <- factor(GM_OM$year,c("1", "2"))
GM_OM$reef <- factor(GM_OM$reef, c("1", "2", "3"))
GM_OM$in.out <- factor(GM_OM$in.out,c("IN", "OUT")) 
GM_OM$position <- factor(GM_OM$position,c("-1", "1", "8", "16"))


#### ALL REEFS - 2019 ####

#### Top, split by IN/OUT #

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="1"&GM_OM$top.bot=="Top",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, All Reefs, Top, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.2, 3.6)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#### Bottom, split by IN/OUT #

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="1"&GM_OM$top.bot=="Bot",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, All Reefs, Bot, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.8, 4.2)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#### IN, split by Top/Bot #

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="1"&GM_OM$in.out=="IN",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("Top"="Top", "Bot"="Bottom")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, All Reefs, IN, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.4, 3.6)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### OUT, split by Top/Bot #

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="1"&GM_OM$in.out=="OUT",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("Top"="Top", "Bot"="Bottom")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, All Reefs, OUT, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 4.2, 0.5), limits=c(1.3, 4.2)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))
# Top & Bot flipped....

#### ALL REEFS - 2020 ####

#### Top, split by IN/OUT #

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="2"&GM_OM$top.bot=="Top",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, All Reefs, Top, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.0, 0.5), limits=c(1.3, 3.0)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#### Bottom, split by IN/OUT #

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="2"&GM_OM$top.bot=="Bot",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, All Reefs, Bot, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.5, 0.5), limits=c(1.4, 3.5)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))

#### IN, split by Top/Bot #

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="2"&GM_OM$in.out=="IN",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("Top"="Top", "Bot"="Bottom")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, All Reefs, IN, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.5, 0.5), limits=c(1.5, 3.5)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))
# Top & Bot flipped....

#### OUT, split by Top/Bot #

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="2"&GM_OM$in.out=="OUT",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("Top"="Top", "Bot"="Bottom")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, All Reefs, OUT, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.5, 0.5), limits=c(1.4, 3.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))
# Top & Bot flipped....

#### Select year & reef, split by in/out ####
#
#
#2019, reef 1
#
GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="1"&GM_OM$reef=="1",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, Reef 1, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.2, 3.6)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#
#2019, reef 2
#
GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="1"&GM_OM$reef=="2",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, Reef 2, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.5), limits=c(1.2, 3.7)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#2019, reef 3
#
GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="1"&GM_OM$reef=="3",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2019, Reef 3, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.6, 4.2, 0.5), limits=c(1.6, 4.2)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#2020, reef 1
#
GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="2"&GM_OM$reef=="1",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, Reef 1, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.1, 0.5), limits=c(1.4, 3.1)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#2020, reef 2
#
GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="2"&GM_OM$reef=="2",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, Reef 2, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.2, 0.5), limits=c(1.4, 3.2)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#2020, reef 3
#
GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="2"&GM_OM$reef=="3",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

GMOM1+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="GM, 2020, Reef 3, % Organic Matter") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.5, 3.5, 0.5), limits=c(1.5, 3.5)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
####Select Top, split IN/OUT####
#
#
GM1b <- ggplot(data=GM_OM[GM_OM$top.bot=="Top"&GM_OM$year=="1",], aes(x=position, y=X.om, color=in.out))
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
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 3.4, 0.3))
#
####Select Bot, split IN/OUT####
#
GM1b <- ggplot(data=GM_OM[GM_OM$top.bot=="Bot"&GM_OM$year=="1",], aes(x=position, y=X.om, color=in.out))
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
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(1.4, 4.2, 0.3))
#
####Select IN, split Top/Bot####
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="IN"&GM_OM$year=="1",], aes(x=position, y=X.om, color=top.bot))

GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("deepskyblue", "navy")) +
  labs(title="2019 % OM, Inside All Reefs - GM") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="OM (%)", breaks=seq(1.4, 3.7, 0.3))
#
####Select OUT, split Top/Bot####
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="OUT"&GM_OM$year=="1",], aes(x=position, y=X.om, color=top.bot))

GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("deepskyblue", "navy")) +
  labs(title="2019 % OM, Outside All Reefs - GM") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0), 
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="OM (%)", breaks=seq(1.4, 4.2, 0.3))
#
####Select only IN/OUT####
#
#2019
#
#Reef 1 IN
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="IN"&GM_OM$reef=="1"&GM_OM$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 1", x="Reef Position", y="OM (%)")
#
#Reef 1 OUT
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="OUT"&GM_OM$reef=="1"&GM_OM$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 1", x="Reef Position", y="OM (%)")
#
#Reef 2 IN
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="IN"&GM_OM$reef=="2"&GM_OM$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 2", x="Reef Position", y="OM (%)")
#
#Reef 2 OUT
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="OUT"&GM_OM$reef=="2"&GM_OM$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 2", x="Reef Position", y="OM (%)")
#
#Reef 3 IN
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="IN"&GM_OM$reef=="3"&GM_OM$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 3", x="Reef Position", y="OM (%)")
#
#Reef 3 OUT
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="OUT"&GM_OM$reef=="3"&GM_OM$year=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 3", x="Reef Position", y="OM (%)")
#
#2020#
#
#Reef 1 IN
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="IN"&GM_OM$reef=="1"&GM_OM$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 1", x="Reef Position", y="OM (%)")
#
#Reef 1 OUT
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="OUT"&GM_OM$reef=="1"&GM_OM$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 1", x="Reef Position", y="OM (%)")
#
#Reef 2 IN
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="IN"&GM_OM$reef=="2"&GM_OM$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 2", x="Reef Position", y="OM (%)")
#
#Reef 2 OUT
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="OUT"&GM_OM$reef=="2"&GM_OM$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 2", x="Reef Position", y="OM (%)")
#
#Reef 3 IN
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="IN"&GM_OM$reef=="3"&GM_OM$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Inside Reef 3", x="Reef Position", y="OM (%)")
#
#Reef 3 OUT
#
GM1b <- ggplot(data=GM_OM[GM_OM$in.out=="OUT"&GM_OM$reef=="3"&GM_OM$year=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
GM1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="%OM Outside Reef 3", x="Reef Position", y="OM (%)")

#### LSP report ####

GM_OM <- read.csv("Sediment core data/GM_OM.csv") #read "GM_OM" in file Sediment core data, make it file "GM_OM"
library(ggplot2)
#
GM_OM$year <- factor(GM_OM$year,c("1", "2"))
GM_OM$reef <- factor(GM_OM$reef, c("1", "2", "3"))
GM_OM$in.out <- factor(GM_OM$in.out,c("IN", "OUT")) 
GM_OM$top.bot <- factor(GM_OM$top.bot,c("Top", "Bot")) 
GM_OM$position <- factor(GM_OM$position,c("-1", "1", "8", "16"))

#ORIGINAL#

GMOM1 <- ggplot(data=GM_OM[GM_OM$year=="2",], aes(x=position, y=X.om, color=top.bot))

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

GMOM2 <- ggplot(data=GM_OM[GM_OM$year=="2",], aes(x=position, y=X.om, color=in.out))

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
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.4, 3.5, 0.5), limits=c(1.4, 3.5)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### NEW STUFF ####

GM_OM$in.out <- factor(GM_OM$in.out, c("OUT", "IN")) 

GMOM <- ggplot(data=GM_OM, aes(x=position, y=X.om, color=in.out))

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
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(1.4, 4.25, 0.5), limits=c(1.4, 4.25)) +
  facet_grid(top.bot~year, labeller = labeller(year=yearlabel, top.bot=topbotlabel))

#### ANALYSIS ####

GM_OM <- read.csv("Sediment core data/GM_OM.csv")

hist(GM_OM$X.om)
#data is normal
#gaussian

glm1 <- glm(X.om ~ position * in.out * top.bot * year, data = GM_OM, family = "gaussian")
# + = main effects
# : = interactive effects
# * = main AND interactive effects
plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#glmm
library(lme4)

glmm1 <- lmer(X.om ~ position * in.out * top.bot * year + (1|reef), data = GM_OM)
plot(glmm1)
summary(glmm1)
anova(glmm1)

# Ed Connor Stuff #

#glmmTMB

freef=as.factor(GM_OM$reef)
fcore=as.factor(GM_OM$core)

newdf=data.frame(GM_OM, freef, fcore)
head(newdf)

newdf=newdf[1:160,]
dim(newdf)

library(car)

#random intercepts model
model1 = glmmTMB(X.om/100 ~ in.out * position * top.bot * year + (1|freef) + (1|freef:fcore), data = newdf, family=beta_family(link="logit"))

model1

anova.model1 = Anova(model1)

anova.model1

#removing core
#random intercepts model - best model
model2 = glmmTMB(X.om/100 ~ in.out * position * top.bot * year + (1|freef), data = newdf, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#compare 1 & 2

anova(model1,model2)

#random intercepts model
model3 = glmmTMB(X.om/100 ~ in.out * position * top.bot * year + (1|freef:fcore), data = newdf, family=beta_family(link="logit"))

model3

anova.model3 = Anova(model3)

anova.model3

#compare all models

anova(model1, model2, model3)
