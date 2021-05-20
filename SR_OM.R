# Sediment core data. 
# stuff to know! #
rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

SR_OM <- read.csv("Sediment core data/SR_OM.csv")

SR_combine <- read.csv("Sediment core data/SR_combine.csv")

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
#FACTORS#
#in "SR_OM", select column "position", factor "SR_OM$position", c (list) "catagories" of data from the selected column.
SR_OM$reef <- factor(SR_OM$reef, c("1", "2"))
SR_OM$in.out <- factor(SR_OM$in.out, c("IN", "OUT")) 
SR_OM$position <- factor(SR_OM$position, c("-1", "1", "8", "16"))
SR_OM$top.bot <- factor(SR_OM$top.bot, c("Top", "Bot"))
#
#### BOTH REEFS ####
#
# Top, compare Reef/No Reef #
#
TNC1b <- ggplot(data=SR_OM[SR_OM$top.bot=="Top",], aes(x=position, y=X.om, color=in.out))
#[]inside square brackets always write row,column

TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef/No Reef", breaks=c("IN", "OUT"), labels=c("Reef", "No Reef"), values=c("green2", "darkorange4")) +
  labs(title="SR, %OM, Top (0-2cm), Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3))
#
# Bottom, compare Reef/No Reef #
#
TNC1b <- ggplot(data=SR_OM[SR_OM$top.bot=="Bot",], aes(x=position, y=X.om, color=in.out))
#[]inside square brackets always write row,column

TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef/No Reef", breaks=c("IN", "OUT"), labels=c("Reef", "No Reef"), values=c("green2", "darkorange4")) +
  labs(title="SR, %OM, Bottom (2-8cm), Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.2, 0.3))
#
# IN, compare Top/Bot #
#
TNC1b <- ggplot(data=SR_OM[SR_OM$in.out=="IN",], aes(x=position, y=X.om, color=top.bot))

TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="SR, %OM, Inside, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3))
#
# OUT, compare Top/Bot #
#
TNC1b <- ggplot(data=SR_OM[SR_OM$in.out=="OUT",], aes(x=position, y=X.om, color=top.bot))

TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="SR, %OM, Outside, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.2, 0.3))
#
#### SELECT REEFS ####
#
# Reef 1 IN #
#
TNC1b <- ggplot(data=SR_OM[SR_OM$in.out=="IN"&SR_OM$reef=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="SR, %OM, Inside, Reef 1") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3))
#
# Reef 1 OUT #
#
TNC1b <- ggplot(data=SR_OM[SR_OM$in.out=="OUT"&SR_OM$reef=="1",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="SR, %OM, Outside, Reef 1") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.2, 0.3))
#
#Reef 2 IN #
#
TNC1b <- ggplot(data=SR_OM[SR_OM$in.out=="IN"&SR_OM$reef=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="SR, %OM, Inside, Reef 2") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3))
#
#Reef 2 OUT #
#
TNC1b <- ggplot(data=SR_OM[SR_OM$in.out=="OUT"&SR_OM$reef=="2",], aes(x=position, y=X.om, color=top.bot))
###this just selects things from reef 1###
TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="SR, %OM, Outside, Reef 2") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3))
#
#
#### Facet plots - WSN plots ####
#
#Reef 1
#
TNC1b <- ggplot(data=SR_OM[SR_OM$reef=="1",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

TNC1b+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="SR, %OM, Reef 1") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above

#Reef 2

TNC1b <- ggplot(data=SR_OM[SR_OM$reef=="2",], aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

TNC1b+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="SR, %OM, Reef 2") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above

#Reef 1&2

TNC1b <- ggplot(data=SR_OM, aes(x=position, y=X.om, color=top.bot))

labeldf <- c("IN"="with reef influence", "OUT"="without reef influence")

TNC1b+
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1) +
  scale_color_manual(name="Core Section", breaks=c("Top", "Bot"), labels=c("Top", "Bottom"), values=c("deepskyblue", "navy")) +
  labs(title="SR, %OM, Reefs 1&2") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#OUT#
#
TNC1b <- ggplot(data=SR_OM[SR_OM$in.out=="OUT",], aes(x=position, y=X.om, color=top.bot))
#make "TNC1" using ggplot. selected data, use this stuff to plot. x=axis name, y=axis name, shape=data in the graph (can remove, do it below also), color=data in the graph
TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  coord_flip() +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("deepskyblue", "navy")) +
  labs(title="SR, %OM Outside Reefs 1&2") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.5, 4.5, 0.5), limits=c(2.5, 4.5))
#
#IN#
#
TNC1b <- ggplot(data=SR_OM[SR_OM$in.out=="IN",], aes(x=position, y=X.om, color=top.bot))
#make "TNC1" using ggplot. selected data, use this stuff to plot. x=axis name, y=axis name, shape=data in the graph (can remove, do it below also), color=data in the graph
TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  coord_flip() +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("deepskyblue", "navy")) +
  labs(title="SR, %OM, Inside Reefs 1&2") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position") +
  scale_y_continuous(name="%OM", breaks=seq(2.5, 4.5, 0.5), limits=c(2.5, 4.5))

#breaks=seq(2.8, 4.2, 0.3))
#sets the breaks in the continuous y-axis. seq(starting#, ending#, spacing)

#### LSP report ####

SR_OM$top.bot <- factor(SR_OM$top.bot, c("Top", "Bot")) 

TNC1b <- ggplot(data=SR_OM, aes(x=position, y=X.om, color=in.out))

labeldf <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")

TNC1b+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 1) +
  scale_color_manual(name="Reef Influence", breaks=c("IN", "OUT"), labels=c("With Reef", "Without Reef"), values=c("navy", "deepskyblue")) +
  labs(title="San Rafael, %OM, Both Reefs") +
  theme(axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey70"),
        plot.background = element_rect(color = "white")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(2.8, 4.4, 0.5), limits=c(2.8, 4.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### PERS/CAERS ####

#switch reef/no reef position - out first

SR_OM$in.out <- factor(SR_OM$in.out, c("OUT", "IN")) 

SROM <- ggplot(data=SR_OM, aes(x=position, y=X.om, color=in.out))

topbotlabel <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")

SROM+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 1) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  labs(title="San Rafael, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(2.8, 4.4, 0.5), limits=c(2.8, 4.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(topbotlabel))

#### NEW PLOT coloring specific point for reef 1/2 ####

SR_OM$in.out <- factor(SR_OM$in.out, c("OUT", "IN")) 

SROM <- ggplot(data=SR_OM, aes(x=position, y=X.om, color=in.out))

topbotlabel <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")

SROM+
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_point(position = position_dodge(0.7), shape = 1, size = 1) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  labs(title="San Rafael, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Reef Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(2.8, 4.4, 0.5), limits=c(2.8, 4.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(topbotlabel))

#### ANALYSIS ####

hist(SR_combine$X.om)
#data is normal
#gaussian

glm1 <- glm(X.om ~ position * in.out * top.bot, data = SR_OM, family = "gaussian")
# + = main effects
# : = interactive effects
# * = main AND interactive effects
plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#glmm
library(lme4)

glmm1 <- lmer(X.om ~ position * in.out * top.bot + (1|reef), data = SR_OM)
plot(glmm1)
summary(glmm1)
anova(glmm1)

# Ed Connor Analysis #

#glmmTMB

freef=as.factor(SR_combine$reef)
fcore=as.factor(SR_combine$core)

newdf=data.frame(SR_combine, freef, fcore)
head(newdf)

newdf=newdf[1:160,]
dim(newdf)

library(car)

#random intercepts model - reef and core within reef
model1 = glmmTMB(X.om/100 ~ in.out * position * top.bot + (1|freef) + (1|freef:fcore), data = newdf, family=beta_family(link="logit"))

model1

anova.model1 = Anova(model1)

anova.model1

#removing core
#random intercepts model - only reef - best model
model2 = glmmTMB(X.om/100 ~ in.out * position * top.bot + (1|freef), data = newdf, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#compare 1 & 2

anova(model1,model2)

#random intercepts model - only core within reef
model3 = glmmTMB(X.om/100 ~ in.out * position * top.bot + (1|freef:fcore), data = newdf, family=beta_family(link="logit"))

model3

anova.model3 = Anova(model3)

anova.model3

anova(model1, model2, model3)

#model2