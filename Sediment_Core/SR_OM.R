# Sediment core data. 
# stuff to know! #
rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

# DATA
SR_combine <- read.csv("Sediment core data/SR_combine.csv")

# PACKAGES
library(ggplot2)
library(plotly)
library(dplyr)
library(plyr)
library(tidyr)
library(lme4)
library(nlme)
library(TMB)
library(glmmTMB)
library(Matrix)
library(car)

# SUMMARIZE DATA
SR_combine_sum <- SR_combine %>%
  select(reef, in.out, position, top.bot, X.om) %>%
  group_by(reef, in.out, position, top.bot) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

SR_combine_sum

# FACTORS
SR_combine$reef <- factor(SR_combine$reef, c("1", "2"))
SR_combine$in.out <- factor(SR_combine$in.out, c("OUT", "IN")) 
SR_combine$top.bot <- factor(SR_combine$top.bot, c("Top", "Bot"))
SR_combine$position <- factor(SR_combine$position, c("0", "12", "19", "27"))
#factor position as CATEGORIES with evenly spaced positions
#AND
#factor position as NUMERIC with middle of the reef as "0m"
SR_combine$position1 <- (as.numeric(as.character(SR_combine$position))-6)

# FACTORS for SUMMARY DATA
SR_combine_sum$reef <- factor(SR_combine_sum$reef, c("1", "2"))
SR_combine_sum$in.out <- factor(SR_combine_sum$in.out, c("OUT", "IN")) 
SR_combine_sum$top.bot <- factor(SR_combine_sum$top.bot, c("Top", "Bot"))
SR_combine_sum$position <- factor(SR_combine_sum$position, c("0", "12", "19", "27"))
#factor position as CATEGORIES with evenly spaced positions
#AND
#factor position as NUMERIC with middle of the reef as "0m"
SR_combine_sum$position1 <- (as.numeric(as.character(SR_combine_sum$position))-6)

#VIEW DATA
hist(SR_combine$X.om, breaks = 50)
#data is normal
#gaussian

# geom_boxplot
# upper/lower hinges = 1st and 3rd quartile (1st -> 3rd = IQR)
# upper/lower whisker = extends hinge -> last value not further than 1.5 * IQR
# outliers plotted individually

#### BOTH REEFS ####

# Top : With/Without Reef #
TNC1b <- ggplot(data=SR_combine[SR_combine$top.bot=="Top",], aes(x=position, y=X.om, color=in.out))
#[]inside square brackets always write row,column
#Factor not in data subset should be in other    (Top/Bottom)                    aes(color=in.out)

TNC1b +
  geom_boxplot(outlier.shape = NA) +
  #outlier.shape = NA gets rid of the outliers from the boxplot
  geom_jitter(position = position_dodge(0.7)) +
  # this adds all points onto boxplot, regardless of outlier
  scale_color_manual(name="Reef Presence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("green2", "darkorange4")) +
  labs(title="SR, %OM, Top (0-2cm), Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3), limits=c(2.8, 4.4))
  
# Bottom : With/Without Reef #
TNC1b <- ggplot(data=SR_combine[SR_combine$top.bot=="Bot",], aes(x=position, y=X.om, color=in.out))

TNC1b +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef Presence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("green2", "darkorange4")) +
  labs(title="SR, %OM, Bottom (2-8cm), Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3), limits=c(2.8, 4.4))

# With Reef : Top/Bot #

TNC1b <- ggplot(data=SR_combine[SR_combine$in.out=="IN",], aes(x=position, y=X.om, color=top.bot))

TNC1b +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="SR, %OM, With Reef, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3), limits=c(2.8, 4.4))

# Without Reef : Top/Bot #

TNC1b <- ggplot(data=SR_combine[SR_combine$in.out=="OUT",], aes(x=position, y=X.om, color=top.bot))

TNC1b +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Top/Bot", breaks=c("Top", "Bot"), labels=c("Top", "Bot"), values=c("tan1", "tan4")) +
  labs(title="SR, %OM, Without Reef, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3), limits=c(2.8, 4.4))

#### SELECT REEFS ####

# Reef 1 ####

# Top #

TNC1b <- ggplot(data=SR_combine[SR_combine$top.bot=="Top"&SR_combine$reef=="1",], aes(x=position, y=X.om, color=in.out))
#this just selects things from reef 1
TNC1b +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef Presence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("green2", "darkorange4")) +
  labs(title="SR, %OM, Top (0-2cm), Reef 1") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3), limits=c(2.8, 4.4))

# Bottom #

TNC1b <- ggplot(data=SR_combine[SR_combine$top.bot=="Bot"&SR_combine$reef=="1",], aes(x=position, y=X.om, color=in.out))

TNC1b +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef Presence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("green2", "darkorange4")) +
  labs(title="SR, %OM, Bottom (2-8cm), Reef 1") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3), limits=c(2.8, 4.4))

# Reef 2 ####

# Top #

TNC1b <- ggplot(data=SR_combine[SR_combine$top.bot=="Top"&SR_combine$reef=="2",], aes(x=position, y=X.om, color=in.out))

TNC1b +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef Presence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("green2", "darkorange4")) +
  labs(title="SR, %OM, Top (0-2cm), Reef 2") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3), limits=c(2.8, 4.4))

# Bottom #

TNC1b <- ggplot(data=SR_combine[SR_combine$top.bot=="Bot"&SR_combine$reef=="2",], aes(x=position, y=X.om, color=in.out))

TNC1b +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_dodge(0.7)) +
  scale_color_manual(name="Reef Presence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("green2", "darkorange4")) +
  labs(title="SR, %OM, Bottom (2-8cm), Reef 2") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.4, 0.3), limits=c(2.8, 4.4))

#### WSN plots - flip X axis ####

#Reef 1

TNC1b <- ggplot(data=SR_combine[SR_combine$reef=="1",], aes(x=position, y=X.om, color=top.bot))

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
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above

#Reef 2

TNC1b <- ggplot(data=SR_combine[SR_combine$reef=="2",], aes(x=position, y=X.om, color=top.bot))

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
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above

#Reef 1&2

TNC1b <- ggplot(data=SR_combine, aes(x=position, y=X.om, color=top.bot))

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
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.8, 4.3, 0.3)) +
  facet_wrap(~in.out, nrow=1, labeller = as_labeller(labeldf))
#labeling within the facet function. labeller changes the labels, using the dataframe created above
#
#OUT#
#
TNC1b <- ggplot(data=SR_combine[SR_combine$in.out=="OUT",], aes(x=position, y=X.om, color=top.bot))
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
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.5, 4.5, 0.5), limits=c(2.5, 4.5))
#
#IN#
#
TNC1b <- ggplot(data=SR_combine[SR_combine$in.out=="IN",], aes(x=position, y=X.om, color=top.bot))
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
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="%OM", breaks=seq(2.5, 4.5, 0.5), limits=c(2.5, 4.5))

#breaks=seq(2.8, 4.2, 0.3))
#sets the breaks in the continuous y-axis. seq(starting#, ending#, spacing)

#### LSP report ####

SR_combine$top.bot <- factor(SR_combine$top.bot, c("Top", "Bot")) 

TNC1b <- ggplot(data=SR_combine, aes(x=position, y=X.om, color=in.out))

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
  scale_x_discrete(name="Mudflat Position (m) (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(2.8, 4.4, 0.5), limits=c(2.8, 4.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(labeldf))

#### PERS/CAERS ####

#switch reef/no reef position - out first

SR_combine$reef <- factor(SR_combine$reef, c("1", "2"))
SR_combine$position <- factor(SR_combine$position, c("0", "12", "19", "27"))
SR_combine$top.bot <- factor(SR_combine$top.bot, c("Top", "Bot"))
SR_combine$in.out <- factor(SR_combine$in.out, c("OUT", "IN")) 

SROM <- ggplot(data=SR_combine, aes(x=position, y=X.om, color=in.out))

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
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(2.8, 4.4, 0.5), limits=c(2.8, 4.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(topbotlabel))

#### NEW PLOTS selecting shapes for specific points for reefs 1/2 ####

SR_combine$in.out <- factor(SR_combine$in.out, c("OUT", "IN"))
SR_combine$reef <- factor(SR_combine$reef, c("1", "2"))
SR_combine$position <- factor(SR_combine$position, c("0", "12", "19", "27"))
SR_combine$top.bot <- factor(SR_combine$top.bot, c("Top", "Bot"))
SR_combine$position1 <- (as.numeric(as.character(SR_combine$position))-6)

# scale_shape_manual(name="Reef Number", breaks=c("1", "2"), labels=c("Reef 1", "Reef 2"), values=c(1, 2)) #

# 2 sets of bars, 2 error bars # - hard to see everything

SROM <- ggplot(data=SR_combine, aes(x=position, y=X.om, color=in.out, shape=reef))

topbotlabel <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")

SROM+
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(0.7), size = 1) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  scale_shape_manual(name="Reef Location", breaks=c("1", "2"), labels=c("Reef 1 (south)", "Reef 2 (north)"), values=c(1, 2)) +
  labs(title="San Rafael, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(2.8, 4.4, 0.5), limits=c(2.8, 4.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(topbotlabel))

# geom_point(position = position_dodge(0.7), size = 1, aes(shape = reef)) #

# 2 sets of dots, 1 error bar # - lots of points, not dodged correctly

SROM <- ggplot(data=SR_combine, aes(x=position, y=X.om, color=in.out))

topbotlabel <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")

SROM+
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(0.7), size = 1, aes(shape = reef)) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  scale_shape_manual(name="Reef Location", breaks=c("1", "2"), labels=c("Reef 1 (south)", "Reef 2 (north)"), values=c(1, 2)) +
  labs(title="San Rafael, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(2.8, 4.4, 0.5), limits=c(2.8, 4.4)) +
  facet_wrap(~top.bot, nrow=2, labeller = as_labeller(topbotlabel))

# Plotting reef 1 reef 2 separately in same figure # - grid of plots, reef:layer

####WINNER WINNER CHICKEN DINNER####

SROM <- ggplot(data=SR_combine, aes(x=position, y=X.om, color=in.out))

topbotlabel <- c("Top"="Top Layer (0-2cm)", "Bot"="Bottom Layer (2-8cm)")
reeflabel <- c("1"="Reef 1 (south)", "2"="Reef 2 (north)")

SROM+
  geom_boxplot(aes(color=in.out), outlier.shape = NA) +
  geom_point(position = position_dodge(0.7), size = 1, aes(shape = reef)) +
  scale_color_manual(name="Reef Influence", breaks=c("OUT", "IN"), labels=c("Without Reef", "With Reef"), values=c("deepskyblue", "navy")) +
  scale_shape_manual(name="Reef Location", breaks=c("1", "2"), labels=c("Reef 1 (south)", "Reef 2 (north)"), values=c(1, 2)) +
  labs(title="San Rafael, Both Reefs") +
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0),
        axis.line = element_line(color="black", size=0.5, linetype="solid")) +
  scale_x_discrete(name="Mudflat Position (m)") +
  scale_y_continuous(name="% Organic Matter", breaks=seq(2.8, 4.4, 0.5), limits=c(2.8, 4.4)) +
  facet_grid(top.bot~reef, labeller = labeller(top.bot=topbotlabel, reef=reeflabel))