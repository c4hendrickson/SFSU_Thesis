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
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(nlme)
library(glmmTMB)
library(TMB)
library(Matrix)
library(car)
library(visreg)

# SUMMARIZE DATA #

SR_GS_sum <- SR_GS %>%
  select(reef, in.out, position, top.bot, grain, percent) %>%
  group_by(reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

SR_GS_sum

# FACTORS
SR_combine$reef <- factor(SR_combine$reef, c("1", "2"))
SR_combine$in.out <- factor(SR_combine$in.out, c("IN", "OUT")) 
SR_combine$top.bot <- factor(SR_combine$top.bot, c("Top", "Bot"))

#position

#factor as CATEGORIES with evenly spaced positions
SR_combine$position <- factor(SR_combine$position, c("0", "12", "19", "27"))

#AND

#factor as NUMERIC with middle of the reef as "0m"
SR_combine$position1 <- (as.numeric(as.character(SR_combine$position))-6)
#SR_combine$position1 <- (as.numeric(as.character(SR_combine$position)))

# VIEW DATA
hist(SR_combine$X.om, breaks = 50)
#data is normal
#gaussian

#### FULL DATA SET ####

#ANOVA
# + = main effects
# : = interactive effects
# * = main AND interactive effects

####Categorical####

#GLM

glm1 <- glm(X.om ~ position * in.out * top.bot, data = SR_combine, family = "gaussian")

visreg(glm1, "position" , by="in.out", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#use this to get * above specific significant results
#removed top/bot because it's not significant
#categorical only

anova1 <- aov(X.om ~ position * in.out, data = SR_combine)

TukeyHSD(anova1)

#GLMM

glmm1 <- lmer(X.om ~ position * in.out * top.bot + (1|reef), data = SR_combine)
plot(glmm1)
summary(glmm1)
anova(glmm1)

####Numerical####

#adding possible squared term to see if linear model is a good fit - linear model fits

#SR_combine$position2 = (SR_combine$position1 + 6)
#SR_combine$positionsq = (SR_combine$position2 ^ 2)
#rm(SR_combine$position2)


#glm1 <- glm(X.om ~ position2 + in.out + top.bot + position2 : in.out + position2 : top.bot + in.out : top.bot + position2 : in.out : top.bot, data = SR_combine, family = "gaussian")

#plot(glm1)
#summary(glm1)
#anova(glm1, test = "F")

#visreg(glm1, "position2" , by="in.out", partial=TRUE, rug=FALSE)

#back to using regular linear model

#GLM

glm1 <- glm(X.om ~ position1 * in.out * top.bot, data = SR_combine, family = "gaussian")

visreg(glm1, "position1" , by="in.out", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * in.out * top.bot + (1|reef), data = SR_combine)

visreg(glmm1, "position1" , by="in.out", partial=TRUE, rug=FALSE)

plot(glmm1)
summary(glmm1)
anova(glmm1)

#### SUBSET DATA ####

#### bayward positions - with reef/without reef ####

SR_combine_bay <- subset(SR_combine, position == 0)

hist(SR_combine_bay$X.om)
#data is normal
#gaussian

#NUMERIC

#GLM
glm1 <- glm(X.om ~ position1 * in.out * top.bot, data = SR_combine_bay, family = "gaussian")

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * in.out * top.bot + (1|reef), data = SR_combine_bay)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#use this to get * above specific significant results
#removed top/bot because it's not significant

anova1 <- aov(X.om ~ in.out, data = SR_combine_bay)

TukeyHSD(anova1)

#### 1m from reef - with reef ####

SR_combine_reef_1m <- subset(SR_combine, in.out == "IN" & position1 <= 12)

hist(SR_combine_reef_1m$X.om)
#data is bimodal?! (gaussian?!)

SR_combine_reef_1m_bimodal <- subset(SR_combine_reef_1m, position == 0)
hist(SR_combine_reef_1m_bimodal$X.om)

SR_combine_reef_1m_bimodal <- subset(SR_combine_reef_1m, position == 12)
hist(SR_combine_reef_1m_bimodal$X.om)

#NUMERIC

#GLM

glm1 <- glm(X.om ~ position1 * top.bot, data = SR_combine_reef_1m, family = "gaussian")

visreg(glm1, "top.bot" , by="position1", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * top.bot + (1|reef), data = SR_combine_reef_1m)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#### shoreward positions - with reef/without reef ####

SR_combine_shore <- subset(SR_combine, position1 >= 0)

hist(SR_combine_shore$X.om)
#data is normal
#gaussian

#CATEGORICAL

#GLM

glm1 <- glm(X.om ~ position * in.out * top.bot, data = SR_combine_shore, family = "gaussian")

visreg(glm1, "position" , by="in.out", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position * in.out * top.bot + (1|reef), data = SR_combine_shore)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#use this to get * above specific significant results
#removed top/bot because it's not significant

anova1 <- aov(X.om ~ position * in.out, data = SR_combine_shore)

TukeyHSD(anova1)

#NUMERIC

#GLM

glm1 <- glm(X.om ~ position1 * in.out * top.bot, data = SR_combine_shore, family = "gaussian")

visreg(glm1, "position1" , by="in.out", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * in.out * top.bot + (1|reef), data = SR_combine_shore)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#### ED CONNOR ANALYSIS ####

#GLMMTMB - beta distributed data from 0-1

SR_combine$core <- factor(SR_combine$core, c("1", "2", "3", "4", "5"))

#### Categorical ####

#random intercepts model - reef and core within reef
model1 = glmmTMB(X.om/100 ~ position * in.out * top.bot + (1|reef) + (1|reef:core), data = SR_combine, family=beta_family(link="logit"))

model1

anova.model1 = Anova(model1)

anova.model1

#removing core
#random intercepts model - only reef - best model
model2 = glmmTMB(X.om/100 ~ position * in.out * top.bot + (1|reef), data = SR_combine, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#random intercepts model - only core within reef
model3 = glmmTMB(X.om/100 ~ position * in.out * top.bot + (1|reef:core), data = SR_combine, family=beta_family(link="logit"))

model3

anova.model3 = Anova(model3)

anova.model3

#compare all 3 models

anova(model1, model2, model3)

#model2 is best (lowest AIC & BIC)

# WITH SUBSET DATA #

SR_combine_bay <- subset(SR_combine, position == 0)
SR_combine_reef_1m <- subset(SR_combine, in.out == "IN" & position <= 12)
SR_combine_shore <- subset(SR_combine, position >= 0)

model2 = glmmTMB(X.om/100 ~ position * in.out * top.bot + (1|reef), data = SR_combine, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#### Numeric ####

#random intercepts model - reef and core within reef
model1 = glmmTMB(X.om/100 ~ position1 * in.out * top.bot + (1|reef) + (1|reef:core), data = SR_combine, family=beta_family(link="logit"))

model1

anova.model1 = Anova(model1)

anova.model1

#removing core
#random intercepts model - only reef - best model
model2 = glmmTMB(X.om/100 ~ position1 * in.out * top.bot + (1|reef), data = SR_combine, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#random intercepts model - only core within reef
model3 = glmmTMB(X.om/100 ~ position1 * in.out * top.bot + (1|reef:core), data = SR_combine, family=beta_family(link="logit"))

model3

anova.model3 = Anova(model3)

anova.model3

#compare all 3 models

anova(model1, model2, model3)

#model2 is best (lowest AIC & BIC)

# WITH SUBSET DATA #

SR_combine_bay <- subset(SR_combine, position1 == 0)
SR_combine_reef_1m <- subset(SR_combine, in.out == "IN" & position1 <= 12)
SR_combine_shore <- subset(SR_combine, position1 >= 0)

model2 = glmmTMB(X.om/100 ~ position1 * in.out * top.bot + (1|reef), data = SR_combine_shore, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

# visualize numeric positions #
modelnew = glmmTMB(X.om ~ position1 * in.out * top.bot + (1|reef), data = SR_combine)
visreg(modelnew, "position1", by="in.out")
visreg(modelnew, "position1", by="in.out", scale="response", rug=FALSE, gg=TRUE)


#### FIX ME ####

#### GRAIN SIZE ANALYSIS ####

hist(SR_GS$silt)
#data is NOT normal for sand
#gaussian

# uses SR_GS, which has all of the data repeated 3X so that the grain and percent are specified in new columns

# splitting grain parts

# SAND SR_GS[1:160,]
# SILT SR_GS[161:320,]
# CLAY SR_GS[321:480,]

SR_GS_grain=SR_GS[1:160,]

glm1 <- glm(percent ~ position * in.out * top.bot, data = SR_GS_grain, family = "gaussian")
# + = main effects
# : = interactive effects
# * = main AND interactive effects
plot(glm1)
summary(glm1)
anova(glm1, test = "F")

# uses SR_combine where column 'sand' or 'silt' or 'clay' contains the percent

glm2 <- glm(sand ~ position * in.out * top.bot, data = SR_combine, family = "gaussian")
plot(glm2)
summary(glm2)
anova(glm2, test = "F")

#for % data, logit transform - logit(df$colname)

#glmm
library(lme4)
install.packages("scales")
library(scales)
install.packages("reshape2")
library(reshape2)

logit(SR_combine$clay)
#not important?

#glmm
library(lme4)

glmm1 <- lmer(sand ~ position * in.out * top.bot + (1|reef), data = SR_GS_grain)
summary(glmm1)
plot(glmm1)
anova(glmm1)

# Ed Connor analysis #
#### glmmTMB ####

freef=as.factor(SR_combine$reef)
fcore=as.factor(SR_combine$core)

newdf=data.frame(SR_combine, freef, fcore)
head(newdf)

library(car)

#random intercepts model
model1 = glmmTMB(clay/100 ~ in.out * position * top.bot + (1|freef) + (1|freef:fcore), data = newdf, family=beta_family(link="logit"))

model1

anova.model1 = Anova(model1)

anova.model1

#removing core
#random intercepts model - best model
model2 = glmmTMB(clay/100 ~ in.out * position * top.bot + (1|freef), data = newdf, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#compare 1 & 2

anova(model1,model2)

#random intercepts model
model3 = glmmTMB(clay/100 ~ in.out * position * top.bot + (1|freef:fcore), data = newdf, family=beta_family(link="logit"))

model3

anova.model3 = Anova(model3)

anova.model3

anova(model1, model2, model3)


#### MCMCglmm ####

install.packages("MCMCglmm")
library(MCMCglmm)

data("BTdata")

# example
m1 <- MCMCglmm(
  + fixed = cbind(tarsus, back) ~ trait:sex + trait:hatchdate - 1,
  + random = ~ us(trait):animal + us(trait):fosternest,
  + rcov = ~ us(trait):units, prior = prior,
  + family = c("gaussian", "gaussian"), nitt = 60000, burnin = 10000,
  + thin = 25, data = BTdata, pedigree = BTped)

# my data
SR_combine <- read.csv("Sediment core data/SR_combine.csv")

silt.scale = scale(SR_combine$silt.mass, center = TRUE, scale = TRUE)
sand.scale = scale(SR_combine$sand.mass, center = TRUE, scale = TRUE)
clay.scale = scale(SR_combine$clay.mass, center = TRUE, scale = TRUE)

SR_combine_scale <- data.frame(SR_combine, silt.scale, sand.scale, clay.scale)

#old prior attempt
prior = list(R = list (V = diag(3), nu = 0, fix = 3), G = list(G1 = list(V = diag(3), nu = 1)))

#new prior attempt - changed V = diag(3)
prior = list(R = list (V = diag(3), fix=1), G = list (G1 = list(V = diag(3), nu=0.5)))

#my attempt
m1 <- MCMCglmm(fixed = cbind(sand.scale, silt.scale, clay.scale) ~ trait:in.out + trait:position + trait:top.bot - 1, random = ~ us(trait):reef, rcov = ~ us(trait):units, prior=prior, family = c("gaussian", "gaussian", "gaussian"), nitt = 60000, burnin = 10000, thin = 25, data = SR_combine_scale, pedigree = NULL)

#### MCGLM ####

install.packages("mcglm")
library(mcglm)

dat=read.csv("C:/Users/900007191/Downloads/SR_combine.csv", header=TRUE)


# center and scale responses and offset 
sand.scale=scale(dat$sand.mass,center=TRUE,scale=TRUE)
silt.scale=scale(dat$silt.mass,center=TRUE,scale=TRUE)
clay.scale=scale(dat$clay.mass,center=TRUE,scale=TRUE)
total.scale=scale(dat$total.mass,center=TRUE,scale=TRUE)

dat_scale=data.frame(dat,total.scale,sand.scale,silt.scale,clay.scale)

# use 1/3 of data file so no duplication
dat_scale2=dat_scale[1:160,]

# formulas for linear predictor
form.sand = sand.scale ~ in.out * position * top.bot
form.silt = silt.scale ~ in.out * position * top.bot
form.clay = clay.scale ~ in.out * position * top.bot

# matrix predictors 
z_0=mc_id(dat_scale2)
z_1=mc_mixed(~0 + reef, data=dat_scale2)

# not used
#fit.sand = mcglm(linear_pred = c(form.sand), matrix_pred = list(c(z_0,z_1)),data = dat)

#fit.silt = mcglm(linear_pred = c(form.silt), matrix_pred = list(c(z_0,z_1)),data = dat)

#fit.clay = mcglm(linear_pred = c(form.clay), matrix_pred = list(c(z_0,z_1)),data = dat)


# fit model with scaled data and offset, random effect on reef
fit_1=mcglm(linear_pred=c(form.sand,form.silt,form.clay),matrix_pred=list(c(z_0,z_1),c(z_0,z_1),c(z_0,z_1)),control_algorithm = list(max_iter = 200, verbose = FALSE), offset=list(dat_scale2$total.scale,dat_scale2$total.scale,dat_scale2$total.scale),data=dat_scale2)


summary(fit_1)
anova(fit_1)


#### LETS GET 3D ####

figure3D <- plot_ly(
  SR_combine[SR_combine$in.out=="IN",],
  x = ~ sand,
  y = ~ silt,
  z = ~ clay,
  marker = list(size = 4),
  symbol = ~position,
  symbols = c('circle-open', 'square-open', 'circle', 'square'),
  colors = c('aquamarine4', 'salmon', 'green', 'white')
) %>%
  layout(title = "Grain Size Proportions (sand/silt/clay)") %>%
  add_markers(color=~position) %>%
  layout(scene = list(
    xaxis = list(title = 'sand'),
    yaxis = list(title = 'silt'),
    zaxis = list(title = 'clay')
  ))

axx <- list(
  backgroundcolor="lightgrey",
  gridcolor="black",
  showbackground=TRUE,
  zerolinecolor="black"
)

axy <- list(
  backgroundcolor="lightgrey",
  gridcolor="black",
  showbackground=TRUE,
  zerolinecolor="black"
)

axz <- list(
  backgroundcolor="lightgrey",
  gridcolor="black",
  showbackground=TRUE,
  zerolinecolor="black"
)

figure3D %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
