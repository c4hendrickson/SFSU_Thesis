# Sediment core data. 
# stuff to know! #
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

# SUMMARIZE DATA

GM_GS_sum <- GM_GS %>%
  select(year, reef, in.out, position, top.bot, grain, percent) %>%
  group_by(year, reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

GM_GS_sum

GM_combine_sum <- GM_combine %>%
  select(year, reef, in.out, position, top.bot, grain, percent) %>%
  group_by(year, reef, in.out, position, top.bot, grain) %>%
  summarize_all(list(mean = mean, sd = sd)) %>% 
  mutate(se = sd/sqrt(5))

GM_combine_sum

# FACTORS

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

hist(GM_combine$X.om, breaks = 50)
#data is normal
#gaussian

#### FULL DATA SET ####
####Categorical####

#GLM

glm1 <- glm(X.om ~ position * in.out * top.bot * year, data = GM_combine, family = "gaussian")

visreg(glm1, "position", by="in.out", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#use this to get * above specific significant results
#removed top/bot because it's not significant
#categorical only

anova1 <- aov(X.om ~ position * in.out * reef * year * top.bot, data = GM_combine)

#TOO MUCH
TukeyHSD(anova1)
#DATA

#GLMM

glmm1 <- lmer(X.om ~ position * in.out * top.bot * year + (1|reef), data = GM_combine)
plot(glmm1)
summary(glmm1)
anova(glmm1)

####Numerical####

#adding possible squared term to see if linear model is a good fit - linear model fits

#GM_combine$position2 = (GM_combine$position1 + 6)
#GM_combine$positionsq = (GM_combine$position2 ^ 2)
#rm(GM_combine$position2)


#glm1 <- glm(X.om ~ position2 + in.out + top.bot + position2 : in.out + position2 : top.bot + in.out : top.bot + position2 : in.out : top.bot, data = GM_combine, family = "gaussian")

#plot(glm1)
#summary(glm1)
#anova(glm1, test = "F")

#visreg(glm1, "position2" , by="in.out", partial=TRUE, rug=FALSE)

#back to using regular linear model

#GLM

glm1 <- glm(X.om ~ position1 * in.out * top.bot * year, data = GM_combine, family = "gaussian")

visreg(glm1, "position1" , by="in.out", partial=TRUE, rug=FALSE)
visreg(glm1, "position1" , by="year", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * in.out * top.bot * year + (1|reef), data = GM_combine)

plot(glmm1)
summary(glmm1)
anova(glmm1)

glmm2 <- lmer(X.om ~ position1 * in.out * top.bot + year + (1|reef), data = GM_combine)

plot(glmm2)
summary(glmm2)
anova(glmm2)

AIC(glmm1, glmm2)

# Chelsey model selection #

install.packages("AICcmodavg")
library(AICcmodavg)
install.packages("plm")
library(plm) #for random factors

# ***+ - BEST???
m1 <- lmer(X.om ~ position1 * in.out * top.bot * year + (1|reef), data = GM_combine, REML = FALSE)
# **++
m2 <- lmer(X.om ~ position1 * in.out * top.bot + year + (1|reef), data = GM_combine, REML = FALSE)
# **+*
m3 <- lmer(X.om ~ position1 * in.out * top.bot + year * (1|reef), data = GM_combine, REML = FALSE)
# ++++
m4 <- lmer(X.om ~ position1 + in.out + top.bot + year + (1|reef), data = GM_combine, REML = FALSE)

summary (m1)
#define list of models
models <- list(m1, m2, m3, m4)

#name list of models
modelnames <- list('m1', 'm2', 'm3', 'm4')

#calculate AIC of each model - NEED TO FIX
#Error in `$<-.data.frame`(`*tmp*`, "K", 
#value = c(18, 11, 11, 7)) : 
#replacement has 4 rows, data has 1
aictab(cand.set = models, modnames = modelnames)

#Karina method
AIC(m1, m2, m3, m4)

#### SUBSET DATA ####

#### bayward positions - with reef/without reef ####

GM_combine_bay <- subset(GM_combine, position == 0)

hist(GM_combine_bay$X.om)
#data is normal
#gaussian

#ANOVA
# + = main effects
# : = interactive effects
# * = main AND interactive effects

#NUMERIC

#GLM
glm1 <- glm(X.om ~ position1 * in.out * top.bot, data = GM_combine_bay, family = "gaussian")

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * in.out * top.bot + (1|reef), data = GM_combine_bay)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#use this to get * above specific significant results
#removed top/bot because it's not significant

anova1 <- aov(X.om ~ in.out, data = GM_combine_bay)

TukeyHSD(anova1)

#### 1m from reef - with reef ####

GM_combine_reef_1m <- subset(GM_combine, in.out == "IN" & position1 <= 12)

hist(GM_combine_reef_1m$X.om)
#data is bimodal?! (gaussian?!)

GM_combine_reef_1m_bimodal <- subset(GM_combine_reef_1m, position == 0)
hist(GM_combine_reef_1m_bimodal$X.om)

GM_combine_reef_1m_bimodal <- subset(GM_combine_reef_1m, position == 12)
hist(GM_combine_reef_1m_bimodal$X.om)

#ANOVA
# + = main effects
# : = interactive effects
# * = main AND interactive effects

#CATEGORICAL

#GLM

glm1 <- glm(X.om ~ position * top.bot, data = GM_combine_reef_1m, family = "gaussian")

visreg(glm1, "top.bot" , by="position", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position * top.bot + (1|reef), data = GM_combine_reef_1m)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#use this to get * above specific significant results

anova1 <- aov(X.om ~ top.bot * position, data = GM_combine_reef_1m)

TukeyHSD(anova1)

#NUMERIC

#GLM

glm1 <- glm(X.om ~ position1 * top.bot, data = GM_combine_reef_1m, family = "gaussian")

visreg(glm1, "top.bot" , by="position1", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * top.bot + (1|reef), data = GM_combine_reef_1m)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#### shoreward positions - with reef/without reef ####

GM_combine_shore <- subset(GM_combine, position1 >= 0)

hist(GM_combine_shore$X.om)
#data is normal
#gaussian

#ANOVA
# + = main effects
# : = interactive effects
# * = main AND interactive effects

#CATEGORICAL

#GLM

glm1 <- glm(X.om ~ position * in.out * top.bot, data = GM_combine_shore, family = "gaussian")

visreg(glm1, "position" , by="in.out", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position * in.out * top.bot + (1|reef), data = GM_combine_shore)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#use this to get * above specific significant results
#removed top/bot because it's not significant

anova1 <- aov(X.om ~ position * in.out, data = GM_combine_shore)

TukeyHSD(anova1)

#NUMERIC

#GLM

glm1 <- glm(X.om ~ position1 * in.out * top.bot, data = GM_combine_shore, family = "gaussian")

visreg(glm1, "position1" , by="in.out", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * in.out * top.bot + (1|reef), data = GM_combine_shore)
plot(glmm1)
summary(glmm1)
anova(glmm1)

#### ED CONNOR ANALYSIS ####

#GLMMTMB - beta distributed data from 0-1

GM_combine$core <- factor(GM_combine$core, c("1", "2", "3", "4", "5"))

# CATEGORICAL #

#random intercepts model - reef and core within reef
model1 = glmmTMB(X.om/100 ~ position * in.out * top.bot + (1|reef) + (1|reef:core) + (1|year), data = GM_combine, family=beta_family(link="logit"))

model1

anova.model1 = Anova(model1)

anova.model1

#removing core
#random intercepts model - only reef - best model
model2 = glmmTMB(X.om/100 ~ position * in.out * top.bot + (1|reef) + (1|year), data = GM_combine, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#random intercepts model - only core within reef
model3 = glmmTMB(X.om/100 ~ position * in.out * top.bot + (1|reef:core) + (1|year), data = GM_combine, family=beta_family(link="logit"))

model3

anova.model3 = Anova(model3)

anova.model3

#compare all 3 models

anova(model1, model2, model3)

#model2 is best (lowest AIC & BIC)

# NUMERIC #

#random intercepts model - reef and core within reef
model1 = glmmTMB(X.om/100 ~ position1 * in.out * top.bot + (1|reef) + (1|reef:core) + (1|year), data = GM_combine, family=beta_family(link="logit"))

model1

anova.model1 = Anova(model1)

anova.model1

#removing core
#random intercepts model - only reef - best model
model2 = glmmTMB(X.om/100 ~ position1 * in.out * top.bot + (1|reef) + (1|year), data = GM_combine, family=beta_family(link="logit"))

model2

anova.model2 = Anova(model2)

anova.model2

#random intercepts model - only core within reef
model3 = glmmTMB(X.om/100 ~ position1 * in.out * top.bot + (1|reef:core) + (1|year), data = GM_combine, family=beta_family(link="logit"))

model3

anova.model3 = Anova(model3)

anova.model3

#compare all 3 models

anova(model1, model2, model3)

#model2 is best (lowest AIC & BIC)

# visualize numeric positions #
modelnew = glmmTMB(X.om ~ position1 * in.out * top.bot + (1|reef), data = GM_combine)
visreg(modelnew, "position1", by="in.out")
visreg(modelnew, "position1", by="in.out", scale="response", rug=FALSE, gg=TRUE)

#### FIX ME ####

#### ANALYSIS ####

hist(SR_GS$sand)
#data is normal
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
form.sand = sand.scale~ in.out*position*top.bot
form.silt = silt.scale~ in.out*position*top.bot
form.clay = clay.scale~ in.out*position*top.bot

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
  GM_combine,
  x = ~ sand,
  y = ~ silt,
  z = ~ clay,
  marker = list(size = 2),
  symbol = ~reef,
  symbols = c("circle", "square", "diamond"),
  colors = c('aquamarine4', 'salmon', "goldenrod1")
) %>%
  layout(title = "Grain Size Proportions (sand/silt/clay") %>%
  add_markers(color=~reef) %>%
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
  backgroundcolor="grey",
  gridcolor="black",
  showbackground=TRUE,
  zerolinecolor="black"
)

axz <- list(
  backgroundcolor="darkgrey",
  gridcolor="black",
  showbackground=TRUE,
  zerolinecolor="black"
)

figure3D %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

#### FIX ME TOO ####

# Sediment core data. 
# stuff to know! #
rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

GM_combine <- read.csv("Sediment core data/GM_combine.csv")

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

hist(GM_combine$X.om, breaks = 50)
#data is normal
#gaussian

#### FULL DATA SET ####
####Categorical####

#GLM

glm1 <- glm(X.om ~ position * in.out * top.bot * year, data = GM_combine, family = "gaussian")

visreg(glm1, "position", by="in.out", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#use this to get * above specific significant results
#removed top/bot because it's not significant
#categorical only

anova1 <- aov(X.om ~ position * in.out * reef * year * top.bot, data = GM_combine)

#TOO MUCH
TukeyHSD(anova1)
#DATA

#GLMM

glmm1 <- lmer(X.om ~ position * in.out * top.bot * year + (1|reef), data = GM_combine)
plot(glmm1)
summary(glmm1)
anova(glmm1)

####Numerical####

#adding possible squared term to see if linear model is a good fit - linear model fits

#GM_combine$position2 = (GM_combine$position1 + 6)
#GM_combine$positionsq = (GM_combine$position2 ^ 2)
#rm(GM_combine$position2)


#glm1 <- glm(X.om ~ position2 + in.out + top.bot + position2 : in.out + position2 : top.bot + in.out : top.bot + position2 : in.out : top.bot, data = GM_combine, family = "gaussian")

#plot(glm1)
#summary(glm1)
#anova(glm1, test = "F")

#visreg(glm1, "position2" , by="in.out", partial=TRUE, rug=FALSE)

#back to using regular linear model

#GLM

glm1 <- glm(X.om ~ position1 * in.out * top.bot * year, data = GM_combine, family = "gaussian")

visreg(glm1, "position1" , by="in.out", partial=TRUE, rug=FALSE)
visreg(glm1, "position1" , by="year", partial=TRUE, rug=FALSE)

plot(glm1)
summary(glm1)
anova(glm1, test = "F")

#GLMM

glmm1 <- lmer(X.om ~ position1 * in.out * top.bot * year + (1|reef), data = GM_combine)

plot(glmm1)
summary(glmm1)
anova(glmm1)

glmm2 <- lmer(X.om ~ position1 * in.out * top.bot + year + (1|reef), data = GM_combine)

plot(glmm2)
summary(glmm2)
anova(glmm2)

AIC(glmm1, glmm2)

# Chelsey model selection #
library(AICcmodavg)
library(plm) #for random factors

#Table 
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(insight)
library(tibble)
library(tidyverse)
library(ggpubr)
library(scales)
library(chron)
library(plotly)
library(taRifx)
library(aweek)
library(easypackages)
library(renv)
library(here)
library(ggthemes)
library(gridExtra)
library(patchwork)
library(tidyquant)
library(recipes)
library(cranlogs)
library(knitr)
library(openair)
library(xts)
library(dplyr)

# FIRST ATTEMPT #

# ***+ - BEST???
m1 <- lmer(X.om ~ position1 * in.out * top.bot * year + (1|reef), data = GM_combine, REML = FALSE)
# **++
m2 <- lmer(X.om ~ position1 * in.out * top.bot + year + (1|reef), data = GM_combine, REML = FALSE)
# **+*
m3 <- lmer(X.om ~ position1 * in.out * top.bot + year * (1|reef), data = GM_combine, REML = FALSE)
# ++++
m4 <- lmer(X.om ~ position1 + in.out + top.bot + year + (1|reef), data = GM_combine, REML = FALSE)

summary (m1)
#define list of models
models <- list(m1, m2, m3, m4)

#name list of models
modelnames <- list('m1', 'm2', 'm3', 'm4')

#calculate AIC of each model

aictab(cand.set = models, modnames = modelnames)

#Error in `$<-.data.frame`(`*tmp*`, "K", 
#value = c(18, 11, 11, 7)) : 
#replacement has 4 rows, data has 1

# NEW ATTEMPT - position instead of position1 #

m1 <- lmer(X.om ~ position * in.out * top.bot * year + (1|reef), data = GM_combine, REML = FALSE)
# **++
m2 <- lmer(X.om ~ position * in.out * top.bot + year + (1|reef), data = GM_combine, REML = FALSE)
# **+*
m3 <- lmer(X.om ~ position * in.out * top.bot + year * (1|reef), data = GM_combine, REML = FALSE)
# ++++
m4 <- lmer(X.om ~ position + in.out + top.bot + year + (1|reef), data = GM_combine, REML = FALSE)

summary (m1)
#define list of models
models <- list(m1, m2, m3, m4)

#name list of models
modelnames <- list('m1', 'm2', 'm3', 'm4')

#calculate AIC of each model - NEED TO FIX

aictab(cand.set = models, modnames = modelnames)

#Error in `$<-.data.frame`(`*tmp*`, "K", 
#value = c(34, 19, 19, 9)) : 
#replacement has 4 rows, data has 1
#new values for c() from above

# NEW ATTEMPT - lm instead of lmer? #

# ***+ - BEST???
m1 <- lm(X.om ~ position1 * in.out * top.bot * year + (1|reef), data = GM_combine, REML = FALSE)
# **++
m2 <- lm(X.om ~ position1 * in.out * top.bot + year + (1|reef), data = GM_combine, REML = FALSE)
# **+*
m3 <- lm(X.om ~ position1 * in.out * top.bot + year * (1|reef), data = GM_combine, REML = FALSE)
# ++++
m4 <- lm(X.om ~ position1 + in.out + top.bot + year + (1|reef), data = GM_combine, REML = FALSE)

summary (m1)
#define list of models
models <- list(m1, m2, m3, m4)

#name list of models
modelnames <- list('m1', 'm2', 'm3', 'm4')

#calculate AIC of each model - NEED TO FIX

aictab(cand.set = models, modnames = modelnames)

# NEW ATTEMPT - removing (1|reef) from models #
# by doing this, we need to switch to lm instead of lmer (also don't need REML)

m1 <- lm(X.om ~ position * in.out * top.bot * year, data = GM_combine, REML = FALSE)

m2 <- lm(X.om ~ position * in.out * top.bot + year, data = GM_combine, REML = FALSE)

m3 <- lm(X.om ~ position + in.out + top.bot + year, data = GM_combine, REML = FALSE)

summary (m1)
#define list of models
models <- list(m2)

#name list of models
modelnames <- list('m2')

#calculate AIC of each model - NEED TO FIX

aictab(cand.set = models, modnames = modelnames)

#Error in `$<-.data.frame`(`*tmp*`, "K", 
#value = c(33, 18, 8)) :     the values in c() are the degrees of freedom....
#replacement has 3 rows, data has 1
#replacement now has 3 instead of 4, so that's the number of models

#Karina method
AIC(m1, m2)

#### LETS GET 3D ####

figure3D <- plot_ly(
  GM_combine,
  x = ~ sand,
  y = ~ silt,
  z = ~ clay,
  marker = list(size = 3),
  symbol = ~reef,
  symbols = c("cross", "square", "circle-open"),
  colors = c('aquamarine4', 'goldenrod1', 'salmon')
) %>%
  layout(title = "Grain Size Proportions (sand/silt/clay") %>%
  add_markers(color=~reef) %>%
  layout(scene = list(
    xaxis = list(title = 'sand'),
    yaxis = list(title = 'silt'),
    zaxis = list(title = 'clay')
  ))

figure3D <- plot_ly(
  GM_combine,
  x = ~ sand,
  y = ~ silt,
  z = ~ clay,
  marker = list(size = 4),
  symbol = ~year,
  symbols = c("cross", "square-open"),
  colors = c('aquamarine4', 'goldenrod1')
) %>%
  layout(title = "Grain Size Proportions (sand/silt/clay") %>%
  add_markers(color=~year) %>%
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
