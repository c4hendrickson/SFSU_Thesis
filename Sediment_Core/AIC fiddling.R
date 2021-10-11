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
# gives AIC
anova(glm1, test = "F")


#use this to get * above specific significant results
#removed top/bot because it's not significant
#categorical only

#anova1 <- aov(X.om ~ position * in.out * reef * year * top.bot, data = GM_combine)

# TOO MUCH
#TukeyHSD(anova1)
# DATA

#GLMM

glmm1 <- lmer(X.om ~ position * in.out * top.bot * year + (1|reef), data = GM_combine)
plot(glmm1)
summary(glmm1)
# doesn't give AIC
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
# gives AIC
anova(glm1, test = "F")

#GLMM

# year interactive effect, reef random effect
glmm1 <- lmer(X.om ~ position1 * in.out * top.bot * year + (1|reef), data = GM_combine)

plot(glmm1)
summary(glmm1)
# doesn't give AIC
anova(glmm1)
# includes all combinations

# year additive effect, reef random effect
glmm2 <- lmer(X.om ~ position1 * in.out * top.bot + year + (1|reef), data = GM_combine)

plot(glmm2)
summary(glmm2)
# doesn't give AIC
anova(glmm2)
# doesn't include year in interactions

AIC(glmm1, glmm2)

# Chelsey model selection ####
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

#

m1 <- lmer(X.om ~ position + in.out + top.bot + year + (1|reef), data = GM_combine)

m2 <- lmer(X.om ~ position + in.out + top.bot+ (1|reef), data = GM_combine)

summary (m1, m2)
#define list of models
models <- list(m1, m2)

#name list of models
modelnames <- list('m1', 'm2')

#calculate AIC of each model - NEED TO FIX

aictab(cand.set = models, modnames = modelnames)

# Karina method ####
AIC(m1, m2)

# LETS GET 3D #

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
