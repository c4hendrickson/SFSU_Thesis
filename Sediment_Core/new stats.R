rm(list=ls()) #remove all objects

# mcglm - cannot download #

#from zip
install.packages("/Users/carlhendrickson/Desktop/MY STUFF/R things/Sediment Cores/mcglm_0.6.0.tar.gz", lib.lock="/Library/Frameworks/R.framework/Resources/library", repos=NULL, dependencies=TRUE)

#from file out of zip
install.packages("/Users/carlhendrickson/Desktop/MY STUFF/R things/Sediment Cores/mcglm", lib.lock="/Library/Frameworks/R.framework/Resources/library", dependencies=TRUE)

install.packages("mcglm")
library(mcglm)

#from github
library(devtools)
install_github("wbonat/mcglm")
remotes::install_github("wbonat/mcglm")

library(mcglm)

#last method
install.packages("mcglm_0.6.0.tar.gz", repos = NULL,
                 lib.loc = "/Library/Frameworks/R.framework/Resources/library",
                 dependencies = TRUE)

#archived cran version
devtools::install_version("mcglm", "0.6.0")

#install downloaded version
install.packages('/Users/carlhendrickson/Desktop/MY STUFF/R things/Sediment Cores/mcglm_0.6.0.tar.gz', repos = NULL, type = 'source')

#margot
library(devtools)
install_github("wbonat/mcglm")

# MCMCglmm - stuck on priors #

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

SR_combine_scale = SR_combine_scale[1:160,]

#old prior attempt
prior = list(R = list (V = diag(3), nu = 0, fix = 3), G = list(G1 = list(V = diag(3), nu = 1)))

#new prior attempt - changed V = diag(3)
prior = list(R = list (V = diag(3), fix=1), G = list (G1 = list(V = diag(3), nu=3)))
#working now that [nu=3]

#tweaking priors
prior = list(
  R = list (V = diag(3), nu=3), 
  G = list (G1 = list(V = diag(3), nu=3)))
#R list - according to MCMCglmm methods, feb 10, 2010 - this is the correct 'R list'
#G list - 

#my attempt
m1 <- MCMCglmm(fixed = cbind(sand.scale, silt.scale, clay.scale) ~ trait:in.out + trait:position + trait:top.bot - 1, random = ~ us(trait):reef, rcov = ~ us(trait):units, prior=prior, family = c("gaussian", "gaussian", "gaussian"), nitt = 60000, burnin = 10000, thin = 25, data = SR_combine_scale, pr = TRUE)

m1

summary(m1)

#when running m1, save what the prior settings are for the model, save the model as well. then save the output of summary(m1)

plot(m1)

plot(m1, random=TRUE)

anova(m1)
#doesn't work