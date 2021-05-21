rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

setwd("/Users/carlhendrickson/Desktop/MY STUFF/R things/Heat Map")

#### NEW STUFF ####

library(ggplot2)
library(plotly)
library(dplyr)
library(plyr)
library(tidyr)

# set font for text
font <- list(
  family = "Courier, monospace",
  size = 14,
  color = "black")

# set x-axis attributes
x <- list(
  title = "Reef Position (m)",
  titlefont = font, 
  tick0 = 0.0, 
  dtick = 10.0,
  tickmode = "linear")

# set y-axis attributes
y <- list(
  title = "Distance from Reef (m)",
  titlefont = font, 
  tick0 = 0.0, 
  dtick = 3.0, 
  tickmode = "linear")

# TNC 1 winter #

# corrected data
TNCheatmap_1_corrected <- read.csv("Heat map data/TNCheatmap_1_corrected.csv")

# summarize means
TNCheatmap_1_sum <- TNCheatmap_1_corrected %>%
  select(distance_from_reef, row_ID, mass_lost) %>%
  group_by(distance_from_reef, row_ID) %>%
  summarize_all(list(mean = mean)) %>%
  mutate(mean = mean)

# plot #
TNChotnew1 <- plot_ly(data = TNCheatmap_1_sum, 
                      x = TNCheatmap_1_sum$row_ID, 
                      y = TNCheatmap_1_sum$distance_from_reef, 
                      z = TNCheatmap_1_sum$mean, 
                      type = "contour")

TNChotnew1 %>% 
  layout(title = 'San Rafael, winter 2019/2020', 
         xaxis = x,
         yaxis = y) %>%
  colorbar(title = "Mass 
lost (g)",
           titlefont = font)

# TNC 2 summer #

# corrected #
TNCheatmap_2_corrected <- read.csv("Heat map data/TNCheatmap_2_corrected.csv")
# removed #
TNCheatmap_2_corrected_removed <- TNCheatmap_2_corrected[-c(64),]

# summarize means
TNCheatmap_2_sum <- TNCheatmap_2_corrected %>%
  select(distance_from_reef, row_ID, mass_lost) %>%
  group_by(distance_from_reef, row_ID) %>%
  summarize_all(list(mean = mean)) %>%
  mutate(mean = mean)

# plot #
TNChotnew2 <- plot_ly(data = TNCheatmap_2_sum, 
                      x = TNCheatmap_2_sum$row_ID, 
                      y = TNCheatmap_2_sum$distance_from_reef, 
                      z = TNCheatmap_2_sum$mean, 
                      type = "contour")

TNChotnew2 %>% 
  layout(title = 'San Rafael, summer 2020', 
         xaxis = x,
         yaxis = y) %>%
  colorbar(title = "Mass 
lost (g)",
           titlefont = font)

# TRYING NEW STUFF - WORKS #

# set font for text
font <- list(
  family = "Courier, monospace",
  size = 14,
  color = "black")

# set x-axis attributes
x <- list(
  title = "Reef Position (m)",
  titlefont = font, 
  dtick = 10.0, 
  tick0 = 0.0, 
  tickmode = "linear")

# set y-axis attributes
y <- list(
  title = "Distance from Reef (m)",
  titlefont = font, 
  dtick = 3.0, 
  tick0 = 0.0, 
  tickmode = "linear")

TNChotnew1 %>% 
  layout(xaxis = x,
         yaxis = y) %>%
  colorbar(title = "Mass 
lost (g)",
           titlefont = font)

#### OSU STUFF ####

#Install required packages for spatial analysis
install.packages("ncf")
install.packages("spatial")
install.packages("spdep")
install.packages("sp")
install.packages("deldir")
install.packages("coda")
install.packages("maptools")
library (ncf)
library (spatial)
library (sp)
library (spdep)
#issue loading - 'no' to binary question
library (deldir)
#deldir 0.1-28
library (coda)
library (maptools)
#issue with licence

### First Deployment ###

# Removing Outliers #

TNCheatmap_1 <- read.csv("Heat map data/TNCheatmap_1.csv")
TNCheatmap_1_removed <- TNCheatmap_1[-c(49),]
#remove that stuff! [-c(get out!),]

#corrected data
TNCheatmap_1_corrected <- read.csv("Heat map data/TNCheatmap_1_corrected.csv")

hist (TNCheatmap_1_corrected$mass_lost) 
###NOT NEEDED log transformation###
#TNCheatmap$mass_lost <- log(TNCheatmap$mass_lost + 1)

###1. Testing for Autocorrelation###

#Run correlogram code (like pearsons r but for space). "resamp" is the number of times you are resampling the data to get the null distribution against which you test for significant autocorrelation. Increment is the spatial grain of the correlogram (distance in 'X' and 'Y' units you would like to run the test). z = your response variable (e.g., abundance of clover) get positive or negative correlation from +1 to -1, x axis = distance lags and time lags according to toblers law youd expect things closer together to be more related this is Moran's I in a correlogram
??correlog

Correl1 <- correlog(x=TNCheatmap_1_corrected$row_ID, y=TNCheatmap_1_corrected$distance_from_reef, z=TNCheatmap_1_corrected$mass_lost, resamp=500, increment=1) #asking if a neighbor is more likely to be correlated than a random point on the landscape
plot (Correl1)

Correl1

#moran fiddling - IGNORE
coords <- TNCheatmap_1_corrected$row_ID + TNCheatmap_1_corrected$distance_from_reef
coords

correlog(coords, z=TNCheatmap_1_corrected$mass_lost, method="Moran", nbclass=NULL)

#Note that the y-axix is Moran's I, which ranges from -1 to 1 and reflects the fine-scale spatial autocorrelation in the data. Filled circles indicate statisically significant autocorrelation. "Distance" are the spatial extents for which spatial autocorrelation was tested (these are called "spatial lags").

#You can look at the sample size (N) in each spatial lag, the spatial extent of each lag ("mean.of.class"), the lag-specific Moran's I ("correlation"), and the p-values ("p"):

#First, calculate the square of ‘y’ and ‘x’ (non-linear patterns of your response in relation to x or y), as well as product of ‘x’ and ‘y’ (). You are attributing these calculations to new variables called "x2", "y2", and "xy".

y2 <- TNCheatmap_1_corrected$distance_from_reef^2
x2 <- TNCheatmap_1_corrected$row_ID^2
xy <- TNCheatmap_1_corrected$row_ID * TNCheatmap_1_corrected$distance_from_reef

# Now test to see whether gradients are present in your data by running a generalized linear model

### Stats on Data 1 ###

#modeling abundance based on these variables 
Clov.glm <- glm(mass_lost ~ row_ID + distance_from_reef + y2 + x2 + xy, family = gaussian, data = TNCheatmap_1_corrected)

#Table showing parameter estimates, SEs and p-values:
summary (Clov.glm)
plot(Clov.glm)
anova (Clov.glm, test="Chi")
#Anova table showing deviance explained ("Deviance") by each variable. 
##dev = the amount of dev explained by that variable after accounting for the others 

#As shown in class, you need to alter the order of variables in the model with the variable of interest last (i.e., Type I Sums of Squares) to get a measure of variable importance (% independently explained deviance).

# Total explained deviance = (Dev_null - Dev_r)/ Dev_null
# Where Dev_null is the Resid.Deviance for the Null model (top row), and Dev_r is residual devience after all terms are in model Resid.Dev - bottom row

#Independently explained deviance =   (Devi  - Devj )/ DevN
#Where is DevN null deviance, Devi and Devj is the deviance of the second last and last terms in the model respectively.

#Deviance is available in the anova output (see above).

#"S" = scale. has no effect
#Clov.glm.S <- glm(mass_lost ~ scale(row_ID) + scale(distance_from_reef) + scale(y2) + scale(x2) + scale(xy), family = poisson, data = TNCheatmap_1_corrected)
#summary (Clov.glm.S)
#plot(Clov.glm.S)
#anova (Clov.glm.S, test="Chi")

### Plot ###

library (spatial)

poly2 <- surf.ls (2, TNCheatmap_1_corrected$row_ID, TNCheatmap_1_corrected$distance_from_reef, TNCheatmap_1_corrected$mass_lost)
###SECOND ORDER POLYNOMIAL###

# "trmat" evaluates a trend surface over a grid. 
# (x min, x max, y min ,y max)
# the last entry is number of neighbors considered. can change output

coords <- trmat (poly2, 0,55, 0.0,18.0, 10)
contour (coords) 
filled.contour(coords, 
               plot.title = title(main = "SR Wave Attenuation, Winter 2019-2020", xlab = "Reef Position (m)", ylab = "Distance from Reef (m)"),
               key.title = title(main = "
          Dissolution (g)"))

### Stats on Data 2 ###

###spatial regression 
##abundance at negiboring points ~ autocor1 + Hab1 = predictor for the abundance in the area (would change based on corelogram, i.e. lag 1 and 2...make autocoveriates for those, save the residuals and test for autocorrelation)
library (deldir)
library (coda)
library (maptools)
library (spdep)

#Calculate a spatial covariate for the spatial extent where you detected the most autocorrelation
# (For a detailed discussion of autocovariates see Betts et al. 2006 - Ecol. Mod., ). 
# You will add this autocovariate as a term in a regression and report the results (intercept, coefficients etc.). 

#generate a matrix of x,y coordinates
xy <- as.matrix(TNCheatmap_1_corrected[,1:2]) 
#Note that the term [,1:2] corresponds to the columns 1 and 2 that contain the x and y coordinates in your data. 
#Change to different #numbers to reflect the correct columns.

#Calculate autocovariate 
#This variable is a summary of the number of neighboring locations that contain individuals (and the abundance of those individuals)
#nbs is the neighborhood size (i.e., the spatial lag [i.e., distance] at which you want to account for spatial autocorrelation. 
# In this case, the 4th lag was used. For details see:

?autocov_dist

auto <- autocov_dist (TNCheatmap_1_corrected$mass_lost, xy, nbs=10, type="inverse", style="W")

#To model your response variable as a function of the autocovariate and other variables, e.g: 

test.glm <- glm (mass_lost ~ auto , data=TNCheatmap_1_corrected, family=gaussian)

summary (test.glm)
plot(test.glm)
anova (test.glm, test="Chi")

### Second Deployment ###

# Removing Outliers #

TNCheatmap_2 <- read.csv("Heat map data/TNCheatmap_2.csv")
TNCheatmap_2f <- TNCheatmap_2[-c(5,10,11,12,20,39,45,46,48,49,57,58,64,66,70,74,78,90),]
#remove that stuff! [-c(number),]
#using ee-5 as the limit

#corrected
TNCheatmap_2_corrected <- read.csv("Heat map data/TNCheatmap_2_corrected.csv")
TNCheatmap_2_corrected_removed <- TNCheatmap_2_corrected[-c(64),]
  
hist (TNCheatmap_2_corrected_removed$mass_lost) 
###NOT NEEDED log transformation###
#TNCheatmap$mass_lost <- log(TNCheatmap$mass_lost + 1)
#hist (TNCheatmap$mass_lost)
###1. Testing for Autocorrelation###

#Run correlogram code (like pearsons r but for space). "resamp" is the number of times you are resampling the data to get the null distribution against which you test for significant autocorrelation. Increment is the spatial grain of the correlogram (distance in 'X' and 'Y' units you would like to run the test). z = your response variable (e.g., abundance of clover) get positive or negative correlation from +1 to -1, x axis = distance lags and time lags according to toblers law youd expect things closer together to be more related this is Moran's I in a correlogram
??correlog

Correl1 <- correlog(x=TNCheatmap_2_corrected_removed$row_ID, y=TNCheatmap_2_corrected_removed$distance_from_reef, z=TNCheatmap_2_corrected_removed$mass_lost, resamp=500, increment=1) #asking if a neighbor is more likely to be correlated than a random point on the landscape
plot (Correl1)

Correl1

#moran fiddling - IGNORE
coords <- TNCheatmap_2_corrected_removed$row_ID + TNCheatmap_2_corrected_removed$distance_from_reef
coords

correlog(coords, z=TNCheatmap_2_corrected_removed$mass_lost, method="Moran", nbclass=NULL)

#Note that the y-axix is Moran's I, which ranges from -1 to 1 and reflects the fine-scale spatial autocorrelation in the data. Filled circles indicate statisically significant autocorrelation. "Distance" are the spatial extents for which spatial autocorrelation was tested (these are called "spatial lags").

#You can look at the sample size (N) in each spatial lag, the spatial extent of each lag ("mean.of.class"), the lag-specific Moran's I ("correlation"), and the p-values ("p"):

#First, calculate the square of ‘y’ and ‘x’ (non-linear patterns of your response in relation to x or y), as well as product of ‘x’ and ‘y’ (). You are attributing these calculations to new variables called "x2", "y2", and "xy".

y2 <- TNCheatmap_2_corrected_removed$distance_from_reef^2
x2 <- TNCheatmap_2_corrected_removed$row_ID^2
xy <- TNCheatmap_2_corrected_removed$row_ID * TNCheatmap_2_corrected_removed$distance_from_reef

# Now test to see whether gradients are present in your data by running a generalized linear model

### Stats on Data 1 ###

#modeling abundance based on these variables 
Clov.glm <- glm(mass_lost ~ row_ID + distance_from_reef + y2 + x2 + xy, family = gaussian, data = TNCheatmap_2_corrected_removed)

#Table showing parameter estimates, SEs and p-values:
summary (Clov.glm)
plot(Clov.glm)
anova (Clov.glm, test="Chi")
#Anova table showing deviance explained ("Deviance") by each variable. 
##dev = the amount of dev explained by that variable after accounting for the others 

#As shown in class, you need to alter the order of variables in the model with the variable of interest last (i.e., Type I Sums of Squares) to get a measure of variable importance (% independently explained deviance).

# Total explained deviance = (Dev_null - Dev_r)/ Dev_null
# Where Dev_null is the Resid.Deviance for the Null model (top row), and Dev_r is residual devience after all terms are in model Resid.Dev - bottom row

#Independently explained deviance =   (Devi  - Devj )/ DevN
#Where is DevN null deviance, Devi and Devj is the deviance of the second last and last terms in the model respectively.

#Deviance is available in the anova output (see above).

#"S" = scale. has no effect
#Clov.glm.S <- glm(mass_lost ~ scale(row_ID) + scale(distance_from_reef) + scale(y2) + scale(x2) + scale(xy), family = poisson, data = TNCheatmap_1_corrected)
#summary (Clov.glm.S)
#plot(Clov.glm.S)
#anova (Clov.glm.S, test="Chi")

### Plot ###

library (spatial)

poly2 <- surf.ls (2, TNCheatmap_2_corrected_removed$row_ID, TNCheatmap_2_corrected_removed$distance_from_reef, TNCheatmap_2_corrected_removed$mass_lost)
###SECOND ORDER POLYNOMIAL###

# "trmat" evaluates a trend surface over a grid. 
# (x min, x max, y min ,y max)
# the last entry is number of neighbors considered. can change output

coords <- trmat (poly2, 0,55, 0.0,18.0, 10)
contour (coords) 
filled.contour(coords, 
               plot.title = title(main = "SR Wave Attenuation, Summer 2020", xlab = "Reef Position (m)", ylab = "Distance from Reef (m)"),
               key.title = title(main = "
          Dissolution (g)"))

### Stats on Data 2 ###

###spatial regression 
##abundance at negiboring points ~ autocor1 + Hab1 = predictor for the abundance in the area (would change based on corelogram, i.e. lag 1 and 2...make autocoveriates for those, save the residuals and test for autocorrelation)
library (deldir)
library (coda)
library (maptools)
library (spdep)

#Calculate a spatial covariate for the spatial extent where you detected the most autocorrelation
# (For a detailed discussion of autocovariates see Betts et al. 2006 - Ecol. Mod., ). 
# You will add this autocovariate as a term in a regression and report the results (intercept, coefficients etc.). 

#generate a matrix of x,y coordinates
xy <- as.matrix(TNCheatmap_2_corrected_removed[,1:2]) 
#Note that the term [,1:2] corresponds to the columns 1 and 2 that contain the x and y coordinates in your data. 
#Change to different #numbers to reflect the correct columns.

#Calculate autocovariate 
#This variable is a summary of the number of neighboring locations that contain individuals (and the abundance of those individuals)
#nbs is the neighborhood size (i.e., the spatial lag [i.e., distance] at which you want to account for spatial autocorrelation. 
# In this case, the 4th lag was used. For details see:

?autocov_dist

auto <- autocov_dist (TNCheatmap_2_corrected_removed$mass_lost, xy, nbs=10, type="inverse", style="W")

#To model your response variable as a function of the autocovariate and other variables, e.g: 

test.glm <- glm (mass_lost ~ auto , data=TNCheatmap_2_corrected_removed, family=gaussian)

summary (test.glm)
plot(test.glm)
anova (test.glm, test="Chi")

#### WSN Plots ####

coords <- trmat (poly2, 0,55, 0.0,18.0, 10)
contour (coords) 
filled.contour(coords, 
               plot.title = title(ylab = "Distance from Reef (m)"),
               key.title = title(main = "
          
           Dissolution (g)"))
