rm(list=ls()) #remove all objects
rm() #remove one object
getwd() #get working directory
setwd() #set working directory
list(ls) #list all objects

setwd("/Users/carlhendrickson/Desktop/MY STUFF/R things/Heat Map")

library(tidyverse)
library(RColorBrewer)
library(scales)
library(openair)

# DATA #

# SR plaster ~~~

SR_plaster_wind_inst1 <- read.csv("Heat map data/SR_plaster_wind_inst1.csv")
# wind_speed [0-17]
SR_plaster_wind_inst2 <- read.csv("Heat map data/SR_plaster_wind_inst2.csv")
# wind_speed [0-18]

# SR sediment ~~~

SR_sediment_wind_inst <- read.csv("Heat map data/SR_sediment_wind_inst.csv")
# wind_speed [0-19]

# GM plaster ~~~

GM_plaster_wind_inst1 <- read.csv("Heat map data/GM_plaster_wind_inst1.csv")
# wind_speed [1.1-9.5]
GM_plaster_wind_inst2 <- read.csv("Heat map data/GM_plaster_wind_inst2.csv")
# wind_speed [1-9.3]

# GM sediment ~~~

GM_sediment_wind_inst1 <- read.csv("Heat map data/GM_sediment_wind_inst1.csv")
# wind_speed [1-12.8]
GM_sediment_wind_inst2 <- read.csv("Heat map data/GM_sediment_wind_inst2.csv")
# wind_speed [1.1-13.4]

# check min and max wind_speed in dataframe
min(GM_sediment_wind_inst2$wind_speed)
max(GM_sediment_wind_inst2$wind_speed)

# find NAs in dataframe
which(is.na(GM_sediment_wind_inst2), arr.ind=TRUE)

# remove certain values from dataframe
SR_plaster_wind_inst1_0 <- subset(SR_plaster_wind_inst1, wind_speed != 0)

#

#### github ####
# (https://github.com/tomhopper/windrose)

# uses openair package, windRose function

library(devtools)
install_github("tomhopper/windrose")

load(file = "Heat map data/wind_data.rda") # load .rda file
wind_data2 <- subset(wind_data, select = -DateTime) # removing DataTime column - DOESN'T HELP

data(wind_data) # doesn't work....
wind_rose <- windRose(wind_data, spd = Wind_Speed_meter_per_second, dir = Wind_Direction_deg)
# Error in `[[<-.data.frame`(`*tmp*`, vars[i], value = numeric(0)) : 
# replacement has 0 rows, data has 90815
plot(wind_rose) # can't find wind_rose from previous error

#### stack overflow ####

# (https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r/17266781#17266781)

# next version post

# test data

#datatest <- structure(list(date = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "1/1/2013", class = "factor"), hr = 1:9, spd = c(5, 7, 7, 51.9, 11, 12, 9, 11, 17), dir = c(30, 30, 30, 180, 180, 180, 269, 270, 271)), .Names = c("date", "hr", "spd", "dir" ), row.names = c(NA, -9L), class = "data.frame")

# WindRose.R

plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 22.5,
                          spdmin = 1,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")
    
  }  
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned
                           ,y = (..count..)/sum(..count..)
                       ))+
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","NNE","NE","ENE", "E", 
                                "ESE", "SE","SSE", 
                                "S","SSW", "SW","WSW", "W", 
                                "WNW","NW","NNW")) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) + #I can put in my own theme settings
    scale_y_continuous(labels = percent) + # these are lined up with North bar, showing percent of time in a bin
    ylab("Frequency")
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}

plot.windrose(data = SR_plaster_wind_inst1_0,
              spd = SR_plaster_wind_inst1_0$wind_speed,
              dir = SR_plaster_wind_inst1_0$wind_direction,
              spdmin = 1,
              spdmax = 20,
              spdres = 2,
              dirres = 22.5,
              palette = "YlGnBu")

              # spdseq = NULL,
              # countmax = NA, # THESE GET LEFT OUT
              # debug = 0)

# Why are there so many NAs for the inst data?