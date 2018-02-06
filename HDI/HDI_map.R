# these are packages you will need, but probably already have.
# Don't bother installing if you already have them
install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
install.packages(c('reshape2','geosphere','digest'))
# some standard map packages.
install.packages(c("maps", "mapdata"))
install.packages('mapdata')
install.packages('digest')
install.packages('data.table')
devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(colorspace)
library(scales)

#Load world map and check data
world <- map_data("world")
dim(world)
head(world)
tail(world)
#world <- world[world$region!='Antarctica',1:5] # remove Antarctica, drop irrelevant columns
world <-  subset(world, long < 180) #clip world map so within 180 degrees

#Load in HDI cell catch and HDI country list data for 2014
raw_data <- read.csv('_____.csv', header = TRUE)
names(raw_data) <- c('fishing_entity_id', 'sum', 'long', 'lat')
HDI <- read.csv('HDI.csv', header = TRUE) #read list of HDI 
#cells_coords <- data.frame(as.numeric(cells$ï..cell_id), cells$lon, cells$lat)
#Re-name HDI country list
names(HDI) <- c("country", "sau_eez", "hdi", 'fishing_entity_id', 'fishing_entity', 'hdi_level')
#Join raw_data with HDI tables based on fishing_entity_id
d <- merge(x = HDI, y = raw_data, by = "fishing_entity_id", all.y = TRUE)


###Low HDI
#Subset new merged table 'd' for Low HDI countries and exclude all countries where hdi = 0 (No HDI data)
dlow <- subset.data.frame(d, hdi_level == 'Low' & hdi != 0)
#convert dataframe to a data table to allow for grouping by multiple columns
dtlow <- data.table(dlow) 
#Group columns lon, lat and hdi_level by the log of catch sum. 'sum' with in sum() is column name for catch sum
dtlow1 <- dtlow[ ,list(catch = log(sum(sum/10)+1)), by = list(long, lat, hdi_level)]
#Graphing Low HDI
lmap <- ggplot( ) + 
  geom_point(data = dtlow1, mapping =  aes(x=long, y=lat, colour = catch), shape = 15, size = 1) + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = 'grey') + 
  coord_fixed(xlim=c(-165,165), ylim =c(-75,75), ratio = 1) +
  scale_color_gradient(low = 'white', high = 'red', breaks = c(0,3,6,9,12)) + 
  labs(title = 'Tiltle of chart', color = 'Log of catch', x = 'Longitude', y = 'Latitude') + 
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = NULL)) +
  scale_x_continuous(breaks=seq(-180,180,60)) + 
  scale_y_continuous(breaks = seq(-90,90,30))
lmap

###Medium HDI
#log of catch for Medium HDI
dmed <- subset.data.frame(d, hdi_level == 'Medium' & hdi != 0)
dtmed <- data.table(dmed)
dtmed1 <- dtmed[ ,list(hdi_level,catch = log(sum(sum/10)+1)), by = list(long, lat, hdi_level)]

#Graphing Medium HDI
mmap <- ggplot( ) +
  geom_point(data = dtmed1, mapping =  aes(x=long, y=lat, colour = catch), shape = 15, size = 1) + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = 'grey') + 
  coord_fixed(xlim=c(-165,165), ylim =c(-75,75), ratio = 1) +
  scale_color_gradient(low = 'white', high = 'red', breaks = c(0,3,6,9,12)) + 
  labs(title = 'Title of chart', color = 'Log of catch', x = 'Longitude', y = 'Latitude') + 
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = NULL)) +
  scale_x_continuous(breaks=seq(-180,180,60)) + 
  scale_y_continuous(breaks = seq(-90,90,30))
mmap

###High HDI
dhigh <- subset.data.frame(d, hdi_level == 'High' & hdi != 0)
dthigh <- data.table(dhigh)
#Group columns lon, lat and hdi_level by the log of catch sum. 'sum' with in sum() is column name for catch sum
dthigh1 <- dthigh[ ,list(hdi_level,catch = log(sum(sum/10)+1)), by = list(long, lat, hdi_level)]
#Graphing High HDI
hmap <- ggplot( ) + 
  geom_point(data = dthigh1, mapping =  aes(x=long, y=lat, colour = catch), shape = 15, size = 1) + 
  coord_fixed(xlim=c(-165,165), ylim =c(-75,75), ratio = 1) +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = 'grey') + 
  scale_color_gradient(low = 'white', high = 'red', breaks = c(0,3,6,9,12)) + 
  labs(title = 'Title of chart', color = 'Log of catch', x = 'Longitude', y = 'Latitude') +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = NULL)) +
  scale_x_continuous(breaks=seq(-180,180,60)) + 
  scale_y_continuous(breaks = seq(-90,90,30))
hmap