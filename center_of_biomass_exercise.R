# 2022-07-11
# Author - Jeewantha Bandara
# Email - jeewantha.bandara@rutgers.edu

# Here you will learn how to plot the center of biomass for a species over time
# The Center of Biomass is a useful way indicating trends of species movements
# This allows us to show where they are moving through time and the rate at which they are moving

# First we'll import the necessary packages to R required to visualize what we need
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(ggspatial)


# First we will read in some survey data and see what they look like
# Different federal and state agencies conduct annual trawl surveys throughout the Eastern Coast
# to gather data on the status of different species. Many of these species are important in either
# a commercial sense or biological sense

sp_data <- read.csv("data/summer_course_dataset.csv")

# Let's view this data
view(sp_data)

# Let's see what species is in this data
unique(sp_data$sppocean)

# This is your species. You can fill this out with a species of your liking!!!
sp_int <- "centropristis striata_Atl"

# Location to write out the images
# Automatically create a directory if it doesn't already exist
image_out <- paste("c_biomass_images_yearly/",sp_int,"/",sep="")

# Create the directory
if (dir.exists(image_out)){
  print("Directory exists!! Going to rewrite existing images")
} else{
  dir.create(image_out)
  print("Directory has been created!!!")
}

# Filter the dataset to get only observations for the species of our interest
# The '_Atl' part is for informative purpose to show that this is a species in the Atlantic Ocean
sp_sel <- sp_data[sp_data$sppocean==sp_int,]

# Let's see what sort of years we have data for this species
print(unique(sp_sel$year))

# Now to calculate the center of biomass through the years

# Get the number of years
years <- sort(unique(sp_sel$year))

# Create an empty dataframe for this
# Each dataframe gets a year, latitude, longitude, depth, and biomass
c_biomass_time <- data.frame(year=years, latitude=NA, longitude=NA, depth=NA, biomass=NA, obs=NA)

for (y in years){
  print(y)
  # Get the trawl surveys per year
  sp_obs <- sp_sel[sp_sel$year==y,]
  # Now calculate the means for latitude, longitude, depth, and sum of biomass for them
  c_biomass_time[c_biomass_time$year==y,]$latitude <- weighted.mean(sp_obs$lat, sp_obs$wtcpue)
  c_biomass_time[c_biomass_time$year==y,]$longitude <- weighted.mean(sp_obs$lon, sp_obs$wtcpue)
  c_biomass_time[c_biomass_time$year==y,]$depth <- weighted.mean(sp_obs$depth, sp_obs$wtcpue)
  c_biomass_time[c_biomass_time$year==y,]$biomass <- sum(sp_obs$wtcpue)
  c_biomass_time[c_biomass_time$year==y,]$obs <- length(sp_obs$year)
}

view(c_biomass_time)

# You can see that for some year we don't have enough observations (Less than 20 observations)
# We'll remove those years
c_biomass_time <- c_biomass_time[c_biomass_time$obs>20,]

# A quick intro to plotting maps with R
# We'll use a set of packages called 'tidyverse' that allows for making nice plots in R

# First let's get a map layer of the world!!!
world <- ne_countries(scale = "medium", returnclass = "sf")

theme_set(theme_bw())

# Bounding box coordinates -> (-84.9,23.39),(-69.01,42.87)
# This basically determines the boundaries of the map we are making
# We'll limit it to the east coasts
# You can use this website to make bounding boxes - https://boundingbox.klokantech.com/
lon_min = -84.9
lat_min = 23.39
lon_max = -69.01
lat_max = 42.87


# Now to create the actual plot
coast_plot <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  ylab("Latitude") + xlab("Longitude")

# Let's see the plot
coast_plot

# Now we'll make a series of plots and animate them
# First we need to set limits for the x and y axes
x_min <- min(sp_sel$lat)*.9
x_max <- max(sp_sel$lat)*1.1
y_min <- min(sp_sel$depth)*.9
y_max <- max(sp_sel$depth)*1.1

# Then set the text location
x_text <- x_max*.85
y_text <- y_max*.85

# Get the years
years <- sort(unique(c_biomass_time$year))

# Making a GIF
for (y in years){
  # Filter the dataset by the year
  c_biomass_y = c_biomass_time[c_biomass_time$year==y,]
  # Make the plot that shows the change of center of biomass through latitute and longitude
  plot_1 <- ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
    geom_point(data=c_biomass_y,aes(x=longitude,y=latitude),fill='white',size=3,pch=1) +
    ylab("Latitude") + xlab("Longitude")
  
  # Make the plot that shows the change of center of biomass through latitute and depth
  plot_2 <- ggplot(data=c_biomass_y, aes(x=latitude, y=depth)) + xlim(x_min,x_max) + ylim(y_min,y_max) +
    geom_point(size=3) +
    annotate("text", label = y, x = x_text, y = y_text, size = 8) +
    ylab("Depth (m)") + xlab("Latitude")
  
  png(paste(image_out,y,"_ver.png",sep=""), width=8, height=6, units="in", res=300)
  gridExtra::grid.arrange(plot_1, plot_2,
                          nrow=1, ncol=2)
  dev.off()
}


# Next step is to make a GIF from these
# You can use this website to make some animated GIFS - https://imgflip.com/gif-maker