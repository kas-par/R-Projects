
# Header----#############################################################################

# Install required packages
# install.packages("ggmap")
# install.packages("raster")
# install.packages("rgdal")
# install.packages("gridExtra")
# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("readr")
# install.packages("gender")

library(tidyverse)
library(ggmap) # to get coordinates from a address
library(ggplot2)
register_google(key = "") # the service is free but requires email registration and a API
library(rgdal)
library(dplyr)
library(raster) # for shapefile
library(RSelenium) # Web Scraping
library(rvest) # # Web Scraping
library(readr)
library(gender)

rm(list = ls())

#########################################################################################

# downlaod and first cleaning of inspection data----#####################################

# Initially download and save the file
#inspect_data <- read.csv("https://data.ny.gov/api/views/d6dy-3h7r/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)
#save(inspect_data, file = "./data/inspect_data_original.RData")

# Load original file previously downloaded
load("./data/inspect_data_original.RData")

inspect_data <- as_tibble(inspect_data)

# Inspection grade as numbers
# Required because we calculate averages later on which is not possible with Factors
mapping <- c("A" = 1, "B" = 2, "C" = 3)
inspect_data <- inspect_data %>%
  mutate(Inspection.Grade = mapping[Inspection.Grade])

rm(mapping)

inspect_data = inspect_data %>% 
  mutate(Inspection.Date = as.Date(Inspection.Date, format = "%m/%d/%Y")) %>% #convert values to dates for calculation
  filter(Trade.Name != "") %>% #drops 7obs with missing trade name
  arrange(Inspection.Date) #sort by date

#if the same shop has several inspections - keep only the first
inspect_data = inspect_data %>%
  group_by(Trade.Name) %>% 
  summarise_all(funs(first)) %>% #<----- check here Warning message: funs() is soft deprecated as of dplyr 0.8.0
  ungroup

# finds all shop chains
inspect_data_chains = inspect_data %>% 
  group_by(Owner.Name) %>%  #group by owner to see who owns more than one company
  dplyr::summarise(count = n())

inspect_data_chains$chain[which(inspect_data_chains$count == 1)] = 0 #gives every owner the value 0 if only one shop owned
inspect_data_chains$chain[which(inspect_data_chains$count >= 2)] = 1 #gives value 1 if > 1 shop owned

#merges the data
inspect_data = inspect_data %>%
  inner_join(inspect_data_chains, by = "Owner.Name")

rm(inspect_data_chains)

save(inspect_data, file = "./data/inspect_data.RData")

#########################################################################################

# Add Coordinates of shops----###########################################################

# Function extracts the coordinates as two vectors from Column 12
coord <- function(string_vector){
  # splits character at "("
  string_vector <- strsplit(string_vector, "\\(")
  # keeps 2nd split
  string_vector <- sapply(string_vector, "[", 2)
  # splits character at ","
  string_vector <- strsplit(string_vector, "\\,")
  # keeps 1st split
  latitude <- sapply(string_vector, "[", 1)
  latitude <- as.numeric(latitude)
  # keeps 2nd element of previous split
  string_vector <- sapply(string_vector, "[", 2)
  # splits string at ")"
  string_vector <- strsplit(string_vector, "\\)")
  # keeps 1st split
  longitude <- sapply(string_vector, "[", 1)
  longitude <- as.numeric(longitude)
  location <- cbind(longitude, latitude)
  return(location)
}

# replace location by Longitude and Latitude in the df
inspect_data <- inspect_data %>%
  mutate(Longitude = coord(Location)[,1],
         Latitude = coord(Location)[,2]) %>%
  dplyr::select(-Location)

rm(coord)

# 748 coordinates are missing
table(is.na(inspect_data$Longitude))

# create address column
inspect_data <- inspect_data %>%
  mutate(Address = str_c(Street, Zip.Code, sep = ", ")) %>%
  mutate(Address = str_c(Address, City, sep = " ")) %>%
  mutate(Address = str_c(Address, State.Code, sep = ", "))

# use Google maps to get missing coordiantes (takes few minutes an requires API in the head)
inspect_data_na <- inspect_data %>%
  filter(is.na(Latitude)) %>% # all missing coordinates
  mutate_geocode(Address) %>% # applies Google Maps API
  mutate(Latitude = lat, Longitude = lon) %>%
  dplyr::select(-c(lat, lon)) %>%
  filter(!is.na(Latitude)) # 248 still missing and dropped

# add new coordinates
inspect_data <- inspect_data %>%
  filter(!is.na(Latitude)) %>%
  bind_rows(inspect_data_na)

table(is.na(inspect_data$Longitude)) # no more coordinates with NA

rm(inspect_data_na)

save(inspect_data, file = "./data/inspect_data.RData")

#########################################################################################

# Spatial Data----#######################################################################

# Haversine Formula
# (calculates the distance of two points on the earth surface)
haversine <- function(lat1, lon1, lat2, lon2){
  # from coordinate (degree) to radians
  φ1 <- (lat1 * pi) / (180)
  φ2 <- (lat2 * pi) / (180)
  Δφ <- ((lat2 - lat1) * pi) / (180)
  Δλ <- ((lon2 - lon1) * pi) / (180)
  R = 6371000 # eath's radius in metres
  a = sin(Δφ/2)^2 + cos(φ1) *cos(φ2) *sin(Δλ/2)^2
  c = 2*atan2(sqrt(a), sqrt(1-a))
  d = R*c
  return(d)
}

# the n closest obs. to coordniates in df with rows latitude and longitude
n_closest <- function(df, n, lat, lon){
  # distance to all points
  dist_vect <- haversine(lat, lon, as.matrix(df$Latitude), as.matrix(df$Longitude))
  # n closest points
  dist_sort <- sort(dist_vect, decreasing = FALSE)[1:n]
  # n closest obs.
  rows <- which(dist_vect %in% dist_sort)
  inspections_sub <- df[rows, ]
  return(inspections_sub)
}

# get shop density and rating of closest shop
rating_closest_neighb <- c()
shop_density <- c()
for (i in 1:nrow(inspect_data)){
  lat = as.numeric(inspect_data$Latitude[i])
  lon = as.numeric(inspect_data$Longitude[i])
  # all data except the i-th obs.
  inspect_data_sub <- inspect_data %>%
    slice(-i)
  # distance to i-th obs.
  distances <- haversine(lat, lon, inspect_data_sub$Latitude, inspect_data_sub$Longitude)
  # density of shops in 1km distance
  distances <- length(which(distances < 1000))
  shop_density <- c(shop_density, distances)
  # get the grade of the closest shop
  inspect_data_sub <- n_closest(inspect_data_sub, 1, lat, lon)
  inspect_grade <- inspect_data_sub$Inspection.Grade
  # rounded mean from multiple shops with same closest distance
  inspect_grade <- round(mean(inspect_grade))
  rating_closest_neighb <- c(rating_closest_neighb, inspect_grade)
}

# append the df
inspect_data <- inspect_data %>%
  mutate(shop_density = shop_density,
         rating_closest_neighb = rating_closest_neighb)

rm(distances, i, inspect_grade, lat, lon, rating_closest_neighb, shop_density, n_closest, inspect_data_sub)

save(inspect_data, file = "./data/inspect_data.RData")
#########################################################################################

# Web Scraping for Google Ratings----####################################################

# start the Selenium server
rD <- rsDriver(verbose = FALSE, browser = "firefox")

#set the xPaths
xPath = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div[1]/div/div[1]/div/div[4]/div/div/span[1]"
xPath2 = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div[1]/div/div[1]/div/div[1]/div[2]/div[2]/div[2]/div/div/span[1]"
x_Path_rev_number = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div[1]/div/div[1]/div/div[4]/div/div/span[2]/span/a/span"
x_Path_rev_number2 = "/html/body/div[6]/div[3]/div[10]/div[1]/div[3]/div/div[1]/div/div[1]/div/div[1]/div[2]/div[2]/div[2]/div/div/span[2]/span/a/span"

# assign the client to a new variable, visit a webpage
myclient <- rD$client

#prepare data for scraper
scraping_parameter = inspect_data %>%
  dplyr::select(Trade.Name, City)
scraping_parameter = unite(scraping_parameter, "searching", Trade.Name:City, remove = FALSE, sep = " ")
scraping_p_vector = as.factor(pull(scraping_parameter, searching))

results_scraping = select(scraping_parameter, -searching)
results_scraping = results_scraping %>%
  mutate(Reviews = rep(1, length(scraping_p_vector)), Number_of_Reviews = rep(1, length(scraping_p_vector)))

#defining "a" for filtering purpose in for-loop
a <- character(0)

#scraping loop
for (i in 1:length(scraping_p_vector)) {
  print(i)
  myclient$navigate("http://www.google.com/ncr")
  Sys.sleep(1)
  webElem <- myclient$findElement('xpath', "//input[@name='q']") #select typing element
  
  webElem$sendKeysToElement(list(scraping_p_vector[i], key = "enter")) #enter the respective adress and trade name
  Sys.sleep(2)
  html_doc = read_html(myclient$getPageSource()[[1]]) #download the page content
  link_nodes = html_nodes(html_doc, xpath = xPath)
  link_nodes_number = html_nodes(html_doc, xpath = x_Path_rev_number)
  link_nodes2 = html_nodes(html_doc, xpath = xPath2)
  link_nodes_number2 = html_nodes(html_doc, xpath = x_Path_rev_number2)
  
  if (!identical(a, html_text(html_nodes(link_nodes2, xpath = xPath2)))){
    
    results_scraping[i,3] = html_text(html_nodes(link_nodes2, xpath = xPath2)) #filter the review data out of the html document
    results_scraping[i,4] = html_text(html_nodes(link_nodes_number2, xpath = x_Path_rev_number2))
    Sys.sleep(1)
    print("review found")
    
  } else if (!identical(a, html_text(html_nodes(link_nodes, xpath = xPath)))) {
    
    results_scraping[i,3] = html_text(html_nodes(link_nodes, xpath = xPath)) #filter the review data out of the html document
    results_scraping[i,4] = html_text(html_nodes(link_nodes_number, xpath = x_Path_rev_number))
    Sys.sleep(1)
    print("review found")
    
    
  } else {
    results_scraping[i,3] = 0
    results_scraping[i,4] = 0
    Sys.sleep(1)
    
  }
}

#Cleaning the scraped data
#removes "google reviews" string +  converts to numeric
results_scraping[,4] = as.numeric(str_remove(results_scraping[,4], "Google review" ))
results_scraping[,3] = as.numeric(results_scraping[,3])
results_scraping$Number_of_Reviews = substr(results_scraping$Number_of_Reviews,1,nchar(results_scraping$Number_of_Reviews)-14)
results_scraping$Number_of_Reviews[which(results_scraping$Number_of_Reviews == "")] = 0
results_scraping$Number_of_Reviews = as.numeric(str_trim(str_remove_all(results_scraping$Number_of_Reviews, "’")))

#Saving the Review-Data in csv
write.csv(results_scraping, file="data/results_scraping_final.csv")

# close the Selenium server
myclient$close()
rD$server$stop()

#########################################################################################

# Add Google Ratings----#################################################################

# Load Google ratings resulted from web scraping
load("data/results_scraping_final.csv")
google_ratings <-  data
google_ratings <- google_ratings %>%
  dplyr::select(-X)

# Merge data with ratings via Trade.Name
# (Trade.Name is by construction unique)
inspect_data <-  inspect_data %>%
  inner_join(google_ratings, by = "Trade.Name")

# remove unneeded columns
inspect_data <- inspect_data %>%
  dplyr::rename(City = City.x)  %>%
  dplyr::select(-City.y)

rm(google_ratings, data)

save(inspect_data, file = "./data/inspect_data.RData")

#########################################################################################

# Filter for New York City----###########################################################

# Next we reduce our data set to the neighbourhoods of NY City.
# We work only with NYC data because data for more variables is available
# and the data is less imbalanced.
# The following codes, therefore, only deal with NYC data

ny_counties <-  c("New York", "Kings", "Bronx", "Richmond", "Queens") # NYC counties
ny_inspect_data <- inspect_data[which(inspect_data$County %in% ny_counties),]
rm(ny_counties)

save(ny_inspect_data, file = "./data/ny_inspect_data.RData")

#########################################################################################

# Prepare Demographic Data----###########################################################

# Start with raw inspection data
load("./data/inspect_data_original.RData")
inspections <- inspect_data
inspections <- as_tibble(inspections)
inspectionsSave <- inspections
x <- length(unique(inspections$County))

# Experimenting with dem. Datasets & modifying them
nycdBlockLoc <- read.csv("./data/nycd_census_block_loc.csv")
nycdBlockLoc <- as_tibble(nycdBlockLoc)
nycdCen <- read.csv("./data/nycd_nyc_census_tracts.csv")
nycdCen <- as_tibble(nycdCen)

# 2017 Cesuns data on census tract level
# taken from: https://www.kaggle.com/muonneutrino/us-census-demographic-data
us17Cen <- read_csv("./data/K_us-census-demographic-data/acs2017_census_tract_data.csv")
us17Cen <- as_tibble(us17Cen)
ny17Cen <- filter(us17Cen, State == "New York")
#not necessary to add second category, since dataset strictly selected in either male or female
ny17Cen <- mutate(ny17Cen, percentageFemale = (TotalPop - Men)/TotalPop)

# 2017 Cesuns data on county level
# taken from: https://www.kaggle.com/muonneutrino/us-census-demographic-data
us17county <- read.csv("./data/K_us-census-demographic-data/acs2017_county_data.csv")
us17county <- as_tibble(us17county)
ny17county <- filter(us17county, State == "New York")
#not necessary to add second category, since dataset strictly selected in either male or female
ny17county <- mutate(ny17county,percentageFemale = (TotalPop - Men)/TotalPop)

ny17countyNames <- names(ny17county)
ny17countyNames <- paste(ny17countyNames, "per County")
ny17county <- setNames(ny17county, ny17countyNames)

#Merging inspections with ny17county
class(inspections)==class(ny17county)
class(ny17county$`County per County`)
ny17county[,"County per County"] <- str_remove(ny17county$`County per County`, " County")
inspectionsCounty <- merge(inspections, ny17county, by.x = "County", by.y = "County per County")

# There is no column so far to merge our data with ny17Cen
# To get one, we first create a third dataframe AddTrac, 
# that contains the data of both the Addresses as well as the corresponding Census Tract IDs
# getting Data from official geocoding services
# https://geocoding.geo.census.gov/geocoder/geographies/addressbatch?form

geocoderIn <- inspections[ ,c("Street", "City", "State.Code", "Zip.Code")]
geocoderIn1 <- geocoderIn[1:9000, ]
write.csv(geocoderIn1, "geocoderIn1.csv")
geocoderIn2 <- geocoderIn[9001:18999, ]
write.csv(geocoderIn2, "geocoderIn2.csv")
geocoderIn3 <- geocoderIn[19000:nrow(geocoderIn), ]
write.csv(geocoderIn3, "geocoderIn3.csv")
geocoderInTest <- geocoderIn[1:10, ]
write.csv(geocoderInTest, "geocoderInTest.csv")


geoOut1 <- read.csv("./data/geoOut1.csv", skip = 1)
geoOut2 <- read.csv("./data/geoOut2.csv", skip = 1)
geoOut3 <- read.csv("./data/geoOut3.csv", skip = 1)
line1geo <- as.vector(names(geoOut1))
line2geo <- names(geoOut2)
line3geo <- names(geoOut3)
names(geoOut1) <- c(1:ncol(geoOut1))
names(geoOut2) <- c(1:ncol(geoOut2))
names(geoOut3) <- c(1:ncol(geoOut3))
AddTrac <- rbind(geoOut1, geoOut2,geoOut3)
CC <- complete.cases(AddTrac$`10`)
AddTrac <- AddTrac[CC, ]

#Since the TractId geocode contains placeholder zeros, 
#which are not displayed in the batch goecoding output, those zeros have to be added, to make merging possible
f1 = function(x){
  if(nchar(x) == 1){paste("00", x, sep = "")}
  else if (nchar(x) == 2){paste("0", x, sep = "")}
  else {paste(x, "", sep = "")}
}

f2 = function(x){
  if(nchar(x) == 3){paste("000", x, sep = "")}
  else if (nchar(x) == 4){paste( "00",x, sep = "")}
  else if (nchar(x) == 5){paste( "0",x, sep = "")}
  else {paste(x, "", sep = "")}
}

AddTracTest <- AddTrac[1:100,]
fT <- function(x){paste(x, "0", sep = "")}
test <- lapply(AddTracTest$`10`, fT)   
AddTracTest$`10` <- lapply(AddTracTest$`10`, f1)
AddTrac$`10` <- lapply(AddTrac$`10`, f1)
AddTrac$`11` <- lapply(AddTrac$`11`, f2)


AddTrac <- unite(AddTrac, TractId, c("9","10","11"), sep = "")
colnames(AddTrac)[1:2] <- c("Numbers","Address")
AddTrac <- AddTrac[,c("Address","TractId")]
AddTrac <- AddTrac %>% distinct(Address, .keep_all = TRUE)

# Match AddTrac with Inspections to have the TractId numbers included in the Inspections Dataframe
inspections <- inspectionsSave
inspections$County <- toupper(inspections$County)

inspections <- unite(inspections, Address , c(Street, City, State.Code, Zip.Code), sep = ", ", remove = FALSE)
inspectionsTrac <- merge(inspections, AddTrac, by = "Address")

#Match InspectionsTrac with democraphic Data by Census data per census tract
inspectionsCen <- merge(inspectionsTrac, ny17Cen, by.x = "TractId", by.y = "TractId per CenTrac")

# Match all demographic data
ny17county$`County per County` <- toupper(ny17county$`County per County`)
inspectionsDem <- merge(inspectionsCen, ny17county, by.x = "County", by.y = "County per County")

# Export data
write.csv(inspectionsDem, file = gzfile("./data/inspectionsDem.cvs.gz"))


#########################################################################################

# Add Demographic Data----###############################################################

demographic_data <- read.csv("./data/inspectionsDem.cvs.gz")
# demographic data with unique addresses
demographic_data <- demographic_data %>%
  distinct(Address, .keep_all = TRUE)
# delete variables that are not used for merging, but are still present in both dataframes, so they dont show up twice, afterwardsded
demographic_data <- demographic_data %>%
  dplyr::select(-c("County", 
             "Inspection.Grade" , 
             "Inspection.Date", 
             "Owner.Name" , 
             "Trade.Name", 
             "Street", 
             "City", 
             "State.Code", 
             "Zip.Code", 
             "Deficiency.Number", 
             "Deficiency.Description"))
ny_inspect_data <- unite(ny_inspect_data, Address , c(Street, City, State.Code, Zip.Code), sep = ", ", remove = FALSE)
#merge
ny_inspect_dem <- merge(ny_inspect_data, demographic_data, by = "Address")
ny_inspect_dem <- ny_inspect_dem[complete.cases(ny_inspect_dem),]
ny_inspect_data <- ny_inspect_dem

rm(ny_inspect_dem, demographic_data)

save(ny_inspect_data, file = "./data/ny_inspect_data.RData")

#########################################################################################

# Add Airbnb Data----###################################################################

data_bnb <- read.csv("data/ab_nyc_19.csv") #reading data

longitude = as.numeric(data_bnb$longitude)
latitude = as.numeric(data_bnb$latitude)
Coordinates  = tibble(longitude, latitude)
s = shapefile("./data/geolocation/ZIP_CODE_040114.shp") #read in the shapefile which contains the geo data of the different 
pts <- Coordinates                                    #ZIP Code regions in new york
pts <- pts[complete.cases(pts),]
coordinates(pts) <- ~longitude+latitude               #transformation package specific
proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
pts <- spTransform(pts, proj4string(s))

# this does the lon/lat to zip mapping
zip_where <- pts %over% s
data_bnb = data_bnb %>%  #attach the ZIP codes to the airbnb data frame by inner_join of longitude and latitude
  mutate(ZIP = zip_where$ZIPCODE) %>% 
  inner_join(Coordinates, data_bnb, by = c("longitude", "latitude")) %>% 
  dplyr::select(ZIP, price, neighbourhood_group)

summary = data_bnb %>%  #summary of the different ZIP codes in order to calculate the average room price for each ZIP code
  group_by(ZIP) %>% 
  dplyr::summarise(count = n(), mean = mean(price))

data_bnb = inner_join(data_bnb, summary, by = "ZIP") #join the new average room price data with the aribnb data frame
data_bnb$ZIP = as.numeric(data_bnb$ZIP) #convert new ZIP codes to numceric

#tidying the data_bnb dataframe and renaming the new attached parameters
data_bnb = data_bnb %>% 
  distinct(ZIP, .keep_all = TRUE) %>% 
  dplyr::select(-price) %>% 
  dplyr::rename(Zip.Code = ZIP, Numb_Rooms = count, Avr_Price = mean) 

ny_inspect_data = inner_join(ny_inspect_data, data_bnb, by = "Zip.Code") #join the airbnb data frame with the original data frame by zip code

rm(Coordinates, data_bnb, pts, s, summary, zip_where, latitude, longitude)

save(ny_inspect_data, file = "./data/ny_inspect_data.RData")

#########################################################################################

# NYC Subway locations----###############################################################

# Initially download and save the file
#subway_data <- read.csv("https://data.ny.gov/api/views/i9wp-a4ja/rows.csv?accessType=DOWNLOAD&sorting=true", stringsAsFactors = FALSE)
#save(subway_data, file = "./data/subway_data.RData")

# Load the previously saved subway locations
load("./data/subway_data.RData")

subway_data <- as_tibble(subway_data)

# Keep only unique stations
subway_data <- subway_data %>%
  distinct(Station.Name, .keep_all = TRUE) %>%
  dplyr::rename(Latitude = Station.Latitude,
         Longitude = Station.Longitude)

# Distances in meter to next subway station
subway_distance <- c()
for (i in 1:nrow(ny_inspect_data)){
  # coordinates of i-th obs.
  lat <-  as.numeric(ny_inspect_data$Latitude[i])
  lon <-  as.numeric(ny_inspect_data$Longitude[i])
  # distance of i-th point to all subway stations
  distances <- haversine(lat, lon, subway_data$Latitude, subway_data$Longitude)
  # shortest distance
  distances <- min(distances)
  subway_distance <-  c(subway_distance, distances)
}

ny_inspect_data <- ny_inspect_data %>%
  mutate(subway_distance = subway_distance)

rm(i, lat, lon, haversine, subway_distance, distances)

save(ny_inspect_data, file = "./data/ny_inspect_data.RData")

#########################################################################################

# Map Plots of State Data and NY City Data----###########################################

# Plot1: Map of NY State with Shop Locations
# Retrieves State Map (Takes a while)
map_ny_state <- get_stamenmap(bbox = c(left = -79.7517, bottom = 40.5092, right = -71.9069, top = 44.9967),
                            zoom = 11,
                            maptype ='terrain',
                            color = 'color',
                            scale = 4)
# Plot of NY State
Plot1 <- ggmap(map_ny_state) +
  geom_point(aes(x = Longitude, y = Latitude, color=factor(Inspection.Grade)), data = inspect_data, size = 0.6) +
  scale_color_manual(labels = c("A", "B", "C"), values = c("green", "yellow", "red")) +
  labs(color = "Rating") +
  theme(legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot1_Map.png", plot = Plot1, dpi = 300)


# Plot2: Map of NY City with Shop Locations
# Retrieves City Map
map_nyc <- get_stamenmap(bbox = c(left = -74.2000, bottom = 40.5500, right = -73.6500, top = 40.9500),
                         zoom = 11,
                         maptype ='terrain',
                         color = 'color',
                         scale = 4)
# Plot of NY City
Plot2 <- ggmap(map_nyc) +
  geom_point(aes(x = Longitude, y = Latitude, color=factor(Inspection.Grade)), data = ny_inspect_data, size = 0.6) +
  scale_color_manual(labels = c("A", "B", "C"), values = c("green", "yellow", "red")) +
  labs(color = "Rating") +
  theme(legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot2_Map.png", plot = Plot2, dpi = 300)

# Plot3 of NY City with Subway Stations
Plot3 <- ggmap(map_nyc) +
  geom_point(aes(x = Longitude, y = Latitude, color=factor(Inspection.Grade)), data = ny_inspect_data, size = 0.6, alpha = 0.5) +
  scale_color_manual(labels = c("A", "B", "C"), values = c("green", "yellow", "red")) +
  labs(color = "Rating") +
  geom_point(aes(x = Longitude, y = Latitude), data = subway_data, size = 0.8, show.legend = FALSE, alpha = 1)
  theme(legend.text = element_text(size = 17),
        legend.title = element_text(size = 16),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

ggsave("./plots/Plot3_Map.png", plot = Plot3, dpi = 300)

rm(subway_data, inspect_data)
#########################################################################################
