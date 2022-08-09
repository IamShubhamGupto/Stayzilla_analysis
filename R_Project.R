setwd("F:/workspace/R")
dir()
df<-read.csv("stayzilla.csv", na.strings="")
View(df)
class(df)
dim(df)
df<-subset(df,select=-c(additional_info, crawl_date, description, highlight_value, image_urls, pageurl, qts, query_time_stamp, search_term, similar_hotel, sitename, uniq_id))
View(df)
#install.packages("tidyr")
library(tidyr)
df<-separate(df, room_price, c("room_price", NA), sep="per")
View(df)
cat("Amenities: ", sum(is.na(df$amenities)), "\n",
    "City: ", sum(is.na(df$city)), "\n",
    "Country: ", sum(is.na(df$country)), "\n",
    "Hotel Star Rating: ", sum(is.na(df$hotel_star_rating)), "\n",
    "Image Count: ", sum(is.na(df$image_count)), "\n",
    "Internet: ", sum(is.na(df$internet)), "\n",
    "Landmark: ", sum(is.na(df$landmark)), "\n",
    "Latitude: ", sum(is.na(df$latitude)), "\n",
    "Longitude: ", sum(is.na(df$longitude)), "\n",
    "Occupancy: ", sum(is.na(df$occupancy)), "\n",
    "Room Price: ", sum(is.na(df$room_price)), "\n",
    "Room types: ", sum(is.na(df$room_types)), "\n" )
#Drop columns with too many NA values.
df<-subset(df, select=-c(hotel_star_rating, internet, landmark, property_address, property_id))
View(df)
#install.packages("stringr")
library("stringr")
country=list("India"=0)
for(i in df$country)
  if(strcmp(i,"India"))
     country[["India"]]=country[["India"]]+1
country
#=1139 that implies all the fields are India or NA. therefore, column dropped.
df<-subset(df, select=-c(country))
View(df)
#Replacing NA in image_count with median
df$image_count[is.na(df$image_count)]<-0
x=median(df$image_count)
for(i in seq(1,1207))
  if((df$image_count[i]==0))
    df$image_count[i]=x

#Replace NAs in room_price with mean
df$room_price[is.na(df$room_price)]<-0
x=mean(df$room_price)
for(i in seq(1,1207))
{
  if((df$room_price[i]==0))
    df$room_price[i]=x
  if((df$room_price<100))
    df$room_price[i]=x
}

df<-separate(df, occupancy,c("adult_occupancy",NA,"child_occupancy",NA), sep=" ")
View(df)

#Replace NAs in occupancy with median

df$adult_occupancy[is.na(df$adult_occupancy)]<-0
df$child_occupancy[is.na(df$child_occupancy)]<-0
x<-median(df$adult_occupancy)
y<-median(df$child_occupancy)
for(i in seq(1,1207))
  if(df$adult_occupancy[i]==0 && df$child_occupancy[i]==0)
      df$adult_occupancy[i]=x
for(i in seq(1,1207))
  if(df$adult_occupancy[i]==2 && df$child_occupancy[i]==0)
      df$child_occupancy[i]=y

#Splitting the amenities column to represent the top 12 amenities
df$AC<-c("FALSE")
df$Newspaper<-c("FALSE")
df$Parking<-c("FALSE")
df$WiFi<-c("FALSE")
df$Card_Payment<-c("FALSE")
df$Elevator<-c("FALSE")
df$Pickup_and_Drop<-c("FALSE")
df$Free_Breakfast<-c("FALSE")
df$Veg_Only<-c("FALSE")
df$Bar<-c("FALSE")
df$Laundry<-c("FALSE")
df$Car_Parking<-c("FALSE")

x<-TRUE


for(i in seq(1,1207))
{
  if(str_detect("AC",as.character(df$amenities[i]),negate=FALSE))
    df$AC[i]=x
  
  if(str_detect("Newspaper",as.character(df$amenities[i]),negate=FALSE))
    df$Newspaper[i]=x
  
  if(str_detect("Parking",as.character(df$amenities[i]),negate=FALSE))
    df$Parking[i]=x
  
  if(str_detect("WiFi",as.character(df$amenities[i]),negate=FALSE))
    df$WiFi[i]=x
  
  if(str_detect("Card Payment",as.character(df$amenities[i]),negate=FALSE))
    df$Card_Payment[i]=x
  
  if(str_detect("Elevator",as.character(df$amenities[i]),negate=FALSE))
    df$Elevator[i]=x
  
  if(str_detect("Pickup & Drop",as.character(df$amenities[i]),negate=FALSE))
    df$Pickup_and_Drop[i]=x
  
  if(str_detect("Free Breakfast",as.character(df$amenities[i]),negate=FALSE))
    df$Free_Breakfast[i]=x
  
  if(str_detect("Veg Only",as.character(df$amenities[i]),negate=FALSE))
    df$Veg_Only[i]=x
  
  if(str_detect("Bar",as.character(df$amenities[i]),negate=FALSE))
    df$Bar[i]=x
  
  if(str_detect("Laundry",as.character(df$amenities[i]),negate=FALSE))
    df$Laundry[i]=x
  
  if(str_detect("Car Parking",as.character(df$amenities[i]),negate=FALSE))
    df$Car_Parking[i]=x
  
}
df <- transform(df, room_price = as.numeric(room_price))
#install.packages("rworldmap")
#install.packages("ggplot2")
#install.packages("reshape")
install.packages("tigerstats")
#VISUALIZATIONS
library(rworldmap)
#library(ggplot2)
#library(reshape)
library(tigerstats)
newmap <- getMap(resolution ="medium")
plot(newmap, ylim = c(8, 37), xlim = c(68, 97), asp = 1)
points(df$lon, df$lat, col = "red", cex = .6)
ac_c = sum(df$AC==TRUE)
news_c = sum(df$Newspaper==TRUE)
park_c = sum(df$Parking==TRUE)
wifi_c = sum(df$WiFi==TRUE)
card_c = sum(df$Card_Payment==TRUE)
ele_c = sum(df$Elevator==TRUE)
pick_c = sum(df$Pickup_and_Drop==TRUE)
fbf_c = sum(df$Free_Breakfast==TRUE)
veg_c = sum(df$Veg_Only==TRUE)
laundry_c =sum(df$Laundry==TRUE)
counts = c(ac_c,news_c,park_c,wifi_c,card_c,ele_c,pick_c,fbf_c,veg_c,laundry_c)
barplot(counts,
        main = "Comparison of number of amenities available",
        xlab = "Amenity",
        ylab = "Count",
        names.arg = c("AC","Newspaper","Parking","Wifi","Card Payment","Elevator","Pick n Drop","Free Breakfast","Veg only","Laundry"),
        col = "darkred",
        horiz = FALSE,)
bp1 <- boxplot(df$room_price,main="Room Price boxplot distribution")
df_clean <- df[!(df$room_price %in% bp1$out),]
bp2 <- boxplot(df_clean$room_price,main="Room Price boxplot distribution After cleaning")
densityplot(df$room_price,
            xlab="Room price",
            main="Distribution of room price before cleaning")
densityplot(df_clean$room_price,
            xlab="Room price",
            main="Distribution of room price after cleaning")