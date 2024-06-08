library(sqldf)

library(dplyr)

library(data.table)

library(ggplot2)


#london bicycles
london <- read.csv("C:\\Users\\eliss\\Downloads\\archive (1)\\london_merged.csv")
london$timestamp <- as.POSIXct(london$timestamp, format = "%Y-%m-%d %H:%M:%S")
#london$timestamp <- as.POSIXct(london$timestamp, format = "%Y-%m-%d %H:%M")
#london$timestamp <- format(london$timestamp, "%Y-%m-%d %H:%M")
london <- london[,c(1,2)]
typeof(london$timestamp)
london$year <- as.numeric(format(london$timestamp, "%Y"))
london$month <- as.numeric(format(london$timestamp, "%m"))
str(london)
london <- london[london$year==2015 & london$month>=3 & london$month<=9, ]

#weather
weather <- read.csv("C:\\Users\\eliss\\Downloads\\open-meteo-51.49N0.16W23m (2).csv")
weather$time <- as.POSIXct(weather$time, format = "%Y-%m-%dT%H:%M")
weather$year <- as.numeric(format(weather$time, "%Y"))
weather$month <- as.numeric(format(weather$time, "%m"))
str(weather)
weather <- weather[weather$year==2015 & weather$month>=3 & weather$month<=9, ]


#weather$time <- format(weather$time, "%Y-%m-%d %H:%M")

#main df
df <- merge(london, weather, by.x= 'timestamp', by.y = 'time')
names(df) <- c("time" ,     "count" ,"year.x","month.x", "temperature"  ,"relative_humidity",
               "apparent_temperature" , "precipitation",  "rain"     ,        "snowfall"    ,    
               "wind_speed")
df$time <- as.Date(df$time, format = "%Y-%m-%d %H:%M")
df <- df[, -c(12,13)]
df <- df[, -c(4,3)]
str(df)

#main plots

#-----------------------RAIN------------------------
rain_data <- df[df$rain>0,]
df$rainfall_flag <- df$rain > 0

ggplot(df, aes(x = time)) +
  geom_bar(aes(y = count, fill = rainfall_flag), stat = "identity", alpha = 0.7) +
  geom_line(data = df[df$rain > 0, ], aes(y = rain * 3000), color = "red", size = 0.75) + 
  labs(title = "Bike rentals and rainfall chart",
       x = "Date",
       y = "Bike rentals count") +
  scale_y_continuous(
    name = "Bike rentals count",
    sec.axis = sec_axis(~ . / 10, name = "Rainfall (cm)")
  ) +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "#5b5b5b")) +
  theme_minimal()


#-----------------------RAIN2---------------------------------------------------------
ggplot(df, aes(x = time)) +
  #geom_line(aes(y = count), color = "gray", size = 0.9) + 
  geom_bar(aes(y = count, fill = rainfall_flag), stat = "identity", alpha = 0.7) +
  geom_bar(aes(y = rain * 850), stat = "identity", fill = "red", alpha = 0.7) + 
  labs(title = "Bike rentals and rainfall chart",
       x = "Date",
       y = "Bike rentals count")+
  scale_y_continuous(
    name = "Bike rentals count",
    sec.axis = sec_axis(~ . / 10, name = "Rainfall (cm)")
  ) +
  # scale_x_datetime(
  #   name = "Date",
  #   date_breaks = "1 hour", 
  #   date_labels = "%Y-%m-%d %H:%M"
  # ) +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "#5b5b5b")) + 
  theme_minimal()

#---------------------TEMPERATURE---------------------------
ggplot(df, aes(x = time)) +
  geom_bar(aes(y = count), stat = "identity", fill = "gray", alpha = 0.7) + 
  geom_line(aes(y = temperature * 1700), color = "red", size = 0.5) + 
  labs(title = "Bike rentals and rainfall chart",
       x = "Date",
       y = "Bike rentals count") +
  scale_y_continuous(
    name = "Bike rentals count",
    sec.axis = sec_axis(~ . / 5, name = "Temperature (°C)")
  ) +
  theme_minimal()

#---------------------WIND-----------------------
ggplot(df, aes(x = time)) +
  geom_bar(aes(y = count), stat = "identity", fill = "gray", alpha = 0.7) + 
  geom_line(aes(y = wind_speed * 1000), color = "red", size = 0.7) + 
  labs(title = "Bike rentals and rainfall chart",
       x = "Date",
       y = "Bike rentals count") +
  scale_y_continuous(
    name = "Bike rentals count",
    sec.axis = sec_axis(~ . / 5, name = "Wind speed")
  ) +
  theme_minimal()

#--------------------HUMIDITY----------------------

ggplot(df, aes(x = time)) +
  geom_bar(aes(y = count), stat = "identity", fill = "gray", alpha = 0.7) + 
  geom_line(aes(y = relative_humidity * 200), color = "red", size = 0.7) + 
  labs(title = "Bike rentals and rainfall chart",
       x = "Date",
       y = "Bike rentals count") +
  scale_y_continuous(
    name = "Bike rentals count",
    sec.axis = sec_axis(~ . / 5, name = "Humidity ")
  ) +
  theme_minimal()

#----------------PRECIPITATION----------------------
ggplot(df, aes(x = time)) +
  geom_bar(aes(y = count), stat = "identity", fill = "gray", alpha = 0.7) + 
  geom_line(aes(y = precipitation * 1500), color = "red", size = 0.7) + 
  labs(title = "Bike rentals and rainfall chart",
       x = "Date",
       y = "Bike rentals count") +
  scale_y_continuous(
    name = "Bike rentals count",
    sec.axis = sec_axis(~ . / 5, name = "Precipitation")
  ) +
  theme_minimal()
