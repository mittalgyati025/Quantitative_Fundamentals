# load data
airlines_df <- read.csv("Data_RegionEx.csv", header = TRUE) 


# Question1

# Removing the rows that denoted the cancelled flights 
rowdel <- which(airlines_df$Actual.arrival.time == "Cancelled") 
new_airlines_df <- airlines_df[-rowdel, ]

# Splitting into separate dataframes
regionex_df <- new_airlines_df[new_airlines_df$Airline == "RegionEx",  ] 
mda_df <-  new_airlines_df[new_airlines_df$Airline == "MDA",  ] 

regionex_df$Arrival.delay.in.minutes <- as.numeric(as.character(regionex_df$Arrival.delay.in.minutes))
mda_df$Arrival.delay.in.minutes <- as.numeric(as.character(mda_df$Arrival.delay.in.minutes))

# Calculating the mean ,median,standard deviation, 90th percentile for RegionEx
mean_regionex <- mean(regionex_df$Arrival.delay.in.minutes,na.rm = TRUE)
mean_regionex
median_regionex <- median(regionex_df$Arrival.delay.in.minutes,na.rm = TRUE)
median_regionex
sd_regionex <- sd(regionex_df$Arrival.delay.in.minutes,na.rm = TRUE)
sd_regionex
percentile_regionex <- quantile(regionex_df$Arrival.delay.in.minutes,.90)
percentile_regionex
# Calclating the mean ,meadian,standard deviation, 90th percentile for MDA
mean_mda <- mean(mda_df$Arrival.delay.in.minutes,na.rm = TRUE)
mean_mda
median_mda <- median(mda_df$Arrival.delay.in.minutes,na.rm = TRUE)
median_mda
sd_mda <- sd(mda_df$Arrival.delay.in.minutes, na.rm = TRUE)
sd_mda
percentile_mda <- quantile(mda_df$Arrival.delay.in.minutes,.90)
percentile_mda


# Question2
# Histograms for Regionex and MDA
hist(regionex_df$Arrival.delay.in.minutes) 
hist(mda_df$Arrival.delay.in.minutes)

# Question3
# Function to calculate On time and delay percent for different airlines

on_time_delay_percent <- function(df, airlineName){
  
  df$Delay.indicator<- as.numeric(as.character(df$Delay.indicator))
  sum_total <- nrow(df)
  sum_on_time <- length(which(df$Delay.indicator == 0))
  sum_delay <- length(which(df$Delay.indicator == 1 ))
  on_time_percent <- (sum_on_time /sum_total)*100
  delay_percent <- (sum_delay / sum_total)*100
  
  print(paste(airlineName,': On-time percent is: ',on_time_percent))
  print(paste(airlineName,': Delay percent is: ',delay_percent))
}

on_time_delay_percent(mda_df, 'MDA')
on_time_delay_percent(regionex_df, 'RegionEx')

# Question4
# Function to calculate mean,median,SD and 90 percentile for different airlines 
# on each of their routes

route_calculation <- function(df, route, airline){
  
  mean_route <- with(df, mean(Arrival.delay.in.minutes[Route.Code == route]))
  median_route <- with(df, median(Arrival.delay.in.minutes[Route.Code == route]))
  sd_route <- with(df, sd(Arrival.delay.in.minutes[Route.Code == route]))
  quantile_route <- with(df, quantile(Arrival.delay.in.minutes[Route.Code == route],.90))
  sum_delay_route <- length(which(df$Delay.indicator == 1 & df$Route.Code == route ))
  sum_total_route <- length(which(df$Route.Code == route))
  delay_percent_route <- (sum_delay_route / sum_total_route)*100
  
  
  print(paste(airline, 'Route ', route, ': Mean is: ', mean_route ))
  print(paste(airline, 'Route ', route, ': Median is: ', median_route ))
  print(paste(airline, 'Route ', route, ': Standard Deviation is: ', sd_route ))
  print(paste(airline, 'Route ', route, ': 90percent Quantile is: ', quantile_route ))
  print(paste(airline, 'Route ', route, ': Delay percent is: ', delay_percent_route ))
  
}

route_calculation(mda_df,1,'MDA')
route_calculation(mda_df,2,'MDA')
route_calculation(mda_df,3,'MDA')
route_calculation(mda_df,4,'MDA')

route_calculation(regionex_df,1,'Regionex')
route_calculation(regionex_df,2,'Regionex')
route_calculation(regionex_df,3,'Regionex')
route_calculation(regionex_df,4,'Regionex')

# Question5
# Scatterplot for no. of passengers vs Arrival delay and calculating Correlation Co-Efficient

plot(x = regionex_df$Number.of.passengers, y = regionex_df$Arrival.delay.in.minutes, xlab = "Number of passengers",
     ylab = "Arrival delay in minutes")
correlation_coeff <- cor(regionex_df$Number.of.passengers , regionex_df$Arrival.delay.in.minutes)
print(paste('Correlation co-efficient is: ' , correlation_coeff))

# Question6
# Calculating the actual and scheduled duration for each airline

duration_calculation <- function(df, actual_arrival , departure, scheduled_arrival ){

  actual_dep<- as.POSIXct(df$Scheduled.departure.time , format = "%H:%M")
  actual_arr <- as.POSIXct(df$Actual.arrival.time, format = "%H:%M")
  df$actual_duration<- as.numeric(difftime(actual_arr,actual_dep),units = "mins")
  
  scheduled_dep <- as.POSIXct(df$Scheduled.departure.time , format = "%H:%M")
  scheduled_arr <- as.POSIXct(df$Scheduled.arrival.time, format = "%H:%M")
  df$scheduled_duration<- as.numeric(difftime(scheduled_arr,scheduled_dep),units = "mins")
  
  return (df)
}
regionex_df <- duration_calculation(regionex_df,regionex_df$Actual.arrival.time,regionex_df$Scheduled.departure.time,
                     regionex_df$Scheduled.arrival.time)
mda_df <- duration_calculation(mda_df,regionex_df$Actual.arrival.time,mda_df$Scheduled.departure.time,
                               mda_df$Scheduled.arrival.time)

#Comparing scheduled duration for the two airlines by route

regionex_df[regionex_df$Route.Code == '1', "scheduled_duration"]
mda_df[mda_df$Route.Code == '1', "scheduled_duration"]

regionex_df[regionex_df$Route.Code == '2', "scheduled_duration"]
mda_df[mda_df$Route.Code == '2', "scheduled_duration"]

regionex_df[regionex_df$Route.Code == '3', "scheduled_duration"]
mda_df[mda_df$Route.Code == '3', "scheduled_duration"]

regionex_df[regionex_df$Route.Code == '4', "scheduled_duration"]
mda_df[mda_df$Route.Code == '4', "scheduled_duration"]

#Comparing actual duration for the two airlines by route

regionex_df[regionex_df$Route.Code == '1', "actual_duration"]
mda_df[mda_df$Route.Code == '1', "actual_duration"]

regionex_df[regionex_df$Route.Code == '2', "actual_duration"]
mda_df[mda_df$Route.Code == '2', "actual_duration"]

regionex_df[regionex_df$Route.Code == '3', "actual_duration"]
mda_df[mda_df$Route.Code == '3', "actual_duration"]

regionex_df[regionex_df$Route.Code == '4', "actual_duration"]
mda_df[mda_df$Route.Code == '4', "actual_duration"]

# Assuming the same scheduled duartion for both airlines by assigning mda's scheduled duration to regionex

regionex_df$assumed_same_scheduled_duration[regionex_df$Route.Code == 1] <- 100
regionex_df$assumed_same_scheduled_duration[regionex_df$Route.Code == 2] <- 100
regionex_df$assumed_same_scheduled_duration[regionex_df$Route.Code == 3] <- 75
regionex_df$assumed_same_scheduled_duration[regionex_df$Route.Code == 4] <- 75

# Calculating the new assumed delay for RegionEx
regionex_df$assumed_delay <- regionex_df$actual_duration - regionex_df$assumed_same_scheduled_duration

# Calculating the mean. median,sd,90th percentile for assumed delay for regionex
mean_assumed_regionex <- mean(regionex_df$assumed_delay,na.rm = TRUE)
median_assumed_regionex <- median(regionex_df$assumed_delay,na.rm = TRUE)
sd_assumed_regionex <- sd(regionex_df$assumed_delay,na.rm = TRUE)
percentile_assumed_regionex <- quantile(regionex_df$assumed_delay,.90)

# New histogram for RegionEx based on the assumed scheduled duration
hist(regionex_df$assumed_delay) 

# Existing histogram for regionEx and MDA for comparison
hist(regionex_df$Arrival.delay.in.minutes) 
hist(mda_df$Arrival.delay.in.minutes)

# Question7

#boxplot for arrival dealay in minutes for 4 routes of RegionEx flight

boxplot(regionex_df$Arrival.delay.in.minutes~regionex_df$Route.Code, ylab= 'Arrival Delay in minutes',main ="Boxplot Distibution of RegionEX flight")


Route1_RegionEx_Data <- regionex_df[regionex_df$Route.Code == "1",]

#boxplot for arrival dealay in minutes for route 1 vs departure date for Region Ex flight

boxplot(Route1_RegionEx_Data$Arrival.delay.in.minutes~Route1_RegionEx_Data$Departure.date, ylab= 'Arrival Delay in minutes',main ="Boxplot Distibution of RegionEX flight with departure date")
















  








                            













