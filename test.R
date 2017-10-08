
library(dplyr)
library(rlist)
library(XML)
library(RCurl)
library(stringr)

test_Data <- read.csv('test_stationid_time.txt', header=TRUE, sep=",")
read_Data <- read.csv('data.txt', header=TRUE, sep=",")
#select random station id from test data set
random <-sample(1:8, 1, replace=F)
#random <- 10
print(random)
Station_ID_test <- as.vector(test_Data$Station_Id[random])
mydate_test <- as.vector(test_Data$Input_Time[random])


Station_ID <- as.character(Station_ID_test)
mydate <- as.character(mydate_test)

mydate <- as.POSIXct(paste(mydate,mydate), format="%Y-%m-%d %H:%M" )
Station_ID <- IsStationvalid(Station_ID)
IsDateInvalid(mydate)

# Parameters required to generate URL to fetch data
m <- as.integer(format(as.Date(mydate), '%m'))
d <- as.integer(format(as.Date(mydate), '%d'))
Y <- as.integer(format(as.Date(mydate), '%Y'))
H <- as.character(substr(mydate,12,13))
H_1 <- as.character(as.integer(H)-1)

M <- as.character(substr(mydate,15,16))
c <- subset(read_Data,subset=IATA==Station_ID)
ICAO <- as.vector(c[[1,6]])
Coordinates <-  paste( c$Latitude,c$Longitude,c$Altitude, sep=",")


# Generate URL based on value of Station_ID and Timestamp passed to fetch weather 

URL <- paste0("https://www.wunderground.com/history/airport/",ICAO, "/" ,Y, "/", m,"/",d,"/DailyHistory.html" )
theurl <- getURL(URL ,.opts = list(ssl.verifypeer = FALSE))
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
# Store extracted Data
finaldata <- tables$obsTable
#str(finaldata)
# Fetch required columns from data frame
#WeatherData <- finaldata[, c(1,2,4,5,12)]
WeatherData <- finaldata[, -c(3,4,7,8,12)]

WeatherData_Time_Formatted <- mutate(WeatherData, WeatherReportTime = as.POSIXct(paste(substr(mydate,1,10),WeatherData$Time), format="%Y-%m-%d %I:%M %p"))
WeatherData_Time_Formatted_M <- WeatherData_Time_Formatted[(str_sub(WeatherData_Time_Formatted$WeatherReportTime, start=15L, end=16L) <= M),]
WeatherData_Time_Formatted_M_nRows <- nrow(WeatherData_Time_Formatted_M)
# Find the latest data available at that point in time
if (as.integer(WeatherData_Time_Formatted_M_nRows==0)) {
  WeatherData_Time_Formatted <- WeatherData_Time_Formatted[grep(H_1, substr(WeatherData_Time_Formatted$WeatherReportTime,12,13)),]
  } else {
    WeatherData_Time_Formatted <- WeatherData_Time_Formatted[grep(H, substr(WeatherData_Time_Formatted$WeatherReportTime,12,13)),]
  }

WeatherData_Time_Formatted <- tail(WeatherData_Time_Formatted,1)

stg_output <- cbind(Coordinates,WeatherData_Time_Formatted)
final_output <- paste(Station_ID,stg_output$Coordinates,stg_output$WeatherReportTime,
                      stg_output$Conditions, stg_output$Temp., stg_output$Pressure ,stg_output$Humidity, sep = "|")

# Final output dataframe
final_output
# Write to file
write.table(final_output, file="Weather_Report.csv", row.names = FALSE, append = TRUE, col.names = F)
# Display final output
View(x = final_output , title = "Weather_Report")
