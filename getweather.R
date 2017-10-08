# @title get weather data for a given Location and Time
# Install required packages
install.packages("XML")
install.packages("RCurl")
install.packages("rlist")
install.packages("dplyr")
install.packages("stringr")

library(dplyr)
library(rlist)
library(XML)
library(RCurl)
library(stringr)

# @param Station_ID - 3 digit IATA code
# @param mydate (format 'yyyy-mm-dd H:M) - Enter date and time for which weather conditions are required

# Enter Station_Id(IATA) and Timestamp 
Station_ID <- as.character("MMK")
mydate <- "2017-10-06 22:12"

# Read data for 
read_Data <- read.csv('data.txt', header=TRUE, sep=",")
#head(read_Data)


mydate <- as.POSIXct(paste(mydate,mydate), format="%Y-%m-%d %H:%M" )


# Function to Validate station ID
IsStationvalid <- function ( Station_ID ) { 
  if (Station_ID %in% read_Data$IATA ) {
    print("Valid station id supplied")
    return(Station_ID)
  } else {
    print("Invalid station id supplied. Defaulting to SYD")
    # Default to Sydney
    Station_ID <- as.character("SYD")
    return(Station_ID)  
  } 
}

# Validate Station id value as passed by user. Defaulted to "SYD" if incorrect value is passed
Station_ID <- IsStationvalid(Station_ID)
Station_ID


#Function to check for validity of Date
IsDateInvalid <- function (date, opt_warnings=TRUE) {
  d <- try( as.Date( date, format= "%Y-%m-%d %H:%M" ) ) 
  if( class(d) == "try-error" || is.na(d) ){
    stop(paste("\n\nInvalid date supplied:", date))
    return(1)
  } else {
    print("Valid Date has been supplied")
    return(0)
  }
}

# Check if valid date was passed by user
IsDateInvalid(mydate)
mydate

# Parameters required to generate URL to fetch data
m <- as.integer(format(as.Date(mydate), '%m'))
d <- as.integer(format(as.Date(mydate), '%d'))
Y <- as.integer(format(as.Date(mydate), '%Y'))
H <- as.character(substr(mydate,12,13))
M <- as.character(substr(mydate,15,16))
c <- subset(read_Data,subset=IATA==Station_ID)
ICAO <- as.vector(c[[1,6]])
Coordinates <-  paste( c$Latitude,c$Longitude,c$Altitude, sep=",")


# Generate URL based on value of Station_ID and Timestamp passed to fetch weather 
# Sample URL
#
#URL <- "https://www.wunderground.com/history/airport/YBBN/2017/10/6/DailyHistory.html"
URL <- paste0("https://www.wunderground.com/history/airport/",ICAO, "/" ,Y, "/", m,"/",d,"/DailyHistory.html" )
theurl <- getURL(URL ,.opts = list(ssl.verifypeer = FALSE))
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
# Store extracted Data
finaldata <- tables$obsTable
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
write.table(final_output, file="Weather_Report.csv", row.names = FALSE, append = FALSE, col.names = F)
# Display final output
View(x = final_output , title = "Weather_Report")
