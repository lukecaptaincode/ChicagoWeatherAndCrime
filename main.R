#Package Installs
install.packages("dplyr")
install.packages("lubridate")
install.packages("weathermetrics")
install.packages("cowplot")
install.packages("Hmisc")
#Load Libraries
library(readr)
library(dplyr)
library(lubridate)
library(weathermetrics)
require(ggplot2)
require(cowplot)
library(scales)
require(Hmisc)
#clean slate - comment out when actually using the code
#rm(list = ls())

#' CleanWeatherFunction
#' cleans the weather data frame by converting th max and min
#' temperature values to numerics and getting their mean. The mean is
#' them converted from fahrenheit to celsius and the date is converted
#' to a date value
#' @return returns the cleaned weather dataframe
#' @examples
#' dataframe = cleanWeatherFunction()
cleanWeatherFunction <- function() {
  #Load weather data
  weather = read.csv("./weather.csv")
  #Convert the Min and Max daily temps to numerics
  weather$TMIN <- as.numeric(as.character(weather$TMIN))
  weather$TMAX <- as.numeric(as.character(weather$TMAX))
  #The daily temperature mean from the min and max
  weather <-
    mutate(weather, temp_mean = rowMeans(weather[5:6], na.rm = TRUE))
  #Convert the mean temps to celsius
  weather$temp_mean <-
    fahrenheit.to.celsius(weather$temp_mean, round = 2)
  #convert date field to date and give it a common name
  weather$date <- as.Date(weather$DATE)
  #returm the weather data to a new data frame while dropping cols that wont be used
  return(select(weather,-c(1, 6:15, "DATE")))
  
}
#' cleanCrimeFunction
#' cleans the crime data by formatting the date field and removing
#' unused fields from the data and returning the frame
#' @return returns the cleaned crime function data frame
#' @examples
#' dataFrame = cleanCrimeFunction()
cleanCrimeFunction <- function() {
  #Load Crime Data
  crime = read.csv("./crime/Chicago_Crimes_2012_to_2017.csv")
  #Format the date field
  crime$Date <- as.POSIXct(crime$Date,
                           format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
  #Convert date to date datatype and give common name
  crime$date <- as.Date(crime$Date)
  #Retunr the crime data to a new data frame while dropping cols that wont be used
  return(select(crime,-c(1:3, 5:6, 10:23, "Date")))
}

#' combineDataFrames
#' Left joins the two data sets by the date, makes it easier to manage and access data
#' @param crimeData 
#' @param weatherData 
#'
#' @return the combined data frame
#' @examples
#' dataFrame = combineDataFrames(crimeDataFrame,weatherDataFrame)
combineDataFrames <- function(crimeData, weatherData) {
  #join the datasets by date
  combindeFrames <-
    na.omit(left_join(crimeData, weatherData, by = c("date")))
  return(combindeFrames)
}
#' plot_kmeans_rain
#' uses the passed data frame (of all crimes) to perform kmeans using the rain in mm
#' and the tempeature to see if there is any particular "sweet spot" for crime
#' when it rains
#' @param df - the data frame to kmeans nad plot
#' @examples
#' plot_kmeans_snow(dataFrame)
plot_kmeans_rain = function(df) {
  #set seed
  set.seed(1993)
  #create matrix using temp and rain fail
  matrix = as.matrix(cbind(df$temp_mean, df$PRCP), ncol = 2)
  #cluster the matrix with kmeans and 3 centers
  cl = (kmeans(matrix, 3))
  #add the data cluster to the data frame as a fatcor
  df$cluster = factor(cl$cluster)
  #get the centers
  centers = as.data.frame(cl$centers)
  
  #Create Charts
  allDataPlot_Rain <-
    ggplot(
      data = df,
      aes(x = temp_mean, y = PRCP, color = Primary.Type),
      show.legend = F
    ) +
    labs(x = "Mean temperature in C", y = "Precipitation in mm") +
    geom_point(show.legend = F) +
    geom_point(data = centers,
               aes(x = V1, y = V2, color = 'Center'),
               show.legend = F) +
    geom_point(
      data = centers,
      aes(x = V1, y = V2, color = 'Center'),
      size = 52,
      alpha = .3,
      show.legend = F
    ) +
    theme(legend.key = element_rect(
      colour = 'purple',
      size = 0.5,
      linetype = 'dashed'
    )) +
    # use dark theme because its noce
    theme_dark()
  #Limited plot to "zoom" a center
  allDataSegement1Plot <- allDataPlot_Rain +
    xlim(-10.0,-5.0) +
    ylim(0.0, 0.5)
  #Limited plot to "zoom" a center
  allDataSegement2Plot <- allDataPlot_Rain +
    xlim(-5.0, 0.0) +
    ylim(0.0, 0.5)
  #Limited plot to "zoom" a center
  allDataSegement3Plot <- allDataPlot_Rain +
    xlim(3.0, 6.0) +
    ylim(0.0, 0.5)
  #Add all plots to grid
  plot_grid(
    allDataPlot_Rain,
    allDataSegement1Plot,
    allDataSegement2Plot,
    allDataSegement3Plot ,
    labels = "AUTO"
  )
  #Save
  ggsave("rain_and_temp_kmeans.png",
         width = 10,
         height = 10)
}

#' plot_kmeans_snow
#' uses the passed data frame (of all crimes) to perform kmeans using the snowfall in mm
#' and the tempeature to see if there is any particular "sweet spot" for crime
#' when it snow - DIDNT PLOT MORE THAT MAIN IMAGE AS THERE ARE ONLY 1 CENTER ANYWAY
#' @param df - the data frame to kmeans nad plot
#' @examples
#' plot_kmeans_snow(dataFrame)
plot_kmeans_snow = function(df) {
  #set seed
  set.seed(1993)
  #create matrix using temp and snow fall
  matrix = as.matrix(cbind(df$temp_mean, df$SNOW), ncol = 2)
  #add kmeanas proccessed matrix with 3 centers to cluster
  cl = (kmeans(matrix, 3))
  #cluster as factor of data frame
  df$cluster = factor(cl$cluster)
  #get the centers
  centers = as.data.frame(cl$centers)
  
  allDataPlot_Snow <-
    ggplot(
      data = df,
      aes(x = temp_mean, y = SNOW, color = Primary.Type),
      show.legend = F
    ) +
    labs(x = "Mean temperature in C", y = "Snowfall in mm") +
    geom_point(show.legend = F) +
    geom_point(data = centers,
               aes(x = V1, y = V2, color = 'Center'),
               show.legend = F) +
    geom_point(
      data = centers,
      aes(x = V1, y = V2, color = 'Center'),
      size = 52,
      alpha = .3,
      show.legend = F
    ) +
    theme(legend.key = element_rect(
      colour = 'purple',
      size = 0.5,
      linetype = 'dashed'
    )) +
    theme_dark()
  #add to plot grid
  plot_grid(allDataPlot_Snow, labels = "AUTO")
  #save
  ggsave("snow_and_temp_kmeans.png",
         width = 10,
         height = 10)
}
#' Count Crime
#' Takes the dataframe and the desired crime to count then
#' filters the dataframe using the crime string into a new frame
#' the date is then added to a table and then pushed into
#' a new data frame formated and the table vector used as the value of
#' CrimeCount. The data is then plotted and save to png with the file name
#' that uses the crimeToCount variable as part of the name.
#' @param df - the dataframe
#' @param crimeToCount - string name of crime to count
#' @return joinedData - returned for later user
#' @examples
#' count_crime(dataframe, crimetoCount)
count_crime = function(df, crimeToCount) {
  #new data frame consisting of the only the crime to count
  dfFiltered <- filter(df, Primary.Type == crimeToCount)
  #Makesure the date is a date
  dfFiltered$date <- as.Date(dfFiltered$date)
  #Create a table from the date
  dateTable <- table(dfFiltered$date)
  #Add the new data to a data frame and use the vector of the table as the count
  countedData <-
    data.frame(date = format(as.Date(names(dateTable)), "%Y/%m/%d"),
               CrimeCount = as.vector(dateTable))
  #Makesure date is date
  countedData$date <- as.Date(countedData$date)
  #Plot the counted data
  crime_count_plot <-
    ggplot(
      data = countedData,
      aes(
        x = as.Date(countedData$date),
        y = countedData$CrimeCount,
        group = 1,
        color = countedData$CrimeCount
      )
    ) +
    labs(
      x = "date",
      y = paste(crimeToCount),
      color = paste(crimeToCount)
    ) +
    geom_point() +
    geom_line() +
    #Makesure that the x axis labels are read able
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    #save the plot
    ggsave(paste(crimeToCount, "counted.png"),
           width = 20,
           height = 20)
  #join data together on date and retunr for later use
  return(joinedData <-
           na.omit(left_join(countedData, dfFiltered, by = c("date"))))
  
}
#' doRegression
#' Performs three regresion analysis on the joinedata frame from
#' counted crime, does regression against, temp, rain and snow
#' @param df - Joinedata datafame
#' @param crimeToCount - The crime
#' @examples
#' doRegression(dataToDoRegressionON, crimeToCount)
doRegression = function(df, crimeToCount) {
  # the temperature regression plot
  regression_plot_temp <-
    ggplot(df,
           aes(
             x = df$temp_mean,
             y = df$CrimeCount,
             color = df$CrimeCount
           )) +
    #perform the regression using the passed date
    stat_summary(fun.data = mean_cl_normal) +
    geom_smooth(method = 'lm', formula = y ~ x) +
    labs(
      x = "Mean Temperature(c)",
      y = paste(crimeToCount),
      color = paste(crimeToCount)
    )
  #Rain regression plot
  regression_plot_PRCP <-
    ggplot(df, aes(
      x = df$PRCP,
      y = df$CrimeCount,
      color = df$CrimeCount
    )) +
    #perform the regression using the passed date
    stat_summary(fun.data = mean_cl_normal) +
    geom_smooth(method = 'lm', formula = y ~ x) +
    labs(
      x = "Precipitation(mm)",
      y = paste(crimeToCount),
      color = paste(crimeToCount)
    )
  #Snow regression plot
  regression_plot_SNOW <-
    ggplot(df, aes(
      x = df$SNOW,
      y = df$CrimeCount,
      color = df$CrimeCount
    )) +
    #perform the regression using the passed date
    stat_summary(fun.data = mean_cl_normal) +
    geom_smooth(method = 'lm', formula = y ~ x) +
    labs(
      x = "Snowfall(mm)",
      y = paste(crimeToCount),
      color = paste(crimeToCount)
    )
  #Add all 3 plots to grid
  plot_grid(regression_plot_temp,
            regression_plot_PRCP,
            regression_plot_SNOW,
            labels = "AUTO")
  ggsave(paste(crimeToCount, "regression.png"),
         width = 20,
         height = 20)
}
#' crimeTempAndCountPlot
#' Plots the crime data with temperature data to
#' @param df - the data frame containing the data
#' @param crimeToCount - the crime to run the function for
#' @examples
#' crimeTempAndCountPlot(dataFrame, crimeToCount)
crimeTempAndCountPlot = function(df, crimeToCount) {
  #Set a scale factor to see see overlap
  scaleFactor <- max(df$CrimeCount) / max(df$temp_mean)
  #create plot
  crime_count_temp_plot <-
    ggplot(df,aes(x = as.Date(df$date))) +
    labs(
      x = "date",
      y = paste(crimeToCount),
      color = paste(crimeToCount)
    ) +
    geom_line(data = df,
              aes(
                y = df$CrimeCount,
                group = 1,
                color = df$CrimeCount
              )) +
    geom_line(data = df,
              #plot the second Y and multiply by the scale factor to overlap
              aes(y = df$temp_mean * scaleFactor),
              color = "red") +
    scale_y_continuous(name=paste(crimeToCount) ,sec.axis=sec_axis(~./scaleFactor, name="Temperature Mean(c)")) +
    #Makesure that the x axis labels are read able
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    #save the plot
    ggsave(paste(crimeToCount, "tempCount.png"),
           width = 20,
           height = 20)
}
#'=================EXECUTION SECTION ========================
#'        CALL ALL CODE FUNCTION BELOW HERE
#'        THIS MAKES IT EASIER TO SEE THE ORDER OF
#'        EXECUTION
#'===========================================================

#Get data from the fucntions and combine cleaned data
combinedData = combineDataFrames(cleanCrimeFunction(), cleanWeatherFunction())
#Plot Snow and rain with kmeans applied
plot_kmeans_rain(combinedData)
plot_kmeans_snow(combinedData)
#Perform regression and count on the desired crimes
crimeList  <- list("MOTOR VEHICLE THEFT","ASSAULT","BATTERY","THEFT","BURGLARY","KIDNAPPING","HOMICIDE")
for(i in crimeList){
  #Set crime to Count
  crimeToCount = i
  #Call it and get data
  dataToDoRegressionON = count_crime(combinedData, crimeToCount)
  #do regression
  doRegression(dataToDoRegressionON, crimeToCount)
  crimeTempAndCountPlot(dataToDoRegressionON, crimeToCount)
}

