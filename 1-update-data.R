library(readxl)
library(stringi)
library(zoo)
library(tibble)
library(rvest)

cities <- as.tibble(readxl::read_xlsx('cities.xlsx'))
cities$meteo_station <- as.character(cities$meteo_station)
nm = c('day', 'month', 'year', 'temp_min', 'tem_avg', 'temp_max', 'temp_diff', 'precip_mm')
mainDir <-  'data_out/'
stations <-  cities$meteo_station

read_weather <- function(id, year, month){
   url = paste0('http://www.pogodaiklimat.ru/monitor.php?id=', id, '&month=', month, '&year=', year )
   df = rvest::html(url)
   ll <- html_table(df, fill = TRUE)
   df = ll[[3]]
   df = df[df[,1] != 'Дата',]
   names(df) = c('day', 'temp_min', 'temp_avg', 'temp_max', 'temp_diff', 'precip_mm')
   df$month = month
   df$year = year 
   return(df)
}


folders <- list.dirs(mainDir)[-1]
folder = folders[1]
year <- format(Sys.Date(), '%Y')
month <- as.numeric(format(Sys.Date(), '%m'))
if(as.numeric(format(Sys.Date()-1, '%m')) == month-1) month = month-1 # for the last day of the month

for(station in stations){
   cat(station, '\n')
   ID = station
   df = read_weather(ID, year, month)
   write.csv(df, file  = paste0('data_out/', ID, '/', year,'-', month, '.csv'), row.names = FALSE)
}



