library(shiny)
library(shinythemes)
library(highcharter)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
#library(forcats)
library(purrr)
library(rmarkdown)

source('weather-functions.R', encoding = 'UTF-8')
load('data.RData')
options(highcharter.theme = hc_theme_smpl())
options(shiny.launch.browser = TRUE)

url_base <- "http://graphics8.nytimes.com/newsgraphics/2016/01/01/weather"

cities <- as.tibble(readxl::read_xlsx('cities.xlsx'))
cities$meteo_station <- as.character(cities$meteo_station)

citiesv <- setNames(cities$meteo_station, cities$city)
