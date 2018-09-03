library(shiny)
library(shinythemes)
library(highcharter)
library(dplyr)
library(tibble)
library(readr)
library(readxl)
library(forcats)

#options(encoding = 'UTF-8')
source('weather-functions.R', encoding = 'UTF-8')
load('data.RData')

options(highcharter.theme = hc_theme_smpl())
options(shiny.launch.browser = TRUE)


cities <- as.tibble(readxl::read_xlsx('cities.xlsx'))
cities$meteo_station <- as.character(cities$meteo_station)

citiesv <- setNames(cities$meteo_station, cities$city)
