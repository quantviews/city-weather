library(shiny)
library(shinythemes)
library(highcharter)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
require(readxl)
require(stringr)
require(data.table)
## input data

#rm(list=ls()) # remove all vars 
#rm(list = setdiff(ls(), lsf.str())) remove all variables except functions
# 
# AVG_YEARS = 10
# YEAR = as.numeric(format(Sys.Date(), '%Y'))
# ID = 27612
# CITY = 'Москва'
# n_m = 12

precip_df = read.table('19812010_rus_prcp_normals.txt', skip = 2, sep = '')
names(precip_df) <- c('code', 
                       "январь" ,  "февраль",  "март" , "апрель" ,  "май", 
                       "июнь" , "июль", "август",   "сентябрь", "октябрь",
                       "ноябрь" ,  "декабрь" )
precip_df$code <- as.character(precip_df$code)

###
munge_weather_data <- function(code){
   files = list.files(paste0('data_out/', code))
   years = as.numeric(str_extract_all(files, '[0-9]{4}')) # extract years from characters
   files = files[(years<=YEAR)&(years>=YEAR-AVG_YEARS)]
   city = fread(paste0('data_out/',ID,'/',files[1]))
   for(file in files[-1]){
      df = data.table::fread(paste0('data_out/',ID,'/',file))
      #df = read.csv(paste0('data_out/',ID,'/',file))
      city = plyr::rbind.fill(city, df)
   
   }
   city$date <-  as.Date(ISOdate(as.numeric(city$year), as.numeric(city$month), as.numeric(city$day)))
   city = city[order(city$date),]
   names(city)[3] = 'temp_avg'
   dates = seq(from = as.Date(paste0(YEAR,"-01-01")), to =  as.Date(paste0(YEAR,"-12-31")),by = 'day')
   dates = data.frame(date = dates)
   city = merge(dates,city, by = 'date', all = TRUE)
   city$month <- as.numeric(format(city$date, '%m'))
   city$day <- as.numeric(format(city$date, '%d'))
   city$year <- as.numeric(format(city$date, '%Y'))
   return(city)
   
}



## get data on normal precip 
get_precip_data <- function(code){
   precip_avg = precip_df[precip_df$code == code,][-1]
   precip_normal = data.frame(as.numeric(precip_avg))
   names(precip_normal)[1] = 'precip_normal'
   precip_normal$date = seq(as.Date(paste0(YEAR,"-01-01")),length = 12,by = "months")
   # precip_avg = read_excel(path = paste0('data/', code,'.xls'), sheet = 'Осадки')
   # precip_avg = precip_avg[precip_avg[,1] == 'Средняя 1981-2010',]
   # precip_avg = precip_avg[!is.na(precip_avg[,1]),]
   # precip_normal = data.frame(as.numeric(precip_avg[,2:13]))
   # names(precip_normal)[1] = 'precip_normal'
   # precip_normal$date = seq(as.Date(paste0(YEAR,"-01-01")),length = 12,by="months")
   return(precip_normal)
}



arrange_weather_data <- function(city, year = 2018){
   bb = city %>% 
      group_by(month, day) %>% 
      filter(temp_max == max(temp_max, na.rm=TRUE)) %>% 
      rename(temp_rec_high = temp_max) %>% 
      select(day, month, date, temp_rec_high)
   
   city = merge(city, bb[,c('date', 'temp_rec_high')], by = 'date', all.x = TRUE)
   
   bb = city %>% 
      group_by(month, day) %>% 
      filter(temp_min == min(temp_min, na.rm=TRUE)) %>% 
      rename(temp_rec_low = temp_min) %>% 
      select(day, month, date, temp_rec_low)
   
   city = merge(city, bb[,c('date', 'temp_rec_low')], by = 'date', all.x=TRUE)
   
   avgs = city %>% 
      filter(year>= year - AVG_YEARS) %>%
      group_by(day, month) %>% 
      summarise(temp_avg_max = mean(temp_max, na.rm=TRUE),
                temp_avg_min = mean(temp_min, na.rm=TRUE),
                temp_rec_max = max(temp_max, na.rm=TRUE),
                temp_rec_min = min(temp_min, na.rm=TRUE))
   
   city = merge(city,avgs, by = c('month', 'day'))
   
   city = city[order(city$date,decreasing = TRUE),]
   
   data = city %>% filter(year == YEAR)
   #data = city[city$year == year,]
   
   data <- mutate(data, dt = highcharter::datetime_to_timestamp(date))
   data = data[order(data$date),]
   
   data = data %>% 
      group_by(month) %>% 
      mutate(precip_value  = cumsum(precip_mm))
   
   data = merge(data, precip_normal, by = 'date', all.x = TRUE)
   return(data)
   
}



chart_tufte_weather <- function(data){
   
   data$dt <- datetime_to_timestamp(data$date)
   # прошлый год = data_1
   data_1 = arrange_weather_data(city, year = YEAR-1)
   data_1 = data_1[!is.na(data_1$date),]
   data_1 <- data_1[,c('day', "month", 'temp_avg')]
   names(data_1) <- c('day',"month", 'temp_avg_prev')
   data_1 = merge(data[,c('day',"month", "dt")], data_1, by = c('day', 'month'), all.y=TRUE)
   
   # абсолютный рекорд по дням
   
   
   dtempgather <- data %>% 
      dplyr::select(dt,date,starts_with("temp")) %>% 
      dplyr::select(-temp_rec_high, -temp_rec_low, -temp_diff, -temp_avg) %>% 
      #dplyr::select( -temp_diff, -temp_avg) %>% 
      gather(key, value, -dt, -date) %>% 
      mutate(key = str_replace(key, "temp_", ""),
             value = as.numeric(value)) 
   
   dtempgather$value = round(dtempgather$value,1)
   #  "avg_max" "avg_min" "max"     "min"     "rec_max" "rec_min"
   #summary(as.factor(dtempgather$key))
   
   dtempgather$key[dtempgather$key == 'max'] = 'actual_max'
   dtempgather$key[dtempgather$key == 'min'] = 'actual_min'
   
   dtempspread <- dtempgather %>% 
      separate(key, c("serie", "type"), sep = "_")
   
   dtempgather = unique(dtempgather)
   
   dtempspread <- dtempgather %>% 
      separate(key, c("serie", "type"), sep = "_") %>% 
      spread(type, value) %>% 
      filter(!is.na(max) | !is.na(min))
   
   temps <- dtempspread %>%
      mutate(serie = factor(serie, levels = c("rec", "avg", "actual")),
             serie = fct_recode(serie, Рекордная = "rec", Нормальная = "avg", Фактическая = "actual"))
   
   temps = temps[!is.na(temps$dt),]
   
   colors <- c("#ECEBE3", "#C8B8B9", "#C85C8A")
   #colors <- c("#ECEBE3", "#C8B8B9", "#A90048")
   colors <- colors[which(levels(temps$serie) %in% unique(temps$serie))]
   
   
   hc <- highchart() %>%
      hc_title(text = paste0(CITY, " -  погода в ", YEAR, ' году'), style = list(fontSize = '14px', fontWeight = "bold"), align = "left") %>%
      hc_xAxis(type = "datetime", showLastLabel = FALSE,
               dateTimeLabelFormats = list(month = "%B")) %>% 
      hc_tooltip(shared = TRUE, useHTML = TRUE,
                 headerFormat = as.character(tags$small("{point.x: %b %d}", tags$br()))) %>% 
      hc_plotOptions(series = list(borderWidth = 0, pointWidth = 4))
   
   hc <- hc %>% 
      hc_add_series(temps, type = "columnrange",
                    hcaes(dt, low = min, high = max, group = serie),
                    color = colors) 
   
   #hc
   data = data[complete.cases(data$temp_avg),]
   data_1 = data_1[order(data_1$dt),]
   
   hc <- hc %>% 
      hc_add_series(data, type = "line", hcaes(x = dt, y = temp_avg),
                    name = 'Среднедневная',lineWidth=2, color = 'black') %>% 
      hc_add_series(data_1, type = "line", hcaes(x = dt, y = temp_avg_prev),
                    name = 'Среднедневная год назад',lineWidth=1.5, dashStyle = 'Dash', color = 'grey') 
   #hc
   records <- data %>%
      select(dt, temp_rec_high, temp_rec_low) %>% 
      filter(!is.na(temp_rec_high) | !is.na(temp_rec_low)) %>% 
      #dmap_if(is.character, str_extract, "\\d+") %>% 
      #dmap_if(is.character, as.numeric) %>% 
      gather(type, value, -dt) %>% 
      filter(!is.na(value)) %>% 
      mutate(type = str_replace(type, "temp_rec_", ""),
             type = paste("Рекорд этого года", type))
   
   pointsyles_high <- list(
      symbol = "circle",
      lineWidth= 1,
      radius= 4,
      fillColor= "#bc0909",
      lineColor= "#bc0909"
   )
   
   pointsyles_low <- list(
      symbol = "circle",
      lineWidth= 1,
      radius= 4,
      fillColor= "#0099CC",
      lineColor= "#0099CC"
   )
   
   
   if(nrow(records) > 0) {
      hc <- hc %>% 
         hc_add_series(dplyr::filter(records, type == "Рекорд этого года high"), "point", hcaes(x = dt, y = value, group = type),
                       marker = pointsyles_high, showInLegend = FALSE) %>% 
         hc_add_series(dplyr::filter(records, type == "Рекорд этого года low"), "point", hcaes(x = dt, y = value, group = type),
                       marker = pointsyles_low, showInLegend = FALSE)
   }
   
   #hc
   
   axis <- create_yaxis(
      naxis = 2,
      heights = c(3,1),
      sep = 0.05,
      turnopposite = FALSE,
      showLastLabel = FALSE,
      startOnTick = FALSE)
   
   axis[[1]]$title <- list(text = "Температура, °C")
   axis[[1]]$labels <- list(format = "{value}°C")
   
   axis[[2]]$title <- list(text = "Осадки, мм")
   axis[[2]]$min <- 0
   
   hc <- hc_yAxis_multiples(hc, axis)
   
   precip <- select(data, dt, precip_value, month)
   n_months = max(data$month)
   
   hc <- hc %>%
      hc_add_series(precip, type = "area", hcaes(dt, precip_value, group = month),
                    name = "Осадки", color = "#008ED0", lineWidth = 1,
                    yAxis = 1, fillColor = "#EBEAE2", 
                    id = c("p", rep(NA, n_months-1)), linkedTo = c(NA, rep("p", n_months-1)))
   n_months = 12
   precip_normal$month = 1:12
   
   bb = seq(min(data$date),length=13,by="months")-1
   bb = bb[2:13]
   buff = precip_normal
   buff$date = bb
   precip_normal = rbind(buff, precip_normal)
   precip_normal$dt = datetime_to_timestamp(precip_normal$date)
   
   hc <- hc %>% 
      hc_add_series(precip_normal, "line", hcaes(x = dt, y = precip_normal, group = month), 
                    name = "Normal Precipitation", color = "#008ED0", yAxis = 1, showInLegend = FALSE,
                    id = c("np", rep(NA, n_months - 1)), linkedTo = c(NA, rep("np", n_months - 1)),
                    lineWidth = 2, marker = FALSE)
   #hc %>% hc_plotOptions(series = list(marker = list(enabled = FALSE)))
   
   
   hc$x$conf_opts$lang$months = c("Январь"	,"Февраль",	"Март"	,"Апрель"	,"Май",	"Июнь",
                                  "Июль",	"Август",	"Сентябрь",	"Октябрь",	"Ноябрь",	"Декабрь")
   
   hc$x$conf_opts$lang$shortMonths = c("Янв", 	"Фев",	"Мар",	"Апр",	"Май",	"Июн",	"Июл",	"Авг",	"Сен",	"Окт",	"Ноя",	"Дек")
   return(hc)
   
}


