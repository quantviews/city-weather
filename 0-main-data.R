#rm(list=ls()) # remove all vars 
#rm(list = setdiff(ls(), lsf.str())) remove all variables except functions

source('weather-functions.R',encoding = 'UTF-8')
cities <- as.tibble(readxl::read_xlsx('cities.xlsx'))
cities$meteo_station <- as.character(cities$meteo_station)
stations <-  cities$meteo_station

AVG_YEARS = 15
YEAR = as.numeric(format(Sys.Date(), '%Y'))

#create list of hc's
hc_list <- vector(mode="list", length = length(stations))
names(hc_list) <- stations

for(i in 1:length(stations)){
   ID <-  cities$meteo_station[i]
   cat(cities$city[i], '\n')
   CITY <- cities$city[i]
   city = munge_weather_data(ID)
   precip_normal = get_precip_data(ID)
   data = arrange_weather_data(city = city, year = YEAR)
   hc  = chart_tufte_weather(data)
   cur_date <- data$date[which(is.na(data$temp_avg))[1]]
   hc  = hc_subtitle(hc, text = paste0(' по состоянию на ', cur_date), align = "left")
   hc = hc %>% hc_exporting(enabled = TRUE)
   #hc_list = hc_list + hc
   hc_list[[i]] = hc
   
   #cities$hc[i] = hc
   
}

save.image('data.RData')
# hc_list[4]
# 
# 
# 
# library(ggplot2)
# library(lubridate)
# city$yday = yday(city$date)
# city = city %>% 
#    filter(!is.na(year))
# city$current_year = city$year == 2018
# p = ggplot(city, aes(yday, temp_avg, group = year))
# 
# p + geom_line(aes(color=current_year, size = current_year ))+theme_minimal()+scale_alpha("year")+
#    scale_colour_manual(values = c("TRUE" = "#358a81", "FALSE" = 'grey85'))+
#    scale_size_manual(values = c("TRUE" = 1.0, "FALSE" = 0.2))+
#    theme(legend.position="none")+ylab('РіСЂР°РґСѓСЃРѕРІ Р¦РµР»СЊСЃРёСЏ')+xlab(NULL)+
#    ggtitle('РўРµРјРїРµСЂР°С‚СѓСЂР° РІ РњРѕСЃРєРІРµ РІ 2018 РіРѕРґСѓ')+
#    theme(axis.title.x=element_blank(),
#          axis.text.x=element_blank(),
#          axis.ticks.x=element_blank())+
#    theme(panel.grid.major.x = element_blank(),
#          panel.grid.minor.x = element_blank(),
#          panel.grid.minor.y = element_blank(),
#          panel.grid.major.y = element_blank()) 
# 
# ggsave('fig/moscow1.jpg', units = 'cm', width = 20, height = 10, dpi = 300)
# 
# city2 = city %>% 
#    filter(!is.na(year)) %>% 
#    filter(year>=2015)
# 
# hc2 <- highchart() %>% 
#    #hc_xAxis() %>% 
#    hc_add_series(data = city2, type = "line", hcaes(y=temp_avg, x=yday, group=year))
#    
# hc2$
# 
# hc2
