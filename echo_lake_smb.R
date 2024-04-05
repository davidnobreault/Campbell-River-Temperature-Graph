#performed by D. Breault 2024 on temperature data from Campbell River and Echo Lake (SMB Removal)

#set up workspace ----

.libPaths("C:/R/library")

#set working directory

setwd(("C:/R/projects/echo_lake"))

#remove any objects from previous analysis

rm(list = ls())

#Install and load packages ----

install.packages("dplyr")

install.packages("data.table")

install.packages('gmodels') #crosstable

install.packages("ggrepel")

install.packages("descr")

install.packages("car")

install.packages("lubridate")

install.packages("ggpubr") #for arranging multiple ggplots together

install.packages("devtools")

install.packages("mvnormtest") #MANOVA; Multibariate test of normality (mshapiro.test)

install.packages("PMCMRplus") #Kruskal-Walis

#Load packages

library("dplyr")

library("data.table")

library('gmodels')

library("ggrepel")

library("descr")

library("car")

library("lubridate")

library('ggpubr')

library('devtools')

library('mvnormtest') 

library('PMCMRplus')

#Plot Campbell River and Echo Lake hourly water temperatures together----

#convert 2024 Echo Lake data from 10-min intervals to hourly

rm(list = ls())

dat.echo <- read.csv("data/echo_lake_temp_2.csv", header = TRUE)

dat.echo <- dat.echo %>% 
              mutate(date_time = mdy_hms(date_time), Date = as.Date(date_time), Hour = hour(date_time))

dat.echo.hour <- dat.echo %>% 
                    group_by(Date, Hour) %>% 
                        summarize(Avg = mean(temp), Count = n())

#combine date and hour into one column

dat.echo.hour$Date <- as.Date(dat.echo.hour$Date, format =  "%Y/%m/%d")

dat.echo.hour$date_hour <- ymd_h(paste(dat.echo.hour$Date, dat.echo.hour$Hour))

dat.echo.hour <- dat.echo.hour[, c("date_hour", "Avg")]

names(dat.echo.hour)[names(dat.echo.hour) == 'Avg'] <- 'temp'

dat.echo.day <- dat.echo.hour %>% 
                        group_by(date = date(date_hour)) %>% 
                        summarize(temp = mean(temp, na.rm = TRUE))

dat.echo.day$series <- c("Echo Lake Water (2024)")

#read in 2024 campbell river water temp data

dat.cr <- read.csv("data/campbell_river_temp.csv", header = TRUE)

#remove data prior to start of Echo Lake data

dat.cr <- dat.cr[-c(1:13), ]

#format date_time as Date (POSIXct) rather than character

head(dat.cr)

dat.cr$date_hour <- ymd_hm(dat.cr$ï..date_hour)

dat.cr <- dat.cr[, c("date_hour", "temp")]

dat.cr.day <- dat.cr %>% 
                    group_by(date = date(date_hour)) %>% 
                    summarize(temp = mean(temp, na.rm = TRUE))

dat.cr.day$series <- c("Campbell River Water (2024)")

#read in 2023 campbell river water temp data

dat.cr23 <- read.csv("data/campbell_river_temp_2023.csv", header = TRUE)

dat.cr23$date_hour <- ymd_hm(dat.cr23$ï..date_hour)

dat.cr23 <- dat.cr23[, c("date_hour", "temp")]

dat.cr23 <- dat.cr23[-c(1:13), ]

dat.cr23.day <- dat.cr23 %>% 
                    group_by(date = date(date_hour)) %>% 
                    summarize(temp = mean(temp, na.rm = TRUE))

dat.cr23.day$series <- c("Campbell River Water (2023)")

#read in 2024 campbell river air temperature data

dat.air <- read.csv("data/en_climate_hourly.csv", header = TRUE)

dat.air$date_hour <- ymd_hm(dat.air$date_time)

dat.air <- dat.air[, c("date_hour", "temp")]

dat.air <- dat.air[-c(1:925), ]

dat.air.day <- dat.air %>% 
                        group_by(date = date(date_hour)) %>% 
                        summarize(temp = mean(temp, na.rm = TRUE))

dat.air.day$series <- c("Campbell River Air (2024)")

#read in 2023 campbell river air temperature data

dat.air.23 <- read.csv("data/en_climate_hourly_2023.csv", header = TRUE)

head(dat.air.23)

dat.air.23$date_hour <- ymd_hm(dat.air.23$Date.Time..LST.)

dat.air.23$temp <- dat.air.23$Temp..Â.C.

dat.air.23 <- dat.air.23[, c("date_hour", "temp")]

dat.air.23 <- dat.air.23[-c(1:925), ]

dat.air.23.day <- dat.air.23 %>% 
                        group_by(date = date(date_hour)) %>% 
                        summarize(temp = mean(temp, na.rm = TRUE))

dat.air.23.day$series <- c("Campbell River Air (2023)")

#combine all time series into one dataframe

dat.all <- rbind(dat.echo.day, dat.cr.day, 
                 dat.cr23.day, dat.air.day, dat.air.23.day) 

dat.all$date <- ymd(dat.all$date)

#merge time series together

#dat.echo.cr <- merge(dat.echo.hour, dat.cr, by.x = "date_hour", 
#                     by.y = "date_hour", all.x = TRUE, all.y = FALSE)
#
#dat.echo.cr.23.24 <- merge(dat.echo.cr, dat.cr23, by.x = "date_hour", 
#                           by.y = "date_hour", all.x = TRUE, all.y = TRUE)
#
#dat.echo.cr.23.24.air <- merge(dat.echo.cr.23.24, dat.air, by.x = "date_hour", 
#                               by.y = "date_hour", all.x = TRUE, all.y = TRUE)
#
#dat.echo.cr.23.24.air2 <- merge(dat.echo.cr.23.24.air, dat.air.23, by.x = "date_hour", 
#                               by.y = "date_hour", all.x = TRUE, all.y = TRUE)

#rename temperature columns

#dat.echo.cr.23.24.air2 <- dat.echo.cr.23.24.air2 %>% 
#                          rename(date_time = date_hour, echo_temp = Avg, 
#                                 cr_temp_24 = temp.x, cr_temp_23 = temp.y, 
#                                 cr_air_temp_24 = temp, cr_air_temp_23 = temp23)

#plot daily average temperature

#daily <- dat.echo.cr.23.24.air2 %>% 
#              group_by(date = date(date_time)) %>% 
#              summarize(echo_temp = mean(echo_temp, na.rm = TRUE), 
#                        cr_temp_24 = mean(cr_temp_24, na.rm = TRUE),
#                        cr_temp_23 = mean(cr_temp_23, na.rm = TRUE), 
#                        cr_air_temp_24 = mean(cr_air_temp_24, na.rm = TRUE),
#                        cr_air_temp_23 = mean(cr_air_temp_23, na.rm = TRUE))


#plot all time series together

echo_cr_temp <- ggplot() +
  
  geom_line(data = dat.all, aes(x = as.POSIXct(date), y = temp, 
                              color = series), size = 1) +
  
#  geom_line(data = daily, aes(x = date, y = cr_temp_24, 
#                              color = "Campbell River Water (2024)"), color = "chocolate") +
#  geom_line(data = daily, aes(x = date, y = cr_temp_23, 
#                              color = "Campbell River Water (2023)"), color = "darkgreen") +
#  geom_line(data = daily, aes(x = date, y = cr_air_temp_24, 
#                              color = "Campbell River Air (2024)"), color = "orchid1") +
#  geom_line(data = daily, aes(x = date, y = cr_air_temp_23, 
#                              color = "Campbell River Air (2023)"), color = "chartreuse") +
  
 scale_y_continuous(breaks = seq(-6, 32, 2), limits = c(-6, 32), expand = c(0, 0)) +
 scale_x_datetime(date_breaks = "1 week",
                  date_labels = "%b %d",
                  limits = c(as.POSIXct("2024-02-08"),
                              as.POSIXct("2024-06-30"))) +
                     
#  scale_colour_manual(labels = c("Echo Lake Water (2024)", "Campbell River Water (2024)", 
#                                 "Campbell River Water (2023)", "Campbell River Air (2024)",
#                                 "Campbell River Air (2023)"), 
#                      values = c( "blue4"  ,    "chocolate" ,  "darkgreen" ,  "orchid1" ,  "chartreuse"   )) +
  
  geom_hline(yintercept = 15, linetype = "dashed", color = "red", size = 1) +
  
  ylab(expression(paste('Air and Water Temperature (',~degree,'C)',sep=''))) +
  xlab("Date") +
  labs(title = "Daily Air and Water Temperature (C)", 
       subtitle = expression(paste("in Echo Lake and the Campbell River"))) +
  theme(plot.title = element_text(size=16, face = "bold"),
        plot.subtitle = element_text(size=16, face = "bold"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x= element_text(colour = "black", size = "12"),
        axis.text.y= element_text(colour = "black", size = "12"),
        legend.position = c(0.88, .175), legend.direction = "vertical",
        legend.spacing = unit(0, 'cm'),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(colour = "black", size = "18", face = "bold"),
        legend.text = element_text(colour = "black", size = "14"))

  echo_cr_temp <- echo_cr_temp + guides(color = guide_legend(title = "Time Series"))  
  
  tiff(file = "figures/daily_air_water_temp_echo_cr_20240404.tif", 
       units = "cm", width = 60, height = 30, res = 300)

echo_cr_temp

dev.off()




# test linear correlation between Temperature and Time

cor(dat.echo.cr.23.24$cr_temp_23, dat.echo.cr.23.24$date_time) #y must be numeric

