# Chapter 1: Fuel log loading
fuel_path <- "D:/DS/Crane/Fuel Data - Sheet1.csv"
fuel_dat <- read.csv(fuel_path)


fuel_fill <-
  fuel_dat %>% 
  dplyr::filter(row_number() <= 35 & row_number() >= 10) %>%  #L&T - RREC	LE180902 - MNEP	MNEP- Camp 1 FUEL SENSOR VEHICLES	NL01AA1712
  dplyr::select(c(5,6)) %>% 
  dplyr::rename(start_time = 1, end_time = 2)

fuel_fill$start_time <- lubridate::dmy_hm(fuel_fill$start_time, tz = Sys.timezone() )
fuel_fill$end_time <- lubridate::dmy_hm(fuel_fill$end_time, tz = Sys.timezone() )


# Chapter 2: Raw data loading from sensor
rawdata_path <- "D:/DS/Crane/Crane/NL01AA1712_ST_02-01-2020T02-27_ET_03-01-2020T02-27.csv"
rawdata <- read.csv(rawdata_path)
rawdata$date_time <- lubridate::dmy_hms(rawdata$Date.IST, tz = Sys.timezone())




# Chapter 3: Merging of raw data and fuel_filling log
merged <- rawdata %>% 
  mutate(dummy = T) %>% 
  dplyr::left_join(fuel_fill %>% mutate(dummy = T)) %>% 
  dplyr::filter(date_time <= end_time & date_time >= start_time)


# Chapter 4: Detection of 'fill'















