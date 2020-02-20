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
rawdata_path <- "D:/DS/Crane/Crane/NL01AA1712_ST_01-01-2020T00-00_ET_31-01-2020T00-00.csv"
rawdata <- read.csv(rawdata_path)
rawdata$date_time <- lubridate::dmy_hms(rawdata$Date.IST, tz = Sys.timezone())




# Chapter 3: Merging of raw data and fuel_filling log
merged <- rawdata %>% 
  mutate(dummy = T) %>% 
  dplyr::left_join(fuel_fill %>% mutate(dummy = T)) %>% 
  dplyr::filter(date_time <= end_time & date_time >= start_time)

merged2 <- merged %>% 
  dplyr::select(date_time, start_time, end_time) %>% 
  mutate(fill_indicator = 1)


rawdata_final <- rawdata %>% 
  dplyr::left_join(merged2, by = "date_time" ) %>% 
  mutate(fill_indicator = if_else(is.na(fill_indicator), 0, 1))





# Chapter 4: Detection of 'fill' -- To be Done
numeic_col <- sapply(rawdata_final, is.numeric)
train <- rawdata_final %>% dplyr::filter(date_time <= "2020-01-20 23:59:59")
test <- rawdata_final %>% dplyr::filter(date_time > "2020-01-20 23:59:59")

trainf <- train[,numeic_col]
testf <- test[, numeic_col]

library(randomForest)

rf <- randomForest::randomForest(as.factor(fill_indicator) ~ ., data = trainf )






















