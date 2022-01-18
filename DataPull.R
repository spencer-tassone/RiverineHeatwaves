library(dataRetrieval)
library(data.table)
library(dplyr)
library(lubridate)
library(geojsonio)
library(sp)
library(readr)

rm(list = ls())
dev.off()

### Find stations with available daily mean water temperature data between 1996-2021
statCd <- "00003" # statistics parameter code = mean
startDate <- "1996-01-01"
endDate <- "2021-12-31"
states <- stateCd[1:51,]
result <- vector('list', nrow(states))
for(i in 1:nrow(states)) {
  print(i)
  tempDat_states = whatNWISdata(stateCd = states$STUSAB[i],parameterCd = "00010") #water temperature(C)
  temp_states = tempDat_states %>%
    filter(stat_cd == "00003") %>% #mean
    mutate(period = as.Date(endDate) - as.Date(startDate))
  temp_states$end_year <- year(temp_states$end_date)
  temp_states$start_year <- year(temp_states$begin_date)
  temp_states <- temp_states[temp_states$start_year <= 1995 & temp_states$end_year >= 2021,]
  result[[i]] <- temp_states
} # Takes a while to run ~10-15 min
full_station_list <- do.call(rbind.data.frame, result)
full_station_list$site_no_chr <- as.character(full_station_list$site_no)
full_station_list$Order <- seq(from = 1, to = nrow(full_station_list), by = 1)
full_station_list <- full_station_list[!duplicated(full_station_list$site_no_chr),] # All available USGS stations with daily mean water temperature data between 1996-2020 
rm(list=setdiff(ls(), "full_station_list")) # 284 stations

### Extract daily mean water temperature from full_station_list. Also takes a while to run ~1 hour
startDate <- "1996-01-01"
endDate <- "2021-12-31"
dat_final_large <- readNWISdv(siteNumbers = full_station_list$site_no, # readNWISdv pulls daily mean values
                              parameterCd = "00010", # water temperature (C)
                              startDate = startDate,
                              endDate = endDate) 
dat_final_large <- renameNWISColumns(dat_final_large)
Wtemp_daily <- dat_final_large[,1:5]

startDate <- as.Date("1996-01-01")
endDate <- as.Date("2021-12-31")
full_ts <- as.data.frame(rep(seq(from = startDate, to = endDate, by = "day"),times = 284))
colnames(full_ts)[1] <- "Date"
full_site <- as.data.frame(rep(unique(Wtemp_daily$site_no),times = 9497))
colnames(full_site)[1] <- "site_no"
site_ts <- cbind(full_ts, full_site)
Wtemp_daily <- merge(site_ts, Wtemp_daily, by = c("site_no","Date"), all = TRUE)

### Determine how much Wtemp data is missing for each station
missing_data <- Wtemp_daily %>%
  group_by(site_no) %>%
  summarise(Total_Wtemp_DataAvail = sum(!is.na(Wtemp)))

missing_data$Frac_Wtemp_Avail <- round(missing_data$Total_Wtemp_DataAvail/9497,2) # There are 9,497 days between 12/31/2021 - 1/1/1996

threshold <- 0.75
keep_sites_temp <- subset(missing_data, missing_data[,3] > threshold) # 132 stations

### Remove stations that are tidally influenced or are lakes

remove_sites <- keep_sites_temp[c(15,20:23,31:34,71:73,77),]
keep_sites_temp <- keep_sites_temp[!(keep_sites_temp$site_no %in% remove_sites$site_no),] # 119 stations

### Append pertinent information to sites that you actually need data for
nm <- c("station_nm", "dec_lat_va", "dec_long_va", "alt_va", "alt_datum_cd", "STUSAB")
keep_sites_temp[nm] <- lapply(nm, function(x) full_station_list[[x]][match(keep_sites_temp$site_no, full_station_list$site_no)])
rm(list=setdiff(ls(), c("keep_sites_temp", "full_station_list", "startDate", "endDate", "nm", "Wtemp_daily")))

### Keep only Wtemp data for stations that have > 75% of data available,
### then remove data that is not Approved (A), Approved Revised (A R), Approved Edited (A e) or Provisional (P)
Wtemp_daily_dat <- Wtemp_daily[Wtemp_daily$site_no %in% keep_sites_temp$site_no,]
Wtemp_daily_dat$Wtemp[Wtemp_daily_dat$Wtemp >= 50] <- NA
table(Wtemp_daily_dat$Wtemp_cd)
Wtemp_daily_dat$Wtemp[Wtemp_daily_dat$Wtemp_cd == "A [4]"] <- NA
Wtemp_daily_dat$Wtemp[Wtemp_daily_dat$Wtemp_cd == "A <"] <- NA
Wtemp_daily_dat$Wtemp[Wtemp_daily_dat$Wtemp_cd == "P ***"] <- NA
Wtemp_daily_dat$Wtemp[Wtemp_daily_dat$Wtemp_cd == "P [4]"] <- NA
Wtemp_daily_dat$Wtemp[Wtemp_daily_dat$Wtemp_cd == "P Dis"] <- NA
Wtemp_daily_dat$Wtemp[Wtemp_daily_dat$Wtemp_cd == "P Eqp"] <- NA
Wtemp_daily_dat$Wtemp[Wtemp_daily_dat$Wtemp_cd == "P Mnt"] <- NA

### Get station latitude and longitude

lat_long <- keep_sites_temp %>%
  group_by(site_no) %>%
  summarise(lat = mean(dec_lat_va),
            long = mean(dec_long_va))

### Add state name and abbreviation to lat_long
usa <- geojson_read(
  "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json", 
  what = "sp"
)

lat_long$state <- NA

for (i in 1:nrow(lat_long)) {
  coords <- c(lat_long$long[i], lat_long$lat[i])
  if(any(is.na(coords))) next
  point <- sp::SpatialPoints(
    matrix(
      coords,
      nrow = 1
    )
  )
  sp::proj4string(point) <- sp::proj4string(usa)
  polygon_check <- sp::over(point, usa)
  lat_long$state[i] <- as.character(polygon_check$NAME)
}

lat_long$STUSAB <- state.abb[match(lat_long$state, state.name)]
table(lat_long$STUSAB)
nrow(table(lat_long$STUSAB))

setwd("F:/School/USGSdata/GitHub")
write.csv(lat_long, 'UPDATE_119USGSsites_26YearWTemp_LatLong.csv')

### Extract daily mean discharge (Q) from keep_sites_temp. Takes ~10-15 min to run.
Q_daily_dat <- readNWISdv(siteNumbers = keep_sites_temp$site_no,
                             parameterCd = "00060",
                             startDate = startDate,
                             endDate = endDate)
Q_daily_dat <- renameNWISColumns(Q_daily_dat)
Q_daily_dat$Flow[Q_daily_dat$Flow_cd < 0] <- NA
table(Q_daily_dat$Flow_cd)
Q_daily_dat$Flow[Q_daily_dat$Flow_cd == "A >"] <- NA
Q_daily_dat$Flow[Q_daily_dat$Flow_cd == "P e"] <- NA
Q_daily_dat$Flow[Q_daily_dat$Flow_cd == "P Ice"] <- NA
Q_daily_dat$flow_cms <- round(Q_daily_dat$Flow*0.0283168,2)
Q_daily_dat <- Q_daily_dat[,c(1:3,6,5)]

### Determine how much Q data is missing for each station

startDate <- as.Date("1996-01-01")
endDate <- as.Date("2021-12-31")
full_ts <- as.data.frame(rep(seq(from = startDate, to = endDate, by = "day"),times = 109))
colnames(full_ts)[1] <- "Date"
full_site <- as.data.frame(rep(unique(Q_daily_dat$site_no),times = 9497))
colnames(full_site)[1] <- "site_no"
site_ts <- cbind(full_ts, full_site)
Q_daily_dat <- merge(site_ts, Q_daily_dat, by = c("site_no","Date"), all = TRUE)

missing_data <- Q_daily_dat %>%
  group_by(site_no) %>%
  summarise(Total_Q_DataAvail = sum(!is.na(flow_cms)))
missing_data$Frac_Wtemp_Avail <- round(missing_data$Total_Q_DataAvail/9497,2) # There are 9,497 days between 12/31/2021 - 1/1/1996

threshold <- 0.75
keep_sites_Q <- subset(missing_data, missing_data[,3] > threshold) # 104 stations

Q_daily_dat <- Q_daily_dat[Q_daily_dat$site_no %in% keep_sites_Q$site_no,]

station_list <-  full_station_list[(full_station_list$site_no %in% Wtemp_daily_dat$site_no),]
station_list <- station_list[,c(1:3,5:12)]
station_list$altitude_ft <- station_list$alt_va
station_list$altitude_ft <- as.numeric(station_list$altitude_ft)
station_list$altitude_ft_NAVD88 <- ifelse(station_list$alt_datum_cd == "NGVD29",
                                  station_list$altitude_ft + 3.6, ### see USGS report https://pubs.usgs.gov/sir/2010/5040/section.html 
                                  station_list$altitude_ft)
station_list$altitude_m_NAVD88 <- round(station_list$altitude_ft_NAVD88 * 0.3048, digits = 2) ### convert feet to meters

station_details <- read.csv('Station_Details.csv')
station_details$site_no <- as.character(station_details$site_no)
station_details <- station_details %>%
  mutate(site_no = ifelse(row_number()<=86, paste0("0", site_no), site_no))
station_details[132,2] <- "420451121510000"
station_details$DrainageArea_km2 <- station_details$DrainageArea_mi2 * 2.59

station_details <- station_details[station_details$site_no %in% station_list$site_no,]
station_details <- merge(station_details, station_list, by = "site_no")
station_details <- station_details[,c(25,1,3,4,8:23,36)]

write.csv(station_details, 'station_details_corrected.csv')
write.csv(Wtemp_daily_dat,'Wtemp_daily_dat.csv')
write.csv(Q_daily_dat,'Q_daily_dat.csv')
