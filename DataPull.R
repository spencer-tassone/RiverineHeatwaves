library(dataRetrieval)
library(data.table)
library(dplyr)
library(lubridate)
library(geojsonio)
library(sp)

rm(list = ls())
dev.off()

### Find stations with available daily mean water temperature data between 1996-2020
statCd <- "00003" # statistics parameter code = mean
startDate <- "1996-01-01"
endDate <- "2020-12-31"
states <- stateCd[1:51,]
result <- vector('list', nrow(states))
for(i in 1:nrow(states)) {
  print(i)
  tempDat_states = whatNWISdata(stateCd = states$STUSAB[i],parameterCd = "00010") # water temperature (C)
  temp_states = tempDat_states %>%
    filter(stat_cd == "00003") %>%
    mutate(period = as.Date(endDate) - as.Date(startDate))
  temp_states$end_year <- year(temp_states$end_date)
  temp_states$start_year <- year(temp_states$begin_date)
  temp_states <- temp_states[temp_states$start_year <= 1995 & temp_states$end_year >= 2020,]
  result[[i]] <- temp_states
} # Takes a while to run ~10-15 min
full_station_list <- do.call(rbind.data.frame, result)
full_station_list$site_no_chr <- as.character(full_station_list$site_no)
full_station_list$Order <- seq(from = 1, to = nrow(full_station_list), by = 1)
full_station_list <- full_station_list[!duplicated(full_station_list$site_no_chr),] # All available USGS stations with daily mean water temperature data between 1996-2020 
rm(list=setdiff(ls(), "full_station_list")) # 291 stations

### Extract daily mean water temperature from full_station_list. Also takes a while to run ~1 hour
startDate <- "1996-01-01"
endDate <- "2020-12-31"
dat_final_large <- readNWISdv(siteNumbers = full_station_list$site_no, # readNWISdv pulls daily mean values
                              parameterCd = "00010", # water temperature (C)
                              startDate = startDate,
                              endDate = endDate) 
dat_final_large <- renameNWISColumns(dat_final_large)
Wtemp_daily <- dat_final_large[,1:5]

### Determine how much Wtemp data is missing for each station
missing_data <- Wtemp_daily %>%
  group_by(site_no) %>%
  summarise(Total_Wtemp_DataAvail = sum(!is.na(Wtemp)))

missing_data$Frac_Wtemp_Avail <- round(missing_data$Total_Wtemp_DataAvail/9131,2) # There are 9,131 days between 12/31/2020 - 1/1/1996

threshold <- 0.75
keep_sites_temp <- subset(missing_data, missing_data[,3] > threshold) # 131 stations

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

### Get station latitutde and longitude

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
lat_long[15,4] <- "Delaware"
lat_long[15,5] <- "DE"
lat_long[70,4] <- "Louisiana"
lat_long[70,5] <- "LA"
table(lat_long$STUSAB)
nrow(table(lat_long$STUSAB))                             
                              
setwd("D:/School/USGSdata/GitHub")
# write.csv(lat_long, '131USGSsites_25YearWTemp_LatLong.csv')

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
missing_data <- Q_daily_dat %>%
  group_by(site_no) %>%
  summarise(Total_Q_DataAvail = sum(!is.na(flow_cms)))
missing_data$Frac_Wtemp_Avail <- round(missing_data$Total_Q_DataAvail/9131,2) # There are 9,131 days between 12/31/2020 - 1/1/1996

threshold <- 0.75
keep_sites_Q <- subset(missing_data, missing_data[,3] > threshold) # 104 stations

Q_daily_dat <- Q_daily_dat[Q_daily_dat$site_no %in% keep_sites_Q$site_no,]

write.csv(Wtemp_daily_dat,'Wtemp_daily_dat.csv')
write.csv(Q_daily_dat,'Q_daily_dat.csv')
