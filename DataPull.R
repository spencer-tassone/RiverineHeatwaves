library(dataRetrieval)
library(data.table)
library(dplyr)
library(lubridate)
library(geojsonio)
library(sp)
library(readr)
library(ggplot2)

rm(list = ls())
dev.off()

### Find stations with available daily mean water temperature data between 1996-2021
statCd <- "00003" # statistics parameter code = mean
startDate <- "1996-01-01"
endDate <- "2021-12-31"
states <- stateCd[2:52,]
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

### Extract state name and abbreviation based on each sites lat and long
usa <- geojson_read(
  "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json", 
  what = "sp"
)

for (i in 1:nrow(full_station_list)) {
  coords <- c(full_station_list$dec_long_va[i], full_station_list$dec_lat_va[i])
  if(any(is.na(coords))) next
  point <- sp::SpatialPoints(
    matrix(
      coords,
      nrow = 1
    )
  )
  sp::proj4string(point) <- sp::proj4string(usa)
  polygon_check <- sp::over(point, usa)
  full_station_list$state[i] <- as.character(polygon_check$NAME)
}
full_station_list$STUSAB <- state.abb[match(full_station_list$state,state.name)]
rm(list=setdiff(ls(), "full_station_list")) # 290 stations

### Extract daily mean water temperature from full_station_list. Also takes a while to run ~1-1.5 hours
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
NROW(unique(Wtemp_daily$site_no)) # 289
full_ts <- as.data.frame(rep(seq(from = startDate, to = endDate, by = "day"), times = 289))
colnames(full_ts)[1] <- "Date"
length(seq(from = startDate, to = endDate, by = 'day'))
full_site <- as.data.frame(rep(unique(Wtemp_daily$site_no),times = 9497))
colnames(full_site)[1] <- "site_no"
site_ts <- cbind(full_ts, full_site)
Wtemp_daily <- merge(site_ts, Wtemp_daily, by = c("site_no","Date"), all = TRUE)

### Remove data that is not Approved (A), Approved Revised (A R), Approved Edited (A e) or Provisional (P)
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp >= 50] <- NA
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp < 0] <- 0
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp_cd == "A [4]"] <- NA
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp_cd == "A <"] <- NA
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp_cd == "P ***"] <- NA
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp_cd == "P [4]"] <- NA
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp_cd == "P Dis"] <- NA
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp_cd == "P Eqp"] <- NA
Wtemp_daily$Wtemp[Wtemp_daily$Wtemp_cd == "P Mnt"] <- NA

### Linear interpolate for data gaps less than or equal to 2 days

library(zoo)

interpolate <- function(df){
  df$Wtemp_int <- na.approx(df$Wtemp, maxgap = 2, na.rm = T)
  return(df)
}

Wtemp_daily <- interpolate(Wtemp_daily)
Wtemp_daily$Wtemp <- ifelse(is.na(Wtemp_daily$Wtemp),Wtemp_daily$Wtemp_int, Wtemp_daily$Wtemp)

detach("package:zoo", unload = TRUE)

### Determine how much Wtemp data is missing for each station
missing_data <- Wtemp_daily %>%
  group_by(site_no) %>%
  summarise(Total_Wtemp_DataAvail = sum(!is.na(Wtemp)))

missing_data$Frac_Wtemp_Avail <- round(missing_data$Total_Wtemp_DataAvail/9497,2) # There are 9,497 days between 12/31/2021 - 1/1/1996

threshold <- 0.90
keep_sites_temp <- subset(missing_data, missing_data[,3] > threshold) # 93 stations & 0.90

### Remove stations that are tidally influenced or are lakes

remove_sites <- data.frame(c("01482800","02110704","02110777","02110802","02110815","02172020","02172040",
                             "02172050","02172053","07374526","07381328","08017044","08067118"))
colnames(remove_sites)[1] <- "site_no"
keep_sites_temp <- keep_sites_temp[!(keep_sites_temp$site_no %in% remove_sites$site_no),] # 82 stations @ 0.90
Wtemp_daily <- Wtemp_daily[Wtemp_daily$site_no %in% keep_sites_temp$site_no,]

### Append pertinent information to sites that you actually need data for
nm <- c("station_nm", "dec_lat_va", "dec_long_va", "alt_va", "alt_datum_cd", "STUSAB")
keep_sites_temp[nm] <- lapply(nm, function(x) full_station_list[[x]][match(keep_sites_temp$site_no, full_station_list$site_no)])

### Get station latitude and longitude

lat_long <- keep_sites_temp %>%
  group_by(site_no) %>%
  summarise(lat = mean(dec_lat_va),
            lon = mean(dec_long_va))

station_details <-  full_station_list[(full_station_list$site_no %in% Wtemp_daily$site_no),]
station_details <- station_details[,c(1:3,31,5:6,8:9,11)]
station_details$altitude_ft <- station_details$alt_va
station_details$altitude_ft <- as.numeric(station_details$altitude_ft)
station_details$altitude_ft_NAVD88 <- ifelse(station_details$alt_datum_cd == "NGVD29",
                                          station_details$altitude_ft + 3.6, ### see USGS report https://pubs.usgs.gov/sir/2010/5040/section.html 
                                          station_details$altitude_ft)
station_details$altitude_m_NAVD88 <- round(station_details$altitude_ft_NAVD88 * 0.3048, digits = 2) ### convert feet to meters

# Grab meteorological data from Daymet web services to build water temperature multiple linear regressions
# https://daac.ornl.gov/
# https://www.nature.com/articles/s41597-021-00973-0#code-availability
library(daymetr)

colnames(lat_long)[1] <- 'site'
write.table(lat_long, paste0(tempdir(),"/lat_long.csv"),
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

met_dat <- download_daymet_batch(file_location = paste0(tempdir(),
                                                        "/lat_long.csv"),
                                 start = 1996,
                                 end = 2021,
                                 internal = TRUE,
                                 simplify = TRUE)
library(tidyr)
met_dat_wide <- spread(met_dat, measurement, value)
met_dat_wide <- met_dat_wide %>%
  mutate(tmean = (tmax..deg.c. + tmin..deg.c.)/2,
         Date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"),
         site_no = ifelse(row_number()<=483990, paste0("0", site), site))
met_dat_wide <- met_dat_wide[,c(17,16,2:5,12,13,15,8:11,14)]
long_name <- met_dat_wide[768691:NROW(met_dat_wide),]
long_name$site_no <- "420451121510000"
met_dat_wide <- met_dat_wide[1:768690,]
met_dat_wide <- rbind(met_dat_wide,long_name)
met_dat_wide$totalRadiation <- (met_dat_wide$srad..W.m.2.*met_dat_wide$dayl..s.)/1000000 # calculation based on daymetr website https://daymet.ornl.gov/overview
wmet <- merge(met_dat_wide, Wtemp_daily, by = c("site_no","Date"), all = T)

# remove leap days
remove_leap <- as.Date(c("1996-02-29","2000-02-29","2004-02-29",
                         "2008-02-29","2012-02-29","2016-02-29","2020-02-29"))
wmet <- wmet[!wmet$Date %in% remove_leap,]

# day of year that does not recognize leap day
wmet <- wmet %>% 
  mutate(DoY = day(Date),
         Month = month(Date),
         Year = year(Date)) %>% 
  group_by(Year, Month, site_no) %>%
  mutate(DoY = DoY - lag(DoY, default = 0)) %>%
  group_by(Year,site_no) %>%
  mutate(DoY = cumsum(DoY)) %>%
  select(-Month)
wmet <- as.data.frame(wmet)

library(broom)

models_fit <- wmet %>%
  group_by(site_no) %>%
  do(model = glance(lm(Wtemp~tmax..deg.c.+tmin..deg.c.+prcp..mm.day.+vp..Pa.+totalRadiation+DoY, data = .))) %>%
  unnest(model)
models_fit$r.squared <- round(models_fit$r.squared, digits = 2)
good_model_fits <- models_fit[models_fit$r.squared >= 0.80,] # 70 sites with r-square >= 0.80
round(mean(good_model_fits$r.squared),digits = 2) # answer is 0.91
round(sd(good_model_fits$r.squared),digits = 2) # answer is 0.04
wmet <- wmet[wmet$site_no %in% good_model_fits$site_no,]
met_dat_wide <- met_dat_wide[met_dat_wide$site_no %in% keep_sites_temp$site_no,]

library(purrr)

f <- function (.fit, .new_data) {
  predict(.fit, newdata = .new_data)
}

set.seed(8992)
wmet <- wmet %>%
  nest(data = -site_no) %>% 
  mutate(
    fit  = map(data, ~ lm(Wtemp~tmax..deg.c.+tmin..deg.c.+prcp..mm.day.+vp..Pa.+totalRadiation+DoY, data = .x)),
    yhat = map2(.x = fit, .y = data, f)
  ) %>% 
  unnest(cols = c(data, yhat)) %>% 
  select(-fit)

wmet$predWtemp <- ifelse(is.na(wmet$Wtemp),wmet$yhat,wmet$Wtemp)
wmet$predWtemp <- round(wmet$predWtemp, digits = 2)
wmet$corWtemp <- ifelse(wmet$predWtemp < 0,0,wmet$predWtemp)

missing_Wtemp <- wmet %>%
  group_by(site_no) %>%
  summarise(Total_Wtemp_DataAvail = sum(!is.na(predWtemp)),
            Frac_Wtemp_DataAvail = round((Total_Wtemp_DataAvail/9497), digits = 3))

### Extract daily mean discharge (Q) from keep_sites_temp. Takes ~10-15 min to run.
keep_sites_temp <- keep_sites_temp[keep_sites_temp$site_no %in% wmet$site_no,]
Q_daily_dat <- readNWISdv(siteNumbers = keep_sites_temp$site_no,
                          parameterCd = "00060",
                          startDate = startDate,
                          endDate = endDate)
Q_daily_dat <- renameNWISColumns(Q_daily_dat)
Q_daily_dat$Flow[Q_daily_dat$Flow_cd < 0] <- NA
# table(Q_daily_dat$Flow_cd)
Q_daily_dat$Flow[Q_daily_dat$Flow_cd == "A >"] <- NA
Q_daily_dat$Flow[Q_daily_dat$Flow_cd == "P e"] <- NA
Q_daily_dat$Flow[Q_daily_dat$Flow_cd == "P Ice"] <- NA
Q_daily_dat$flow_cms <- round(Q_daily_dat$Flow*0.0283168,2)
Q_daily_dat <- Q_daily_dat[,c(1:3,6,5)]

### Determine how much Q data is missing for each station

startDate <- as.Date("1996-01-01")
endDate <- as.Date("2021-12-31")
unique(Q_daily_dat$site_no) # 61 out of 70 sites have concurrent daily discharge data available
full_ts <- as.data.frame(rep(seq(from = startDate, to = endDate, by = "day"),times = 61)) # 61 out of 70 sites have concurrent daily discharge data available
colnames(full_ts)[1] <- "Date"
full_site <- as.data.frame(rep(unique(Q_daily_dat$site_no),times = 9497))
colnames(full_site)[1] <- "site_no"
site_ts <- cbind(full_ts, full_site)
Q_daily_dat <- merge(site_ts, Q_daily_dat, by = c("site_no","Date"), all = TRUE)

missing_data <- Q_daily_dat %>%
  group_by(site_no) %>%
  summarise(Total_Q_DataAvail = sum(!is.na(flow_cms)))
missing_data$Frac_Wtemp_Avail <- round(missing_data$Total_Q_DataAvail/9497,2) # There are 9,497 days between 12/31/2021 - 1/1/1996

threshold <- 0.90
keep_sites_Q <- subset(missing_data, missing_data[,3] > threshold) # 52 stations

Q_daily_dat <- Q_daily_dat[Q_daily_dat$site_no %in% keep_sites_Q$site_no,]

library(wql)

month_mean_wtemp <- wmet %>%
  mutate(Month = month(Date)) %>%
  group_by(site_no, Year, Month) %>%
  summarise(monthly_mean_wtemp = mean(corWtemp, na.rm = TRUE))
wide_month_mean_wtemp <- month_mean_wtemp %>%
  pivot_wider(names_from = site_no, values_from = monthly_mean_wtemp)

wtemp_ts = ts(data = wide_month_mean_wtemp[, 3:72],
              start = c(1996,1),
              end = c(2021,12),
              frequency = 12)
wtemp_seaken <- seaKen(wtemp_ts)
wtemp_seaken <- wtemp_seaken %>%
  data.frame() %>%
  cbind(site_no = row.names(wtemp_seaken),.) %>%
  mutate(Status = if_else(
    p.value < 0.05, "sig", "not sig"))
wtemp_seaken <- wtemp_seaken[,c(1:2,4:6)]
colnames(wtemp_seaken) <- c("site_no","sen.slope.wtemp","p.value.wtemp","miss.wtemp","status.wtemp")

month_mean_atemp <- wmet %>%
  mutate(Month = month(Date)) %>%
  group_by(site_no, Year, Month) %>%
  summarise(monthly_mean_atemp = mean(tmean, na.rm = TRUE))
wide_month_mean_atemp <- month_mean_atemp %>%
  pivot_wider(names_from = site_no, values_from = monthly_mean_atemp)

atemp_ts = ts(data = wide_month_mean_atemp[, 3:72],
              start = c(1996,1),
              end = c(2021,12),
              frequency = 12)
atemp_seaken <- seaKen(atemp_ts)
atemp_seaken <- atemp_seaken %>%
  data.frame() %>%
  cbind(site_no = row.names(atemp_seaken),.) %>%
  mutate(Status = if_else(
    p.value < 0.05, "sig", "not sig"))
atemp_seaken <- atemp_seaken[,c(1:2,4:6)]
colnames(atemp_seaken) <- c("site_no","sen.slope.atemp","p.value.atemp","miss.atemp","status.atemp")

month_mean_q <- Q_daily_dat %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(site_no, Year, Month) %>%
  summarise(monthly_mean_q = mean(flow_cms, na.rm = TRUE))
month_mean_q <- month_mean_q[!month_mean_q$site_no == "02423397",] # site is missing 21 months of data between 2007-2008
wide_month_mean_q <- month_mean_q %>%
  pivot_wider(names_from = site_no, values_from = monthly_mean_q)

q_ts = ts(data = wide_month_mean_q[, 3:53],
          start = c(1996,1),
          end = c(2021,12),
          frequency = 12)
q_seaken <- seaKen(q_ts)
q_seaken <- q_seaken %>%
  data.frame() %>%
  cbind(site_no = row.names(q_seaken),.) %>%
  mutate(Status = if_else(
    p.value < 0.05, "sig", "not sig"))
q_seaken <- q_seaken[,c(1:2,4:6)]
colnames(q_seaken) <- c("site_no","sen.slope.q","p.value.q","miss.q","status.q")

met_dat_precip <- met_dat_wide[met_dat_wide$site_no %in% keep_sites_temp$site_no,]
month_total_precip <- met_dat_precip %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(site_no, Year, Month) %>%
  summarise(monthly_total_precip_mm = sum(prcp..mm.day.))
wide_month_total_precip <- month_total_precip %>%
  pivot_wider(names_from = site_no, values_from = monthly_total_precip_mm)

precip_ts = ts(data = wide_month_total_precip[, 3:72],
               start = c(1996,1),
               end = c(2021,12),
               frequency = 12)
precip_seaken <- seaKen(precip_ts)
precip_seaken <- precip_seaken %>%
  data.frame() %>%
  cbind(site_no = row.names(precip_seaken),.) %>%
  mutate(Status = if_else(
    p.value < 0.05, "sig", "not sig"))
precip_seaken <- precip_seaken[,c(1:2,4:6)]
colnames(precip_seaken) <- c("site_no","sen.slope.precip","p.value.precip","miss.precip","status.precip")

precipQ_trends <- merge(q_seaken,precip_seaken, by = "site_no", all = TRUE)
temp_trends <- merge(wtemp_seaken,atemp_seaken, by = "site_no", all = TRUE)
temp_precipQ_trends <- merge(temp_trends, precipQ_trends, by = "site_no", all = TRUE)

usa_region <- data.frame(matrix(ncol = 2, nrow = 50))
z <- c("STUSAB", "Region")
colnames(usa_region) <- z
usa_region$STUSAB <- c("IL","IN","KY","MO","OH","TN","WV",
                       "IA","MI","MN","WI",
                       "CT","DE","ME","MD","MA","NH","NJ","NY","PA","RI","VT",
                       "ID","OR","WA",
                       "AR","KS","LA","MS","OK","TX",
                       "AL","FL","GA","NC","SC","VA",
                       "AZ","CO","NM","UT",
                       "CA","NV",
                       "MT","NE","ND","SD","WY","AK","HI")
usa_region$Region <- c("Central","Central","Central","Central","Central","Central","Central",
                       "ENC","ENC","ENC","ENC",
                       "NE","NE","NE","NE","NE","NE","NE","NE","NE","NE","NE",
                       "NW","NW","NW",
                       "South","South","South","South","South","South",
                       "SE","SE","SE","SE","SE","SE",
                       "SW","SW","SW","SW",
                       "West","West",
                       "WNC","WNC","WNC","WNC","WNC","Alaska","Hawaii")

#######################################################################################
# Test relationships between annual mean water temp and annual normalized discharge
# Test relationships between annual normalized discharge and annual total precipitation

annual_precip <- met_dat_precip %>%
  mutate(Year = year(Date)) %>%
  group_by(site_no, Year) %>%
  summarise(TotalPrecip_mm = sum(prcp..mm.day.))

annual_mean_wtemp <- wmet %>%
  group_by(site_no, Year) %>%
  summarise(annual_mean_wtemp = mean(corWtemp, na.rm = TRUE))

test <- Q_daily_dat
test <- test[!test$site_no == "02423397",]
longterm_meanQ <- test %>%
  group_by(site_no) %>%
  summarise(LongTermMeanQ = mean(flow_cms, na.rm = TRUE))
test <- test %>%
  mutate(Year = year(Date)) %>%
  group_by(site_no, Year) %>%
  summarise(AnnualMeanQ = mean(flow_cms, na.rm = TRUE))
test <- merge(test, longterm_meanQ, by = 'site_no')
test$NormalizedAnnualMeanQ <- round(((test$AnnualMeanQ-test$LongTermMeanQ)/test$LongTermMeanQ)*100,digits = 1)
test <- merge(test, annual_precip, by = c('site_no', 'Year'))
test <- merge(test, annual_mean_wtemp, by = c('site_no', 'Year'))
test <- merge(test, station_details, by = 'site_no')

usa <- geojson_read(
  "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json", 
  what = "sp"
)
for (i in 1:nrow(test)) {
  coords <- c(test$dec_long_va[i], test$dec_lat_va[i])
  if(any(is.na(coords))) next
  point <- sp::SpatialPoints(
    matrix(
      coords,
      nrow = 1
    )
  )
  sp::proj4string(point) <- sp::proj4string(usa)
  polygon_check <- sp::over(point, usa)
  test$state[i] <- as.character(polygon_check$NAME)
}
test$STUSAB <- state.abb[match(test$state,state.name)]
test <- left_join(test, usa_region, by = 'STUSAB')

test %>%
  nest(data = -Region) %>%
  mutate(model = map(data, ~lm(annual_mean_wtemp~NormalizedAnnualMeanQ, data = .)), tidied = map(model, tidy)) %>%
  unnest(tidied)

library(ggpubr)

ggplot(data = test, aes(y = NormalizedAnnualMeanQ, x = TotalPrecip_mm)) +
  geom_point() +
  ylab("Normalized Annual Mean Discharge (%)") +
  xlab("Annual Total Precip (mm)") +
  stat_smooth(method = 'lm', se = FALSE, color = "red") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 2000, label.y = 225, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 2000, label.y = 300, size = 4) +
  theme_bw() +
  facet_wrap(~Region)

ggplot(data = test, aes(x = NormalizedAnnualMeanQ, y = annual_mean_wtemp)) +
  geom_point() +
  xlab("Normalized Annual Mean Discharge (%)") +
  ylab("Annual Mean Water Temp (C)") +
  stat_smooth(method = 'lm', se = FALSE, color = "red") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 75, label.y = 22, size = 4) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 75, label.y = 17, size = 4) +
  theme_bw() +
  facet_wrap(~Region)

#######################################################################################

hw_site <- left_join(station_details, usa_region, by = 'STUSAB')
temp_precipQ_trends <- merge(temp_precipQ_trends,hw_site, by = "site_no")

cols <- c("NE" = "#d73027", "ENC" = "#f46d43", "SE" = "#ffffbf",
          "WNC" = "#e0f3f8", "South" = "#abd9e9","SW" = "#74add1",
          "NW" = "#4575b4","West" = "#313695","Alaska" = "#a50026")

summary(lm(sen.slope.wtemp~sen.slope.atemp, data = temp_precipQ_trends)) # p-value = 0.024, slope +/- SE = 0.43 +/- 0.19, r2 = 0.07
temp_precipQ_trends %>%
  group_by(Region) %>%
  summarise(MeanATemp = round(mean(sen.slope.atemp),2),
            MeanWTemp = round(mean(sen.slope.wtemp),2),
            MeanPrecip = round(mean(sen.slope.precip),2),
            MeanQ = round(mean(sen.slope.q, na.rm = TRUE),2),
            MinWtemp = round(min(sen.slope.wtemp),2),
            MaxWtemp = round(max(sen.slope.wtemp),2),
            MinQ = round(min(sen.slope.q, na.rm = TRUE),2),
            MaxQ = round(max(sen.slope.q, na.rm = TRUE),2))

ggplot(data = temp_precipQ_trends, aes(x = sen.slope.atemp, y = sen.slope.wtemp)) +
  geom_abline(slope = 1, linetype = 'longdash') +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color ="black") +
  stat_smooth(method = 'lm', se = F, color = "red") +
  geom_point(shape = 21, size = 4, aes(fill = factor(Region))) +
  scale_fill_manual(values = cols) +
  ylab(expression(atop(Annual~Water~Temp.~Trend,
                       (degree*C~yr^-1)))) +
  xlab(expression(Annual~Atmo~Temp.~Trend~(degree*C~yr^-1))) +
  labs(fill = "Region") +
  scale_y_continuous(breaks = seq(-0.06,0.06,0.02), limits = c(-0.06,0.07)) +
  scale_x_continuous(breaks = seq(-0.02,0.06,0.02), limits = c(-0.02,0.06)) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 0.001, label.y = 0.06, size = 6) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 0.001, label.y = 0.07, size = 6) +
  # annotate("text", x = 0.001, y = 0.07, label = "y = 0.44x + 0.007", size = 6, hjust = 0) +
  # annotate("text", x = 0.001, y = 0.06, label = "p-value = 0.022", size = 6, hjust = 0) +
  # annotate("text", x = 0.001, y = 0.05, label = expression(paste(R^2," = 0.07")), size = 6, hjust = 0) +
  annotate("text", x = 0.06, y = 0.07, label = "(a", size = 8) +
  guides(fill = guide_legend(ncol = 2)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 18, color = "black"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = 'black'),
        legend.title.align = 0.5,
        legend.position = c(0.17,0.17))

sum(temp_precipQ_trends$sen.slope.atemp > temp_precipQ_trends$sen.slope.wtemp) # 43 out of 70 (61%)
sum(temp_precipQ_trends$sen.slope.atemp < temp_precipQ_trends$sen.slope.wtemp) # 27 out of 70 (39%)
sum(temp_precipQ_trends$p.value.wtemp < 0.05) # water temp: 46 out of 70 (66%)
sum(temp_precipQ_trends$sen.slope.wtemp > 0) # water temp: 59 out of 70 (84%)
sum(temp_precipQ_trends$p.value.wtemp < 0.05 & temp_precipQ_trends$sen.slope.wtemp > 0) # water temp: 43 out of 70 (61%)
sum(temp_precipQ_trends$p.value.atemp < 0.05) # air temp: 44 out of 70 (63%)
sum(temp_precipQ_trends$sen.slope.atemp > 0) # air temp: 67 out of 70 (96%)
sum(temp_precipQ_trends$p.value.atemp < 0.05 & temp_precipQ_trends$sen.slope.atemp > 0) # air temp: 44 out of 70 (63%)
sum(temp_precipQ_trends$p.value.wtemp < 0.05 & temp_precipQ_trends$p.value.atemp < 0.05) # 31 out of 70 (44%)
temp_precipQ_trends[temp_precipQ_trends$p.value.wtemp < 0.05 & temp_precipQ_trends$p.value.atemp < 0.05,]

anova_precip <- aov(sen.slope.precip~Region, data = temp_precipQ_trends)
summary(anova_precip)
precip_anova_region <- TukeyHSD(anova_precip)
precip_anova_region <- as.data.frame(precip_anova_region$Region)
precip_anova_region[precip_anova_region$`p adj` < 0.05,]
ggplot(temp_precipQ_trends, aes(x=Region, y = sen.slope.precip)) +
  geom_boxplot() + theme_bw() +
  ylab(expression(Annual~Precip.~Trend~(mm~yr^-1)))

temp_precipQ_trends %>%
  group_by(Region) %>%
  summarise(MeanPrecip = round(mean(sen.slope.precip),digits = 1),
            SD_Precip = round(sd(sen.slope.precip),digits = 1),
            MeanQ = mean(sen.slope.q, na.rm = TRUE),
            SD_Q = sd(sen.slope.q, na.rm = TRUE))

summary(lm(sen.slope.q~sen.slope.precip, data = temp_precipQ_trends)) # p-value < 0.001, slope +/- SE = 0.73 +/- 0.20, r2 = 0.21

ggplot(data = temp_precipQ_trends, aes(x = sen.slope.precip, y = sen.slope.q)) +
  geom_abline(slope = 1, linetype = 'longdash') +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color ="black") +
  stat_smooth(method = 'lm', se = F, color = 'red') +
  geom_point(shape = 21, size = 4, aes(fill = factor(Region))) +
  scale_fill_manual(values = cols) +
  ylab(expression(atop(Annual~Mean~Discharge~Trend, (m^3~s^-1~yr^-1)))) +
  xlab(expression(Annual~Total~Precip.~Trend~(mm~yr^-1))) +
  labs(fill = "Region") +
  scale_y_continuous(breaks = seq(-5,2,1), limits = c(-5,2)) +
  scale_x_continuous(breaks = seq(-1,2,0.5), limits = c(-1,2)) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 1.25, label.y = -4, size = 6) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.x = 1.25, label.y = -3.5, size = 6) +
  # annotate("text", x = 1.25, y = -3.5, label = "y = 0.73x - 0.20", size = 6, hjust = 0) +
  # annotate("text", x = 1.25, y = -4, label = "p-value < 0.001", size = 6, hjust = 0) +
  # annotate("text", x = 1.25, y = -4.5, label = expression(paste(R^2," = 0.21")), size = 6, hjust = 0) +
  annotate("text", x = -1, y = 2, label = "(b", size = 8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 18, color = "black"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = 'black'),
        legend.position = "none")

sum(temp_precipQ_trends$p.value.q < 0.05, na.rm = TRUE) # 24 stat sig. discharge trends out of 51 (47%)
test <- temp_precipQ_trends[temp_precipQ_trends$p.value.q < 0.05,]
round(min(test$sen.slope.q, na.rm = TRUE), digits = 2) # min stat. sig. Q trend = -4.28 cms
round(max(test$sen.slope.q, na.rm = TRUE), digits = 2) # max stat. sig. Q trend = 1.04  cms
sum(test$sen.slope.q < 0, na.rm = TRUE) # 17 out of 24 sites had stat. sig. negative trends
table(test$Region) # 24 sites total: 2 (out of 4) in ENC, 2 (out of 11) in NE, 2 (out of 13) in SE, 10 (out of 16) in NW, 6 (out of 6) in SW, and 2 (out of 2) in West

summary(lm(sen.slope.wtemp~sen.slope.q, data = temp_precipQ_trends)) # p-value = 0.688

temp_precipQ_trends <- merge(temp_trends, precipQ_trends, by = "site_no", all = TRUE)
usa_region <- data.frame(matrix(ncol = 2, nrow = 50))
z <- c("STUSAB", "Region")
colnames(usa_region) <- z
usa_region$STUSAB <- c("IL","IN","KY","MO","OH","TN","WV",
                       "IA","MI","MN","WI",
                       "CT","DE","ME","MD","MA","NH","NJ","NY","PA","RI","VT",
                       "ID","OR","WA",
                       "AR","KS","LA","MS","OK","TX",
                       "AL","FL","GA","NC","SC","VA",
                       "AZ","CO","NM","UT",
                       "CA","NV",
                       "MT","NE","ND","SD","WY","AK","HI")
usa_region$Region <- c("Central","Central","Central","Central","Central","Central","Central",
                       "ENC","ENC","ENC","ENC",
                       "NE","NE","NE","NE","NE","NE","NE","NE","NE","NE","NE",
                       "NW","NW","NW",
                       "South","South","South","South","South","South",
                       "SE","SE","SE","SE","SE","SE",
                       "SW","SW","SW","SW",
                       "West","West",
                       "WNC","WNC","WNC","WNC","WNC","Alaska","Hawaii")

hw_site <- left_join(station_details, usa_region, by = 'STUSAB')
temp_precipQ_trends <- merge(temp_precipQ_trends,hw_site, by = "site_no")

cols <- c("NE" = "#d73027", "ENC" = "#f46d43", "SE" = "#ffffbf",
          "WNC" = "#e0f3f8", "South" = "#abd9e9","SW" = "#74add1",
          "NW" = "#4575b4","West" = "#313695","Alaska" = "#a50026")

cols <- data.frame(Region = names(cols), color = cols)
temp_precipQ_trends <- merge(temp_precipQ_trends, cols, by = "Region", all.x = TRUE)
temp_precipQ_trends$site_no_col <- paste0("<span style=\"color: ", temp_precipQ_trends$color, "\">", temp_precipQ_trends$site_no, "</span>")
temp_precipQ_trends$Region <- factor(temp_precipQ_trends$Region,
                                     levels = c("SE","South","SW","West","NE","ENC","WNC","NW","Alaska"))

temp_precipQ_trends <- temp_precipQ_trends[order(temp_precipQ_trends$Region),]
temp_precipQ_trends$site_no <- factor(temp_precipQ_trends$site_no, levels = unique(temp_precipQ_trends$site_no))

SIfig4_atemp <- ggplot(data = temp_precipQ_trends, aes(x = site_no, y = sen.slope.atemp)) +
  geom_rect(xmin = 69.5, xmax = Inf, fill = "#a50026", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 48.5, xmax = 69.5, fill = "#4575b4", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 45.5, xmax = 48.5, fill = "#e0f3f8", ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_rect(xmin = 41.5, xmax = 45.5, fill = "#f46d43", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 30.5, xmax = 41.5, fill = "#d73027", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 28.5, xmax = 30.5, fill = "#313695", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 18.5, xmax = 28.5, fill = "#74add1", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 13.5, xmax = 18.5, fill = "#abd9e9", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = -Inf, xmax = 13.5, fill = "#ffffbf", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_hline(yintercept = 0, linetype = 'longdash') +
  geom_point(aes(fill = factor(status.atemp)), shape = 21, size = 3) +
  scale_fill_manual(name = "",
                    labels = c("p-value > 0.05","p-value < 0.05"),
                    values = c("white","black")) +
  xlab("") +
  ylab(expression(atop(Air~Temp.~Trend,(degree*C~yr^-1)))) +
  scale_y_continuous(breaks = seq(-0.01,0.06,0.01), limits = c(-0.01,0.06)) +
  annotate(geom="label",x = 68, y = 0.06,label = "(a", fill = NA, label.size = NA, size = 6) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "black",linetype="longdash",size=0.1),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.position = 'none',
        plot.margin=unit(c(1,1,1,-0.5), "cm")) +
  coord_flip()

SIfig4_wtemp <- ggplot(data = temp_precipQ_trends, aes(x = site_no, y = sen.slope.wtemp)) +
  geom_rect(xmin = 69.5, xmax = Inf, fill = "#a50026", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 48.5, xmax = 69.5, fill = "#4575b4", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 45.5, xmax = 48.5, fill = "#e0f3f8", ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_rect(xmin = 41.5, xmax = 45.5, fill = "#f46d43", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 30.5, xmax = 41.5, fill = "#d73027", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 28.5, xmax = 30.5, fill = "#313695", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 18.5, xmax = 28.5, fill = "#74add1", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 13.5, xmax = 18.5, fill = "#abd9e9", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = -Inf, xmax = 13.5, fill = "#ffffbf", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_hline(yintercept = 0, linetype = 'longdash') +
  geom_point(aes(fill = factor(status.wtemp)), shape = 21, size = 3) +
  scale_fill_manual(name = "",
                    labels = c("p-value > 0.05","p-value < 0.05"),
                    values = c("white","black")) +
  xlab("") +
  ylab(expression(atop(Water~Temp.~Trend,(degree*C~yr^-1)))) +
  annotate(geom="label",x = 68, y = 0.07,label = "(b", fill = NA, label.size = NA, size = 6) +
  scale_y_continuous(breaks = seq(-0.06,0.06,0.02), limits = c(-0.06,0.07)) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "black",linetype="longdash",size=0.1),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(1,1,1,-1), "cm")) +
  coord_flip()

SIfig4_precip <- ggplot(data = temp_precipQ_trends, aes(x = site_no, y = sen.slope.precip)) +
  geom_rect(xmin = 69.5, xmax = Inf, fill = "#a50026", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 48.5, xmax = 69.5, fill = "#4575b4", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 45.5, xmax = 48.5, fill = "#e0f3f8", ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_rect(xmin = 41.5, xmax = 45.5, fill = "#f46d43", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 30.5, xmax = 41.5, fill = "#d73027", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 28.5, xmax = 30.5, fill = "#313695", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 18.5, xmax = 28.5, fill = "#74add1", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 13.5, xmax = 18.5, fill = "#abd9e9", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = -Inf, xmax = 13.5, fill = "#ffffbf", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_hline(yintercept = 0, linetype = 'longdash') +
  geom_point(aes(fill = factor(status.precip)), shape = 21, size = 3) +
  scale_fill_manual(name = "",
                    labels = c("p-value > 0.05","p-value < 0.05"),
                    values = c("white","black")) +
  xlab("") +
  ylab(expression(atop(Precipatation~Trend,(mm~yr^-1)))) +
  annotate(geom="label",x = 68, y = 1.5,label = "(c", fill = NA, label.size = NA, size = 6) +
  scale_y_continuous(breaks = seq(-0.5,1.5,0.5), limits = c(-0.75,1.5)) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "black",linetype="longdash",size=0.1),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(1,1,1,-1), "cm")) +
  coord_flip()

SIfig4_q <- ggplot(data = temp_precipQ_trends, aes(x = site_no, y = sen.slope.q)) +
  geom_rect(xmin = 69.5, xmax = Inf, fill = "#a50026", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 48.5, xmax = 69.5, fill = "#4575b4", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 45.5, xmax = 48.5, fill = "#e0f3f8", ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_rect(xmin = 41.5, xmax = 45.5, fill = "#f46d43", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 30.5, xmax = 41.5, fill = "#d73027", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 28.5, xmax = 30.5, fill = "#313695", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 18.5, xmax = 28.5, fill = "#74add1", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = 13.5, xmax = 18.5, fill = "#abd9e9", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_rect(xmin = -Inf, xmax = 13.5, fill = "#ffffbf", ymin = -Inf, ymax = Inf, alpha = 0.03) +
  geom_hline(yintercept = 0, linetype = 'longdash') +
  geom_point(aes(fill = factor(status.q)), shape = 21, size = 3, na.rm = T) +
  scale_fill_manual(name = "",
                    labels = c("p-value > 0.05","p-value < 0.05"),
                    values = c("white","black"),
                    na.translate=FALSE) +
  xlab("") +
  ylab(expression(atop(Discharge~Trend,(m^3~s^-1~~yr^-1)))) +
  scale_y_continuous(breaks = seq(-4.0,1.0,0.5), limits = c(-4.5,1.25)) +
  annotate(geom="label",x = 68, y = 1.25,label = "(d", fill = NA, label.size = NA, size = 6) +
  annotate(geom="label",x = 70.2, y = -4.5,label = "Alaska", fill = NA, label.size = NA, size = 5, hjust = 0, color = 'gray85') +
  annotate(geom="label",x = 49.75, y = -4.5,label = "Northwest", fill = NA, label.size = NA, size = 5, hjust = 0) +
  annotate(geom="label",x = 46.5, y = -4.5,label = "WNC", fill = NA, label.size = NA, size = 5, hjust = 0) +
  annotate(geom="label",x = 42.5, y = -4.5,label = "ENC", fill = NA, label.size = NA, size = 5, hjust = 0) +
  annotate(geom="label",x = 31.5, y = -4.5,label = "Northeast", fill = NA, label.size = NA, size = 5, hjust = 0) +
  annotate(geom="label",x = 29.5, y = -4.5,label = "West", fill = NA, label.size = NA, size = 5, hjust = 0, color = 'gray85') +
  annotate(geom="label",x = 19.5, y = -4.5,label = "Southwest", fill = NA, label.size = NA, size = 5, hjust = 0) +
  annotate(geom="label",x = 14.5, y = -4.5,label = "South", fill = NA, label.size = NA, size = 5, hjust = 0) +
  annotate(geom="label",x = 1.5, y = -4.5,label = "Southeast", fill = NA, label.size = NA, size = 5, hjust = 0) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "black",linetype="longdash",size=0.1),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12, color = "black", angle = 40, hjust = 1, vjust = 1),
        axis.text.y = element_blank(),
        legend.position = c(0.28,0.93),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.margin=unit(c(1,1,1,-1), "cm")) +
  coord_flip()

# width = 1500 height = 1100
ggarrange(SIfig4_atemp,SIfig4_wtemp,SIfig4_precip,SIfig4_q, nrow = 1, align = 'h', widths = c(1.4,1,1,1))

station_details <- station_details[station_details$site_no %in% keep_sites_temp$site_no,]
station_details <- station_details[order(station_details$site_no),]
station_details <- station_details[,c(3,2,5,6,4)]
colnames(station_details)[3] <- 'lat'
colnames(station_details)[4] <- 'long'

### Stream Order and Reservoir position came from ArcMap using the USGS NHDPlus High Resolution geospatial database and USACE National Inventory of Dams (NID) respectively
station_details$StreamOrder <- c(5,6,6,7,7,7,7,5,4,7,6,6,7,6,7,8,8,4,6,4,4,3,6,6,5,6,1,3,7,7,8,7,8,8,7,
                                 7,7,9,5,7,8,8,7,8,7,6,1,4,6,5,4,6,7,7,9,5,4,3,5,8,7,7,7,8,8,7,7,8,3,4)
station_details$Reservoir <- c('Below','Below','Below','None','None','None','None','Below','Above','None','None','None','Below','Above','None','None','None','None','None','None',
                               'None','None','Below','Below','None','Below','None','None','Below','None','None','None','None','None','None','None','Below','None','Below','None',
                               'None','None','Below','None','None','None','None','Above','Above','None','Above','None','None','Below','Below','Above','Above','Above','Above',
                               'None','Above','Below','None','None','None','None','None','None','None','Above')
setwd("D:/School/USGSdata/GitHub")
write.csv(station_details, 'Station_Details.csv', row.names = FALSE)
write.csv(wmet,'Wtemp_daily_dat.csv', row.names = FALSE)
write.csv(Q_daily_dat,'Q_daily_dat.csv', row.names = FALSE)
