library(ggplot2)
library(heatwaveR)
library(Kendall)
library(trend)
library(geojsonio)
library(sp)

rm(list = ls())
dev.off()

setwd("F:/School/USGSdata/GitHub")

Wtemp_daily_dat <- read.csv('Wtemp_daily_dat.csv')
Wtemp_daily_dat$Date <- as.Date(Wtemp_daily_dat$Date)
Wtemp_daily_dat$site_no <- as.character(Wtemp_daily_dat$site_no)
Wtemp_daily_dat <- Wtemp_daily_dat %>%
  mutate(site_no = ifelse(row_number()<=638386, paste0("0", site_no), site_no))

station_details <- read.csv('Station_Details.csv')
station_details$site_no <- as.character(station_details$site_no)
station_details <- station_details %>%
  mutate(site_no = ifelse(row_number()<=86, paste0("0", site_no), site_no))
station_details[132,2] <- "420451121510000"
station_details$DrainageArea_km2 <- station_details$DrainageArea_mi2 * 2.59

residualQ <- read.csv('ResidualQ.csv')
residualQ$Date <- as.Date(residualQ$Date)
residualQ$site_no <- as.character(residualQ$site_no)
residualQ <- residualQ %>%
  mutate(site_no = ifelse(row_number()<=582784, paste0("0", site_no), site_no))

### Run HW & CS analysis

zz <- unique(Wtemp_daily_dat$site_no)
for(i in 1:length(zz)){
  curDat = Wtemp_daily_dat[Wtemp_daily_dat$site_no == zz[i],]
  ts_Warm = ts2clm(curDat, x = Date, y = Wtemp,
                   climatologyPeriod = c(min(curDat$Date), max(curDat$Date)))
  ts_Cold = ts2clm(curDat, x = Date, y = Wtemp,
                   climatologyPeriod = c(min(curDat$Date), max(curDat$Date)), pctile = 10)
  de_Warm = detect_event(ts_Warm, x = Date, y = Wtemp )
  de_Cold = detect_event(ts_Cold, x = Date, y = Wtemp, coldSpells = TRUE )
  cat_Warm = category(de_Warm, y = Wtemp, S = FALSE)
  cat_Cold = category(de_Cold, y = Wtemp, S = FALSE)
  curEventsWarm = de_Warm$event
  curEventsCold = de_Cold$event
  curEventsWarm$Station = zz[i]
  curEventsCold$Station = zz[i]
  curCatWarm = cat_Warm
  curCatCold = cat_Cold
  curCatWarm$Station = zz[i]
  curCatCold$Station = zz[i]
  if( i == 1){
    saveDatWarm = curEventsWarm
    saveDatCold = curEventsCold
    saveCatWarm = curCatWarm
    saveCatCold = curCatCold
  } else{
    saveDatWarm = rbind(saveDatWarm, curEventsWarm)
    saveDatCold = rbind(saveDatCold, curEventsCold)
    saveCatWarm = rbind(saveCatWarm, curCatWarm)
    saveCatCold = rbind(saveCatCold, curCatCold)
  }
}

### Count number of HW and CS events per station

HW_event_station <- as.data.frame(table(saveDatWarm$Station))
colnames(HW_event_station)[1]  <- "site_no"
colnames(HW_event_station)[2] <- "TotalEvents_HW"

CS_event_station <- as.data.frame(table(saveDatCold$Station))
colnames(CS_event_station)[1]  <- "site_no"
colnames(CS_event_station)[2] <- "TotalEvents_CS"

### Do sites with high # of HW have low # of CS? Positive slope would suggest not

test <- left_join(HW_event_station,CS_event_station, by = "site_no")
test %>%
  ggplot(aes(x=TotalEvents_HW,y=TotalEvents_CS)) +
  geom_point() +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0, linetype = "longdash") +
  labs(x = 'Total Number of Heatwave Events',
       y = 'Total Number of Coldspell Events') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 14, color = "black"), 
        axis.title.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"))
test_mod <- lm(test$TotalEvents_CS~test$TotalEvents_HW)
summary(test_mod)

###

colnames(saveDatWarm)[23] <- "site_no"
hw <- unique(setDT(station_details)[, .(site_no, STUSAB)])[setDT(saveDatWarm), on = "site_no"]
hw <- unique(setDT(station_details)[, .(site_no, StreamOrder)])[setDT(hw), on = "site_no"]
hw <- unique(setDT(station_details)[, .(site_no, DrainageArea_km2)])[setDT(hw), on = "site_no"]
hw <- unique(setDT(station_details)[, .(site_no, Reservoir)])[setDT(hw), on = "site_no"]
hw$Year <- year(hw$date_peak)
hw$Month <- month(hw$date_peak)
hw$Season <- ifelse(hw$Month >=12, "Winter",
                    ifelse(hw$Month >= 9, "Fall",
                           ifelse(hw$Month >= 6, "Summer",
                                  ifelse(hw$Month >= 3, "Spring",
                                         ifelse(hw$Month >=1, "Winter", NA)))))

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

station_details <- left_join(station_details, usa_region, by = 'STUSAB')
hw <- left_join(hw, usa_region, by = 'STUSAB')

### Mean residual Q (cms) during heatwave event

ranges <- mapply(function(x, y, z)  seq.Date(y, z, 1), hw$site_no,  hw$date_start, hw$date_end, USE.NAMES = TRUE)
hw$MeanResidaulQ <- mapply(function(a, b)
  mean(residualQ$mhw_residualQ[residualQ$site_no == b][match(a, residualQ$Date[residualQ$site_no == b])], na.rm = T), ranges, names(ranges))

###

hw_region <- hw %>%
  group_by(Year, Region) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            SumEvents = length(duration))

hw_season <- hw %>%
  group_by(Year, Season) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            Frequency = length(duration)/131)

hw_time <- hw %>%
  group_by(Year) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            Frequency = length(duration)/131)

hw_order <- hw %>%
  group_by(Year, StreamOrder) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            SumEvents = length(duration))

hw %>%
  group_by(Region) %>%
  summarise(count = n_distinct(site_no))

hw_region$nStations[hw_region$Region == 'Alaska'] <- 1
hw_region$nStations[hw_region$Region == 'Central'] <- 2
hw_region$nStations[hw_region$Region == 'ENC'] <- 4
hw_region$nStations[hw_region$Region == 'NE'] <- 17
hw_region$nStations[hw_region$Region == 'NW'] <- 38
hw_region$nStations[hw_region$Region == 'SE'] <- 22
hw_region$nStations[hw_region$Region == 'South'] <- 11
hw_region$nStations[hw_region$Region == 'SW'] <- 13
hw_region$nStations[hw_region$Region == 'West'] <- 6
hw_region$nStations[hw_region$Region == 'WNC'] <- 5
hw_region$Frequency = round(hw_region$SumEvents/hw_region$nStations, digits = 2)

station_details %>%
  group_by(StreamOrder) %>%
  summarise(count = n_distinct(site_no))

hw_order$nStations[hw_order$StreamOrder == 1] <- 2
hw_order$nStations[hw_order$StreamOrder == 2] <- 1
hw_order$nStations[hw_order$StreamOrder == 3] <- 6
hw_order$nStations[hw_order$StreamOrder == 4] <- 15
hw_order$nStations[hw_order$StreamOrder == 5] <- 14
hw_order$nStations[hw_order$StreamOrder == 6] <- 25
hw_order$nStations[hw_order$StreamOrder == 7] <- 37
hw_order$nStations[hw_order$StreamOrder == 8] <- 26
hw_order$nStations[hw_order$StreamOrder == 9] <- 6
hw_order$Frequency = round(hw_order$SumEvents/hw_order$nStations, digits = 2)

x1 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x2 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x3 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x4 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x5 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x6 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x7 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x8 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x9 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x10 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
names(x1)[1] <- "Year"
names(x2)[1] <- "Year"
names(x3)[1] <- "Year"
names(x4)[1] <- "Year"
names(x5)[1] <- "Year"
names(x6)[1] <- "Year"
names(x7)[1] <- "Year"
names(x8)[1] <- "Year"
names(x9)[1] <- "Year"
names(x10)[1] <- "Year"
x1$Region <- "Alaska"
x2$Region <- "Central"
x3$Region <- "ENC"
x4$Region <- "NE"
x5$Region <- "NW"
x6$Region <- "SE"
x7$Region <- "South"
x8$Region <- "SW"
x9$Region <- "West"
x10$Region <- "WNC"
region <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
hw_region <- merge(hw_region, region, by = c("Year","Region"), all = TRUE)
hw_region[is.na(hw_region)] <- 0

x1 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x2 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x3 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x4 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x5 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x6 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x7 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x8 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x9 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
names(x1)[1] <- "Year"
names(x2)[1] <- "Year"
names(x3)[1] <- "Year"
names(x4)[1] <- "Year"
names(x5)[1] <- "Year"
names(x6)[1] <- "Year"
names(x7)[1] <- "Year"
names(x8)[1] <- "Year"
names(x9)[1] <- "Year"
x1$StreamOrder <- 1
x2$StreamOrder <- 2
x3$StreamOrder <- 3
x4$StreamOrder <- 4
x5$StreamOrder <- 5
x6$StreamOrder <- 6
x7$StreamOrder <- 7
x8$StreamOrder <- 8
x9$StreamOrder <- 9
streamOrder <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
hw_order <- merge(hw_order, streamOrder, by = c("Year","StreamOrder"), all = TRUE)
hw_order[is.na(hw_order)] <- 0

hw_time$Type <- "HW"
hw_time <- hw_time[,c(1,7,2:6)]
aa <- unique(hw_time$Type)
bb <- unique(hw_region$Region)
cc <- unique(hw_season$Season)
dd <- unique(hw_order$StreamOrder)

#######################################################################################
#                                                                                     #
#                                     Time                                            #
#                                                                                     #
#######################################################################################

for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Type == aa[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = aa[i]
  cur_hw_time = data.frame(TestType = "Time",
                           Variable = "Avg.Duration",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    hw_time_duration = cur_hw_time
  } else{
    hw_time_duration = rbind(hw_time_duration, cur_hw_time)
  }
}
for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Type == aa[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = aa[i]
  cur_hw_time = data.frame(TestType = "Time",
                           Variable = "Avg.CuInt",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    hw_time_intensity = cur_hw_time
  } else{
    hw_time_intensity = rbind(hw_time_intensity, cur_hw_time)
  }
}
for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Type == aa[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = aa[i]
  cur_hw_time = data.frame(TestType = "Time",
                           Variable = "Avg.Onset",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    hw_time_onset = cur_hw_time
  } else{
    hw_time_onset = rbind(hw_time_onset, cur_hw_time)
  }
}
for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Type == aa[i],]
  ts = ts(data = curDat[, 6],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = aa[i]
  cur_hw_time = data.frame(TestType = "Time",
                           Variable = "Avg.Decline",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    hw_time_decline = cur_hw_time
  } else{
    hw_time_decline = rbind(hw_time_decline, cur_hw_time)
  }
}
for(i in 1:length(aa)){
  curDat = hw_time[hw_time$Type == aa[i],]
  ts = ts(data = curDat[, 7],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = aa[i]
  cur_hw_time = data.frame(TestType = "Time",
                           Variable = "Frequency",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    hw_time_freq = cur_hw_time
  } else{
    hw_time_freq = rbind(hw_time_freq, cur_hw_time)
  }
}

#######################################################################################
#                                                                                     #
#                                     Region                                          #
#                                                                                     #
#######################################################################################

for(i in 1:length(bb)){
  curDat = hw_region[hw_region$Region == bb[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = bb[i]
  cur_hw_region = data.frame(TestType = "Region",
                             Variable = "Avg.Duration",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_region_duration = cur_hw_region
  } else{
    hw_region_duration = rbind(hw_region_duration, cur_hw_region)
  }
}
for(i in 1:length(bb)){
  curDat = hw_region[hw_region$Region == bb[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = bb[i]
  cur_hw_region = data.frame(TestType = "Region",
                             Variable = "Avg.CuInt",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_region_intensity = cur_hw_region
  } else{
    hw_region_intensity = rbind(hw_region_intensity, cur_hw_region)
  }
}
for(i in 1:length(bb)){
  curDat = hw_region[hw_region$Region == bb[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = bb[i]
  cur_hw_region = data.frame(TestType = "Region",
                             Variable = "Avg.Onset",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_region_onset = cur_hw_region
  } else{
    hw_region_onset = rbind(hw_region_onset, cur_hw_region)
  }
}
for(i in 1:length(bb)){
  curDat = hw_region[hw_region$Region == bb[i],]
  ts = ts(data = curDat[, 6],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = bb[i]
  cur_hw_region = data.frame(TestType = "Region",
                             Variable = "Avg.Decline",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_region_decline = cur_hw_region
  } else{
    hw_region_decline = rbind(hw_region_decline, cur_hw_region)
  }
}
for(i in 1:length(bb)){
  curDat = hw_region[hw_region$Region == bb[i],]
  ts = ts(data = curDat[, 9],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = bb[i]
  cur_hw_region = data.frame(TestType = "Region",
                             Variable = "Frequency",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_region_freq = cur_hw_region
  } else{
    hw_region_freq = rbind(hw_region_freq, cur_hw_region)
  }
}

#######################################################################################
#                                                                                     #
#                                     Season                                          #
#                                                                                     #
#######################################################################################

for(i in 1:length(cc)){
  curDat = hw_season[hw_season$Season == cc[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = cc[i]
  cur_hw_season = data.frame(TestType = "Season",
                             Variable = "Avg.Duration",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_season_duration = cur_hw_season
  } else{
    hw_season_duration = rbind(hw_season_duration, cur_hw_season)
  }
}
for(i in 1:length(cc)){
  curDat = hw_season[hw_season$Season == cc[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = cc[i]
  cur_hw_season = data.frame(TestType = "Season",
                             Variable = "Avg.CuInt",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_season_intensity = cur_hw_season
  } else{
    hw_season_intensity = rbind(hw_season_intensity, cur_hw_season)
  }
}
for(i in 1:length(cc)){
  curDat = hw_season[hw_season$Season == cc[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = cc[i]
  cur_hw_season = data.frame(TestType = "Season",
                             Variable = "Avg.Onset",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_season_onset = cur_hw_season
  } else{
    hw_season_onset = rbind(hw_season_onset, cur_hw_season)
  }
}
for(i in 1:length(cc)){
  curDat = hw_season[hw_season$Season == cc[i],]
  ts = ts(data = curDat[, 6],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = cc[i]
  cur_hw_season = data.frame(TestType = "Season",
                             Variable = "Avg.Decline",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_season_decline = cur_hw_season
  } else{
    hw_season_decline = rbind(hw_season_decline, cur_hw_season)
  }
}
for(i in 1:length(cc)){
  curDat = hw_season[hw_season$Season == cc[i],]
  ts = ts(data = curDat[, 7],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = cc[i]
  cur_hw_season = data.frame(TestType = "Season",
                             Variable = "Frequency",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    hw_season_freq = cur_hw_season
  } else{
    hw_season_freq = rbind(hw_season_freq, cur_hw_season)
  }
}

#######################################################################################
#                                                                                     #
#                                     Stream Order                                    #
#                                                                                     #
#######################################################################################

for(i in 1:length(dd)){
  curDat = hw_order[hw_order$StreamOrder == dd[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = dd[i]
  cur_hw_order = data.frame(TestType = "StreamOrder",
                            Variable = "Avg.Duration",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    hw_order_duration = cur_hw_order
  } else{
    hw_order_duration = rbind(hw_order_duration, cur_hw_order)
  }
}
for(i in 1:length(dd)){
  curDat = hw_order[hw_order$StreamOrder == dd[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = dd[i]
  cur_hw_order = data.frame(TestType = "StreamOrder",
                            Variable = "Avg.CuInt",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    hw_order_intensity = cur_hw_order
  } else{
    hw_order_intensity = rbind(hw_order_intensity, cur_hw_order)
  }
}
for(i in 1:length(dd)){
  curDat = hw_order[hw_order$StreamOrder == dd[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = dd[i]
  cur_hw_order = data.frame(TestType = "StreamOrder",
                            Variable = "Avg.Onset",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    hw_order_onset = cur_hw_order
  } else{
    hw_order_onset = rbind(hw_order_onset, cur_hw_order)
  }
}
for(i in 1:length(dd)){
  curDat = hw_order[hw_order$StreamOrder == dd[i],]
  ts = ts(data = curDat[, 6],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = dd[i]
  cur_hw_order = data.frame(TestType = "StreamOrder",
                            Variable = "Avg.Decline",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    hw_order_decline = cur_hw_order
  } else{
    hw_order_decline = rbind(hw_order_decline, cur_hw_order)
  }
}
for(i in 1:length(dd)){
  curDat = hw_order[hw_order$StreamOrder == dd[i],]
  ts = ts(data = curDat[, 9],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = dd[i]
  cur_hw_order = data.frame(TestType = "StreamOrder",
                            Variable = "Frequency",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    hw_order_freq = cur_hw_order
  } else{
    hw_order_freq = rbind(hw_order_freq, cur_hw_order)
  }
}

hw_time_output <- rbind(hw_time_duration, hw_time_intensity, hw_time_freq, hw_time_onset, hw_time_decline)
hw_time_output <- hw_time_output %>% arrange(-desc(p.val))
hw_time_output$Rank <- seq(1,5,1)

hw_region_output <- rbind(hw_region_duration, hw_region_intensity, hw_region_freq, hw_region_onset, hw_region_decline)
hw_region_output <- hw_region_output %>% arrange(-desc(p.val))
hw_region_output$Rank <- seq(1,50,1)

hw_season_output <- rbind(hw_season_duration, hw_season_intensity, hw_season_freq, hw_season_onset, hw_season_decline)
hw_season_output <- hw_season_output %>% arrange(-desc(p.val))
hw_season_output$Rank <- seq(1,20,1)

hw_order_output <- rbind(hw_order_duration, hw_order_intensity, hw_order_freq, hw_order_onset, hw_order_decline)
hw_order_output <- hw_order_output %>% arrange(-desc(p.val))
hw_order_output$Rank <- seq(1,45,1)

### 10% False Discovery Rate (see link for details: http://www.biostathandbook.com/multiplecomparisons.html)

fdr_table <- data.frame(matrix(ncol = 3, nrow = 120))
x <- c("TestType", "Rank", "FDR_0.1")
colnames(fdr_table) <- x
fdr_table[1:5,1] <- "Time"
fdr_table[6:55,1] <- "Region"
fdr_table[56:75,1] <- "Season"
fdr_table[76:120,1] <- "StreamOrder"
fdr_table[1:5,2] <- seq(1,5,1)
fdr_table[6:55,2] <- seq(1,50,1)
fdr_table[56:75,2] <- seq(1,20,1)
fdr_table[76:120,2] <- seq(1,45,1)
fdr_table[1:5,3] <- round((fdr_table$Rank[1:5]/5)*.1,4)
fdr_table[6:55,3] <- round((fdr_table$Rank[6:55]/50)*.1,4)
fdr_table[56:75,3] <- round((fdr_table$Rank[56:75]/20)*.1,4)
fdr_table[76:120,3] <- round((fdr_table$Rank[76:120]/45)*.1,4)

hw_MKSS_results <- rbind(hw_time_output, hw_region_output, hw_season_output, hw_order_output)
hw_MKSS_results <- left_join(hw_MKSS_results,fdr_table, by = c("TestType","Rank"))
hw_MKSS_results$SigTest <- ifelse(hw_MKSS_results$p.val < hw_MKSS_results$FDR_0.1,"Sig","NS")
hw_MKSS_results_sig <- hw_MKSS_results[c(1:2,56:59,76),]
View(hw_MKSS_results_sig)

hw %>%
  group_by(StreamOrder) %>%
  summarise(q = mean(MeanResidaulQ, na.rm = TRUE))
