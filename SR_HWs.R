library(data.table)
library(ggplot2)
library(heatwaveR)
library(Kendall)
library(trend)
library(geojsonio)
library(sp)
library(tidyverse)
library(ggpubr)
library(broom)

rm(list = ls())
dev.off()

setwd("D:/School/USGSdata/GitHub")

Wtemp_daily_dat <- read.csv('Wtemp_daily_dat.csv', colClasses = c(site_no = "character",
                                                                  Date = "Date"))

residualQ <- read.csv('ResidualQ_v2.csv', colClasses = c(site_no = "character",
                                                      Date = "Date"))

station_details <- read.csv('Station_Details.csv', colClasses = c(site_no = "character"))

### Are there long-term trends in annual mean water temperature and discharge?

Wtemp_daily_dat$Month <- month(Wtemp_daily_dat$Date)
month_mean_wtemp <- Wtemp_daily_dat %>%
  group_by(site_no, Year, Month) %>%
  summarise(monthly_mean_wtemp = mean(corWtemp, na.rm = TRUE))
wide_month_mean_wtemp <- month_mean_wtemp %>%
  pivot_wider(names_from = site_no, values_from = monthly_mean_wtemp)

residualQ$Year <- year(residualQ$Date)
residualQ$Month <- month(residualQ$Date)
month_mean_Q <- residualQ %>%
  group_by(site_no, Year, Month) %>%
  summarise(MeanQ = mean(flow_cms, na.rm = TRUE))
month_mean_Q <- month_mean_Q[!month_mean_Q$site_no == "02423397",]
wide_month_mean_Q <- month_mean_Q %>%
  pivot_wider(names_from = site_no, values_from = MeanQ)

library(wql) # Seasonal Kendall trend test for water temperature and discharge
library(ggtext)

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

sum(wtemp_seaken$p.value <= 0.05) # 45
sum(wtemp_seaken$sen.slope > 0) # 59
sum(wtemp_seaken$sen.slope > 0 & wtemp_seaken$p.value <= 0.05) # 43 (MK = 19)

Q_ts = ts(data = wide_month_mean_Q[, 3:53],
              start = c(1996,1),
              end = c(2021,12),
              frequency = 12)
Q_seaken <- seaKen(Q_ts)
Q_seaken <- Q_seaken %>%
  data.frame() %>%
  cbind(site_no = row.names(Q_seaken),.) %>%
  mutate(Status = if_else(
    p.value < 0.05, "sig", "not sig"))
Q_seaken <- Q_seaken[,c(1:2,4:6)]
colnames(Q_seaken) <- c("site_no","sen.slope.Q","p.value.Q","miss.Q","status.Q")

sum(Q_seaken$p.value <= 0.05) # 24
sum(Q_seaken$sen.slope > 0) # 24
sum(Q_seaken$sen.slope > 0 & Q_seaken$p.value <= 0.05) # 7

trends_wtemp_Q <- merge(wtemp_seaken,Q_seaken, by = "site_no", all = TRUE)

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

cols1 <- c("NE" = "#d73027", "ENC" = "#f46d43", "SE" = "#cfcf74",
           "WNC" = "#bababa", "South" = "#abd9e9","SW" = "#74add1",
           "NW" = "#4575b4","West" = "#313695","Alaska" = "#a50026")
cols1 <- data.frame(Region = names(cols1), color = cols1)

trends_wtemp_Q <- merge(trends_wtemp_Q,station_details, by ="site_no")
trends_wtemp_Q <- merge(trends_wtemp_Q, usa_region, by = "STUSAB")
trends_wtemp_Q <- merge(trends_wtemp_Q, cols1, by = "Region", all.x = TRUE)
trends_wtemp_Q$site_no <- paste0("<span style=\"color: ", trends_wtemp_Q$color, "\">", trends_wtemp_Q$site_no, "</span>")
trends_wtemp_Q$Region <- factor(trends_wtemp_Q$Region,
                             levels = c("SE","South","SW","West","NE","ENC","WNC","NW","Alaska"))

trends_wtemp_Q <- trends_wtemp_Q[order(trends_wtemp_Q$Region),]
trends_wtemp_Q$site_no <- factor(trends_wtemp_Q$site_no, levels = unique(trends_wtemp_Q$site_no))

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

NROW(saveDatWarm) # 3,984 events
round(mean(saveDatWarm$duration)) # 9 days
max(saveDatWarm$duration) # 103 days
round(mean(saveDatWarm$intensity_max_relThresh),digits = 1) # 1.7 degrees C
round(max(saveDatWarm$intensity_max_relThresh),digits = 1) # 9.0 degrees C
round(mean(saveDatWarm$intensity_max),digits = 1) # 3.8 degrees C
round(max(saveDatWarm$intensity_max),digits = 1) # 12.7 degrees C

### Count number of heatwave and coldspell events per station

HW_event_station <- as.data.frame(table(saveDatWarm$Station))
colnames(HW_event_station)[1]  <- "site_no"
colnames(HW_event_station)[2] <- "TotalEvents_HW"

CS_event_station <- as.data.frame(table(saveDatCold$Station))
colnames(CS_event_station)[1]  <- "site_no"
colnames(CS_event_station)[2] <- "TotalEvents_CS"

hw_cs_site <- merge(HW_event_station, CS_event_station, by = "site_no")
hw_cs_site <- merge(station_details, hw_cs_site, by = "site_no")

hw_cs_mod <- lm(hw_cs_site$TotalEvents_CS~hw_cs_site$TotalEvents_HW)
summary(hw_cs_mod)

cols2 <- c("Below" = "#edf8b1", "Above" = "#7fcdbb", "None" = "#2c7fb8")
cols3 <- c("1" = "#ffffd9", "3" = "#edf8b1", "4" = "#c7e9b4", "5" = "#7fcdbb",
          "6" = "#41b6c4", "7" = "#1d91c0", "8" = "#225ea8", "9" = "#0c2c84")

hw_cs_site %>%
  group_by(Reservoir) %>%
  summarise(Median = round(median(TotalEvents_HW)),
            SD = round(round(sd(TotalEvents_HW))),
            n = n())

hw_cs_site <- left_join(hw_cs_site, usa_region, by = 'STUSAB')

hw_anova <- aov(TotalEvents_HW~Reservoir, data = hw_cs_site)
summary(hw_anova) # p-value = 0.193

ggplot(data = hw_cs_site, aes(x = Reservoir, y = TotalEvents_HW)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Reservoir Position',
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10)) +
  coord_cartesian(ylim = c(0, 80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'))

library(dplyr)
library(rstatix)

test <- hw_cs_site
test$Region <- as.factor(test$Region)
hw_region.aov <- test %>% anova_test(TotalEvents_HW ~ Region)
hw_region.tukey <- test %>% tukey_hsd(TotalEvents_HW ~ Region)
hw_region.tukey <- hw_region.tukey %>% add_xy_position(x = "Region")

# width = 700 height = 550
ggboxplot(test, x = "Region", y = "TotalEvents_HW", outlier.shape = NA) +
  stat_pvalue_manual(hw_region.tukey, hide.ns = TRUE, y.position = c(90,85,80),
                     label = "p = {scales::pvalue(p.adj)}", digits = 3) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(subtitle = get_test_label(hw_region.aov, detailed = T)) +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,90)) +
  xlab("Region") +
  ylab("Total HW Events (1996-2021)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.subtitle = element_text(vjust = -105, hjust = 0.05),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

hw_cs_site %>%
  group_by(Region) %>%
  summarise(Mean = round(mean(TotalEvents_HW)),
            Median = round(median(TotalEvents_HW)),
            SD = round(sd(TotalEvents_HW)),
            max = max(TotalEvents_HW),
            min = min(TotalEvents_HW))

###

colnames(saveDatWarm)[23] <- "site_no"
colnames(saveDatCold)[23] <- "site_no"
hw <- unique(setDT(station_details)[, .(site_no, STUSAB)])[setDT(saveDatWarm), on = "site_no"]
hw <- unique(setDT(station_details)[, .(site_no, StreamOrder)])[setDT(hw), on = "site_no"]
hw <- unique(setDT(station_details)[, .(site_no, Reservoir)])[setDT(hw), on = "site_no"]

hw$Year <- year(hw$date_peak)
hw$Month <- month(hw$date_peak)
hw$Season <- ifelse(hw$Month >=12, "Winter",
                    ifelse(hw$Month >= 9, "Fall",
                           ifelse(hw$Month >= 6, "Summer",
                                  ifelse(hw$Month >= 3, "Spring",
                                         ifelse(hw$Month >=1, "Winter", NA)))))

hw_season_tab <- as.data.frame(table(hw$Season))
colnames(hw_season_tab)[1] <- "Season"
colnames(hw_season_tab)[2] <- "Total_Events"
hw_season_tab$Type = "HW"
hw_season_tab$Frac = round(hw_season_tab$Total_Events/sum(hw_season_tab$Total_Events),digits = 2)

cols <- c("HW" = "#000000", "CS" = "#ffffff")

hw_season_tab %>%
  mutate(Season = factor(Season, levels = c("Winter","Spring","Summer","Fall"))) %>%
  ggplot(aes(x = Season, y = Total_Events)) +
  geom_col(position = "dodge", color = "black", fill = "white") +
  labs(y = "Total Number of Events",
       x = "Season") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(breaks = seq(0,1300,100)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.position = 'none')

hw_anova <- aov(intensity_cumulative_relThresh~factor(Month), data = hw)
summary(hw_anova)
TukeyHSD(hw_anova)

SIfig3 <- hw %>%
  mutate(Month = factor(Month, levels = c("1","2","3","4","5","6","7","8","9","10",'11',"12"))) %>%
  ggplot(aes(x = Month, y = intensity_cumulative_relThresh)) +
  geom_boxplot(position = "dodge", color = "black", outlier.shape = NA) +
  labs(y = "Cumulative Intensity (°C days)",
       x = "Month") +
  scale_y_continuous(breaks = seq(0,30,5)) +
  coord_cartesian(ylim = c(0, 30)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.background = element_blank())
SIfig3

### Mean residual Q (cms) during heatwaves

ranges <- mapply(function(x, y, z)  seq.Date(y, z, 1), hw$site_no,  hw$date_start, hw$date_end, USE.NAMES = TRUE)
hw$MeanResidaulQ <- mapply(function(a, b)
  mean(residualQ$hw_residualQ[residualQ$site_no == b][match(a, residualQ$Date[residualQ$site_no == b])], na.rm = T), ranges, names(ranges))

hw <- left_join(hw, usa_region, by = 'STUSAB')

hw_site <- hw %>%
  group_by(Year, site_no) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            SumEvents = length(duration))

hw_region <- hw %>%
  group_by(Year, Region) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            SumEvents = length(duration)) # Frequency determined based on number of sites per region

hw_season <- hw %>%
  group_by(Year, Season) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Frequency = length(duration)/NROW(unique(hw$site_no))) # Frequency determined based on total number of unique sites (70) per season

hw_time <- hw %>%
  group_by(Year) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Frequency = length(duration)/NROW(unique(hw$site_no))) # Frequency determined based on total number of unique sites (70)

hw_order <- hw %>%
  group_by(Year, StreamOrder) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            SumEvents = length(duration)) # Frequency determined based on number of sites per stream order

hw_reservoir <- hw %>%
  group_by(Year, Reservoir) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            SumEvents = length(duration)) # Frequency determined based on number of sites per reservoir class

hw %>%
  group_by(Region) %>%
  summarise(count = n_distinct(site_no))

hw_region$nStations <- NA
hw_region$nStations[hw_region$Region == 'Alaska'] <- 1
hw_region$nStations[hw_region$Region == 'ENC'] <- 4
hw_region$nStations[hw_region$Region == 'NE'] <- 11
hw_region$nStations[hw_region$Region == 'NW'] <- 21
hw_region$nStations[hw_region$Region == 'SE'] <- 13
hw_region$nStations[hw_region$Region == 'South'] <- 5
hw_region$nStations[hw_region$Region == 'SW'] <- 10
hw_region$nStations[hw_region$Region == 'West'] <- 2
hw_region$nStations[hw_region$Region == 'WNC'] <- 3
hw_region$Frequency = round(hw_region$SumEvents/hw_region$nStations, digits = 2)

station_details %>%
  group_by(StreamOrder) %>%
  summarise(count = n_distinct(site_no))

hw_order$nStations <- NA
hw_order$nStations[hw_order$StreamOrder == 1] <- 2
hw_order$nStations[hw_order$StreamOrder == 3] <- 4
hw_order$nStations[hw_order$StreamOrder == 4] <- 8
hw_order$nStations[hw_order$StreamOrder == 5] <- 7
hw_order$nStations[hw_order$StreamOrder == 6] <- 12
hw_order$nStations[hw_order$StreamOrder == 7] <- 23
hw_order$nStations[hw_order$StreamOrder == 8] <- 12
hw_order$nStations[hw_order$StreamOrder == 9] <- 2
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
names(x1)[1] <- "Year"
names(x2)[1] <- "Year"
names(x3)[1] <- "Year"
names(x4)[1] <- "Year"
names(x5)[1] <- "Year"
names(x6)[1] <- "Year"
names(x7)[1] <- "Year"
names(x8)[1] <- "Year"
names(x9)[1] <- "Year"
x1$Region <- "Alaska"
x2$Region <- "ENC"
x3$Region <- "NE"
x4$Region <- "NW"
x5$Region <- "SE"
x6$Region <- "South"
x7$Region <- "SW"
x8$Region <- "West"
x9$Region <- "WNC"
region <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
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
names(x1)[1] <- "Year"
names(x2)[1] <- "Year"
names(x3)[1] <- "Year"
names(x4)[1] <- "Year"
names(x5)[1] <- "Year"
names(x6)[1] <- "Year"
names(x7)[1] <- "Year"
names(x8)[1] <- "Year"
x1$StreamOrder <- 1
x2$StreamOrder <- 3
x3$StreamOrder <- 4
x4$StreamOrder <- 5
x5$StreamOrder <- 6
x6$StreamOrder <- 7
x7$StreamOrder <- 8
x8$StreamOrder <- 9
streamOrder <- rbind(x1,x2,x3,x4,x5,x6,x7,x8)
hw_order <- merge(hw_order, streamOrder, by = c("Year","StreamOrder"), all = TRUE)
hw_order[is.na(hw_order)] <- 0

hw_time$Type <- "HW"
hw_time <- hw_time[,c(1,5,2:4)]

hw %>%
  group_by(Reservoir) %>%
  summarise(count = n_distinct(site_no))

hw_reservoir$nStations <- NA
hw_reservoir$nStations[hw_reservoir$Reservoir == 'Above'] <- 11
hw_reservoir$nStations[hw_reservoir$Reservoir == 'Below'] <- 15
hw_reservoir$nStations[hw_reservoir$Reservoir == 'None'] <- 44
hw_reservoir$Frequency = round(hw_reservoir$SumEvents/hw_reservoir$nStations, digits = 2)

x1 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x2 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x3 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
x4 <- as.data.frame(seq(from = 1996, to = 2021, by = 1))
names(x1)[1] <- "Year"
names(x2)[1] <- "Year"
names(x3)[1] <- "Year"
names(x4)[1] <- "Year"
x1$Season <- "Fall"
x2$Season <- "Winter"
x3$Season <- "Spring"
x4$Season <- "Summer"
season <- rbind(x1,x2,x3,x4)
hw_season <- merge(hw_season, season, by = c("Year","Season"), all = TRUE)
hw_season[is.na(hw_season)] <- 0

zz <- unique(station_details$site_no)
fillout <- as.data.frame(rep(zz, times = 26))
colnames(fillout)[1] <- "site_no"
fillout <- fillout %>% arrange(-desc(fillout$site_no))
fillout$Year <- rep(1996:2021, times = 70)
hw_site <- merge(hw_site,fillout, by = c('site_no',"Year"), all = TRUE)
hw_site[is.na(hw_site)] <- 0
colnames(hw_site)[5] <- 'Frequency'

NROW(hw_time) # should be 26 rows (26 years = 26)
NROW(hw_region) # should be 234 rows (26 years * 9 regions = 234)
NROW(hw_season) # should be 104 rows (26 years * 4 seasons = 104)
NROW(hw_order) # should be 208 rows (26 years * 8 stream orders = 208)
NROW(hw_reservoir) # should be 78 rows (26 years * 3 reservoir classes = 78)
NROW(hw_site) # should be 1820 rows (26 years * 70 sites = 1820)

aa <- unique(hw_time$Type)
bb <- unique(hw_region$Region)
cc <- unique(hw_season$Season)
dd <- unique(hw_order$StreamOrder)
ee <- unique(hw_reservoir$Reservoir)
ff <- unique(hw_site$site_no)

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
  ts = ts(data = curDat[, 7],
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
  ts = ts(data = curDat[, 7],
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

#######################################################################################
#                                                                                     #
#                                     Reservoir                                       #
#                                                                                     #
#######################################################################################

for(i in 1:length(ee)){
  curDat = hw_reservoir[hw_reservoir$Reservoir == ee[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Reservoir = ee[i]
  cur_hw_reservoir = data.frame(TestType = "Reservoir",
                                Variable = "Avg.Duration",
                                Category = Reservoir,
                                slope = round(slope, 3),
                                p.val = round(p.val, 4))
  if( i == 1){
    hw_reservoir_duration = cur_hw_reservoir
  } else{
    hw_reservoir_duration = rbind(hw_reservoir_duration, cur_hw_reservoir)
  }
}
for(i in 1:length(ee)){
  curDat = hw_reservoir[hw_reservoir$Reservoir == ee[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Reservoir = ee[i]
  cur_hw_reservoir = data.frame(TestType = "Reservoir",
                                Variable = "Avg.CuInt",
                                Category = Reservoir,
                                slope = round(slope, 3),
                                p.val = round(p.val, 4))
  if( i == 1){
    hw_reservoir_intensity = cur_hw_reservoir
  } else{
    hw_reservoir_intensity = rbind(hw_reservoir_intensity, cur_hw_reservoir)
  }
}
for(i in 1:length(ee)){
  curDat = hw_reservoir[hw_reservoir$Reservoir == ee[i],]
  ts = ts(data = curDat[, 7],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Reservoir = ee[i]
  cur_hw_reservoir = data.frame(TestType = "Reservoir",
                                Variable = "Frequency",
                                Category = Reservoir,
                                slope = round(slope, 3),
                                p.val = round(p.val, 4))
  if( i == 1){
    hw_reservoir_freq = cur_hw_reservoir
  } else{
    hw_reservoir_freq = rbind(hw_reservoir_freq, cur_hw_reservoir)
  }
}

#######################################################################################
#                                                                                     #
#                                     Site                                            #
#                                                                                     #
#######################################################################################

for(i in 1:length(ff)){
  curDat = hw_site[hw_site$site_no == ff[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  site_no = ff[i]
  cur_hw_site = data.frame(TestType = "site_no",
                                Variable = "Avg.Duration",
                                Category = site_no,
                                slope = round(slope, 3),
                                p.val = round(p.val, 4))
  if( i == 1){
    hw_site_duration = cur_hw_site
  } else{
    hw_site_duration = rbind(hw_site_duration, cur_hw_site)
  }
}
for(i in 1:length(ff)){
  curDat = hw_site[hw_site$site_no == ff[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  site_no = ff[i]
  cur_hw_site = data.frame(TestType = "site_no",
                                Variable = "Avg.CuInt",
                                Category = site_no,
                                slope = round(slope, 3),
                                p.val = round(p.val, 4))
  if( i == 1){
    hw_site_intensity = cur_hw_site
  } else{
    hw_site_intensity = rbind(hw_site_intensity, cur_hw_site)
  }
}
for(i in 1:length(ff)){
  curDat = hw_site[hw_site$site_no == ff[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  site_no = ff[i]
  cur_hw_site = data.frame(TestType = "site_no",
                                Variable = "Frequency",
                                Category = site_no,
                                slope = round(slope, 3),
                                p.val = round(p.val, 4))
  if( i == 1){
    hw_site_freq = cur_hw_site
  } else{
    hw_site_freq = rbind(hw_site_freq, cur_hw_site)
  }
}

hw_time_output <- rbind(hw_time_duration, hw_time_intensity, hw_time_freq)
hw_time_output <- hw_time_output %>% arrange(-desc(p.val))
hw_time_output$Rank <- seq(1,3,1)

hw_region_output <- rbind(hw_region_duration, hw_region_intensity, hw_region_freq)
hw_region_output <- hw_region_output %>% arrange(-desc(p.val))
hw_region_output$Rank <- seq(1,27,1)

hw_season_output <- rbind(hw_season_duration, hw_season_intensity, hw_season_freq)
hw_season_output <- hw_season_output %>% arrange(-desc(p.val))
hw_season_output$Rank <- seq(1,12,1)

hw_order_output <- rbind(hw_order_duration, hw_order_intensity, hw_order_freq)
hw_order_output <- hw_order_output %>% arrange(-desc(p.val))
hw_order_output$Rank <- seq(1,24,1)

hw_reservoir_output <- rbind(hw_reservoir_duration, hw_reservoir_intensity, hw_reservoir_freq)
hw_reservoir_output <- hw_reservoir_output %>% arrange(-desc(p.val))
hw_reservoir_output$Rank <- seq(1,9,1)

hw_site_output <- rbind(hw_site_duration, hw_site_intensity, hw_site_freq)
hw_site_output <- hw_site_output %>% arrange(-desc(p.val))
hw_site_output$Rank <- seq(1,210,1)
colnames(hw_site_output)[3] <- 'site_no'

### 15% False Discovery Rate (see link for details: http://www.biostathandbook.com/multiplecomparisons.html)

fdr_table <- data.frame(matrix(ncol = 3, nrow = 75))
x <- c("TestType", "Rank", "FDR_0.15")
colnames(fdr_table) <- x
fdr_table[1:3,1] <- "Time"
fdr_table[4:30,1] <- "Region"
fdr_table[31:42,1] <- "Season"
fdr_table[43:66,1] <- "StreamOrder"
fdr_table[67:75,1] <- "Reservoir"
fdr_table[1:3,2] <- seq(1,3,1)
fdr_table[4:30,2] <- seq(1,27,1)
fdr_table[31:42,2] <- seq(1,12,1)
fdr_table[43:66,2] <- seq(1,24,1)
fdr_table[67:75,2] <- seq(1,9,1)
fdr_table[1:3,3] <- round((fdr_table$Rank[1:3]/3)*0.15,4)
fdr_table[4:30,3] <- round((fdr_table$Rank[4:30]/27)*0.15,4)
fdr_table[31:42,3] <- round((fdr_table$Rank[31:42]/12)*0.15,4)
fdr_table[43:66,3] <- round((fdr_table$Rank[43:66]/24)*0.15,4)
fdr_table[67:75,3] <- round((fdr_table$Rank[67:75]/9)*0.15,4)

hw_MKSS_results <- rbind(hw_time_output, hw_region_output, hw_season_output, hw_order_output, hw_reservoir_output)
hw_MKSS_results <- left_join(hw_MKSS_results,fdr_table, by = c("TestType","Rank"))
hw_MKSS_results$SigTest <- ifelse(hw_MKSS_results$p.val < hw_MKSS_results$FDR_0.1,"Sig","NS")
hw_MKSS_results_sig <- hw_MKSS_results[c(1,31:32,43:48,67:69),] # three results have p-value < 0.05 but greater than the adjusted critical value. Felt worthy of reporting them along with their critical value.
View(hw_MKSS_results_sig) # Table 1

hw_meanResidQ <- hw %>%
  group_by(StreamOrder) %>%
  summarise(Mean_ResidaulQ_HW = round(mean(MeanResidaulQ, na.rm = TRUE),digits = 1),
            Median_ResidaulQ_HW = round(median(MeanResidaulQ, na.rm = TRUE),digits = 1),
            SD_HW = round(sd(MeanResidaulQ, na.rm = TRUE),digits = 1))

### Heatwave categories

saveCatWarm$year <- year(saveCatWarm$peak_date)
hw_cat_temp <- saveCatWarm %>% count(category, year, sort = TRUE)

fillout_year <- rep(seq(from = 1996, to = 2021, by = 1), 4)
fillout_year <- sort(fillout_year)
fillout_cat <- rep(c("I Moderate", "II Strong", "III Severe", "IV Extreme"), 26)
fillout <- cbind(fillout_year,fillout_cat)
fillout <- as.data.frame(fillout)
names(fillout)[1] <- "year"
names(fillout)[2] <- "category"

hw_temp_fill <- merge(hw_cat_temp, fillout, by = c("year","category"), all = TRUE)
hw_temp_fill$n[is.na(hw_temp_fill$n)] <- 0

colnames(hw_temp_fill)[3] <- "TotalEvents_HW"

hw_sum_cat <- saveCatWarm %>%
  group_by(year, category) %>%
  summarise(TotalDuration_HW = sum(duration),
            TotalIntensity_HW = sum(i_max))

hw_sum_cat <- merge(hw_sum_cat, fillout, by = c("year","category"), all = TRUE)
hw_cat_total <- cbind(hw_sum_cat, hw_temp_fill)
hw_cat_total <- hw_cat_total[,c(1:4,7)]
hw_cat_total[is.na(hw_cat_total)] <- 0

hw_cat_total %>%
  group_by(category) %>%
  summarise(TotalEvents = sum(TotalEvents_HW),
            FracTotEvents = round(((TotalEvents/3984)*100),digits = 2))

test <- saveCatWarm
colnames(test)[12] <- "site_no"
test <- merge(test, station_details, by = "site_no")
test <- merge(test, usa_region, by = "STUSAB")
test_extreme <- test[test$category == 'IV Extreme',] # 13 events
test_severe <- test[test$category == 'III Severe',]
test_severe <- test_severe %>% filter(rank(desc(i_max))<=26) # need 26 events to add to the 13 cat 4 events to get 39 total aka top 1% most intense heatwaves
test <- rbind(test_extreme, test_severe)
test$month <- month(test$peak_date)
table(test$month)

Fig2a <- ggplot(data = hw_cat_total) +
  geom_col(aes(x = as.numeric(year), y = TotalEvents_HW,
               fill = fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0,350,50)) +
  ylab("Total HW Events") +
  annotate("text", x = 2020.75, y = 350, label = "(a", size = 6) +
  guides(fill=guide_legend(title="Category")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = c(0.18,0.75),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

moderate <- hw_cat_total[hw_cat_total$category == "I Moderate",]
strong <- hw_cat_total[hw_cat_total$category == "II Strong",]
severe <- hw_cat_total[hw_cat_total$category == "III Severe",]
extreme <- hw_cat_total[hw_cat_total$category == "IV Extreme",]
moderate$year <- as.numeric(moderate$year)
strong$year <- as.numeric(strong$year)
severe$year <- as.numeric(severe$year)
extreme$year <- as.numeric(extreme$year)

summary(lm(TotalEvents_HW~year, data = moderate)) # p-value = 0.003, slope = 3.4
summary(lm(TotalEvents_HW~year, data = strong))
summary(lm(TotalEvents_HW~year, data = severe))
summary(lm(TotalEvents_HW~year, data = extreme))

summary(lm(TotalDuration_HW~year, data = moderate)) # p-value = 0.002, slope = 30.1
summary(lm(TotalDuration_HW~year, data = strong))
summary(lm(TotalDuration_HW~year, data = severe))
summary(lm(TotalDuration_HW~year, data = extreme))

summary(lm(TotalIntensity_HW~year, data = moderate)) # p-value = 0.01, slope = 10.5
summary(lm(TotalIntensity_HW~year, data = strong))
summary(lm(TotalIntensity_HW~year, data = severe))
summary(lm(TotalIntensity_HW~year, data = extreme))

library(scales)

Fig2b <- ggplot(data = hw_cat_total) +
  geom_col(aes(x = as.numeric(year), y = TotalDuration_HW,
               fill = fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0,4000,500),
                     labels = label_number(big.mark = ",")) +
  ylab("Total HW Duration (days)") +
  annotate("text", x = 2020.75, y = 4000, label = "(b", size = 6) +
  guides(fill=guide_legend(title="Category")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = 'none')

Fig2c <- ggplot(data = hw_cat_total) +
  geom_col(aes(x = as.numeric(year), y = TotalIntensity_HW,
               fill = fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#7f1416","#cb3827","#f26722","#ffda68")) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0,1400,200),
                     labels = label_number(big.mark = ",")) +
  ylab(expression(HW~Max~Intensity~(degree*C))) +
  annotate("text", x = 2020.75, y = 1400, label = "(c", size = 6) +
  guides(fill=guide_legend(title="Category")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = 'none')

# width = 600 height = 900
ggarrange(Fig2a,Fig2b,Fig2c, ncol = 1, align = 'v')

hw_site_output <- rbind(hw_site_duration, hw_site_intensity, hw_site_freq)
colnames(hw_site_output)[3] <- 'site_no'
hw_site_output <- hw_site_output[,2:5]
hw_site_output <- hw_site_output %>%
  pivot_wider(names_from = Variable,
              values_from = c(slope, p.val))

hw_site_output <- hw_site_output %>%
  data.frame() %>%
  mutate(status_p.val_Frequency = if_else(
    p.val_Frequency < 0.05, "sig", "not sig"))
hw_site_output <- hw_site_output %>%
  data.frame() %>%
  mutate(status_p.val_Avg.CuInt = if_else(
    p.val_Avg.CuInt < 0.05, "sig", "not sig"))
hw_site_output <- hw_site_output %>%
  data.frame() %>%
  mutate(status_p.val_Avg.Duration = if_else(
    p.val_Avg.Duration < 0.05, "sig", "not sig"))

hw_site_output <- hw_site_output[,c(1:2,5,8,3,6,9,4,7,10)]

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
hw_site_output <- merge(hw_site_output,hw_site, by = "site_no")

cols1 <- c("NE" = "#d73027", "ENC" = "#f46d43", "SE" = "#cfcf74",
           "WNC" = "#bababa", "South" = "#abd9e9","SW" = "#74add1",
           "NW" = "#4575b4","West" = "#313695","Alaska" = "#a50026")
cols1 <- data.frame(Region = names(cols1), color = cols1)

hw_site_output <- merge(hw_site_output, cols1, by = "Region", all.x = TRUE)
hw_site_output$site_no_col <- paste0("<span style=\"color: ", hw_site_output$color, "\">", hw_site_output$site_no, "</span>")
hw_site_output$Region <- factor(hw_site_output$Region,
                                     levels = c("SE","South","SW","West","NE","ENC","WNC","NW","Alaska"))

hw_site_output <- hw_site_output[order(hw_site_output$Region),]
hw_site_output$site_no <- factor(hw_site_output$site_no, levels = unique(hw_site_output$site_no))
round(sum(hw_site_output$slope_Frequency > 0)/70,2) # 49%
round(sum(hw_site_output$slope_Frequency <= 0)/70,2) # 51%
round(sum(hw_site_output$slope_Frequency < 0)/70,2) # 6%
round(sum(hw_site_output$slope_Avg.Duration > 0)/70,2) # 61%
round(sum(hw_site_output$slope_Avg.Duration <= 0)/70,2) # 39%
round(sum(hw_site_output$slope_Avg.Duration < 0)/70,2) # 13%
round(sum(hw_site_output$slope_Avg.CuInt > 0)/70,2) # 60%
round(sum(hw_site_output$slope_Avg.CuInt <= 0)/70,2) # 40%
round(sum(hw_site_output$slope_Avg.CuInt < 0)/70,2) # 20%

Fig3a <- ggplot(data = hw_site_output, aes(x = site_no, y = slope_Frequency)) +
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
  geom_point(aes(fill = factor(status_p.val_Frequency)), shape = 21, size = 3) +
  scale_fill_manual(name = "",
                    labels = c("p-value > 0.05","p-value < 0.05"),
                    values = c("white","black")) +
  xlab("") +
  ylab(expression(atop(Frequency~Trend,
                       (events~yr^-1)))) +
  scale_y_continuous(breaks = seq(-0.1,0.2,0.05), limits = c(-0.1,0.2)) +
  annotate(geom="label",x = 68, y = -0.095,label = "(a", fill = NA, label.size = NA, size = 6) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "black",linetype="longdash",size=0.1),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.position = 'none',
        plot.margin=unit(c(1,1,1,-0.5), "cm")) +
  coord_flip()

Fig3b <- ggplot(data = hw_site_output, aes(x = site_no, y = slope_Avg.Duration)) +
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
  geom_point(aes(fill = factor(status_p.val_Avg.Duration)), shape = 21, size = 3) +
  scale_fill_manual(name = "",
                    labels = c("p-value > 0.05","p-value < 0.05"),
                    values = c("white","black")) +
  xlab("") +
  ylab(expression(atop(Duration~Trend,
                       (days~yr^-1)))) +
  scale_y_continuous(breaks = seq(-0.3,0.5,0.1), limits = c(-0.3,0.5),
                     labels = scales::number_format(accuracy = 0.1)) +
  annotate(geom="label",x = 68, y = 0.5,label = "(b", fill = NA, label.size = NA, size = 6) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "black",linetype="longdash",size=0.1),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        legend.position = 'none',
        plot.margin=unit(c(1,1,1,-1), "cm")) +
  coord_flip()

Fig3c <- ggplot(data = hw_site_output, aes(x = site_no, y = slope_Avg.CuInt)) +
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
  geom_point(aes(fill = factor(status_p.val_Avg.CuInt)), shape = 21, size = 3) +
  scale_fill_manual(name = "",
                    labels = c("p-value > 0.05","p-value < 0.05"),
                    values = c("white","black")) +
  xlab("") +
  ylab(expression(atop(Intensity~Trend,
                       (degree*C~days~yr^-1)))) +
  scale_y_continuous(breaks = seq(-0.4,0.4,0.1), limits = c(-0.4,0.4)) +
  annotate(geom="label",x = 68, y  = 0.4,label = "(c", fill = NA, label.size = NA, size = 6) +
  annotate(geom="label",x = 70.2, y = -0.4,label = "Alaska", fill = NA, label.size = NA, size = 5, hjust  = 0.1, color = 'gray85') +
  annotate(geom="label",x = 49.75, y = -0.4,label = "Northwest", fill = NA, label.size = NA, size = 5, hjust  = 0.1) +
  annotate(geom="label",x = 46.5, y = -0.4,label = "WNC", fill = NA, label.size = NA, size = 5, hjust  = 0.15) +
  annotate(geom="label",x = 42.5, y = -0.4,label = "ENC", fill = NA, label.size = NA, size = 5, hjust  = 0.15) +
  annotate(geom="label",x = 31.5, y = -0.4,label = "Northeast", fill = NA, label.size = NA, size = 5, hjust  = 0.1) +
  annotate(geom="label",x = 29.5, y = -0.4,label = "West", fill = NA, label.size = NA, size = 5, hjust  = 0.15, color = 'gray85') +
  annotate(geom="label",x = 19.5, y = -0.4,label = "Southwest", fill = NA, label.size = NA, size = 5, hjust  = 0.1) +
  annotate(geom="label",x = 14.5, y = -0.4,label = "South", fill = NA, label.size = NA, size = 5, hjust  = 0.15) +
  annotate(geom="label",x = 1.5, y = -0.4,label = "Southeast", fill = NA, label.size = NA, size = 5, hjust  = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "black",linetype="longdash",size=0.1),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        legend.position = c(0.25,0.95),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.margin=unit(c(1,1,1,-1), "cm")) +
  coord_flip()

# width = 1100 height = 1200
ggarrange(Fig3a,Fig3b,Fig3c, ncol = 3, align = 'h', widths = c(2,1.4,1.4))

hw_sum <- saveCatWarm %>%
  group_by(year, Station) %>%
  summarise(TotalDuration = sum(duration, na.rm = TRUE),
            Avg.MaxIntensity = mean(i_max, na.rm = TRUE),
            Frequency = length(duration))
colnames(hw_sum)[2] <- 'site_no'

zz <- unique(station_details$site_no)
fillout <- as.data.frame(rep(zz, times = 26))
colnames(fillout)[1] <- "site_no"
fillout <- fillout %>% arrange(-desc(fillout$site_no))
fillout$year <- rep(1996:2021, times = 70)
hw_sum <- merge(hw_sum,fillout, by = c('site_no',"year"), all = TRUE)
hw_sum[is.na(hw_sum)] <- 0

hw_sum2 <- hw_sum %>%
  group_by(year) %>%
  summarise(MeanAnnualTotalDuration = mean(TotalDuration, na.rm = TRUE),
            MeanAnnualMaxIntensity = mean(Avg.MaxIntensity, na.rm = TRUE))

ts_duration = ts(data = hw_sum2[, 2],
        frequency = 1,
        start = 1996,
        end = 2021)
ts_intensity = ts(data = hw_sum2[, 3],
                 frequency = 1,
                 start = 1996,
                 end = 2021)
MannKendall(ts_duration) # p-value = 0.015
MannKendall(ts_intensity) # p-value = 0.201

library(zyp) # Need this package to get intercept for Sen's slope

zyp.sen(MeanAnnualTotalDuration~year, hw_sum2)
round((0.5253*1996)-1037.0127) # 11 HW days in 1996
round((0.5253*2021)-1037.0127) # 25 HW days in 2021

ggplot(data = hw_sum2, aes(x = year, y = MeanAnnualTotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "red", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0, 60, 5), limits = c(0,60)) +
  ylab("Avg. Total HW Days") +
  annotate("text", x = 1998, y = 60, label = "y = 0.5253x - 1037.0127", size = 5, hjust = 0) +
  annotate("text", x = 1998, y = 54, label = "p-value = 0.015", size = 5, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

residualQ$year <- year(residualQ$Date)
annualQ <- residualQ %>%
  group_by(site_no,year) %>%
  summarise(AnnualMeanQ = round(mean(flow_cms, na.rm = TRUE),1))
annualHW <- hw %>%
  group_by(site_no) %>%
  count(Year, sort = FALSE) %>%
  arrange(-desc(site_no))
annualHW$n <- ifelse(is.na(annualHW$n),0,annualHW$n)
longtermQ <- residualQ %>%
  group_by(site_no) %>%
  summarise(LongTermMeanQ = round(mean(flow_cms, na.rm = TRUE),1))

x1 <- as.data.frame(rep(seq(from = 1996, to = 2021, by = 1), times = 70)) # 70 stations
x2 <- as.data.frame(rep(unique(hw$site_no),times = 26)) # 26 years
names(x1)[1] <- "Year"
names(x2)[1] <- "site_no"
x2 <- x2 %>% arrange(-desc(site_no))
test <- cbind(x1, x2)
annualHW <- merge(annualHW, test, by = c("site_no","Year"), all = TRUE)
names(annualHW)[2] <- "year"
hw_q <- merge(annualQ, annualHW, by = c("site_no","year"), all = TRUE)
colnames(hw_q)[4] <- "TotalEvents_HW"
hw_q$TotalEvents_HW <- ifelse(is.na(hw_q$TotalEvents_HW),0,hw_q$TotalEvents_HW)
hw_q <- merge(hw_q, longtermQ, by = "site_no")
hw_q$NormalizedAnnualMeanQ <- round(((hw_q$AnnualMeanQ-hw_q$LongTermMeanQ)/hw_q$LongTermMeanQ)*100,digits = 1)

test <- hw_q
test2 <- merge(hw_q, hw_cs_site, by = "site_no")
round(mean(test2$TotalEvents_HW.x), digits = 0) # average 2 HW events per year
round(sd(test2$TotalEvents_HW.x), digits = 0) # standard dev. of 2 HW events per year

library(ggridges)

cols4 <- c("NE" = "#d73027", "ENC" = "#f46d43", "SE" = "#ffffbf",
           "WNC" = "#e0f3f8", "South" = "#abd9e9","SW" = "#74add1",
           "NW" = "#4575b4","West" = "#313695","Alaska" = "#a50026")

ggplot(test2, aes(x = NormalizedAnnualMeanQ, y = Region, fill = Region)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01) +
  scale_fill_manual(values = cols4) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  labs(y = "Region",
       x = "Normalized Annual Q (%)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

SIfig1_totHW <- ggplot(test2, aes(x = TotalEvents_HW.x, y = Region, fill = Region, height = ..density..)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01,
                      stat = "density", trim = TRUE) +
  scale_fill_manual(values = cols4) +
  scale_x_continuous(breaks = seq(0,12,1), limits = c(0,12)) +
  labs(y = "Region",
       x = "Annual Total HW Events") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

SIfig1_durHW <- ggplot(hw, aes(x = duration, y = Region, fill = Region, height = ..density..)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.0005,
                      stat = "density", trim = TRUE) +
  scale_fill_manual(values = cols4) +
  scale_x_continuous(breaks = seq(5,105,10), limits = c(5,105)) +
  labs(y = "Region",
       x = "HW Duration (days)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())

SIfig1_intHW <- ggplot(hw, aes(x = intensity_max_relThresh, y = Region, fill = Region, height = ..density..)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.0005,
                      stat = "density", trim = TRUE) +
  scale_fill_manual(values = cols4) +
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10)) +
  ylab("Region") +
  xlab(expression(HW~Max~Intensity~(degree*C))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())

# width = 1200, height = 500
ggarrange(SIfig1_totHW,SIfig1_durHW,SIfig1_intHW, nrow = 1, align = 'h', widths = c(1.2,1,1))

# width = 750 height = 550

SIfig2a <- hw %>%
  mutate(Month = factor(Month, levels = c("1","2","3","4","5","6","7","8","9","10",'11',"12"))) %>%
  ggplot(aes(x = Month, y = MeanResidaulQ)) +
  geom_boxplot(position = "dodge", color = "black", outlier.shape = NA) +
  # geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  ylab(expression(Mean~Residual~Q~During~HW~(m^3~s^-1))) +
  xlab("Month") +
  annotate("text", x = 11.5, y = 70, label = "(a", size = 6) +
  scale_fill_manual(values = cols) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  scale_y_continuous(breaks = seq(-150,75,25)) +
  coord_cartesian(ylim = c(-150, 75)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.12,0.95),
        legend.background = element_blank())

SIfig2b <- ggplot(data = hw_q, aes(x = AnnualMeanQ, y = TotalEvents_HW)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "black", stroke = 1, alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 1500, 100), limits = c(0,1500)) +
  scale_y_continuous(breaks = seq(0, 12, 1), limits = c(0,12)) +
  xlab(expression(Annual~Daily~Mean~Q~(m^3~s^-1))) +
  ylab("Total Annual HW Events") +
  annotate("text", x = 1450, y = 11.5, label = "(b", size = 6) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 45, vjust = 1, hjust= 1),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

# width = 600 height = 900
ggarrange(SIfig2a,SIfig2b, ncol = 1, align = 'v')
