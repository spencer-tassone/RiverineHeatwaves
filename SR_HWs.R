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

setwd("F:/School/USGSdata/GitHub")

Wtemp_daily_dat <- read.csv('Wtemp_daily_dat.csv')
Wtemp_daily_dat$Date <- as.Date(Wtemp_daily_dat$Date)
Wtemp_daily_dat$site_no <- as.character(Wtemp_daily_dat$site_no)
Wtemp_daily_dat <- Wtemp_daily_dat[,2:25]
head(which(Wtemp_daily_dat$site_no == "10301500", arr.ind=TRUE)) # take 1 minus the smallest number and put into next line of code
Wtemp_daily_dat <- Wtemp_daily_dat %>%
  mutate(site_no = ifelse(row_number()<=483990, paste0("0", site_no), site_no))

station_details <- read.csv('station_details_corrected.csv')
station_details$site_no <- as.character(station_details$site_no)
head(which(station_details$site_no == "10301500", arr.ind=TRUE)) # take 1 minus the smallest number and put into next line of code
station_details <- station_details %>%
  mutate(site_no = ifelse(row_number()<=51, paste0("0", site_no), site_no))
station_details[82,3] <- "420451121510000"

residualQ <- read.csv('ResidualQ.csv')
residualQ$Date <- as.Date(residualQ$Date)
residualQ$site_no <- as.character(residualQ$site_no)
head(which(residualQ$site_no == "10301500", arr.ind=TRUE)) # take 1 minus the smallest number and put into next line of code
residualQ <- residualQ %>%
  mutate(site_no = ifelse(row_number()<=360620, paste0("0", site_no), site_no))

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

hw_cs_site <- merge(HW_event_station, CS_event_station, by = "site_no")
hw_cs_site <- merge(station_details, hw_cs_site, by = "site_no")

hw_cs_mod <- lm(hw_cs_site$TotalEvents_CS~hw_cs_site$TotalEvents_HW)
summary(hw_cs_mod)

ggplot(data = hw_cs_site, aes(x = TotalEvents_HW, y = TotalEvents_CS)) +
  geom_point(shape = 16, size = 2, alpha = 0.4) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0, linetype = "longdash") +
  labs(x = 'Total HW Events (1996-2021)',
       y = 'Total CS Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  scale_x_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  annotate("text", x = 2.25, y = 80, label = "y = 0.79x + 8.02", size = 5, hjust = 0.15) +
  annotate("text", x = 0, y = 70,
           label = "paste(R ^ 2, \" = 0.58\")", parse = TRUE, size = 5, hjust = 0) +
  annotate("text", x = 0, y = 75, label = "p-value < 0.05", size = 5, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

Fig2a <- ggplot(data = hw_cs_site, aes(x = altitude_m_NAVD88, y = TotalEvents_HW)) +
  geom_point(shape = 16, size = 2, alpha = 0.4) +
  stat_smooth(method = 'lm') +
  labs(x = 'NAVD88 Altitude (m)',
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  scale_x_continuous(breaks = seq(0,2800,250), limits = c(0,2800)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"))
hw_alt_mod <- lm(hw_cs_site$TotalEvents_HW~hw_cs_site$altitude_m_NAVD88)
summary(hw_alt_mod)

Fig2b <- ggplot(data = hw_cs_site, aes(x = altitude_m_NAVD88, y = TotalEvents_CS)) +
  geom_point(shape = 16, size = 2, alpha = 0.4) +
  stat_smooth(method = 'lm') +
  labs(x = 'NAVD88 Altitude (m)',
       y = 'Total CS Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  scale_x_continuous(breaks = seq(0,2800,250), limits = c(0,2800)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))
cs_alt_mod <- lm(hw_cs_site$TotalEvents_CS~hw_cs_site$altitude_m_NAVD88)
summary(cs_alt_mod)

ggarrange(Fig2a,Fig2b, ncol = 1)

Fig3a <- ggplot(data = hw_cs_site, aes(x = log10(DrainageArea_km2), y = TotalEvents_HW)) +
  geom_point(shape = 16, size = 2, alpha = 0.4) +
  stat_smooth(method = 'lm') +
  labs(x = expression(Drainage~Area~(km^2)),
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  # scale_x_continuous(breaks = seq(0,2800,250), limits = c(0,2800)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = "black"))

Fig3b <- ggplot(data = hw_cs_site, aes(x = log10(DrainageArea_km2), y = TotalEvents_CS)) +
  geom_point(shape = 16, size = 2, alpha = 0.4) +
  stat_smooth(method = 'lm') +
  labs(x = expression(log[10]~Drainage~Area~(km^2)),
       y = 'Total CS Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  # scale_x_continuous(breaks = seq(0,2800,250), limits = c(0,2800)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

ggarrange(Fig3a,Fig3b, ncol = 1)

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

hw_cs_site <- left_join(hw_cs_site, usa_region, by = 'STUSAB')
# hw <- left_join(hw, usa_region, by = 'STUSAB')

Fig4a <- ggplot(data = hw_cs_site, aes(x = Reservoir, y = TotalEvents_HW)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Reservoir Position',
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = 'black'))

Fig4b <- ggplot(data = hw_cs_site, aes(x = Reservoir, y = TotalEvents_CS)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Reservoir Position',
       y = 'Total CS Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'))

ggarrange(Fig4a, Fig4b, ncol = 1)

Fig5a <- ggplot(data = hw_cs_site, aes(x = factor(StreamOrder), y = TotalEvents_HW)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Stahler Stream Order',
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = 'black'))

Fig5b <- ggplot(data = hw_cs_site, aes(x = factor(StreamOrder), y = TotalEvents_CS)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Stahler Stream Order',
       y = 'Total CS Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'))

ggarrange(Fig5a, Fig5b, ncol = 1)

Fig6a <- hw_cs_site %>%
  mutate(Region = factor(Region, levels = c("Alaska", "NW","West","WNC","SW","ENC","Central","South","NE",'SE'))) %>%
  ggplot(aes(x = Region, y = TotalEvents_HW)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Region',
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = 'black'))

Fig6b <- hw_cs_site %>%
  mutate(Region = factor(Region, levels = c("Alaska", "NW","West","WNC","SW","ENC","Central","South","NE",'SE'))) %>%
  ggplot(aes(x = Region, y = TotalEvents_CS)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Region',
       y = 'Total CS Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'))

ggarrange(Fig6a, Fig6b, ncol = 1)

###

colnames(saveDatWarm)[23] <- "site_no"
colnames(saveDatCold)[23] <- "site_no"
hw <- unique(setDT(station_details)[, .(site_no, STUSAB)])[setDT(saveDatWarm), on = "site_no"]
hw <- unique(setDT(station_details)[, .(site_no, StreamOrder)])[setDT(hw), on = "site_no"]
hw <- unique(setDT(station_details)[, .(site_no, DrainageArea_km2)])[setDT(hw), on = "site_no"]
hw <- unique(setDT(station_details)[, .(site_no, Reservoir)])[setDT(hw), on = "site_no"]
cs <- unique(setDT(station_details)[, .(site_no, STUSAB)])[setDT(saveDatCold), on = "site_no"]
cs <- unique(setDT(station_details)[, .(site_no, StreamOrder)])[setDT(cs), on = "site_no"]
cs <- unique(setDT(station_details)[, .(site_no, DrainageArea_km2)])[setDT(cs), on = "site_no"]
cs <- unique(setDT(station_details)[, .(site_no, Reservoir)])[setDT(cs), on = "site_no"]
hw$Year <- year(hw$date_peak)
hw$Month <- month(hw$date_peak)
hw$Season <- ifelse(hw$Month >=12, "Winter",
                    ifelse(hw$Month >= 9, "Fall",
                           ifelse(hw$Month >= 6, "Summer",
                                  ifelse(hw$Month >= 3, "Spring",
                                         ifelse(hw$Month >=1, "Winter", NA)))))
cs$Year <- year(cs$date_peak)
cs$Month <- month(cs$date_peak)
cs$Season <- ifelse(cs$Month >=12, "Winter",
                    ifelse(cs$Month >= 9, "Fall",
                           ifelse(cs$Month >= 6, "Summer",
                                  ifelse(cs$Month >= 3, "Spring",
                                         ifelse(cs$Month >=1, "Winter", NA)))))

hw_season_tab <- as.data.frame(table(hw$Season))
colnames(hw_season_tab)[1] <- "Season"
colnames(hw_season_tab)[2] <- "Total_Events"
hw_season_tab$Type = "HW"
cs_season_tab <- as.data.frame(table(cs$Season))
colnames(cs_season_tab)[1] <- "Season"
colnames(cs_season_tab)[2] <- "Total_Events"
cs_season_tab$Type = "CS"
hw_cs_season_tab <- rbind(hw_season_tab, cs_season_tab)

cols <- c("HW" = "#000000", "CS" = "#ffffff")

hw_cs_season_tab %>%
  mutate(Type = factor(Type, levels = c("HW","CS"))) %>%
  mutate(Season = factor(Season, levels = c("Winter","Spring","Summer","Fall"))) %>%
  ggplot(aes(x = Season, y = Total_Events)) +
  geom_col(aes(fill = Type), position = "dodge", color = "black") +
  labs(y = "Total Number of Events",
       x = "Season") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(breaks = seq(0,1300,100)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.12,0.95),
        legend.background = element_blank())

### Mean residual Q (cms) during heatwave & coldspell events

ranges <- mapply(function(x, y, z)  seq.Date(y, z, 1), hw$site_no,  hw$date_start, hw$date_end, USE.NAMES = TRUE)
hw$MeanResidaulQ <- mapply(function(a, b)
  mean(residualQ$mhw_residualQ[residualQ$site_no == b][match(a, residualQ$Date[residualQ$site_no == b])], na.rm = T), ranges, names(ranges))

ranges <- mapply(function(x, y, z)  seq.Date(y, z, 1), cs$site_no,  cs$date_start, cs$date_end, USE.NAMES = TRUE)
cs$MeanResidaulQ <- mapply(function(a, b)
  mean(residualQ$mhw_residualQ[residualQ$site_no == b][match(a, residualQ$Date[residualQ$site_no == b])], na.rm = T), ranges, names(ranges))

hw <- left_join(hw, usa_region, by = 'STUSAB')
cs <- left_join(cs, usa_region, by = 'STUSAB')

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
hw_region$nStations[hw_region$Region == 'ENC'] <- 4
hw_region$nStations[hw_region$Region == 'NE'] <- 13
hw_region$nStations[hw_region$Region == 'NW'] <- 27
hw_region$nStations[hw_region$Region == 'SE'] <- 15
hw_region$nStations[hw_region$Region == 'South'] <- 6
hw_region$nStations[hw_region$Region == 'SW'] <- 11
hw_region$nStations[hw_region$Region == 'West'] <- 2
hw_region$nStations[hw_region$Region == 'WNC'] <- 3
hw_region$Frequency = round(hw_region$SumEvents/hw_region$nStations, digits = 2)

station_details %>%
  group_by(StreamOrder) %>%
  summarise(count = n_distinct(site_no))

hw_order$nStations[hw_order$StreamOrder == 1] <- 2
hw_order$nStations[hw_order$StreamOrder == 3] <- 4
hw_order$nStations[hw_order$StreamOrder == 4] <- 9
hw_order$nStations[hw_order$StreamOrder == 5] <- 8
hw_order$nStations[hw_order$StreamOrder == 6] <- 16
hw_order$nStations[hw_order$StreamOrder == 7] <- 27
hw_order$nStations[hw_order$StreamOrder == 8] <- 13
hw_order$nStations[hw_order$StreamOrder == 9] <- 3
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
hw_time <- hw_time[,c(1,7,2:6)]

#

cs_region <- cs %>%
  group_by(Year, Region) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            SumEvents = length(duration))

cs_season <- cs %>%
  group_by(Year, Season) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            Frequency = length(duration)/131)

cs_time <- cs %>%
  group_by(Year) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            Frequency = length(duration)/131)

cs_order <- cs %>%
  group_by(Year, StreamOrder) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            SumEvents = length(duration))

cs %>%
  group_by(Region) %>%
  summarise(count = n_distinct(site_no))

cs_region$nStations[cs_region$Region == 'Alaska'] <- 1
cs_region$nStations[cs_region$Region == 'ENC'] <- 4
cs_region$nStations[cs_region$Region == 'NE'] <- 13
cs_region$nStations[cs_region$Region == 'NW'] <- 27
cs_region$nStations[cs_region$Region == 'SE'] <- 15
cs_region$nStations[cs_region$Region == 'South'] <- 6
cs_region$nStations[cs_region$Region == 'SW'] <- 11
cs_region$nStations[cs_region$Region == 'West'] <- 2
cs_region$nStations[cs_region$Region == 'WNC'] <- 3
cs_region$Frequency = round(cs_region$SumEvents/cs_region$nStations, digits = 2)

station_details %>%
  group_by(StreamOrder) %>%
  summarise(count = n_distinct(site_no))

cs_order$nStations[cs_order$StreamOrder == 1] <- 2
cs_order$nStations[cs_order$StreamOrder == 3] <- 4
cs_order$nStations[cs_order$StreamOrder == 4] <- 9
cs_order$nStations[cs_order$StreamOrder == 5] <- 8
cs_order$nStations[cs_order$StreamOrder == 6] <- 16
cs_order$nStations[cs_order$StreamOrder == 7] <- 27
cs_order$nStations[cs_order$StreamOrder == 8] <- 13
cs_order$nStations[cs_order$StreamOrder == 9] <- 3
cs_order$Frequency = round(cs_order$SumEvents/cs_order$nStations, digits = 2)

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
cs_region <- merge(cs_region, region, by = c("Year","Region"), all = TRUE)
cs_region[is.na(cs_region)] <- 0

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
x1$StreamOrder <- 1
x2$StreamOrder <- 3
x3$StreamOrder <- 4
x4$StreamOrder <- 5
x5$StreamOrder <- 6
x6$StreamOrder <- 7
x7$StreamOrder <- 8
x8$StreamOrder <- 9
streamOrder <- rbind(x1,x2,x3,x4,x5,x6,x7,x8)
cs_order <- merge(cs_order, streamOrder, by = c("Year","StreamOrder"), all = TRUE)
cs_order[is.na(cs_order)] <- 0

cs_time$Type <- "CS"
cs_time <- cs_time[,c(1,7,2:6)]

aa <- unique(hw_time$Type)
bb <- unique(hw_region$Region)
cc <- unique(hw_season$Season)
dd <- unique(hw_order$StreamOrder)
ee <- unique(cs_time$Type)
ff <- unique(cs_region$Region)
gg <- unique(cs_season$Season)
hh <- unique(cs_order$StreamOrder)

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

for(i in 1:length(ee)){
  curDat = cs_time[cs_time$Type == ee[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = ee[i]
  cur_cs_time = data.frame(TestType = "Time",
                           Variable = "Avg.Duration",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    cs_time_duration = cur_cs_time
  } else{
    cs_time_duration = rbind(cs_time_duration, cur_cs_time)
  }
}
for(i in 1:length(ee)){
  curDat = cs_time[cs_time$Type == ee[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = ee[i]
  cur_cs_time = data.frame(TestType = "Time",
                           Variable = "Avg.CuInt",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    cs_time_intensity = cur_cs_time
  } else{
    cs_time_intensity = rbind(cs_time_intensity, cur_cs_time)
  }
}
for(i in 1:length(ee)){
  curDat = cs_time[cs_time$Type == ee[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = ee[i]
  cur_cs_time = data.frame(TestType = "Time",
                           Variable = "Avg.Onset",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    cs_time_onset = cur_cs_time
  } else{
    cs_time_onset = rbind(cs_time_onset, cur_cs_time)
  }
}
for(i in 1:length(ee)){
  curDat = cs_time[cs_time$Type == ee[i],]
  ts = ts(data = curDat[, 6],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = ee[i]
  cur_cs_time = data.frame(TestType = "Time",
                           Variable = "Avg.Decline",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    cs_time_decline = cur_cs_time
  } else{
    cs_time_decline = rbind(cs_time_decline, cur_cs_time)
  }
}
for(i in 1:length(ee)){
  curDat = cs_time[cs_time$Type == ee[i],]
  ts = ts(data = curDat[, 7],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Type = ee[i]
  cur_cs_time = data.frame(TestType = "Time",
                           Variable = "Frequency",
                           Category = "Time",
                           slope = round(slope, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    cs_time_freq = cur_cs_time
  } else{
    cs_time_freq = rbind(cs_time_freq, cur_cs_time)
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

for(i in 1:length(ff)){
  curDat = cs_region[cs_region$Region == ff[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = ff[i]
  cur_cs_region = data.frame(TestType = "Region",
                             Variable = "Avg.Duration",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_region_duration = cur_cs_region
  } else{
    cs_region_duration = rbind(cs_region_duration, cur_cs_region)
  }
}
for(i in 1:length(ff)){
  curDat = cs_region[cs_region$Region == ff[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = ff[i]
  cur_cs_region = data.frame(TestType = "Region",
                             Variable = "Avg.CuInt",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_region_intensity = cur_cs_region
  } else{
    cs_region_intensity = rbind(cs_region_intensity, cur_cs_region)
  }
}
for(i in 1:length(ff)){
  curDat = cs_region[cs_region$Region == ff[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = ff[i]
  cur_cs_region = data.frame(TestType = "Region",
                             Variable = "Avg.Onset",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_region_onset = cur_cs_region
  } else{
    cs_region_onset = rbind(cs_region_onset, cur_cs_region)
  }
}
for(i in 1:length(ff)){
  curDat = cs_region[cs_region$Region == ff[i],]
  ts = ts(data = curDat[, 6],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = ff[i]
  cur_cs_region = data.frame(TestType = "Region",
                             Variable = "Avg.Decline",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_region_decline = cur_cs_region
  } else{
    cs_region_decline = rbind(cs_region_decline, cur_cs_region)
  }
}
for(i in 1:length(ff)){
  curDat = cs_region[cs_region$Region == ff[i],]
  ts = ts(data = curDat[, 9],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Region = ff[i]
  cur_cs_region = data.frame(TestType = "Region",
                             Variable = "Frequency",
                             Category = Region,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_region_freq = cur_cs_region
  } else{
    cs_region_freq = rbind(cs_region_freq, cur_cs_region)
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

for(i in 1:length(gg)){
  curDat = cs_season[cs_season$Season == gg[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = gg[i]
  cur_cs_season = data.frame(TestType = "Season",
                             Variable = "Avg.Duration",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_season_duration = cur_cs_season
  } else{
    cs_season_duration = rbind(cs_season_duration, cur_cs_season)
  }
}
for(i in 1:length(gg)){
  curDat = cs_season[cs_season$Season == gg[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = gg[i]
  cur_cs_season = data.frame(TestType = "Season",
                             Variable = "Avg.CuInt",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_season_intensity = cur_cs_season
  } else{
    cs_season_intensity = rbind(cs_season_intensity, cur_cs_season)
  }
}
for(i in 1:length(gg)){
  curDat = cs_season[cs_season$Season == gg[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = gg[i]
  cur_cs_season = data.frame(TestType = "Season",
                             Variable = "Avg.Onset",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_season_onset = cur_cs_season
  } else{
    cs_season_onset = rbind(cs_season_onset, cur_cs_season)
  }
}
for(i in 1:length(gg)){
  curDat = cs_season[cs_season$Season == gg[i],]
  ts = ts(data = curDat[, 6],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = gg[i]
  cur_cs_season = data.frame(TestType = "Season",
                             Variable = "Avg.Decline",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_season_decline = cur_cs_season
  } else{
    cs_season_decline = rbind(cs_season_decline, cur_cs_season)
  }
}
for(i in 1:length(gg)){
  curDat = cs_season[cs_season$Season == gg[i],]
  ts = ts(data = curDat[, 7],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  Season = gg[i]
  cur_cs_season = data.frame(TestType = "Season",
                             Variable = "Frequency",
                             Category = Season,
                             slope = round(slope, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    cs_season_freq = cur_cs_season
  } else{
    cs_season_freq = rbind(cs_season_freq, cur_cs_season)
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

for(i in 1:length(hh)){
  curDat = cs_order[cs_order$StreamOrder == hh[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = hh[i]
  cur_cs_order = data.frame(TestType = "StreamOrder",
                            Variable = "Avg.Duration",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    cs_order_duration = cur_cs_order
  } else{
    cs_order_duration = rbind(cs_order_duration, cur_cs_order)
  }
}
for(i in 1:length(hh)){
  curDat = cs_order[cs_order$StreamOrder == hh[i],]
  ts = ts(data = curDat[, 4],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = hh[i]
  cur_cs_order = data.frame(TestType = "StreamOrder",
                            Variable = "Avg.CuInt",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    cs_order_intensity = cur_cs_order
  } else{
    cs_order_intensity = rbind(cs_order_intensity, cur_cs_order)
  }
}
for(i in 1:length(hh)){
  curDat = cs_order[cs_order$StreamOrder == hh[i],]
  ts = ts(data = curDat[, 5],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = hh[i]
  cur_cs_order = data.frame(TestType = "StreamOrder",
                            Variable = "Avg.Onset",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    cs_order_onset = cur_cs_order
  } else{
    cs_order_onset = rbind(cs_order_onset, cur_cs_order)
  }
}
for(i in 1:length(hh)){
  curDat = cs_order[cs_order$StreamOrder == hh[i],]
  ts = ts(data = curDat[, 6],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = hh[i]
  cur_cs_order = data.frame(TestType = "StreamOrder",
                            Variable = "Avg.Decline",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    cs_order_decline = cur_cs_order
  } else{
    cs_order_decline = rbind(cs_order_decline, cur_cs_order)
  }
}
for(i in 1:length(hh)){
  curDat = cs_order[cs_order$StreamOrder == hh[i],]
  ts = ts(data = curDat[, 9],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts)
  p.val = ManKen$sl
  slope = ss$estimates
  StreamOrder = hh[i]
  cur_cs_order = data.frame(TestType = "StreamOrder",
                            Variable = "Frequency",
                            Category = StreamOrder,
                            slope = round(slope, 3),
                            p.val = round(p.val, 4))
  if( i == 1){
    cs_order_freq = cur_cs_order
  } else{
    cs_order_freq = rbind(cs_order_freq, cur_cs_order)
  }
}

hw_time_output <- rbind(hw_time_duration, hw_time_intensity, hw_time_freq, hw_time_onset, hw_time_decline)
hw_time_output <- hw_time_output %>% arrange(-desc(p.val))
hw_time_output$Rank <- seq(1,5,1)

hw_region_output <- rbind(hw_region_duration, hw_region_intensity, hw_region_freq, hw_region_onset, hw_region_decline)
hw_region_output <- hw_region_output %>% arrange(-desc(p.val))
hw_region_output$Rank <- seq(1,45,1)

hw_season_output <- rbind(hw_season_duration, hw_season_intensity, hw_season_freq, hw_season_onset, hw_season_decline)
hw_season_output <- hw_season_output %>% arrange(-desc(p.val))
hw_season_output$Rank <- seq(1,20,1)

hw_order_output <- rbind(hw_order_duration, hw_order_intensity, hw_order_freq, hw_order_onset, hw_order_decline)
hw_order_output <- hw_order_output %>% arrange(-desc(p.val))
hw_order_output$Rank <- seq(1,40,1)

#

cs_time_output <- rbind(cs_time_duration, cs_time_intensity, cs_time_freq, cs_time_onset, cs_time_decline)
cs_time_output <- cs_time_output %>% arrange(-desc(p.val))
cs_time_output$Rank <- seq(1,5,1)

cs_region_output <- rbind(cs_region_duration, cs_region_intensity, cs_region_freq, cs_region_onset, cs_region_decline)
cs_region_output <- cs_region_output %>% arrange(-desc(p.val))
cs_region_output$Rank <- seq(1,45,1)

cs_season_output <- rbind(cs_season_duration, cs_season_intensity, cs_season_freq, cs_season_onset, cs_season_decline)
cs_season_output <- cs_season_output %>% arrange(-desc(p.val))
cs_season_output$Rank <- seq(1,20,1)

cs_order_output <- rbind(cs_order_duration, cs_order_intensity, cs_order_freq, cs_order_onset, cs_order_decline)
cs_order_output <- cs_order_output %>% arrange(-desc(p.val))
cs_order_output$Rank <- seq(1,40,1)

### 10% False Discovery Rate (see link for details: http://www.biostathandbook.com/multiplecomparisons.html)

fdr_table <- data.frame(matrix(ncol = 3, nrow = 110))
x <- c("TestType", "Rank", "FDR_0.1")
colnames(fdr_table) <- x
fdr_table[1:5,1] <- "Time"
fdr_table[6:50,1] <- "Region"
fdr_table[51:70,1] <- "Season"
fdr_table[71:110,1] <- "StreamOrder"
fdr_table[1:5,2] <- seq(1,5,1)
fdr_table[6:50,2] <- seq(1,45,1)
fdr_table[51:70,2] <- seq(1,20,1)
fdr_table[71:110,2] <- seq(1,40,1)
fdr_table[1:5,3] <- round((fdr_table$Rank[1:5]/5)*.1,4)
fdr_table[6:50,3] <- round((fdr_table$Rank[6:50]/45)*.1,4)
fdr_table[51:70,3] <- round((fdr_table$Rank[51:70]/20)*.1,4)
fdr_table[71:110,3] <- round((fdr_table$Rank[71:110]/40)*.1,4)

hw_MKSS_results <- rbind(hw_time_output, hw_region_output, hw_season_output, hw_order_output)
hw_MKSS_results <- left_join(hw_MKSS_results,fdr_table, by = c("TestType","Rank"))
hw_MKSS_results$SigTest <- ifelse(hw_MKSS_results$p.val < hw_MKSS_results$FDR_0.1,"Sig","NS")
hw_MKSS_results_sig <- hw_MKSS_results[c(1:3,51:52,71:72),]
hw_MKSS_results_sig$HWorCS <- "HW"
cs_MKSS_results <- rbind(cs_time_output, cs_region_output, cs_season_output, cs_order_output)
cs_MKSS_results <- left_join(cs_MKSS_results,fdr_table, by = c("TestType","Rank"))
cs_MKSS_results$SigTest <- ifelse(cs_MKSS_results$p.val < cs_MKSS_results$FDR_0.1,"Sig","NS")
cs_MKSS_results_sig <- cs_MKSS_results[c(1,6:7,71),]
cs_MKSS_results_sig$HWorCS <- "CS"
MKSS_results_sig <- rbind(hw_MKSS_results_sig, cs_MKSS_results_sig)
View(MKSS_results_sig)

hw_meanResidQ <- hw %>%
  group_by(StreamOrder) %>%
  summarise(MeanResidaulQ_HW = round(mean(MeanResidaulQ, na.rm = TRUE),digits = 1))

cs_meanResidQ <- cs %>%
  group_by(StreamOrder) %>%
  summarise(MeanResidaulQ_CS = round(mean(MeanResidaulQ, na.rm = TRUE),digits = 1))

streamOrder_meanResidQ <- merge(hw_meanResidQ,cs_meanResidQ, by = "StreamOrder")
streamOrder_meanResidQ

### HW & CS categories

saveCatWarm$year <- year(saveCatWarm$peak_date)
saveCatCold$year <- year(saveCatCold$peak_date)
hw_cat_temp <- saveCatWarm %>% count(category, year, sort = TRUE)
cs_cat_temp <- saveCatCold %>% count(category, year, sort = TRUE)

fillout_year <- rep(seq(from = 1996, to = 2021, by = 1), 4)
fillout_year <- sort(fillout_year)
fillout_cat <- rep(c("I Moderate", "II Strong", "III Severe", "IV Extreme"), 26)
fillout <- cbind(fillout_year,fillout_cat)
fillout <- as.data.frame(fillout)
names(fillout)[1] <- "year"
names(fillout)[2] <- "category"

hw_temp_fill <- merge(hw_cat_temp, fillout, by = c("year","category"), all = TRUE)
cs_temp_fill <- merge(cs_cat_temp, fillout, by = c("year","category"), all = TRUE)
hw_temp_fill$n[is.na(hw_temp_fill$n)] <- 0
cs_temp_fill$n[is.na(cs_temp_fill$n)] <- 0
colnames(hw_temp_fill)[3] <- "TotalEvents_HW"
colnames(cs_temp_fill)[3] <- "TotalEvents_CS"
hw_cs_totalEvents <- merge(hw_temp_fill, cs_temp_fill, by = c("year","category"))

hw_sum_cat <- saveCatWarm %>% group_by(year, category) %>% summarise(TotalDuration_HW = sum(duration))
cs_sum_cat <- saveCatCold %>% group_by(year, category) %>% summarise(TotalDuration_CS = sum(duration))

hw_sum_cat <- merge(hw_sum_cat, fillout, by = c("year","category"), all = TRUE)
cs_sum_cat <- merge(cs_sum_cat, fillout, by = c("year","category"), all = TRUE)
hw_cs_totalEvents <- merge(hw_cs_totalEvents, hw_sum_cat, by = c("year","category"))
hw_cs_totalEvents <- merge(hw_cs_totalEvents, cs_sum_cat, by = c("year","category"))

Fig7a <- ggplot(data = hw_cs_totalEvents) +
  geom_col(aes(x = as.numeric(year), y = TotalEvents_HW,
               fill = fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0,400,50)) +
  ylab("Total HW Events") +
  annotate("text", x = 2019.5, y = 400, label = "(a", size = 6) +
  guides(fill=guide_legend(title="Category")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = c(0.2,0.8),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

Fig7b <- ggplot(data = hw_cs_totalEvents) +
  geom_col(aes(x = as.numeric(year), y = TotalEvents_CS,
               fill = fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0,400,50)) +
  ylab("Total CS Events") +
  annotate("text", x = 2019.5, y = 400, label = "(b", size = 6) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 30, vjust = 0.95, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

hw_sum <- saveCatWarm %>% group_by(year) %>% summarise(TotalDurationPerSite = sum(duration)/82)
cs_sum <- saveCatCold %>% group_by(year) %>% summarise(TotalDurationPerSite = sum(duration)/82)

hw_sum_lm <- lm(hw_sum$TotalDurationPerSite~hw_sum$year)
cs_sum_lm <- lm(cs_sum$TotalDurationPerSite~hw_sum$year)
summary(hw_sum_lm)
summary(cs_sum_lm)

Fig7c <- ggplot(data = hw_sum, aes(x = year, y = TotalDurationPerSite)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0, 75, 10)) +
  ylab(expression(Avg.~Total~HW~Days~Station^-1)) +
  annotate("text", x = 1998.5, y = 75, label = "y = 0.62x - 1221", size = 5, hjust = 0.15) +
  annotate("text", x = 1996.75, y = 60,
           label = "paste(R ^ 2, \" = 0.17\")", parse = TRUE, size = 5, hjust = 0) +
  annotate("text", x = 1996.75, y = 68, label = "p-value = 0.037", size = 5, hjust = 0) +
  annotate("text", x = 2019.5, y = 75, label = "(c", size = 6, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

Fig7d <- ggplot(data = cs_sum, aes(x = year, y = TotalDurationPerSite)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  ylab(expression(Avg.~Total~CS~Days~Station^-1)) +
  annotate("text", x = 1998.5, y = 45, label = "y = -0.40x + 830", size = 5, hjust = 0.15) +
  annotate("text", x = 1996.75, y = 35,
           label = "paste(R ^ 2, \" = 0.13\")", parse = TRUE, size = 5, hjust = 0) +
  annotate("text", x = 1996.75, y = 40, label = "p-value = 0.072", size = 5, hjust = 0) +
  annotate("text", x = 2019.5, y = 45, label = "(d", size = 6, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 30, vjust = 0.95, hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

# width = 800 height = 700
ggarrange(Fig7a,Fig7c,Fig7b,Fig7d, nrow = 2, ncol = 2)

residualQ$year <- year(residualQ$Date)
annualQ <- residualQ %>%
  group_by(site_no,year) %>%
  summarise(AnnualMeanQ = round(mean(flow_cms, na.rm = TRUE),1))
annualHW <- hw %>%
  group_by(site_no) %>%
  count(Year, sort = FALSE) %>%
  arrange(-desc(site_no))
annualHW$n <- ifelse(is.na(annualHW$n),0,annualHW$n)
annualCS <- cs %>%
  group_by(site_no) %>%
  count(Year, sort = FALSE) %>%
  arrange(-desc(site_no))
annualCS$n <- ifelse(is.na(annualCS$n),0,annualCS$n)
x1 <- as.data.frame(rep(seq(from = 1996, to = 2021, by = 1), times = 82)) # 82 stations
x2 <- as.data.frame(rep(unique(hw$site_no),times = 26)) # 26 years
names(x1)[1] <- "Year"
names(x2)[1] <- "site_no"
x2 <- x2 %>% arrange(-desc(site_no))
test <- cbind(x1, x2)
annualHW <- merge(annualHW, test, by = c("site_no","Year"), all = TRUE)
annualCS <- merge(annualCS, test, by = c("site_no","Year"), all = TRUE)
names(annualHW)[2] <- "year"
hw_q <- merge(annualQ, annualHW, by = c("site_no","year"), all = TRUE)
names(annualCS)[2] <- "year"
cs_q <- merge(annualQ, annualCS, by = c("site_no","year"), all = TRUE)
colnames(hw_q)[4] <- "TotalEvents_HW"
colnames(cs_q)[4] <- "TotalEvents_CS"
hw_cs_q <- merge(hw_q, cs_q, by = c("site_no","year","AnnualMeanQ"))
hw_cs_q$TotalEvents_HW <- ifelse(is.na(hw_cs_q$TotalEvents_HW),0,hw_cs_q$TotalEvents_HW)
hw_cs_q$TotalEvents_CS <- ifelse(is.na(hw_cs_q$TotalEvents_CS),0,hw_cs_q$TotalEvents_CS)

Fig8a <- ggplot(data = hw_cs_q, aes(x = AnnualMeanQ, y = TotalEvents_HW)) + 
  geom_point(shape = 21, size = 2, color = "black", fill = "black", stroke = 1, alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 600, 50)) +
  scale_y_continuous(breaks = seq(0, 14, 2), limits = c(0,12)) +
  xlab(expression(Annual~Daily~Mean~Q~(m^3~s^-1))) +
  ylab("Total Annual HW Events") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

Fig8b <- ggplot(data = hw_cs_q, aes(x = AnnualMeanQ, y = TotalEvents_CS)) + 
  geom_point(shape = 21, size = 2, color = "black", fill = "black", stroke = 1, alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 600, 50)) +
  scale_y_continuous(breaks = seq(0, 14, 2), limits = c(0,12)) +
  xlab(expression(Annual~Daily~Mean~Q~(m^3~s^-1))) +
  ylab("Total Annual CS Events") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

# width = 700 height = 700
ggarrange(Fig8a,Fig8b, ncol = 1) # many NA values b/c not all sites have Q data

