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
  mutate(site_no = ifelse(row_number()<=427050, paste0("0", site_no), site_no))

station_details <- read.csv('70station_details.csv') # be sure to remove the first column in the csv prior to importing
station_details$site_no <- as.character(station_details$site_no)
head(which(station_details$site_no == "10301500", arr.ind=TRUE)) # take 1 minus the smallest number and put into next line of code
station_details <- station_details %>%
  mutate(site_no = ifelse(row_number()<=45, paste0("0", site_no), site_no))
station_details[70,2] <- "420451121510000"

residualQ <- read.csv('ResidualQ.csv')
residualQ$Date <- as.Date(residualQ$Date)
residualQ$site_no <- as.character(residualQ$site_no)
head(which(residualQ$site_no == "10301500", arr.ind=TRUE)) # take 1 minus the smallest number and put into next line of code
residualQ <- residualQ %>%
  mutate(site_no = ifelse(row_number()<=303680, paste0("0", site_no), site_no))

### Are there long-term trends in annual mean water temperature and discharge?

annual_mean_wtemp <- Wtemp_daily_dat %>%
  group_by(site_no, Year) %>%
  summarise(annual_mean_wtemp = mean(corWtemp, na.rm = TRUE))

residualQ$Year <- year(residualQ$Date)
annual_mean_Q <- residualQ %>%
  group_by(site_no, Year) %>%
  summarise(MeanQ = mean(flow_cms, na.rm = TRUE))
annual_mean_Q <- annual_mean_Q[!annual_mean_Q$site_no == "02423397",]

library(Kendall)
library(trend)

aa <- unique(annual_mean_wtemp$site_no)
for(i in 1:length(aa)){
  curDat = annual_mean_wtemp[annual_mean_wtemp$site_no == aa[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts, conf.level = 0.95)
  p.val = ManKen$sl
  slope = ss$estimates
  lower95 = ss$conf.int[1]
  upper95 = ss$conf.int[2]
  site_no = aa[i]
  cur_temp_time = data.frame(site_no = aa[i],
                             wtemp_slope = round(slope, 3),
                             upper95 = round(upper95, 3),
                             lower95 = round(lower95, 3),
                             p.val = round(p.val, 4))
  if( i == 1){
    wtemp_trends = cur_temp_time
  } else{
    wtemp_trends = rbind(wtemp_trends, cur_temp_time)
  }
}

# write.csv(wtemp_trends, 'WaterTemp_LongTermTrends.csv')

zz <- unique(annual_mean_Q$site_no)
for(i in 1:length(zz)){
  curDat = annual_mean_Q[annual_mean_Q$site_no == zz[i],]
  ts = ts(data = curDat[, 3],
          frequency = 1,
          start = min(curDat$Year),
          end = max(curDat$Year))
  ManKen = MannKendall(ts)
  ss = sens.slope(ts, conf.level = 0.95)
  p.val = ManKen$sl
  slope = ss$estimates
  lower95 = ss$conf.int[1]
  upper95 = ss$conf.int[2]
  site_no = zz[i]
  cur_Q_trends = data.frame(site_no = site_no,
                           Q_slope = round(slope, 3),
                           upper95 = round(upper95, 3),
                           lower95 = round(lower95, 3),
                           p.val = round(p.val, 4))
  if( i == 1){
    Q_trends = cur_Q_trends
  } else{
    Q_trends = rbind(Q_trends, cur_Q_trends)
  }
}

View(wtemp_trends)
View(Q_trends)

sum(wtemp_trends$wtemp_slope > 0) # 65
sum(wtemp_trends$p.val <= 0.05) # 20
sum(wtemp_trends$p.val <= 0.05 & wtemp_trends$wtemp_slope > 0 ) # 19
sum(Q_trends$Q_slope > 0) # 26
sum(Q_trends$p.val <= 0.05) # 10
sum(Q_trends$p.val <= 0.05 & Q_trends$Q_slope > 0) # 3

trends_wtemp_Q <- merge(wtemp_trends,Q_trends, by = "site_no", all = TRUE)
test1 <- wtemp_trends[wtemp_trends$p.val <= 0.05,]
test2 <- merge(test1, Q_trends, by = "site_no")
trends_wtemp_Q <- merge(trends_wtemp_Q, station_details, by = "site_no")
trends_wtemp_Q$Q_yes_no <- ifelse(is.na(trends_wtemp_Q$Q_slope),"no", "yes")

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

cols <- c("1" = "#ffffd9", "3" = "#edf8b1", "4" = "#c7e9b4", "5" = "#7fcdbb",
          "6" = "#41b6c4", "7" = "#1d91c0", "8" = "#225ea8", "9" = "#0c2c84")
cols2 <- c("Below" = "#edf8b1", "Above" = "#7fcdbb", "None" = "#2c7fb8")

hw_alt_mod <- lm(hw_cs_site$TotalEvents_HW~hw_cs_site$altitude_m_NAVD88)
summary(hw_alt_mod)

ggplot(data = hw_cs_site, aes(x = altitude_m_NAVD88, y = TotalEvents_HW)) +
  geom_point(shape = 16, size = 2, alpha = 0.7, color = "black") +
  stat_smooth(method = 'lm') +
  labs(x = 'NAVD88 Altitude (m)',
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  scale_x_continuous(breaks = seq(0,2800,250), limits = c(0,2800)) +
  annotate("text", x = 2000, y = 12, label = "y = -0.006x + 59.2", size = 5) +
  annotate("text", x = 2000, y = 6, label = "p-value = 0.001", size = 5) +
  annotate("text", x = 2000, y = 0, label = expression(paste(R^2," = 0.15")), size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

ggplot(data = hw_cs_site, aes(x = log10(DrainageArea_km2), y = TotalEvents_HW)) +
  geom_point(shape = 16, size = 2, alpha = 0.4) +
  stat_smooth(method = 'lm') +
  labs(x = expression(log[10]~Drainage~Area~(km^2)),
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10), limits = c(0,80)) +
  # scale_x_continuous(breaks = seq(0,2800,250), limits = c(0,2800)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

hw_cs_site %>%
  group_by(Reservoir) %>%
  summarise(Median = round(median(TotalEvents_HW)),
            SD = round(round(sd(TotalEvents_HW))),
            n = n())

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

trends_wtemp_Q <- left_join(trends_wtemp_Q, usa_region, by = 'STUSAB')
wtemp_q_lm <- lm(wtemp_slope~Q_slope, data = trends_wtemp_Q)
summary(wtemp_q_lm)

cols3 <- c("NE" = "#d73027", "ENC" = "#f46d43", "SE" = "#ffffbf",
          "WNC" = "#e0f3f8", "South" = "#abd9e9","SW" = "#74add1",
          "NW" = "#4575b4","West" = "#313695","Alaska" = "#a50026")

ggplot(data = trends_wtemp_Q, aes(x = Q_slope, y = wtemp_slope)) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(shape = 21, size = 3, aes(fill = factor(Region))) +
  scale_fill_manual(values = cols3) +
  stat_smooth(method = 'lm', se = FALSE) +
  scale_y_continuous(breaks = seq(-0.07, 0.07, 0.02), limits = c(-0.07, 0.07)) +
  scale_x_continuous(breaks = seq(-8.0, 3.0, 1.0), limits = c(-8.0, 3.0)) +
  labs(x = expression(Discharge~Trend~(m^3~s^-1~yr^-1)),
       y = expression(Water~Temperature~Trend~(C~yr^-1)),
       fill = "Region") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.title.align = 0.5,
        legend.position = c(0.2,0.2)) +
  guides(fill=guide_legend(ncol=2))

hw_cs_site <- left_join(hw_cs_site, usa_region, by = 'STUSAB')

hw_anova <- aov(TotalEvents_HW~Reservoir, data = hw_cs_site)
summary(hw_anova)

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

hw_anova <- aov(TotalEvents_HW~factor(StreamOrder), data = hw_cs_site)
summary(hw_anova)

ggplot(data = hw_cs_site, aes(x = factor(StreamOrder), y = TotalEvents_HW)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Stahler Stream Order',
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10)) +
  coord_cartesian(ylim = c(0, 80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'))

hw_anova <- aov(TotalEvents_HW~Region, data = hw_cs_site)
summary(hw_anova)
hw_anova_region <- TukeyHSD(hw_anova)
hw_anova_region <- as.data.frame(hw_anova_region$Region)
hw_anova_region[hw_anova_region$`p adj` < 0.05,]

library(dplyr)
library(rstatix)
library(ggpubr)

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
  summarise(MedianTotEvents = round(median(TotalEvents_HW)),
            SD = round(sd(TotalEvents_HW)),
            max = max(TotalEvents_HW),
            min = min(TotalEvents_HW))

###

colnames(saveDatWarm)[23] <- "site_no"
colnames(saveDatCold)[23] <- "site_no"
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

hw %>%
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

### Mean residual Q (cms) during heatwave & coldspell events

ranges <- mapply(function(x, y, z)  seq.Date(y, z, 1), hw$site_no,  hw$date_start, hw$date_end, USE.NAMES = TRUE)
hw$MeanResidaulQ <- mapply(function(a, b)
  mean(residualQ$mhw_residualQ[residualQ$site_no == b][match(a, residualQ$Date[residualQ$site_no == b])], na.rm = T), ranges, names(ranges))

hw <- left_join(hw, usa_region, by = 'STUSAB')

hw_region <- hw %>%
  group_by(Year, Region) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            SumEvents = length(duration))

hw_season <- hw %>%
  group_by(Year, Season) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Frequency = length(duration)/NROW(unique(hw$site_no)))

hw_time <- hw %>%
  group_by(Year) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Frequency = length(duration)/NROW(unique(hw$site_no)))

hw_order <- hw %>%
  group_by(Year, StreamOrder) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            SumEvents = length(duration))

hw_reservoir <- hw %>%
  group_by(Year, Reservoir) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            SumEvents = length(duration))

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

aa <- unique(hw_time$Type)
bb <- unique(hw_region$Region)
cc <- unique(hw_season$Season)
dd <- unique(hw_order$StreamOrder)
ee <- unique(hw_reservoir$Reservoir)

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

### 15% False Discovery Rate (see link for details: http://www.biostathandbook.com/multiplecomparisons.html)
### using 10% FDR we had 7 stat. sig. results = 7 * 0.1 = 0.7 tests were false positives
### using 15% FDR we had 9 stat. sig. results = 9 * 0.15 = 1.35 tests were false positives & largest raw p-value = 0.018
### using 20% FDR we had 12 stat. sig. results = 12 * 0.2 = 2.4 tests were false positives & largest raw p-value = 0.048

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
hw_MKSS_results_sig <- hw_MKSS_results[c(1,31,43:48,67:69),] # two results have p-value < 0.05 but greater than the adjusted critical value. Felt worthy of reporting them along with their critical value.
View(hw_MKSS_results_sig)
# write.csv(hw_MKSS_results_sig, '70sites_MKSS_results.csv')

hw_meanResidQ <- hw %>%
  group_by(StreamOrder) %>%
  summarise(Mean_ResidaulQ_HW = round(mean(MeanResidaulQ, na.rm = TRUE),digits = 1),
            Median_ResidaulQ_HW = round(median(MeanResidaulQ, na.rm = TRUE),digits = 1),
            SD_HW = round(sd(MeanResidaulQ, na.rm = TRUE),digits = 1))

### HW & CS categories

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

hw_sum_cat <- saveCatWarm %>% group_by(year, category) %>% summarise(TotalDuration_HW = sum(duration))

hw_sum_cat <- merge(hw_sum_cat, fillout, by = c("year","category"), all = TRUE)
hw_cat_total <- cbind(hw_sum_cat, hw_temp_fill)
hw_cat_total <- hw_cat_total[,c(1:3,6)]

hw_cat_total %>%
  group_by(category) %>%
  summarise(TotalEvents = sum(TotalEvents_HW),
            FracTotEvents = round(((TotalEvents/3985)*100),digits = 2))

test <- saveCatWarm
colnames(test)[12] <- "site_no"
test <- merge(test, station_details, by = "site_no")
test <- merge(test, usa_region, by = "STUSAB")
test <- test[,c(1:21,32:34)]

Fig2a <- ggplot(data = hw_cat_total) +
  geom_col(aes(x = as.numeric(year), y = TotalEvents_HW,
               fill = fct_rev(as_factor(category))), color = "black",
           width = 1) +
  scale_fill_manual(values = c("#000000", "#777777","#C7C7C7","#FFFFFF")) +
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
        legend.position = c(0.18,0.8),
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
moderate_lm <- lm(TotalEvents_HW~year, data = moderate)
strong_lm <- lm(TotalEvents_HW~year, data = strong)
severe_lm <- lm(TotalEvents_HW~year, data = severe)
extreme_lm <- lm(TotalEvents_HW~year, data = extreme)
summary(moderate_lm)
summary(strong_lm)
summary(severe_lm)
summary(extreme_lm)

hw_sum <- saveCatWarm %>%
  group_by(year, Station) %>%
  summarise(TotalDuration = sum(duration, na.rm = TRUE))
hw_sum2 <- hw_sum %>%
  group_by(year) %>%
  summarise(MeanAnnualTotalDuration = mean(TotalDuration, na.rm = TRUE))
hw_sum_lm <- lm(hw_sum2$MeanAnnualTotalDuration~hw_sum2$year)
summary(hw_sum_lm)

Fig2b <- ggplot(data = hw_sum2, aes(x = year, y = MeanAnnualTotalDuration)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0, 60, 5), limits = c(0,60)) +
  ylab("Avg. Total HW Days") +
  annotate("text", x = 1998.4, y = 60, label = "y = 0.64x - 1262", size = 5, hjust = 0.23) +
  annotate("text", x = 1996.75, y = 54,
           label = "paste(R ^ 2, \" = 0.23\")", parse = TRUE, size = 5, hjust = 0) +
  annotate("text", x = 1996.75, y = 48, label = "p-value = 0.013", size = 5, hjust = 0) +
  annotate("text", x = 2019.5, y = 55, label = "(b", size = 6, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

# width = 600 height = 800
ggarrange(Fig2a,Fig2b, ncol = 1, align = 'v')

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

library(ggridges)

ggplot(test2, aes(x = NormalizedAnnualMeanQ, y = Region, fill = Region)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01) +
  scale_fill_manual(values = cols3) +
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
  scale_fill_manual(values = cols3) +
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
  scale_fill_manual(values = cols3) +
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
  scale_fill_manual(values = cols3) +
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

Fig3a <- hw %>%
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

Fig3b <- ggplot(data = hw_q, aes(x = AnnualMeanQ, y = TotalEvents_HW)) +
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
ggarrange(Fig3a,Fig3b, ncol = 1, align = 'v')

