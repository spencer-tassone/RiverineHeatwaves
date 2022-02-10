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

station_details <- read.csv('70station_details.csv')
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

sum(wtemp_trends$wtemp_slope > 0)
sum(wtemp_trends$p.val <= 0.05)
sum(wtemp_trends$p.val <= 0.05 & wtemp_trends$wtemp_slope > 0 )
sum(Q_trends$Q_slope > 0)
sum(Q_trends$p.val <= 0.05)
sum(Q_trends$p.val <= 0.05 & Q_trends$Q_slope > 0)

# write.csv(Q_trends, 'Discharge_LongTermTrends.csv')

top9_sites <- wtemp_trends %>%
  arrange(desc(wtemp_slope)) %>%
  slice_head(n = 9)
top9_sites <- merge(top9_sites, station_details, by = "site_no")
four_sites <- wtemp_trends[wtemp_trends$site_no %in% c("07311782","02160700","0422026250","01388000"),]
four_sites <- merge(four_sites, station_details, by = "site_no")
four_dat <- annual_mean_wtemp[annual_mean_wtemp$site_no %in% four_sites$site_no,]
four_dat <- merge(four_dat, station_details, by = "site_no")

ylab = "Annual Mean Water Temp. (°C)"
wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")
my_label <- "S. Wichita River at Low Flow Dam near Guthrie, TX"
Fig1a <- ggplot(data = four_dat[four_dat$site_no == "07311782",], aes(x = Year, y = annual_mean_wtemp)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = y~x) +
  scale_y_continuous(breaks = seq(0, 22, 2), limits = c(0, 22)) +
  labs(x = "Year",
       y = ylab) +
  annotate("text", x = 2009, y = 0, label = "p-value = 0.001", size = 5) +
  annotate("text", x = 2009, y = 2, label = "Sen's slope = 0.043", size = 5) +
  annotate("text", x = 2009, y = 5, label = wrapper(my_label, width = 35), size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16, color = "black"))

Fig1b <- ggplot(data = four_dat[four_dat$site_no == "02160700",], aes(x = Year, y = annual_mean_wtemp)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = y~x) +
  scale_y_continuous(breaks = seq(0, 22, 2), limits = c(0, 22)) +
  labs(x = "Year",
       y = ylab) +
  annotate("text", x = 2009, y = 0, label = "p-value < 0.000", size = 5) +
  annotate("text", x = 2009, y = 2, label = "Sen's slope = 0.062", size = 5) +
  annotate("text", x = 2009, y = 4, label = "Enoree River at Whitmire, SC", size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, color = "white"))

Fig1c <- ggplot(data = four_dat[four_dat$site_no == "0422026250",], aes(x = Year, y = annual_mean_wtemp)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = y~x) +
  scale_y_continuous(breaks = seq(0, 22, 2), limits = c(0, 22)) +
  labs(x = "Year",
       y = ylab) +
  annotate("text", x = 2009, y = 0, label = "p-value = 0.017", size = 5) +
  annotate("text", x = 2009, y = 2, label = "Sen's slope = 0.041", size = 5) +
  annotate("text", x = 2009, y = 4, label = "Northrup Creek at North Greece, NY", size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

Fig1d <- ggplot(data = four_dat[four_dat$site_no == "01388000",], aes(x = Year, y = annual_mean_wtemp)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = y~x) +
  scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20)) +
  labs(x = "Year",
       y = ylab) +
  annotate("text", x = 2009, y = 0, label = "p-value = 0.015", size = 5) +
  annotate("text", x = 2009, y = 2, label = "Sen's slope = 0.041", size = 5) +
  annotate("text", x = 2009, y = 4, label = "Ramapo River at Pompton Lakes, NJ", size = 5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "white"),
        axis.title.y = element_blank())

ggarrange(Fig1a,Fig1b,Fig1c,Fig1d, ncol = 2, nrow = 2)

trends_wtemp_Q <- merge(wtemp_trends,Q_trends, by = "site_no", all = TRUE)
test1 <- wtemp_trends[wtemp_trends$p.val <= 0.05,]
test2 <- merge(test1, Q_trends, by = "site_no")
trends_wtemp_Q <- merge(trends_wtemp_Q, station_details, by = "site_no")
trends_wtemp_Q$Q_yes_no <- ifelse(is.na(trends_wtemp_Q$Q_slope),"no", "yes")
# write.csv(trends_wtemp_Q, "70station_details.csv")

ggplot(data = trends_wtemp_Q, aes(x = Q_slope, y = wtemp_slope)) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE) +
  scale_y_continuous(breaks = seq(-0.07, 0.07, 0.02), limits = c(-0.07, 0.07)) +
  scale_x_continuous(breaks = seq(-8.0, 3.0, 1.0), limits = c(-8.0, 3.0)) +
  labs(x = expression(Discharge~Trend~(m^3~s^-1~yr^-1)),
       y = expression(Water~Temperature~Trend~(C~yr^-1))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

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
TukeyHSD(hw_anova)

hw_cs_site %>%
  mutate(Region = factor(Region, levels = c("Alaska", "NW","West","WNC","SW","ENC","Central","South","NE",'SE'))) %>%
  ggplot(aes(x = Region, y = TotalEvents_HW)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  labs(x = 'Region',
       y = 'Total HW Events (1996-2021)') +
  scale_y_continuous(breaks = seq(0,80,10)) +
  coord_cartesian(ylim = c(0, 80)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'))

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
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.12,0.95),
        legend.background = element_blank())

hw %>%
  mutate(Month = factor(Month, levels = c("1","2","3","4","5","6","7","8","9","10",'11',"12"))) %>%
  ggplot(aes(x = Month, y = duration)) +
  geom_boxplot(position = "dodge", color = "black", outlier.shape = NA) +
  labs(y = "Duration (days)",
       x = "Month") +
  scale_y_continuous(breaks = seq(0,25,5)) +
  coord_cartesian(ylim = c(0, 25)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.12,0.95),
        legend.background = element_blank())

onset_fig <- hw %>%
  mutate(Month = factor(Month, levels = c("1","2","3","4","5","6","7","8","9","10",'11',"12"))) %>%
  ggplot(aes(x = Month, y = rate_onset)) +
  geom_boxplot(position = "dodge", color = "black", outlier.shape = NA) +
  ylab(expression(Onset~Rate~(degree*C~day^-1))) +
  xlab("Month") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(breaks = seq(0,2.0,0.25)) +
  coord_cartesian(ylim = c(0, 2.0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16, color = "white"),
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.position = 'none')

decline_fig <- hw %>%
  mutate(Month = factor(Month, levels = c("1","2","3","4","5","6","7","8","9","10",'11',"12"))) %>%
  ggplot(aes(x = Month, y = rate_decline)) +
  geom_boxplot(position = "dodge", color = "black", outlier.shape = NA) +
  ylab(expression(Decline~Rate~(degree*C~day^-1))) +
  xlab("Month") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(breaks = seq(0,2.25,0.25)) +
  coord_cartesian(ylim = c(0, 2.25)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.position = 'none')

# width = 850 height = 800
ggarrange(onset_fig,decline_fig,ncol = 1)

### Mean residual Q (cms) during heatwave & coldspell events

ranges <- mapply(function(x, y, z)  seq.Date(y, z, 1), hw$site_no,  hw$date_start, hw$date_end, USE.NAMES = TRUE)
hw$MeanResidaulQ <- mapply(function(a, b)
  mean(residualQ$mhw_residualQ[residualQ$site_no == b][match(a, residualQ$Date[residualQ$site_no == b])], na.rm = T), ranges, names(ranges))

hw <- left_join(hw, usa_region, by = 'STUSAB')

hw %>%
  mutate(Month = factor(Month, levels = c("1","2","3","4","5","6","7","8","9","10",'11',"12"))) %>%
  ggplot(aes(x = Month, y = MeanResidaulQ)) +
  geom_boxplot(position = "dodge", color = "black", outlier.shape = NA) +
  # geom_jitter(shape=16, alpha = 0.4, size = 2, position=position_jitter(0.1)) +
  ylab(expression(Mean~Residual~Q~During~HW~(m^3~s^-1))) +
  xlab("Month") +
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
            Frequency = length(duration)/NROW(unique(hw$site_no)))

hw_time <- hw %>%
  group_by(Year) %>%
  summarise(Avg.Duration = mean(duration, na.rm = T),
            Avg.CuInt = mean(intensity_cumulative_relThresh, na.rm = T),
            Avg.Onset = mean(rate_onset, na.rm = T),
            Avg.Decline = mean(rate_decline, na.rm = T),
            Frequency = length(duration)/NROW(unique(hw$site_no)))

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
hw_region_output$Rank <- seq(1,45,1)

hw_season_output <- rbind(hw_season_duration, hw_season_intensity, hw_season_freq, hw_season_onset, hw_season_decline)
hw_season_output <- hw_season_output %>% arrange(-desc(p.val))
hw_season_output$Rank <- seq(1,20,1)

hw_order_output <- rbind(hw_order_duration, hw_order_intensity, hw_order_freq, hw_order_onset, hw_order_decline)
hw_order_output <- hw_order_output %>% arrange(-desc(p.val))
hw_order_output$Rank <- seq(1,40,1)

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
hw_MKSS_results_sig <- hw_MKSS_results[c(1:2,51:52,71:78),]
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

Fig7a <- ggplot(data = hw_cat_total) +
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

hw_sum <- saveCatWarm %>% group_by(year) %>% summarise(TotalDurationPerSite = sum(duration)/NROW(unique(saveCatWarm$Station)))
hw_sum_lm <- lm(hw_sum$TotalDurationPerSite~hw_sum$year)
summary(hw_sum_lm)

Fig7b <- ggplot(data = hw_sum, aes(x = year, y = TotalDurationPerSite)) +
  geom_smooth(method = "lm", formula = y~x, color = "black", size = 0.5, se = TRUE) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white", stroke = 1) +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1996, 2021, 4)) +
  scale_y_continuous(breaks = seq(0, 60, 5)) +
  ylab(expression(Avg.~Total~HW~Days~Station^-1)) +
  annotate("text", x = 1998.5, y = 55, label = "y = 0.64x - 1282", size = 5, hjust = 0.23) +
  annotate("text", x = 1996.75, y = 50,
           label = "paste(R ^ 2, \" = 0.21\")", parse = TRUE, size = 5, hjust = 0) +
  annotate("text", x = 1996.75, y = 45, label = "p-value = 0.017", size = 5, hjust = 0) +
  annotate("text", x = 2019.5, y = 55, label = "(b", size = 6, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"))

# width = 600 height = 800
ggarrange(Fig7a,Fig7b, ncol = 1)

residualQ$year <- year(residualQ$Date)
annualQ <- residualQ %>%
  group_by(site_no,year) %>%
  summarise(AnnualMeanQ = round(mean(flow_cms, na.rm = TRUE),1))
annualHW <- hw %>%
  group_by(site_no) %>%
  count(Year, sort = FALSE) %>%
  arrange(-desc(site_no))
annualHW$n <- ifelse(is.na(annualHW$n),0,annualHW$n)

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

ggplot(data = hw_q, aes(x = AnnualMeanQ, y = TotalEvents_HW)) + 
  geom_point(shape = 21, size = 2, color = "black", fill = "black", stroke = 1, alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 1500, 100), limits = c(0,1500)) +
  scale_y_continuous(breaks = seq(0, 13, 2), limits = c(0,13)) +
  xlab(expression(Annual~Daily~Mean~Q~(m^3~s^-1))) +
  ylab("Total Annual HW Events") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16, color = "black", angle = 45, vjust = 1, hjust= 1),
        axis.text.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        legend.title = element_blank())

