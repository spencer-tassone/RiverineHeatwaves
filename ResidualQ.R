library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(zoo)
library(broom)
library(mgcv)
library(purrr)

rm(list = ls())
dev.off()

setwd("F:/School/USGSdata/GitHub")

Q_daily_dat <- read_csv('Q_daily_dat.csv', col_types = list(
  agency_cd = col_character(),
  site_no = col_character(),
  Date = col_date(),
  flow_cms = col_double(),
  Flow_cd = col_character()))
Q_daily_dat <- as.data.frame(Q_daily_dat)

# remove leap days
remove_leap <- as.Date(c("1996-02-29","2000-02-29","2004-02-29",
                         "2008-02-29","2012-02-29","2016-02-29","2020-02-29"))
Q_daily_dat <- Q_daily_dat[!Q_daily_dat$Date %in% remove_leap,]

# day of year that does not recognize leap day
Q_daily_dat <- Q_daily_dat %>% 
  mutate(DoY = day(Date),
         Month = month(Date),
         Year = year(Date)) %>% 
  group_by(Year, Month, site_no) %>%
  mutate(DoY = DoY - lag(DoY, default = 0)) %>%
  group_by(Year,site_no) %>%
  mutate(DoY = cumsum(DoY)) %>%
  select(-Month)
Q_daily_dat <- Q_daily_dat[,1:7]
Q_daily_dat <- as.data.frame(Q_daily_dat)

### Get median Q for each DoY at each station
Q_daily_dat <- Q_daily_dat %>%
  group_by(site_no, DoY) %>%
  mutate(DoY_median_flow_cms = round(median(flow_cms),2)) %>%
  ungroup

### Get a centered 14-day rolling mean of median DoY Q at each station
Q_daily_dat <- Q_daily_dat %>%
  group_by(site_no) %>%
  mutate(flow_14d_rollmean = round(rollmean(DoY_median_flow_cms, 14, fill = NA, align = "center"),2)) %>%
  ungroup

### Get GAM predictions based on median DoY Q at each station
mod_func <-function(Q_daily_dat){
  gam(DoY_median_flow_cms ~ s(DoY, bs = 'cc'), data = Q_daily_dat)
}

get_pred <- function(mod){
  predict(mod, newdata =  data.frame(DoY = 1:365), type = "response")
}

df2 <- Q_daily_dat %>% 
  {. ->> df_output} %>% 
  group_by(site_no) %>% 
  nest() %>% 
  mutate(mod = map(data, mod_func),
         pred = map(mod, get_pred)) %>% 
  unnest(pred) %>% 
  mutate(DoY = 1:365) %>% 
  dplyr::select(-data, -mod) %>%
  left_join(.,df_output, by = c("site_no","DoY"))

sorted <- df2 %>% arrange(site_no, Date)
sorted <- data.frame(sorted)
Qdat_resid <- sorted[,c(5,1,6,3,7:10,2)]
names(Qdat_resid)[names(Qdat_resid) == "pred"] <- "flow_gam_pred"

### Trying to determine if using the heatwave analysis on discharge data
### will be useful. Idea being that the climatology values could be used
### as the baseline to determine if obs. Q is high or low.

library(heatwaveR)

zz <- unique(Q_daily_dat$site_no)

for(i in 1:length(zz)){
  curDat = Q_daily_dat[Q_daily_dat$site_no == zz[i],]
  ts_Q = ts2clm(curDat, x = Date, y = flow_cms,
                climatologyPeriod = c(min(curDat$Date), max(curDat$Date)),
                clmOnly = TRUE)
  cur_ts_Q = ts_Q
  cur_ts_Q$site_no = zz[i]
  if( i == 1){
    save_ts_Q = cur_ts_Q
  } else{
    save_ts_Q = rbind(save_ts_Q, cur_ts_Q)
  }
}

save_ts_Q$date <- as.Date(save_ts_Q$doy, origin = "2019-12-31")
remove_leap <- as.Date("2020-02-29")
save_ts_Q <- save_ts_Q[!save_ts_Q$date %in% remove_leap,]
save_ts_Q$DoY <- rep(seq(1,365,1),104)
save_ts_Q <- save_ts_Q[,c(4,6,2)]

Qdat_resid <- merge(Qdat_resid,save_ts_Q, by = c("site_no","DoY"), all.x = T)
names(Qdat_resid)[names(Qdat_resid) == "seas"] <- "flow_mhw_seas"

### Difference between observed daily mean Q and predicted Q is residual Q
Qdat_resid$flow_gam_pred <- round(as.numeric(Qdat_resid$flow_gam_pred),2)
Qdat_resid$flow_mhw_seas <- round(as.numeric(Qdat_resid$flow_mhw_seas),2)
Qdat_resid$gam_residualQ <- Qdat_resid$flow_cms - Qdat_resid$flow_gam_pred
Qdat_resid$mhw_residualQ <- Qdat_resid$flow_cms - Qdat_resid$flow_mhw_seas
Qdat_resid$gam_residualQ_percent <- round((Qdat_resid$flow_cms/Qdat_resid$flow_gam_pred)*100,0)
Qdat_resid$mhw_residualQ_percent <- round((Qdat_resid$flow_cms/Qdat_resid$flow_mhw_seas)*100,0)

Qdat_resid <- Qdat_resid %>%
  arrange(Qdat_resid$site_no, Qdat_resid$Date)

write.csv(Qdat_resid,'ResidualQ.csv')
