library(readr)
library(zoo)
library(broom)
library(purrr)
library(heatwaveR)

rm(list = ls())
dev.off()

setwd("D:/School/USGSdata/GitHub")

Q_daily_dat <- read.csv('Q_daily_dat.csv', colClasses = c(agency_cd = "character",
                                                          site_no = "character",
                                                          Date = "Date",
                                                          flow_cms = "numeric",
                                                          Flow_cd = "character"))
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
Q_daily_dat <- as.data.frame(Q_daily_dat)

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
save_ts_Q$DoY <- rep(seq(1,365,1),52) # 52 sites
save_ts_Q <- save_ts_Q[,c(4,6,2)]

Qdat_resid <- merge(Q_daily_dat,save_ts_Q, by = c("site_no","DoY"), all.x = T)
names(Qdat_resid)[names(Qdat_resid) == "seas"] <- "flow_hw_seasonal"

### Difference between observed daily mean Q and predicted Q is residual Q
Qdat_resid$flow_hw_seasonal <- round(as.numeric(Qdat_resid$flow_hw_seasonal),2)
Qdat_resid$hw_residualQ <- Qdat_resid$flow_cms - Qdat_resid$flow_hw_seasonal
Qdat_resid$hw_residualQ_percent <- round((Qdat_resid$flow_cms/Qdat_resid$flow_hw_seasonal)*100,0)

Qdat_resid <- Qdat_resid %>%
  arrange(Qdat_resid$site_no, Qdat_resid$Date)

Qdat_resid <- Qdat_resid[,c(1,3,2,5:6,8:10)]

write.csv(Qdat_resid,'ResidualQ_v2.csv', row.names = FALSE)
