library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(zoo)
library(broom)
library(mgcv)
library(purrr)

rm(list = ls())
dev.off()

setwd("D:/School/USGSdata/GitHub")

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

### Difference between observed daily mean Q and predicted Q is residual Q
Qdat_resid$flow_gam_pred <- round(as.numeric(Qdat_resid$flow_gam_pred),2)
Qdat_resid$ResidualQ <- Qdat_resid$flow_cms - Qdat_resid$flow_gam_pred
Qdat_resid$ResidualQ_percent <- round((Qdat_resid$ResidualQ/Qdat_resid$flow_gam_pred)*100,0)

write.csv(Qdat_resid,'ResidualQ.csv')
