library(dplyr)
library(purrr)
library(tidyr)
# devtools::install_github("markwh/streamstats")
library(streamstats)

rm(list = ls())

setwd("F:/School/USGSdata/GitHub")
dat <- read.csv("70station_details.csv")
dat <- dat %>%
  mutate(site_no = ifelse(site_no<=9217212100, paste0("0", site_no), site_no))
dat$site_no <- as.character(dat$site_no)

setTimeout(500000)

d1 <- delineateWatershed(xlocation = dat[1,4], ylocation = dat[1,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[1,6])
d2 <- delineateWatershed(xlocation = dat[2,4], ylocation = dat[2,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[2,6])
d3 <- delineateWatershed(xlocation = dat[3,4], ylocation = dat[3,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[3,6])
d4 <- delineateWatershed(xlocation = dat[4,4], ylocation = dat[4,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[4,6])
d5 <- delineateWatershed(xlocation = dat[5,4], ylocation = dat[5,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[5,6])
writeShapefile(watershed = d1, layer = d1,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d2, layer = d2,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d3, layer = d3,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d4, layer = d4,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d5, layer = d5,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d6 <- delineateWatershed(xlocation = dat[6,4], ylocation = dat[6,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[6,6])
d7 <- delineateWatershed(xlocation = dat[7,4], ylocation = dat[7,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[7,6])
d8 <- delineateWatershed(xlocation = dat[8,4], ylocation = dat[8,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[8,6])
d9 <- delineateWatershed(xlocation = dat[9,4], ylocation = dat[9,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[9,6])
d10 <- delineateWatershed(xlocation = dat[10,4], ylocation = dat[10,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[10,6])
writeShapefile(watershed = d6, layer = d6,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d7, layer = d7,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d8, layer = d8,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d9, layer = d9,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d10, layer = d10,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d11 <- delineateWatershed(xlocation = dat[11,4], ylocation = dat[11,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[11,6])
d12 <- delineateWatershed(xlocation = dat[12,4], ylocation = dat[12,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[12,6])
d13 <- delineateWatershed(xlocation = dat[13,4], ylocation = dat[13,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[13,6])
d14 <- delineateWatershed(xlocation = dat[14,4], ylocation = dat[14,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[14,6])
d15 <- delineateWatershed(xlocation = dat[15,4], ylocation = dat[15,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[15,6])
writeShapefile(watershed = d11, layer = d11,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d12, layer = d12,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d13, layer = d13,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d14, layer = d14,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d15, layer = d15,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d16 <- delineateWatershed(xlocation = dat[16,4], ylocation = dat[16,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[16,6])
d17 <- delineateWatershed(xlocation = dat[17,4], ylocation = dat[17,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[17,6])
d18 <- delineateWatershed(xlocation = dat[18,4], ylocation = dat[18,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[18,6])
d19 <- delineateWatershed(xlocation = dat[19,4], ylocation = dat[19,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[19,6])
d20 <- delineateWatershed(xlocation = dat[20,4], ylocation = dat[20,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[20,6])
writeShapefile(watershed = d16, layer = d16,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d17, layer = d17,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d18, layer = d18,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d19, layer = d19,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d20, layer = d20,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d21 <- delineateWatershed(xlocation = dat[21,4], ylocation = dat[21,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[21,6])
d22 <- delineateWatershed(xlocation = dat[22,4], ylocation = dat[22,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[22,6])

### StreamStats does not work in Michigan (d23-d26)
# d23 <- delineateWatershed(xlocation = dat[23,4], ylocation = dat[23,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[23,6])
# d24 <- delineateWatershed(xlocation = dat[24,4], ylocation = dat[24,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[24,6])
# d25 <- delineateWatershed(xlocation = dat[25,4], ylocation = dat[25,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[25,6])
writeShapefile(watershed = d21, layer = d21,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d22, layer = d22,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
# writeShapefile(watershed = d23, layer = d23,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
# writeShapefile(watershed = d24, layer = d24,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
# writeShapefile(watershed = d25, layer = d25,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
rm(list=setdiff(ls(), "dat"))
# d26 <- delineateWatershed(xlocation = dat[26,4], ylocation = dat[26,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[26,6])
d27 <- delineateWatershed(xlocation = dat[27,4], ylocation = dat[27,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[27,6])
d28 <- delineateWatershed(xlocation = dat[28,4], ylocation = dat[28,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[28,6])
d29 <- delineateWatershed(xlocation = dat[29,4], ylocation = dat[29,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[29,6])
d30 <- delineateWatershed(xlocation = dat[30,4], ylocation = dat[30,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[30,6])
# writeShapefile(watershed = d26, layer = d26,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
writeShapefile(watershed = d27, layer = d27,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d28, layer = d28,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d29, layer = d29,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d30, layer = d30,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d31 <- delineateWatershed(xlocation = dat[31,4], ylocation = dat[31,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[31,6])
d32 <- delineateWatershed(xlocation = dat[32,4], ylocation = dat[32,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[32,6])
d33 <- delineateWatershed(xlocation = dat[33,4], ylocation = dat[33,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[33,6])
d34 <- delineateWatershed(xlocation = dat[34,4], ylocation = dat[34,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[34,6])
d35 <- delineateWatershed(xlocation = dat[35,4], ylocation = dat[35,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[35,6])
writeShapefile(watershed = d31, layer = d31,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d32, layer = d32,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d33, layer = d33,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d34, layer = d34,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d35, layer = d35,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d36 <- delineateWatershed(xlocation = dat[36,4], ylocation = dat[36,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[36,6])
d37 <- delineateWatershed(xlocation = dat[37,4], ylocation = dat[37,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[37,6])
d38 <- delineateWatershed(xlocation = dat[38,4], ylocation = dat[38,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[38,6])

### Texas not covered by StreamStats
# d39 <- delineateWatershed(xlocation = dat[39,4], ylocation = dat[39,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[39,6])
# d40 <- delineateWatershed(xlocation = dat[40,4], ylocation = dat[40,3], crs = 4326,
#                           includeparameters = "true", includeflowtypes = "true",
#                           rcode = dat[40,6])
writeShapefile(watershed = d36, layer = d36,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d37, layer = d37,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d38, layer = d38,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
# writeShapefile(watershed = d39, layer = d39,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
# writeShapefile(watershed = d40, layer = d40,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
rm(list=setdiff(ls(), "dat"))
# d41 <- delineateWatershed(xlocation = dat[41,4], ylocation = dat[41,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[41,6])
# d42 <- delineateWatershed(xlocation = dat[42,4], ylocation = dat[42,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[42,6])
# d43 <- delineateWatershed(xlocation = dat[43,4], ylocation = dat[43,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[43,6])
d44 <- delineateWatershed(xlocation = dat[44,4], ylocation = dat[44,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[44,6])
d45 <- delineateWatershed(xlocation = dat[45,4], ylocation = dat[45,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[45,6])
# writeShapefile(watershed = d41, layer = d41,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
# writeShapefile(watershed = d42, layer = d42,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
# writeShapefile(watershed = d43, layer = d43,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
writeShapefile(watershed = d44, layer = d44,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d45, layer = d45,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))

### Nevada not covered by StreamStats
# d46 <- delineateWatershed(xlocation = dat[46,4], ylocation = dat[46,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[46,6])
d47 <- delineateWatershed(xlocation = dat[47,4], ylocation = dat[47,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[47,6])
d48 <- delineateWatershed(xlocation = dat[48,4], ylocation = dat[48,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[48,6])
d49 <- delineateWatershed(xlocation = dat[49,4], ylocation = dat[49,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[49,6])
d50 <- delineateWatershed(xlocation = dat[50,4], ylocation = dat[50,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[50,6])
# writeShapefile(watershed = d46, layer = d46,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
writeShapefile(watershed = d47, layer = d47,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d48, layer = d48,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d49, layer = d49,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d50, layer = d50,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d51 <- delineateWatershed(xlocation = dat[51,4], ylocation = dat[51,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[51,6])
d52 <- delineateWatershed(xlocation = dat[52,4], ylocation = dat[52,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[52,6])
d53 <- delineateWatershed(xlocation = dat[53,4], ylocation = dat[53,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[53,6])
d54 <- delineateWatershed(xlocation = dat[54,4], ylocation = dat[54,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[54,6])
d55 <- delineateWatershed(xlocation = dat[55,4], ylocation = dat[55,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[55,6])
writeShapefile(watershed = d51, layer = d51,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d52, layer = d52,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d53, layer = d53,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d54, layer = d54,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d55, layer = d55,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d56 <- delineateWatershed(xlocation = dat[56,4], ylocation = dat[56,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[56,6])
d57 <- delineateWatershed(xlocation = dat[57,4], ylocation = dat[57,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[57,6])
d58 <- delineateWatershed(xlocation = dat[58,4], ylocation = dat[58,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[58,6])
d59 <- delineateWatershed(xlocation = dat[59,4], ylocation = dat[59,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[59,6])
d60 <- delineateWatershed(xlocation = dat[60,4], ylocation = dat[60,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[60,6])
writeShapefile(watershed = d56, layer = d56,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d57, layer = d57,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d58, layer = d58,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d59, layer = d59,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d60, layer = d60,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d61 <- delineateWatershed(xlocation = dat[61,4], ylocation = dat[61,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[61,6])
d62 <- delineateWatershed(xlocation = dat[62,4], ylocation = dat[62,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[62,6])
d63 <- delineateWatershed(xlocation = dat[63,4], ylocation = dat[63,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[63,6])
d64 <- delineateWatershed(xlocation = dat[64,4], ylocation = dat[64,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[64,6])
d65 <- delineateWatershed(xlocation = dat[65,4], ylocation = dat[65,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[65,6])
writeShapefile(watershed = d61, layer = d61,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d62, layer = d62,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d63, layer = d63,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d64, layer = d64,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d65, layer = d65,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
rm(list=setdiff(ls(), "dat"))
d66 <- delineateWatershed(xlocation = dat[66,4], ylocation = dat[66,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[66,6])
d67 <- delineateWatershed(xlocation = dat[67,4], ylocation = dat[67,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[67,6])
d68 <- delineateWatershed(xlocation = dat[68,4], ylocation = dat[68,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[68,6])

### Alaska not covered by StreamStats
# d69 <- delineateWatershed(xlocation = dat[69,4], ylocation = dat[69,3], crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat[69,6])
d70 <- delineateWatershed(xlocation = dat[70,4], ylocation = dat[70,3], crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat[70,6])
writeShapefile(watershed = d66, layer = d66,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d67, layer = d67,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
writeShapefile(watershed = d68, layer = d68,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
# writeShapefile(watershed = d69, layer = d69,
#                dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
#                what = "boundary")
writeShapefile(watershed = d70, layer = d70,
               dir = "F:/School/USGSdata/GitHub/Watershed_Shapefiles",
               what = "boundary")
