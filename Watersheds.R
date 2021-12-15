library(dplyr)
library(purrr)
library(tidyr)
# devtools::install_github("markwh/streamstats")
library(streamstats)

rm(list = ls())

setwd("F:/School/USGSdata/GitHub")
dat <- read.csv("Station_Details_corLatlong.csv")
dat <- dat[!dat$STUSAB %in% c("TX","NV"),c(3:4,6)]

### Data broken into chunks due to size limit errors when writing the shapefiles
dat1 <- dat[1:5,]
dat2 <- dat[6:10,]
dat3 <- dat[11:15,]
dat4 <- dat[16:20,]
dat5 <- dat[21:25,]
dat6 <- dat[21:22,]
dat7 <- dat[23,]
dat8 <- dat[24,]
dat9 <- dat[25,]
dat10 <- dat[26:30,]
dat11 <- dat[31:35,]
dat12 <- dat[36:40,]
dat13 <- dat[41:45,]
dat14 <- dat[46:48,]
dat15 <- dat[49:50,]
dat16 <- dat[51:55,]
dat17 <- dat[56:60,]
dat18 <- dat[61,]
dat19 <- dat[62,]
dat20 <- dat[63:65,]
dat21 <- dat[66:70,]
dat22 <- dat[71:75,]
dat23 <- dat[76:80,]
dat24 <- dat[81:85,]
dat25 <- dat[86:90,]
dat26 <- dat[91:95,]
dat27 <- dat[96:100,]
dat28 <- dat[101,]
dat29 <- dat[102,]
dat30 <- dat[103,]

setTimeout(500000)

# dat1 <- data.frame(matrix(ncol = 3, nrow = 2))
# colnames(dat1) <- c("state", "lat", "long")
# dat1$state <- c("NJ", "NY")
# dat1$lat <- c(40.99194, 42.02458)
# dat1$long <- c(-74.28000, -75.11928)
# dat1

water_shed <- list()
for(i in 1:nrow(dat1)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat1$long[i], ylocation = dat1$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat1$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat2)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat2$long[i], ylocation = dat2$lat[i], crs = 4326,
                       includeparameters = "false",
                       includeflowtypes = "false",
                       includefeatures = "true",
                       rcode = dat2$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat3)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat3$long[i], ylocation = dat3$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat3$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat4)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat4$long[i], ylocation = dat4$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat4$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat5)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat5$long[i], ylocation = dat5$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat5$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat6)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat6$long[i], ylocation = dat6$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat6$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

d7 <- delineateWatershed(xlocation = dat7$long, ylocation = dat7$lat, crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat7$STUSAB)
writeShapefile(watershed = d7, layer = d7,
               dir = "F:/School/USGSdata/Watershed_Shapefiles",
               what = "boundary")

d8 <- delineateWatershed(xlocation = dat8$long, ylocation = dat8$lat, crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat8$STUSAB)
writeShapefile(watershed = d8, layer = d8,
               dir = "F:/School/USGSdata/Watershed_Shapefiles",
               what = "boundary")

d9 <- delineateWatershed(xlocation = dat9$long, ylocation = dat9$lat, crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat9$STUSAB)
writeShapefile(watershed = d9, layer = d9,
               dir = "F:/School/USGSdata/Watershed_Shapefiles",
               what = "boundary")

water_shed <- list()
for(i in 1:nrow(dat10)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat10$long[i], ylocation = dat10$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat10$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat11)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat11$long[i], ylocation = dat11$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat11$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat12)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat12$long[i], ylocation = dat12$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat12$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat13)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat13$long[i], ylocation = dat13$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat13$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat14)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat14$long[i], ylocation = dat14$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat14$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat15)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat15$long[i], ylocation = dat15$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat15$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat16)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat16$long[i], ylocation = dat16$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat16$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat17)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat17$long[i], ylocation = dat17$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat17$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

d18 <- delineateWatershed(xlocation = dat18$long, ylocation = dat18$lat, crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat18$STUSAB)
writeShapefile(watershed = d18, layer = d18,
               dir = "F:/School/USGSdata/Watershed_Shapefiles",
               what = "boundary")

d19 <- delineateWatershed(xlocation = dat19$long, ylocation = dat19$lat, crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat19$STUSAB)
writeShapefile(watershed = d19, layer = d19,
               dir = "F:/School/USGSdata/Watershed_Shapefiles",
               what = "boundary")

water_shed <- list()
for(i in 1:nrow(dat20)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat20$long[i], ylocation = dat20$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat20$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat21)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat21$long[i], ylocation = dat21$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat21$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat22)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat22$long[i], ylocation = dat22$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat22$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat23)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat23$long[i], ylocation = dat23$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat23$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat24)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat24$long[i], ylocation = dat24$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat24$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat25)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat25$long[i], ylocation = dat25$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat25$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat26)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat26$long[i], ylocation = dat26$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat26$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

water_shed <- list()
for(i in 1:nrow(dat27)){
  water_shed[[i]] <- 
    delineateWatershed(xlocation = dat27$long[i], ylocation = dat27$lat[i], crs = 4326,
                       includeparameters = "true", includeflowtypes = "true",
                       rcode = dat27$STUSAB[i])
}
for(i in 1:length(water_shed)){
  writeShapefile(watershed = water_shed[[i]],
                 layer = water_shed[[i]],
                 dir = "F:/School/USGSdata/Watershed_Shapefiles",
                 what = "boundary")
  
}

d28 <- delineateWatershed(xlocation = dat28$long, ylocation = dat28$lat, crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat28$STUSAB)
writeShapefile(watershed = d28, layer = d28,
               dir = "F:/School/USGSdata/Watershed_Shapefiles",
               what = "boundary")

### No streamstats for Alaska, below does not work
# d29 <- delineateWatershed(xlocation = dat29$long, ylocation = dat29$lat, crs = 4326,
#                          includeparameters = "true", includeflowtypes = "true",
#                          rcode = dat29$STUSAB)
# writeShapefile(watershed = d29, layer = d29,
#                dir = "F:/School/USGSdata/Watershed_Shapefiles",
#                what = "boundary")

d30 <- delineateWatershed(xlocation = dat30$long, ylocation = dat30$lat, crs = 4326,
                         includeparameters = "true", includeflowtypes = "true",
                         rcode = dat30$STUSAB)
writeShapefile(watershed = d30, layer = d30,
               dir = "F:/School/USGSdata/Watershed_Shapefiles",
               what = "boundary")
