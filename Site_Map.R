library(ggplot2)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(dplyr)
library(cowplot)

rm(list = ls())
dev.off()

world <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf")

n_america <- world %>% 
  filter(adm0_a3  %in% c("MEX", "CAN", "USA"))

n_america_usa <- world %>% 
  filter(adm0_a3  %in% c("USA"))

alaska <- world %>%
  filter(adm0_a3 %in% "USA")
alaska$state_nm <- "Alaska"

lakes <- rnaturalearth::ne_download(scale = 110,
                                    type = 'lakes',
                                    category = 'physical') %>%
  sf::st_as_sf(lakes110, crs = 4269)

usa_states <- st_as_sf(maps::map("state", 
                                 fill=TRUE, 
                                 plot =FALSE),
                       crs = 4269)

NE <- usa_states[usa_states$ID %in% c("connecticut","delaware","maine","maryland",
                                      "massachusetts","new hampshire","new jersey",
                                      "new york","pennsylvania","rhode island","vermont",
                                      "district of columbia"),]
upperMW <- usa_states[usa_states$ID %in% c("iowa","michigan","minnesota","wisconsin"),]
central <- usa_states[usa_states$ID %in% c("illinois","indiana","kentucky","missouri",
                                           "ohio","tennessee","west virginia"),]
SE <- usa_states[usa_states$ID %in% c("alabama","florida","georgia","north carolina",
                                      "south carolina","virginia"),]
WNC <- usa_states[usa_states$ID %in% c("montana","nebraska","north dakota",
                                       "south dakota",'wyoming'),]
south <- usa_states[usa_states$ID %in% c("arkansas","kansas","louisiana",
                                         "mississippi","oklahoma","texas"),]
SW <- usa_states[usa_states$ID %in% c("arizona","colorado","new mexico","utah"),]
NW <- usa_states[usa_states$ID %in% c("idaho","oregon","washington"),]
west <- usa_states[usa_states$ID %in% c("california","nevada"),]
Alaska_state <- usa_states[usa_states$ID %in% "Alaska",]

setwd("D:/School/USGSdata/GitHub")
dat <- read.csv("Station_Details.csv", colClasses = c(site_no = "character"))
qdat <- read.csv('Q_daily_dat.csv', colClasses = c(agency_cd = "character",
                                                   site_no = "character",
                                                   Date = "Date",
                                                   flow_cms = "numeric",
                                                   Flow_cd = "character"))

df <- data.frame(site_no = unique(qdat$site_no),
                   QyesNo = "Yes")
dat <- merge(dat, df, by = "site_no", all = TRUE)
dat$QyesNo <- ifelse(is.na(dat$QyesNo),"No",dat$QyesNo)

sf_all <- st_as_sf(dat, 
                   coords = c("long", "lat"),
                   crs = 4269)

############################################################################

dat[18,8] <- "No" # recall site was not usable due to missing Q data
table(dat$QyesNo) 

region_info <- data.frame(region = c("Northeast","Southeast","Central","South","ENC","WNC","Southwest","West","Northwest","Alaska"),
                          lat = c(39.77,31.22,40.0,27.68,44.37,46.5,35.09,37.96,46.20,26.0),
                          long = c(-69.7,-76.5,-87.8,-94.42,-94.5,-106.32,-109.03,-126.04,-128.5,-111.0))

cols <- c("Yes" = "black", "No" = "yellow")

big_blank <- ggplot() +
  geom_sf(data = NE,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#d73027") +
  geom_sf(data = upperMW,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#f46d43") +
  geom_sf(data = central,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#fdae61") +
  geom_sf(data = SE,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#ffffbf") +
  geom_sf(data = WNC,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#e0f3f8") +
  geom_sf(data = south,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#abd9e9") +
  geom_sf(data = SW,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#74add1") +
  geom_sf(data = NW,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#4575b4") +
  geom_sf(data = west,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "#313695") +
  geom_sf(data = lakes,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "white")  +
  geom_sf(data = sf_all,
          aes(fill = QyesNo),
          color = "black",
          shape = 21,
          size = 3,
          alpha = 0.5) +
  scale_fill_manual(labels = c("No (n = 19)",
                               "Yes (n = 51)"),
                    values = cols) +
  coord_sf(ylim = c(23, 49),
           xlim = c(-132, -69),
           expand = TRUE) +
  labs(fill = "Discharge Available") +
  annotation_scale(location = "br",
                   width_hint = 0.25, 
                   text_cex = 1) +
  annotation_north_arrow(location = "br", 
                         which_north = "grid", 
                         pad_x = unit(0.15, "in"),
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_text(data = region_info, aes(x = long, y = lat, label = region), size = 5) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        legend.position = c(0.55,0),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.title.align = 0.5,
        legend.background = element_rect(colour = "white", fill = "white"),
        legend.key = element_rect(colour = "white", fill = "white"))

inset_alaska <- ggplot(data = alaska) +
  geom_sf(color = "black", fill = "#a50026") +
  geom_sf(data = sf_all,
          aes(fill = QyesNo),
          color = "black",
          shape = 21,
          size = 3) +
  scale_fill_manual(values = c("red","yellow")) +
  # geom_sf_text(aes(label = Alaska_state$ID, geometry = Alaska_state$geom), fun.geometry = st_centroid) +
  coord_sf(xlim = c(-170, -130), ylim = c(52, 71), expand = TRUE) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")

### https://spatialreference.org/ref/?search=lambert+azimuthal+equal+area&srtext=Search
inset_world <- ggplot(data = world) +
  geom_sf(mapping = aes(geometry = geometry),
          color = "black",
          fill = "gray90") +
  geom_sf(data = n_america_usa,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "firebrick1")  +
  coord_sf(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +
  theme(panel.grid.major = element_line(color = gray(0.75), linetype = "dashed", size = 0.25),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank())

# width = 850 height = 750
ggdraw() +
  draw_plot(big_blank, x = -0.05, y = -0.05) +
  draw_plot(inset_alaska, x = -0.03, y = -0.03, width = 0.45, height = 0.45) +
  draw_plot(inset_world, x = 0.6, y = 0.63, width = 0.37, height = 0.37) +
  panel_border(remove = TRUE)
                   
