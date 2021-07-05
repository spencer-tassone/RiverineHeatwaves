library(ggplot2)
library(ggspatial)
library(sf)
library(rnaturalearth)

rm(list = ls())
dev.off()

world <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf")

alaska <- world %>%
  filter(adm0_a3 %in% "USA")

lakes <- rnaturalearth::ne_download(scale = 110, 
                                    type = 'lakes', 
                                    category = 'physical') %>% 
  sf::st_as_sf(lakes110, crs = 4269)

usa_states <- st_as_sf(maps::map("state", 
                                 fill=TRUE, 
                                 plot =FALSE),
                       crs = 4269)

setwd("D:/School/USGSdata/GitHub")
dat <- read.csv("Station_Details.csv")
dat <- dat[c(1:75,77:131),] # Remove `lake` site
dat$DA_km2 <- dat$DrainageArea_mi2 * 2.59

sf_all <- st_as_sf(dat, 
                   coords = c("long", "lat"),
                   crs = 4269)
                   
table(dat$Reservoir)
cols <- c("None" = "#d73027", "Above" = "#fec44f", "Below" = "#0571b0")

big_blank <- ggplot() +
  geom_sf(data = usa_states,
          mapping = aes(geometry = geom),          
          color = "black", 
          fill = "white") +
  geom_sf(data = lakes,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "white")  +
  geom_sf(data = sf_all,
          aes(fill = Reservoir),
          color = "black",
          shape = 21,
          size = 3) +
  scale_fill_manual(labels = c("Above (n = 14)",
                               "Below (n = 32)",
                               "None (n = 84)"),
                               values = cols) +
  coord_sf(ylim = c(23, 49),
           xlim = c(-132, -69),
           expand = TRUE) +
  labs(fill = "Reservoir Position") +
  annotation_scale(location = "br",
                   width_hint = 0.25, 
                   text_cex = 1) +
  annotation_north_arrow(location = "br", 
                         which_north = "grid", 
                         pad_x = unit(0.15, "in"),
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        legend.position = c(0.55,0),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.title.align = 0.5,
        legend.background = element_blank())
  

inset_alaska <- ggplot(data = alaska) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = sf_all,
          aes(fill = Reservoir),
          color = "black",
          shape = 21,
          size = 3) +
  scale_fill_manual(values = cols) +
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

ggdraw() +
  draw_plot(big_blank, x = -0.05, y = -0.05) +
  draw_plot(inset_alaska, x = -0.03, y = -0.03, width = 0.45, height = 0.45) +
  draw_plot(inset_world, x = 0.6, y = 0.63, width = 0.37, height = 0.37) +
  panel_border(remove = TRUE)
                   
