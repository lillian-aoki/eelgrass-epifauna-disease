library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
# library(rnaturalearthhires)
library(ggspatial)
library(patchwork)
library(ggrepel)

sites <- read_csv("data/epifauna_site_for_plotting.csv")
sites <- na.omit(sites)
sites <- subset(sites, Year=="2019")
sites_sf <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

mapFull <- ggplot(data = world) + 
  geom_sf(fill="grey90")+
  # geom_sf(data=sites_sf, size=2, aes(color=MonthlyMeanTemp_June), alpha=0.5, nudge_y = 0.1) + 
  geom_sf(data=sites_sf, size=2, aes(color=MonthlyMeanTemp_June), alpha=0.7) + 
  # geom
  coord_sf(xlim = c(max(sites$Longitude), min(sites$Longitude-4)), 
           ylim = c(max(sites$Latitude), min(sites$Latitude)),
           default_crs = sf::st_crs(4326))+
  scale_color_continuous(type="viridis", name="Mean Daily Temperature\n(June 2019)",
                         breaks=c(10, 12, 14, 16, 18, 20))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw(base_size = 12)+
  theme(legend.position = "bottom", plot.margin = unit(c(1,2,1,2), "pt"), legend.title.align = 0.5,
        legend.title = element_text(size=11))

mapFull + 
  # geom_sf_label(data=regionsf, aes(label=label))
  annotate(geom = "label", color = "grey30", size = 3.5,
                   x = c(-136, -133, -128, -127, -127, -120.5), 
                   y = c(55, 51, 48, 44, 38, 32.5),
                  fontface = "italic",
                   label = c("Alaska", "British Colulmbia", "Washington", "Oregon", "Bodega Bay", "San Diego"))

ggsave("output/map_study_sites.jpg", width = 4, height = 6)

# 
# region_dat <- data.frame(x = c(-125, -124, -130, -127, -128, -125), 
#                          y = c(33, 39, 43, 48, 52, 56),
#                          label = c("San\nDiego", "Bodega\nBay", "Oregon", "Washington","British\nColumbia", "Alaska"))
# regionsf <- st_as_sf(region_dat, coords = c("x", "y"), crs = 4326, agr = "constant")


ak <- ne_countries(scale = "large", returnclass = "sf")
# ak <- ne_coastline(scale = "large", returnclass = "sf")

ak_coords <- data.frame(xlim=c(max(sites$Longitude[sites$Region=="AK"])+0.5, min(sites$Longitude[sites$Region=="AK"]))-0.5,
                        ylim=c(max(sites$Latitude[sites$Region=="AK"])+1, min(sites$Latitude[sites$Region=="AK"]))-1)
ak_coords <- data.frame(xlim=c(max(sites$Longitude[sites$Region=="AK"]), min(sites$Longitude[sites$Region=="AK"])),
                        ylim=c(max(sites$Latitude[sites$Region=="AK"]), min(sites$Latitude[sites$Region=="AK"])))
mapAK <- ggplot(data = ak) + 
  geom_sf(fill="lightgrey")+
  geom_sf(data=sites_sf, size=2, aes(color=MonthlyMeanTemp_June), alpha=0.5, nudge_y = 0.1) + 
  # geom
  # coord_sf(xlim=c(max(sites$Longitude[sites$Region=="AK"]), min(sites$Longitude[sites$Region=="AK"])),
  #          ylim=c(max(sites$Latitude[sites$Region=="AK"]), min(sites$Latitude[sites$Region=="AK"])))+
  coord_sf(xlim=ak_coords$xlim, ylim=ak_coords$ylim)+
  scale_color_continuous(type="viridis")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

mapAK
