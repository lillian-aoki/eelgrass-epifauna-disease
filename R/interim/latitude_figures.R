## Figures for epifauna paper

# note, there are three datasets needed for visualization
# epiphyte_SEM_data_all_large.csv includes only MUR sites (1338 complete entries)
# epifauna_for_region_specific_models.csv includes all sites and years but only sites with epiphytes measured (2259 complete entries)
# epifauna_for_region_specific_models_no_epiphytes.csv includes all sites and years, without epiphytes (2370 entries)

library(tidyverse)
library(patchwork)
sem_dat <- read_csv("data/epiphyte_SEM_data_all_large.csv")
epi_dat <- read_csv("data/epifauna_for_region_specific_models.csv")
all_dat <- read_csv("data/epifauna_for_region_specific_models_no_epiphyte.csv")

region_order <- c("AK", "BC", "WA", "OR", "BB", "SD")

site_dat <- read_csv("data/epifauna_site_for_plotting.csv")
site_dat$fYear <- ordered(as.factor(site_dat$Year), levels = c("2019", "2020", "2021"))
site_dat$Region <- ordered(site_dat$Region, levels=region_order)

# epifauna distributions ####
amp_plot <- ggplot(site_dat, aes(x=Latitude, y=Ampithoid_large, color=fYear))+
  geom_point(size=1.5, alpha=0.75)+
  xlab("")+
  ylab("Log Ampithoid \nabundance")+
  scale_y_continuous(limits=c(-2, 1))+
  scale_x_continuous(breaks = c(30,35,40, 45, 50, 55, 60), trans = "reverse")+
  scale_color_manual(values=c('#CCBB44', '#66CCEE', '#AA3377'))+
  # scale_x_reverse(breaks = c(60, 55, 50, 45, 40, 35, 30))+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), 
        legend.position = "bottom", legend.box.spacing = unit(0, "pt"),legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "pt"))
amp_plot
lac_plot <- ggplot(site_dat, aes(x=Latitude, y=Lacuna_large, color=fYear))+geom_point(size=1.5, alpha=0.75)+
  xlab("")+
  ylab("Log Lacuna \nabundance")+
  scale_x_continuous(breaks = c(30,35,40, 45, 50, 55, 60), trans = "reverse")+
  scale_y_continuous(limits=c(-2, 1))+
  scale_color_manual(values=c('#CCBB44', '#66CCEE', '#AA3377'))+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), 
        legend.position = "bottom", legend.box.spacing = unit(0, "pt"),legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "pt"))

lac_plot

ido_plot <- ggplot(site_dat, aes(x=Latitude, y=Idoteid_large, color=fYear))+geom_point(size=1.5, alpha=0.75)+
  annotate(geom="text", x = c(33, 38.5, 43.5, 48, 52, 55),
           y=0.4, color = "grey30", size = 2.5,
           label = c("San\nDiego", "Bodega\nBay", "Oregon", "Washington","British\nColumbia", "Alaska"),
           angle = 45)+
  ylab("Log Idoteid \nabundance")+
  xlab("Latitude (ºN)")+
  scale_y_continuous(limits=c(-2, 1))+
  scale_x_continuous(breaks = c(30,35,40, 45, 50, 55, 60), trans = "reverse")+
  scale_color_manual(values=c('#CCBB44', '#66CCEE', '#AA3377'))+
  # scale_color_manual(values=c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377'))+
  # scale_color_manual(values=c('#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99'))+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), 
        legend.position = "bottom", legend.box.spacing = unit(0, "pt"), legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "pt"))

ido_plot

lac_plot / amp_plot / ido_plot / guide_area() + plot_layout(guides="collect", heights = c(1,1,1,0.2))
ggsave("output/taxa_latitude_2.jpg", width = 4, height =6 )

# flip axes of epifauna dist ####
amp_plot2 <- ggplot(site_dat, aes(y=Latitude, x=Ampithoid_large, color=fYear))+
  geom_point(size=1.5, alpha=0.75)+
  ylab("")+
  xlab("Log Ampithoid \nabundance")+
  scale_x_continuous(limits=c(-2, 1))+
  scale_y_continuous(breaks = c(30,35,40, 45, 50, 55, 60))+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), 
        legend.position = "bottom", legend.box.spacing = unit(0, "pt"), legend.title = element_blank(),legend.margin = margin(2,0,0,0,unit="pt"),
        plot.margin = unit(c(0,0,0,0), "pt"))
amp_plot2

lac_plot2 <- ggplot(site_dat, aes(y=Latitude, x=Lacuna_large, color=fYear))+geom_point(size=1.5, alpha=0.75)+
  ylab("Latitude (ºN)")+
  xlab("Log Lacuna \nabundance")+
  scale_x_continuous(limits=c(-2, 1))+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), 
        legend.position = "bottom", legend.box.spacing = unit(0, "pt"), legend.title = element_blank(),legend.margin = margin(2,0,0,0,unit="pt"),
        plot.margin = unit(c(0,0,0,0), "pt"))

lac_plot2

ido_plot2 <- ggplot(site_dat, aes(y=Latitude, x=Idoteid_large, color=fYear))+geom_point(size=1.5, alpha=0.75)+
  annotate(geom="text", y = c(33, 38.5, 43.5, 48, 52, 55),
           x=0.4, color = "grey30", size = 2.5,
           label = c("San\nDiego", "Bodega\nBay", "Oregon", "Washington","British\nColumbia", "Alaska"),
           angle = 20)+
  xlab("Log Idoteid \nabundance")+
  ylab("")+
  scale_x_continuous(limits=c(-2, 1))+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(), 
        legend.position = "bottom", legend.box.spacing = unit(0, "pt"), legend.title = element_blank(),legend.margin = margin(2,0,0,0,unit="pt"),
        plot.margin = unit(c(0,0,0,0), "pt"))

ido_plot2

(lac_plot2 + amp_plot2 + ido_plot2 ) / guide_area() + plot_layout(guides="collect", widths = c(1,1,1), heights = c(1,0.2))
ggsave("output/taxa_latitude_2_rev.jpg", width = 6, height =6 )
# this is okay but I don't really like it - it's harder to follow, swapping axes rarely works :(
####

ggplot(site_dat, aes(x=Latitude, y=Prevalence, color=Region, shape=fYear))+geom_point(size=1)
ggplot(site_dat, aes(x=Latitude, y=LesionAreaLog, color=Region, shape=fYear))+geom_point(size=1)

ggplot(site_dat, aes(x=Density, y=CanopyHeight, color=Region, shape=fYear, size=Prevalence))+geom_point(alpha=0.8)+
  scale_x_continuous(trans = "log10")+
  xlab("Shoot Density (shoots per m2)")+
  ylab("Canopy Height (m)")+
  scale_size(breaks=c(0.25, 0.50, 0.75),labels = c("25%", "50%", "75%"), name="Disease \n prevalence")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom", legend.direction = "vertical")

ggsave("output/seagrass_structure_prevalence.jpg", width = 4, height = 6)
# epiphytes
epi_dat$fYear <- as.factor(epi_dat$Year)
epi_dat$Region <- ordered(epi_dat$Region, levels=region_order)
names(epi_dat)
les_dat <- subset(epi_dat, LesionArea>0)
# this plot - shows as blades get larger, lesions get larger, no strong epiphyte pattern:
ggplot(les_dat, aes(x=BladeArea, y=LesionArea, color=Region, shape=fYear, size=EpiphytePerAreaMg))+geom_point()+
  scale_y_continuous(trans = "log10")+
  scale_x_continuous(trans = "log10")+
  scale_size(name="Epiphyte load \n(mg per cm2)")+
  xlab("Blade Area (cm2)")+
  ylab("Lesion area (cm2)")+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = "bottom", legend.direction = "vertical")
ggsave("output/lesion_blade_area.jpg", width=4, height = 6)
ggplot(epi_dat, aes(x=LesionArea, y=EpiphytePerAreaMg, color=Region, shape=fYear))+geom_point()+
  scale_y_continuous(trans = "log10")+
  scale_x_continuous(trans = "log10")

ggplot(epi_dat[epi_dat$EpiphytePerAreaMg>0,], 
       aes(x=BladeArea, y=log10(EpiphytePerAreaMg), shape=as.factor(Prevalence), color=Region))+geom_point()+
  facet_wrap(~as.factor(Prevalence))+
  scale_x_continuous(trans="log10")

les_epi <- subset(epi_dat, LesionArea>0 & EpiphytePerAreaMg>0)
ggplot(les_epi, aes(x=EpiphytePerAreaMg, y=LesionArea, color=Region))+geom_point()+
  scale_y_continuous(trans = "log10")+
  scale_x_continuous(trans = "log10")

ggplot(epi_dat, aes(x=Latitude, y=EpiphytePerAreaMg, color=Region, shape=fYear))+geom_point()

