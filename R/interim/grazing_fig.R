# Grazing Scar figure??

library(tidyverse)
library(patchwork)
library(ggmosaic)
sem_dat <- read_csv("data/epiphyte_SEM_data_all_large.csv")

# sem_dat$GrazingScarsNum <- ifelse(sem_dat$GrazingScars=="Y", 1, sem_dat$GrazingScars)
# sem_dat$GrazingScarsNum <- ifelse(sem_dat$GrazingScars=="N", 0, sem_dat$GrazingScars)

# sem_dat$GrazingScarsFac <- ifelse(sem_dat$GrazingScarsNum>0, "Y", "N")
# sem_dat$GrazingScarsNum <- ifelse(sem_dat$GrazingScarsFac=="N", 0, 1)

reg <- sem_dat %>%
  group_by(Year, Region, Meadow, Latitude, Epifauna_large, Lacuna_large, Ampithoid_large) %>%
  summarise(Grazing=mean(GrazingScars, na.rm = T), Prevalence=mean(Prevalence, na.rm = T), LesionArea=mean(LesionArea, na.rm = T))
reg$Region <- ordered(reg$Region, levels=c("AK", "BC", "WA", "OR", "BB", "SD"))

# ggplot(reg, aes(x=Grazing, y=Prevalence, color = Region, shape = as.factor(Year)))+geom_point()+
#   theme_bw(base_size=14)+
#   scale_x_continuous(labels = scales::percent)+
#   scale_y_continuous(labels = scales::percent)+
#   xlab("Proportion of leaves \nwith grazing scars")+
#   ylab("Proportion of leaves \ninfected with wasting disease")+
#   theme(panel.grid = element_blank(),
#         legend.position = "bottom")
# 
# ggplot(reg, aes(x=Latitude, y=Grazing, color=as.factor(Year)))+geom_point()+
#   scale_y_continuous(labels = scales::percent)+
#   theme_bw(base_size=14)+
#   theme(panel.grid = element_blank(),
#         legend.position = "bottom")
# 
# ggplot(reg, aes(x=Grazing, y=LesionArea, color = Region, shape = as.factor(Year)))+geom_point()#+facet_wrap(~Year)
# ggplot(sem_dat, aes(x=GrazingScars))+geom_histogram()+facet_wrap(~Year)
# ggplot(reg, aes(x=Epifauna_large, y=Grazing, color=Region, shape=as.factor(Year)))+geom_point()
# ggplot(reg, aes(x=Lacuna_large, y=Grazing, color=Region, shape=as.factor(Year)))+geom_point()
# ggplot(reg, aes(x=Ampithoid_large, y=Grazing, color=Region, shape=as.factor(Year)))+geom_point()
# 
# ggplot(data = flights) +
#   geom_mosaic(aes(x=product(do_you_recline), fill = do_you_recline, 
#                   conds = product(rude_to_recline))) +
#   labs(title='f(do_you_recline | rude_to_recline)')
sem_dat$fGrazing <- as.factor(sem_dat$GrazingScars)
sem_dat$fGrazing <- recode_factor(sem_dat$fGrazing, "0" = "Absent", "1" = "Present")
sem_dat$fYear <- as.factor(sem_dat$Year)
sem_dat$fPrevalence <- as.factor(sem_dat$Prevalence)
sem_dat$fPrevalence <- recode_factor(sem_dat$fPrevalence, "0" = "Healthy", "1" = "Diseased")
sem_dat <- drop_na(sem_dat, c("Prevalence", "GrazingScars"))
ggplot(sem_dat)+
  geom_mosaic(aes(x=product(fPrevalence), fill= fPrevalence, conds = product(fGrazing)))

mosaic <- ggplot(sem_dat)+
  geom_mosaic(aes(x=product(fPrevalence, fGrazing), fill= fPrevalence))+
  geom_mosaic_text(aes(x = product(fPrevalence, fGrazing), label = after_stat(.wt)), as.label=TRUE, size = 4)+
  # scale_color_manual(values=c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377'))+
  # facet_wrap(~fYear)+
  theme_bw(base_size=14)+
  # scale_fill_viridis_d(labels=c("Healthy", "Diseased"),begin = 0.6, end = 0)+
  scale_fill_manual(labels=c("Healthy", "Diseased"), values = c('#228833', '#CCBB44'))+
  # scale_x_continuous(labels=c("Absent", "Present"))+
  xlab("Grazing scars")+
  ylab("Wasting disease infection")+
  # labs(tag = "(a)   ")+
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        # plot.margin = unit(0, "pt"),
        legend.position = "bottom",
        legend.text = element_text(size=10))
# mosaic + guide_area() + plot_layout(guides= "collect", widths = c(1,0.5))
mosaic / guide_area() + plot_layout(guides= "collect", heights = c(1, 0.1))

ggsave("output/grazing_scar_mosaic_colors.jpg", width = 4, height = 3.5)

