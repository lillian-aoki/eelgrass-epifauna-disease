# Grazing Scar figure??

library(tidyverse)
library(patchwork)
sem_dat <- read_csv("data/epiphyte_SEM_data_all_large.csv")

# sem_dat$GrazingScarsNum <- ifelse(sem_dat$GrazingScars=="Y", 1, sem_dat$GrazingScars)
# sem_dat$GrazingScarsNum <- ifelse(sem_dat$GrazingScars=="N", 0, sem_dat$GrazingScars)

# sem_dat$GrazingScarsFac <- ifelse(sem_dat$GrazingScarsNum>0, "Y", "N")
# sem_dat$GrazingScarsNum <- ifelse(sem_dat$GrazingScarsFac=="N", 0, 1)

reg <- sem_dat %>%
  group_by(Year, Region, Meadow, Epifauna_large, Lacuna_large, Ampithoid_large) %>%
  summarise(Grazing=mean(GrazingScars, na.rm = T), Prevalence=mean(Prevalence, na.rm = T), LesionArea=mean(LesionArea, na.rm = T))

ggplot(reg, aes(x=Grazing, y=Prevalence, color = Region, shape = as.factor(Year)))+geom_point()#+facet_wrap(~Year)
ggplot(reg, aes(x=Grazing, y=LesionArea, color = Region, shape = as.factor(Year)))+geom_point()#+facet_wrap(~Year)
ggplot(sem_dat, aes(x=GrazingScars))+geom_histogram()+facet_wrap(~Year)
ggplot(reg, aes(x=Epifauna_large, y=Grazing, color=Region, shape=as.factor(Year)))+geom_point()
ggplot(reg, aes(x=Lacuna_large, y=Grazing, color=Region, shape=as.factor(Year)))+geom_point()
ggplot(reg, aes(x=Ampithoid_large, y=Grazing, color=Region, shape=as.factor(Year)))+geom_point()
