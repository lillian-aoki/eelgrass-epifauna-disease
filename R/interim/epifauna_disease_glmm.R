# Load packages:
library(tidyverse)
library(nlme)
library(lme4)
library(DHARMa)


EWD_2019_transect_data <- read.csv("EGWD_2019_data_transect_level_20210203.csv", header = TRUE)

# Data frame has NA's for the temp anomaly (CDiffMeanHeat) for several sites. Delete these so all models have same data: 
EWD_2019_transect_data_nona <- EWD_2019_transect_data %>% drop_na(CDiffMeanHeat) # requires tidyr/tidyverse
tran <- read.csv("data/all_survey_metrics_transect.csv")
tran$TidalHeightTransect <- paste(tran$TidalHeight, tran$Transect, sep="")
dat <- left_join(EWD_2019_transect_data_nona, tran, by=c("region"="Region", "site_code"="SiteCode", "transect" = "TidalHeightTransect"))

# create GLMM of prevalence - note need to use nlme() not glmmTMB() (piecewiseSEM doesn't support glmmTMB yet)
prev_cpta <- glmer(disease_prevalence_transect ~ CDiffMeanHeat + (1|site_unique_code), 
                   data = dat,
                   weights=CountBlades,
                   family=binomial)
summary(prev_cpta)
dat$Diseased <- dat$CountBlades*dat$PrevalenceMean
dat$Healthy <- dat$CountBlades*(1-dat$PrevalenceMean)
prev_cpta_matrix <- glmer(cbind(Diseased,Healthy) ~ CDiffMeanHeat + (1|site_unique_code), 
                          data = dat,
                          #weights=CountBlades,
                          family=binomial)
summary(prev_cpta_matrix)
AIC(prev_cpta, prev_cpta_matrix)


E.sim <- simulateResiduals(prev_cpta)
plot(E.sim)
plot(E.sim$scaledResiduals~dat$epifauna_per_g_plant_transect)
plot(E.sim$scaledResiduals~dat$CDiffMeanHeat)
hist(E.sim$scaledResiduals)
summary(prev_cpta)

fit_prev2.1s <- glmmTMB(PrevalenceMean~sBladeAreaMean+sDensityShootsMean+sEpiphytePerAreaMean+sCDiffMeanHeat+TidalHeight+
                          sDensityShootsMean:TidalHeight+
                          sEpiphytePerAreaMean:TidalHeight+sCDiffMeanHeat:TidalHeight+
                          (1|site_unique_code),
                        data=dat,
                        weights=CountBlades,
                        family=binomial)

head(dat)
