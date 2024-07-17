# grazing scars and prevalence glmms
# goal is to develop the model structure to use in SEMs
# last updated: 2022-07-18
# updated by: Lillian Aoki

library(nlme)
library(lme4)
library(dplyr)
library(readr)
library(DHARMa)

# read in data
egwd <- read_csv("data/full_seagrass_epifauna_for_SEM.csv")
egwd19 <- subset(egwd, Year=="2019")
egwd19 <- subset(egwd19, !is.na(GrazingScarsMeanTransect))

egwd1 <- subset(egwd, Year=="2019")
egwd1$TempAnomWarm_MarchMay <- sum(egwd1$TempAnomWarm_March, egwd1$TempAnomWarm_April, egwd1$TempAnomWarm_May)
egwd1 <- select(egwd1, c("PrevalenceMean", "GrazingScarsMeanTransect", "TempAnomWarm_June", "TempAnomWarm_March",
                         "TidalHeight", "Region", "Meadow", "TransectBeginDecimalLatitude", "TempAnomWarm_MarchMay",
                         "CountBlades", "StructureLog", "CountScars", "LesionAreaMean", "SeverityMean", "DensityShootsMean",
                         "BladeAreaMean", "EpiphytePerAreaMean", "CanopyHeight", "epifauna_per_g_log_transect"))
egwd1 <- na.omit(egwd1)
egwd1$TidalHeightBinary <- ifelse(egwd1$TidalHeight=="L", 0, 1)


# prevalence models ####
# looking to see if tidal height needs to be included 
prev_fit1 <- glmer(PrevalenceMean ~ GrazingScarsMeanTransect + TempAnomWarm_June + TidalHeight + StructureLog + (1|Region) + (1|Meadow),
                   family="binomial",
                   data=egwd19,
                   weights=CountBlades)
# fit is singular (too complex model structure, likely due to Region in random)
# try fit with Region as fixed effect
prev_fit2 <- glmer(PrevalenceMean ~ GrazingScarsMeanTransect + TempAnomWarm_June + TidalHeight + StructureLog + Region + (1|Meadow),
                   family="binomial",
                   data=egwd19,
                   weights=CountBlades)
# this is better, but Region is a categorical value, which may not play nicely with the piecewiseSEM
# use Latitude instead
prev_fit3 <- glmer(PrevalenceMean ~ GrazingScarsMeanTransect + TempAnomWarm_June + TidalHeight + StructureLog + TransectBeginDecimalLatitude + (1|Meadow),
                   family="binomial",
                   data=egwd19,
                   weights=CountBlades)
summary(prev_fit3)
E3.sim <- simulateResiduals(prev_fit3)
plot(E3.sim)
# residuals are look nice! ha
F3 <- fitted(prev_fit3)
plot(E3.sim$scaledResiduals~F3)
plot(E3.sim$scaledResiduals~egwd19$GrazingScarsMeanTransect)
plot(E3.sim$scaledResiduals~egwd19$TempAnomWarm_June)
plot(E3.sim$scaledResiduals~as.factor(egwd19$TidalHeight))
plot(E3.sim$scaledResiduals~as.factor(egwd19$Region))
plot(E3.sim$scaledResiduals~egwd19$StructureLog)
plot(E3.sim$scaledResiduals~egwd19$EpiphyteBladeAreaMean)
# no patterns in residuals
drop1(prev_fit3)
# dropping TransectLatitude marginally improves AIC
prev_fit4 <- glmer(PrevalenceMean ~ GrazingScarsMeanTransect + TempAnomWarm_June + TidalHeight + StructureLog + (1|Meadow),
                   family="binomial",
                   data=egwd19,
                   weights=CountBlades)
summary(prev_fit4)
drop1(prev_fit4)
# suggests we should keep tidal height
# for the SEM, will need to convert to 0 and 1 to estimate coefs

# grazing models ####
# again looking to see if tidal height needs to be included
# use same random structure as above (i.e. Meadow as random, use Latitude instead of Region as fixed)
gz_fit1 <- glmer(GrazingScarsMeanTransect ~ StructureLog + TempAnomWarm_June + TidalHeight + TransectBeginDecimalLatitude + (1|Meadow),
                 family="binomial",
                 data=egwd19,
                 weights=CountScars)
summary(gz_fit1)
drop1(gz_fit1)
# by AIC, tidal height stays. Oh well. 
Ez1.sim <- simulateResiduals(gz_fit1)
plot(Ez1.sim)
# residuals are fine
Fz1 <- fitted(gz_fit1)
plot(Ez1.sim$scaledResiduals~Fz1)
plot(Ez1.sim$scaledResiduals~egwd19$PrevalenceMean)
plot(Ez1.sim$scaledResiduals~egwd19$TempAnomWarm_June)
plot(Ez1.sim$scaledResiduals~as.factor(egwd19$TidalHeight))
plot(Ez1.sim$scaledResiduals~as.factor(egwd19$Region)) # not ideal but with latitude, trying to account for this
plot(Ez1.sim$scaledResiduals~egwd19$StructureLog)
plot(Ez1.sim$scaledResiduals~egwd19$EpiphyteBladeAreaMean)
plot(Ez1.sim$scaledResiduals~egwd19$CanopyHeight)

egwd1$sepifauna_per_g_log_transect <- scale(egwd1$epifauna_per_g_log_transect)
egwd1$sStructureLog <- scale(egwd1$StructureLog)
egwd1$sTempAnomWarm_June <- scale(egwd1$TempAnomWarm_June)
egwd1$sEpiphytePerAreaMean <- scale(egwd1$EpiphytePerAreaMean)
egwd1$sTransectBeginDecimalLatitude <- scale(egwd1$TransectBeginDecimalLatitude)
egwd1$EpiphytePerAreaMeanLog <- log10(egwd1$EpiphytePerAreaMean+1)
# just checking on importance of epifauna abundance - seems important to grazing!
gz_fit2 <- glmer(GrazingScarsMeanTransect ~ sepifauna_per_g_log_transect + 
                   sTempAnomWarm_June + EpiphytePerAreaMeanLog +
                   TidalHeight + sTransectBeginDecimalLatitude +
                   (1|Meadow),
                 family="binomial",
                 data=egwd1,
                 weights=CountScars)
summary(gz_fit2)
# so maybe do want the more complex model 

drop1(gz_fit2)
# by AIC, tidal height stays. Oh well. 
Ez2.sim <- simulateResiduals(gz_fit2)
plot(Ez2.sim)
# residuals are fine
Fz2 <- fitted(gz_fit2)
plot(Ez2.sim$scaledResiduals~Fz2)
plot(Ez2.sim$scaledResiduals~egwd1$PrevalenceMean)
plot(Ez2.sim$scaledResiduals~egwd1$TempAnomWarm_June)
plot(Ez2.sim$scaledResiduals~as.factor(egwd1$TidalHeight))
plot(Ez2.sim$scaledResiduals~as.factor(egwd1$Region)) # not ideal but with latitude, trying to account for this
plot(Ez2.sim$scaledResiduals~egwd1$StructureLog)
plot(Ez2.sim$scaledResiduals~egwd1$sEpiphytePerAreaMean)
plot(Ez2.sim$scaledResiduals~egwd1$CanopyHeight)

# seagrass model ####
# can this be linear?

sg_fit1 <- lme(fixed = StructureLog ~ TempAnomWarm_MarchMay + TidalHeightBinary + TransectBeginDecimalLatitude,
               random = c(~1|Meadow),
               data=egwd1)
summary(sg_fit1)
drop1(sg_fit1)

# lesion area ####
# best to use gamma distribution but needs to be >0. hurdle approach to take all transects with disease present
egwd2 <- subset(egwd1, PrevalenceMean>0)
egwd2$sDensityShootsMean <- scale(egwd2$DensityShootsMean)
egwd2$sTempAnomWarm_June <- scale(egwd2$TempAnomWarm_June)
egwd2$sGrazingScarsMeanTransect <- scale(egwd2$GrazingScarsMeanTransect)
egwd2$sStructureLog <- scale(egwd2$StructureLog)
egwd2$sBladeAreaMean <- scale(egwd2$BladeAreaMean)
egwd2$sEpiphytePerAreaMeanMg <- scale(egwd2$EpiphytePerAreaMeanMg)
egwd2$sLatitude <- scale(egwd2$TransectBeginDecimalLatitude)
egwd2$LogEpiphyte <- log10(egwd2$EpiphytePerAreaMeanMg + 1)
egwd2$sLogEpiphyte <- scale(egwd2$LogEpiphyte)
egwd2$sCanopyHeight <- scale(egwd2$CanopyHeight)
egwd2$LogLesionArea <- log10(egwd2$LesionAreaMean)
egwd2$LogSeverityMean <- log10(egwd2$SeverityMean)

les1<- glmer(LesionAreaMean ~ TempAnomWarm_June + TidalHeight + StructureLog + TransectBeginDecimalLatitude + (1|Meadow),
             family = Gamma(link = "log"),
            data=egwd2)
summary(les1)
E1 <- resid(les1)
F1 <- fitted(les1)
plot(E1~F1)
E1.sim <- simulateResiduals(les1)
plot(E1.sim)
plot(E1.sim$scaledResiduals~fitted(les1))
# this doesn't look good!

les2<- glmer(LesionAreaMean ~ TempAnomWarm_June + GrazingScarsMeanTransect + TidalHeight + StructureLog + TransectBeginDecimalLatitude + (1|Meadow),
             family = Gamma(link = "log"),
             data=egwd2)
summary(les2)
E2 <- resid(les2)
F2 <- fitted(les2)
plot(E2~F2)
E2.sim <- simulateResiduals(les2)
plot(E2.sim)
plot(E2.sim$scaledResiduals~fitted(les2))
# this still doesn't look good!

les2<- glmer(LesionAreaMean ~ TempAnomWarm_June + GrazingScarsMeanTransect + TidalHeight + TransectBeginDecimalLatitude + (1|Meadow),
             family = Gamma(link = "log"),
             data=egwd2)
summary(les2)
E2 <- resid(les2)
F2 <- fitted(les2)
plot(E2~F2)
E2.sim <- simulateResiduals(les2)
plot(E2.sim)
plot(E2.sim$scaledResiduals~fitted(les2))
# this still doesn't look good!

les3 <-  lmer(LesionAreaMean ~ TempAnomWarm_June + GrazingScarsMeanTransect + TidalHeight + StructureLog + TransectBeginDecimalLatitude + (1|Meadow),
               data=egwd2)
E3.sim <- simulateResiduals(les3)
plot(E3.sim)
# really bad!!

les <- lmer(LogLesionArea ~ sTempAnomWarm_June + sTempAnomWarm_March + sGrazingScarsMeanTransect + TidalHeight + sStructureLog + sLatitude + (1|Meadow),
     # family = Gamma(link = "inverse"),
     # weights = BladeAreaMean,
     offset= BladeAReaMean, data=egwd2)
AIC(les)
E1 <- resid(les)
F1 <- fitted(les)
plot(E1~F1)
E1.sim <- simulateResiduals(les)
plot(E1.sim)
plot(E1.sim$scaledResiduals~fitted(les))
# also the best I can do here 

# Severity ####
sev1<- lmer(LogSeverityMean ~ sTempAnomWarm_June + sTempAnomWarm_March + sGrazingScarsMeanTransect + TidalHeight + sStructureLog + sLatitude + (1|Meadow),
             # family = Gamma(link = "inverse"),
            # weights = BladeAreaMean,
             data=egwd2)
summary(sev1)
AIC(sev1)
E1 <- resid(sev1)
F1 <- fitted(sev1)
plot(E1~F1)
E1.sim <- simulateResiduals(sev1)
plot(E1.sim)
plot(E1.sim$scaledResiduals~fitted(sev1))
# this is the best I can do. It doesn't violate dispersion, normality, though it is not perfect. 


summary(egwd2$LogSeverityMean)

sev1<- glmer(SeverityMean ~ sTempAnomWarm_June + sGrazingScarsMeanTransect + TidalHeight + sBladeAreaMean + 
               sDensityShootsMean + sCanopyHeight + sLatitude + (1|Meadow),
            family = Gamma(link = "log"),
            # weights = BladeAreaMean,
            data=egwd2)
summary(sev1)
E1 <- resid(sev1)
F1 <- fitted(sev1)
plot(E1~F1)
E1.sim <- simulateResiduals(sev1)
plot(E1.sim)
plot(E1.sim$scaledResiduals~fitted(sev1))
plot(E1.sim$scaledResiduals~as.factor(egwd2$Region))
plot(E1.sim$scaledResiduals~egwd2$sDensityShootsMean)
plot(E1.sim$scaledResiduals~egwd2$sDensityShootsMean)
plot(E1.sim$scaledResiduals~egwd2$StructureLog)

summary(egwd2$LogSeverityMean)

sev1<- glmer(LogSeverityMean ~ sTempAnomWarm_June + sGrazingScarsMeanTransect + TidalHeight + sBladeAreaMean + TransectBeginDecimalLatitude + (1|Meadow),
             family = Gamma(link = "inverse"),
             # weights = BladeAreaMean,
             data=egwd2)
summary(sev1)
E1 <- resid(sev1)
F1 <- fitted(sev1)
plot(E1~F1)
E1.sim <- simulateResiduals(sev1)
plot(E1.sim)
plot(E1.sim$scaledResiduals~fitted(sev1))
summary(egwd2$LogSeverityMean)
egwd2$LogDensityShootsMean <- log10(egwd2$DensityShootsMean)
sev1 <- lme(LogSeverityMean ~ sTempAnomWarm_June + sGrazingScarsMeanTransect + TidalHeight + sStructureLog + sLatitude,
           random = c(~1|Meadow),
           # weights = varComb(varExp(form=~BladeAreaMean), varIdent(form=~1|Region)),
           # weights = varExp(form=~BladeAreaMean),
           # weights = varIdent(form=~1|Region),
           # weights = varFixed(~sTempAnomWarm_June),
           data=egwd2)
AIC(sev1)
summary(sev1)
E1 <- resid(sev1)
F1 <- fitted(sev1)
plot(E1~F1)
hist(E1)
qqline(E1)
qqnorm(E1)
plot(E1~egwd2$BladeAreaMean)
plot(E1~egwd2$CanopyHeight)
plot(E1~as.factor(egwd2$Region))
plot(E1~as.factor(egwd2$TidalHeight))
plot(E1~egwd2$EpiphytePerAreaMean)
plot(E1~log10(egwd2$DensityShootsMean))
plot(E1~(egwd2$TransectBeginDecimalLatitude))

# E1.sim <- simulateResiduals(sev1)
# plot(E1.sim)
# plot(E1.sim$scaledResiduals~fitted(sev1))

# epifauna ####
ef <- lme(sepifauna_per_g_log_transect ~ sTempAnomWarm_March + sStructureLog + 
      TidalHeightBinary + sTransectBeginDecimalLatitude,
    random = c(~1|Meadow),
    data=egwd1)
plot(ef$residuals~ef$fitted)
hist(ef$residuals)
egwd1$sEpiphytePerAreaMeanLog <- scale(egwd1$EpiphytePerAreaMeanLog)

eph <- lme(sEpiphytePerAreaMeanLog ~ sStructureLog + sTempAnomWarm_June + sTransectBeginDecimalLatitude,
    random = c(~1|Meadow, ~1|TidalHeight),
    data=egwd3)
plot(eph$residuals~eph$fitted)
hist(eph$residuals)
plot(E~as.factor(egwd3$Region))
plot(E~egwd3$TempAnomWarm_March)
length(eph$residuals)
E <- resid(eph)

# there's just one epiphyte that is massively larger than any others at False Bay - try getting rid of it?
egwd3 <- egwd1[-which(egwd1$EpiphytePerAreaMean==max(egwd1$EpiphytePerAreaMean)),]
eph2 <- gls(sEpiphytePerAreaMeanLog ~ sStructureLog + TempAnomWarm_March + BladeAreaMean + 
              TidalHeightBinary + sTransectBeginDecimalLatitude,
            weights=varIdent(form=~1|Meadow*TidalHeight), 
            # weights = varComb(varExp(form=~BladeAreaMean), varIdent(form=~1|TidalHeight)),
            # weights = varExp(form=~BladeAreaMean),
            # weights = varIdent(form=~1|Region),
            # weights = varFixed(~CanopyHeight),
            data=egwd3)
hist(eph2$residuals)
plot(eph2$residuals~eph2$fitted)
plot(eph2$residuals~as.factor(egwd3$Region))
qqnorm(eph2$residuals)
qqline(eph2$residuals)
shapiro.test(eph2$residuals)
# this is way closer but still not great
summary(eph2)
egwd1$sBladeAreaMean <- scale(egwd1$BladeAreaMean)
egwd4 <- subset(egwd1, EpiphytePerAreaMean>0)
egwd4$sEpiphytePerAreaMean <- scale(egwd4$EpiphytePerAreaMean)
egwd1$EpiphytePerAreaMeanLog <- log10(egwd1$EpiphytePerAreaMean+1)
egwd1$sBladeAreaMean <- scale(egwd1$BladeAreaMean)
eph1 <- lmer(EpiphytePerAreaMeanLog ~ sStructureLog + TidalHeightBinary + sTransectBeginDecimalLatitude + sTempAnomWarm_March + 
                (1|Meadow), offset = sBladeAreaMean,
            # family = gaussian(link = "identity"),
           data=egwd1)
hist(resid(eph1))
qqnorm(resid(eph1))
qqline(resid(eph1))
shapiro.test(resid(eph1))
# E.sim <- simulateResiduals(eph1)
# plot(E.sim)
summary(eph1)
# this is pretty reasonable from the residuals 
# use linear model with an offset for epiphytes - but doesn't work if the response variable is scaled
# so, need to unscale in other models?
egwd1$EpiphytePerAreaMeanSqrt <- sqrt(egwd1$EpiphytePerAreaMean)
egwd1$sEpiphytePerAreaMeanSqrt <- scale(egwd1$EpiphytePerAreaMeanSqrt)
egwd1$sTempAnomWarm_March <- scale(egwd1$TempAnomWarm_March)
eph3 <- lme(sEpiphytePerAreaMeanSqrt ~ sStructureLog + sTempAnomWarm_June + sTransectBeginDecimalLatitude,
    random = c(~1|Meadow, ~1|TidalHeight),
    data=egwd1)
hist(eph3$residuals)
plot(eph3$residuals~eph3$fitted)
qqnorm(eph3$residuals)
qqline(eph3$residuals)
shapiro.test(eph3$residuals)
ggplot(egwd4, aes(x=EpiphytePerAreaMeanLog, y=PrevalenceMean, color=Meadow))+geom_point()+facet_wrap(~Region)
lim <- select(egwd1, c("EpiphytePerAreaMean", "EpiphytePerAreaMeanLog", "StructureLog", "TempAnomWarm_June", 
                       "TempAnomWarm_March", "BladeAreaMean"))
GGally::ggpairs(lim)
