# Partial predictor plots
library(lme4)
library(dplyr)
library(readr)
library(DHARMa)
library(ggplot2)
library(GGally)
library(optimx)
library(performance)
library(piecewiseSEM)
library(effects)
library(ggeffects)
library(patchwork)
library(sjPlot)
# data ###
dis <- read_csv("data/epiphyte_SEM_data_all_large.csv")
# updating the SEM to compare the effects of using large vs all animals
dis$BladeAreaLog <- log10(dis$BladeArea)
dis$EpiphyteLog <- log10(dis$EpiphytePerAreamgcm2+0.01)
# use the full data set without subsetting because the SEM no longer includes epiphytes or grazing
dis1 <- select(dis, c(Epifauna = Epifauna_all, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, 
                      DensityLog, YearBinary, Year, Meadow, Region,BladeAreaLog, TidalHeightBinary, 
                      Prevalence, LesionArea, EpiphytePerAreamgcm2, EpiphyteLog,
                      Lacuna = Lacuna_all, 
                      Ampithoid = Ampithoid_all, Idoteid = Idoteid_all, Richness = Richness_all))

dis1_large <- select(dis, c(Epifauna = Epifauna_large, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, 
                            DensityLog, YearBinary, Year, Meadow, Region,BladeAreaLog, TidalHeightBinary, 
                            Prevalence, LesionArea, EpiphyteLog, Lacuna = Lacuna_large, 
                            Ampithoid = Ampithoid_large, Idoteid = Idoteid_large, Richness = Richness_large))
dis_large <- na.omit(dis1_large)
dis_large$Meadow_Year <- paste(dis_large$Meadow, dis_large$Year, sep = "_")
site_large <- distinct(dis_large, Meadow_Year, .keep_all = T)
site_large <- select(site_large, -c(Prevalence, LesionArea, EpiphyteLog, TidalHeightBinary, BladeAreaLog))

les_large <- subset(dis_large, LesionArea>0)
les_large$LesionAreaLog <- log10(les_large$LesionArea)
les_large <- na.omit(les_large)

site_les <- distinct(les_large, Meadow_Year, .keep_all = T)

# prev #####
prev_epi_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Epifauna + CanopyHeight + DensityLog + 
                      TempAnomWarm_June + MonthlyMeanTemp_June + 
                      TidalHeightBinary + YearBinary +
                      (1|Region) +(1|Meadow), 
                    data=dis_large,
                    family="binomial")
epi1 <- predictorEffect("Epifauna", prev_epi_1, partial.residuals=T)
epi2 <- as.data.frame(epi1)


epi_prev <- ggplot(epi2)+geom_ribbon(aes(x=Epifauna, y=fit, ymax=upper, ymin=lower),fill="lightgrey")+
  geom_line(aes(x=Epifauna, y=fit))+
  geom_point(data=dis_large, aes(x=Epifauna, y=0.01), shape="|", size=3)+
  scale_y_continuous(limits=c(-0, 1), expand=c(0,0))+
  xlab("Log epifauna \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank())
epi_prev
ggsave(filename = "output/sem_partial_epifauna_prev.jpg", width=4.5, height=4)
# default is to plot the effect on the "rescale" type, which shows the probability labels on a logit scale
# to me this is harder to interpret, but it shows the linearity of the relationship. 
plot(predictorEffect("Epifauna", prev_epi_1, partial.residuals=F),
     main="New Title", 
     axes=list(y=list(type="rescale", lab="Odds ratio (increasing odds of disease)"),
               x=list(lab="Log Epifauna abundance per g macrophytes")))
# alternatively, plot the log odds of disease risk - negative values indicate more likely to be healthy
# positive values indicate more likely to be diseased
plot(predictorEffect("Epifauna", prev_epi_1, partial.residuals=F),
     main="New Title", 
     axes=list(y=list(type="link", lab="Log odds of disease risk"),
               x=list(lab="Log Epifauna abundance per g macrophytes")))
# can futher transform with exp() to get the odds ratio
# plot the odds ratio - centered at 1, values <1 indicate more likely to be healthy than diseased
# values >1 indicate more likely to be diseased than healthy
plot(predictorEffect("Epifauna", prev_epi_1, partial.residuals=F),
     main="New Title", 
     axes=list(y=list(type="link", lab="Odds ratio (increasing odds of disease)", transform=exp),
               x=list(lab="Log Epifauna abundance per g macrophytes")))

# OR, plot on the response scale ("untransformed") - y-axis = probability of being diseased
# thsi is non-linear but easier to interpret (to me)
plot(predictorEffect("Epifauna", prev_epi_1, partial.residuals=F),
     main="", 
     axes=list(y=list(type="response", lab="Infection risk (probability leaf is diseased)"),
               x=list(Epifauna=list(lab="Log Epifauna (abundance per g macrophytes)"))))
# looks better without the residuals
# keep this one
png(filename = "output/sem_partial_epifauna_prev.png",)
plot(predictorEffect("Epifauna", prev_epi_1, partial.residuals=F),
     main="", 
     cex.lab=1.5,
     axes=list(y=list(type="response", lab="Infection risk (probability leaf is diseased)", cex=1.5),
               x=list(Epifauna=list(lab="Log Epifauna (abundance per g macrophytes)"), cex=1.5),
               cex.lab=1.5))
dev.off()
# now, this is the same as doing it via ggplot. And I have better control over ggplot (though not sure how to get the ticks)
# maybe worth getting plot to work with the effects package - seems likely I will use in the future. 

one <- ggpredict(prev_epi_1, "Epifauna")
plot(one,residuals = T, jitter=c(0.1,0.05))
pone <- plot(one,residuals = T, jitter=0.1)+
  labs(title="")+
  xlab("Log epifauna \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=12)+
  theme(panel.grid = element_blank())
pone
plot_model(prev_epi_1)

# okay use ggplot approach - it's a lot nicer than trying to get base r plot to work
prev_lac_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Lacuna + CanopyHeight + DensityLog + 
                      TempAnomWarm_June + MonthlyMeanTemp_June + 
                      TidalHeightBinary + YearBinary +
                      (1|Region) +(1|Meadow), 
                    data=dis_large,
                    family="binomial")
lac1 <- predictorEffect("Lacuna", prev_lac_1, partial.residuals=T)
lac2 <- as.data.frame(lac1)
two <- ggpredict(prev_lac_1, "Lacuna")
ptwo <- plot(two, residuals=T, jitter = 0.1)+
  labs(title="")+
  xlab("Log lacuna \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank())
ptwo
plot(two, residuals=T, jitter = 0)
lac_prev <- ggplot(lac2)+geom_ribbon(aes(x=Lacuna, y=fit, ymax=upper, ymin=lower),fill="lightgrey")+
  geom_line(aes(x=Lacuna, y=fit))+
  geom_point(data=dis_large, aes(x=Lacuna, y=0.01), shape="|", size=3)+
  scale_y_continuous(limits=c(-0, 1), expand=c(0,0))+
  xlab("Log lacuna \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank())
lac_prev

prev_amp_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Ampithoid + CanopyHeight + DensityLog + 
                      TempAnomWarm_June + MonthlyMeanTemp_June + 
                      TidalHeightBinary + YearBinary +
                      (1|Region) +(1|Meadow), 
                    data=dis_large,
                    family="binomial")
amp1 <- predictorEffect("Ampithoid", prev_amp_1, partial.residuals=T)
amp2 <- as.data.frame(amp1)

three <- ggpredict(prev_amp_1, "Ampithoid")
pthree <- plot(three, residuals=T, jitter = c(0.1, 0.1),ci = F,line.size = 0)+
  labs(title="")+
  xlab("Log ampithoid \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank())
pthree
amp_prev <- ggplot(amp2)+geom_ribbon(aes(x=Ampithoid, y=fit, ymax=upper, ymin=lower),fill="lightgrey")+
  geom_line(aes(x=Ampithoid, y=fit))+
  geom_point(data=dis_large, aes(x=Ampithoid, y=0.01), shape="|", size=3)+
  geom_text(aes(x=-1, y=0.85), label="N.S.")+
  scale_y_continuous(limits=c(-0, 1), expand=c(0,0))+
  xlab("Log ampithoid \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank())
amp_prev

prev_ido_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + Idoteid + CanopyHeight + DensityLog + 
                      TempAnomWarm_June + MonthlyMeanTemp_June + 
                      TidalHeightBinary + YearBinary +
                      (1|Region) +(1|Meadow), 
                    data=dis_large,
                    family="binomial")
ido1 <- predictorEffect("Idoteid", prev_ido_1, partial.residuals=T)
ido2 <- as.data.frame(ido1)

four <- ggpredict(prev_ido_1, "Idoteid")
pfour <- plot(four, residuals=T, jitter = c(0.05, 0.05))+
  labs(title="")+
  xlab("Log idoteid \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank())
pfour

ido_prev <- ggplot(ido2)+geom_ribbon(aes(x=Idoteid, y=fit, ymax=upper, ymin=lower),fill="lightgrey")+
  geom_line(aes(x=Idoteid, y=fit))+
  geom_point(data=dis_large, aes(x=Idoteid, y=0.01), shape="|", size=3)+
  geom_text(aes(x=-1.25, y=0.85), label="N.S.")+
    scale_y_continuous(limits=c(-0, 1), expand=c(0,0))+
  xlab("Log idoteid \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank())
ido_prev

epi_prev / lac_prev / amp_prev / ido_prev
ggsave("output/sem_partials_prev.jpg", width = 4, height=8)

pone / ptwo / pthree / pfour
ggsave("output/sem_partials_prev_2.jpg", width=4, height=8)
# lesion area ####
les_epi_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Epifauna + CanopyHeight + DensityLog + 
                    TempAnomWarm_June + MonthlyMeanTemp_June + 
                    TidalHeightBinary + YearBinary +
                    (1|Region) +(1|Meadow), 
                  data=les_large)
# plot(predictorEffect("Epifauna", les_epi_1, partial.residuals=TRUE))


les_lac_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Lacuna + CanopyHeight + DensityLog + 
                    TempAnomWarm_June + MonthlyMeanTemp_June + 
                    TidalHeightBinary + YearBinary +
                    (1|Region) +(1|Meadow), 
                  data=les_large)
# plot(predictorEffect("Lacuna", les_lac_1, partial.residuals=TRUE))


les_amp_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Ampithoid + CanopyHeight + DensityLog + 
                    TempAnomWarm_June + MonthlyMeanTemp_June + 
                    TidalHeightBinary + YearBinary +
                    (1|Region) +(1|Meadow), 
                  data=les_large)
# plot(predictorEffect("Ampithoid", les_amp_1, partial.residuals=TRUE))


les_ido_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Idoteid + CanopyHeight + DensityLog + 
                    TempAnomWarm_June + MonthlyMeanTemp_June + 
                    TidalHeightBinary + YearBinary +
                    (1|Region) +(1|Meadow), 
                  data=les_large)
# plot(predictorEffect("Idoteid", les_ido_1, partial.residuals=TRUE))


les_rich_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + Richness + CanopyHeight + DensityLog + 
                     TempAnomWarm_June + MonthlyMeanTemp_June + 
                     TidalHeightBinary + YearBinary +
                     (1|Region) +(1|Meadow), 
                   data=les_large)
# plot(predictorEffect("Richness", les_rich_1, partial.residuals=T, type="rescale"))

epi3 <- predictorEffect("Epifauna", les_epi_1, partial.residuals=T)
epi4 <- as.data.frame(epi3)

lac3 <- predictorEffect("Lacuna", les_lac_1, partial.residuals=T)
lac4 <- as.data.frame(lac3)

amp3 <- predictorEffect("Ampithoid", les_amp_1, partial.residuals=T)
amp4 <- as.data.frame(amp3)

ido3 <- predictorEffect("Idoteid", les_ido_1, partial.residuals=T)
ido4 <- as.data.frame(ido3)

epi_les <- ggplot(epi4)+geom_ribbon(aes(x=Epifauna, y=fit, ymax=upper, ymin=lower),fill="lightgrey")+
  geom_line(aes(x=Epifauna, y=fit))+
  geom_point(data=les_large, aes(x=Epifauna, y=LesionAreaLog))+
  # scale_y_continuous(trans = "logit")+
  xlab("Log epifauna (abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size=12)+
  theme(panel.grid = element_blank())
epi_les
# here we could use gg predict instead
epi5 <- ggpredict(les_epi_1, terms= "Epifauna [all]", type="fixed")
epi_les <- plot(epi5, residuals = T)+
  labs(title="")+
  xlab("Log epifauna (abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw()+
  theme(panel.grid = element_blank())

epi5 <- ggpredict(les_epi_1, terms= "Epifauna [all]", type="fixed")
pfive <- plot(epi5, residuals = T,limit.range = T, jitter = 0.05)+
  labs(title="")+
  xlab("Log epifauna \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank())
pfive
six <- ggpredict(les_lac_1, terms="Lacuna [all]")
psix <- plot(six, residuals = T, jitter = 0.05)+
  labs(title="")+
  xlab("Log lacuna \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank())
psix

seven <- ggpredict(les_amp_1, terms="Ampithoid [all]")
pseven <- plot(seven, residuals = T, jitter = 0.05)+ # use ci=F and linesize=0 to remove from plot if desired
  geom_text(aes(x=-1, y=0.85), label="N.S.")+
  labs(title="")+
  xlab("Log ampithoid \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank())
pseven

eight <- ggpredict(les_ido_1, terms="Idoteid [all]")
peight <- plot(eight, residuals=T, jitter=0.05)+
  labs(title="")+
  xlab("Log idoteid \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank())
peight

pfive / psix / pseven / peight
ggsave("output/sem_partials_les.jpg", width=4, height=10)
