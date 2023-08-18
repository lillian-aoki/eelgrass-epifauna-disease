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
library(partR2)
# data ###
region_order <- c("AK", "BC", "WA", "OR", "BB", "SD")
dis <- read_csv("data/epiphyte_SEM_data_all_large.csv")
dis$Region <- ordered(dis$Region, levels=region_order)
# updating the SEM to compare the effects of using large vs all animals
dis$BladeAreaLog <- log10(dis$BladeArea)
dis$EpiphyteLog <- log10(dis$EpiphytePerAreamgcm2+0.01)
# # use the full data set without subsetting because the SEM no longer includes epiphytes or grazing
# dis1 <- select(dis, c(Epifauna = Epifauna_all, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, 
#                       DensityLog, YearBinary, Year, Meadow, Region,BladeAreaLog, TidalHeightBinary, 
#                       Prevalence, LesionArea, EpiphytePerAreamgcm2, EpiphyteLog,
#                       Lacuna = Lacuna_all, 
#                       Ampithoid = Ampithoid_all, Idoteid = Idoteid_all, Richness = Richness_all))
# 
# dis1_large <- select(dis, c(Epifauna = Epifauna_large, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, 
#                             DensityLog, YearBinary, Year, Meadow, Region,BladeAreaLog, TidalHeightBinary, 
#                             Prevalence, LesionArea, EpiphyteLog, Lacuna = Lacuna_large, 
#                             Ampithoid = Ampithoid_large, Idoteid = Idoteid_large, Richness = Richness_large))
# dis_large <- na.omit(dis1_large)
# dis_large$Meadow_Year <- paste(dis_large$Meadow, dis_large$Year, sep = "_")
# site_large <- distinct(dis_large, Meadow_Year, .keep_all = T)
# site_large <- select(site_large, -c(Prevalence, LesionArea, EpiphyteLog, TidalHeightBinary, BladeAreaLog))
# updated 2023-08-08 to include grazing scars in the models
dis1_large <- select(dis, c(Epifauna = Epifauna_large, TempAnomWarm_June, MonthlyMeanTemp_June, CanopyHeight, 
                            DensityLog, YearBinary, Year, Meadow, Region, Transect, Blade, BladeAreaLog, TidalHeightBinary, GrazingScars,
                            Prevalence, LesionArea, EpiphyteLog, Lacuna = Lacuna_large, 
                            Ampithoid = Ampithoid_large, Idoteid = Idoteid_large, Richness = Richness_large))
dis1_large$GrazingScars <- as.factor(dis1_large$GrazingScars)
dis_large <- na.omit(dis1_large)
dis_large$Meadow_Year <- paste(dis_large$Meadow, dis_large$Year, sep = "_")
site_large <- distinct(dis_large, Meadow_Year, .keep_all = T)
site_large <- select(site_large, -c(Prevalence, LesionArea, EpiphyteLog, TidalHeightBinary, BladeAreaLog))
les_large <- subset(dis_large, LesionArea>0)
les_large$LesionAreaLog <- log10(les_large$LesionArea)
les_large <- na.omit(les_large)

site_les <- distinct(les_large, Meadow_Year, .keep_all = T)

# prev #####
prev_epi_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + GrazingScars + Epifauna + CanopyHeight + DensityLog + 
                      TempAnomWarm_June + MonthlyMeanTemp_June + 
                      TidalHeightBinary + YearBinary +
                      (1|Region) +(1|Meadow), 
                    data=dis_large,
                    family="binomial")
epi1 <- predictorEffect("Epifauna", prev_epi_1, partial.residuals=T)
epi2 <- as.data.frame(epi1)

# calculating partial R2 - but this isn't super intuitive and confusing for non-significant models so leave out
# epi_part <- partR2(prev_epi_1, partvars = c("Epifauna"))
# epi_part_r2 <- signif(epi_part$R2[2,]$estimate, 2)
# epi_label <- paste("'partial r'^2==~", epi_part_r2)
# epi_beta <- signif(epi_part$Ests) # I think including Betas will be confusing (b/c it's logistic regression)


epi_prev <- ggplot(epi2)+geom_ribbon(aes(x=Epifauna, y=fit, ymax=upper, ymin=lower),fill="lightgrey")+
  geom_line(aes(x=Epifauna, y=fit))+
  geom_point(data=dis_large, aes(x=Epifauna, y=0.01, color=Region), shape="|", size=3)+
  # annotate(geom = "text", x = -0.8, y = 0.8, parse = TRUE,label = epi_label )+
  scale_y_continuous(limits=c(-0, 1), expand=c(0,0))+
  xlab("Log epifauna \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))
epi_prev
ggsave(filename = "output/gz_sem_partial_epifauna_prev.jpg", width=4.5, height=4)
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
png(filename = "output/gz_sem_partial_epifauna_prev.png",)
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
prev_lac_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + GrazingScars + Lacuna + CanopyHeight + DensityLog + 
                      TempAnomWarm_June + MonthlyMeanTemp_June + 
                      TidalHeightBinary + YearBinary +
                      (1|Region) +(1|Meadow), 
                    data=dis_large,
                    family="binomial")
lac1 <- predictorEffect("Lacuna", prev_lac_1, partial.residuals=T)
lac2 <- as.data.frame(lac1)
# lac_part <- partR2(prev_lac_1, partvars = c("Lacuna"))
# lac_part_r2 <- signif(lac_part$R2[2,]$estimate, 2)
# lac_label <- paste("'partial r'^2==~", lac_part_r2)

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
  geom_point(data=dis_large, aes(x=Lacuna, y=0.01, color=Region), shape="|", size=3)+
  # annotate(geom = "text", x = -0.8, y = 0.8, parse = TRUE,label = lac_label )+
  scale_y_continuous(limits=c(-0, 1), expand=c(0,0))+
  xlab("Log lacuna \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))
lac_prev

prev_amp_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + GrazingScars + Ampithoid + CanopyHeight + DensityLog + 
                      TempAnomWarm_June + MonthlyMeanTemp_June + 
                      TidalHeightBinary + YearBinary +
                      (1|Region) +(1|Meadow), 
                    data=dis_large,
                    family="binomial")
amp1 <- predictorEffect("Ampithoid", prev_amp_1, partial.residuals=T)
amp2 <- as.data.frame(amp1)
# amp_part <- partR2(prev_amp_1, partvars = c("GrazingScars"))
# amp_part_r2 <- signif(amp_part$R2[2,]$estimate, 2)
# amp_label <- paste("'partial r'^2==~", amp_part_r2)

three <- ggpredict(prev_amp_1, "Ampithoid")
pthree <- plot(three, residuals=T, jitter = c(0.1, 0.1),ci = F,line.size = 0)+
  labs(title="")+
  xlab("Log ampithoid \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank())
pthree
amp_prev <- ggplot(amp2)+geom_ribbon(aes(x=Ampithoid, y=fit, ymax=upper, ymin=lower),fill="grey95")+
  geom_line(aes(x=Ampithoid, y=fit), linetype="dashed", color="grey80")+
  geom_point(data=dis_large, aes(x=Ampithoid, y=0.01, color=Region), shape="|", size=3)+
  geom_text(aes(x=-1, y=0.85), label="N.S.")+
  scale_y_continuous(limits=c(-0, 1), expand=c(0,0))+
  xlab("Log ampithoid \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))
amp_prev

prev_ido_1 <- glmer(Prevalence ~ BladeAreaLog + EpiphyteLog + GrazingScars + Idoteid + CanopyHeight + DensityLog + 
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
  geom_point(data=dis_large, aes(x=Idoteid, y=0.01, color=Region), shape="|", size=3)+
  # geom_text(aes(x=-1.25, y=0.85), label="N.S.")+ # new model with GrazingScars, Ido is significant
    scale_y_continuous(limits=c(-0, 1), expand=c(0,0))+
  xlab("Log idoteid \n(abundance per g macrophytes)")+
  ylab("Infection risk \n(probability leaf is diseased)")+
  theme_bw(base_size=11)+
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))
ido_prev

prev_lab <- ggplot(data.frame(l=epi_prev$labels$y, x=1, y=1))+geom_text(aes(x,y, label=l), angle = 90)+
  theme_void()+
  coord_cartesian(clip = "off")

epi_prev$labels$y <- lac_prev$labels$y <- amp_prev$labels$y <- ido_prev$labels$y <- ""

prev_lab + (epi_prev / lac_prev / amp_prev / ido_prev) + plot_layout(widths = c(1, 5), guides = "collect")
ggsave("output/gz_sem_partials_prev.jpg", width = 4, height=8)

pone / ptwo / pthree / pfour
ggsave("output/sem_partials_prev_2.jpg", width=4, height=8)
# lesion area ####
les_epi_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + GrazingScars + Epifauna + CanopyHeight + DensityLog + 
                    TempAnomWarm_June + MonthlyMeanTemp_June + 
                    TidalHeightBinary + YearBinary +
                    (1|Region) +(1|Meadow), 
                  data=les_large)
# plot(predictorEffect("Epifauna", les_epi_1, partial.residuals=TRUE))


les_lac_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + GrazingScars + Lacuna + CanopyHeight + DensityLog + 
                    TempAnomWarm_June + MonthlyMeanTemp_June + 
                    TidalHeightBinary + YearBinary +
                    (1|Region) +(1|Meadow), 
                  data=les_large)
# plot(predictorEffect("Lacuna", les_lac_1, partial.residuals=TRUE))


les_amp_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + GrazingScars + Ampithoid + CanopyHeight + DensityLog + 
                    TempAnomWarm_June + MonthlyMeanTemp_June + 
                    TidalHeightBinary + YearBinary +
                    (1|Region) +(1|Meadow), 
                  data=les_large)
# plot(predictorEffect("Ampithoid", les_amp_1, partial.residuals=TRUE))


les_ido_1 <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + GrazingScars + Idoteid + CanopyHeight + DensityLog + 
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
  geom_jitter(data=les_large, aes(x=Epifauna, y=LesionAreaLog, color = Region), width=0.05, height = 0.05, alpha = 0.3)+
  # scale_y_continuous(trans = "logit")+
  xlab("Log epifauna \n(abundance per g macrophytes)")+
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
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))

epi5 <- ggpredict(les_epi_1, terms= "Epifauna [all]", type="fixed")
pfive <- plot(epi5, residuals = T,limit.range = T, jitter = 0.05)+
  labs(title="")+
  xlab("Log epifauna \n(abundance per g macrophytes)")+
  ylab(expression(paste("Log lesion area (",mm^2,")")))+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))
pfive

six <- ggpredict(les_lac_1, terms="Lacuna [all]")
psix <- plot(six, residuals = T, jitter = 0.05)+
  labs(title="")+
  xlab("Log lacuna \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))
psix

ples_lac <- ggplot(lac4)+geom_ribbon(aes(x=Lacuna, y=fit, ymax=upper, ymin=lower),fill="lightgrey")+
  geom_line(aes(x=Lacuna, y=fit), color="black")+
  geom_jitter(data=les_large, aes(x=Lacuna, y=LesionAreaLog, color=Region), width=0.05, height = 0.05, alpha = 0.3)+
  # scale_y_continuous(trans = "logit")+
  # geom_text(aes(x=-1, y=0.85), label="N.S.")+
  xlab("Log lacuna \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size=12)+
  theme(panel.grid = element_blank())
ples_lac

seven <- ggpredict(les_amp_1, terms="Ampithoid [all]")
pseven <- plot(seven, residuals = T, jitter = 0.05, colors = "grey80")+ # use ci=F and linesize=0 to remove from plot if desired
  geom_text(aes(x=-1, y=0.85), label="N.S.")+
  # scale_linetype_manual(values=c(17))+
  labs(title="")+
  xlab("Log ampithoid \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))
pseven

ples_amp <- ggplot(amp4)+geom_ribbon(aes(x=Ampithoid, y=fit, ymax=upper, ymin=lower),fill="grey95")+
  geom_line(aes(x=Ampithoid, y=fit), color="grey80", linetype="dashed")+
  geom_jitter(data=les_large, aes(x=Ampithoid, y=LesionAreaLog, color=Region), width=0.05, height = 0.05, alpha = 0.3)+
  # scale_y_continuous(trans = "logit")+
  geom_text(aes(x=-1, y=0.85), label="N.S.")+
  xlab("Log ampithoid \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size=12)+
  theme(panel.grid = element_blank())
ples_amp

ples_ido <- ggplot(ido4)+geom_ribbon(aes(x=Idoteid, y=fit, ymax=upper, ymin=lower),fill="lightgrey")+
  geom_line(aes(x=Idoteid, y=fit), color="black")+
  geom_jitter(data=les_large, aes(x=Idoteid, y=LesionAreaLog, color=Region), width=0.05, height = 0.05, alpha = 0.3)+
  # scale_y_continuous(trans = "logit")+
  # geom_text(aes(x=-1, y=0.85), label="N.S.")+
  xlab("Log idoteid \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size=12)+
  theme(panel.grid = element_blank())
ples_ido

ggeight <- ggpredict(les_ido_1, terms="Idoteid [all]")
peight <- plot(eight, residuals=T, jitter=0.05)+
  labs(title="")+
  xlab("Log idoteid \n(abundance per g macrophytes)")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(2,2,2,2), "pt"))
peight

mm2 <- paste0("'Log lesion area ('*mm^2*')'")
les_lab <- ggplot(data.frame(l=mm2, x=1, y=1))+geom_text(aes(x,y, label=l), parse = T, angle = 90)+
  theme_void()+
  coord_cartesian(clip = "off")

# pfive$labels$y <- psix$labels$y <- pseven$labels$y <- peight$labels$y <- ples_amp$labels$y <-  ""


# les_lab + (pfive / psix / ples_amp / peight) + plot_layout(widths = c(1, 5))

epi_les$labels$y <- ples_lac$labels$y <- ples_amp$labels$y <- ples_ido$labels$y <- ""

les_lab + (epi_les / ples_lac / ples_amp / ples_ido) + plot_layout(widths = c(1, 5), guides = "collect")
ggsave("output/gz_sem_partials_les.jpg", width=4, height=10)

# partial of grazing scars on lesion area
gzdat <- predictorEffect("GrazingScars", les_epi_1, partial.residuals=T)
gzdatdf <- as.data.frame(gzdat)
# a little tricky because Grazing Scars is binary and numeric for the piecewise SEM, therefore take means at 0 and 1
gzdatadf <- subset(gzdatdf, GrazingScars==0 | GrazingScars==1)

ggplot(gzdatadf)+ 
  geom_jitter(data=les_large, aes(x=as.factor(GrazingScars), y=LesionAreaLog, color=Region), width=0.06, height = 0.04, alpha = 0.25)+
  geom_errorbar(aes(x=as.factor(GrazingScars), ymax=upper, ymin=lower), width=0.3)+
  geom_point(aes(x=as.factor(GrazingScars), y=fit), size=2)+
  xlab("Grazing scars")+
  ylab("Log lesion area (mm2)")+
  theme_bw(base_size = 14)+
  theme(panel.grid = element_blank())
  # geom_ribbon(aes(x=GrazingScars, y=fit, ymax=upper, ymin=lower), fill="lightgrey")+
  # geom_line(aes(x=GrazingScars, y = fit), color="black")+
ggsave("output/grazing_scar_lesion.jpg", width=4, height=3)  
  gz_pred <- ggpredict(les_epi_1, terms="GrazingScars [all]")
  gz_plot <- plot(gz_pred, residuals=T, jitter=0.05)#+
    labs(title="")+
    xlab("Log idoteid \n(abundance per g macrophytes)")+
    ylab("Log lesion area (mm2)")+
    theme_bw(base_size = 12)+
    theme(panel.grid = element_blank(),
          plot.margin = unit(c(2,2,2,2), "pt"))
    gz_plot
plot_model(les_epi_1)    

les_factor <- lmer(LesionAreaLog ~ BladeAreaLog + EpiphyteLog + as.factor(GrazingScars) + Epifauna + CanopyHeight + DensityLog + 
                    TempAnomWarm_June + MonthlyMeanTemp_June + 
                    TidalHeightBinary + YearBinary +
                    (1|Region) +(1|Meadow), 
                  data=les_large)
plot_model(les_factor)
gzf <- ggpredict(les_factor, terms= "GrazingScars")
gzf_plot <- ggplot(gzf, residuals=T, jitter=0.05)
gzf_plot
