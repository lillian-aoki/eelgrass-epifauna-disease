# Epifauna abundances for figure
library(tidyverse)

#transect level epifauna - need to collapse to site
epi <- read_csv("data/EGWD_transect_data_v20230307_big_epi_with_BB_transect.csv")
region_order <- c("AK", "BC", "WA", "OR", "BB", "SD")
names(epi)
meta <- read_csv("data/combined_site_metadata.csv")
meta19 <- subset(meta, Year=="2019")
loc19 <- select(meta19, c("Region","SiteCode", "TidalHeight", "Transect", "TransectBeginDecimalLatitude",
                          "TransectBeginDecimalLongitude", "TransectEndDecimalLatitude", "TransectEndDecimalLongitude"))
full_meta <- meta %>%
  rows_update(loc19, by = c("Region", "SiteCode", "TidalHeight", "Transect"))
meta_site <- full_meta %>%
  group_by(Year, Region, SiteCode) %>%
  summarise(Latitude=mean(as.numeric(TransectBeginDecimalLatitude)), Longitude=mean(TransectBeginDecimalLongitude))


epi$site <- str_extract(epi$site_unique_code, "\\.[A-F]\\.")
epi$site <- gsub("\\.", "", epi$site)
epi <- left_join(epi, meta_site, by=c("year"="Year", "region" = "Region", "site" = "SiteCode"))
epi$region <- ordered(epi$region, levels=region_order)
epi_summ <- epi %>%
  group_by(year, region, site, site_unique_code, Latitude, meadow=paste(region, site, sep="_")) %>%
  mutate(meadow=paste(region, site, sep="_"))%>%
  summarise(total_abun_large=mean(epifauna_per_g_transect_large),
            lac_abun_large=mean(lacuna_per_g_transect_large),
            amp_abun_large=mean(ampithoid_per_g_transect_large),
            ido_abun_large=mean(idoteid_per_g_transect_large),
            other_abun_large=total_abun_large-sum(lac_abun_large, amp_abun_large, ido_abun_large))
epi_summ_long <- pivot_longer(epi_summ, cols = c("other_abun_large", "lac_abun_large", "amp_abun_large", "ido_abun_large"),
                              names_to = "taxon") %>%
  arrange(Latitude)
meadow_order <- c("AK_A", "AK_B", "AK_C", "AK_D", "AK_E", "AK_F",
                  "BC_A", "BC_B", "BC_C", "BC_D", "BC_E",
                  "WA_A", "WA_B", "WA_C", "WA_D", "WA_E", 
                  "OR_A", "OR_B", "OR_C", "OR_D", "OR_E", 
                  "BB_A", "BB_B", "BB_C", "BB_D", "BB_E", "BB_F",
                  "SD_A", "SD_B", "SD_C", "SD_D", "SD_E")
epi_summ_long$meadow <- ordered(epi_summ_long$meadow, levels=meadow_order)
epi_summ$other_per <- epi_summ$other_abun_large/epi_summ$total_abun_large
epi_summ$gz_per <- 1-epi_summ$other_per
epi_summ$lac_per <- epi_summ$lac_abun_large/epi_summ$total_abun_large
epi_summ$ido_per <- epi_summ$ido_abun_large/epi_summ$total_abun_large
epi_summ$amp_per <- epi_summ$amp_abun_large/epi_summ$total_abun_large
summary(epi_summ$gz_per)
epi_summ %>% filter(gz_per>0.5) %>% filter(total_abun_large>1) %>% select(c(site_unique_code, lac_per, ido_per, amp_per))

ggplot(epi_summ_long, aes(x=meadow, y=value, fill=taxon))+
  geom_rect(aes(xmin="BB_A", xmax="BB_F", ymin=0, ymax=20), linetype="dashed", fill="grey90")+
  geom_col()+
  facet_grid(rows = "year")+
  ylab("Count of animals per g macrophytes")+
  scale_fill_viridis_d(labels=c("ampithiod \namphipods", "idoteid \nisopods", expression(atop(italic("Lacuna"), "snails")), "other \ntaxa"))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw(base_size=14)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle=45, vjust = 1, size=9, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(hjust = 0))

ggsave("output/rel_abun.jpg", width=8, height = 6)
