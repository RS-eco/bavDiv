#' ---
#' title: "Scale comparison of GLMM of species richness in Bavaria"
#' author: "RS-eco"
#' ---

#+setup, include = FALSE
knitr::opts_chunk$set(collapse = T, tidy=T, comment = NA, warning=F, message=F, eval=T, error=F, 
                      fig.path = "../figures/", dpi=100, fig.width=8, fig.height=6)
rm(list=ls()); invisible(gc())

#+ data_setup, results="hide"
# Load packages
library(data.table); library(dtplyr); library(dplyr, warn.conflicts = FALSE)
library(magrittr); library(tidyr); library(forcats); library(sf); library(lubridate)
library(ggplot2); library(patchwork); library(ggpmisc); library(scico)

# Load outline of bavaria
library(bavDC)
data(bavaria)
bavaria <- sf::st_transform(bavaria, 31468)

# Specify colour scheme
bluered <- rev(scico(n=255, palette="roma"))

# Set filedir
filedir <- sub("/R", "", getwd())

# Load data
load(paste0(filedir, "/extdata/ask_sub.rda"))

#+ ng_data
# Load naturraum data
data("ng_ssymank")

# Load TK25 grid
data("tk25_grid", package="bavDC")
tk25_r <- raster::rasterFromXYZ(tk25_grid)
raster::projection(tk25_r) <- sp::CRS("+init=epsg:31468")

# Get Naturräume
ng_ssymank <- sf::st_transform(ng_ssymank, 31468)
# Specify raster resolution (1km)
ng_ssymank <- fasterize::fasterize(ng_ssymank, raster=tk25_r, by="Haupt_Nr")
ng_ssymank <- ng_ssymank %>% raster::rasterToPoints() %>% as.data.frame() %>%
  tidyr::pivot_longer(cols=-c(x,y), names_to="Haupt_Nr", values_to="value") %>% 
  tidyr::drop_na() %>% select(-value)

#+ water_data
library(bavDC)
data("diva_water_lines_deu")
data("diva_water_areas_deu")
diva_water_lines_deu <- sf::st_transform(diva_water_lines_deu, 31468)
diva_water_areas_deu <- sf::st_transform(diva_water_areas_deu, 31468)

# Specify raster resolution (1km)
diva_water_lines_bav <- raster::rasterize(diva_water_lines_deu, raster::disaggregate(tk25_r, fact=10), field=1)
diva_water_areas_bav <- fasterize::fasterize(diva_water_areas_deu, raster=raster::disaggregate(tk25_r, fact=10))
water <- raster::aggregate(raster::mosaic(diva_water_lines_bav, diva_water_areas_bav, fun=mean), fact=10, fun=sum, na.rm=T)
names(water) <- "water_cover" 
water <- raster::mask(water, bavaria)
water_dat <- water %>% raster::rasterToPoints() %>% as.data.frame()

#+ pa_data
# Load PA data
data(pa_bav)
cover <- fasterize::fasterize(pa_bav, raster=raster::disaggregate(tk25_r, fact=10), by="IUCN_CAT")
cover_all <- fasterize::fasterize(pa_bav, raster=raster::disaggregate(tk25_r, fact=10))
cover <- raster::aggregate(raster::stack(cover, cover_all), fact=10, fun=sum)
names(cover)[6] <- "Total" 

pa_bav_tk25 <- as.data.frame(raster::rasterToPoints(cover))
pa_bav_tk25 <- pa_bav_tk25 %>% tidyr::gather(iucn_cat, perc_cov, -c(x,y))
pa_bav_tk25$iucn_cat <- factor(pa_bav_tk25$iucn_cat, 
                                 levels=c("I", "II", "III", "IV", "V", "VI",
                                          "Not.Assigned", "Not.Applicable", "Not.Reported", "Total"), 
                                 labels=c("I", "II", "III-IV", "III-IV", "V", "VI", "Not designated", 
                                          "Not designated", "Not designated", "Total"))

# Remove data with mon = 0
ask_sub %<>% filter(mon != 0)

# Add KARTE variable
ask_sub$KARTE <- as.numeric(substr(ask_sub$KARTE_QUAD, start=1, stop=4))

#+ glmm
dat_mod_tk25 <- ask_sub %>% 
  group_by(jahr, mon, KARTE, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% 
  inner_join(pa_bav_tk25 %>% filter(iucn_cat == "Total") %>% 
               left_join(tk25_grid) %>% left_join(ng_ssymank)) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(.$perc_cov, 0)) %>% 
  #mutate(dist = replace_na(.$dist, 0)) %>% 
  #mutate(dist = cut(.$dist, breaks=c(0,10,15,20,25,30), include.lowest=T, na.rm=T)) %>%
  #mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>%  
  dplyr::select(KARTE, jahr, mon, 
                Haupt_Nr, class_order, perc_cov, sum_art, sum_n) %>% drop_na() %>%
  #mutate(mon = factor(mon, labels=month.abb)) %>%
  mutate(KARTE = factor(KARTE)) %>%
  mutate(sum_n = sqrt(sum_n)) %>%
  mutate(jahrIndex = as.numeric(factor(jahr))); invisible(gc())
save(dat_mod_tk25, file=paste0(filedir, "/extdata/dat_mod_tk25.rda"), compress="xz")

mod_all <- ask_sub %>% group_by(jahr, mon, KARTE) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% 
  inner_join(pa_bav_tk25 %>% filter(iucn_cat == "Total") %>% 
               left_join(tk25_grid) %>% left_join(ng_ssymank)) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(.$perc_cov, 0)) %>% 
  #mutate(dist = replace_na(.$dist, 0)) %>% 
  #mutate(dist = cut(.$dist, breaks=c(0,10,15,20,25,30), include.lowest=T, na.rm=T)) %>%
  #mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>%  
  select(KARTE, jahr, mon, Haupt_Nr, perc_cov, sum_art, sum_n) %>% drop_na() %>%
  #mutate(mon = factor(mon, labels=month.abb)) %>%
  mutate(KARTE = factor(KARTE)) %>%
  mutate(sum_n = sqrt(sum_n)) %>%
  mutate(jahrIndex = as.numeric(factor(jahr))) %>%
  mutate(perc_cov = scale(perc_cov)#, dist = scale(dist)
         ); invisible(gc())

dat_aves <- dat_mod_tk25 %>% filter(class_order == "Aves") %>%
  mutate(perc_cov = scale(perc_cov)#, dist = scale(dist)
         )
dat_lepi <- dat_mod_tk25  %>% filter(class_order == "Lepidoptera") %>%
  mutate(perc_cov = scale(perc_cov)#, dist = scale(dist)
         )
dat_odo <- dat_mod_tk25 %>% filter(class_order == "Odonata") %>%
  mutate(perc_cov = scale(perc_cov)#, dist = scale(dist)
         )
dat_ortho <- dat_mod_tk25 %>% filter(class_order == "Orthoptera") %>%
  mutate(perc_cov = scale(perc_cov)#, dist = scale(dist)
         )
invisible(gc())

# generalized mixed effects model
library(glmmTMB)
if(!file.exists(paste0(filedir, "/data/lmer_all_tk25.rda"))){
  lmer_all <- glmmTMB(sum_art ~ jahrIndex + mon + 
                        perc_cov + jahrIndex:perc_cov + 
                      (1 | KARTE) + (1 | Haupt_Nr),
                      ziformula=~0, family=poisson, 
                      data=mod_all, REML = FALSE, weights=sum_n)
  save(lmer_all, file=paste0(filedir, "/data/lmer_all_tk25.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_all_tk25.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_aves_tk25.rda"))){
  lmer_aves <- glmmTMB(sum_art ~ jahrIndex + mon + 
                         perc_cov + jahrIndex:perc_cov + 
                         (1 | KARTE) + (1 | Haupt_Nr),
                       ziformula=~0, data=dat_aves, family=poisson, 
                       REML = FALSE, weights=sum_n)
  save(lmer_aves, file=paste0(filedir, "/data/lmer_aves_tk25.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_aves_tk25.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_lepi_tk25.rda"))){
  lmer_lepi <- glmmTMB(sum_art ~ jahrIndex + mon + 
                         perc_cov + jahrIndex:perc_cov + 
                         (1 | KARTE) + (1 | Haupt_Nr),
                       ziformula=~0, data=dat_lepi, family=poisson, 
                       REML = FALSE, weights=sum_n)
  save(lmer_lepi, file=paste0(filedir, "/data/lmer_lepi_tk25.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_lepi_tk25.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_odo_tk25.rda"))){
  lmer_odo <- glmmTMB(sum_art ~ jahrIndex + mon + 
                        perc_cov + jahrIndex:perc_cov + 
                        (1 | KARTE) + (1 | Haupt_Nr),
                      ziformula=~0, data=dat_odo, family=poisson, 
                      REML = FALSE, weights=sum_n)
  save(lmer_odo, file=paste0(filedir, "/data/lmer_odo_tk25.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_odo_tk25.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_ortho_tk25.rda"))){
  lmer_ortho <- glmmTMB(sum_art ~ jahrIndex + mon + 
                          perc_cov + jahrIndex:perc_cov + 
                          (1 | KARTE) + (1 | Haupt_Nr),
                        ziformula=~0, data=dat_ortho, family=poisson, 
                        REML = FALSE, weights=sum_n)
  save(lmer_ortho, file=paste0(filedir, "/data/lmer_ortho_tk25.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_ortho_tk25.rda"))
}

#library(performance)
#check_collinearity(lmer_ortho)
#summary(lmer_ortho)
#car::Anova(lmer_ortho, type="III")
#print(VarCorr(lmer_ortho), comp=("Variance"))
#library(DHARMa)
#plot(simulateResiduals(lmer_ortho))
#testResiduals(simulateResiduals(lmer_ortho))

#+ SupportingFigure15
library(broom.mixed)
effects_all <- broom.mixed::tidy(lmer_all) %>% as.data.frame() %>% mutate(class_order = "Total")
effects_aves <- broom.mixed::tidy(lmer_aves) %>% as.data.frame() %>% mutate(class_order = "Aves")
effects_lepi <- broom.mixed::tidy(lmer_lepi) %>% as.data.frame() %>% mutate(class_order = "Lepidoptera")
effects_ortho <- broom.mixed::tidy(lmer_ortho) %>% as.data.frame() %>% mutate(class_order = "Orthoptera")
effects_odo <- broom.mixed::tidy(lmer_odo) %>% as.data.frame() %>% mutate(class_order = "Odonata")
effects_full <- full_join(effects_all, effects_lepi) %>% full_join(., effects_aves) %>% 
  full_join(., effects_ortho) %>% full_join(., effects_odo)

# define taxon colour
taxon_cols <- c("Aves"= "red", "Lepidoptera" = "#00c6a2", 
                "Orthoptera" = "#ffd706", "Odonata" = "#00486d",
                "Total" = "lightgrey")

effects_full$class_order <- factor(effects_full$class_order, levels = c("Aves", "Lepidoptera", "Orthoptera", "Odonata", "Total"))
 unique(effects_full$term)

# Effects plot
effects_full %>% filter(term != "(Intercept)", term != "sd__(Intercept)",
                        term != "sd__jahrIndex", term != "cor__(Intercept).jahrIndex") %>%
  mutate(term = factor(term, levels=rev(c("jahrIndex", "mon", 
                                          "perc_cov", "jahrIndex:perc_cov")),
                       labels=rev(c("Year", "Month", 
                                    "% Cover", "Year:% Cover")))) %>%
  #mutate(term = factor(term, levels=rev(c("jahrIndex", "mon", "perc_cov(0,25]", "perc_cov(25,50]", "perc_cov(50,75]", "perc_cov(75,100]", 
  #                                        "jahrIndex:perc_cov(0,25]",  "jahrIndex:perc_cov(25,50]",  "jahrIndex:perc_cov(50,75]", 
  #                                        "jahrIndex:perc_cov(75,100]")), 
  #                     labels=rev(c("Year", "Month", "0 - 25 % Cover",  "25 - 50 % Cover", "50 - 75 % Cover", "75 - 100 % Cover",
  #                                  "Year:0 - 25 % Cover", "Year:25 - 50 % Cover", "Year:50 - 75 % Cover", "Year:75 - 100 % Cover")))) %>%
  mutate(p.value = cut(p.value, breaks=c(0, 0.001, 0.01, 0.05, 1), labels=c("***", "**", "*", "n.s."))) %>% 
  mutate(p.value = replace_na(p.value, "***")) %>%
  ggplot(aes(x=term, y=estimate, ymin=(estimate-std.error), ymax=(estimate+std.error), 
             colour = class_order, shape = p.value)) +
  geom_pointrange(position=position_dodge(width=0.75), size=1) + 
  geom_hline(yintercept=0, lty=2) + coord_flip() + 
  scale_y_continuous(expand=expansion(mult = c(.025, .025))) + 
  scale_colour_manual(name="Class/Order", values = taxon_cols) + 
  guides(color = guide_legend(reverse=TRUE)) + 
  labs(y="Effect size", x="") + theme_bw() + 
  theme(legend.position=c(0.82,0.18), legend.box = "horizontal", 
        legend.background = element_blank())

#' **Supporting Figure 15.** Effect sizes of generalized linear mixed effects model 
#' of species richness on a rough grid. <!-- Significances to labels??? -->

library(emmeans)
#' ### emmeans: 
#' calculate estimated marginal means for each explanatory variable in relationship with other explanatory variables
#' 
#' Explore possible reference grids:
#' 
#' 1. uses factors and mean of continuous variables
#ref_grid(lmer_all)
#' 2. uses factors and max/min values for continuous variables
#ref_grid(lmer_lepi_5, cov.reduce = perc_cov)

# emmeans for rangesize ----
#' With a look at the distribution I decided to compare the response for a scaled rangesize of - 1.5 (representing small European range sizes), 0 (representing the mean rangesize) and 1.5 (representing large European ranges).
#' Using these values, the model can predict occupancy rates for each taxon within the limits of the ranges the species really have.

#+ SupportingFigure16
emm_all <- emmeans(lmer_all, ~ perc_cov*jahrIndex,
                   #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                   cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                   cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Total") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))
emm_aves <- emmeans(lmer_aves, ~ perc_cov*jahrIndex,
                    #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                    cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                    cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Aves") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))
emm_lepi <- emmeans(lmer_lepi, ~ perc_cov*jahrIndex,
                    #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                    cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                    cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Lepidoptera") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))
emm_ortho <- emmeans(lmer_ortho, ~ perc_cov*jahrIndex,
                     #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                     cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                     cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Orthoptera") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))
emm_odo <- emmeans(lmer_odo, ~ perc_cov*jahrIndex,
                   #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                   cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                   cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Odonata") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))
emm <- bind_rows(list(emm_all, emm_aves,
                      emm_lepi, emm_ortho, emm_odo))
emm %>% mutate(jahrIndex = jahrIndex+1984) %>%
  ggplot(aes(x=jahrIndex, y=rate)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=perc_cov), alpha=0.5) +
  geom_line(aes(lty=perc_cov), size=1) +
  facet_wrap(.~taxa, scales="free_y") + theme_bw() + 
  labs(x="Year", y="SR", fill="% Cover", linetype = "% Cover") + 
  scale_x_continuous(limits=c(1985, 2019), expand=c(0.01,0.01), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  theme(strip.background=element_blank(), 
        strip.text=element_text(size=12, face="bold"),
        legend.position="bottom",
        legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size)

#' **Supporting Figure 16.** Modelled change in species richness over time for min, 
#' median and max percentage cover protection on a rough grid (TK25).

# What about emtrends???
#emt <- emtrends(lmer_all, ~ jahrIndex | perc_cov, var="jahrIndex", at=list(perc_cov = c(10, 20, 30)))
#emt %>% as.data.frame()
#summary(emt, infer=c(T,T))