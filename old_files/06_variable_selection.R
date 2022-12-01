#' ---
#' title: "Variable selection of species richness models for Bavaria"
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
data("tk4tel_grid")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
raster::projection(tk4tel_r) <- sp::CRS("+init=epsg:31468")

# Get Naturräume
ng_ssymank <- sf::st_transform(ng_ssymank, 31468)
# Specify raster resolution (1km)
ng_ssymank <- fasterize::fasterize(ng_ssymank, raster=tk4tel_r, by="Haupt_Nr")
ng_ssymank <- ng_ssymank %>% raster::rasterToPoints() %>% as.data.frame() %>%
  tidyr::pivot_longer(cols=-c(x,y), names_to="Haupt_Nr", values_to="value") %>% 
  tidyr::drop_na() %>% select(-value)

#+ water_data
library(bavDC)
data("diva_water_lines_deu")
data("diva_water_areas_deu")

# Transform water data
diva_water_lines_deu <- sf::st_transform(diva_water_lines_deu, 31468)
diva_water_areas_deu <- sf::st_transform(diva_water_areas_deu, 31468)

# Specify raster resolution (1km)
diva_water_lines_bav <- raster::rasterize(diva_water_lines_deu, raster::disaggregate(tk4tel_r, fact=10), field=1)
diva_water_areas_bav <- fasterize::fasterize(diva_water_areas_deu, raster=raster::disaggregate(tk4tel_r, fact=10))
water <- raster::aggregate(raster::mosaic(diva_water_lines_bav, diva_water_areas_bav, fun=mean), fact=10, fun=sum, na.rm=T)
names(water) <- "water_cover" 
water <- raster::mask(water, bavaria)
water_dat <- water %>% raster::rasterToPoints() %>% as.data.frame()

#+ pa_data
# Load PA data
data(pa_bav_tk4tel)
pa_bav_tk4tel <- pa_bav_tk4tel %>% filter(var == "perc_cov") %>% 
  rename(perc_cov=value) %>% select(-var)
#unique(pa_bav_tk4tel$iucn_cat)
pa_bav_tk4tel$iucn_cat <- factor(pa_bav_tk4tel$iucn_cat, 
                                 levels=c("I", "II", "III", "IV", "V", "VI",
                                          "Not.Assigned", "Not.Applicable", "Not.Reported", "Total"), 
                                 labels=c("I", "II", "III-IV", "III-IV", "V", "VI", "Not designated", 
                                          "Not designated", "Not designated", "Total"))

# Remove data with mon = 0
ask_sub %<>% filter(mon != 0)

#+ glmm
dat_mod <- ask_sub %>% group_by(jahr, mon, 
                                KARTE_QUAD, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% 
  inner_join(pa_bav_tk4tel %>% filter(iucn_cat == "Total") %>% 
               left_join(tk4tel_grid) %>% left_join(dist_pa_bav_tk4tel) %>%
               left_join(ng_ssymank)) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(.$perc_cov, 0)) %>% mutate(dist = replace_na(.$dist, 0)) %>% 
  #mutate(dist = cut(.$dist, breaks=c(0,10,15,20,25,30), include.lowest=T, na.rm=T)) %>%
  #mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>%  
  dplyr::select(KARTE_QUAD, jahr, mon, 
                Haupt_Nr, class_order, dist, perc_cov, sum_art, sum_n) %>% drop_na() %>%
  #mutate(mon = factor(mon, labels=month.abb)) %>%
  mutate(KARTE_QUAD = factor(KARTE_QUAD)) %>%
  mutate(sum_n = sqrt(sum_n)) %>%
  mutate(jahrIndex = as.numeric(factor(jahr))); invisible(gc())
save(dat_mod, file=paste0(filedir, "/extdata/dat_mod.rda"), compress="xz")

mod_all <- ask_sub %>% group_by(jahr, mon, 
                                KARTE_QUAD) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% 
  inner_join(pa_bav_tk4tel %>% filter(iucn_cat == "Total") %>% 
               left_join(tk4tel_grid) %>% left_join(dist_pa_bav_tk4tel) %>%
               left_join(ng_ssymank)) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(.$perc_cov, 0)) %>% mutate(dist = replace_na(.$dist, 0)) %>% 
  #mutate(dist = cut(.$dist, breaks=c(0,10,15,20,25,30), include.lowest=T, na.rm=T)) %>%
  #mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>%  
  select(KARTE_QUAD, jahr, mon, 
         Haupt_Nr, dist, perc_cov, sum_art, sum_n) %>% drop_na() %>%
  #mutate(mon = factor(mon, labels=month.abb)) %>%
  mutate(KARTE_QUAD = factor(KARTE_QUAD)) %>%
  mutate(sum_n = sqrt(sum_n)) %>%
  mutate(jahrIndex = as.numeric(factor(jahr))) %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist)); invisible(gc())
mod_all$class_order <- "Total"

dat_aves <- dat_mod %>% filter(class_order == "Aves") %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist))
dat_lepi <- dat_mod %>% filter(class_order == "Lepidoptera") %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist))
dat_odo <- dat_mod %>% filter(class_order == "Odonata") %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist))
dat_ortho <- dat_mod %>% filter(class_order == "Orthoptera") %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist))
invisible(gc())

dat_all <- bind_rows(dat_aves, dat_lepi, dat_odo, dat_ortho, mod_all)
rm(dat_aves, dat_lepi, dat_odo, dat_ortho, mod_all); invisible(gc())

# generalized mixed effects model
library(glmmTMB)
if(!file.exists(paste0(filedir, "/data/lmer_01.rda"))){
  lmer_01 <- dat_all %>% nest_by(class_order) %>%
    mutate(mod = list(glmmTMB(sum_art ~ jahrIndex + (1 | KARTE_QUAD), data = data,
                              ziformula=~0, family=poisson, 
                              REML = FALSE, weights=sum_n)))
  save(lmer_01, file=paste0(filedir, "/data/lmer_01.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_01.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_02.rda"))){
  lmer_02 <- dat_all %>% nest_by(class_order) %>%
    mutate(mod = list(glmmTMB(sum_art ~ perc_cov + (1 | KARTE_QUAD), data=data, 
                              ziformula=~0, family=poisson, 
                              REML = FALSE, weights=sum_n)))
  save(lmer_02, file=paste0(filedir, "/data/lmer_02.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_02.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_03.rda"))){
  lmer_03 <- dat_all %>% nest_by(class_order) %>%
    mutate(mod = list(glmmTMB(sum_art ~ jahrIndex + mon + (1 | KARTE_QUAD), data=data, 
                              ziformula=~0, family=poisson, 
                              REML = FALSE, weights=sum_n)))
  save(lmer_03, file=paste0(filedir, "/data/lmer_03.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_03.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_04.rda"))){
  lmer_04 <- dat_all %>% nest_by(class_order) %>%
    mutate(mod = list(glmmTMB(sum_art ~ jahrIndex + perc_cov + (1 | KARTE_QUAD), data=data, 
                              ziformula=~0, family=poisson, 
                              REML = FALSE, weights=sum_n)))
  save(lmer_04, file=paste0(filedir, "/data/lmer_04.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_04.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_05.rda"))){
  lmer_05 <- dat_all %>% nest_by(class_order) %>%
    mutate(mod = list(glmmTMB(sum_art ~ jahrIndex + mon + perc_cov + (1 | KARTE_QUAD), data=data, 
                              ziformula=~0, family=poisson, 
                              REML = FALSE, weights=sum_n)))
  save(lmer_05, file=paste0(filedir, "/data/lmer_05.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_05.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_06.rda"))){
  lmer_06 <- dat_all %>% nest_by(class_order) %>%
    mutate(mod = list(glmmTMB(sum_art ~ jahrIndex + mon + perc_cov + Haupt_Nr + (1 | KARTE_QUAD),
                     ziformula=~0, data=data, family=poisson, 
                     REML = FALSE, weights=sum_n)))
  save(lmer_06, file=paste0(filedir, "/data/lmer_06.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_06.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_07.rda"))){
  lmer_07 <- dat_all %>% nest_by(class_order) %>%
    mutate(mod = list(glmmTMB(sum_art ~ jahrIndex + mon + perc_cov + 
                                jahrIndex:perc_cov + (1 | KARTE_QUAD),
                     ziformula=~0, data=data, family=poisson, 
                     REML = FALSE, weights=sum_n)))
  save(lmer_07, file=paste0(filedir, "/data/lmer_07.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_07.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_08.rda"))){
  lmer_08 <- dat_all %>% nest_by(class_order) %>%
    mutate(mod = list(glmmTMB(sum_art ~ jahrIndex + mon + perc_cov + 
                                jahrIndex:perc_cov + Haupt_Nr + (1 | KARTE_QUAD),
                     ziformula=~0, data=data, family=poisson, 
                     REML = FALSE, weights=sum_n)))
  save(lmer_08, file=paste0(filedir, "/data/lmer_08.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_08.rda"))
}

library(broom.mixed)
df_01 <- lmer_01 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ jahr + (1 | KARTE_QUAD)")
df_02 <- lmer_02 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ perc_cov + (1 | KARTE_QUAD)")
df_03 <- lmer_03 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ jahr + monat + (1 | KARTE_QUAD)")
df_04 <- lmer_04 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ jahr + perc_cov + (1 | KARTE_QUAD)")
df_05 <- lmer_05 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ jahr + monat + perc_cov + (1 | KARTE_QUAD)")
df_06 <- lmer_06 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ jahr + monat + perc_cov + naturraum + (1 | KARTE_QUAD)")
df_07 <- lmer_07 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ jahr + monat + perc_cov + jahr:perc_cov + (1 | KARTE_QUAD)")
df_08 <- lmer_08 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ jahr + monat + perc_cov + naturraum + jahr:perc_cov + (1 | KARTE_QUAD)")

# Use tidy to extract p-values
#lmer_01 %>% summarise(tidy(mod)) %>%
#  mutate(formula="sum_art ~ jahrIndex")

df_mod_glance <- bind_rows(list(df_01, df_02, df_03, df_04, df_05,
                         df_06, df_07, df_08))
save(df_mod_glance, file=paste0(filedir, "/data/df_mod_glance.rda"), compress="xz")
rm(df_01, df_02, df_03, df_04, df_05, df_06, df_07, df_08)

#' **Table 1.** Model performance (BIC) of different variable combinations for each taxon 
#' (Aves, Lepidoptera, Odonata, Orthoptera) and all taxa together (Total).

#+ echo=F
# Create table
df_mod_glance %>% dplyr::select(formula, class_order, BIC) %>%
  tidyr::pivot_wider(names_from="class_order", values_from="BIC") %>% gt::gt()

#' **Table S1.** Model performance (AIC) of different variable combinations for each taxon 
#' (Aves, Lepidoptera, Odonata, Orthoptera) and all taxa together (Total).

#+ echo=F
# Create table
df_mod_glance %>% dplyr::select(formula, class_order, AIC) %>%
  tidyr::pivot_wider(names_from="class_order", values_from="AIC") %>% gt::gt()

library(performance); library(insight)
df_01 <- lmer_01 %>% do(data.frame(class_order=.$class_order, R2 = get_variance(.$mod))) %>%
  mutate(formula="sum_art ~ jahr + (1 | KARTE_QUAD)")
df_02 <- lmer_02 %>% do(data.frame(class_order=.$class_order, R2 = get_variance(.$mod))) %>%
  mutate(formula="sum_art ~ perc_cov + (1 | KARTE_QUAD)")
df_03 <- lmer_03 %>% do(data.frame(class_order=.$class_order, R2 = get_variance(.$mod))) %>%
  mutate(formula="sum_art ~ jahr + monat + (1 | KARTE_QUAD)")
df_04 <- lmer_04 %>% do(data.frame(class_order=.$class_order, R2 = get_variance(.$mod))) %>%
  mutate(formula="sum_art ~ jahr + perc_cov + (1 | KARTE_QUAD)")
df_05 <- lmer_05 %>% do(data.frame(class_order=.$class_order, R2 = get_variance(.$mod))) %>%
  mutate(formula="sum_art ~ jahr + monat + perc_cov + (1 | KARTE_QUAD)")
df_06 <- lmer_06 %>% do(data.frame(class_order=.$class_order, R2 = get_variance(.$mod))) %>%
  mutate(formula="sum_art ~ jahr + monat + perc_cov + naturraum + (1 | KARTE_QUAD)")
df_07 <- lmer_07 %>% do(data.frame(class_order=.$class_order, R2 = get_variance(.$mod))) %>%
  mutate(formula="sum_art ~ jahr + monat + perc_cov + jahr:perc_cov + (1 | KARTE_QUAD)")
df_08 <- lmer_08 %>% do(data.frame(class_order=.$class_order, R2 = get_variance(.$mod))) %>%
  mutate(formula="sum_art ~ jahr + monat + perc_cov + naturraum + jahr:perc_cov + (1 | KARTE_QUAD)")
#r2()
#get_variance()

df_mod_var <- bind_rows(list(df_01, df_02, df_03, df_04, df_05,
                                df_06, df_07, df_08))
save(df_mod_var, file=paste0(filedir, "/data/df_mod_var.rda"), compress="xz")
rm(df_01, df_02, df_03, df_04, df_05, df_06, df_07, df_08)
rm(lmer_01, lmer_02, lmer_03, lmer_04, lmer_05, lmer_06, lmer_07, lmer_08)

#'
#' **Table 2.** Model variance (R2.var.fixed) of different variable combinations for each taxon 
#' (Aves, Lepidoptera, Odonata, Orthoptera) and all taxa together (Total).

#+ echo=F
# Create table
df_mod_var %>% dplyr::select(formula, class_order, R2.var.fixed) %>%
  mutate(R2.var.fixed = signif(R2.var.fixed, digits=3)) %>%
  tidyr::pivot_wider(names_from="class_order", values_from="R2.var.fixed") %>% gt::gt()

#' **Table S2.** Model variance (R2.var.random) of different variable combinations for each taxon 
#' (Aves, Lepidoptera, Odonata, Orthoptera) and all taxa together (Total).

#+ echo=F
# Create table
df_mod_var %>% dplyr::select(formula, class_order, R2.var.random) %>%
  mutate(R2.var.fixed = signif(R2.var.fixed, digits=3)) %>%
  tidyr::pivot_wider(names_from="class_order", values_from="R2.var.random") %>% gt::gt()
