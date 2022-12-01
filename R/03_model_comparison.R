#' ---
#' title: "Model comparison of species richness models for Bavaria"
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

# Load TK25 grid
data("tk4tel_grid")

#+ model data

# Yearly data
dat_mod_year <- ask_sub %>% 
  group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, 
           KARTE_QUAD, jahr, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% 
  left_join(tk4tel_grid) %>% ungroup() %>% 
  dplyr::select(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, 
                KARTE_QUAD, jahr, class_order, sum_art, sum_n) %>% 
  drop_na(); invisible(gc())
save(dat_mod_year, file=paste0(filedir, "/extdata/dat_mod_year.rda"), compress="xz")

# Monthly data
dat_mod_mon <- ask_sub %>% 
  filter(mon != 0) %>% # Remove data with mon = 0
  group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, 
           KARTE_QUAD, jahr, mon, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% 
  left_join(tk4tel_grid) %>% ungroup() %>% 
  mutate(mon = factor(mon, levels=1:12, labels=month.abb)) %>%
  dplyr::select(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, 
                KARTE_QUAD, jahr, mon, class_order, sum_art, sum_n) %>% 
  drop_na(); invisible(gc())
save(dat_mod_mon, file=paste0(filedir, "/extdata/dat_mod_mon.rda"), compress="xz")

#+ gam
dat_total_year <- dat_mod_year %>% 
  group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, 
           KARTE_QUAD, jahr) %>% 
  summarise(sum_art=sum(sum_art), sum_n=sum(sum_n))
dat_total_year$class_order <- "Total"
dat_total_mon <- dat_mod_mon
dat_total_mon$class_order <- "Total"
dat_all_year <- dat_mod_year %>% bind_rows(dat_total_year)
dat_all_mon <- dat_mod_mon %>% bind_rows(dat_total_mon)

# generalized mixed effects model
library(mgcv)
if(!file.exists(paste0(filedir, "/data/gam_01.rda"))){
  gam_01 <- dat_all_year %>% nest_by(class_order) %>%
    mutate(mod = list(gam(sum_art ~ s(jahr) + s(sum_n), 
                          data = data, family=poisson)))
  save(gam_01, file=paste0(filedir, "/data/gam_01.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_01.rda"))
}
if(!file.exists(paste0(filedir, "/data/gam_02.rda"))){
  gam_02 <- dat_all_year %>% nest_by(class_order) %>%
    mutate(mod = list(gam(sum_art ~ s(jahr) + s(XLU) + s(YLU) + s(sum_n), 
                          data=data, family=poisson)))
  save(gam_02, file=paste0(filedir, "/data/gam_02.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_02.rda"))
}
if(!file.exists(paste0(filedir, "/data/gam_03.rda"))){
  gam_03 <- dat_all_year %>% nest_by(class_order) %>%
    mutate(mod = list(gam(sum_art ~ s(jahr) + s(XLU, YLU) + s(sum_n), 
                          data=data, family=poisson)))
  save(gam_03, file=paste0(filedir, "/data/gam_03.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_03.rda"))
}
if(!file.exists(paste0(filedir, "/data/gam_04.rda"))){
  gam_04 <- dat_all_mon %>% nest_by(class_order) %>%
    mutate(mod = list(gam(sum_art ~ mon + s(jahr) + s(sum_n), 
                          data=data, family=poisson)))
  save(gam_04, file=paste0(filedir, "/data/gam_04.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_04.rda"))
}
if(!file.exists(paste0(filedir, "/data/gam_05.rda"))){
  gam_05 <- dat_all_mon %>% nest_by(class_order) %>%
    mutate(mod = list(gam(sum_art ~ mon + s(jahr) + s(XLU) + s(YLU) + s(sum_n), 
                          data=data, family=poisson)))
  save(gam_05, file=paste0(filedir, "/data/gam_05.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_05.rda"))
}
if(!file.exists(paste0(filedir, "/data/gam_06.rda"))){
  gam_06 <- dat_all_mon %>% nest_by(class_order) %>%
    mutate(mod = list(gam(sum_art ~ mon + s(jahr) + s(XLU, YLU) + s(sum_n), 
                          data=data, family=poisson)))
  save(gam_06, file=paste0(filedir, "/data/gam_06.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_06.rda"))
}

library(broom.mixed)
df_01 <- gam_01 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ s(jahr) + s(sum_n)")
df_02 <- gam_02 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ s(jahr) + s(XLU) + s(YLU) + s(sum_n)")
df_03 <- gam_03 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ s(jahr) + s(XLU, YLU) + s(sum_n)")
df_04 <- gam_04 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ mon + s(jahr) + s(sum_n)")
df_05 <- gam_05 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ mon + s(jahr) + s(XLU) + s(YLU) + s(sum_n)")
df_06 <- gam_06 %>% summarise(glance(mod)) %>%
  mutate(formula="sum_art ~ mon + s(jahr) + s(XLU, YLU) + s(sum_n)")

# Use tidy to extract p-values
df_mod_glance <- bind_rows(list(df_01, df_02, df_03, df_04, df_05,
                         df_06))
save(df_mod_glance, file=paste0(filedir, "/data/df_mod_glance.rda"), compress="xz")
rm(df_01, df_02, df_03, df_04, df_05, df_06)

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

df_01 <- gam_01 %>% do(data.frame(class_order=.$class_order, 
                                  R2 = summary(.$mod)$r.sq)) %>%
  mutate(formula="sum_art ~ s(jahr) + s(sum_n)")
df_02 <- gam_02 %>% do(data.frame(class_order=.$class_order, 
                                  R2 = summary(.$mod)$r.sq)) %>%
  mutate(formula="sum_art ~ s(jahr) + s(XLU) + s(YLU) + s(sum_n)")
df_03 <- gam_03 %>% do(data.frame(class_order=.$class_order, 
                                  R2 = summary(.$mod)$r.sq)) %>%
  mutate(formula="sum_art ~ s(jahr) + s(XLU, YLU) + s(sum_n)")
df_04 <- gam_04 %>% do(data.frame(class_order=.$class_order, 
                                  R2 = summary(.$mod)$r.sq)) %>%
  mutate(formula="sum_art ~ mon + s(jahr) + s(sum_n)")
df_05 <- gam_05 %>% do(data.frame(class_order=.$class_order, 
                                  R2 = summary(.$mod)$r.sq)) %>%
  mutate(formula="sum_art ~ mon + s(jahr) + s(XLU) + s(YLU) + s(sum_n)")
df_06 <- gam_06 %>% do(data.frame(class_order=.$class_order, 
                                  R2 = summary(.$mod)$r.sq)) %>%
  mutate(formula="sum_art ~ mon + s(jahr) + s(XLU, YLU) + s(sum_n)")

#r2()
#get_variance()

df_mod_var <- bind_rows(list(df_01, df_02, df_03, df_04, df_05, df_06))
save(df_mod_var, file=paste0(filedir, "/data/df_mod_var.rda"), compress="xz")
rm(df_01, df_02, df_03, df_04, df_05, df_06)
rm(gam_01, gam_02, gam_03, gam_04, gam_05, gam_06)

#'
#' **Table 2.** Model variance (R2) of different variable combinations for each taxon 
#' (Aves, Lepidoptera, Odonata, Orthoptera) and all taxa together (Total).

#+ echo=F
# Create table
df_mod_var %>% dplyr::select(formula, class_order, R2) %>%
  mutate(R2 = signif(R2, digits=3)) %>%
  tidyr::pivot_wider(names_from="class_order", values_from="R2") %>% gt::gt()
