#' ---
#' title: "GAM of species richness in Bavaria"
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

# Load bavaria shapefile
data("bavaria", package="bavDC")
bavaria_gk <- sf::st_transform(bavaria, 31468)

# define taxon colour
taxon_cols <- c("Aves"= "red", "Lepidoptera" = "#00c6a2", 
                "Orthoptera" = "#ffd706", "Odonata" = "#00486d",
                "Total" = "black")

# Create custom ggplot2 theme
theme_map <- function(base_size = 10, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), axis.title = element_blank(), panel.border = element_blank(),
          plot.title = element_text(size=10, face="bold", hjust=0.5),
          strip.background = element_blank(),
          plot.tag.position = c(0.05,0.9),
          strip.text = element_text(size=10, face="bold"))
}

load(paste0(filedir, "/extdata/dat_mod_year.rda"))

dat_aves <- dat_mod_year %>% filter(class_order == "Aves")
dat_lepi <- dat_mod_year %>% filter(class_order == "Lepidoptera")
dat_odo <- dat_mod_year %>% filter(class_order == "Odonata")
dat_ortho <- dat_mod_year %>% filter(class_order == "Orthoptera")
invisible(gc())

dat_all <- dat_mod_year %>% 
  group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, jahr) %>% 
  summarise(sum_art=sum(sum_art), sum_n=sum(sum_n))

# generalized additive model
library(mgcv); library(gratia)
if(!file.exists(paste0(filedir, "/data/gam_all.rda"))){
  gam_all <- gam(sum_art ~ s(jahr) + s(sum_n) + s(XLU, YLU), 
                 family=poisson, data=dat_all)
  save(gam_all, file=paste0(filedir, "/data/gam_all.rda"), 
       compress="xz")
} else{
  load(paste0(filedir, "/data/gam_all.rda"))
}
summary(gam_all)
#plot(gam_all, pages=1)
#plot(gam_all, pages=1,residuals=TRUE)  ## show partial residuals
#plot(gam_all,pages=1,seWithMean=TRUE) ## `with intercept' CIs

#+ Figure2, fig.width=6, fig.height=9
draw(gam_all, ncol=1, residuals=T, contour=F, n=50)
gam.check(gam_all)

#' **Figure 2.** Generalized additive model (GAM) plots showing the partial effects on total species richness.
#' Tick marks on the x-axis are observed data points. The y-axis represents the effect function of isolation on Kipp's distance. 
#' Dark gray shadows indicate 95% confidence bounds. (Note that the scale of the y-axis differs between the two columns).

if(!file.exists(paste0(filedir, "/data/gam_aves.rda"))){
  gam_aves <- gam(sum_art ~ s(jahr) + s(sum_n) + s(XLU, YLU), 
                  data=dat_aves, family=poisson)
  save(gam_aves, file=paste0(filedir, "/data/gam_aves.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_aves.rda"))
}
if(!file.exists(paste0(filedir, "/data/gam_lepi.rda"))){
  gam_lepi <- gam(sum_art ~ s(jahr) + s(sum_n) + s(XLU, YLU), 
                  data=dat_lepi, family=poisson)
  save(gam_lepi, file=paste0(filedir, "/data/gam_lepi.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_lepi.rda"))
}
if(!file.exists(paste0(filedir, "/data/gam_odo.rda"))){
  gam_odo <- gam(sum_art ~ s(jahr) + s(sum_n) + s(XLU, YLU), 
                 data=dat_odo, family=poisson)
  save(gam_odo, file=paste0(filedir, "/data/gam_odo.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_odo.rda"))
}
if(!file.exists(paste0(filedir, "/data/gam_ortho.rda"))){
  gam_ortho <- gam(sum_art ~ s(jahr) + s(sum_n) + s(XLU, YLU), 
                   data=dat_ortho, family=poisson)
  save(gam_ortho, file=paste0(filedir, "/data/gam_ortho.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/gam_ortho.rda"))
}

library(broom.mixed)
effects_all <- broom.mixed::tidy(gam_all) %>% as.data.frame() %>% mutate(class_order = "Total")
effects_aves <- broom.mixed::tidy(gam_aves) %>% as.data.frame() %>% mutate(class_order = "Aves")
effects_lepi <- broom.mixed::tidy(gam_lepi) %>% as.data.frame() %>% mutate(class_order = "Lepidoptera")
effects_ortho <- broom.mixed::tidy(gam_ortho) %>% as.data.frame() %>% mutate(class_order = "Orthoptera")
effects_odo <- broom.mixed::tidy(gam_odo) %>% as.data.frame() %>% mutate(class_order = "Odonata")
effects_full <- full_join(effects_all, effects_lepi) %>% full_join(., effects_aves) %>% 
  full_join(., effects_ortho) %>% full_join(., effects_odo)

effects_full$class_order <- factor(effects_full$class_order, levels = c("Aves", "Lepidoptera", "Orthoptera", "Odonata", "Total"))
unique(effects_full$term)

#+ Figure3, fig.width=8, fig.height=12

set.seed(876)
sub_xy <- dat_odo %>% group_by(XLU, YLU) %>%
  group_keys() %>% sample_n(3)

summary(dat_all$sum_n)
dat_pred <- dat_all %>% inner_join(sub_xy) %>% 
  rename(sum_raw = sum_n) %>% mutate(sum_n = 500) %>%
  unite("xy", XLU, YLU, sep="_", remove=F)
dat_pred$fit <- predict(object=gam_all, newdata=dat_pred, 
                        type="response")
dat_pred <- dat_pred %>% 
  pivot_longer(cols=c("sum_art", "fit"), names_to="var", values_to="val") %>%
  mutate(var = factor(var, labels=c("Predicted richness", "Observed richness"))) %>%
  mutate(xy = factor(xy))
dat_pred$xy <- factor(dat_pred$xy, labels=c("Site 1", "Site 2", "Site 3"))
unique(dat_pred$xy)

p1 <- ggplot() + 
  geom_line(data=dat_pred, aes(x=jahr, y=val, col=var)) +
  geom_point(data=dat_pred %>% filter(var=="Observed richness"), 
             aes(x=jahr, y=val, size=sum_raw)) + 
  facet_wrap(. ~ xy) + theme_bw() + 
  scale_color_manual(name="", values=c("black", "red")) + 
  labs(x="Jahr", y="Total richness", size="Sampling size") +
  theme(strip.background = element_blank(), legend.position = "none",
        strip.text = element_text(size=12, face="bold"))

summary(dat_aves$sum_n)
aves_pred <- dat_aves %>% inner_join(sub_xy) %>% 
  rename(sum_raw = sum_n) %>% mutate(sum_n = 500) %>%
  unite("xy", XLU, YLU, sep="_", remove=F)
aves_pred$fit <- predict(object=gam_aves, newdata=aves_pred, 
                        type="response")
aves_pred <- aves_pred %>% 
  pivot_longer(cols=c("sum_art", "fit"), names_to="var", values_to="val") %>%
  mutate(var = factor(var, labels=c("Predicted richness", "Observed richness"))) %>%
  mutate(xy = factor(xy))
aves_pred$xy <- factor(aves_pred$xy, labels=c("Site 1", "Site 2", "Site 3"))

p2 <- ggplot() + 
  geom_line(data=aves_pred, aes(x=jahr, y=val, col=var)) +
  geom_point(data=aves_pred %>% filter(var=="Observed richness"), 
             aes(x=jahr, y=val, size=sum_raw)) + 
  facet_wrap(. ~ xy) + theme_bw() + 
  scale_color_manual(name="", values=c("black", "red")) + 
  labs(x="Jahr", y="Aves richness", size="Sampling size") +
  theme(strip.background = element_blank(), legend.position = "none",
        strip.text = element_blank())

summary(dat_lepi$sum_n)
lepi_pred <- dat_lepi %>% inner_join(sub_xy) %>% 
  rename(sum_raw = sum_n) %>% mutate(sum_n = 500) %>%
  unite("xy", XLU, YLU, sep="_", remove=F)
lepi_pred$fit <- predict(object=gam_lepi, newdata=lepi_pred, 
                         type="response")
lepi_pred <- lepi_pred %>% 
  pivot_longer(cols=c("sum_art", "fit"), names_to="var", values_to="val") %>%
  mutate(var = factor(var, labels=c("Predicted richness", "Observed richness"))) %>%
  mutate(xy = factor(xy))
lepi_pred$xy <- factor(lepi_pred$xy, labels=c("Site 1", "Site 2", "Site 3"))

p3 <- ggplot() + 
  geom_line(data=lepi_pred, aes(x=jahr, y=val, col=var)) +
  geom_point(data=lepi_pred %>% filter(var=="Observed richness"), 
             aes(x=jahr, y=val, size=sum_raw)) + 
  facet_wrap(. ~ xy) + theme_bw() + 
  scale_color_manual(name="", values=c("black", "red")) + 
  labs(x="Jahr", y="Lepidoptera richness", size="Sampling size") +
  theme(strip.background = element_blank(), legend.position = "none",
        strip.text = element_blank())

summary(dat_odo$sum_n)
odo_pred <- dat_odo %>% inner_join(sub_xy) %>% 
  rename(sum_raw = sum_n) %>% mutate(sum_n = 500) %>%
  unite("xy", XLU, YLU, sep="_", remove=F)
odo_pred$fit <- predict(object=gam_odo, newdata=odo_pred, 
                         type="response")
odo_pred <- odo_pred %>% 
  pivot_longer(cols=c("sum_art", "fit"), names_to="var", values_to="val") %>%
  mutate(var = factor(var, labels=c("Predicted richness", "Observed richness"))) %>%
  mutate(xy = factor(xy))
odo_pred$xy <- factor(odo_pred$xy, labels=c("Site 1", "Site 2", "Site 3"))

p4 <- ggplot() + 
  geom_line(data=odo_pred, aes(x=jahr, y=val, col=var)) +
  geom_point(data=odo_pred %>% filter(var=="Observed richness"), 
             aes(x=jahr, y=val, size=sum_raw)) + 
  facet_wrap(. ~ xy) + theme_bw() + 
  scale_color_manual(name="", values=c("black", "red")) + 
  labs(x="Jahr", y="Odonata richness", size="Sampling size") +
  theme(strip.background = element_blank(), legend.position = "none",
        strip.text = element_blank())

summary(dat_ortho$sum_n)
ortho_pred <- dat_ortho %>% inner_join(sub_xy) %>% 
  rename(sum_raw = sum_n) %>% mutate(sum_n = 500) %>%
  unite("xy", XLU, YLU, sep="_", remove=F)
ortho_pred$fit <- predict(object=gam_ortho, newdata=ortho_pred, 
                        type="response")
ortho_pred <- ortho_pred %>% 
  pivot_longer(cols=c("sum_art", "fit"), names_to="var", values_to="val") %>%
  mutate(var = factor(var, labels=c("Predicted richness", "Observed richness"))) %>%
  mutate(xy = factor(xy))
ortho_pred$xy <- factor(ortho_pred$xy, labels=c("Site 1", "Site 2", "Site 3"))

p5 <- ggplot() + 
  geom_line(data=ortho_pred, aes(x=jahr, y=val, col=var)) +
  geom_point(data=ortho_pred %>% filter(var=="Observed richness"), 
             aes(x=jahr, y=val, size=sum_raw)) + 
  facet_wrap(. ~ xy) + theme_bw() + 
  scale_color_manual(name="", values=c("black", "red")) + 
  labs(x="Jahr", y="Orthoptera richness", size="Sampling size") +
  theme(strip.background = element_blank(), legend.position = "bottom",
        strip.text = element_blank())

p1 + p2 + p3 + p4 + p5 + plot_layout(ncol=1)

#' **Figure 3.** Observed and modeled species richness over time for 
#' three randomly select sites. 
#' Modeled species richness was predicted for a sampling effort of N = 500.

#+ Figure4, fig.width=8, fig.height=7

summary(dat_all$sum_n)
dat_pred <- dat_all %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
dat_pred$fit <- predict(object=gam_all, newdata=dat_pred, type="response")
dat_pred$class_order <- "Total"
aves_pred <- dat_aves %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
aves_pred$fit <- predict(object=gam_aves, newdata=aves_pred, type="response")
lepi_pred <- dat_lepi %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
lepi_pred$fit <- predict(object=gam_lepi, newdata=lepi_pred, type="response")
odo_pred <- dat_odo %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
odo_pred$fit <- predict(object=gam_odo, newdata=odo_pred, type="response")
ortho_pred <- dat_ortho %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
ortho_pred$fit <- predict(object=gam_all, newdata=ortho_pred, type="response")

dat_pred_alltaxa <- bind_rows(list(dat_pred, aves_pred, lepi_pred, odo_pred, ortho_pred))

dat_past <- dat_pred %>% filter(jahr == 1985) %>% group_by(XLU, YLU)
dat_cur <- dat_pred %>% filter(jahr == 2019) %>% group_by(XLU, YLU)
mean_pred <- dat_pred_alltaxa %>% group_by(class_order, jahr) %>% 
  summarise(mn_fit = mean(fit, na.rm=T), sd_fit = sd(fit, na.rm=T))

p1 <- dat_past %>% ggplot() + 
  geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=fit)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + theme_map() +
  scale_fill_gradientn(name="Total richness", colours=bluered)
p2 <- dat_cur %>% ggplot() + 
  geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=fit)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + theme_map() +
  scale_fill_gradientn(name="Total richness", colours=bluered)
p3 <- mean_pred %>% ggplot(aes(x=jahr, y=mn_fit, ymin=mn_fit-sd_fit, ymax=mn_fit+sd_fit, 
                               fill=class_order, color=class_order)) + 
  geom_ribbon(alpha=0.3) + geom_line() + theme_bw() + 
  scale_colour_manual(name="Class/Order", values = taxon_cols) + 
  scale_fill_manual(name="Class/Order", values = taxon_cols) + 
  labs(x="Year", y="Mean predicted richness")

(p1 + p2) / p3 + plot_layout(heights=c(1.5,1))

#' **Figure 4.** a) past (1985) and b) present (2018) modeled species richness, 
#' as well as the mean modeled species richness over time for Bavaria.
#' Modeled species richness was predicted for a sampling effort of N = 500.
