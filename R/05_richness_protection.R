#' ---
#' title: "Protection coverage and its relationship with biodiversity in Bavaria"
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

# Specify colour scheme
bluered <- rev(scico(n=255, palette="roma"))

# Load bavaria shapefile
data("bavaria", package="bavDC")
bavaria_gk <- sf::st_transform(bavaria, 31468)

# Set filedir
filedir <- sub("/R", "", getwd())

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

#+ pa_data
# Load PA data
data(pa_bav_tk4tel, package="bavDC")
data("tk4tel_grid", package="bavDC")

pa_bav_tk4tel <- pa_bav_tk4tel %>% filter(var == "perc_cov") %>% 
  rename(perc_cov=value) %>% select(-var)
#unique(pa_bav_tk4tel$iucn_cat)
pa_bav_tk4tel$iucn_cat <- factor(pa_bav_tk4tel$iucn_cat, 
                                 levels=c("I", "II", "III", "IV", "V", "VI",
                                          "Not.Assigned", "Not.Applicable", "Not.Reported", "Total"), 
                                 labels=c("I", "II", "III-IV", "III-IV", "V", "VI", "Not designated", 
                                          "Not designated", "Not designated", "Total"))

#' **Table S3.** Minimum, mean and maximum percentage cover protected under the different IUCN categories in Bavaria.

pa_bav_tk4tel %>% rename(`IUCN Category`=iucn_cat) %>% group_by(`IUCN Category`) %>% 
  mutate(perc_cov = tidyr::replace_na(perc_cov, 0)) %>%
  summarise(`Minimum`=min(perc_cov), `Mean`=mean(perc_cov), `Max`=max(perc_cov)) %>% 
  knitr::kable(format="pipe")

#+ Figure5
# Plot percentage cover by iucn cat
pa_bav_tk4tel %>% drop_na() %>% ggplot() + geom_tile(aes(x=x, y=y, fill=perc_cov)) + 
  facet_wrap(iucn_cat ~.) + theme_map() + labs(x="", y="") + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf(ndiscr=0) +  
  scale_fill_gradientn(name="% Cover", na.value= "grey50", colours=rev(bluered)) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position=c(0.85,0.2))

#' **Figure 5.** Map of percentage cover per grid cell for the different 
#' IUCN protection categories.

# Filter data by perc_cover
pa <- pa_bav_tk4tel %>% inner_join(tk4tel_grid) %>% filter(perc_cov >= 50)
nonpa <- tk4tel_grid %>% anti_join(pa)

#' There is no IUCN category I in entire Bavaria?
#'
#' **Note:** There are cells that have protection across multiple groups!!! 
#' This is currently not considered and some data points could potentially 
#' be duplicated due to this!

# Sample same number of locations for PA and non-PA areas
(samp_n <- pa %>% group_by(iucn_cat) %>% summarise(n=n()) %>% 
  summarise(min(n)) %>% unlist())
(samp_n2 <- nonpa %>% summarise(n=n()) %>% summarise(min(n)) %>% unlist())

withr::with_seed(1234, pa_bav_sub <- pa %>% filter(iucn_cat != "Total") %>% 
                   group_by(iucn_cat) %>% sample_n(samp_n))
withr::with_seed(1234, nonpa_bav_sub <- nonpa %>% sample_n(samp_n*4))

#' **Note:** For some reason there are NA values in the dist output, 
#' probably due to the probs specification in the quantile function!
#'

# Join locations
sample_loc <- bind_rows(pa_bav_sub %>% mutate(iucn_cat2 = "Protected"), 
                        nonpa_bav_sub %>% mutate(iucn_cat2 = "Not Protected"))

#+ Figure6, fig.width=9, fig.height=6
ggplot() + geom_tile(data=pa_bav_sub, aes(x=x, y=y, fill=iucn_cat)) + 
  geom_tile(data=nonpa_bav_sub, aes(x=x, y=y), fill="black") + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_discrete(name="IUCN") + 
  theme_map() #theme_bw() + labs(x="", y="")

#' **Figure 6.** Randomly sampled locations with sufficient observations (N >= 20) 
#' for non-protected grid cells (Protection <= 20 %, N =  `r nrow(nonpa_bav_sub)`, black) 
#' and protected grid cells (Protection >= 20 %, N = `r nrow(pa_bav_sub)`) 
#' for each IUCN category present (II, IV, V and Not-Reported, N = `r samp_n`).

# Load data
load(paste0(filedir, "/extdata/dat_mod_year.rda"))
dat_aves <- dat_mod_year %>% filter(class_order == "Aves")
dat_lepi <- dat_mod_year %>% filter(class_order == "Lepidoptera")
dat_odo <- dat_mod_year %>% filter(class_order == "Odonata")
dat_ortho <- dat_mod_year %>% filter(class_order == "Orthoptera")
dat_all <- dat_mod_year %>% 
  group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, jahr) %>% 
  summarise(sum_art=sum(sum_art), sum_n=sum(sum_n)) %>% mutate(class_order="Total")
rm(dat_mod_year); invisible(gc())

# Load models
load(paste0(filedir, "/data/gam_all.rda"))
load(paste0(filedir, "/data/gam_aves.rda"))
load(paste0(filedir, "/data/gam_lepi.rda"))
load(paste0(filedir, "/data/gam_odo.rda"))
load(paste0(filedir, "/data/gam_ortho.rda"))

# Predict richness with higher sampling effort
set.seed(876)
summary(dat_all$sum_n)
dat_pred <- dat_all %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
library(mgcv)
dat_pred$fit <- predict(object=gam_all, newdata=dat_pred, type="response")
dat_aves <- dat_aves %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
dat_aves$fit <- predict(object=gam_aves, newdata=dat_aves, type="response")
dat_lepi <- dat_lepi %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
dat_lepi$fit <- predict(object=gam_lepi, newdata=dat_lepi, type="response")
dat_odo <- dat_odo %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
dat_odo$fit <- predict(object=gam_odo, newdata=dat_odo, type="response")
dat_ortho <- dat_ortho %>% rename(sum_raw = sum_n) %>% mutate(sum_n = 500)
dat_ortho$fit <- predict(object=gam_ortho, newdata=dat_ortho, type="response")

dat_all <- bind_rows(list(dat_pred, dat_aves, dat_lepi, dat_odo, dat_ortho))
dat_prot <- inner_join(sample_loc, tk4tel_grid) %>% inner_join(dat_all) %>%
  mutate(iucn_cat = replace_na(as.character(iucn_cat), "Not protected"))
unique(dat_prot$iucn_cat)
dat_prot$iucn_cat <- factor(dat_prot$iucn_cat, levels=c("II", "III-IV", "V", "Not designated", "Not protected"))
unique(dat_prot$iucn_cat2)

#+ Figure7, fig.width=8, fig.height=10
dat_prot %>% group_by(jahr, class_order, iucn_cat2) %>% 
  summarise(mn_sr = mean(fit), sd_sr = sd(fit)) %>%
  ggplot(aes(x=jahr, y=mn_sr, ymin=mn_sr-sd_sr, ymax=mn_sr+sd_sr, 
             fill=iucn_cat2, colour=iucn_cat2)) + 
  geom_ribbon(alpha=0.2) + geom_line() + 
  facet_wrap(class_order~., ncol=1, scales="free_y", strip.position="left") + 
  scale_x_continuous(limits=c(1985, 2019), expand=c(0,0), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous(limits=c(0,NA)) + labs(x="Year", y="") + theme_bw() + 
  scale_colour_scico_d(name="Protection", palette = "roma") + 
  scale_fill_scico_d(name="Protection", palette = "roma") + 
  scale_linetype(name="Protection") + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        legend.position="bottom")

#' **Figure 7.** Plot of species richness over time separately for 
#' each taxonomic group and for protected versus non-protected areas.

#+ Figure8, fig.width=8, fig.height=10
dat_prot %>% group_by(jahr, class_order, iucn_cat) %>% 
  summarise(mn_sr = mean(fit), sd_sr = sd(fit)) %>%
  ggplot(aes(x=jahr, y=mn_sr, ymin=mn_sr-sd_sr, ymax=mn_sr+sd_sr, 
             fill=iucn_cat, colour=iucn_cat)) + 
  geom_ribbon(alpha=0.2) + geom_line() + 
  facet_wrap(class_order~., ncol=1, scales="free_y", strip.position="left") + 
  scale_x_continuous(limits=c(1985, 2019), expand=c(0,0), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous(limits=c(0,NA)) + labs(x="Year", y="") + theme_bw() + 
  scale_colour_scico_d(name="Protection", palette = "roma", direction=-1) + 
  scale_fill_scico_d(name="Protection", palette = "roma", direction=-1) + 
  scale_linetype(name="Protection") + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        legend.position="bottom")

#' 
#' **Figure 8** Plot of species richness, sampling effort and 
#' species richness/sampling effort over time for protected areas 
#' divided into the different IUCN categories.