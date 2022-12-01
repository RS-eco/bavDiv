#' ---
#' title: "Biodiversity in Bavaria and its difference across nature scapes"
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

# Set filedir
filedir <- sub("/R", "", getwd())

# Load data
load(paste0(filedir, "/extdata/ask_sub.rda"))

# Load bavaria shapefile
data("bavaria", package="bavDC")
bavaria <- sf::st_transform(bavaria, 31468)

#+ ng_data
# Load naturraum data
data("ng_ssymank", package="bavDC")

# Load TK25 grid
data("tk4tel_grid", package="bavDC")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
raster::projection(tk4tel_r) <- sp::CRS("+init=epsg:31468")

# Get Naturräume
ng_ssymank <- sf::st_transform(ng_ssymank, 31468)
# Specify raster resolution (1km)
ng_dat <- fasterize::fasterize(ng_ssymank, raster=tk4tel_r, by="Haupt_Nr")
#raster::plot(ng_dat)
ng_dat <- ng_dat %>% raster::rasterToPoints() %>% as.data.frame() %>%
  tidyr::pivot_longer(cols=-c(x,y), names_to="Haupt_Nr", values_to="value") %>% 
  tidyr::drop_na() %>% select(-value)

#+ SupportingFigure7
# Plot percentage cover by iucn cat
ng_dat <- ng_dat %>% left_join(ng_ssymank %>% select(-geometry)) %>% drop_na() 
ng_dat %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=Haupt_Nr)) + 
  geom_sf(data=bavaria, color="black", fill=NA) + 
  coord_sf(ndiscr=0) + theme_classic() + labs(x="", y="") + 
  scale_fill_discrete(name="Naturraum", na.value= "grey50") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position="right")

#' **Supporting Figure 7.** Map of Naturräume.

#+ SupportingFigure8, fig.width=10, fig.height=6
# Summarise data
ask_xy_taxa <- ask_sub %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% ungroup()
# Merge species and PA data
ng_xy_taxa <- ask_xy_taxa %>% inner_join(ng_dat %>% left_join(tk4tel_grid)) %>% 
  ungroup() #%>% mutate(perc_cov = replace_na(perc_cov, 0))

library(ggpubr)
ng_xy_taxa %>% 
  ggviolin(x="Haupt_Nr", y="sum_art", fill="Haupt_Nr",
           add = "boxplot", add.params = list(fill = "white"))+
  facet_wrap(.~class_order, scales="free_y") + 
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult = c(0,.01))) + 
  labs(x="Naturraum", y="") + theme_bw() + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        legend.position="none")

#' **Supporting Figure 8.** Variation in species richness across the different Naturraeume.