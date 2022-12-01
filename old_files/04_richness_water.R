#' ---
#' title: "Lake and river coverage and its relationship with biodiversity in Bavaria"
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

#+ water_data
library(bavDC)
data("diva_water_lines_deu")
data("diva_water_areas_deu")
diva_water_lines_deu <- sf::st_transform(diva_water_lines_deu, 31468)
diva_water_areas_deu <- sf::st_transform(diva_water_areas_deu, 31468)

# Load bavaria shapefile
data("bavaria", package="bavDC")
bavaria <- sf::st_transform(bavaria, 31468)

# Load TK25 grid
data("tk4tel_grid", package="bavDC")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
raster::projection(tk4tel_r) <- sp::CRS("+init=epsg:31468")

# Specify raster resolution (1km)
diva_water_lines_bav <- raster::rasterize(diva_water_lines_deu, raster::disaggregate(tk4tel_r, fact=10), field=1)
diva_water_areas_bav <- fasterize::fasterize(diva_water_areas_deu, raster=raster::disaggregate(tk4tel_r, fact=10))
water <- raster::aggregate(raster::mosaic(diva_water_lines_bav, diva_water_areas_bav, fun=mean), fact=10, fun=sum, na.rm=T)
names(water) <- "water_cover" 
water <- raster::mask(water, bavaria)

water_dat <- water %>% raster::rasterToPoints() %>% as.data.frame()

#+ SupportingFigure9
# Plot percentage cover by iucn cat
water_dat %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=water_cover)) + 
  geom_sf(data=bavaria, color="black", fill=NA) + 
  coord_sf(ndiscr=0) + theme_classic() + labs(x="", y="") + 
  scale_fill_gradientn(name="River/Lake coverage", 
                       na.value= "grey50", colours=rev(bluered)) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position="right")

#' **Supporting Figure 9.** Map of percentage coverage of rivers and lakes.

#+ SupportingFigure10, fig.width=10, fig.height=6

# Summarise data
ask_xy_taxa <- ask_sub %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% ungroup()
# Merge species and PA data
water_xy_taxa <- ask_xy_taxa %>% inner_join(water_dat %>% left_join(tk4tel_grid)) %>% 
  ungroup() %>% mutate(water_cover = replace_na(water_cover, 0))

library(ggpubr)
water_xy_taxa %>% 
  ggplot(aes(x=water_cover, y=sum_art)) + geom_point() + 
  geom_smooth() + facet_wrap(.~class_order, scales="free_y") + 
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult = c(0,.01))) + 
  labs(x="Lake/River coverage", y="Species richness") + theme_bw() + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        legend.position="none")

#' **Supporting Figure 10.** Species richness against water (lake/river) coverage.