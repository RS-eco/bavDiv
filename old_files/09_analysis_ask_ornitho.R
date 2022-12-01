#' ---
#' title: "ASK & Ornitho comparison of GLMM of species richness in Bavaria"
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

# Re-project shapefile to Northings and Eastings
bavaria_gk <- sf::st_transform(bavaria, crs=31468)

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

# Load Ornitho database
dat_ornitho <- vroom::vroom(paste0(filedir, "/extdata/dda-observations_bayern_2021-03-17.csv"))
#colnames(dat_ornitho)

# Only select needed columns
dat_ornitho %<>% mutate(jahr = year(DATE), mon = month(DATE)) %>% 
  filter(jahr >= 1985, jahr <= 2019) %>% # Filter data by Jahr!!!
  select(jahr, mon, COORD_F, COORD_E, COORD_N, FULL_FORM, LATIN_SPECIES, TOTAL_COUNT) %>% 
  arrange(COORD_F, COORD_E, COORD_N); invisible(gc())
#unique(dat_ornitho$mon)

# Turn TK25 into polygons
load(paste0(filedir, "/extdata/tk25.rda"))
pls <- lapply(1:nrow(tk25), function(x){
  st_polygon(list(rbind(c(tk25[x,]$XLU, tk25[x,]$YLU), c(tk25[x,]$XRU,tk25[x,]$YRU), 
                        c(tk25[x,]$XRO, tk25[x,]$YRO), c(tk25[x,]$XLO,tk25[x,]$YLO),
                        c(tk25[x,]$XLU, tk25[x,]$YLU))))
})
pl <- st_sfc(pls) %>% st_set_crs(31468)
tk25$KARTE_QUAD <- as.numeric(sub("/", "", tk25$quadrant))
tk25 <- st_set_geometry(tk25, pl)

# Intersect Ornitho points with TK25 polygons
ornitho_groups <- dat_ornitho %>% group_by(COORD_F, COORD_E, COORD_N) %>% group_keys()
coords_32 <- ornitho_groups %>% filter(COORD_F == 32) %>% select(-COORD_F) %>% 
  st_as_sf(coords=c("COORD_E", "COORD_N"), crs=25832) %>% st_transform(31468)
coords_33 <- ornitho_groups %>% filter(COORD_F == 33) %>% select(-COORD_F) %>% 
  st_as_sf(coords=c("COORD_E", "COORD_N"), crs=25833) %>% st_transform(31468)
coords <- bind_rows(coords_32, coords_33); rm(coords_32, coords_33); invisible(gc())
coords$id <- 1:nrow(coords)
coords_pl <- st_intersection(x=coords, y=tk25)
#head(coords_pl)
#tail(coords_pl)
coords_tk25 <- coords %>% as.data.frame() %>% select(-geometry) %>% 
  left_join(coords_pl %>% as.data.frame() %>% select(-geometry), by="id") %>%
  select(id, XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD); rm(coords, coords_pl)

# Add tk25 info to Ornitho data
dat_ornitho$id <- dat_ornitho %>% group_by(COORD_F, COORD_E, COORD_N) %>% 
  arrange(COORD_F, COORD_E, COORD_N) %>% group_indices()
dat_ornitho <- left_join(dat_ornitho, coords_tk25) %>% select(-id); rm(coords_tk25, ornitho_groups, pl, pls); invisible(gc())
#head(dat_ornitho)

# Add scientificNameStd to Ornitho data
dat_ornitho_sub <- dat_ornitho %>% mutate(LATIN_SPECIES = toupper(LATIN_SPECIES)) %>%
  left_join(taxonomyStd, by=c("LATIN_SPECIES" = "scientificName")) %>% drop_na(scientificNameStd)
dat_ornitho_sub$class_order <- "Aves"
dat_ornitho_sub$db <- "Ornitho"

dat_all <- ask_sub %>% bind_rows(dat_ornitho_sub %>% select(-c(class, COORD_F, COORD_E, COORD_N, FULL_FORM, 
                                                               TOTAL_COUNT, LATIN_SPECIES)))
rm(dat_ornitho, dat_ornitho_sub); invisible(gc())

#+ SupportingFigure17
### Plot number of observations per year
p1 <- dat_all %>% group_by(jahr, class_order) %>% 
  summarize(Total=n()) %>% #filter(jahr >= 1950) %>%
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = T) +
  aes(x=as.numeric(jahr), y=Total, fill=class_order) +
  labs(x="", y='Total number of records') + 
  scale_x_continuous(limits=c(1984, 2020), expand=c(0,0), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult = c(0, .02))) + 
  theme_bw() + theme(legend.position = "none", axis.text.x=element_blank(),
                     axis.title.x = element_blank())

p2 <- dat_all %>% group_by(jahr, class_order) %>% 
  summarize(Total=n_distinct(scientificNameStd)) %>% #filter(jahr >= 1950) %>%
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = T) +
  aes(x=as.numeric(jahr), y=Total, fill=class_order) +
  xlab("Year") + ylab('Total number of species') + 
  scale_fill_discrete(name="") + 
  scale_x_continuous(limits=c(1984, 2020), expand=c(0,0), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult = c(0, .02))) + 
  theme_bw() + theme(legend.position = "bottom")

# Summarise data
dat_xy <- dat_all %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% ungroup()

### Plot map of number of observations per grid cell

p3 <- dat_xy %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nrecords", 
                       colours=bluered, values=scales::rescale(c(0, max(dat_xy$sum_n)))) + 
  theme_map()

p4 <- dat_xy %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(dat_xy$sum_art)))) + 
  theme_map()

leg <- ggpubr::as_ggplot(ggpubr::get_legend(p2))

((p1 + p3) / ({p2 + theme(legend.position="none")} + p4)) + leg + plot_layout(heights=c(7,7,1))

#' **Supporting Figure 17.** a) Number of observations recorded per year, b) number of observations per grid cell, c) Species richness recorded per year, d) Species richness per grid cell for both the ASK & Ornitho database. Different colours represent number of species per taxonomic group.

# Remove data with mon = 0
dat_all %<>% filter(mon != 0)

#+ glmm2 
dat_aves2 <- dat_all %>% filter(class_order == "Aves") %>% 
  group_by(jahr, mon, 
           KARTE_QUAD) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% 
  inner_join(pa_bav_tk4tel %>% filter(iucn_cat == "Total") %>% 
               left_join(tk4tel_grid) %>% left_join(dist_pa_bav_tk4tel) %>%
               left_join(ng_ssymank)) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(.$perc_cov, 0)) %>% mutate(dist = replace_na(.$dist, 0)) %>% 
  #mutate(dist = cut(.$dist, breaks=c(0,10,15,20,25,30), include.lowest=T, na.rm=T)) %>%
  #mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>%  
  select(KARTE_QUAD, jahr, mon, Haupt_Nr,
         dist, perc_cov, sum_art, sum_n) %>% drop_na() %>%
  #mutate(mon = factor(mon, labels=month.abb)) %>%
  mutate(KARTE_QUAD = factor(KARTE_QUAD)) %>%
  mutate(sum_n = sqrt(sum_n)) %>%
  mutate(jahrIndex = as.numeric(factor(jahr))) %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist)); invisible(gc())

mod_all2 <- dat_all %>% group_by(jahr, mon, 
                                 KARTE_QUAD) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% 
  inner_join(pa_bav_tk4tel %>% filter(iucn_cat == "Total") %>% 
               left_join(tk4tel_grid) %>% left_join(dist_pa_bav_tk4tel) %>%
               left_join(ng_ssymank)) %>%  
  ungroup() %>% mutate(perc_cov = replace_na(.$perc_cov, 0)) %>% mutate(dist = replace_na(.$dist, 0)) %>% 
  #mutate(dist = cut(.$dist, breaks=c(0,10,15,20,25,30), include.lowest=T, na.rm=T)) %>%
  #mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>%  
  select(KARTE_QUAD, jahr, mon, Haupt_Nr,
         dist, perc_cov, sum_art, sum_n) %>% drop_na() %>%
  #mutate(mon = factor(mon, labels=month.abb)) %>%
  mutate(KARTE_QUAD = factor(KARTE_QUAD)) %>%
  mutate(sum_n = sqrt(sum_n)) %>%
  mutate(jahrIndex = as.numeric(factor(jahr))) %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist)); invisible(gc())

# generalized mixed effects model
library(glmmTMB)
if(!file.exists(paste0(filedir, "/data/lmer_all_ask_ornitho.rda"))){
  lmer_all_ask_ornitho <- glmmTMB(sum_art ~ jahrIndex + mon + 
                                    perc_cov + jahrIndex:perc_cov + 
                                    (1 | KARTE_QUAD) + (1 | Haupt_Nr),
                                  ziformula=~0, data=mod_all2, family=poisson, REML = FALSE, weights=sum_n)
  save(lmer_all_ask_ornitho, file=paste0(filedir, "/data/lmer_all_ask_ornitho.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_all_ask_ornitho.rda"))
}
if(!file.exists(paste0(filedir, "/data/lmer_aves_ask_ornitho.rda"))){
  lmer_aves_ask_ornitho <- glmmTMB(sum_art ~ jahrIndex + mon + 
                                     perc_cov + jahrIndex:perc_cov + 
                                     (1 | KARTE_QUAD) + (1 | Haupt_Nr),
                                   ziformula=~0, data=dat_aves2, family=poisson, 
                                   REML = FALSE, weights=sum_n)
  save(lmer_aves_ask_ornitho, file=paste0(filedir, "/data/lmer_aves_ask_ornitho.rda"), compress="xz")
} else{
  load(paste0(filedir, "/data/lmer_aves_ask_ornitho.rda"))
}

load(paste0(filedir, "/data/dat_mod.rda"))

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
         dist, perc_cov, sum_art, sum_n) %>% drop_na() %>%
  #mutate(mon = factor(mon, labels=month.abb)) %>%
  mutate(KARTE_QUAD = factor(KARTE_QUAD)) %>%
  mutate(sum_n = sqrt(sum_n)) %>%
  mutate(jahrIndex = as.numeric(factor(jahr))) %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist)); invisible(gc())

dat_aves <- dat_mod %>% filter(class_order == "Aves") %>%
  mutate(perc_cov = scale(perc_cov), dist = scale(dist))

load(paste0(filedir, "/data/lmer_all.rda"))
load(paste0(filedir, "/data/lmer_aves.rda"))

#+ SupportingFigure18
library(broom.mixed)
effects_all <- broom.mixed::tidy(lmer_all) %>% as.data.frame() %>% mutate(class_order = "Total (ASK)")
effects_aves <- broom.mixed::tidy(lmer_aves) %>% as.data.frame() %>% mutate(class_order = "Aves (ASK)")
effects_all2 <- broom.mixed::tidy(lmer_all_ask_ornitho) %>% as.data.frame() %>% mutate(class_order = "Total (ASK + Ornitho)")
effects_aves2 <- broom.mixed::tidy(lmer_aves_ask_ornitho) %>% as.data.frame() %>% mutate(class_order = "Aves (ASK + Ornitho)")

effects_full <- full_join(effects_all, effects_aves) %>% full_join(., effects_all2) %>% 
  full_join(., effects_aves2)

# define taxon colour
taxon_cols <- c("Aves (ASK)"= "red", "Total (ASK)" = "lightgrey", 
                "Aves (ASK + Ornitho)" = "#ffd706", "Total (ASK + Ornitho)" = "#00486d")

effects_full$class_order <- factor(effects_full$class_order)

# Effects plot
effects_full %>% filter(term != "(Intercept)", term != "sd__(Intercept)",
                        term != "sd__jahrIndex", term != "cor__(Intercept).jahrIndex") %>%
  mutate(term = factor(term, levels=rev(c("jahrIndex", "mon", "perc_cov", "jahrIndex:perc_cov")),
                       labels=rev(c("Year", "Month", "% Cover", "Year:% Cover")))) %>%
  mutate(p.value = cut(p.value, breaks=c(0, 0.001, 0.01, 0.05, 1), labels=c("***", "**", "*", "n.s."))) %>% 
  mutate(p.value = replace_na(p.value, "***")) %>%
  ggplot(aes(x=term, y=estimate, ymin=(estimate-std.error), ymax=(estimate+std.error), 
             colour = class_order, shape = p.value)) +
  geom_pointrange(position=position_dodge(width=0.75), size=1) + 
  geom_hline(yintercept=0, lty=2) + coord_flip() + 
  scale_y_continuous(expand=expansion(mult = c(.025, .025))) + 
  scale_colour_manual(name="Dataset", values = taxon_cols) + 
  guides(color = guide_legend(reverse=TRUE)) + 
  labs(y="Effect size", x="") + theme_bw() + 
  theme(legend.position=c(0.15,0.78), legend.background = element_blank())

#' **Supporting Figure 18.** Effect sizes of generalized linear mixed effects model of species richness for Aves and Total 
#' using ASK only and both ASK and Ornitho data. 
#' <!-- Significances to labels??? And shape for dataset!!! -->

library(emmeans); library(glmmTMB)
emm_all <- emmeans(lmer_all, ~ perc_cov*jahrIndex,
                   #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                   cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                   cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Total (ASK)") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))
emm_aves <- emmeans(lmer_aves, ~ perc_cov*jahrIndex,
                    #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                    cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                    cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Aves (ASK)") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))
emm_all_ask_ornitho  <- emmeans(lmer_all_ask_ornitho, ~ perc_cov*jahrIndex,
                   #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                   cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                   cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Total (ASK + Ornitho)") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))
emm_aves_ask_ornitho  <- emmeans(lmer_aves_ask_ornitho , ~ perc_cov*jahrIndex,
                    #at = list(perc_cov = c(0, 2.5, 5, 10, 20)),
                    cov.reduce = function(x) quantile(x, c(0, 0.5, 1)),
                    cov.keep = "jahrIndex", transform = "response") %>% 
  as.data.frame() %>% mutate(taxa = "Aves (ASK + Ornitho)") %>%
  mutate(perc_cov = factor(perc_cov, labels=c("Min", "Median", "Max")))

#+ SupportingFigure19

emm <- bind_rows(list(emm_all, emm_aves, emm_all_ask_ornitho, emm_aves_ask_ornitho))
emm %>% mutate(jahrIndex = jahrIndex+1984) %>%
  ggplot(aes(x=jahrIndex, y=rate)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=perc_cov), alpha=0.5) +
  geom_line(aes(lty=perc_cov), size=1) +
  facet_wrap(.~taxa, scales="free_y") + theme_bw() + 
  labs(x="Year", y="Species Richness", fill="% Cover", linetype = "% Cover") + 
  scale_x_continuous(limits=c(1985, 2019), expand=c(0.01,0.01), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  theme(strip.background=element_blank(), 
        strip.text=element_text(size=12, face="bold"),
        legend.position="bottom",
        legend.key.size = unit(1.0, 'cm'), #change legend key size
        legend.key.height = unit(1.0, 'cm'), #change legend key height
        legend.key.width = unit(1.0, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=12)) #change legend text font size)

#' **Supporting Figure 19.** Modelled change in species richness over time for min, median and max percentage cover protection 
#' for Aves and Total using ASK only and both ASK and Ornitho data. 