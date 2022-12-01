#' ---
#' title: "Additional Figures (old)"
#' author: "RS-eco"
#' ---

#+setup, include = FALSE
knitr::opts_chunk$set(collapse=T, tidy=T, comment=NA, warning=F, message=F, error=F, 
                      self.contained=T, fig.width=8, fig.height=6, dpi=100)
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

# Set file directory
filedir <- sub("/R", "", getwd())

# Load ASK database
my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(filedir, "/extdata/ASK.db"))
# Pull part of ASK database including data on species 
ask_data <- dplyr::tbl(my_db, paste("ask_art")) %>% collect()

# Load background grid
tk25 <- dplyr::tbl(my_db, "geo_tk25_quadranten") %>% dplyr::collect() %>%
  tidyr::unite(col="quadrant", c("KARTE", "QUADRANT"), sep="/", remove = TRUE) %>% 
  dplyr::select(-c("KARTE_QUAD")) 
save(tk25, file=paste0(filedir, "/data/tk25.rda"), compress="xz")

# Load taxonomy
library(bavDC)
data("taxonomyStd")

# Load ask_fuo
ask_fuo <- dplyr::tbl(my_db, "ask_fuo") %>% collect()
DBI::dbDisconnect(my_db)

# combine gridded map from dat_ask with full locations of recorded species and taxonomy
dat_ask <- ask_data %>% dplyr::select(-c(gkk_rw, gkk_hw, objnr, rw, hw)) %>% 
  left_join(ask_fuo) %>% left_join(tk25) %>%
  left_join(taxonomyStd, by=c("art" = "scientificName"))
dat_ask <- dat_ask %>% drop_na(class, order)
dat_ask$KARTE_QUAD <- as.numeric(sub("/", "", dat_ask$quadrant))

nrow(dat_ask)
ask_tk25 <- dat_ask %>% select(KARTE_QUAD) %>% drop_na()
nrow(ask_tk25)

ask_gkk <- ask_data %>% select(gkk_rw, gkk_hw) %>% drop_na()
nrow(ask_gkk)

# Create custom taxon vector (Aves, Lepidoptera, Odonata, Orthoptera)
dat_ask$class_order <- "Aves"
dat_ask$class_order[dat_ask$class == "Insecta"] <- dat_ask$order[dat_ask$class == "Insecta"]
unique(dat_ask$class_order)

dat_ask %<>% select(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, 
                    jahr, mon, class_order, scientificNameStd, art)
rm(ask_fuo, ask_data, my_db); invisible(gc())

# Remove data with mon = 0
#dat_ask %<>% filter(mon != 0)
#nrow(dat_ask); min(dat_ask$jahr)
dat_ask$db <- "ASK"

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

#+  divPA-FigureX, fig.width=8, fig.height=6
#########################

### Plot number of observations per year
library(ggforce)
dat_ask %>% group_by(jahr, class_order) %>% 
  summarize(Total=n()) %>% filter(jahr >= 1950) %>%
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = T) +
  aes(x=as.numeric(jahr), y=Total, fill=class_order) +
  xlab("Year") + ylab('Total number of records') + 
  scale_x_continuous(expand=c(0.018,0.018)) + 
  scale_y_continuous(limits=c(0,NA), expand=c(0.01,0.01)) + 
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5),
                     legend.position = c(0.2,0.3)) + 
  facet_zoom(xlim = c(1985, 2019), split=T)

#' **Figure X.** Number of observations recorded per year a) from 1950 and b) from 1985 until 2019. Different colours represent number of species per taxonomic group.

#+ divPA-FigureXX, fig.width=8, fig.height=6
### Plot number of species per year

dat_ask %>% group_by(jahr, class_order) %>% 
  summarize(Total=n_distinct(scientificNameStd)) %>% filter(jahr >= 1950) %>%
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = T) +
  aes(x=as.numeric(jahr), y=Total, fill=class_order) +
  xlab("Year") + ylab('Total number of species') + 
  scale_x_continuous(expand=c(0.018,0.018
  )) + 
  scale_y_continuous(limits=c(0,NA), expand=c(0.01,0.01)) + 
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5),
                     legend.position = "bottom") + 
  facet_zoom(xlim = c(1985, 2019))

#' ## Subset data by year
ask_sub <- dat_ask %>% filter(jahr >= 1985, jahr <= 2019); invisible(gc())

### Plot number of observations per year

#p3 <- ask_sub %>% group_by(jahr, class_order) %>% 
#  summarize(Total=n()) %>% 
#  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = FALSE) +
#  aes(x=as.factor(jahr), y=Total, fill=class_order) +
#  xlab("Year") + ylab('Total number of records') + 
#  scale_y_continuous(limits=c(0,NA), expand=c(0.01,0.01)) + 
#  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5))

#p4 <- ask_sub %>% group_by(jahr, class_order) %>% 
#  summarize(Total=n_distinct(scientificNameStd)) %>% 
#  ggplot(aes(x=as.factor(jahr), y=Total, fill=class_order)) + 
#  geom_bar(stat='identity', position = 'stack') +
#  labs(x="Year", y='Total number of species', fill="Taxon") + 
#  scale_y_continuous(limits=c(0,NA), expand=c(0.01,0.01)) + 
#  theme_bw() + theme(legend.position="bottom", 
#                     axis.text.x = element_text(angle=45, vjust=0.5))

#leg <- ggpubr::as_ggplot(ggpubr::get_legend(p4))

#(p1 + p3) / (p2 + {p4 + theme(legend.position="none")}) + 
#  leg + plot_layout(heights=c(6,6,1), widths=c(3,2))

#' **Figure XX.** Number of species recorded per year a) from 1950 and b) from 1985 until 2019. Different colours represent number of species per taxonomic group.

#+ divPA-FigureXXX, fig.width=8, fig.height=6, results="hide"
# For rough resolution replace KARTE_QUAD with karte

# Summarise data
ask_xy_taxa <- ask_sub %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% ungroup()

### Plot map of number of observations per grid cell

p1 <- ask_xy_taxa %>% filter(class_order == "Aves") %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + facet_wrap(.~class_order) + 
  scale_fill_gradientn(name="Number of records", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy_taxa[ask_xy_taxa$class_order == "Aves",]$sum_n)))) + 
  theme_map()
p2 <- ask_xy_taxa %>% filter(class_order == "Lepidoptera") %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + facet_wrap(.~class_order) + 
  scale_fill_gradientn(name="Number of records", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy_taxa[ask_xy_taxa$class_order == "Lepidoptera",]$sum_n)))) + 
  theme_map()
p3 <- ask_xy_taxa %>% filter(class_order == "Odonata") %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + facet_wrap(.~class_order) + 
  scale_fill_gradientn(name="Number of records", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy_taxa[ask_xy_taxa$class_order == "Odonata",]$sum_n)))) + 
  theme_map()
p4 <- ask_xy_taxa %>% filter(class_order == "Orthoptera") %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + facet_wrap(.~class_order) + 
  scale_fill_gradientn(name="Number of records", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy_taxa[ask_xy_taxa$class_order == "Orthoptera",]$sum_n)))) + 
  theme_map()
p1 + p2 + p3 + p4

#' **Figure XXX.** Number of observations (n) per grid cell for the different taxonomic groups (Aves, Lepidoptera, Odonata & Orthoptera) for all records from 1985 until 2019.

#+ divPA-FigureXXXX, fig.width=8, fig.height=6
p1 <- ask_xy_taxa %>% filter(class_order == "Aves") %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + facet_wrap(.~class_order) + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy_taxa[ask_xy_taxa$class_order == "Aves",]$sum_art)))) + 
  theme_map()
p2 <- ask_xy_taxa %>% filter(class_order == "Lepidoptera") %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + facet_wrap(.~class_order) + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy_taxa[ask_xy_taxa$class_order == "Lepidoptera",]$sum_art)))) + 
  theme_map()
p3 <- ask_xy_taxa %>% filter(class_order == "Odonata") %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + facet_wrap(.~class_order) + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy_taxa[ask_xy_taxa$class_order == "Odonata",]$sum_art)))) + 
  theme_map()
p4 <- ask_xy_taxa %>% filter(class_order == "Orthoptera") %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + facet_wrap(.~class_order) + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy_taxa[ask_xy_taxa$class_order == "Orthoptera",]$sum_art)))) + 
  theme_map()
p1 + p2 + p3 + p4

#' **Figure XXXX.** Number of species (sr) per grid cell for the different taxonomic groups (Aves, Lepidoptera, Odonata & Orthoptera) for all records from 1985 until 2019.

#+ divPA-FigureXXXXX, fig.width=8, fig.height=8

# Summarise data
ask_xyz_taxa <- ask_sub %>% group_by(jahr, XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n())

# Merge species and PA data
pa_xyz_taxa <- ask_xyz_taxa %>% inner_join(pa_bav_tk4tel %>% left_join(tk4tel_grid)) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(perc_cov, 0))

pa_xyz_taxa %>% mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>%  
  select(-c(KARTE_QUAD, XLU, YLU, XRU, YRU, XLO, YLO, XRO, YRO)) %>%
  select(x,y,jahr,class_order,iucn_cat,perc_cov,sum_art,sum_n) %>%
  #gather(var, value, -c(jahr, x, y, iucn_cat, perc_cov)) %>% 
  group_by(jahr, class_order, iucn_cat, perc_cov) %>% 
  summarise(wmn_sr = weighted.mean(sum_art, sum_n),
            no_obs = sum(sum_n)) %>%
  filter(iucn_cat == "Total") %>%
  ggplot(aes(x=jahr, y=wmn_sr, colour=perc_cov)) + 
  geom_point() + geom_smooth(method="lm", se = FALSE) + 
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), 
               formula =  y ~ x, parse = TRUE) + 
  facet_wrap(.~class_order, scales="free_y") + 
  scale_x_continuous(limits=c(1985, 2019), expand=c(0.01,0.01), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous(expand=expansion(mult = c(.01, .01))) + labs(x="Year", y="") + theme_bw() + 
  scale_colour_scico_d(name="Protection", palette = "roma") + 
  scale_linetype(name="Protection") + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        legend.position="bottom")

#' **Figure XXXXX.** Plot of species richness over time separately for each taxonomic group and for different protection coverage classes (0-25 %, 25-50%, 50-75%, 75-100%) considering all IUCN categories together.

### How does richness change in non-protected areas differ with distance to PAs
  
#+ divPA-FigureXXXXXX
data("dist_pa_bav_tk4tel")
dist_pa_bav_tk4tel <- dist_pa_bav_tk4tel %>% filter(iucn_cat == "Total") %>% dplyr::select(-iucn_cat)
pa_dist <- ask_xyz_taxa %>% left_join(tk4tel_grid) %>% left_join(dist_pa_bav_tk4tel) %>% ungroup()

# Turn NAs to 0
#unique(pa_sp$dist); max(pa_sp$dist, na.rm=T); quantile(pa_sp$dist, na.rm=T)
pa_dist <- pa_dist %>% mutate(dist = replace_na(.$dist, 0)) %>% 
  mutate(dist = cut(.$dist, breaks=c(0,10,15,20,25,30), include.lowest=T, na.rm=T))
#unique(pa_dist$dist)

pa_dist %>% select(-c(KARTE_QUAD, XLU, YLU, XRU, YRU, XLO, YLO, XRO, YRO)) %>%
  group_by(jahr, class_order, dist) %>% 
  summarise(wmn_sr = weighted.mean(sum_art, sum_n),
            no_obs = sum(sum_n)) %>%
  ggplot(aes(x=jahr, y=wmn_sr, colour=dist)) + 
  geom_point() + geom_smooth(method="lm", se = FALSE) + 
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), 
                                 sep = "*\", \"*")), formula =  y ~ x, parse = TRUE) + 
  facet_wrap(.~class_order, scales="free_y") + 
  scale_y_continuous(limits=c(0,NA)) + labs(x="Year", y="") + theme_bw() + 
  scale_colour_scico_d(name="Distance to \nprotection", palette = "roma", direction=-1) + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        legend.position="bottom")

#' **Figure XXXXXX.** Plot of species richness over time by distance to protected area quartiles (km).