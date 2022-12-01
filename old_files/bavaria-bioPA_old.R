#' ---
#' title: "Biodiversity and distribution changes in Bavarian protected areas (old)"
#' author: "RS-eco"
#' ---

#+setup, include = FALSE
knitr::opts_chunk$set(collapse=T, tidy=T, comment=NA, warning=F, message=F, error=F,
                      self.contained=T, fig.width=8, fig.height=6, dpi=100)
rm(list=ls()); invisible(gc())

#' <!--
#' Ideas:
#' 
#' Consider weighted averaging regression, see Bhatta et al. 2018
#' Rolling window for years & grid cells => see Graham et al. 2018 MEE, grainchanger R package
#' Bavaria maps with numbers added to each coloured grid cell (see maps from BSH.de)
#' Consider Hillebrand et al. 2017 Journal of Applied Ecology
#' Additional literature: Kery et al. 2013, Chase et al. 2018, McGlinn et al. 2018, McGlinn et al. 2021
#' 
#' - Provide taxon specific results
#' - Look at presence trends of several individual species after presence-selection
#' 
#' - Exclude birds?
#' - Include Ornitho data for birds!?
#' => Combine ASK & Ornitho???
#' 
#' - Inverse of density of observations in model
#' 
#' - Check out - MOBR package???
#' 
#' - https://grunwaldlab.github.io/analysis_of_microbiome_community_data_in_r/07--diversity_stats.html
#' - Use Bayesian approach? (Statistical Rethinking?)
#' 
#' - Cross-reference with Atlas-Data
#' => Bavarian range data for two time periods? => Available for which taxa?
#' -->
#' 
#' ## Figures
#' 
## ----data_setup, results="hide"----
# Load packages and data

# Load packages
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(forcats)
library(sf)
library(ggplot2)
library(patchwork)
library(ggpmisc)

# Load outline of bavaria
library(bavDC)
data(bavaria)

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
# Re-project shapefile to Northings and Eastings
bavaria_gk <- sf::st_transform(bavaria, crs=31468)

# Specify colour scheme
library(scico)
bluered <- rev(scico(n=255, palette="roma"))

#' 
## ----load_data, results="hide"----
########################

# Load ASK database
my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "../extdata/ASK.db")
# Pull part of ASK database including data on species 
ask_data <- dplyr::tbl(my_db, paste("ask_art"))

# Load background grid
tk25 <- dplyr::tbl(my_db, "geo_tk25_quadranten") %>% dplyr::collect() %>%
  tidyr::unite(col="quadrant", c("KARTE", "QUADRANT"), sep="/", remove = TRUE) %>% 
  dplyr::select(-c("KARTE_QUAD"))

# Load taxonomy
data("taxonomyStd")

# Load ask_fuo
ask_fuo <- dplyr::tbl(my_db, "ask_fuo") %>% collect()

# combine gridded map from ask_art with full locations of recorded species and taxonomy
ask_art <- ask_data %>% dplyr::select(-c(gkk_rw, gkk_hw, objnr, rw, hw)) %>% collect() %>% 
  left_join(ask_fuo) %>% left_join(tk25) %>%
  left_join(taxonomyStd, by=c("art" = "scientificName"))
ask_art <- ask_art %>% drop_na(class, order)
ask_art$KARTE_QUAD <- as.numeric(sub("/", "", ask_art$quadrant))

# Create custom taxon vector (Aves, Lepidoptera, Odonata, Orthoptera)
ask_art$class_order <- "Aves"
ask_art$class_order[ask_art$class == "Insecta"] <- ask_art$order[ask_art$class == "Insecta"]
unique(ask_art$class_order)

#ask_sub %>% group_by(class) %>% summarize(Total=n_distinct(scientificNameStd))
#length(unique(ask_sub$scientificNameStd))
#length(unique(ask_sub$art))

#' 
## ----bioPA-Figure1, fig.width=10, fig.height=6----
#########################

### Plot number of observations per year
p1 <- ask_art %>% group_by(jahr, class_order) %>% 
  summarize(Total=n()) %>% filter(jahr >= 1950) %>%
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = FALSE) +
  aes(x=as.numeric(jahr), y=Total, fill=class_order) +
  xlab("Year") + ylab('Total number of records') + 
  scale_x_continuous(limits=c(1950,2020), breaks=seq(1950, 2018, 2), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0,NA), expand=c(0.01,0.01)) + 
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5))

### Plot number of species per year

p2 <- ask_art %>% group_by(jahr, class_order) %>% 
  summarize(Total=n_distinct(scientificNameStd)) %>% filter(jahr >= 1950) %>%
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = FALSE) +
  aes(x=as.numeric(jahr), y=Total, fill=class_order) +
  xlab("Year") + ylab('Total number of species') + 
  scale_x_continuous(limits=c(1950,2020), breaks=seq(1950, 2018, 2), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0,NA), expand=c(0.01,0.01)) + 
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5))

ask_sum <- ask_art %>% group_by(jahr, class_order) %>% summarize(Total=n_distinct(scientificNameStd))

#' ## Subset data by year
ask_sub <- ask_art %>% filter(jahr >= 1985, jahr <= 2019); invisible(gc())

### Plot number of observations per year

p3 <- ask_sub %>% group_by(jahr, class_order) %>% 
  summarize(Total=n()) %>% 
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = FALSE) +
  aes(x=as.factor(jahr), y=Total, fill=class_order) +
  xlab("Year") + ylab('Total number of records') + 
  scale_y_continuous(limits=c(0,NA), expand=c(0.01,0.01)) + 
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5))

p4 <- ask_sub %>% group_by(jahr, class_order) %>% 
  summarize(Total=n_distinct(scientificNameStd)) %>% 
  ggplot(aes(x=as.factor(jahr), y=Total, fill=class_order)) + 
  geom_bar(stat='identity', position = 'stack') +
  labs(x="Year", y='Total number of species', fill="Taxon") + 
  scale_y_continuous(limits=c(0,NA), expand=c(0.01,0.01)) + 
  theme_bw() + theme(legend.position="bottom", 
                     axis.text.x = element_text(angle=45, vjust=0.5))

leg <- ggpubr::as_ggplot(ggpubr::get_legend(p4))

(p1 + p2) / (p3 + {p4 + theme(legend.position="none")}) + 
  leg + plot_layout(heights=c(6,6,1))

#' 
#' **Figure 1.** Overview of the Bavarian biodiversity data. a) Number of observations per year (from 1950), b) Number of species recorded per year (from 1950), c) Number of observations per year (from 1985 until 2019), b) Number of species recorded per year (from 1985 until 2019). Different colours represent number of observations/species per family.
#' 
#' **Note:** From now on, we only use observations between 1985 and 2019.
#' 
## ----bioPA-Figure2, results="hide"----
# For rough resolution replace KARTE_QUAD with karte

# Summarise data
ask_xyz <- ask_sub %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>%
  mutate("sr/n"=sum_art*(sum_art/sum_n), "sr*sr/n"=sum_art*(sum_art/sum_n)) %>% ungroup()

### Plot map of number of observations per grid cell

p1 <- ask_xyz %>% ggplot() + 
  geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nrecords (n)", 
                       colours=bluered, values=scales::rescale(c(0,  max(ask_xyz$sum_n)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))

### Plot map of number of species per grid cell

p2 <- ask_xyz %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xyz$sum_art)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))

p3 <- ask_xyz %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=`sr/n`)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="SR/n", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xyz$`sr/n`)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))

#' ## Subset data to only include species with a minimum number of samples

#+ include=F
#ask_sub %>% group_by(scientificNameStd, class_order, jahr) %>% 
#  summarize(Total=n()) %>% filter(Total >= 5) %>% ungroup() %>% 
#  summarise(n = n_distinct(scientificNameStd))

sp <- ask_sub %>% group_by(scientificNameStd, class_order, jahr) %>% 
  summarize(Total=n()) %>% filter(Total >= 5) %>% ungroup() %>% 
  summarise(sp=unique(.$scientificNameStd))

ask_sub2 <- ask_sub %>% filter(scientificNameStd %in% unlist(sp))

# Summarise data
ask_xyz2 <- ask_sub2 %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>%
  mutate("sr/n"=sum_art/sum_n, "sr*sr/n"=sum_art*(sum_art/sum_n)) %>% ungroup()

### Plot map of number of observations per grid cell

p4 <- ask_xyz2 %>% ggplot() + 
  geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nrecords (n)", colours=bluered, values=scales::rescale(c(0,  max(ask_xyz2$sum_n)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))

### Plot map of number of species per grid cell

p5 <- ask_xyz2 %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xyz2$sum_art)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))

p6 <- ask_xyz2 %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=`sr/n`)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="SR/n", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xyz2$`sr/n`)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))


# Extract lower and upper 5 % Quantile
quant <- quantile(ask_xyz2$sum_n, probs=c(0.05, 0.95))

# Remove grid cells with lower and upper 5 % quantile of records
ask3_xy <- ask_xyz2 %>% filter(sum_n >= quant[1] & sum_n <= quant[2]) %>% 
  group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD) %>% group_keys()
ask_sub3 <- ask_sub2 %>% filter(KARTE_QUAD %in% ask3_xy$KARTE_QUAD)

# Summarise data
ask_xyz3 <- ask_sub3 %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>%
  mutate("sr/n"=sum_art/sum_n, "sr*sr/n"=sum_art*(sum_art/sum_n)) %>% ungroup()

p7 <- ask_xyz3 %>% ggplot() + 
  geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nrecords (n)", colours=bluered, 
                       values=scales::rescale(c(0,  max(ask_xyz2$sum_n)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))

### Plot map of number of species per grid cell

p8 <- ask_xyz3 %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xyz2$sum_art)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))

p9 <- ask_xyz3 %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=`sr/n`)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="SR/n", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xyz2$`sr/n`)))) + 
  theme_map() #+ theme(legend.position=c(0.875,0.825))

(p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9)

#' 
#' **Figure 2.** Number of observations (n), species (sr) and sr/n per grid cell (from 1985 until 2019) for a) all records (top), b) records for all species with more than 5 records per year and c) records for all species with more than 5 records per year, but excluding grid cells with the upper and lower 5% of number of records.
#' 
#' **Note:** From now on, we only use records for species with more than 5 records (irrespective of sampling location) per year and excluding grid cells with the upper and lower 5% of number of records.
#' 

## ----
# Summarise data
ask_xyz4 <- ask_sub3 %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, jahr) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>%
  mutate("sr/n"=sum_art/sum_n, "sr*sr/n"=sum_art*(sum_art/sum_n)) %>% ungroup()

# Filter data by minimum number of samples per year (n >= 20)
ask_xyz4 <- ask_xyz4 %>% filter(sum_n >= 20)
sub <- ask_xyz4 %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, jahr) %>% group_keys()

ask_sub4 <- ask_sub3 %>% right_join(sub) 

#' 
#' **Note:** Now we also drop all data entries with less than 20 records per grid cell and year.
#' 
## ---- eval=F----
## ask_xyz4 %>% arrange(sum_n, KARTE_QUAD) %>%
##   ggplot() + geom_histogram(aes(x=as.factor(KARTE_QUAD), y=sum_n, fill=jahr),
##                             stat="identity", position="stack") +
##   theme_bw() + theme(axis.text.y = element_blank()) + coord_flip() +
##   scale_fill_scico(palette="roma")
## 
## ask_xyz4 %>% arrange(sum_n, KARTE_QUAD) %>%
##   ggplot() + geom_line(aes(x=jahr, y=sum_n, group=KARTE_QUAD)) +
##   theme_bw()

#' 
## ----bioPA-Figure3, fig.width=8, fig.height=10----------------------------------------------------------------------------------
### Plot relationship between number of species observed and total number of observations per grid cell
ask_sub4 %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, class_order) %>% 
  summarise(Total_species=n_distinct(art), Total_observations=n()) %>% 
  ggplot(aes(x=Total_observations, y=Total_species)) + 
  geom_point(shape=16, size = 3, show.legend=F) + 
  geom_smooth(method = "gam") + facet_wrap(.~class_order, scales="free") + 
  labs(x="Total number of records", y="Total number of species") + 
  theme_bw() + theme(strip.background = element_blank(), strip.text = element_text(size=12, face="bold"))

#' 
#' **Figure 3.** Relationship between number of observations and the number of species observed per grid cell.
#' 
#' **Table 1.** Minimum, mean and maximum percentage cover protected under the different IUCN categories in Bavaria.
#' 
## ----pa_summary_table, asis="TRUE"----------------------------------------------------------------------------------------------
# Load bavarian PA data
data(pa_bav_tk4tel)
colnames(pa_bav_tk4tel) <- c("x", "y", "IUCN Category", "value", "var")
pa_bav_tk4tel$`IUCN Category` <- factor(pa_bav_tk4tel$`IUCN Category`, 
                                        levels=c("I", "II", "III", "IV", "V", "Not.Assigned", 
                                                 "Not.Applicable", "Not.Reported"), 
                                        labels=c("I", "II", "III", "IV", "V", "Not designated", 
                                                 "Not designated", "Not designated"))
pa_bav_tk4tel %>% filter(var=="perc_cov") %>% 
  group_by(`IUCN Category`) %>% mutate(perc_cov = tidyr::replace_na(value, 0)) %>%
  summarise(`Minimum`=min(perc_cov), `Mean`=mean(perc_cov), `Max`=max(perc_cov)) %>% knitr::kable(format="pipe")

#' 
## ----bioPA-Figure4--------------------------------------------------------------------------------------------------------------
# Load TK25 data
data("tk4tel_grid")
r_tk4tel <- raster::rasterFromXYZ(tk4tel_grid)
raster::projection(r_tk4tel) <- "+init=epsg:31468"

# Calculate distance raster
data(pa_bav)
pa_bav <- sf::st_transform(pa_bav, crs=31468)
dist_pa_bav <- raster::distance(raster::rasterize(pa_bav, r_tk4tel, mask=T))
#raster::plot(dist_pa_bav)
dist_pa_bav2 <- raster::mask(dist_pa_bav, r_tk4tel)
dist_pa_bav <- as.data.frame(raster::rasterToPoints(dist_pa_bav))
colnames(dist_pa_bav) <- c("x", "y", "dist")
dist_pa_bav$dist <- dist_pa_bav$dist/1000
dist_pa_bav2 <- as.data.frame(raster::rasterToPoints(dist_pa_bav2))
colnames(dist_pa_bav2) <- c("x", "y", "dist")
dist_pa_bav2$dist <- dist_pa_bav2$dist/1000
rm(pa_bav); invisible(gc())

ggplot() + geom_tile(data=dist_pa_bav2, aes(x=x, y=y, fill=dist)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_scico(palette="roma") + theme_map()

#' 
#' **Figure 4.** Distance to protected area grid cells in Bavaria. **Note:** This approach currently does not consider the distance to surrounding protected areas outside of Bavaria.
#' 
## ----data_merge, results="hide"-------------------------------------------------------------------------------------------------
# Summarise data
ask_xyz4 <- ask_sub4 %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, jahr) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>%
  mutate("sr/n"=sum_art/sum_n, "sr*sr/n"=sum_art*(sum_art/sum_n)) %>% ungroup()

# Merge different data sources (species, pa and distance data)
data(pa_bav_tk4tel)
pa_bav_tk4tel <- pa_bav_tk4tel %>% filter(var == "perc_cov") %>% rename(perc_cov=value) %>% select(-var)
pa_bav_tk4tel$iucn_cat <- factor(pa_bav_tk4tel$iucn_cat, 
                                 levels=c("I", "II", "III", "IV", "V", 
                                          "Not.Assigned", "Not.Applicable", "Not.Reported"), 
                                 labels=c("I", "II", "III", "IV", "V", "Not designated", 
                                          "Not designated", "Not designated"))
pa_sp <- ask_xyz4 %>% left_join(pa_bav_tk4tel %>% left_join(tk4tel_grid)) %>% 
  left_join(dist_pa_bav %>% mutate(dist = cut(.$dist, quantile(.$dist, na.rm=T)))) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(perc_cov, 0))
#head(pa_sp)
#str(pa_sp)

# Reduce map size to karte rather than karte_quad

# Change distance class to character, and turn NAs to 0
pa_sp$dist <- as.character(pa_sp$dist)
pa_sp <- pa_sp %>% mutate(dist = replace_na(dist, 0))

# Filter data by perc_cover
pa_sp_sub <- pa_sp %>% filter(perc_cov >= 20)
nonpa_sp_sub <- pa_sp %>% filter(perc_cov == 0)

#' **Note:** Should these (sum_n) be the same or different?

# identify maximum number of samples
pa_sp_sub %>% group_by(iucn_cat, jahr) %>% summarise(sp_size=n()) %>% summary()
#pa_sp_sub %>% group_by(jahr) %>% summarise(sp_size=n()) %>% summary()
#pa_sp_sub %>% group_by(iucn_cat) %>% summarise(sp_size=n()) %>% summary()
nonpa_sp_sub %>% group_by(jahr, dist) %>% summarise(sp_size=n()) %>% summary()

#' There is no IUCN category I in entire Bavaria?
#'
#' **Note:** There are cells that have protection across multiple groups!!! 
#' This is currently not considered and some data points could potentially be duplicated due to this!
#'

#########################

# Subset data according to minimum number of samples per group
samp_n <- pa_sp_sub %>% group_by(iucn_cat) %>% summarise(n=n()) %>% 
  summarise(min(n)) %>% unlist()
withr::with_seed(1234, pa_bav_sub <- pa_sp_sub %>% 
                   group_by(iucn_cat) %>% sample_n(samp_n))

#########################

# Sample same number of locations for non-PA areas
withr::with_seed(1234, nonpa_bav_sub <- nonpa_sp_sub %>% 
                   group_by(dist) %>% sample_n(samp_n))

#' **Note:** For some reason there are NA values in the dist output, 
#' probably due to the probs specification in the quantile function!
#'

# Join locations
sample_loc <- bind_rows(pa_bav_sub %>% mutate(iucn_cat2 = "Protected"), 
                        nonpa_bav_sub %>% mutate(iucn_cat2 = "Not Protected"))

#' 
## ----bioPA-Figure5--------------------------------------------------------------------------------------------------------------
ggplot() + geom_tile(data=pa_bav_sub, aes(x=x, y=y, fill=iucn_cat)) + 
  geom_tile(data=nonpa_bav_sub, aes(x=x, y=y), fill="black") + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_discrete(name="IUCN") + 
  theme_map() #theme_bw() + labs(x="", y="")

#' 
#' **Figure 5.** Randomly sampled locations with sufficient observations (N >= 20) for non-protected grid cells (Protection <= 20 %, N =  `r nrow(nonpa_bav_sub)`, black) and protected grid cells (Protection >= 20 %, N = `r nrow(pa_bav_sub)`) for each IUCN category present (II, IV, V and Not-Reported, N = `r samp_n`).
#' 
## ----bioPA-Figure6, fig.width=6, fig.height=8----
sample_loc %>% group_by(x, y, iucn_cat2, jahr) %>% 
  select(-c(KARTE_QUAD, XLU, YLU, XRU, YRU, XLO, YLO, XRO, YRO, perc_cov, iucn_cat, dist)) %>%
  mutate("sr/n"=sum_art/sum_n, "sr*sr/n"=sum_art*(sum_art/sum_n)) %>% 
  rename(sr = sum_art, n = sum_n) %>% 
  gather(var, value, -c(x, y, jahr, iucn_cat2)) %>% 
  ggplot(aes(x=jahr, y=value)) + geom_point() + geom_smooth(method="lm") + 
  stat_poly_eq(aes(label = stat(eq.label)), formula =  y ~ x, parse = TRUE) + 
  facet_grid(var~iucn_cat2, scales="free_y", switch="y") + 
  #scale_y_continuous(limits=c(0,NA)) + 
  labs(x="Year", y="") + theme_bw() + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"))

#' 
#' **Figure 6.** Plot of species richness, sampling effort and species richness/sampling effort over time for protected and non-protected areas.
#' 
## ----bioPA-Figure7--------------------------------------------------------------------------------------------------------------
sample_loc %>% filter(iucn_cat2 == "Protected") %>% group_by(jahr, x, y, iucn_cat) %>%
  select(-c(KARTE_QUAD, XLU, YLU, XRU, YRU, XLO, YLO, XRO, YRO, perc_cov, dist, iucn_cat2)) %>%
  mutate("sr/n"=sum_art/sum_n, "sr*sr/n"=sum_art*(sum_art/sum_n)) %>% 
  rename(sr = sum_art, n = sum_n) %>% 
  gather(var, value, -c(jahr, x, y, iucn_cat)) %>% 
  ggplot(aes(x=jahr, y=value, colour=iucn_cat)) + 
  geom_point() + geom_smooth(method="lm", se = FALSE) + 
  stat_poly_eq(aes(label = stat(eq.label)), formula =  y ~ x, parse = TRUE) + 
  facet_wrap(var~., scales="free_y") + 
  #scale_y_continuous(limits=c(0,NA)) + 
  labs(x="Year", y="") + theme_bw() + 
  scale_colour_scico_d(name="Protection", palette = "roma") + 
  scale_linetype(name="Protection") + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"))

#' 
#' **Figure 7.** Plot of species richness, sampling effort and species richness/sampling effort over time for protected areas divided into the different IUCN categories.
#' 
#' **Note:** If data points/locations with a low sampling effort are filtered out, trends for protected areas increase, while the trends for non-protected areas stay stable.
#' 
#' ### How does richness change in non-protected areas differ with distance to PAs
#' 
## ----bioPA-Figure8--------------------------------------------------------------------------------------------------------------
sample_loc %>% filter(iucn_cat2 == "Not Protected") %>% group_by(jahr, x, y, dist) %>% 
  select(-c(KARTE_QUAD, XLU, YLU, XRU, YRU, XLO, YLO, XRO, YRO, perc_cov, iucn_cat, iucn_cat2)) %>%
  mutate("sr/n"=sum_art/sum_n, "sr*sr/n"=sum_art*(sum_art/sum_n)) %>% 
  rename(sr = sum_art, n = sum_n) %>% 
  mutate(dist = factor(dist)) %>% mutate(dist = forcats::lvls_reorder(dist, c(2,4,1,3,5))) %>%
  gather(var, value, -c(jahr, x, y, dist)) %>% 
  ggplot(aes(x=jahr, y=value)) + geom_point() + geom_smooth(method="lm") + 
  stat_poly_eq(aes(label = stat(eq.label)), formula =  y ~ x, parse = TRUE) + 
  facet_grid(var~dist, scales="free_y", switch="y") + 
  #scale_y_continuous(limits=c(0,NA)) + 
  labs(x="Year", y="") + theme_bw() + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"))

#' 
#' **Figure 8.** Plot of species richness, sampling effort and species richness/sampling effort over time for protected areas (dist = 0) and non-protected areas by distance to protected area quartiles (km).
#' 
## ---- eval=F, include=F---------------------------------------------------------------------------------------------------------
## #########################
## 
## #' ### Rarefaction using SpadeR package
## 
## library(SpadeR)
## 
## #########################
## 
## #' ### Rarefaction using vegan package
## 
## library(vegan)
## 
## veg_dat <- ask_sub %>% group_by(jahr, KARTE_QUAD, iucn_cat, scientificNameStd) %>%
##   summarise(count=n()) %>% spread(scientificNameStd, count)
## head(veg_dat)
## 
## veg_dat_try <- veg_dat %>% filter(jahr == 2017, iucn_cat == "IV") %>% ungroup() %>%
##   select(-c(jahr, iucn_cat, KARTE_QUAD)) %>% mutate_if(is.numeric, list(~replace_na(., 0)))
## veg_dat_try <- veg_dat_try[,colSums(veg_dat_try, na.rm=T)>=1]
## 
## S <- specnumber(veg_dat_try) # observed number of species
## (raremax <- min(rowSums(veg_dat_try)))
## Srare <- rarefy(veg_dat_try, raremax)
## plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
## abline(0, 1)
## rarecurve(veg_dat_try, step = 1, sample = raremax, col = "blue", cex = 0.6)
## 
## #' ### Rarefaction using BioTime code
## 
## source("R/rarefaction.R")
## TSrf <- list()
## IDs <- unique(ask_sub$iucn_cat)
## 
## for(i in c(1:3,5)){
##   if(i == 1){
##     data <- ask_sub %>% filter(is.na(iucn_cat)) %>%
##       group_by(jahr, KARTE_QUAD, scientificNameStd) %>%
##       summarise(n=n())
##   } else{
##     data <- ask_sub %>% filter(iucn_cat == IDs[i]) %>%
##       group_by(jahr, KARTE_QUAD, scientificNameStd) %>%
##       summarise(n=n())
##   }
##   TSrf[[i]]<-rarefysamples(data$jahr, data$KARTE_QUAD, data$scientificNameStd, data$n, 1)
## }
## TSrf <- Filter(Negate(is.null), TSrf)
## names(TSrf)<-IDs[c(1:3,5)]
## 
## rf<-do.call(rbind, TSrf)
## rf<-data.frame(rf, ID=rep(names(TSrf), times=unlist(lapply(TSrf, nrow))))
## rf<-rf[!is.na(rf$Year),-1]
## 
## #### prepare the rarefied output for diversity metric code
## t1<-with(rf, tapply(Abundance, list(Year, Species, ID), sum))
## t2<-unique(rf[, c("ID", "Year")])
## 
## #### produces a list of matrices for each study - in this case is only a single dataset
## dsList<-list()
## 
## for(i in 1:dim(t1)[3]){
##   id<-dimnames(t1)[[3]][i]
##   a<-subset(t2, ID==id)$Year
##   b<-t1[dimnames(t1)[[1]] %in% as.character(a),,i]
##   dsList[[i]]<-data.frame(Year = rownames(b), b)
## }
## 
## names(dsList) <- dimnames(t1)[[3]]
## 
## #### replacing NA with zero
## for(i in 1:(length(dsList))){
##   dsList[[i]][is.na(dsList[[i]])]<-0
## }
## 
## #' ## Investigate biodiversity trends
## 
## #### get the alpha diversity metrics
## alphaX<-lapply(dsList, function(x)getAlpha(x[,-1]))
## alpha<-do.call(rbind,alphaX)
## alpha$Year<-substring(rownames(alpha), nchar(rownames(alpha))-3)
## 
## #### get the beta diversity metrics
## betaX<-lapply(dsList, function(x)getBeta(x[,-1]))
## beta<-do.call(rbind, lapply(betaX, function(x)x[-1,]))
## beta$Year<-substring(rownames(beta), nchar(rownames(beta))-3)
## 
## #' ## Plot these trends using ggplot
## 
## #' N.B. this code is tailored to produce a ‘nice’ plot with year breaks for STUDY_ID 163,
## #' it may need to be adjusted when running for other studies
## 
## #### plot species richness temporal trend with simple regression line
## fitSR<-lm(alpha$S~alpha$Year)
## 
## srPlot<-ggplot(alpha, aes(x=Year, y=S))+ geom_line(size=1, colour="gray30")+
##   geom_point(aes(x=Year, y=S), colour="darkgreen", size=5) +
##   #scale_x_continuous(breaks=c(1993, 1995, 1997, 1999, 2001, 2003, 2005))+
##   geom_abline(intercept=fitSR$coef[1], slope=fitSR$coef[2], colour="gray10", size=1, linetype=3)+
##   themeNoGridAxes()+ylab("S")+xlab("Year")
## srPlot
## 
## #### plot the Jaccard similarity temporal trend with a simple regression line
## fitJ<-lm(beta$Jaccard~beta$Year)
## 
## jPlot<-ggplot(beta, aes(x=Year, y=Jaccard))+geom_line(size=1, colour="gray30")+
##   geom_point(aes(x=Year, y=Jaccard), colour="dodgerblue", size=5)+
##   #scale_x_continuous(breaks=c(1993, 1995, 1997, 1999, 2001, 2003, 2005))+
##   geom_abline(intercept=fitJ$coef[1], slope=fitJ$coef[2], colour="gray10", size=1, linetype=3)+
##   themeNoGridAxes()+ylab("Jaccard Similarity")+xlab("Year")+ylim(0, 1)
## jPlot
## 
## #' ### Rarefaction using iNext package
## 
## next_dat <- ask_sub %>% group_by(jahr, KARTE_QUAD, iucn_cat, scientificNameStd) %>%
##   summarise(count=n()) %>% mutate(count = if_else(is.na(count), 0, 1)) %>%
##   spread(KARTE_QUAD, count) %>%
##   mutate_if(is.numeric, list(~replace_na(., 0))) %>%
##   group_by(jahr, iucn_cat) %>% group_split()
## next_dat <- lapply(next_dat, function(x) dplyr::select(x, -c(jahr, iucn_cat)) %>% as.data.frame() %>%
##                      tibble::column_to_rownames(var = "scientificNameStd"))
## head(next_dat[[1]])
## 
## library(iNEXT)
## out.raw <- iNEXT(next_dat, datatype="incidence_raw")
## out.raw
## 
## #' ### Sample‐size‐based R/E curves
## 
## ggiNEXT(out.raw, type=1, color.var="site") +
##   theme_bw(base_size = 18) +
##   theme(legend.position="none")
## 
## #' ### Sample completeness curves
## ggiNEXT(out.raw, type=2, color.var="site") +
##   theme_bw(base_size = 18) +
##   theme(legend.position="none")
## 
## #' ### Coverage‐based R/E curves
## ggiNEXT(out.raw, type=3, color.var ="site") +
##   theme_bw(base_size = 18) +
##   theme(legend.position="bottom",
##         legend.title=element_blank())
## 
## # Point estimation function
## est_ask <- estimateD(next_dat, datatype="incidence_raw", base="coverage", level=0.985, conf=0.95)

