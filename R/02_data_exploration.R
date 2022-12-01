#' ---
#' title: "Biodiversity patterns in Bavarian - a first overview"
#' author: "RS-eco"
#' ---

#+setup, include = FALSE
knitr::opts_chunk$set(collapse = T, tidy=T, comment = NA, warning=F, message=F, eval=T, error=F, 
                      fig.path = "../figures/", dpi=100, fig.width=8, fig.height=6)
rm(list=ls()); invisible(gc())

#+ data_setup, results="hide"
# Load packages
library(data.table); library(dtplyr); library(dplyr, warn.conflicts = FALSE)
library(magrittr); library(tidyr); library(sf)
library(ggplot2); library(patchwork); library(scico); library(ggpmisc)

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
bluered <- rev(scico(n=255, palette="roma"))

# Set filedir
filedir <- sub("/R", "", getwd())

# Load data
load(paste0(filedir, "/extdata/ask_sub.rda"))

# define taxon colour
taxon_cols <- c("Aves"= "red", "Lepidoptera" = "#00c6a2", 
                "Orthoptera" = "#ffd706", "Odonata" = "#00486d")

#+ Figure1

### Plot number of observations per year
p1 <- ask_sub %>% group_by(jahr, class_order) %>% 
  summarize(Total=n()) %>% #filter(jahr >= 1950) %>%
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = T) +
  aes(x=as.numeric(jahr), y=Total, fill=class_order) +
  labs(x="", y='Total number of records') + 
  scale_x_continuous(limits=c(1984, 2020), expand=c(0,0), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult = c(0, .02))) + 
  scale_fill_manual(name="Class/Order", values = taxon_cols) + 
  theme_bw() + theme(legend.position = "none", axis.text.x=element_blank(),
                     axis.title.x = element_blank())

p2 <- ask_sub %>% group_by(jahr, class_order) %>% 
  summarize(Total=n_distinct(scientificNameStd)) %>% #filter(jahr >= 1950) %>%
  ggplot() + geom_bar(stat='identity', position = 'stack', show.legend = T) +
  aes(x=as.numeric(jahr), y=Total, fill=class_order) +
  xlab("Year") + ylab('Total number of species') + 
  scale_fill_manual(name="Class/Order", values = taxon_cols) + 
  scale_x_continuous(limits=c(1984, 2020), expand=c(0,0), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult = c(0, .02))) + 
  theme_bw() + theme(legend.position = "bottom")

# Summarise data
ask_xy <- ask_sub %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% ungroup()

### Plot map of number of observations per grid cell

p3 <- ask_xy %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_n)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nrecords", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy$sum_n)))) + 
  theme_map()

p4 <- ask_xy %>% ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  scale_fill_gradientn(name="Number of \nspecies (SR)", 
                       colours=bluered, values=scales::rescale(c(0, max(ask_xy$sum_art)))) + 
  theme_map()

leg <- ggpubr::as_ggplot(ggpubr::get_legend(p2))

((p1 + p3) / ({p2 + theme(legend.position="none")} + p4)) + leg + plot_layout(heights=c(7,7,1))

#' **Figure 1** a) Number of observations recorded per year, b) number of observations per grid cell, 
#' c) Species richness recorded per year, d) Species richness per grid cell. 
#' Different colours represent number of species per taxonomic group.

#+ SupportingFigure1, fig.width=8, fig.height=8

# Remove data with mon = 0
ask_sub %<>% filter(mon != 0)
#nrow(dat_ask); min(dat_ask$jahr)

ask_xyz_taxa <- ask_sub %>% group_by(jahr, mon, XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n())

#ask_z_taxa <- ask_sub %>% group_by(jahr, class_order) %>% 
#  summarise(sum_art=n_distinct(art), sum_n=n()) 

#ask_z_taxa %>% ggplot(aes(x=jahr, y=sum_art)) + 
#  geom_point(aes(size = sum_n), shape=16) + 
#  geom_smooth(method = "lm", se=T) + 
#  scale_size(name="Number of \nobservations") + 
#  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), 
#                                 sep = "*\", \"*")), formula =  y ~ x, parse = TRUE) + 
#  facet_wrap(.~class_order, scales="free", ncol=1) + 
#  labs(x="Year", y="Number of species") + theme_bw() + 
#  theme(strip.background = element_blank(), strip.text = element_text(size=12, face="bold"))

ask_xyz_taxa %>% group_by(jahr, class_order) %>% 
  summarise(wmn_sr = weighted.mean(sum_art, sum_n), no_obs = sum(sum_n)) %>%
  ggplot(aes(x=jahr, y=wmn_sr)) + geom_point(aes(size = no_obs), shape=16) + 
  geom_smooth(method = "lm", se=T, formula = y ~ x) + 
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), 
               col="blue", formula =  y ~ x, parse = TRUE) + 
  facet_wrap(.~class_order, scales="free", ncol=1) + 
  scale_size(name="Number of \nobservations", breaks=c(1000,2500,5000,10000,30000,50000)) + 
  scale_x_continuous(limits=c(1985, 2019), expand=c(0.01,0.01), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  labs(x="Year", y="Number of species") + theme_bw() + 
  theme(strip.background = element_blank(), strip.text = element_text(size=12, face="bold"),
        legend.position="bottom")

#' **Supporting Figure 1.** Relationship between mean number of species (weighted by the number of observations) 
#' and the year of observation. Size of the points reflects the total number of observations.

#+ SupportingFigure2, fig.width=8, fig.height=6
p1 <- ask_xyz_taxa %>% filter(class_order == "Aves", jahr %in% c(1985, 1994, 2002, 2010, 2019)) %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  facet_grid(class_order~jahr, switch="y") + theme_map() +
  scale_fill_gradientn(name="Number of \nspecies (SR)", colours=bluered, 
                       values=scales::rescale(c(0, max(ask_xyz_taxa[ask_xyz_taxa$class_order == "Aves",]$sum_art))))

p2 <- ask_xyz_taxa %>% filter(class_order == "Lepidoptera", jahr %in% c(1985, 1994, 2002, 2010, 2019)) %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  facet_grid(class_order~jahr, switch="y") + theme_map() + 
  scale_fill_gradientn(name="", colours=bluered, 
                       values=scales::rescale(c(0, max(ask_xyz_taxa[ask_xyz_taxa$class_order == "Lepidoptera",]$sum_art)))) + 
  theme(strip.text.x=element_blank())
p3 <- ask_xyz_taxa %>% filter(class_order == "Odonata", jahr %in% c(1985, 1994, 2002, 2010, 2019)) %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  facet_grid(class_order~jahr, switch="y") + theme_map() + 
  scale_fill_gradientn(name="", colours=bluered, 
                       values=scales::rescale(c(0, max(ask_xyz_taxa[ask_xyz_taxa$class_order == "Odonata",]$sum_art)))) + 
  theme(strip.text.x=element_blank())
p4 <- ask_xyz_taxa %>% filter(class_order == "Orthoptera", jahr %in% c(1985, 1994, 2002, 2010, 2019)) %>% 
  ggplot() + geom_rect(aes(xmin = XLU, xmax = XRU, ymin = YLU, ymax = YLO, fill=sum_art)) + 
  geom_sf(data=bavaria_gk, colour="black", fill=NA) + coord_sf() + 
  facet_grid(class_order~jahr, switch="y") + theme_map() + 
  scale_fill_gradientn(name="", colours=bluered, 
                       values=scales::rescale(c(0, max(ask_xyz_taxa[ask_xyz_taxa$class_order == "Orthoptera",]$sum_art))))  + 
  theme(strip.text.x=element_blank())
p1 / p2 / p3 / p4 + plot_layout(guides="collect")

#' **Supporting Figure 2.** Number of species (sr) per grid cell for the different taxonomic groups (Aves, Lepidoptera, Odonata & Orthoptera) for selected years (1985, 1994, 2002, 2010, 2019).

#+ SupportingFigure3, fig.width=8, fig.height=8

ask_xyz_taxa$mon <- factor(ask_xyz_taxa$mon, labels=month.abb)
ask_xyz_taxa$date <- as.Date(paste0(ask_xyz_taxa$jahr,ask_xyz_taxa$mon,"15"), "%Y%b%d")
ask_xyz_taxa %>% group_by(date, class_order) %>% 
  summarise(wmn_sr = weighted.mean(sum_art, sum_n), no_obs = sum(sum_n)) %>%
  ggplot(aes(x=date, y=wmn_sr)) + geom_point(aes(size = no_obs), shape=16) + 
  geom_line() + geom_smooth(method = "lm", se=T, formula = y ~ x) + 
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), 
               col="blue", formula =  y ~ x, parse = TRUE) + 
  facet_wrap(.~class_order, scales="free", ncol=1) + 
  scale_size(name="Number of \nobservations", breaks=c(500, 1000, 1500, 2000)) + 
  scale_x_date(expand=expansion(add=1)) + 
  labs(x="Year", y="Number of species") + theme_bw() + 
  theme(strip.background = element_blank(), strip.text = element_text(size=12, face="bold"),
        legend.position="bottom")

#' **Supporting Figure 3.** Relationship between mean number of species per month & year (weighted by the number of observations). 
#' Size of the points reflects the total number of observations.
