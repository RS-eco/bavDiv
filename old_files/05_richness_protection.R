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

# Set filedir
filedir <- sub("/R", "", getwd())

# Load data
load(paste0(filedir, "/extdata/ask_sub.rda"))

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

#+ SupportingFigure11
# Plot percentage cover by iucn cat
pa_bav_tk4tel %>% drop_na() %>% ggplot() + geom_tile(aes(x=x, y=y, fill=perc_cov)) + 
  facet_wrap(iucn_cat ~.) + coord_sf(ndiscr=0) + 
  theme_classic() + labs(x="", y="") + 
  scale_fill_gradientn(name="% Cover", na.value= "grey50", colours=rev(bluered)) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.position=c(0.85,0.2))

#' **Supporting Figure 11.** Map of percentage cover per grid cell for the different IUCN protection categories.

#+ SupportingFigure12, fig.width=8, fig.height=6
# Summarise data
ask_xy_taxa <- ask_sub %>% group_by(XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n()) %>% ungroup()
# Merge species and PA data
pa_xy_taxa <- ask_xy_taxa %>% inner_join(pa_bav_tk4tel %>% left_join(tk4tel_grid)) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(perc_cov, 0))

library(ggpubr)
pa_xy_taxa %>% 
  #mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>% 
  filter(iucn_cat == "Total") %>%
  ggplot(aes(x=perc_cov, y=sum_art)) + geom_point() + geom_smooth(formula=y ~ x) + 
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")), 
               col="blue", formula =  y ~ poly(x,2), parse = TRUE) + 
  facet_wrap(.~class_order, scales="free_y") + 
  scale_x_continuous(limits=c(0, 100), expand=expansion(mult = c(0,.01))) +
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult = c(0,.01))) + 
  labs(x="% Protection", y="") + theme_bw() + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        legend.position="none")

#' **Supporting Figure 12.** Variation in species richness across 
#' percentage coverage considering all IUCN protection categories together.

#+ SupportingFigure13, eval=T, fig.width=8, fig.height=8
pa_xy_taxa %>% mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>% 
  ggviolin(x="perc_cov", y="sum_art", fill="perc_cov",
           add = "boxplot", add.params = list(fill = "white"))+
  #stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  #stat_compare_means(label.y = 50) + 
  facet_grid(class_order~iucn_cat, scales="free_y", switch="y") + 
  labs(x="% Protection", y="") + theme_bw() + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        axis.text.x = element_text(angle=45,vjust=0.5),
        legend.position="none")

#' **Supporting Figure 13.** Variation in species richness across percentage coverage and different IUCN protection classes.

# Summarise data
ask_xyz_taxa <- ask_sub %>% group_by(jahr, XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, KARTE_QUAD, class_order) %>% 
  summarise(sum_art=n_distinct(art), sum_n=n())

# Merge species and PA data
pa_xyz_taxa <- ask_xyz_taxa %>% inner_join(pa_bav_tk4tel %>% left_join(tk4tel_grid)) %>% 
  ungroup() %>% mutate(perc_cov = replace_na(perc_cov, 0))

#+ SupportingFigure14, eval=T, fig.width=8, fig.height=10
pa_xyz_taxa %>% mutate(perc_cov = cut(perc_cov, c(0, 25, 50, 75, 100), include.lowest=T)) %>%  
  select(-c(KARTE_QUAD, XLU, YLU, XRU, YRU, XLO, YLO, XRO, YRO)) %>%
  select(x,y,jahr,class_order,iucn_cat,perc_cov,sum_art,sum_n) %>%
  #gather(var, value, -c(jahr, x, y, iucn_cat, perc_cov)) %>% 
  group_by(jahr, class_order, iucn_cat, perc_cov) %>% 
  summarise(wmn_sr = weighted.mean(sum_art, sum_n),
            no_obs = sum(sum_n)) %>%
  ggplot(aes(x=jahr, y=wmn_sr, colour=perc_cov)) + 
  geom_point() + geom_smooth(method="lm", se = FALSE) + 
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), 
                                 sep = "*\", \"*")), formula =  y ~ x, parse = TRUE) + 
  facet_grid(class_order~iucn_cat, scales="free_y", switch="y") + 
  scale_x_continuous(limits=c(1985, 2019), expand=c(0,0), 
                     breaks=c(1985,1990,1995,2000,2005,2010,2015)) + 
  scale_y_continuous(limits=c(0,NA)) + labs(x="Year", y="") + theme_bw() + 
  scale_colour_scico_d(name="Protection", palette = "roma") + 
  scale_linetype(name="Protection") + 
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        axis.text.x = element_text(angle=45,vjust=0.5),
        legend.position="bottom")

#' **Supporting Figure 14.** Plot of species richness over time separately for 
#' each taxonomic group and for different protection coverage classes 
#' (0-25 %, 25-50%, 50-75%, 75-100%) as well as different IUCN categories.
