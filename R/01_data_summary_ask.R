#' ---
#' title: "Extract biodiversity data for Bavaria"
#' author: "RS-eco"
#' ---

#+ setup, include = FALSE
knitr::opts_chunk$set(collapse = T, tidy=T, comment = NA, warning=F, message=F, eval=T, error=F, 
                      fig.path = "../figures/", dpi=100, fig.width=8, fig.height=6)
rm(list=ls()); invisible(gc())

#+ data_setup, results="hide"
# Load packages
library(data.table); library(dtplyr); library(dplyr, warn.conflicts = FALSE)
library(magrittr); library(tidyr)

########################

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
save(tk25, file=paste0(filedir, "/extdata/tk25.rda"), compress="xz")

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

# Add database name
dat_ask$db <- "ASK"

# Remove AVES data
#dat_ask <- dat_ask %>% filter(class_order != "Aves")

# ## Subset data by year
ask_sub <- dat_ask %>% filter(jahr >= 1985, jahr <= 2019); invisible(gc())
save(ask_sub, file=paste0(filedir, "/extdata/ask_sub.rda"), compress="xz")
