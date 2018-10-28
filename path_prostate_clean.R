# Load relevant libraries
# Data Cleaning
library(stringr)
library(dplyr)
library(magrittr)

# Import data
pp <- read.csv("data/raw/SQL_QC_UODB_SDE_PARSE_20FEB2018.csv",
              header = T,
              stringsAsFactors = F,
              na.strings = "")

# IMPORT new UODB RP DATA
sp <- read.csv("data/raw/surgpath_export_20181018.csv",
               header = T,
               stringsAsFactors = F,
               na.strings = c(""))

pp %<>%
  left_join(sp, by = c("UCSF_MRN" = "UcsfID"))

colnames(pp) <- str_to_lower(colnames(pp))

pp$prostatewgt <- NULL

pp$u_gprimp <- as.numeric(pp$glsnpri)
pp$glsnpri <- NULL

pp$u_gsecondp <- as.numeric(pp$glsnsec)
pp$glsnsec <- NULL

pp$glsntertiary[pp$glsntertiary == "3+4+5"] <- "5"
pp$glsntertiary[pp$glsntertiary == "4+3+5"] <- "5"
pp$u_gtertp <- as.numeric(pp$glsntertiary)
pp$glsntertiary <- NULL


pp$u_nstagep <- pp$pstagen
pp$pstagen <- NULL

pp$u_tstagep <- pp$pstaget
pp$pstaget <- NULL


pp$u_pathmgnpos <- as.numeric(!pp$mgnnone)
pp$mgnnone <- NULL



pp$u_pathecepos <- as.numeric(pp$eceinvlvd)
pp$eceinvlvd <- NULL
pp$ecenone <- NULL

pp$u_pathsvipos <- as.numeric(!pp$svnone)
pp$svnone <- NULL

pp$u_pathnodes_status <- as.numeric(!pp$lymphnodesnone)
pp$lymphnodesnone <- NULL

mysum <- function(x)sum(x,na.rm = any(!is.na(x))) 

pp$u_pathnodes_dissected <- pp %>%
  select(lymphnodesdsctlt, lymphnodesdsctrt, lymphnodesdsctukn) %>%
  apply(., 1, FUN = mysum)

pp$u_pathnodes_positive <- pp %>%
  select(lymphnodesposlt, lymphnodesposrt, lymphnodesposukn) %>%
  apply(., 1, FUN = mysum)


pp %<>%
  select(-lymphnodesdsctlt, -lymphnodesdsctrt, -lymphnodesdsctukn, -lymphnodesposlt, -lymphnodesposrt, -lymphnodesposukn)


# Define useful vectors for field names for use later
v <- list()
v$sources <- c("u", "s", "p")
v$gnames <- c("_gprimp", "_gsecondp", "_gtertp") 
v$gleasonfields <- paste0(rep(v$sources, each=3), v$gnames)

v$tstagefields <- paste0(v$sources, "_tstagep")
v$nstagefields <- paste0(v$sources, "_nstagep")
v$marginfields <- paste0(v$sources, "_pathmgnpos")
v$node_status <- paste0(v$sources, "_pathnodes_status")
v$node_dissected <- paste0(v$sources, "_pathnodes_dissected")
v$node_pos <- paste0(v$sources, "_pathnodes_positive")
v$svi_pos <- paste0(v$sources, "_pathsvipos")
v$ece_pos <- paste0(v$sources, "_pathecepos")

# Convert Gleason Score fields to factors

# pp[,v$gleasonfields] <- as.data.frame(lapply(pp[,v$gleasonfields], 
#                                              factor,
#                                              c(3,4,5,"Missing")
#                                              )
#                                       )

# Convert T stage fields to factors
pp[,c(v$tstagefields)] <- as.data.frame(lapply(pp[,c(v$tstagefields)],
                                              factor,
                                              c("2a", "2b", "2c", "3a", "3b", "4", "x", "Missing")
                                              )
                                        )

# Convert if N stage fields to factors
pp[,c(v$nstagefields)] <- as.data.frame(lapply(pp[,c(v$nstagefields)],
                                               factor,
                                               c("0", "1", "x", "Missing")
                                               )
                                        )

# Convert number of nodes dissected and number of nodes positive to numeric
# will lose any that are slightly incorrect, which is OK (shows parser didnt work)
# pp[,c(v$node_dissected, v$node_pos)] <- as.data.frame(lapply(pp[,c(v$node_dissected, v$node_pos)], as.numeric))

# Convert 0/1 fields to true/false
pp[,c(v$marginfields, v$svi_pos, v$ece_pos, v$node_status)] <- pp[,c(v$marginfields, v$svi_pos, v$ece_pos, v$node_status)] == 1

# Path Nodes Status is left out of calculations because its not populated in the UODB data

# Gleason
pp[!is.na(pp$p_glsn_raw) & (is.na(pp$p_gprimp) | is.na(pp$p_gsecondp)),c("p_gprimp", "p_gsecondp")] <- "Missing"

# T Stage
pp[!is.na(pp$p_stagep_raw) & is.na(pp$p_tstagep),"p_tstagep"] <- "Missing"

# N Stage
pp[!is.na(pp$p_stagep_raw) & is.na(pp$p_tstagep),"p_tstagep"] <- "Missing"

pp[!is.na(pp$p_pathecepos_raw) & is.na(pp$p_pathecepos),"p_pathecepos"] <- "Missing"

pp[!is.na(pp$p_pathmgnpos_raw) & is.na(pp$p_pathmgnpos),"p_pathmgnpos"] <- "Missing"

pp[!is.na(pp$p_pathsvipos_raw) & is.na(pp$p_pathsvipos),"p_pathsvipos"] <- "Missing"

pp[!is.na(pp$p_pathnodes_status_raw) & is.na(pp$p_pathnodes_status),"p_pathnodes_status"] <- "Missing"

pp[!is.na(pp$p_pathnodes_positive_raw) & is.na(pp$p_pathnodes_positive),c("p_pathnodes_positive")] <- "Missing"

pp[!is.na(pp$p_pathnodes_dissected_raw) & is.na(pp$p_pathnodes_dissected),c("p_pathnodes_dissected")] <- "Missing"

# Create combined Gleason score Fields for easier comparisons
pp$u_glcombined = paste0(pp$u_gprimp,"+",pp$u_gsecondp)
pp$s_glcombined = paste0(pp$s_gprimp,"+",pp$s_gsecondp)
pp$p_glcombined = paste0(pp$p_gprimp,"+",pp$p_gsecondp)

# Remove NA fields that are generated when its missing for both
pp$u_glcombined[pp$u_glcombined == "Missing+Missing"] <- "Missing"
pp$p_glcombined[pp$p_glcombined == "Missing+Missing"] <- "Missing"
pp$s_glcombined[pp$s_glcombined == "Missing+Missing"] <- "Missing"

pp$u_glcombined[pp$u_glcombined == "NA+NA"] <- NA
pp$p_glcombined[pp$p_glcombined == "NA+NA"] <- NA
pp$s_glcombined[pp$s_glcombined == "NA+NA"] <- NA

pp$u_glcombined <- factor(pp$u_glcombined, 
                            levels = c(
                            "3+3", "3+4", "3+5",
                            "4+3", "4+4", "4+5",
                            "5+3", "5+4", "5+5",
                            "Missing"
                          )
                        )

pp$p_glcombined <- factor(pp$p_glcombined, 
                          levels = c(
                            "3+3", "3+4", "3+5",
                            "4+3", "4+4", "4+5",
                            "5+3", "5+4", "5+5",
                            "Missing"
                          )
                        )
pp$s_glcombined <- factor(pp$s_glcombined, 
                          levels = c(
                            "3+3", "3+4", "3+5",
                            "4+3", "4+4", "4+5",
                            "5+3", "5+4", "5+5",
                            "Missing"
                          )
                        )

## Fix Parser Path Nodes positive field
# This will eventually be done in java as well, but for now need to get accurate results here
# if the field shows "none" or "negative", it is being ignored, instead of being saved as 0
pp$p_pathnodes_positive[grepl("none", pp$p_pathnodes_positive_raw, ignore.case = T)] <- "0"
pp$p_pathnodes_positive[grepl(" one", pp$p_pathnodes_positive_raw, ignore.case = T)] <- "1"
pp$p_pathnodes_positive[grepl("negative", pp$p_pathnodes_positive_raw, ignore.case = T)] <- "0"

# strip any punctuation from the pathnodes_positive field
pp$p_pathnodes_positive <- as.numeric(str_replace_all(pp$p_pathnodes_positive, "[:punct:]", ""))
pp$p_pathnodes_dissected <- as.numeric(pp$p_pathnodes_dissected)

# Drop PatID (its blank, but just to be sure in future data extracts)
pp$de_id <- 1:nrow(pp)

#pp$ucsf_mrn <- NULL
pp$pat_id <- NULL

pp$p_pathecepos_wmiss <- pp$p_pathecepos
pp$p_pathsvipos_wmiss <- pp$p_pathsvipos
pp$p_pathmgnpos_wmiss <- pp$p_pathmgnpos

pp$p_pathecepos <- as.logical(pp$p_pathecepos)
pp$p_pathsvipos <- as.logical(pp$p_pathsvipos)
pp$p_pathmgnpos <- as.logical(pp$p_pathmgnpos)

# Export Data
save(pp, v, file="data/tidy/pp.rda")