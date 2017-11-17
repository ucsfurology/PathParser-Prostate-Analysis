# Load relevant libraries
# Data Cleaning
library(stringr)

# Import data from Coop
pp <- read.csv("data/raw/SQL_QC_UODB_SDE_PARSE_03NOV2017.csv",
              header = T,
              stringsAsFactors = F,
              na.strings = "")

colnames(pp) <- str_to_lower(colnames(pp))

# Drop PatID (its blank, but just to be sure in future data extracts)
pp$de_id <- 1:nrow(pp)
pp$ucsf_mrn <- NULL
pp$pat_id <- NULL

# Define useful vectors for field names
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
pp[,v$gleasonfields] <- as.data.frame(lapply(pp[,v$gleasonfields], as.factor))

# Convert if T and n stage fields to factors
pp[,c(v$tstagefields, v$nstagefields)] <- as.data.frame(lapply(pp[,c(v$tstagefields, v$nstagefields)], as.factor))

# Convert number of nodes dissected and number of nodes positive to numeric
# will lose any that are slightly incorrect, which is OK (shows parser didnt work)
pp[,c(v$node_dissected, v$node_pos)] <- as.data.frame(lapply(pp[,c(v$node_dissected, v$node_pos)], as.numeric))

# Convert 0/1 fields to true/false
pp[,c(v$marginfields, v$svi_pos, v$ece_pos)] <- pp[,c(v$marginfields, v$svi_pos, v$ece_pos)] == 1

# Certain UODB fields, if blank, need to be set to null
pp$u_nstagep[pp$u_nstagep==""] <- NA

###########################
# Do we need to populate u_pathnodes_status field?

# Convert tstage and nstage fields to characters
pp$u_tstagep <- sapply(pp$u_tstagep, as.character)
pp$p_tstagep <- sapply(pp$p_tstagep, as.character)
pp$s_tstagep <- sapply(pp$s_tstagep, as.character)

pp$u_nstagep <- sapply(pp$u_nstagep, as.character)
pp$p_nstagep <- sapply(pp$p_nstagep, as.character)
pp$s_nstagep <- sapply(pp$s_nstagep, as.character)


# Drop unused levels throughout df
pp <- droplevels(pp)

# Set the BLANK fields to NA
# Parser Gleason Tertiary
pp$p_gtertp[pp$p_gtertp ==""] <- NA

# Parser Gleason RAW
pp$p_glsn_raw[pp$p_glsn_raw ==""] <- NA

# Parser Gleason RAW
pp$p_pathmgnpos_raw[pp$p_pathmgnpos_raw ==""] <- NA


# SDE Gleason RAW
pp$s_glsn_raw[pp$s_glsn_raw ==""] <- NA

# Mark Fields as complete
pp$u_glcomplete <- ifelse(!is.na(pp$u_gprimp) & !is.na(pp$u_gsecondp), T, F)
pp$s_glcomplete <- ifelse(!is.na(pp$s_gprimp) & !is.na(pp$s_gsecondp), T, F)
pp$p_glcomplete <- ifelse(!is.na(pp$p_gprimp) & !is.na(pp$p_gsecondp), T, F)
pp$all_glcomplete <- ifelse(pp$u_glcomplete & pp$s_glcomplete & pp$p_glcomplete, T, F)

# Create combined Gleason score Fields for easier comparisons
pp$u_glcombined = paste0(pp$u_gprimp,"+",pp$u_gsecondp)
pp$s_glcombined = paste0(pp$s_gprimp,"+",pp$s_gsecondp)
pp$p_glcombined = paste0(pp$p_gprimp,"+",pp$p_gsecondp)

# Remove NA fields that are generated when its missing for both
pp$u_glcombined[pp$u_glcombined == "NA+NA"] <- NA
pp$s_glcombined[pp$s_glcombined == "NA+NA"] <- NA
pp$p_glcombined[pp$p_glcombined == "NA+NA"] <- NA

#########################
# need to identify where the parser had RAW text, but failed to pull a value. this isnt a NA
# Because NA in this dataset implies there was no attempt at parsing/abstraction/SDE
# its a MISSING value


# Export Data
save(pp, v, file="data/tidy/pp.rda")
