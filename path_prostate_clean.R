# Load relevant libraries
# Data Cleaning
library(stringr)

# Import data from Coop
pp <- read.csv("data/raw/results-201710150-deidentified.csv",
              header = T,
              stringsAsFactors = F,
              na.strings = c("NULL"))

colnames(pp) <- str_to_lower(colnames(pp))

# Drop PatID (its blank, but just to be sure in future data extracts)
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
lapply(pp[,], is.numeric)

# Convert 0/1 fields to true/false
pp[,c(v$marginfields, v$svi_pos, v$ece_pos)] <- pp[,c(v$marginfields, v$svi_pos, v$ece_pos)] == 1

# Certain UODB fields, if blank, need to be set to null
pp$u_nstagep[pp$u_nstagep==""] <- NA

# Do we need to populate u_pathnodes_status field?



# Drop unused levels throughout df
pp <- droplevels(pp)

# Export Data
save(pp, v, file="data/tidy/pp.rda")
