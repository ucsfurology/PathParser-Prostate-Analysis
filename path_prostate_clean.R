# Load relevant libraries
# Data Cleaning
library(stringr)

# Import data from Coop
pp <- read.csv("data/raw/results-20170710.csv", header = T, stringsAsFactors = T)

colnames(pp) <- str_to_lower(colnames(pp))

# Define useful vectors for field names
v <- list()
v$sources <- c("u", "s", "p")
v$gnames <- c("_gprimp", "_gsecondp", "_gtertp") 
v$gleasonfields <- paste0(rep(sources, each=3), v$gnames)

v$tstagefields <- paste0(v$sources, "_tstagep")
v$nstagefields <- paste0(v$sources, "_nstagep")
v$marginfields <- paste0(v$sources, "_pathmgnpos")
v$node_status <- paste0(v$sources, "_pathnodes_status")
v$node_dissected <- paste0(sources, "_pathnodes_dissected")
v$node_pos <- paste0(sources, "_pathnodes_positive")
v$svi_pos <- paste0(sources, "_pathsvipos")
v$ece_pos <- paste0(sources, "_pathecepos")

# Convert Gleason Score fields to factors
pp[,gleasonfields] <- as.data.frame(lapply(pp[,gleasonfields], as.factor))

# Check if Stage fields are correctly listed as factors
lapply(pp[,c(v$tstagefields, v$nstagefields)], is.factor)

# Check if number of nodes dissected, node positive fields are stored as a number
lapply(pp[,c(v$node_dissected, v$node_pos)], is.numeric)

# Convert 0/1 fields to true/false
pp[,c(v$marginfields, v$node_status, v$svi_pos, v$ece_pos)] <- pp[,c(v$marginfields, v$node_status, v$svi_pos, v$ece_pos)] == 1

# Export Data
save(pp, v, file="data/tidy/pp.rda")
