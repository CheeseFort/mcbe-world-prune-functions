################################################################################
# Author: Francis White, mountainman395 on discord
# https://github.com/CheeseFort/mcbe-world-prune-functions
# Credit: Breeze1074, RufusAtticus, and NavDaz76
# 
# Example usage of world pruning functions
################################################################################

# Load dependencies and functions
setwd("path/to/cloned/mcbe-world-prune-functions")
remotes::install_github("reedacartwright/rbedrock") # update rbedrock from github
source("./PruneWorldFunctions.R")
source("./BoundariesFromExceltoCSV.R")

# Setup world path and safe coordinates to teleport players in pruned chunks
dbpath <- "./mcbe world folder"
safe_coords <- tibble(
  x = 0,
  y = 70,
  z = 0,
  dimension_id = 0
)

# Can use a csv file directly or generate one from an Excel file
excel_file <- "./example world boundaries.xlsx"
world_boundaries_file <- "./example world boundaries.csv"
boundariesFromExceltoCSV(excel_file, world_boundaries_file, data_start_row=6)

################################################################################  
#Delete all chunks and data outside the chunk boundaries, plus garbage data
fullPruneWorld(dbpath, safe_coords, world_boundaries_file, simulate_pruning = F,
               inverse_prune = F, no_plot = F, skip_backup = F)

# Step 0: create a backup of the world
# Step 1: plot chunk data prior to pruning
# Step 2: move players in chunks that will be pruned
# Step 3: prune chunk data
# Step 4: prune entity data
# Step 5: prune portal data
# Step 6: prune lodestone data
# Step 7: plot chunk data post pruning
# Step 8: create a zipped (.mcworld) world copy
