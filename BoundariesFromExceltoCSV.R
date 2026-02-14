################################################################################
# Author: Francis White, mountainman395 on discord
# https://github.com/CheeseFort/mcbe-world-prune-functions
# Required: tidyverse, readxl
# 
# Reads boundary data from an excel boundaries file and writes it to a csv file
# Example xlsx: https://github.com/CheeseFort/mcbe-world-prune-functions
################################################################################
require(tidyverse)
require(readxl)

boundariesFromExceltoCSV <- function(excel_file, csv_name, data_start_row) {
  # Get raw data from excel file
  raw_data <- read_excel(
    excel_file,
    range = cell_rows(c(data_start_row, NA)),
    col_names = FALSE,
    .name_repair = "unique_quiet"
  )
  # Format Overworld data
  Overworld <- raw_data %>% 
    select(1:5) %>% 
    select(x1 = 1, z1 = 2, x2 = 3, z2 = 4, description = 5) %>% 
    filter(!if_all(1:5, is.na)) %>% 
    transmute(
      dimension = "Overworld",
      minX = pmin(x1, x2),
      maxX = pmax(x1, x2),
      minZ = pmin(z1, z2),
      maxZ = pmax(z1, z2),
      dimension_id = 0,
      description = description
    )
  # Format Nether data
  Nether <- raw_data %>% 
    select(6:10) %>% 
    select(x1 = 1, z1 = 2, x2 = 3, z2 = 4, description = 5) %>% 
    filter(!if_all(1:5, is.na)) %>% 
    transmute(
      dimension = "Nether",
      minX = pmin(x1, x2),
      maxX = pmax(x1, x2),
      minZ = pmin(z1, z2),
      maxZ = pmax(z1, z2),
      dimension_id = 1,
      description = description
    )
  # Format End data
  End <- raw_data %>% 
    select(11:15) %>% 
    select(x1 = 1, z1 = 2, x2 = 3, z2 = 4, description = 5) %>% 
    filter(!if_all(1:5, is.na)) %>% 
    transmute(
      dimension = "End",
      minX = pmin(x1, x2),
      maxX = pmax(x1, x2),
      minZ = pmin(z1, z2),
      maxZ = pmax(z1, z2),
      dimension_id = 2,
      description = description
    )
  # Concatenate data
  csv_data <- bind_rows(Overworld, Nether, End)
  # Write csv file
  write_csv(csv_data, csv_name)
  # return
  csv_data
}
