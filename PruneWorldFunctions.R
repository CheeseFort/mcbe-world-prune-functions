################################################################################
# Author: Francis White, mountainman395 on discord
# https://github.com/CheeseFort/mcbe-world-prune-functions
# Forked from: https://gist.github.com/Breeze1074/2bc6e71f5f5bad974a425c5a4494504f
# Credit: Breeze1074, RufusAtticus, and NavDaz76
# Last tested on Minecraft 1.26.0.2.0 and rbedrock 0.4.2.9000
# 
# Functions for pruning a world
################################################################################
require(tidyverse)
require(rbedrock)
require(fs)


# For the purposes of this script, a "chunk id" is a string of the form 
# "x_z_dim"
# where x and z are the x and z chunk coords and dim = 0 for Overworld,
# dim = 1 for Nether, and dim = 2 for The End

chunksFromBoundaryFile <- function(world_boundaries_csv, inverse_prune = F) {
  # Convert block coordinates into chunk bounds
  chunk_boundaries <- read_csv(world_boundaries_csv, show_col_types = F) %>% 
    select(
      "dimension" = 1,
      "minX" = 2,
      "maxX" = 3,
      "minZ" = 4,
      "maxZ" = 5,
      "dimension_id" = 6,
      "description" = 7
    ) %>%
    mutate(
      across(2:5, ~ .x %/% 16)
    )
  
  # shrink the area by 1 in each direction if it's being deleted (not kept)
  if (inverse_prune == TRUE) {
    chunk_boundaries <- chunk_boundaries %>% 
      mutate(
        maxX = maxX - 1,
        maxZ = maxZ - 1,
        minX = minX + 1,
        minZ = minZ + 1
      ) %>% filter(minX <= maxX, minZ <= maxZ)
  }
  
  # Print bounds
  chunk_boundaries %>% knitr::kable(format = "simple") %>% print(show_col_types = FALSE)
  # Convert chunk_boundaries in a list of list for `pmap` to use
  .boundariesList <- list(
    minX = as.list(pull(chunk_boundaries, minX)),
    maxX = as.list(pull(chunk_boundaries, maxX)),
    minZ = as.list(pull(chunk_boundaries, minZ)),
    maxZ = as.list(pull(chunk_boundaries, maxZ)),
    dimension_id = as.list(pull(chunk_boundaries, dimension_id))
  )
  # Expand bounds into list of all chunk IDs contained in the bounds
  chunks_to_keep <-
    pmap(.boundariesList,
         function(minX, maxX, minZ, maxZ, dimension_id)
           expand_grid(
             xBin = minX:maxX,
             zBin = minZ:maxZ,
             dimension_id = dimension_id
           )
    ) %>%
    bind_rows() %>% 
    unite(id, xBin, zBin, dimension_id) %>% 
    pull(id) %>% 
    unique()

  return(chunks_to_keep)
}



plotChunks <- function(dbpath, filename = NULL, time = Sys.time()) {
  # Load keys from mcworld
  db <- bedrockdb(dbpath)
  keys <- get_keys()
  close(db, compact = FALSE)
  # Extract chunk data from chunk: keys
  chunk_data <- keys %>% str_subset("^chunk:") %>%  parse_chunk_keys()

  chunk_plot_format <- chunk_data %>% 
    transmute(
      x = x,
      z = z,
      dimension = recode(dimension,
                         `0` = "0_Overworld",
                         `1` = "1_Nether",
                         `2` = "2_End")
    ) %>% unique()
  
  # Count keys to be plotted
  num_chunks <- nrow(chunk_plot_format)
  
  # Calculate total world file size
  world_file_size <- dir_info(dbpath, all = TRUE, recurse = TRUE) %>%
    pull(size) %>% 
    as_fs_bytes() %>% 
    sum()
  
  level_name <- read_leveldat(dbpath)$LevelName[]

  # Plot keys
  plot <- chunk_plot_format %>% 
    # Theming
    ggplot(aes(
      xmin = 16 * x,
      ymin = 16 * z,
      xmax = 16 * (x + 1),
      ymax = 16 * (z + 1),
      fill = dimension
    )) +
    geom_rect() +
    theme_minimal() +
    scale_fill_manual(
      values = c(
        "0_Overworld" = "green",
        "1_Nether"    = "red",
        "2_End"       = "purple"
      )
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      panel.grid.major = element_line(linewidth = 0.5),
      panel.grid.minor = element_line(linewidth = 0.5),
      aspect.ratio = 1
    ) +
    # Axes and labels
    guides(fill = "none") +
    scale_x_continuous("x") +
    scale_y_reverse("z") + 
    facet_wrap( ~ dimension, scales = "free") +
    labs(
      title = paste(level_name, format(time, "%B %d, %Y")),
      caption = paste(prettyNum(num_chunks, big.mark = ","), "chunks @", world_file_size)
    )
  print(plot)
  
  cat("Finished plotting", prettyNum(num_chunks, big.mark = ","), "chunks\n")
  
  if (!is_null(filename)) {
    ggsave(
      filename,
      bg = "white",
      height = 4.5,
      width = 10
    )
    cat("Plot saved to \"", filename, "\"\n", sep = "")
  }
}



movePlayers <- function(dbpath, safe_coords, chunks_to_keep, simulate_pruning = F, inverse_prune = F) {
  # check safe coords provided
  if (!inherits(safe_coords, "tbl_df") ||
      nrow(safe_coords) != 1 ||
      !all(c("x", "y", "z", "dimension_id") %in% names(safe_coords)) ||
      !all(vapply(safe_coords[c("x", "y", "z", "dimension_id")], is.numeric, logical(1))) ||
      any(is.na(safe_coords[c("x", "y", "z", "dimension_id")]))
  ) stop("safe_coords must be a one-row tibble with numeric, non-NA columns: x, y, z, dimension_id.")
  
  db <- bedrockdb(dbpath)
  on.exit(close(db, compact = FALSE))
  player_keys <- get_keys(prefix = "plain:player_server_")
  player_data <- get_nbt_data(player_keys)
  
  # extract player coordinates and dimension
  player_locations <- player_data %>% 
    map(~ c(.x$Pos, .x$DimensionId) %>% 
          set_names("x", "y", "z", "dimension") %>% 
          as_tibble_row()
    ) %>% 
    list_rbind(names_to = "Player") %>% 
      mutate(
        chunk_x = x %/% 16,
        chunk_z = z %/% 16,
        dim = dimension
      ) %>% 
    unite(chunk_id, chunk_x, chunk_z, dim)
  
  qty_players <- player_locations %>% pull(Player) %>% length()
  cat(prettyNum(qty_players, big.mark = ","), "player keys in the world\n")
  
  # get keys of players to move
  if (inverse_prune == FALSE) {
    players_to_move <- player_locations %>% 
      filter(!(chunk_id %in% chunks_to_keep)) %>% 
      pull(Player)
  } else {
    players_to_move <- player_locations %>% 
      filter(chunk_id %in% chunks_to_keep) %>% 
      pull(Player)
  }
  
  qty_to_move <- length(players_to_move)
  
  if (qty_to_move > 0) {
    cat(prettyNum(qty_to_move, big.mark = ","), "players to move\n")
    # replace position data in initial extracted NBT data
    moved_player_data <- player_data[players_to_move] %>% 
      map(function(x) {
        x$Pos[[1]] <- safe_coords$x
        x$Pos[[2]] <- safe_coords$y + 0.1
        x$Pos[[3]] <- safe_coords$z
        x$DimensionId[] <- safe_coords$dimension_id
        x
      })

    if (simulate_pruning == FALSE) {
      # write to world
      put_nbt_data(values=moved_player_data, keys=players_to_move)
      cat("Finished moving", prettyNum(qty_to_move, big.mark = ","), "players\n")
    }
    
  } else cat("No players require moving\n")
}



pruneChunkData <- function(dbpath, chunks_to_keep, simulate_pruning = F, inverse_prune = F) {
  db <- bedrockdb(dbpath)
  on.exit(close(db, compact = FALSE))
  keys <- get_keys()
  
  chunk_data <- keys %>% 
    str_subset("^chunk:") %>%  
    parse_chunk_keys() %>% 
    unite(id, x, z, dimension, remove = FALSE)
  
  # filter out keys to delete
  if (inverse_prune == FALSE) {
    chunk_delkeys <- chunk_data %>% 
      filter(!(id %in% chunks_to_keep)) %>% 
      pull(key)
  } else {
    chunk_delkeys <- chunk_data %>% 
      filter(id %in% chunks_to_keep) %>% 
      pull(key)
  }
  
  # remove pending tick keys with excessive data (more than 200 pending ticks in a chunk)
  max_pending_ticks <- 200
  
  pending_ticks_remove <- chunk_data %>% 
    filter((tag == "PendingTicks") & !(key %in% chunk_delkeys)) %>% 
    pull(key) %>%  
    get_pending_ticks_data() %>%  
    keep(~ .x[[1]]$tickList[] %>% length() >= max_pending_ticks) %>% 
    names()
  
  # remove deprecated entity data in chunk keys
  legacy_entity_keys <- chunk_data %>% 
    filter((tag == "Entity") & !(key %in% chunk_delkeys)) %>% 
    pull(key)
  
  delkeys <- c(chunk_delkeys,
               pending_ticks_remove,
               legacy_entity_keys) %>% unique()
  
  qtyDelkeys <- length(delkeys)
  
  if (qtyDelkeys > 0) {
    delkeys_summary <- tibble(
      `Chunk keys` =              chunk_delkeys %>% length() %>% prettyNum(big.mark = ","),
      `Large pending tick keys` = pending_ticks_remove %>% length() %>% prettyNum(big.mark = ","),
      `Legacy entity` =           legacy_entity_keys %>% length() %>% prettyNum(big.mark = ","),
      Total =                     qtyDelkeys %>% prettyNum(big.mark = ","))
    
    cat("Chunk keys to prune:")
    delkeys_summary %>% knitr::kable(format = "simple") %>% print(show_col_types = FALSE)
    
    if (simulate_pruning == FALSE) {
      delete_values(delkeys, report = TRUE) -> delete_report
      num_deleted <- delete_report %>% keep(~ .x == TRUE) %>% length()
      cat("Finished pruning", prettyNum(num_deleted, big.mark = ","), "chunk keys\n")
      
      if (num_deleted != qtyDelkeys) {
        cat("Failed to prune", prettyNum(qtyDelkeys-num_deleted, big.mark = ","), "chunk keys\n")
        
        failed_filename = "failed chunk keys.txt"
        failed_keys <- delkeys %>% 
          bind_cols(delete_report, .name_repair = "unique_quiet") %>% 
          filter(...2 == FALSE) %>% 
          pull(...1) %>% tibble()
        
        write_delim(failed_keys, failed_filename, col_names = FALSE)
        cat("Wrote list of failed keys to \"", failed_filename, "\"\n", sep = "")
      }
    }
  } else cat("No chunk keys require pruning\n")
}



pruneEntityData <- function(dbpath, chunks_to_keep, simulate_pruning = F, inverse_prune = F) {
  db <- bedrockdb(dbpath)
  on.exit(close(db, compact = FALSE))
  keys <- get_keys()
  
  # useless entities that get stuck in unloaded chunks
  garbage_entities <- c("minecraft:wither_skull_dangerous", 
                        "minecraft:wither_skull",
                        "minecraft:shulker_bullet",
                        "minecraft:fireball",
                        "minecraft:small_fireball",
                        "minecraft:dragon_fireball",
                        "minecraft:fireworks_rocket",
                        "minecraft:wind_charge_projectile")
  
  # actor keys store entity info
  actor_keys <- str_subset(keys, "^actor:")
  # actor digest keys store a list of all entities in a chunk
  digest_keys <- str_subset(keys, "^acdig:")
  
  actor_keys_in_digest <- digest_keys %>% 
    get_acdig_data() %>% 
    unlist(use.names = FALSE) %>% 
    unique()
  
  # if an entity is in the actor keys but not the digest keys or vise versa
  orphan_actor_keys <- setdiff(actor_keys, actor_keys_in_digest) %>% unique()
  
  # parse digest keys similar to parse_chunk_keys()
  digest_data <- digest_keys %>% as_tibble() %>% set_names("key") %>% 
    bind_cols(
      separate(as_tibble(digest_keys),
               col = 1,
               into = c(NA, "x", "z", "dimension"),
               sep = ":")
    ) %>% 
    unite(col = "chunk_id", x:dimension, remove = FALSE)
  
  # filter out digest keys to delete
  if (inverse_prune == FALSE) {
    digest_keys_remove <- digest_data %>% 
      filter(!(chunk_id %in% chunks_to_keep)) %>% 
      pull(key)
  } else {
    digest_keys_remove <- digest_data %>% 
      filter(chunk_id %in% chunks_to_keep) %>% 
      pull(key)
  }
  
  # remove actors that are referenced in the digest keys to remove
  actor_keys_remove <- digest_keys_remove %>% 
    get_acdig_data() %>% 
    unlist(use.names = FALSE) %>% 
    unique()
  
  garbage_entity_keys <- actor_keys %>% 
    keep(~ !(.x %in% actor_keys_remove)) %>% 
    get_nbt_data() %>% 
    keep(~ .x$identifier[] %in% garbage_entities) %>% 
    names()
  
  delkeys <- c(digest_keys_remove,
               actor_keys_remove,
               orphan_actor_keys,
               garbage_entity_keys) %>% unique()
  
  qtyDelkeys <- length(delkeys)
  
  if (qtyDelkeys > 0) {
    delkeys_summary <- tibble(
      Digest =             digest_keys_remove %>% length() %>% prettyNum(big.mark = ","),
      Actor =              actor_keys_remove %>% length() %>% prettyNum(big.mark = ","),
      `Orphaned actor` =   orphan_actor_keys %>% length() %>% prettyNum(big.mark = ","),
      `Garbage entities` = garbage_entity_keys %>% length() %>% prettyNum(big.mark = ","),
      Total =              qtyDelkeys %>% prettyNum(big.mark = ","))
    
    cat("Actor and digest keys to prune:")
    delkeys_summary %>% knitr::kable(format = "simple") %>% print(show_col_types = FALSE)
    if (simulate_pruning == FALSE) {
      delete_values(delkeys, report = TRUE) -> delete_report
      num_deleted <- delete_report %>% keep(~ .x == TRUE) %>% length()
      cat("Finished pruning", prettyNum(num_deleted, big.mark = ","), "actor and digest keys\n")
      
      if (num_deleted != qtyDelkeys) {
        cat("Failed to prune", prettyNum(qtyDelkeys-num_deleted, big.mark = ","), "actor and digest keys\n")
        
        failed_filename = "failed actor and digest keys.txt"
        failed_keys <- delkeys %>% 
          bind_cols(delete_report, .name_repair = "unique_quiet") %>% 
          filter(...2 == FALSE) %>% 
          pull(...1) %>% tibble()
        
        write_delim(failed_keys, failed_filename, col_names = FALSE)
        cat("Wrote list of failed keys to \"", failed_filename, "\"\n", sep = "")
      }
    }
  } else cat("No entity or digest keys require pruning\n")
}



prunePortalData <- function(dbpath, chunks_to_keep, simulate_pruning = F, inverse_prune = F) {
  db <- bedrockdb(dbpath)
  on.exit(close(db, compact = FALSE))
  portal_data <- get_nbt_value("plain:portals")
  
  if (is_null(portal_data)) {
    cat("No portals data found in world\n")
    return(NA)
  }
  
  portal_info <- portal_data$data$PortalRecords %>% 
    unnbt() %>% 
    bind_rows() %>%
    rownames_to_column(var = "index") %>% 
    mutate(
      chunk_x = TpX %/% 16,
      chunk_z = TpZ %/% 16,
      dim = DimId,
      index = as.numeric(index)
    ) %>%
    unite(portal_id, DimId, TpX, TpY, TpZ, remove = FALSE) %>% 
    unite(chunk_id, chunk_x, chunk_z, dim)
  
  # filter out portals to delete
  if (inverse_prune == FALSE) {
    del_portals <- portal_info %>% 
      filter(!(chunk_id %in% chunks_to_keep)) %>%
      pull(portal_id)
  } else {
    del_portals <- portal_info %>% 
      filter(chunk_id %in% chunks_to_keep) %>%
      pull(portal_id)
  }
  
  qtyPortals <- length(del_portals)
  
  if (qtyPortals > 0) {
    cat(prettyNum(qtyPortals, big.mark = ","), "nether portals to prune\n")
    
    # get the (portal_data) index numbers of the portals to keep
    keep_portal_indices <-
      portal_info %>% 
      filter(!(portal_id %in% del_portals)) %>% 
      pull(index)
    
    # create NBT formatted list of portal records to keep
    portal_records_keep <- list() %>% nbt_compound_list()
    for (i in seq_along(keep_portal_indices)) {
      portal_records_keep[i] <- 
        portal_data$data$PortalRecords[ keep_portal_indices[i] ]
    }
    
    # overwrite portal_data with only the records for the portals to keep
    portal_data$data$PortalRecords <- portal_records_keep
    
    if (simulate_pruning == FALSE) {
      put_nbt_value(portal_data, "plain:portals")
      cat("Finished pruning", prettyNum(qtyPortals, big.mark = ","), "nether portals\n")
    }
    
  } else cat("No portals require pruning\n")
}



pruneLodestoneData <- function(dbpath, chunks_to_keep, simulate_pruning = F, inverse_prune = F) {
  db <- bedrockdb(dbpath)
  on.exit(close(db, compact = FALSE))
  pos_track_keys <- get_keys(prefix = "plain:PosTrackDB-0x")
  
  if (identical(pos_track_keys, character(0))) {
    cat("No lodestone position keys found in world\n")
    return(NA)
  }
  
  # parse position track keys similar to parse_chunk_keys()
  pos_track_data <- get_nbt_data(pos_track_keys) %>% 
    map(~ c(.x$pos, .x$dim) %>% 
          set_names("x", "y", "z", "dimension") %>% 
          as_tibble_row()) %>% 
    list_rbind(names_to = "key") %>% 
    mutate(
      chunk_x = x %/% 16,
      chunk_z = z %/% 16,
      dim = dimension
    ) %>% 
    unite(chunk_id, chunk_x, chunk_z, dim)
  
  # filter out keys to delete
  if (inverse_prune == FALSE) {
    delkeys <- pos_track_data %>% 
      filter(!(chunk_id %in% chunks_to_keep)) %>% 
      pull(key)
  } else{
    delkeys <- pos_track_data %>% 
      filter(chunk_id %in% chunks_to_keep) %>% 
      pull(key)
  }
  
  qtyDelkeys <- length(delkeys)
  
  if (qtyDelkeys > 0) {
    cat(prettyNum(qtyDelkeys, big.mark = ","), "lodestone position keys to prune\n")
    if (simulate_pruning == FALSE) {
      delete_values(delkeys, report = TRUE) -> delete_report
      num_deleted <- delete_report %>% keep(~ .x == TRUE) %>% length()
      cat("Finished pruning", prettyNum(num_deleted, big.mark = ","), "position keys\n")
      
      if (num_deleted != qtyDelkeys) {
        cat("Failed to prune", prettyNum(qtyDelkeys-num_deleted, big.mark = ","), "position keys\n")
        
        failed_filename = "failed lodestone position keys.txt"
        failed_keys <- delkeys %>% 
          bind_cols(delete_report, .name_repair = "unique_quiet") %>% 
          filter(...2 == FALSE) %>% 
          pull(...1) %>% tibble()
        
        write_delim(failed_keys, failed_filename, col_names = FALSE)
        cat("Wrote list of failed keys to \"", failed_filename, "\"\n", sep = "")
      }
    }
  } else cat("No position keys require pruning\n")
}



# if simulate_pruning is set, data will not be deleted from the world files
# if inverse_prune is set, prune inside the boundaries instead of outside
# if no_plot is set, plots of the world will not be computed or saved to disk
# is skip_backup is set, no backup will be made before pruning (useful if you already have a backup)
fullPruneWorld <- function(dbpath, safe_coords, world_boundaries_csv, simulate_pruning = F,
                           inverse_prune = F, no_plot = F, skip_backup = F) {
  cat("\014") # clear screen
  
  start_time <- Sys.time()
  level_name <- read_leveldat(dbpath)$LevelName[]
  
  cat(sep = "",
      "Began pruning \"", level_name, "\" on ", format(start_time, "%b %d, %X"), '\n',
      "Step 0: create a backup of the world\n",
      "Step 1: plot chunk data prior to pruning\n",
      "Step 2: move players in chunks that will be pruned\n",
      "Step 3: prune chunk data\n",
      "Step 4: prune entity data\n",
      "Step 5: prune portal data\n",
      "Step 6: prune lodestone data\n",
      "Step 7: plot chunk data post pruning\n",
      "Step 8: create a zipped (.mcworld) world copy\n"
  )
  
  
  
  if (skip_backup == FALSE) {
    # Make a backup but do not replace the backup if there's already been one today
    cat("\n\nBacking up World\n")
    filename <- str_c(path_sanitize(level_name), "_", format(start_time, "%F"), "_Backup.mcworld", sep = "")
    
    if (file_exists(filename))
      cat("Backup file \"", filename, "\" already exists\n", sep = "")
    
    else {
      export_world(
        id = dbpath,
        file = filename,
        worlds_dir = "./",
        replace = FALSE
      )
      cat("World backed up to \"", filename, "\"\n", sep = "")
    }
  } else cat("\n\nSkipped step 0\n")
  
  
  
  if (no_plot == FALSE) {
    cat("\n\nBeginning step 1: Plot\n")
    
    filename <- str_c(
      path_sanitize(level_name), "_", 
      format(start_time, "%FT%H-%M-%S"), 
      "_BeforePrune.png", sep = ""
    )
    plotChunks(dbpath, filename, start_time)

  } else cat("\n\nSkipped step 1\n")
  
  
  
  cat("\nReading Chunk Boundaries\n")
  chunks_to_keep <- chunksFromBoundaryFile(world_boundaries_csv, inverse_prune)
  
  cat("\n\nBeginning step 2: Move players\n")
  movePlayers(dbpath, safe_coords, chunks_to_keep, simulate_pruning, inverse_prune)
  
  cat("\n\nBeginning step 3: Prune chunks\n")
  pruneChunkData(dbpath, chunks_to_keep, simulate_pruning, inverse_prune)
  
  cat("\n\nBeginning step 4: Prune entites\n")
  pruneEntityData(dbpath, chunks_to_keep, simulate_pruning, inverse_prune)
  
  cat("\n\nBeginning step 5: Prune portals\n")
  prunePortalData(dbpath, chunks_to_keep, simulate_pruning, inverse_prune)
  
  cat("\n\nBeginning step 6: Prune lodestones\n")
  pruneLodestoneData(dbpath, chunks_to_keep, simulate_pruning, inverse_prune)
  
  
  
  # compact world before plotting
  if (simulate_pruning == FALSE) {
    db <- bedrockdb(dbpath)
    close(db, compact = TRUE)
  }
  
  
  
  if (no_plot == FALSE && simulate_pruning == FALSE) {
    cat("\n\nBeginning step 7: Plot\n")
    
    filename <- str_c(
      path_sanitize(level_name), "_", 
      format(start_time, "%FT%H-%M-%S"), 
      "_Pruned.png", sep = ""
    )
    plotChunks(dbpath, filename, start_time)
    
  } else cat(str_c("\n\nSkipped step 7\n"))
  
  
  
  if (simulate_pruning == FALSE) {
    cat("\n\nBeginning step 8: Create .mcworld\n")
    filename <- str_c(path_sanitize(level_name), "_", format(start_time, "%FT%H-%M-%S"), "_Pruned.mcworld", sep = "")
    export_world(
      id = dbpath,
      file = filename,
      worlds_dir = "./",
      replace = TRUE
    )
    cat("Pruned world saved to '", filename, "'\n", sep = "")
  } else cat("\n\nSkipped step 8\n")
  
  cat("Finished on", format(Sys.time(), "%b %d, %X"), "\n")
}


