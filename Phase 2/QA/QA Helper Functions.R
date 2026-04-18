### Helper functions for QA

## Function to check for item pattern in data across waves
# - With option to restrict to columns of a given type
check_item_pattern <- function(dat_ls_cols, pattern, col_type = "all") {
  if (col_type == "all") {
    cat("All columns:\n\n")
  } else {
    cat("Columns of type '", col_type, "':\n\n", sep = "")
  }
  
  lapply(dat_ls_cols, function(dat_cols) {
    if (col_type == "all") {
      cols <- unlist(dat_cols, use.names = FALSE)
    } else {
      cols <- dat_cols[[col_type]]
    }
    
    cols[grepl(pattern, cols)]
  })
}


## Function to check label for item pattern in data across waves
check_item_pattern_label <- function(dat_ls, pattern) {
  lapply(dat_ls, function(dat) {
    target_cols <- names(dat)[grepl(pattern, names(dat))]
    
    sapply(target_cols, function(target_col) {
      attr(dat[[target_col]], "label")
    }, USE.NAMES = FALSE)
  })
}


## Function to compute number of waves in which each stem is present
compute_stem_wave_counts <- function(meas_item_cols_prefixes_stems) {
  waves <- names(meas_item_cols_prefixes_stems)
  n_total_waves <- length(waves)
  stems_by_wave <- lapply(meas_item_cols_prefixes_stems, \(x) x$stems)
  stems <- unique(unlist(stems_by_wave))
  
  stem_wave_count_dfs <- lapply(stems, function(stem) {
    present_waves <- waves[sapply(stems_by_wave, \(s) stem %in% s)]
    absent_waves <- setdiff(waves, present_waves)
    
    data.frame(
      stem = stem,
      n_present_waves = length(present_waves),
      n_total_waves = n_total_waves,
      present_waves = paste(present_waves, collapse = ", "),
      absent_waves = paste(absent_waves, collapse = ", ")
    )
  })
  names(stem_wave_count_dfs) <- stems
  
  stem_wave_counts_df <- bind_rows(stem_wave_count_dfs)
  stem_wave_counts_df <- stem_wave_counts_df[order(stem_wave_counts_df$stem), ]
  row.names(stem_wave_counts_df) <- NULL
  
  return(stem_wave_counts_df)
}


## Function to create data frame of repeated-measure item stems with different labels across waves
# - Note: Whitespace differences can't be reliably detected by viewing data frame
create_diff_repeated_meas_item_lbl_df <- function(meas_item_col_lbls_clss, stem_wave_counts_df) {
  # Get stems for repeated-measure items
  repeated_stems <- stem_wave_counts_df$stem[stem_wave_counts_df$n_present > 1]
  
  # Create data frame of labels at each wave for repeated-measure items
  wave_names <- names(meas_item_col_lbls_clss)
  
  lbl_df <- data.frame(stem = repeated_stems)
  
  for (wave in wave_names) {
    wave_lbls <- meas_item_col_lbls_clss[[wave]]$lbls
    wave_stems <- names(wave_lbls)
    
    wave_repeated_stems <- wave_stems[wave_stems %in% repeated_stems]
    wave_repeated_stems_lbls <- wave_lbls[wave_repeated_stems]
    
    wave_lbl_df <- data.frame(stem = wave_repeated_stems)
    wave_lbl_df[[wave]] <- wave_repeated_stems_lbls
    
    lbl_df <- merge(lbl_df, wave_lbl_df, by = "stem", all.x = TRUE)
  }
  
  # Exclude labels for 9 BADS-SF items assessed only at "yi" but that have the
  # same stems as BADS items assessed at other time points
  if ("yi_raw" %in% names(lbl_df)) lbl_df$yi_raw[lbl_df$stem %in% bads_sf_stems] <- NA
  
  # Define helper to get vector (named by wave) of non-NA labels for given row
  get_row_lbls <- function(row) {
    # Get list (named by wave) of labels (named by item) for given row
    lbls_ls <- row[wave_names]
    
    # Return vector (named by wave) of non-NA labels (stripping item names)
    lbls_vec <- sapply(lbls_ls, \(x) unname(x))
    lbls_vec <- lbls_vec[!is.na(lbls_vec)]
  }
  
  # Filter data frame to rows where non-NA labels differ
  diff_lbl_idx <- apply(lbl_df, 1, \(row) {
    lbls <- get_row_lbls(row)  # Helper above
    
    length(unique(lbls)) > 1
  })
  
  diff_lbl_df <- lbl_df[diff_lbl_idx, ]
  
  # For each stem, get waves with outlying labels
  diff_lbl_df$outlying_waves <- apply(diff_lbl_df, 1, \(row) {
    lbls <- get_row_lbls(row)  # Helper above
    
    tbl <- table(lbls)
    
    if (all(tbl == 1)) {
      return("All waves differ")
    } else {
      outlying_lbls <- names(tbl)[tbl < max(tbl)]
      outlying_waves <- names(lbls)[lbls %in% outlying_lbls]
      
      return(paste(outlying_waves, collapse = ", "))
    }
  })
  
  return(diff_lbl_df)
}


## Function to inspect labels for items with different labels across waves
# - Note: Return list given that labels are hard to inspect in data frame
get_diff_lbls <- function(diff_lbl_df, stems_with_diff_lbls) {
  diff_lbls <- lapply(stems_with_diff_lbls, \(stem) {
    row <- diff_lbl_df[diff_lbl_df$stem == stem, ]
    
    start_cols <- c("stem", "outlying_waves")
    row <- row[c(start_cols, setdiff(names(row), start_cols))]
    
    # Given that each column is unnecessarily a one-element list (named by the row's 
    # stem), extract the element so that each column is a character (named by the full
    # item name at a given wave) instead
    row_of_named_chr <- lapply(row, \(col_ls) col_ls[[1]])
  })
  names(diff_lbls) <- stems_with_diff_lbls
  
  return(diff_lbls)
}


## Function to get repeated-measure item stems with different classes across waves
get_stems_diff_clss <- function(meas_item_col_lbs_clss, stem_wave_counts_df) {
  # Get stems for repeated-measure items
  repeated_stems <- stem_wave_counts_df$stem[stem_wave_counts_df$n_present > 1]
  
  # Find measure item stems that have different classes across waves
  stems_diff_clss <- character()
  
  for (stem in repeated_stems) {
    clss <- unlist(lapply(meas_item_col_lbs_clss, \(wave) wave$clss[[stem]]))
    
    # Exclude class at "yi" for 9 BADS-SF items assessed only at "yi" but that have
    # the same stems as BADS items assessed at other time points
    if (stem %in% bads_sf_stems) clss[["yi_raw"]] <- NA
    
    clss <- clss[!is.na(clss)]
    
    if (length(unique(clss)) > 1) stems_diff_clss <- c(stems_diff_clss, stem)
  }
  
  return(stems_diff_clss)
}


## Function to inspect classes for item stems across waves
get_clss <- function(col_lbls_clss, stems) {
  clss <- lapply(stems, \(stem) {
    unlist(lapply(col_lbls_clss, \(wave) wave$clss[[stem]]))
  })
  names(clss) <- stems
  
  return(clss)
}