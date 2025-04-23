## Helper functions for Qualtrics data cleaning

# New syntax to paste strings together
`%+%` <- paste0

# Function to drop invalid responses
remove_invalid_responses <- function(data, id) {
  
  # Taking the data, filter out cases where ID variable is missing (unclear why) or in invalid_ids
  out <- data %>%
    filter(
      !is.na({{id}}),
      !{{id}} %in% invalid_ids
    )
  
  # Get list of all IDs in the original data
  original_ids <- data %>%
    pull({{id}})
  
  # Get list of all IDs in the filtered data
  filtered_ids <- out %>%
    pull({{id}})
  
  # Get the difference between these lists
  dropped_ids <- setdiff(original_ids, filtered_ids)
  
  # Give a message detailing which IDs were removed
  message("Dropped " %+% length(dropped_ids) %+% " IDs: " %+% paste0(dropped_ids, collapse = ", "))
  
  # Return the filtered dataset
  return(out)
  
}

# Function to fill LifePak ID across duplicates (there is at least one case where a 
# respondent provided their LifePak ID only in a duplicated, noncomplete response)
fill_lifepak_id <- function(data, id) {
  
  ## Check that each LSMH ID has <= 1 LifePak ID
  # Convert "id" from symbol to character for use outside tidyverse
  id_as_char <- as.character(ensym(id))
  
  # Find LifePak ID column and throw error if > 1 exists
  lifepak_id <- grep("LifePak ID", names(data), value = TRUE)
  if (length(lifepak_id) > 1) stop("Data has > 1 column name containing 'LifePak ID'")
  
  # Compute number of unique, non-NA LifePak IDs for each LSMH ID
  n_unique_lifepak_ids <- tapply(data[[lifepak_id]], data[[id_as_char]], function(lifepak_ids) {
    sum(!is.na(unique(lifepak_ids)))
  })
  
  # Throw error if LSMH IDs and LifePak IDs are one to many
  if (any(n_unique_lifepak_ids) > 1) {
    stop("LSMH IDs and LifePak IDs are one to many (resolve before filling LifePak IDs across duplicates)")
  }
  
  ## Fill LifePak ID across duplicates
  # Taking the data...
  data %>%
    # ... group by ID ...
    group_by({{id}}) %>%
    # ... then, fill LifePak ID down-up across participant responses...
    fill(all_of(lifepak_id), .direction = "downup") %>%
    ungroup() %>%
    return()
  
}

# Function to compute indicator of baseline survey completion in assessment window
mark_b_done_in_ax_window <- function(data, id_as_char, ema_notif_dates) {
  
  # Add EMA notification dates to data
  names(ema_notif_dates)[names(ema_notif_dates) == "lsmh_id"] <- id_as_char
  
  data <- data %>%
    left_join(ema_notif_dates, by = id_as_char, relationship = "many-to-one")
  
  # Compute indicator of survey completion before first EMA notification
    # Note: Given that "EndDate" and "first_ema_notif_date" are in different time
    # zones ("America/Denver" for Phase I vs. participants' local times stored as 
    # UTC, respectively), this comparison is approximate. To rule out the role of
    # time zone differences, derive actual time zones for "first_ema_notif_date"
    # from LifePak GPS data (although GPS data are missing for some observations)
  data$in_window_b <- NA
  data$in_window_b <- ifelse(as_date(data$EndDate) < data$first_ema_notif_date, TRUE, FALSE)
    
  # Throw warning if any surveys were not completed in this window (in which case 
  # further analysis to rule out role of differing time zones is warranted)
  if (any(data$in_window_b == FALSE)) {
    warning("Not all baseline surveys are in window. Rule out role of differing time zones.")
  }

  return(data)
  
}

# Function to compute indicators of follow-up survey completion in assessment window
mark_3m_done_in_ax_window <- function(data, id_as_char, ax_windows) {
  
  # Add assessment window dates to data
  names(ax_windows)[names(ax_windows) == "lsmh_id"] <- id_as_char
  
  data <- data %>%
    left_join(ax_windows, by = id_as_char, relationship = "many-to-one")
  
  # Compute indicator of survey completion in assessment window
  data$in_window_3m_v5 <- NA
  data$in_window_3m_v5 <- ifelse(as_date(data$EndDate) >= data$start_window_3m_v5 & 
                                   as_date(data$EndDate) <= data$end_window_3m_v5, TRUE, FALSE)
  data$in_window_3m_v6 <- NA
  data$in_window_3m_v6 <- ifelse(as_date(data$EndDate) >= data$start_window_3m_v6 & 
                                   as_date(data$EndDate) <= data$end_window_3m_v6, TRUE, FALSE)
  
  return(data)
  
}

# Function to compute item completion rate
compute_item_completion_rate <- function(data, survey_prefix) {
  
  # Define columns to ignore when computing completion rate
    # Columns with click and time on page information
  time_cols <- names(data)[grepl("time", names(data)) & grepl("Click|Submit", names(data))]
  
    # Columns with specified responses for response options of "Other" (or similar)
  text_cols <- names(data)[grepl("_TEXT", names(data))]
  
    # Columns for metadata
  meta_cols <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress", 
                 "Duration (in seconds)", "Finished", "RecordedDate", "ResponseId", 
                 "RecipientLastName", "RecipientFirstName", "RecipientEmail", 
                 "ExternalReference", "LocationLatitude", "LocationLongitude", 
                 "DistributionChannel", "UserLanguage")
  
  y_meta_cols <- c("status", "SC0")
  
  if (survey_prefix == "yb") {
    meta_cols <- c(meta_cols, y_meta_cols,
                   "administration", "assent_signature", "yb_lsmh_id", "yb_lsmh_id_ validate", 
                   "yb_phone", "yb_phone_validate", "yb_LifePak ID", "yb_LifePak ID Verify", 
                   "yb_end", "yb_end_3", "password_child", "yb_interview")
  } else if (survey_prefix == "y3m") {
    meta_cols <- c(meta_cols, y_meta_cols,
                   "y3m_lsmh_id", "y3_lsmh_id_ validate", "y3_childname", "y3m_chrome_browser")
  } else if (survey_prefix == "pb") {
    meta_cols <- c(meta_cols,
                   "administration", "consent_signature", "pb_lsmh_id", "pb_lsmh_id_validate", 
                   "pb_child_name", "pb_date", "pb_address", "pb_homephone",
                   "pb_parentcell", "pb_childcell", "pb_workphone", "pb_parentemail", 
                   "pb_childemail", "password_parent", "pb_interview")
  } else if (survey_prefix == "p3m") {
    meta_cols <- c(meta_cols,
                   "p3m_lsmh_id", "p3m_lsmh_id_validate", 
                   "p3m_child_name", "p3m_date", "p3m_address", "p3m_homephone", 
                   "p3m_parentcell", "p3m_childcell", "p3m_workphone", "p3m_parentemail", 
                   "p3m_childemail", "p3m_wrapup_optin")
  }
  
  ignore_cols <- c(meta_cols, time_cols, text_cols)
  
  # Compute completion rate
  item_cols <- names(data)[!(names(data) %in% ignore_cols)]
  
  data$item_completion_rate <- rowMeans(!is.na(data[, item_cols]))
  
  # Print and log items used to compute completion rate in list stored in global environment
  cat("'item_completion_rate' for '", survey_prefix, "' survey is based on these items:\n\n", sep = "")
  print(item_cols)
  
  log$item_completion_rate[[survey_prefix]]$items   <<- item_cols
  log$item_completion_rate[[survey_prefix]]$n_items <<- length(item_cols)
  
  return(data)
  
}

# Function to identify duplicates
identify_duplicates <- function(data, id) {
  
  # Taking the data...
  out <- data %>%
    # ... filter out cases where the ID is missing or in invalid_ids...
    filter(
      !is.na({{id}}),
      !{{id}} %in% invalid_ids
    ) %>%
    # ... then, grouping by the ID variable...
    group_by({{id}}) %>%
    # ... count the total number of rows and the number of rows with completed responses.
    summarize(
      total = n(),
      complete = sum(Finished)
    ) %>%
    # Finally, arrange the dataset such that duplicates are at the top
    arrange(desc(complete), desc(total))
  
  # Print a message about how many rows were duplicated
  ids <- nrow(out)
  duplicates <- sum(out$total > 1)
  completed_duplicates <- sum(out$complete > 1)
  
  message("Out of " %+% ids %+% " IDs, " %+% duplicates %+% " had multiple responses, while " %+% completed_duplicates %+% " had multiple completed responses.\n" %+%
            "(Note: 'complete' only means clicked through survey, not completed all items.)")
  
  # Return the summary table with duplicated rows at the top
  return(out)
  
}

# Function to deduplicate datasets, keeping first (most) complete response
remove_duplicates <- function(data, id) {
  
  # Taking the data...
  data %>%
    # ... group by ID ...
    group_by({{id}}) %>%
    # ... by ID, arrange first by item_completion_rate (putting more completed 
    # responses at top), then by EndDate (putting first/oldest responses at top)...
    arrange(
      desc(item_completion_rate),
      EndDate
    ) %>%
    # ... finally, take only the top response
    slice_head(n = 1) %>%
    ungroup() %>%
    return()
  
}

# Function to return items from the codebook file, given some criteria
get_items <- function(.prefix, .measure, .subscale) {
  
  # Confirm provided measure and subscale are in the codebook
  if(!missing(.measure)) {
    
    if(!.measure %in% codebook$measure) stop(".measure not in codebook")
    
  }
  
  if(!missing(.subscale)) {
    
    if(!.subscale %in% codebook$subscale) stop(".subscale not in codebook")
    
  } 
  
  # If the user provides .prefix and .measure only, only filter the codebook by those criteria
  if(missing(.subscale)) {
    
    # Take the codebook and...
    filtered_codebook <- codebook %>%
      # ... filter to rows where...
      filter(
        # ... the item column starts with .prefix, and...
        grepl("^" %+% .prefix, item),
        # ... the measure column matches .measure
        measure == .measure
      )  
    
  # Otherwise, filter by all criteria
  } else {
    
    # Take the codebook and...
    filtered_codebook <- codebook %>%
      # ... filter to rows where...
      filter(
        # ... the item column starts with .prefix, and...
        grepl("^" %+% .prefix, item),
        # ... the measure column matches .measure, and...
        measure == .measure,
        # ... the subscale column matches .subscale
        subscale == .subscale
      )
    
  }
  
  # If no rows are returned, throw an error
  if(nrow(filtered_codebook) == 0) stop("No items match these criteria")
  
  # Return the items from the filtered codebook
  filtered_codebook %>%
    pull(item) %>%
    return()

}

# Function to take the mean across items from get_items() and to log the items
# used to compute the mean
mean_across <- function(.prefix, .measure, .subscale, name, exclude) {
  
  # Get items
  items <- get_items(.prefix, .measure, .subscale)
  
  # If items are not unique, throw an error
  if(length(items) != length(unique(items))) stop("Item(s) are repeated and will bias mean")

  # Exclude items if argument is provided
  if(!missing(exclude)) {
    
    if(!all(exclude %in% items)) stop("Some items in `exclude` not in item list")
    
    items <- setdiff(items, exclude)
    
  }
  
  # Take mean across items, dropping NA values
  mean <- mean(
    c_across(
      all_of(
        items
      )
    ),
    na.rm = T
  )
  
  # Log the items used to compute the mean in list stored in global environment
  log$mean_items[[name]]$items   <<- items
  log$mean_items[[name]]$n_items <<- length(items)
  
  return(mean)
  
}

# Function to take the mean across items from get_items() and to log the items
# used to compute the mean
mean_across <- function(.prefix, .measure, .subscale, name, exclude) {
  
  # Get items
  items <- get_items(.prefix, .measure, .subscale)
  
  # If items are not unique, throw an error
  if(length(items) != length(unique(items))) stop("Item(s) are repeated and will bias mean")
  
  # Exclude items if argument is provided
  if(!missing(exclude)) {
    
    if(!all(exclude %in% items)) stop("Some items in `exclude` not in item list")
    
    items <- setdiff(items, exclude)
    
  }
  
  # Take mean across items, dropping NA values
  mean <- mean(
    c_across(
      all_of(
        items
      )
    ),
    na.rm = T
  )
  
  if (is.nan(mean)) mean <- NA
  
  # Log the items used to compute the mean in list stored in global environment
  log$mean_items[[name]]$items   <<- items
  log$mean_items[[name]]$n_items <<- length(items)
  
  return(mean)
  
}

# Function to take the sum (count) across items from get_items() and to log the items
# used to compute the mean
count_across <- function(.prefix, .measure, .subscale, name, exclude) {
  
  # Get items
  items <- get_items(.prefix, .measure, .subscale)
  
  # If items are not unique, throw an error
  if(length(items) != length(unique(items))) stop("Item(s) are repeated and will bias mean")
  
  # Exclude items if argument is provided
  if(!missing(exclude)) {
    
    if(!all(exclude %in% items)) stop("Some items in `exclude` not in item list")
    
    items <- setdiff(items, exclude)
    
  }
  
  # Take mean across items, dropping NA values
  count <- sum(
    c_across(
      all_of(
        items
      )
    ),
    na.rm = T
  )
  
  # Log the items used to compute the mean in list stored in global environment
  log$mean_items[[name]]$items   <<- items
  log$mean_items[[name]]$n_items <<- length(items)
  
  return(count)
  
}

# Function to check that values of categorical items are as expected
check_values <- function(.data, .item) {
  
  # Ensure item is in codebook and data
  if(!.item %in% colnames(.data)) stop(".item not in .data")
  if(!.item %in% codebook$item) stop(".item not in codebook")

  # Expected range
  expected_min <- codebook$minimum[codebook$item == .item]
  expected_max <- codebook$maximum[codebook$item == .item]

  # Actual range
  actual_min <- min(.data[[.item]], na.rm = T)
  actual_max <- max(.data[[.item]], na.rm = T)
  
  # Check
  if(actual_min < expected_min) stop("Actual min (" %+% actual_min %+% ") lower than expected min (" %+% expected_min %+% ")")
  if(actual_max > expected_max) stop("Actual max (" %+% actual_max %+% ") higher than expected max (" %+% expected_max %+% ")")
  
  # Confirm if no errors
  print(.item %+% " confirmed: All values in anticipated range")
  
}
