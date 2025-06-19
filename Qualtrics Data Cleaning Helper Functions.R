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
fill_lifepak_id <- function(data, lsmh_id, lifepak_id) {

  ## Check that each LSMH ID has <= 1 LifePak ID
  lsmh_ids_with_multiple_lifepak_ids <- data %>%
    distinct({{lsmh_id}}, {{lifepak_id}}) %>%
    drop_na() %>%
    count({{lsmh_id}}) %>%
    filter(n > 1)
  
  if(nrow(lsmh_ids_with_multiple_lifepak_ids) > 0) stop("Some LSMH IDs correspond to more than one LifePak ID")
  
  ## Fill LifePak ID across duplicates
  # Taking the data...
  data %>%
    # ... group by ID ...
    group_by({{lsmh_id}}) %>%
    # ... then, fill LifePak ID down-up across participant responses...
    fill({{lifepak_id}}, .direction = "downup") %>%
    ungroup() %>%
    return()
  
}

# Function to identify duplicates
identify_duplicates <- function(data, id, completion_indicator = Finished) {
  
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
      complete = sum({{completion_indicator}})
    ) %>%
    # Finally, arrange the dataset such that duplicates are at the top
    arrange(desc(complete), desc(total))
  
  # Print a message about how many rows were duplicated
  ids <- nrow(out)
  duplicates <- sum(out$total > 1)
  completed_duplicates <- sum(out$complete > 1)
  
  message(
    "Out of " %+% ids %+% " IDs, " %+% duplicates %+% " had multiple responses, while " %+% completed_duplicates %+% " had multiple completed responses.\n" %+%
    "(Note: 'complete' only means clicked through survey, not completed all items.)"
  )
  
  # Return the summary table with duplicated rows at the top
  return(out)
  
}

# Function to compute item completion rate
compute_item_completion_rate <- function(data, survey_prefix) {
  
  # Metadata columns
  qualtrics_metadata <- c(
    "StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration (in seconds)", 
    "Finished", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", 
    "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", 
    "DistributionChannel", "UserLanguage", "status", "SC0"
  )
  
  survey_metadata <- c(
    "administration", "assent_signature", "consent_signature", "p3m_address", 
    "p3m_child_name", "p3m_childcell", "p3m_childemail", "p3m_date", "p3m_homephone",
    "p3m_lsmh_id", "p3m_lsmh_id_validate", "p3m_parentcell", "p3m_parentemail",
    "p3m_workphone", "p3m_wrapup_optin", "password_child", "password_parent", 
    "pb_address", "pb_child_name", "pb_childcell", "pb_childemail", "pb_date",
    "pb_homephone", "pb_interview", "pb_lsmh_id", "pb_lsmh_id_validate", "pb_parentcell", 
    "pb_parentemail", "pb_workphone", "y3_childname", "y3_lsmh_id_ validate",
    "y3m_chrome_browser", "y3m_lsmh_id", "yb_end", "yb_end_3", "yb_interview",
    "yb_LifePak ID", "yb_LifePak ID Verify", "yb_lsmh_id", "yb_lsmh_id_ validate", 
    "yb_phone", "yb_phone_validate"
  )
  
  # Remove columns that should not be included in calculation
  data_for_calculation <- data %>%
    select(
      -matches("time.*(Click|Submit)"), # Columns with click and time on page information
      -matches("_TEXT"), # Columns with specified responses for response options of "Other" (or similar)
      -any_of(qualtrics_metadata),
      -any_of(survey_metadata)
    )
  
  # Calculate item completion rate
  data$item_completion_rate <- rowMeans(!is.na(data_for_calculation))
  
  # Print and log items used to compute completion rate in list stored in global environment
  item_cols <- colnames(data_for_calculation)
  
  print("Item completion rate based on these items:")
  print(item_cols)
  
  log$item_completion_rate[[survey_prefix]]$items   <<- item_cols
  log$item_completion_rate[[survey_prefix]]$n_items <<- length(item_cols)
  
  return(data)
  
}

# Function to compute indicator of baseline survey completion in assessment window
mark_b_done_in_ax_window <- function(data, id_as_char, ema_notif_dates) {
  
  # Add EMA notification dates to data
  names(ema_notif_dates)[names(ema_notif_dates) == "lsmh_id"] <- id_as_char
  
  data <- data %>%
    left_join(ema_notif_dates, by = id_as_char, relationship = "many-to-one") %>%
    # Compute indicator of survey completion before first EMA notification
    # Note: Given that "EndDate" and "first_ema_notif_date" are in different time
    # zones ("America/Denver" for Phase I vs. participants' local times stored as 
    # UTC, respectively), this comparison is approximate. To rule out the role of
    # time zone differences, derive actual time zones for "first_ema_notif_date"
    # from LifePak GPS data (although GPS data are missing for some observations)
    mutate(in_window_b = as_date(EndDate) < first_ema_notif_date)
  
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
    left_join(ax_windows, by = id_as_char, relationship = "many-to-one") %>%
    mutate(
      
      # Compute indicators of survey completion in originally intended assessment window
      # and extended window with later end date
      in_window_3m_org = as_date(EndDate) >= start_window_3m_org & as_date(EndDate) <= end_window_3m_org,
      in_window_3m_ext = as_date(EndDate) >= start_window_3m_ext & as_date(EndDate) <= end_window_3m_ext,
      
      # If done early, compute days before start of originally intended window
      days_before_start_window_3m_org = ifelse(
        as_date(EndDate) < start_window_3m_org,
        as_date(EndDate) - start_window_3m_org,
        NA
      ),
      
      # If done late, compute days after end of originally intended window
      days_after_end_window_3m_org = ifelse(
        as_date(EndDate) > end_window_3m_org,
        as_date(EndDate) - end_window_3m_org,
        NA
      )
      
    )
  
  # Throw warning if any surveys were completed before start of original window
  if (any(!is.na(data$days_before_start_window_3m_org))) {
    
    warning("Survey(s) completed before original window. Consider earlier start date for extended window.")
    
  }

  return(data)
  
}

# Function to deduplicate datasets, keeping first (most) complete response
remove_duplicates <- function(data, id, date = EndDate) {
  
  # Taking the data...
  data %>%
    # ... group by ID ...
    group_by({{id}}) %>%
    # ... by ID, arrange first by item_completion_rate (putting more completed 
    # responses at top), then by EndDate (putting first/oldest responses at top)...
    arrange(
      desc(item_completion_rate),
      {{date}}
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
      dplyr::filter(
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
      dplyr::filter(
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
  
  if(is.nan(mean)) mean <- NA
  
  # Log the items used to compute the mean in list stored in global environment
  log$mean_items[[name]]$items   <<- items
  log$mean_items[[name]]$n_items <<- length(items)
  
  return(mean)
  
}

# Function to take the sum (count) across items from get_items() and to log the items
# used to compute the sum
count_across <- function(.prefix, .measure, .subscale, name, exclude) {
  
  # Get items
  items <- get_items(.prefix, .measure, .subscale)
  
  # If items are not unique, throw an error
  if(length(items) != length(unique(items))) stop("Item(s) are repeated and will bias sum")
  
  # Exclude items if argument is provided
  if(!missing(exclude)) {
    
    if(!all(exclude %in% items)) stop("Some items in `exclude` not in item list")
    
    items <- setdiff(items, exclude)
    
  }
  
  # Take sum across items, dropping NA values
  count <- sum(
    c_across(
      all_of(
        items
      )
    ),
    na.rm = T
  )
  
  # Log the items used to compute the sum in list stored in global environment
  log$count_items[[name]]$items   <<- items
  log$count_items[[name]]$n_items <<- length(items)
  
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

# Function to check for duplicate responses to measure's (or subscale's) items over time
# - Note: If excluding items, provide the items' names at every time point
check_dups_over_time <- function(data, prefixes, .measure, .subscale, exclude) {

  ## Get list of sorted items for each time point (.prefix)
  items_ls <- vector("list", length(prefixes))
  names(items_ls) <- prefixes
  
  for (.prefix in prefixes) {
    
    items <- get_items(.prefix, .measure, .subscale)
    items_ls[[.prefix]] <- sort(items)
    
  }
  
  ## Exclude items if argument is provided
  if(!missing(exclude)) {
    
    if (length(setdiff(exclude, unlist(items_ls))) > 0) {
      
      stop("Some items in `exclude` not in item list")
      
    }
    
    items_ls <- lapply(items_ls, function(x) setdiff(x, exclude))
    
  }
  
  ## Confirm that number of items is the same over time
  n_items <- sapply(items_ls, length)
  
  if (length(unique(n_items)) != 1) {
    
    print(items_ls)
    print(n_items)
    
    stop("Different number of items above over time")
    
  }
  
  ## Confirm that, apart from .prefix, items are named identically over time
  items_ls_no_prefix <- lapply(items_ls, function(x) sub("^[^_]+_", "", x))
  
  if (length(unique(items_ls_no_prefix)) != 1) {
    
    print(items_ls_no_prefix)
    
    stop("Items above are not named identically over time when ignoring prefix")
    
  }
  
  ## Check that no corresponding items have duplicate responses over time
  # Restrict data to relevant columns
  item_cols <- unlist(items_ls, use.names = FALSE)
  data <- data[, c("lsmh_id", item_cols)]
  
  # Convert to long format
  item_cols_no_prefix <- unique(unlist(items_ls_no_prefix, use.names = FALSE))
  
  data <- data %>%
    pivot_longer(cols = all_of(item_cols), 
                 names_to = c("survey", "item"),
                 names_pattern = "(^[^_]+)_(.*)",
                 values_to = "value") %>%
    pivot_wider(names_from = "item", values_from = "value") %>%
    select(-survey)

  # Check for duplicate responses over time
  dup_ids <- unique(data$lsmh_id[duplicated(data)])
  
  if (length(dup_ids) == 0) {
    
    cat("No duplicated responses over time")
    
  } else {
    
    cat("Duplicated responses over time for these IDs (see below): ", dup_ids, "\n\n")
    print(data[data$lsmh_id %in% dup_ids, ])
    
  }

}

# Function to load and clean phase 2 codebook, as this is done in each script
load_p2_codebook <- function(codebook_path) {
  
  sheet_name <- "Qualtrics Measure Variables"
  (sheet_last_row <- nrow(openxlsx::read.xlsx(codebook_path, sheet_name)) + 1) # Add 1 for header row
  
  codebook <- openxlsx::read.xlsx(
    codebook_path,
    sheet_name,
    rows = c(1, 3:sheet_last_row) # Skip column description row
  ) %>%
    # Select only necessary variables
    select(
      item = Variable.Name,
      measure = Measure,
      subscale = Subscale,
      minimum = Minimum,
      maximum = Maximum,
      reversed = `Is.the.variable.reverse.coded?`
    ) %>%
    mutate(
      # Make `reversed` logical
      reversed = reversed == 1,
      # Create `reverse_base`: the number a response should be subtracted from to reverse it
      reverse_base = if_else(
        reversed,
        maximum + minimum,
        NA_real_
      )
    ) %>%
    # Expand codebook, such that each row with "[x]" in the item name is now one row per wave,
    # with "[x]" replaced with the wave numbers (e.g., "y[x]" -> "yb", "y3m", etc.)
    # Create a new column to expand by
    mutate(
      wave = if_else(
        # If "[x]" is in the item name...
        grepl("\\[x\\]", item),
        # ... make `waves` a list with one value per wave, otherwise...
        list(c("b", "3m", "6m", "12m", "18m")),
        # ... make it an empty list
        list(c(""))
      )
    ) %>%
    # Unnest such that there is now one row per item per wave
    unnest_longer(col = wave) %>%
    # Overwrite the `item` column so that "[x]"s are replaced with the actual waves
    mutate(
      item = str_replace(
        string = item,
        pattern = "\\[x\\]",
        replacement = wave
      )
    )
  
  return(codebook)
  
}

# Function to load and clean phase 2 participant tracker, as this is done in each script
load_p2_tracker <- function(tracker_path) {
  
  tracker <- read_csv(tracker_path, col_types = "c") %>%
    mutate(
      baseline_date = `Baseline Date/Time` %>%
        lubridate::mdy_hm() %>%
        lubridate::date()
    ) %>%
    select(
      lsmh_id = `LSMH ID`,
      lifepak_id = `LifePak ID`,
      phase = Phase,
      baseline_date
    )
  
}
