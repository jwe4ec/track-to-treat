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
  
  message("Out of " %+% ids %+% " IDs, " %+% duplicates %+% " had multiple responses, while " %+% completed_duplicates %+% " had multiple completed responses")
  
  # Return the summary table with duplicated rows at the top
  return(out)
  
}

# Function to deduplicate datasets, keeping first (most) complete response and
# filling LifePak ID before filtering (there is at least one case where a respondent
# provided their LifePak ID only in a duplicated, noncomplete response)
remove_duplicates <- function(data, id) {
  
  # Taking the data...
  data %>%
    # ... group by ID ...
    group_by({{id}}) %>%
    # ... by ID, arrange first by Finished (putting completed responses at the top),
    # then by Progress (putting more completed responses at the top), then by StartDate
    # (putting first/oldest responses at the top)...
    arrange(
      desc(Finished),
      desc(Progress),
      StartDate
    ) %>%
    # ... then, fill the LifePak ID variable down-up across participant responses...
    fill(contains("LifePak ID"), .direction = "downup") %>%
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
mean_across <- function(.prefix, .measure, .subscale, name) {
  
  # Get items
  items <- get_items(.prefix, .measure, .subscale)
  
  # If items are not unique, throw an error
  if(length(items) != length(unique(items))) stop("Item(s) are repeated and will bias mean")

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


# Function to check that values of categorical items are as expected
check_values <- function(.data, .item) {

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
