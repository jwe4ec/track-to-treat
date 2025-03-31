## Helper functions for Qualtrics data cleaning

# New syntax to paste strings together
`%+%` <- paste0

# Function to drop invalid responses
remove_invalid_responses <- function(data, id) {
  
  # Taking the data, filter out cases where the ID variable is missing or in invalid_ids
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

# Function to take the mean across items from get_items()
mean_across <- function(...) {
  
  # Get items
  items <- get_items(...)

  # Take mean across items, dropping NA values
  mean(
    c_across(
      all_of(
        items
      )
    ),
    na.rm = T
  )
  
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
