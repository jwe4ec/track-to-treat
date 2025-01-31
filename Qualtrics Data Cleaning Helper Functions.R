## Helper functions for Qualtrics data cleaning

# New syntax to paste strings together
`%+%` <- paste0

# Function to drop invalid responses
remove_invalid_responses <- function(data, id) {
  
  out <- data %>%
    filter(
      !is.na({{id}}),
      !{{id}} %in% invalid_ids
    )
  
  original_ids <- data %>%
    pull({{id}})
  
  filtered_ids <- out %>%
    pull({{id}})
  
  dropped_ids <- setdiff(original_ids, filtered_ids)
  
  message("Dropped " %+% length(dropped_ids) %+% " IDs: " %+% paste0(dropped_ids, collapse = ", "))
  
  return(out)
  
}

# Function to identify duplicates
identify_duplicates <- function(data, id) {
  
  out <- data %>%
    filter(
      !is.na({{id}}),
      !{{id}} %in% invalid_ids
    ) %>%
    group_by({{id}}) %>%
    summarize(
      total = n(),
      complete = sum(Finished)
    ) %>%
    arrange(desc(complete), desc(total))
  
  ids <- nrow(out)
  duplicates <- sum(out$total > 1)
  completed_duplicates <- sum(out$complete > 1)
  
  message("Out of " %+% ids %+% " IDs, " %+% duplicates %+% " had multiple responses, while " %+% completed_duplicates %+% " had multiple completed responses")
  
  return(out)
  
}

# Function to deduplicate datasets, keeping first completed response and
# filling LifePak ID before filtering (there is at least one case where a respondent
# provided their LifePak ID only in a duplicated, noncomplete response)
remove_duplicates <- function(data, id) {
  
  data %>%
    group_by({{id}}) %>%
    arrange(
      desc(Finished),
      desc(Progress),
      StartDate
    ) %>%
    fill(contains("LifePak ID"), .direction = "downup") %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    return()
  
}

# Function to return items from the codebook file, given some criteria
get_items <- function(.prefix, .measure, .subscale) {
  
  if(missing(.subscale)) {
    
    filtered_codebook <- codebook %>%
      filter(
        grepl(.prefix, item),
        measure == .measure
      )  
    
  } else {
    
    filtered_codebook <- codebook %>%
      filter(
        grepl("^" %+% .prefix, item),
        measure == .measure,
        subscale == .subscale
      )
    
  }
  
  if(nrow(filtered_codebook) == 0) stop("No items match these criteria")
  
  filtered_codebook %>%
    pull(item) %>%
    return()
  
}

# Function to takes the mean across items from get_items()
mean_across <- function(...) {
  
  items <- get_items(...)
  
  mean(
    c_across(
      all_of(
        items
      )
    ),
    na.rm = T
  )
  
}
