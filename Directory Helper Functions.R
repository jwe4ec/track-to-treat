## Helper functions for directories

# Function to get path to "jslab/" on FSMResFiles server depending on operating system
get_jslab_dir <- function() {
  
  jslab_dir <- if (.Platform$OS.type == "windows") {
    Sys.getenv("JSLAB_DIR_WINDOWS")
  } else if (.Platform$OS.type == "unix") {
    Sys.getenv("JSLAB_DIR_UNIX")
  } else {
    stop(paste("Set path to 'jslab/' for your operating system (OS) as environmental variable",
               "in an '.Renviron' file, then update 'get_jslab_dir()' to read it for your OS"))
  }
  
  if (jslab_dir == "") {
    stop(paste("No path to 'jslab/' set in '.Renviron'. Need to set path as environmental variable",
               "JSLAB_DIR_WINDOWS or JSLAB_DIR_UNIX (depending on your OS), in an '.Renviron' file."))
  }
  
  return(jslab_dir)
  
}

# Function to get directories for Phase 1 data
get_p1_dirs <- function(type = c("raw_lifepak_data", "raw_qualtrics_data",
                                 "clean_data_staging", "clean_data_staging_intermediate",
                                 "clean_data_final_read_only")) {
  
  # User helper to get path to "jslab/"
  jslab_dir <- get_jslab_dir()
  
  # Build all paths using file.path(), which works across operating systems
  raw_lifepak_data_dir   <- file.path(jslab_dir, "TRACK to TREAT", "Data", "LifePak Raw Data (Do Not Modify)")
  raw_qualtrics_data_dir <- file.path(jslab_dir, "TRACK to TREAT", "Data", "Qualtrics Data", "Raw Data")
  clean_data_dir         <- file.path(jslab_dir, "TRACK to TREAT", "Data", "Clean Data (Isaac)")
  
  clean_data_staging_dir              <- file.path(clean_data_dir, "staging")
  clean_data_staging_intermediate_dir <- file.path(clean_data_staging_dir, "intermediate")
  clean_data_final_read_only_dir      <- file.path(clean_data_dir, "final_read_only")
  
  # Select desired directories
  all_dirs <- list(
    raw_lifepak_data = raw_lifepak_data_dir,
    raw_qualtrics_data = raw_qualtrics_data_dir,
    clean_data_staging = clean_data_staging_dir,
    clean_data_staging_intermediate = clean_data_staging_intermediate_dir,
    clean_data_final_read_only = clean_data_final_read_only_dir
  )
  
  dirs <- all_dirs[type]
  
  message("Using these directories:")
  str(dirs)
  
  return(dirs)
  
}

# Function to get directories for Phase 2 data
get_p2_dirs <- function(type = c("raw_tracking_log_data", "raw_lifepak_data", "raw_qualtrics_data",
                                 "clean_data_staging", "clean_data_staging_intermediate",
                                 "clean_data_final_read_only")) {
  
  # User helper to get path to "jslab/"
  jslab_dir <- get_jslab_dir()
  
  # Build all paths using file.path(), which works across operating systems
  raw_tracking_log_data_dir <- file.path(jslab_dir, "TRACK to TREAT P2", "Data", "Tracking Log")
  raw_lifepak_data_dir      <- file.path(jslab_dir, "TRACK to TREAT P2", "Data", "LifePak", "2025.05.21")
  raw_qualtrics_data_dir    <- file.path(jslab_dir, "TRACK to TREAT P2", "Data", "Qualtrics", "Raw", "2026.02.26_final")
  clean_data_dir            <- file.path(jslab_dir, "TRACK to TREAT P2", "Data", "Clean Data (Isaac)")
  
  clean_data_staging_dir              <- file.path(clean_data_dir, "staging")
  clean_data_staging_intermediate_dir <- file.path(clean_data_staging_dir, "intermediate")
  clean_data_final_read_only_dir      <- file.path(clean_data_dir, "final_read_only")
  
  # Select desired directories
  all_dirs <- list(
    raw_tracking_log_data = raw_tracking_log_data_dir,
    raw_lifepak_data = raw_lifepak_data_dir,
    raw_qualtrics_data = raw_qualtrics_data_dir,
    clean_data_staging = clean_data_staging_dir,
    clean_data_staging_intermediate = clean_data_staging_intermediate_dir,
    clean_data_final_read_only = clean_data_final_read_only_dir
  )
  
  dirs <- all_dirs[type]
  
  message("Using these directories:")
  str(dirs)
  
  return(dirs)
  
}