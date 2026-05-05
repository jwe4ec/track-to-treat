####  Helper function to check raw data versions for Phases 1-2  ####
check_raw_data_ver <- function(raw_metadata, path_ls, data_ls, data_types,
                               write_loaded_raw_metadata = FALSE) {
  
  # Get metadata of loaded raw data files
  loaded_raw_metadata <- data.frame(filename = basename(unlist(path_ls)),
                                    size = sapply(path_ls, function(x) file.info(x)$size),
                                    nrow = sapply(data_ls, nrow),
                                    ncol = sapply(data_ls, ncol),
                                    hash = sapply(path_ls, digest, algo = "sha256", file = TRUE),
                                    row.names = NULL)
  
  # Optionally export loaded metadata (for help building "Raw <P1/P2> Metadata.csv"
  # files) to local "./temp_delete/" folder (manually delete this folder after use)
  if (write_loaded_raw_metadata == TRUE) {
    dir.create("./temp_delete/")
    write.csv(loaded_raw_metadata, "./temp_delete/loaded_raw_metadata.csv", row.names = FALSE)
    
    stop("Delete './temp_delete/' and rerun script with 'write_loaded_raw_metadata = FALSE'")
  }
  
  # Compare metadata of loaded raw data files to metadata expected
  expected_raw_metadata <- raw_metadata[raw_metadata$data_type %in% data_types, ]
  expected_raw_metadata[, c("data_type", "survey_name")] <- NULL
  row.names(expected_raw_metadata) <- 1:nrow(expected_raw_metadata)

  comparison <- all.equal(expected_raw_metadata, loaded_raw_metadata)
  
  if (isTRUE(comparison)) {
    cat("Loaded raw data versions match those expected")
  } else {
    cat("Loaded raw data versions do not match those expected!\n\n")
    cat("Metadata of expected raw data files:\n\n")
    print(expected_raw_metadata)
    cat("\nMetadata of loaded raw data files:\n\n")
    print(loaded_raw_metadata)
    cat("\nComparison:\n\n")
    print(comparison)
    
    stop("Resolve discrepancy between raw data versions")
  }

}

####  Helper function to create versioned clean data release for Phases 1-2  ####
create_data_release <- function(clean_data_staging_dir, clean_data_final_dir, phase, staged_filenames) {
  
  ### Load staged files into named list
  staged_files <- lapply(file.path(clean_data_staging_dir, staged_filenames), readRDS)
  names(staged_files) <- staged_filenames
  
  ### Obtain version info from user via console (preventing storage of info in script, 
  ### which would risk overwriting files) and obtain date from system. After obtaining
  ### user's confirmation via console to create versioned folder containing versioned
  ### files, do so. User can press ESC to exit loops for obtaining user input.
  
  ## Obtain version number from user via console and ensure correct format
  repeat {
    ver_prompt <- paste("Enter a version number for Phase", phase, "data in this format (e.g., v0.1): ")
    version <- readline(ver_prompt)
    
    if (!grepl("v", version) | !grepl("\\.", version)) {
      cat("You must provide a response in the requested format! Press ESC to stop.")
    } else break
  }
  
  cat("Valid 'version' entered:", version)
  
  ## Obtain user's first name via console
  repeat {
    ver_prompt <- "Enter your first name in this format (e.g., Jeremy): "
    firstname <- readline(ver_prompt)
    
    if (firstname == "" | grepl(" ", firstname)) {
      cat("You must provide a response in the requested format! Press ESC to stop.")
    } else break
  }
  
  cat("Valid 'firstname' entered:", firstname)
  
  ## Obtain date this script was run from system
  system_date <- as.character(Sys.Date())
  
  ## Obtain date cleaning code was last modified from user via console and ensure format
  repeat {
    code_date_prompt <- "Enter the date the cleaning code was last modified in this format (YYYY-MM-DD): "
    cleaning_code_date <- readline(code_date_prompt)
    
    if (!grepl("-", cleaning_code_date) | nchar(cleaning_code_date) != 10) {
      cat("You must provide a response in the requested format! Press ESC to stop.")
    } else break
  }
  
  cat("Valid 'cleaning_code_date' entered:  ", cleaning_code_date, "\n",
      "Date running script ('system_date'): ", system_date, "\n\n",
      sep = "")
  
  ## Define versioned folder name, prepend version number to staged filenames, and 
  ## define versioned README filename
  system_date_version <- paste0(system_date, "_", version)
  system_date_version_firstname <- paste0(system_date_version, "_", firstname)
  
  folder_name         <- system_date_version_firstname
  names(staged_files) <- paste0(system_date_version, "_", names(staged_files))
  readme_name         <- paste0(system_date_version, "_README.txt")
  
  ## Tell user what folder will be created and what files it will contain
  cat("A folder named '", folder_name, "' will be created in:\n", 
      clean_data_final_dir, "\n\n",
      
      "Containing these Phase ", phase, " clean data files (named per 'system_date'):\n", 
      paste(names(staged_files), collapse = "\n"), "\n\n",
      
      "And this README noting the 'cleaning_code_date' and other info:\n",
      readme_name,
      sep = "")
  
  ## Obtain user's confirmation via console
  repeat {
    ok <- readline("Enter y to create folder and save files or press ESC to stop: ")
    
    if (ok == "y") break
  }
  
  ## Create folder
  clean_data_final_folder_dir <- file.path(clean_data_final_dir, folder_name)
  dir.create(clean_data_final_folder_dir)
  
  ## Save clean data files to folder
  lapply(names(staged_files), function(staged_filename) {
    saveRDS(staged_files[[staged_filename]],
            file = file.path(clean_data_final_folder_dir, staged_filename))
  })
  
  ## Save README file to folder
  sink(file = file.path(clean_data_final_folder_dir, readme_name))
  
  cat("Clean Data for Phase ", phase, " of Project Track-to-Treat\n",
      "Contributors: Jeremy Eberle, Isaac Ahuvia, Alyssa Gorkin\n\n",
      
      "This folder, the following clean data files it contains, and this README\n",
      "were created by running the cleaning code on the GitHub repository below\n\n",
      
      "Repository URL and README: https://github.com/jwe4ec/track-to-treat\n\n",
      
      "The Phase ", phase, " code as of ", cleaning_code_date, " was run on ", system_date, " by the person below,\n",
      "who assigned the following version number\n\n",
      
      "Version:    ", version, "\n",
      "Created By: ", firstname, "\n\n",
      
      "Folder:\n",
      clean_data_final_folder_dir, "\n\n",
      
      "Clean Data Files:\n",
      paste(names(staged_files), collapse = "\n"), "\n\n",
      
      "Review the GitHub README for details and flags before running any analysis,\n",
      "and use the version number above to indicate which clean data you analyzed\n\n",
      
      "DO NOT modify/delete files in this folder. They may be inputs to an analysis!",
      sep = "")
  
  sink()
  
  ## Tell user path to saved files
  cat("Path to saved files:\n",
      clean_data_final_folder_dir,
      sep = "")
  
}