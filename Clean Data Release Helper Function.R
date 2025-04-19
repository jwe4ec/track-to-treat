#### Helper function to create versioned clean data release ####

create_data_release <- function(clean_data_staging_dir, clean_data_final_dir, staged_filenames) {
  ### Load staged files into named list
  
  staged_files <- lapply(paste0(clean_data_staging_dir, "\\", staged_filenames), readRDS)
  names(staged_files) <- staged_filenames
  
  ### Obtain version info from user via console (preventing storage of info in script, 
  ### which would risk overwriting files) and obtain date from system. After obtaining
  ### user's confirmation via console to create versioned folder containing versioned
  ### files, do so. User can press ESC to exit loops for obtaining user input.
  
  ## Obtain version info from user via console and ensure correct format
  
  repeat {
    ver_prompt <- "Enter a version number and your first name in this format (v0.1_Jeremy): "
    version_firstname <- readline(ver_prompt)
    
    if (!grepl("v", version_firstname) | !grepl("_", version_firstname)) {
      cat("You must provide a response in the requested format! Press ESC to stop.")
    } else break
  }
  
  cat("Valid 'version_firstname' entered:", version_firstname)
  
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
  
  ## Define versioned folder name, prepend version info to staged filenames, and 
  ## define versioned README filename
  
  system_date_version_firstname <- paste0(system_date, "_", version_firstname)
  
  folder_name         <- system_date_version_firstname
  names(staged_files) <- paste0(system_date_version_firstname, "_", names(staged_files))
  readme_name         <- paste0(system_date_version_firstname, "_README.txt")
  
  ## Tell user what folder will be created and what files it will contain
  
  cat("A folder named '", folder_name, "' will be created in:\n", 
      clean_data_final_dir, "\n\n",
      
      "Containing these clean data files (named per 'system_date'):\n", 
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
  
  clean_data_final_folder_dir <- paste0(clean_data_final_dir, folder_name, "\\")
  dir.create(clean_data_final_folder_dir)
  
  ## Save clean data files to folder
  
  lapply(names(staged_files), function(staged_filename) {
    saveRDS(staged_files[[staged_filename]],
            file = paste0(clean_data_final_folder_dir, staged_filename))
  })
  
  ## Save README file to folder
  
  sink(file = paste0(clean_data_final_folder_dir, readme_name))
  
  cat("Clean Data for Project Track-to-Treat\n",
      "Contributors: Isaac Ahuvia, Jeremy Eberle, Alyssa Gorkin\n\n",
      
      "This folder, the following clean data files it contains, and this README\n",
      "were created by running the cleaning code on the GitHub repository below\n\n",
      
      "Repository URL and README: https://github.com/isaacahuvia/track-to-treat\n\n",
      
      "The code as of ", cleaning_code_date, " was run on ", system_date, " by the person below,\n",
      "who assigned the following version number\n\n",
      
      "Version:    ", version_firstname, "\n",
      "Created By: ", "TODO", "\n\n",
      
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