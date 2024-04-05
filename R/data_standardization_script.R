#' Error Handle
#'
#' The error_handle() function is an exception/error handling function that will log error messages and print
#' these messages to an output log, allowing the user to read and see what went wrong in the program, allowing
#' them to hopefully understand and make corrections to a specific datasets metadata
#' @param metadata_conn A database connection that is used in addition to the dataset_id, passed from the standardize_data() function. Is also used in the pre_process_data(), and pre_process_chunks() functions.
#' @param err_msg An error message indicated what went wrong in the program, this message will be written to an output .txt file and placed in a folder containing error messages
#' @param cleaned_file A database connection to the sqlite file holding the currently processed dataset that was being written to at the time of error.
#' @param error_file_path A path to the input file.
#' @param error_folder_path A path to the output folder where the error .txt file will be created.
#' @examples
#' error_handle(db, "Invalid Dataset Code - Stopping Program", clean_db, "path/to/file.txt", "path/to/folder");
#' @export
error_handle <- function(metadata_conn, err_msg, cleaned_file, error_file_path, error_folder_path){
  print(paste("Error Message:", err_msg))

  # Create a new error file and write the error message to the file
  if(!is.null(error_file_path) && !is.null(error_folder_path)){
    file_name <- file_path_sans_ext(basename(error_file_path))
    text_file_path <- file.path(error_folder_path, paste0(file_name, "_ERRORS_README.txt"))
    file.create(text_file_path)
    writeLines(err_msg, text_file_path)
  }

  if(!is.null(metadata_conn)){
    dbDisconnect(metadata_conn)
  }

  if(!is.null(cleaned_file)){
    dbDisconnect(cleaned_file)
  }

  stop(err_msg)
}

#' Pre Process Data
#'
#' The pre_process_data() function will take in a source data frame object, and
#' go step-by-step through each part of the pre-processing to turn each source
#' data set into a common normalized format.
#' @param source_data_frame A source data frame, the chunk read and passed from the pre_process_chunks() function.
#' @param split_source_fields A vector of split source fields that have a standardizing module assigned to them in the metadata, grouped by those standardizing modules.
#' @param db_conn A database connection that is used in addition to the dataset_id, passed from the standardize_data() function. Is also used in the pre_process_data() function.
#' @param dataset_to_standardize A dataset_id found and passed from the standardize_data() function, is used to make various calls to the database connection argument to obtain information required to pre-process the data.
#' @param standardized_name_lookup_table A vector containing all standardized column names/destination fields for a standardizing module.
#' @param standardized_function_lookup_table A vector containing all standardizing module names, used by concatenating/pasting the name onto the prefix “pre_process_” which will automatically call the correct module.
#' @param flag_lookup A lookup table containing manually set or default values used by the flag script which will use the values of the flags to determine how to pre-process and output the health data.
#' @return A cleaned/standardized version of the source_data_frame parameter.
#' @examples
#' cleaned_df <- pre_process_data(df, source_fields, db, 1, name_lookup, function_lookup, flag_lookup)
#' @export
pre_process_data <- function(source_data_frame, split_source_fields, db_conn, dataset_to_standardize,
                             standardized_name_lookup_table, standardized_function_lookup_table,
                             flag_lookup){

  num_rows <- nrow(source_data_frame)

  processed_data_frame <- data.frame(matrix(ncol = 0, nrow = num_rows))

  # Add the primary key column from the source data frame to the processed data frame
  processed_data_frame[["record_primary_key"]] <- source_data_frame[["record_primary_key"]]

  # Look up the value for "impute_sex" in the flag look up table to create
  # two temporary vectors of strings and genders to compare between after all
  # functions are completed.
  impute_sex_value <- flag_lookup["impute_sex"]
  if(impute_sex_value == "yes"){
    temporary_names   <- c()
    temporary_genders <- c()
  }

  #----
  # pre_process_record_primary_key()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_record_primary_key() will normalize columns in data sets
  #   which contain the key able to identify a singular record.
  #----
  pre_process_record_primary_key <- function(record_primary_key_field, source_field_ids, df, standardized_col_name){
    processed_primary_keys <- character(nrow(df))

    for (curr_primary_key_field in record_primary_key_field) {
      unprocessed_primary_keys <- source_data_frame[[curr_primary_key_field]]

      unprocessed_primary_keys[is.na(unprocessed_primary_keys)] <- ""

      primary_key_column <- paste0(standardized_col_name, "_", curr_primary_key_field) #record_primary_key_case & record_primary_key_role

      df[[primary_key_column]] <- unprocessed_primary_keys
    }
    return(df)
  }

  #----
  # pre_process_individual_id()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_individual_id() function will normalize the column in data sets
  #   that keep track of the same person having multiple records.
  #----
  pre_process_individual_id <- function(individual_id_field, source_field_ids, df, standardized_col_name){

    processed_individual_ids <- character(nrow(df))

    for (curr_individual_id_field in individual_id_field) {
      unprocessed_individual_ids <- source_data_frame[[curr_individual_id_field]]

      if (curr_individual_id_field == individual_id_field[1]) {
        processed_individual_ids <- unprocessed_individual_ids
      } else {
        processed_individual_ids <- paste(processed_individual_ids, unprocessed_individual_ids, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_individual_ids)
    return(df)
  }

  #----
  # pre_process_phin()
  # ~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_phin() function simply takes the phin field (if it exists)
  #   and rename (if it must be) to the standardized field name.
  #----
  pre_process_phin <- function(phin_field, source_field_ids, df, standardized_col_name) {

    processed_phins <- character(nrow(df))

    for (curr_phin_field in phin_field) {
      # Grab the unprocessed phins from the source data frame
      unprocessed_phins <- source_data_frame[[curr_phin_field]]

      # Replace all NA values with ''
      unprocessed_phins[is.na(unprocessed_phins)] <- ""

      # Replace all punctuation/symbols in the phin field with a blank space
      standardized_phins <- stri_replace_all_regex(unprocessed_phins, "[-.'[:punct:] ]+", "")

      # Replace all non-numerical characters with ''
      standardized_phins <- gsub("[^0-9.-]", "", standardized_phins)

      # Replace a string made entirely of '0's with a blank character
      standardized_phins <- gsub("^0+$", "", standardized_phins)

      if (curr_phin_field == phin_field[1]) {
        processed_phins <- standardized_phins
      } else {
        processed_phins <- paste(processed_phins, standardized_phins, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_phins)
    return(df)
  }

  #----
  # pre_process_registration_no()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_registration_no() function will standardize the 6-char long
  #   registration number of either the CNNNNN or NNNNNN form, with the non-valid
  #   characters as [B, I, Z, S, T, Q, G, AND O].
  #----
  pre_process_registration_no <- function(reg_fields, source_field_ids, df, standardized_col_name){
    processed_registration_nos <- character(nrow(df))

    for (curr_registration_no_field in reg_fields) {
      unprocessed_registration_nos <- source_data_frame[[curr_registration_no_field]]

      # Replace all punctuation/symbols in the phin field with a blank space
      unprocessed_registration_nos <- stri_replace_all_regex(unprocessed_registration_nos, "[-.'[:punct:] ]+", "")

      if (curr_registration_no_field == reg_fields[1]) {
        processed_registration_nos <- unprocessed_registration_nos
      } else {
        processed_registration_nos <- paste(processed_registration_nos, unprocessed_registration_nos, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_registration_nos)
    return(df)
  }

  #----
  # pre_process_first_name()
  # ~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_first_names() function will take in a vector/list of all
  #   the fields that involve a person's first name in a data set, of which we
  #   will take steps to normalize the source data set into a more common and
  #   accessible format.
  #----
  pre_process_primary_given_name <- function(first_name_fields, source_field_ids, df, standardized_col_name){
    processed_first_names <- character(nrow(df))

    for (i in seq_along(first_name_fields)) {
      # Initialize the values and data that we will be normalizing
      curr_first_name_field <- first_name_fields[i]
      curr_source_field_id  <- source_field_ids[i]
      unprocessed_first_names <- source_data_frame[[curr_first_name_field]]

      # Standardize the primary given names using the standardize_names general function
      curr_first_names <- standardize_names(unprocessed_first_names, flag_lookup)

      # Add to our processed primary given names variable before we add it to the data frame
      if (i == 1) {
        processed_first_names <- curr_first_names
      } else {
        processed_first_names <- paste(processed_first_names, curr_first_names, sep = " ")
      }

    }

    # Look up flag value for if we will be trying to impute gender, store the processed names
    impute_sex_value <- flag_lookup["impute_sex"]
    if(impute_sex_value == "yes"){
      temporary_names <<- processed_first_names
    }

    # Look up the flag value for whether we want to create an additional column
    # for every given name or surname of a person
    list_all_names_value <- flag_lookup["list_all_curr_names"]
    if(list_all_names_value == "yes"){
      df <- list_curr_names(processed_first_names, df)
    }

    # Look up the flag value for whether we want to create an additional column
    # for every given name of a person
    list_all_curr_given_names_value <- flag_lookup["list_all_curr_given_names"]
    if(list_all_curr_given_names_value == "yes"){
      df <- list_curr_given_names(processed_first_names, df)
    }

    df[[standardized_col_name]] <- processed_first_names
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)
  }

  #----
  # pre_process_middle_name()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_middle_name() is the exact same as the first_names()
  #   function, following the same normalizing steps.
  #----
  pre_process_secondary_given_name <- function(middle_name_fields, source_field_ids, df, standardized_col_name){

    processed_middle_names <- character(nrow(df))

    for (i in seq_along(middle_name_fields)) {
      curr_middle_name_field <- middle_name_fields[i]
      unprocessed_secondary_given_names <- source_data_frame[[curr_middle_name_field]]

      # Standardize the secondary given names using the standardize_names general function
      curr_secondary_given_names <- standardize_names(unprocessed_secondary_given_names, flag_lookup)

      # Add to our processed secondary given names variable before we add it to the data frame
      if (i == 1) {
        processed_middle_names <- curr_secondary_given_names
      } else {
        processed_middle_names <- paste(processed_middle_names, curr_secondary_given_names, sep = " ")
      }
    }

    # Look up the flag value for whether we want to create an additional column
    # for every given name or surname of a person
    list_all_names_value <- flag_lookup["list_all_curr_names"]
    if(list_all_names_value == "yes"){
      df <- list_curr_names(processed_middle_names, df)
    }

    # Look up the flag value for whether we want to create an additional column
    # for every given name of a person
    list_all_curr_given_names_value <- flag_lookup["list_all_curr_given_names"]
    if(list_all_curr_given_names_value == "yes"){
      df <- list_curr_given_names(processed_middle_names, df)
    }

    df[[standardized_col_name]] <- processed_middle_names
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)
  }

  #----
  # pre_process_primary_surname()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_last_names() function is the exact same as the first_names()
  #   and middles_names() function, following the same normalizing steps.
  #----
  pre_process_primary_surname <- function(primary_surname_fields, source_field_ids, df, standardized_col_name){

    processed_primary_surnames <- character(nrow(df))

    for (i in seq_along(primary_surname_fields)) {
      curr_primary_surname_field <- primary_surname_fields[i]
      unprocessed_primary_surnames <- source_data_frame[[curr_primary_surname_field]]

      # Standardize the primary surnames using the standardize_names general function
      curr_primary_surnames <- standardize_names(unprocessed_primary_surnames, flag_lookup)

      # Add to our processed primary surnames variable before we add it to the data frame
      if (i == 1) {
        processed_primary_surnames <- curr_primary_surnames
      } else {
        processed_primary_surnames <- paste(processed_primary_surnames, curr_primary_surnames, sep = " ")
      }
    }

    # Look up the flag value for whether we want to create an additional column
    # for every given name or surname of a person
    list_all_names_value <- flag_lookup["list_all_curr_names"]
    if(list_all_names_value == "yes"){
      df <- list_curr_names(processed_primary_surnames, df)
    }

    # Look up the flag value for whether we want to create an additional column
    # for every surname of a person
    list_all_curr_surnames_value <- flag_lookup["list_all_curr_surnames"]
    if(list_all_curr_surnames_value == "yes"){
      df <- list_curr_surnames(processed_primary_surnames, df)
    }

    df[[standardized_col_name]] <- processed_primary_surnames
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)
  }

  #----
  # pre_process_prior_surname()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_prior_surname() is the exact same as the pre-
  #   processing functions before it, following the same normalizing steps.
  #----
  pre_process_prior_surname <- function(prior_last_name_fields, source_field_ids, df, standardized_col_name){

    processed_prior_surnames <- character(nrow(df))

    for (i in seq_along(prior_last_name_fields)) {
      curr_prior_surname_field <- prior_last_name_fields[i]
      unprocessed_prior_surnames <- source_data_frame[[curr_prior_surname_field]]

      # Standardize the prior surnames using the standardize_names general function
      curr_prior_surnames <- standardize_names(unprocessed_prior_surnames, flag_lookup)

      # Add to our processed prior surnames variable before we add it to the data frame
      if (i == 1) {
        processed_prior_surnames <- curr_prior_surnames
      } else {
        processed_prior_surnames <- paste(processed_prior_surnames, curr_prior_surnames, sep = " ")
      }
    }
    # Look up the flag value for whether we want to create an additional column
    # for every given name or surname of a person
    list_all_curr_names_value <- flag_lookup["list_all_curr_names"]
    if(list_all_curr_names_value == "yes"){
      df <- list_curr_names(processed_prior_surnames, df)
    }

    df[[standardized_col_name]] <- processed_prior_surnames
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)
  }

  #----
  # pre_process_secondary_surnames()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_secondary_surnames() function is the exact same as the pre-
  #   processing functions before it, following the same normalizing steps.
  #----
  pre_process_secondary_surname <- function(secondary_surnames_fields, source_field_ids, df, standardized_col_name){

    processed_secondary_surnames <- character(nrow(df))

    for (i in seq_along(secondary_surnames_fields)) {
      curr_secondary_surname_field <- secondary_surnames_fields[i]
      unprocessed_secondary_surnames <- source_data_frame[[curr_secondary_surname_field]]

      # Standardize the secondary surnames using the standardize_names general function
      curr_secondary_surnames <- standardize_names(unprocessed_secondary_surnames, flag_lookup)

      # Add to our processed secondary surnames variable before we add it to the data frame
      if (i == 1) {
        processed_secondary_surnames <- curr_secondary_surnames
      } else {
        processed_secondary_surnames <- paste(processed_secondary_surnames, curr_secondary_surnames, sep = " ")
      }
    }
    # Look up the flag value for whether we want to create an additional column
    # for every given name or surname of a person
    list_all_names_value <- flag_lookup["list_all_curr_names"]
    if(list_all_names_value == "yes"){
      df <- list_curr_names(processed_secondary_surnames, df)
    }

    # Look up the flag value for whether we want to create an additional column
    # for every surname of a person
    list_all_curr_surnames_value <- flag_lookup["list_all_curr_surnames"]
    if(list_all_curr_surnames_value == "yes"){
      df <- list_curr_surnames(processed_secondary_surnames, df)
    }

    df[[standardized_col_name]] <- processed_secondary_surnames
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)
  }

  #----
  # pre_process_birth_date()
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_dates() function will take in a list of fields involving
  #   either birthdate or acquisition date.
  #
  #   If the input is instead a single string, then we will need to standardize/
  #   normalize the string using the format that was given to us in the field
  #   dictionary.
  #----
  pre_process_birth_date <- function(date_fields, source_field_ids, df, standardized_col_name){

    for (i in seq_along(date_fields)) {
      curr_birth_date_field <- date_fields[i]
      curr_source_field_id <- source_field_ids[i]

      # Check for compound character records
      check_compound_records_query <- paste("SELECT * FROM compound_fields where source_field_id = ?;")
      compound_records <- dbSendQuery(db_conn, check_compound_records_query)
      dbBind(compound_records, list(curr_source_field_id))
      output_compound_records <- dbFetch(compound_records)
      compound_field_format_id <- output_compound_records$compound_field_format_id
      dbClearResult(compound_records)

      # If the compound record query returns a result, then process it that way
      if (!is.null(compound_field_format_id)) {
        unprocessed_birth_dates <- source_data_frame[[curr_birth_date_field]]

        # Query to grab the compound field's separators
        get_separators_query <- paste("SELECT separator FROM compound_field_separators
                                            WHERE compound_field_format_id = ? and separator is not NULL
                                            ORDER BY separator_order asc;")
        compound_field_separators <- dbSendQuery(db_conn, get_separators_query)
        dbBind(compound_field_separators, list(compound_field_format_id))
        separator_output <- dbFetch(compound_field_separators)
        dbClearResult(compound_field_separators)

        # Query to grab the compound field's substring indexes
        get_separators_query <- paste("SELECT substring_index FROM compound_field_separators
                                            WHERE compound_field_format_id = ? and substring_index is not NULL
                                            ORDER BY separator_order asc;")
        compound_field_indexes <- dbSendQuery(db_conn, get_separators_query)
        dbBind(compound_field_indexes, list(compound_field_format_id))
        index_output <- dbFetch(compound_field_indexes)
        dbClearResult(compound_field_indexes)

        # Initialize the separators potentially used by the split_compound_field function
        separators <- separator_output$separator

        # Initialize the indexes potentially used by the split_compound_field function
        substring_indexes <- index_output$substring_index

        if(length(substring_indexes) > 0){
          #print(unprocessed_birth_dates)
          split_dates <- split_compound_field(unprocessed_birth_dates, substring_indexes, "indexes")
        }
        else{
          #print(unprocessed_birth_dates)
          split_dates <- split_compound_field(unprocessed_birth_dates, separators, "separators")
        }

        # Query to grab the destination fields for each split value (ADDED LEFT JOIN)
        get_destination_field_names_query <- paste("SELECT destination_field_name FROM compound_field_destinations cfd
                                                      LEFT JOIN destination_fields df on cfd.destination_field_id = df.destination_field_id
                                                      WHERE compound_field_format_id = ?
                                                      ORDER BY destination_mapping_order asc;")
        compound_field_destination_names <- dbSendQuery(db_conn, get_destination_field_names_query)
        dbBind(compound_field_destination_names, list(compound_field_format_id))
        compound_field_names_output <- dbFetch(compound_field_destination_names)
        dbClearResult(compound_field_destination_names)

        # Initialize the destination field names for the split compound field
        destination_field_names <- compound_field_names_output$destination_field_name
        #print(destination_field_names)

        #-- Need to add condition here for incase we have multiple date fields! --#
        for(i in seq_along(destination_field_names)){
          if(!is.na(destination_field_names[i])){
            # Define month abbreviations
            month_lookup <- c(tolower(month.abb), tolower(month.name))

            # Convert non-numeric values to NA
            non_numeric_vals <- as.character(split_dates[[i]])
            non_numeric_vals[!grepl("^\\d+$", non_numeric_vals)] <- NA

            # Match non-NA values with month abbreviations and full names
            standardized_date <- match(tolower(split_dates[[i]]), month_lookup)

            # Use modulo 12 on standardized_date, and replace 0 values (December) with 12
            standardized_date <- as.integer(standardized_date)%%12
            standardized_date[standardized_date == 0] <- 12

            # Replace non-NA values in standardized_date with original numeric values
            standardized_date[!is.na(non_numeric_vals)] <- split_dates[[i]][!is.na(non_numeric_vals)]

            # Remove leading 0's from the standardize dates (We can make this add a leading 0 later if we want)
            standardized_date <- str_remove(standardized_date, "^0+")

            df[[destination_field_names[i]]] <- paste(df[[destination_field_names[i]]], standardized_date, sep = " ")
            df[[destination_field_names[i]]] <- trimws(df[[destination_field_names[i]]])
          }
        }
      }
      else {
        print("No records found.")
      }
    }

    #df <- df[, -which(names(df) == 'birth_date')]

    return(df)
  }

  #----
  # pre_process_birth_year()
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_year() function will pre-process dates split up such that
  #   we already have access to the year.
  #----
  pre_process_birth_year <- function(year_field, source_field_ids, df, standardized_col_name){
    processed_birth_years <- character(nrow(df))

    for (curr_birth_year_field in year_field) {
      unprocessed_birth_years <- source_data_frame[[curr_birth_year_field]]

      # Replace all non-numerical characters with ''
      unprocessed_birth_years <- gsub("[^0-9.-]", "", unprocessed_birth_years)

      # Replace a string made entirely of '0's with a blank character
      unprocessed_birth_years <- gsub("^0+$", "", unprocessed_birth_years)

      # Remove leading 0's
      unprocessed_birth_years <- str_remove(unprocessed_birth_years, "^0+")

      if (curr_birth_year_field == year_field[1]) {
        processed_birth_years <- unprocessed_birth_years
      } else {
        processed_birth_years <- paste(processed_birth_years, unprocessed_birth_years, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_birth_years)
    return(df)
  }

  #----
  # pre_process_birth_month()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_birth_month() function will pre-process dates split up such that
  #   we already have access to the month
  #----
  pre_process_birth_month <- function(month_field, source_field_ids, df, standardized_col_name){
    processed_birth_months <- character(nrow(df))

    for (curr_birth_month_field in month_field) {
      unprocessed_birth_months <- source_data_frame[[curr_birth_month_field]]

      # Define month abbreviations
      month_lookup <- c(tolower(month.abb), tolower(month.name))

      # Convert non-numeric values to NA
      non_numeric_vals <- as.character(unprocessed_birth_months)
      non_numeric_vals[!grepl("^\\d+$", non_numeric_vals)] <- NA

      # Match non-NA values with month abbreviations and full names
      standardized_months <- match(tolower(unprocessed_birth_months), month_lookup)

      # Use modulo 12 on standardized_months, and replace 0 values (December) with 12
      standardized_months <- as.integer(standardized_months)%%12
      standardized_months[standardized_months == 0] <- 12

      # Replace non-NA values in standardized_months with original numeric values
      standardized_months[!is.na(non_numeric_vals)] <- unprocessed_birth_months[!is.na(non_numeric_vals)]

      # Remove leading 0's
      standardized_months <- str_remove(standardized_months, "^0+")

      if (curr_birth_month_field == month_field[1]) {
        processed_birth_months <- standardized_months
      } else {
        processed_birth_months <- paste(processed_birth_months, standardized_months, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_birth_months)
    return(df)
  }

  #----
  # pre_process_birth_day()
  # ~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_birth_day() function will pre-process dates split up such that
  #   we already have access to the day.
  #----
  pre_process_birth_day <- function(day_field, source_fields, df, standardized_col_name){
    processed_birth_days <- character(nrow(df))

    for (curr_birth_day_field in day_field) {
      unprocessed_birth_days <- source_data_frame[[curr_birth_day_field]]

      # Replace all non-numerical characters with ''
      unprocessed_birth_days <- gsub("[^0-9.-]", "", unprocessed_birth_days)

      # Replace a string made entirely of '0's with a blank character
      unprocessed_birth_days <- gsub("^0+$", "", unprocessed_birth_days)

      # Remove leading 0's
      unprocessed_birth_days <- str_remove(unprocessed_birth_days, "^0+")

      if (curr_birth_day_field == day_field[1]) {
        processed_birth_days <- unprocessed_birth_days
      } else {
        processed_birth_days <- paste(processed_birth_days, unprocessed_birth_days, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_birth_days)
    return(df)
  }

  #----
  # pre_process_gender()
  # ~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_gender() function will take in a field that is relating to
  #   the gender of a person, and standardize it according to the Manitoba Health
  #   data standards.
  #----
  pre_process_gender <- function(gender_fields, source_field_ids, df, standardized_col_name){

    # Create an empty data-frame for processed genders
    processed_genders <- character(nrow(df))

    for(i in seq_along(gender_fields)){
      curr_gender_field <- gender_fields[i]
      curr_source_field_id <- source_field_ids[i]
      unprocessed_genders <- source_data_frame[[curr_gender_field]]

      # Create a query to join categorical_fields and categorical_values tables for the current field
      query <- paste("SELECT cf.source_field_id, cf.source_value, cv.standardized_value",
                     "FROM categorical_fields cf",
                     "JOIN categorical_values cv ON cf.standardized_value_id = cv.standardized_value_id",
                     "WHERE cf.source_field_id = ", curr_source_field_id)
      dataset_gender_vals <- dbSendQuery(db_conn, query)
      gender_vals <- dbFetch(dataset_gender_vals)
      dbClearResult(dataset_gender_vals)

      # Create a lookup table for the current gender field
      curr_gender_lookup <- setNames(gender_vals$standardized_value, tolower(gender_vals$source_value))

      # Vectorized standardization of gender values for the current field
      standardized_genders <- curr_gender_lookup[tolower(unprocessed_genders)]

      # Check our flag lookup table to determine if user wants to impute gender,
      # if so, we will make note of what records were imputed in an additional
      # column
      impute_sex_value <- flag_lookup["impute_sex"]
      if(impute_sex_value == "yes"){
        temporary_genders <<- standardized_genders
      }
      # else{
      #   # Handle cases where source_value doesn't exist in the look up table
      #   standardized_genders[is.na(standardized_genders)] <- ""
      # }

      # Append to the processed_genders vector
      processed_genders <- paste(processed_genders, standardized_genders, sep = " ")
    }
    # Combine the standardized values for all gender fields
    df[[standardized_col_name]] <- trimws(processed_genders)

    df[[standardized_col_name]] <- na_if(df[[standardized_col_name]], "NA")

    return(df)

  }

  #----
  # pre_process_address1()
  # ~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_address1() function will take in fields relating to a persons
  #   address, and will standardize to MH data standards.
  #----
  pre_process_address1 <- function(address_fields, source_field_ids, df, standardized_col_name){

    processed_addresses <- character(nrow(df))
    alternative_postal_codes <- character(nrow(df))

    for (i in seq_along(address_fields)) {
      curr_address_field <- address_fields[i]
      unprocessed_address <- source_data_frame[[curr_address_field]]

      # Get the extract postal codes value
      extract_postal_code_value <- flag_lookup["extract_postal_code"]

      if(extract_postal_code_value == "yes"){
        # Extract the postal code (if there is one) from the address strings, remove
        # them from the original string, and then append to their own column
        return_vars <- extract_postal_codes(unprocessed_address, df)

        # Use the return variables
        unprocessed_address <- return_vars$orig_field
        extracted_postal_codes <- return_vars$p_codes
      }

      # Use the standardize_addresses() function along with our look up flags
      # to standardize the addresses
      curr_addresses <- standardize_addresses(unprocessed_address, flag_lookup)

      if (i == 1) {
        processed_addresses <- curr_addresses
        if(extract_postal_code_value == "yes")
          alternative_postal_codes <- paste(alternative_postal_codes, extracted_postal_codes, sep = " ")
      } else {
        processed_addresses <- paste(processed_addresses, curr_addresses, sep = " ")
        if(extract_postal_code_value == "yes")
          alternative_postal_codes <- paste(alternative_postal_codes, extracted_postal_codes, sep = " ")
      }
    }
    if(extract_postal_code_value == "yes"){
      alternative_postal_codes <- stri_trim_both(alternative_postal_codes)

      # Call the standardizing_script function to de-duplicate alt postal codes.
      df <- concat_postal_codes(df, alternative_postal_codes)
    }

    df[[standardized_col_name]] <- processed_addresses
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)
  }

  #----
  # pre_process_address2()
  # ~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_address2() function will take in fields relating to a persons
  #   address, and will standardize to MH data standards.
  #----
  pre_process_address2 <- function(address_fields, source_field_ids, df, standardized_col_name){

    processed_addresses <- character(nrow(df))
    alternative_postal_codes <- character(nrow(df))

    for (i in seq_along(address_fields)) {
      curr_address_field <- address_fields[i]
      unprocessed_address <- source_data_frame[[curr_address_field]]

      # Get the extract postal codes value
      extract_postal_code_value <- flag_lookup["extract_postal_code"]

      if(extract_postal_code_value == "yes"){
        # Extract the postal code (if there is one) from the address strings, remove
        # them from the original string, and then append to their own column
        return_vars <- extract_postal_codes(unprocessed_address, df)

        # Use the return variables
        unprocessed_address <- return_vars$orig_field
        extracted_postal_codes <- return_vars$p_codes
      }

      # Use the standardize_addresses() function along with our look up flags
      # to standardize the addresses
      curr_addresses <- standardize_addresses(unprocessed_address, flag_lookup)

      if (i == 1) {
        processed_addresses <- curr_addresses
        if(extract_postal_code_value == "yes")
          alternative_postal_codes <- paste(alternative_postal_codes, extracted_postal_codes, sep = " ")
      } else {
        processed_addresses <- paste(processed_addresses, curr_addresses, sep = " ")
        if(extract_postal_code_value == "yes")
          alternative_postal_codes <- paste(alternative_postal_codes, extracted_postal_codes, sep = " ")
      }
    }

    if(extract_postal_code_value == "yes"){
      alternative_postal_codes <- stri_trim_both(alternative_postal_codes)

      # Call the standardizing_script function to de-duplicate alt postal codes.
      df <- concat_postal_codes(df, alternative_postal_codes)
    }

    df[[standardized_col_name]] <- processed_addresses
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)

  }

  #----
  # pre_process_city()
  # ~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_city() function will take in fields relating to a persons
  #   city, and will standardize to MH data standards.
  #----
  pre_process_city <- function(city_fields, source_field_ids, df, standardized_col_name){

    processed_city <- character(nrow(df))
    alternative_postal_codes <- character(nrow(df))

    for (i in seq_along(city_fields)) {
      curr_city_field <- city_fields[i]
      unprocessed_cities <- source_data_frame[[curr_city_field]]

      # Get the extract postal codes value
      extract_postal_code_value <- flag_lookup["extract_postal_code"]

      if(extract_postal_code_value == "yes"){
        # Extract the postal code (if there is one) from the address strings, remove
        # them from the original string, and then append to their own column
        return_vars <- extract_postal_codes(unprocessed_cities, df)

        # Use the return variables
        unprocessed_cities <- return_vars$orig_field
        extracted_postal_codes <- return_vars$p_codes
      }

      # Use the standardize_addresses() function along with our look up flags
      # to standardize the cities
      curr_cities <- standardize_addresses(unprocessed_cities, flag_lookup)

      if (i == 1) {
        processed_city <- curr_cities
        if(extract_postal_code_value == "yes")
          alternative_postal_codes <- paste(alternative_postal_codes, extracted_postal_codes, sep = " ")
      } else {
        processed_city <- paste(processed_city, curr_cities, sep = " ")
        if(extract_postal_code_value == "yes")
          alternative_postal_codes <- paste(alternative_postal_codes, extracted_postal_codes, sep = " ")
      }
    }
    # Call the standardizing_script function to de-duplicate alt postal codes.
    if(extract_postal_code_value == "yes")
      df <- concat_postal_codes(df, alternative_postal_codes)


    df[[standardized_col_name]] <- processed_city
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)
  }

  #----
  # pre_process_province()
  # ~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_province() function will take in fields relating to the
  #   province of a Manitoba citizen and pre-process it to keep it standardized
  #   across all data sets.
  #----
  pre_process_province <- function(province_fields, source_field_ids, df, standardized_col_name){

    # Create an empty data-frame for processed genders
    processed_provinces <- character(nrow(df))

    # Vectorized standardization of gender values for all gender fields
    for (i in seq_along(province_fields)) {
      curr_province_field <- province_fields[i]
      curr_source_field_id <- source_field_ids[i]
      unprocessed_provinces <- source_data_frame[[curr_province_field]]

      # Create a query to join categorical_fields and categorical_values tables for the current field
      query <- paste("SELECT cf.source_field_id, cf.source_value, cv.standardized_value",
                     "FROM categorical_fields cf",
                     "JOIN categorical_values cv ON cf.standardized_value_id = cv.standardized_value_id",
                     "WHERE cf.source_field_id = ", curr_source_field_id)
      dataset_province_vals <- dbSendQuery(db_conn, query)
      province_vals <- dbFetch(dataset_province_vals)
      dbClearResult(dataset_province_vals)

      # Create a lookup table for the current province field
      province_lookup <- setNames(province_vals$standardized_value, tolower(province_vals$source_value))

      # Vectorized standardization of gender values for the current field
      standardized_provinces <- province_lookup[tolower(unprocessed_provinces)]

      # Replace NA values
      standardized_provinces[is.na(standardized_provinces)] <- ""

      # Append to the processed_genders vector
      processed_provinces <- paste(processed_provinces, standardized_provinces, sep = " ")
    }

    # Combine the standardized values for all gender fields
    df[[standardized_col_name]] <- trimws(processed_provinces)

    return(df)
  }

  #----
  # pre_process_country()
  # ~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_country() function will take in fields relating to a persons
  #   country, and will standardize to MH data standards.
  #----
  pre_process_country <- function(country_fields, source_field_ids, df, standardized_col_name){

    processed_countries <- character(nrow(df))
    alternative_postal_codes <- character(nrow(df))

    for (i in seq_along(country_fields)) {
      curr_country_field <- country_fields[i]
      unprocessed_countries <- source_data_frame[[curr_country_field]]

      # Get the extract postal codes value
      extract_postal_code_value <- flag_lookup["extract_postal_code"]

      if(extract_postal_code_value == "yes"){
        # Extract the postal code (if there is one) from the address strings, remove
        # them from the original string, and then append to their own column
        return_vars <- extract_postal_codes(unprocessed_countries, df)

        # Use the return variables
        unprocessed_countries <- return_vars$orig_field
        extracted_postal_codes <- return_vars$p_codes
      }

      # Use the standardize_addresses() function along with our look up flags
      # to standardize the countries
      curr_country <- standardize_addresses(unprocessed_countries, flag_lookup)

      if (i == 1) {
        processed_countries <- curr_country
        if(extract_postal_code_value == "yes")
          alternative_postal_codes <- paste(alternative_postal_codes, extracted_postal_codes, sep = " ")
      } else {
        processed_countries <- paste(processed_countries, curr_country, sep = " ")
        if(extract_postal_code_value == "yes")
          alternative_postal_codes <- paste(alternative_postal_codes, extracted_postal_codes, sep = " ")
      }
    }

    # Call the standardizing_script function to de-duplicate alt postal codes.
    if(extract_postal_code_value == "yes")
      df <- concat_postal_codes(df, alternative_postal_codes)

    df[[standardized_col_name]] <- processed_countries
    # Remove leading and trailing white-space from the entire column
    df[[standardized_col_name]] <- stri_trim_both(df[[standardized_col_name]])
    return(df)

  }

  #----
  # pre_process_postal_code()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_postal_code() function will take in fields relating to a
  #   persons postal codes, and will standardize to MH data standards.
  #----
  pre_process_postal_code <- function(postal_code_fields, source_field_ids, df, standardized_col_name) {

    processed_postal_codes <- character(nrow(df))

    for (curr_postal_code_field in postal_code_fields) {
      unprocessed_postal_codes <- source_data_frame[[curr_postal_code_field]]

      # Replace NA values with empty string
      unprocessed_postal_codes[is.na(unprocessed_postal_codes)] <- ""

      # Attempt to extract postal codes using two regular expressions
      postal_codes <- stri_extract_all_regex(unprocessed_postal_codes, "[A-Za-z]{1}[0-9]{1}[A-Za-z]{1}[0-9]{1}[A-Za-z]{1}[0-9]{1}|[A-Za-z]{1}[0-9]{1}[A-Za-z] {1}[0-9]{1}[A-Za-z]{1}[0-9]{1}")

      # Replace NA values with empty string
      postal_codes[is.na(postal_codes)] <- ""

      # Append to the data frame
      df[[standardized_col_name]] <- paste(df[[standardized_col_name]], trimws(postal_codes), sep = " ")
      df[[standardized_col_name]] <- trimws(df[[standardized_col_name]])

      # If multiple postal codes were extracted, then clean up the record
      df[[standardized_col_name]] <- gsub('c\\(', "", df[[standardized_col_name]], perl = TRUE)

      # Vectorized standardization of postal codes
      df[[standardized_col_name]] <- str_replace_all(df[[standardized_col_name]], "[^A-Za-z0-9]", "")

      # Convert all postal codes to uppercase
      df[[standardized_col_name]] <- str_to_upper(df[[standardized_col_name]])

      # Insert a space every 6 characters using a regular expression
      df[[standardized_col_name]] <- str_replace_all(df[[standardized_col_name]], "(.{6})", "\\1 ")

      # Replace invalid values in the postal code
      df[[standardized_col_name]] <- str_replace_all(df[[standardized_col_name]], "O", "0")
      df[[standardized_col_name]] <- str_replace_all(df[[standardized_col_name]], "I", "1")
    }

    df[[standardized_col_name]] <- trimws(df[[standardized_col_name]])

    return(df)
  }

  #----
  # pre_process_acquisition_date()
  # ~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_acquisition_date() function will take in a list of fields
  #   involving the acquisition date of a record.
  #----
  pre_process_acquisition_date <- function(date_fields, source_field_ids, df, standardized_col_name){

    for (i in seq_along(date_fields)) {
      curr_date_field <- date_fields[i]
      curr_source_field_id <- source_field_ids[i]

      # Check for compound character records
      check_compound_records_query <- paste("SELECT * FROM compound_fields where source_field_id = ?;")
      compound_records <- dbSendQuery(db_conn, check_compound_records_query)
      dbBind(compound_records, list(curr_source_field_id))
      output_compound_records <- dbFetch(compound_records)
      compound_field_format_id <- output_compound_records$compound_field_format_id
      dbClearResult(compound_records)

      # If the compound record query returns a result, then process it that way
      if (!is.null(compound_field_format_id)) {
        unprocessed_acquisition_dates <- source_data_frame[[curr_date_field]]

        # Query to grab the compound field's separators
        get_separators_query <- paste("SELECT separator FROM compound_field_separators
                                            WHERE compound_field_format_id = ? and separator is not NULL
                                            ORDER BY separator_order asc;")
        compound_field_separators <- dbSendQuery(db_conn, get_separators_query)
        dbBind(compound_field_separators, list(compound_field_format_id))
        separator_output <- dbFetch(compound_field_separators)
        dbClearResult(compound_field_separators)

        # Query to grab the compound field's substring indexes
        get_separators_query <- paste("SELECT substring_index FROM compound_field_separators
                                            WHERE compound_field_format_id = ? and substring_index is not NULL
                                            ORDER BY separator_order asc;")
        compound_field_indexes <- dbSendQuery(db_conn, get_separators_query)
        dbBind(compound_field_indexes, list(compound_field_format_id))
        index_output <- dbFetch(compound_field_indexes)
        dbClearResult(compound_field_indexes)

        # Initialize the separators potentially used by the split_compound_field function
        separators <- separator_output$separator

        # Initialize the indexes potentially used by the split_compound_field function
        substring_indexes <- index_output$substring_index

        if(length(substring_indexes) > 0){
          #print(unprocessed_birth_dates)
          split_dates <- split_compound_field(unprocessed_acquisition_dates, substring_indexes, "indexes")
        }
        else{
          #print(unprocessed_birth_dates)
          split_dates <- split_compound_field(unprocessed_acquisition_dates, separators, "separators")
        }

        # Query to grab the destination fields for each split value (ADDED LEFT JOIN)
        get_destination_field_names_query <- paste("SELECT destination_field_name FROM compound_field_destinations cfd
                                                      LEFT JOIN destination_fields df on cfd.destination_field_id = df.destination_field_id
                                                      WHERE compound_field_format_id = ?
                                                      ORDER BY destination_mapping_order asc;")
        compound_field_destination_names <- dbSendQuery(db_conn, get_destination_field_names_query)
        dbBind(compound_field_destination_names, list(compound_field_format_id))
        compound_field_names_output <- dbFetch(compound_field_destination_names)
        dbClearResult(compound_field_destination_names)

        # Initialize the destination field names for the split compound field
        destination_field_names <- compound_field_names_output$destination_field_name
        #print(destination_field_names)

        #-- Need to add condition here for in-case we have multiple date fields! --#
        for(i in seq_along(destination_field_names)){
          if(!is.na(destination_field_names[i])){
            # Define month abbreviations
            month_lookup <- c(tolower(month.abb), tolower(month.name))

            # Convert non-numeric values to NA
            non_numeric_vals <- as.character(split_dates[[i]])
            non_numeric_vals[!grepl("^\\d+$", non_numeric_vals)] <- NA

            # Match non-NA values with month abbreviations and full names
            standardized_date <- match(tolower(split_dates[[i]]), month_lookup)

            # Use modulo 12 on standardized_date, and replace 0 values (December) with 12
            standardized_date <- as.integer(standardized_date)%%12
            standardized_date[standardized_date == 0] <- 12

            # Replace non-NA values in standardized_date with original numeric values
            standardized_date[!is.na(non_numeric_vals)] <- split_dates[[i]][!is.na(non_numeric_vals)]

            # Remove leading 0's from the standardize dates (We can make this add a leading 0 later if we want)
            standardized_date <- str_remove(standardized_date, "^0+")

            df[[destination_field_names[i]]] <- paste(df[[destination_field_names[i]]], standardized_date, sep = " ")
            df[[destination_field_names[i]]] <- trimws(df[[destination_field_names[i]]])
          }
        }
      }
      else {
        print("No records found.")
      }
    }

    return(df)
  }

  #----
  # pre_process_acquisition_year()
  # ~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_acquisition year() function will pre-process dates split
  #   up such that we already have access to the year.
  #----
  pre_process_acquisition_year <- function(year_field, source_field_ids, df, standardized_col_name){
    processed_acquisition_years <- character(nrow(df))

    for (curr_acquisition_year_field in year_field) {
      unprocessed_acquisition_years <- source_data_frame[[curr_acquisition_year_field]]

      # Replace all non-numerical characters with ''
      unprocessed_acquisition_years <- gsub("[^0-9.-]", "", unprocessed_acquisition_years)

      # Replace a string made entirely of '0's with a blank character
      unprocessed_acquisition_years <- gsub("^0+$", "", unprocessed_acquisition_years)

      # Remove leading 0's
      unprocessed_acquisition_years <- str_remove(unprocessed_acquisition_years, "^0+")

      if (curr_acquisition_year_field == year_field[1]) {
        processed_acquisition_years <- unprocessed_acquisition_years
      } else {
        processed_acquisition_years <- paste(processed_acquisition_years, unprocessed_acquisition_years, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_acquisition_years)
    return(df)

  }

  #----
  # pre_process_acquisition_month()
  # ~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_acquisition_month() function will pre-process dates split
  #   up such that we already have access to the month
  #----
  pre_process_acquisition_month <- function(month_field, source_field_ids, df, standardized_col_name){
    processed_acquisition_months <- character(nrow(df))

    for (curr_acquisition_month_field in month_field) {
      unprocessed_acquisition_months <- source_data_frame[[curr_acquisition_month_field]]

      # Define month abbreviations
      month_lookup <- c(tolower(month.abb), tolower(month.name))

      # Convert non-numeric values to NA
      non_numeric_vals <- as.character(unprocessed_acquisition_months)
      non_numeric_vals[!grepl("^\\d+$", non_numeric_vals)] <- NA

      # Match non-NA values with month abbreviations and full names
      standardized_months <- match(tolower(unprocessed_acquisition_months), month_lookup)

      # User modulo 12 on standardized_months, and replace 0 values (december) with 12
      standardized_months <- as.integer(standardized_months)%%12
      standardized_months[standardized_months == 0] <- 12

      # Replace non-NA values in standardized_months with original numeric values
      standardized_months[!is.na(non_numeric_vals)] <- unprocessed_acquisition_months[!is.na(non_numeric_vals)]

      # Remove leading 0's
      standardized_months <- str_remove(standardized_months, "^0+")

      if (curr_acquisition_month_field == month_field[1]) {
        processed_acquisition_months <- standardized_months
      } else {
        processed_acquisition_months <- paste(processed_acquisition_months, standardized_months, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_acquisition_months)
    return(df)
  }

  #----
  # pre_process_acquisition_day()
  # ~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_acquisition_day() function will pre-process dates split up
  #   such that we already have access to the day.
  #----
  pre_process_acquisition_day <- function(day_field, source_field_ids, df, standardized_col_name){
    processed_acquisition_days <- character(nrow(df))

    for (curr_acquisition_day_field in day_field) {
      unprocessed_acquisition_days <- source_data_frame[[curr_acquisition_day_field]]

      # Replace all non-numerical characters with ''
      unprocessed_acquisition_days <- gsub("[^0-9.-]", "", unprocessed_acquisition_days)

      # Replace a string made entirely of '0's with a blank character
      unprocessed_acquisition_days <- gsub("^0+$", "", unprocessed_acquisition_days)

      # Remove leading 0's
      unprocessed_acquisition_days <- str_remove(unprocessed_acquisition_days, "^0+")

      if (curr_acquisition_day_field == day_field[1]) {
        processed_acquisition_days <- unprocessed_acquisition_days
      } else {
        processed_acquisition_days <- paste(processed_acquisition_days, unprocessed_acquisition_days, sep = " ")
      }
    }

    df[[standardized_col_name]] <- trimws(processed_acquisition_days)
    return(df)
  }

  #----
  # pre_process_compound_name()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_compound_name() function will pre-process names that are
  #   not in already split up fields, this will grab the format and separator
  #   symbols from the metadata data set and use it to split into the proper
  #   variables.
  #
  #   TO-DO: Change the compound processing to use the MUCH better method in the
  #          other name_split R file.
  #----
  pre_process_compound_name <- function(compound_name_fields, source_field_ids, df, standardized_col_name){

    for (i in seq_along(compound_name_fields)) {
      curr_name_field <- compound_name_fields[i]
      curr_source_field_id <- source_field_ids[i]

      # Check for compound character records
      check_compound_records_query <- paste("SELECT * FROM compound_fields where source_field_id = ?;")
      compound_records <- dbSendQuery(db_conn, check_compound_records_query)
      dbBind(compound_records, list(curr_source_field_id))
      output_compound_records <- dbFetch(compound_records)
      dbClearResult(compound_records)

      # Initialize the compound field format id to use in the separator query
      compound_field_format_id <- output_compound_records$compound_field_format_id

      # Query to grab the compound field's separators
      get_separators_query <- paste("SELECT separator FROM compound_field_separators
                                            WHERE compound_field_format_id = ?
                                            ORDER BY separator_order asc;")
      compound_field_separators <- dbSendQuery(db_conn, get_separators_query)
      dbBind(compound_field_separators, list(compound_field_format_id))
      separator_output <- dbFetch(compound_field_separators)
      dbClearResult(compound_field_separators)

      # Initialize the separators used by the split_compound_field function
      separators <- separator_output$separator

      unprocessed_names <- source_data_frame[[curr_name_field]]

      # Split up the unprocessed names using the split_compound_field function
      split_names <- split_compound_field(unprocessed_names, separators, "separators")

      # Query to grab the destination fields for each split value (CHANGED TO LEFT JOIN)
      get_destination_field_names_query <- paste("SELECT destination_field_name FROM compound_field_destinations cfd
                                                      LEFT JOIN destination_fields df on cfd.destination_field_id = df.destination_field_id
                                                      WHERE compound_field_format_id = ?
                                                      ORDER BY destination_mapping_order asc;")
      compound_field_destination_names <- dbSendQuery(db_conn, get_destination_field_names_query)
      dbBind(compound_field_destination_names, list(compound_field_format_id))
      compound_field_names_output <- dbFetch(compound_field_destination_names)
      dbClearResult(compound_field_destination_names)

      # Initialize the destination field names for the split compound field
      destination_field_names <- compound_field_names_output$destination_field_name

      for(i in seq_along(destination_field_names)){
        if(!is.na(destination_field_names[i])){
          curr_name <- standardize_names(split_names[[i]], flag_lookup)
          df[[destination_field_names[i]]] <- paste(df[[destination_field_names[i]]], curr_name, sep = " ")
          df[[destination_field_names[i]]] <- trimws(df[[destination_field_names[i]]])
        }
      }
    }
    return(df)
  }

  #----
  # pre_process_numeric_date()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_numeric_date() function will pre-process dates that use
  #   integers for birth and acquisition dates.
  #----
  pre_process_numeric_date <- function(numeric_date_fields, source_field_ids, df, standardizied_col_name){

    for (i in seq_along(numeric_date_fields)) {
      curr_date_field <- numeric_date_fields[i]
      curr_source_field_id <- source_field_ids[i]

      # Check for numeric records
      check_numeric_records_query <- paste("SELECT * FROM numeric_date_fields where source_field_id = ?;")
      numeric_records <- dbSendQuery(db_conn, check_numeric_records_query)
      dbBind(numeric_records, list(curr_source_field_id))
      output_numeric_records <- dbFetch(numeric_records)
      numeric_date_format_id <- output_numeric_records$numeric_date_format_id
      destination_field_type <- output_numeric_records$numeric_destination_type
      dbClearResult(numeric_records)

      if (!is.null(numeric_date_format_id)) {
        # Get the numeric format of the dataset, which includes the origin date,
        # unit of time measurement, etc.
        get_numeric_format_query <- paste("SELECT * FROM numeric_date_formats where numeric_date_format_id = ?;")
        numeric_format <- dbSendQuery(db_conn, get_numeric_format_query)
        dbBind(numeric_format, list(numeric_date_format_id))
        output_numeric_format <- dbFetch(numeric_format)
        origin_date      <- output_numeric_format$origin_date
        time_measurement <- output_numeric_format$units_label
        #destination_type <- output_numeric_format$numeric_destination_type
        dbClearResult(numeric_format)

        # Grab the unprocessed birth date timings
        unprocessed_dates <- source_data_frame[[curr_date_field]]

        # Replace all non-numeric characters with a blank space
        unprocessed_dates <- gsub("[^0-9]", "", unprocessed_dates)

        # Convert unprocessed_dates to integers
        unprocessed_dates <- suppressWarnings(as.integer(unprocessed_dates))

        # Conversion for Dates
        if(time_measurement == "Days"){
          actual_date = as.POSIXct(unprocessed_dates  * 86400, origin = origin_date, tz = "GMT") #CST6CDT
          formatted_datetime = format(actual_date, "%Y %m %d")
        }
        else if (time_measurement == "Hours"){
          actual_date = as.POSIXct(unprocessed_dates * 3600, origin = origin_date, tz = "GMT")
          formatted_datetime = format(actual_date, "%Y %m %d")
        }
        else if (time_measurement == "Seconds"){
          actual_date = as.POSIXct(unprocessed_dates, origin = origin_date, tz = "GMT")
          formatted_datetime = format(actual_date, "%Y %m %d")
        }
        # Split the dates up using the split_compound_field function from our
        # flag_standardizing_script.R file
        split_dates <- split_compound_field(formatted_datetime, c(" ", " "), "separators")

        # Query to grab the destination fields for each split value
        get_destination_field_names_query <- paste("SELECT destination_field_name FROM numeric_date_destinations ndd
                                                      JOIN destination_fields df on ndd.destination_field_id = df.destination_field_id
                                                      WHERE numeric_destination_type = ?
                                                      ORDER BY destination_mapping_order asc;")
        compound_field_destination_names <- dbSendQuery(db_conn, get_destination_field_names_query)
        dbBind(compound_field_destination_names, list(destination_field_type))
        compound_field_names_output <- dbFetch(compound_field_destination_names)
        dbClearResult(compound_field_destination_names)

        # Initialize the destination field names for the split fields
        destination_field_names <- compound_field_names_output$destination_field_name

        #-- Need to add condition here for incase we have multiple date fields! --#
        for(i in seq_along(destination_field_names)){
          split_dates[[i]] <- as.numeric(split_dates[[i]])
          split_dates[[i]][is.na(split_dates[[i]])] <- ""
          df[[destination_field_names[i]]] <- paste(df[[destination_field_names[i]]], split_dates[[i]], sep = " ")
          df[[destination_field_names[i]]] <- trimws(df[[destination_field_names[i]]])
        }
      }
      else {
        print("No records found.")
      }
    }

    return(df)
  }

  #----
  # pre_process_pass_through_field()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_pass_through_field() function will handle linkage fields
  #   that do not require processing by passing them into the clean dataset, using
  #   the same field name as the raw dataset.
  #----
  pre_process_pass_through_field <- function(pass_through_fields, source_field_ids, df, standardized_col_name){
    for (curr_field in pass_through_fields) {
      pass_through_values <- source_data_frame[[curr_field]]
      df[[curr_field]] <- trimws(pass_through_values)
    }
    return(df)
  }

  #----
  # pre_process_record_priority()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #   The pre_process_record_priority() function will pass through linkage fields
  #   that are used to break ties on pairs, the field names are passed through as
  #   is along with their values, while the priorities are stored along with the
  #   metadata.
  #----
  pre_process_record_priority <- function(record_priority_fields, source_field_ids, df, standardized_col_name){
    for (curr_field in record_priority_fields) {
      record_priority_values <- source_data_frame[[curr_field]]
      df[[curr_field]]       <- trimws(record_priority_values)
    }
    return(df)
  }

  #----------------------------------------------------------------------------#

  #----
  #-- RUN A FOR LOOP, GO THROUGH ALL STANDARD FIELDS THEN NORMALIZE THE DATA --#
  for (name in names(split_source_fields)) {
    split <- split_source_fields[[name]]

    #-- Dynamically call the pre-process function using standardizing module --#
    if(name != "" && !is.na(name)){
      standardized_name <- standardized_name_lookup_table[name]
      function_name     <- standardized_function_lookup_table[name]

      if(flag_lookup["debug_mode"] == "on"){
        print(paste("Function Name:", function_name))
        print(paste(cat("\t"), split$source_field_name))
      }

      pre_process_function <- paste0("pre_process_", tolower(function_name))

      if(flag_lookup["debug_mode"] == "on")
        s <- proc.time()

      processed_data_frame <- do.call(pre_process_function, list(split$source_field_name, split$source_field_id, processed_data_frame, standardized_name))

      if(flag_lookup["debug_mode"] == "on"){
        e <- proc.time()
        paste(cat("\t"), print(e - s))
        cat("\n")
      }

      gc()
    }
    #---------------------------------------------------------------------------#
  }
  #----

  # Once we finish processing all the original field, if the user wants to impute
  # missing gender values, we will call the impute_sex() function using the
  # values we grabbed earlier
  impute_sex_value <- flag_lookup["impute_sex"]
  sex_impute_type  <- flag_lookup["impute_sex_type"]
  if(impute_sex_value == "yes" && sex_impute_type != "internal"){
    processed_data_frame <- impute_sex(processed_data_frame, temporary_names, temporary_genders, flag_lookup)
  }

  return(processed_data_frame)
}

#' Pre Process Chunks
#'
#' The pre_process_chunks() function takes in a path to a file, metadata, the
#' dataset_id of what dataset we're standardizing, as well as pre_processing flags
#' used in the pre_process_data function, and an output folder to move the
#' standardized file to.
#' @param file_path A path to a file.
#' @param dataset_metadata_conn A database connection that is used in addition to the dataset_id, passed from the standardize_data() function. Is also used in the pre_process_data() function.
#' @param dataset_id A dataset_id found and passed from the standardize_data() function, is used to make various calls to the database connection argument to obtain information required to pre-process the health data.
#' @param flag_lookup_table A lookup table containing manually set or default values used by the flag script which will use the values of the flags to determine how to pre-process and output the health data.
#' @param output_folder An output folder that the successfully cleaned data ready for linkage will be placed in once pre-processing is complete.
#' @param dataset_code A dataset code used as prefix for the successful output and error files.
#' @return A clean database connection with a single table called "cleaned_data_table" which contains the processed fields from the input file.
#' @examples
#' clean_db_conn <- pre_process_chunks("path/to/file.txt", db, 1, flag_lookup, "path/to/folder", "fakecode")
#' @export
pre_process_chunks <- function(file_path, dataset_metadata_conn, dataset_id, flag_lookup_table, output_folder, dataset_code){
  # Create reactive value (in case of UI)
  total_rows_read <- reactiveVal()

  # Get the important source fields
  chosen_fields <- dbSendQuery(dataset_metadata_conn, 'SELECT * FROM source_fields WHERE dataset_id = $chosen_dataset ORDER BY field_order asc;')
  dbBind(chosen_fields, list(chosen_dataset=dataset_id))
  output_fields <- dbFetch(chosen_fields)
  dbClearResult(chosen_fields)

  if(flag_lookup_table["debug_mode"] == "on"){
    print(paste("Source Fields:", output_fields$source_field_name))
    cat("\n")
  }

  # Get the field orders so that if the file has a header, we can determine what specific columns to grab
  column_numbers <- output_fields$field_order

  if(flag_lookup_table["debug_mode"] == "on"){
    print(paste("Field Orders:", column_numbers))
    cat("\n")
  }

  # Does the dataset have a header already? If so then we'll have to skip it
  requires_header <- dbSendQuery(dataset_metadata_conn, 'SELECT * FROM datasets WHERE dataset_id = $chosen_dataset;')
  dbBind(requires_header, list(chosen_dataset=dataset_id))
  output_header <- dbFetch(requires_header)
  dataset_requires_header <- output_header$has_header
  dbClearResult(requires_header)

  if(flag_lookup_table["debug_mode"] == "on"){
    if(dataset_requires_header == 0)
      cat("Dataset does not require a header.\n")
    else
      cat("Dataset requires a header.\n")
  }

  # Get the file type from the metadata
  file_type_query <- paste("SELECT file_extension_code FROM datasets",
                           "INNER JOIN file_formats ON file_formats.file_format_id = datasets.file_format_id",
                           "WHERE dataset_id = ?;")
  file_type <- dbSendQuery(dataset_metadata_conn, file_type_query)
  dbBind(file_type, list(dataset_id))
  output_file_type <- dbFetch(file_type)
  my_file_type <- output_file_type$file_extension_code
  dbClearResult(file_type)

  if(flag_lookup_table["debug_mode"] == "on"){
    print(paste("File type:", my_file_type))
    cat("\n")
  }

  #Get the number of fields that the data SHOULD have
  num_of_expected_fields <- nrow(output_fields)

  if(flag_lookup_table["debug_mode"] == "on"){
    print(paste("We are expecting [", num_of_expected_fields, "] fields."))
    cat("\n")
  }

  # Split the output fields data up
  split_data <- split(output_fields, output_fields$standardizing_module_id)

  # Get the column names for the processed data frame, do it here so that we
  # don't need to re-obtain all the column names and look up table for
  # standardized names
  col_names <- c()
  for (name in names(split_data)) {
    split <- split_data[[name]]

    # Use the module/function name as the temporary column name for standardization
    standardizing_name_query <- paste("SELECT distinct standardizing_module_name FROM source_fields sf",
                                     "INNER JOIN standardizing_modules sm ON sm.standardizing_module_id = sf.standardizing_module_id",
                                     "WHERE sf.standardizing_module_id = ? and dataset_id = ?;")
    standardizing_name <- dbSendQuery(dataset_metadata_conn, standardizing_name_query)
    dbBind(standardizing_name, list(name, dataset_id))
    output_standardizing_name <- dbFetch(standardizing_name)
    my_standardized_name <- output_standardizing_name$standardizing_module
    dbClearResult(standardizing_name)

    if(output_standardizing_name != ""){
      col_names = append(col_names, output_standardizing_name)
    }
  }

  # Query to get the standardized output names
  query1 <- paste("SELECT distinct sf.standardizing_module_id, df.destination_field_name",
                  "FROM source_fields sf",
                  "INNER JOIN standardizing_modules sm ON sf.standardizing_module_id = sm.standardizing_module_id",
                  "INNER JOIN destination_fields df ON df.destination_field_id = sm.destination_field_id",
                  "WHERE dataset_id = ", dataset_id)
  dataset_standardized_names_vals <- dbSendQuery(dataset_metadata_conn, query1)
  standardized_names_vals <- dbFetch(dataset_standardized_names_vals)
  dbClearResult(dataset_standardized_names_vals)

  # Query to get the field ids and matching standardized names for a look up table when processing
  query2 <- paste("SELECT distinct sm.standardizing_module_id, sm.standardizing_module_name",
                 "FROM source_fields sf",
                 "JOIN standardizing_modules sm ON sf.standardizing_module_id = sm.standardizing_module_id",
                 "WHERE dataset_id = ", dataset_id)
  module_names <- dbSendQuery(dataset_metadata_conn, query2)
  output_module_names <- dbFetch(module_names)
  dbClearResult(module_names)

  # Create a lookup table for standardized field ids and their matching names
  standardized_name_lookup <- setNames(standardized_names_vals$destination_field_name, standardized_names_vals$standardizing_module_id)
  standardized_function_lookup <- setNames(output_module_names$standardizing_module_name, output_module_names$standardizing_module_id)

  if(flag_lookup_table["debug_mode"] == "on"){
    print(paste("Standardized Name Lookup Table:", standardized_name_lookup), sep = "\n")
    cat("\n")
    print(paste("Standardized Function Name Lookup Table:", standardized_function_lookup), sep = "\n")
    cat("\n")
  }

  output_health_and_program_data_value <- flag_lookup_table["output_health_and_program_data"]
  if(output_health_and_program_data_value == "yes"){
    # Create the full path to the output SQLite file within the output folder
    output_sqlite_file2 <- file.path(output_folder, paste0(dataset_code, "_health_and_program_data.sqlite"))

    # Create a new data base (SQLite) that we can append to
    clean_db_conn2 <- dbConnect(RSQLite::SQLite(), output_sqlite_file2)
  }

  # Get the user defined chunking size
  chunking_size <- as.integer(flag_lookup_table["chunk_size"])
  if(flag_lookup_table["debug_mode"] == "on"){
    print(paste("Chunking Size:", chunking_size))
    cat("\n")
  }

  start <- proc.time()

  # Create a variable to store the clean file path, that way we can return a data frame
  clean_sqlite_path <- ""

  # If the file type is of .txt format, we will handle reading the file this way (FREAD)
  if(my_file_type == "txt"){
    # Set our chunking size using the input value
    chunk_size <- chunking_size

    # Create the full path to the output SQLite file within the output folder
    output_sqlite_file <- file.path(output_folder, paste0(dataset_code, "_cleaned_dataset_txt.sqlite"))
    clean_sqlite_path  <- output_sqlite_file

    # Create a new data base (SQLite) that we can append to
    clean_db_conn <- dbConnect(RSQLite::SQLite(), output_sqlite_file)

    # Establish variables for how many rows have currently been read, and each records primary key
    rows_read <- 0
    record_primary_key <- 1
    header_columns <- NULL

    tryCatch({
      # Run a while loop that will continue reading in chunks until we break or get an error
      while(TRUE){
        # Skip the header if it's not the first iteration, use "select = " to get only the columns we need
        if (rows_read == 0 && dataset_requires_header == 1) {
          header_columns <- as.character(fread(input = file_path, header = FALSE, nrow = 1, fill = TRUE))
          chunk_read <- fread(input = file_path, header = FALSE, nrows = chunk_size, skip = rows_read + 1, fill = TRUE, data.table = TRUE)
          rows_read <- 1
        }
        else{
          chunk_read <- fread(input = file_path, header = FALSE, nrows = chunk_size, skip = rows_read, fill = TRUE, data.table = TRUE)
        }
        # Call garbage collector
        gc()

        # Rename the read chunks column names
        if(!is.null(header_columns))
          colnames(chunk_read) <- header_columns

        # Extract the fields we need for linkage and pass the rest off.
        tryCatch({
          chunk <- select(chunk_read, all_of(column_numbers)) # Added "all_of() around col_nums
        },
        error = function(e){
          stop("ERROR: Check metadata and ensure the source file has a valid header row.")
        })


        #Get the number of fields that the data SHOULD have
        fields_found <- ncol(chunk)

        if(flag_lookup_table["debug_mode"] == "on")
          print(paste("We found [", fields_found, "] fields."))

        if(fields_found != num_of_expected_fields){
          stop(paste("Error In pre_process_chunks(), number of columns read does ",
                     "not match the expected number of columns. ", cat("\n"),
                     "Expected [",num_of_expected_fields, "] columns, found ",
                     "[", fields_found, "]", sep=""))
        }

        # Grab the column names from the metadata
        column_names <- output_fields$source_field_name
        colnames(chunk) <- column_names

        # Add a sequential record primary key to the chunk
        chunk[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(chunk) - 1)

        # Clean the data frame using our pre_process_data() function
        clean_dataframe <- pre_process_data(chunk, split_data, dataset_metadata_conn, dataset_id,
                                            standardized_name_lookup, standardized_function_lookup, flag_lookup_table)
        # Replace NA values with ""
        clean_dataframe[is.na(clean_dataframe)] <- ""

        # # Add record_primary_key column
        # clean_dataframe[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(clean_dataframe) - 1)

        # Append the clean_dataframe to the SQLite database
        dbWriteTable(clean_db_conn, "clean_data_table", clean_dataframe, append = TRUE)

        # If we are outputting the program and health data, run another function to grab all the data we didn't need to standardize
        output_health_and_program_data_value <- flag_lookup_table["output_health_and_program_data"]
        if(output_health_and_program_data_value == "yes"){
          # Call function in "flag_standardizing_script.R" to return a dataset containing the health/program data
          df <- compile_health_and_program_data(chunk_read, dataset_metadata_conn, dataset_id)
          df[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(clean_dataframe) - 1)
          dbWriteTable(clean_db_conn2, "clean_data_table", df, append = TRUE)
        }

        # Increment the record primary key counter
        record_primary_key <- record_primary_key + nrow(clean_dataframe)
        rows_read          <- rows_read + nrow(chunk)

        # Modify the reactive value with how many rows have been read, then send a notification
        total_rows_read(rows_read)
        tryCatch({
          showNotification(paste("Total Rows Read:", as.integer(total_rows_read())), type = "warning", closeButton = FALSE)
        },
        error = function(e){
          print("Can't send notification without a UI.")
        })

        # Determine how many rows were read on this iteration, and print the total amount read
        rows_read_chunk <- nrow(chunk)
        print(paste("Rows Read: ", rows_read))

        # Remove the cleaned data frame and read chunk, then call garbage collector
        rm(clean_dataframe, chunk)
        gc()

        # If the most recently read chunk is less than our chunking size, we've read the last bit so break
        if(rows_read_chunk < chunk_size)
          break
      }
    },
    error = function(e){
      errmsg <- geterrmessage()
      if(grepl("skip=", errmsg, fixed = TRUE) == FALSE)
        error_handle(dataset_metadata_conn, errmsg, clean_db_conn, file_path, output_folder)
      else
        print(errmsg)
    })
    print("Finished Reading!")

    # Last check to see if gender imputation is to be done internally
    sex_impute_type  <- flag_lookup_table["impute_sex_type"]
    impute_sex_value <- flag_lookup_table["impute_sex"]
    if(sex_impute_type == "internal" && impute_sex_value == "yes"){
      source_df <- dbReadTable(clean_db_conn, 'clean_data_table')
      final_df  <- impute_sex(source_df, source_df$primary_given_name, source_df$gender, flag_lookup_table)
      dbWriteTable(clean_db_conn, "clean_data_table", final_df, overwrite = TRUE)
    }

    # Determine whether the user wants the file output in a different format
    standardize_file_output(clean_db_conn, output_folder, flag_lookup_table, dataset_code)

    # Determine whether the user wants the non-linkage data in a different format
    if(flag_lookup_table["output_health_and_program_data"] == "yes"){
      standardize_file_output(clean_db_conn2, output_folder, flag_lookup_table, paste0(dataset_code, "_non_linkage"))
    }

    # Disconnect from the database
    dbDisconnect(clean_db_conn)
  }
  # If the file type is of .csv format, we will handle reading the file this way (FREAD)
  else if (my_file_type == "csv"){
    # Set our chunking size
    chunk_size <- chunking_size

    # Create the full path to the output SQLite file within the output folder
    output_sqlite_file <- file.path(output_folder, paste0(dataset_code, "_cleaned_dataset_csv.sqlite"))
    clean_sqlite_path  <- output_sqlite_file

    # Create a new data base (SQLite) that we can append to
    clean_db_conn <- dbConnect(RSQLite::SQLite(), output_sqlite_file)

    # Establish variables for how many rows have currently been read, and each records primary key
    rows_read <- 0
    record_primary_key <- 1
    header_columns <- NULL

    tryCatch({
      while(TRUE){
        gc()

        # Skip the header if it's not the first iteration
        if (rows_read == 0 && dataset_requires_header == 1) {
          header_columns <- as.character(fread(input = file_path, header = FALSE, nrow = 1, fill = TRUE))
          chunk_read <- fread(input = file_path, header = FALSE, nrows = chunk_size, colClasses = "character", skip = rows_read + 1)
          rows_read <- 1
        }
        else{
          chunk_read <- fread(input = file_path, header = FALSE, nrows = chunk_size, colClasses = "character", skip = rows_read)
        }

        # Call garbage collector
        gc()

        # Rename the read chunks column names
        if(!is.null(header_columns))
          colnames(chunk_read) <- header_columns

        # Extract the fields we need for linkage and pass the rest off.
        chunk <- select(chunk_read, column_numbers)

        #Get the number of fields that the data SHOULD have
        fields_found <- ncol(chunk)
        if(flag_lookup_table["debug_mode"] == "on"){
          print(paste("We found [", fields_found, "] fields."))
          cat("\n")
        }

        if(fields_found != num_of_expected_fields){
          stop(paste("Error In pre_process_chunks(), number of columns read does ",
                     "not match the expected number of columns. ", cat("\n"),
                     "Expected [",num_of_expected_fields, "] columns, found ",
                     "[", fields_found, "]", sep=""))
        }

        # Grab the column names from the metadata
        column_names <- output_fields$source_field_name
        colnames(chunk) <- column_names

        # Add a sequential record primary key to the chunk
        chunk[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(chunk) - 1)

        # Clean the data frame using our pre_process_data() function
        clean_dataframe <- pre_process_data(chunk, split_data, dataset_metadata_conn, dataset_id,
                                            standardized_name_lookup, standardized_function_lookup, flag_lookup_table)

        # Replace NA values with ""
        clean_dataframe[is.na(clean_dataframe)] <- ""

        # # Add record_primary_key column
        # clean_dataframe[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(clean_dataframe) - 1)

        # Append the clean_dataframe to the SQLite database
        dbWriteTable(clean_db_conn, "clean_data_table", clean_dataframe, append = TRUE)

        # If we are outputting the program and health data, run another function
        # to grab all the data we didn't need to standardize
        output_health_and_program_data_value <- flag_lookup_table["output_health_and_program_data"]
        if(output_health_and_program_data_value == "yes"){
          # Call function in "flag_standardizing_script.R" to return a dataset containing the health/program data
          df <- compile_health_and_program_data(chunk_read, dataset_metadata_conn, dataset_id)
          df[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(clean_dataframe) - 1)
          dbWriteTable(clean_db_conn2, "clean_data_table", df, append = TRUE)
        }

        # Increment the record primary key counter
        record_primary_key <- record_primary_key + nrow(clean_dataframe)

        # Determine how many rows were read and print it to console
        rows_read <- rows_read + nrow(chunk)
        print(paste("Rows Read: ", rows_read))

        # Modify the reactive value with how many rows have been read, then send a notification
        total_rows_read(rows_read)
        tryCatch({
          showNotification(paste("Total Rows Read:", as.integer(total_rows_read())), type = "warning", closeButton = FALSE) #put a try/catch around this?
        },
        error = function(e){
          print("Can't send notification without a UI.")
        })

        # Get the rows read from this current chunk
        rows_read_chunk <- nrow(chunk)

        # Remove the cleaned data frame and read chunk, then call garbage collector
        rm(clean_dataframe, chunk)
        gc()

        # If the size of the read chunk is less than our chunk size, break out of the loop
        if(rows_read_chunk < chunk_size)
          break
      }
    },
    error = function(e){
      errmsg <- geterrmessage()
      if(grepl("skip=", errmsg, fixed = TRUE) == FALSE)
        error_handle(dataset_metadata_conn, errmsg, clean_db_conn, file_path, output_folder)
      else
        print(errmsg)
    })

    print("Finished Reading!")

    # Last check to see if gender imputation is to be done internally
    sex_impute_type  <- flag_lookup_table["impute_sex_type"]
    impute_sex_value <- flag_lookup_table["impute_sex"]
    if(sex_impute_type == "internal" && impute_sex_value == "yes"){
      source_df <- dbReadTable(clean_db_conn, 'clean_data_table')
      final_df  <- impute_sex(source_df, source_df$primary_given_name, source_df$gender, flag_lookup_table)
      dbWriteTable(clean_db_conn, "clean_data_table", final_df, overwrite = TRUE)
    }

    # Determine whether the user wants the file output in a different format
    standardize_file_output(clean_db_conn, output_folder, flag_lookup_table, dataset_code)

    # Determine whether the user wants the non-linkage data in a different format
    if(flag_lookup_table["output_health_and_program_data"] == "yes"){
      standardize_file_output(clean_db_conn2, output_folder, flag_lookup_table, paste0(dataset_code, "_non_linkage"))
    }

    # Disconnect from the database connection
    dbDisconnect(clean_db_conn)

  }
  # If the file type is of .sas7bdat format, we will handle reading the file this way (READ_SAS)
  else if (my_file_type == "sas7bdat"){
    # Set our chunking size
    chunk_size <- chunking_size

    # Create the full path to the output SQLite file within the output folder
    output_sqlite_file <- file.path(output_folder, paste0(dataset_code, "_cleaned_dataset_sas.sqlite"))
    clean_sqlite_path  <- output_sqlite_file

    # Create a new data base (SQLite) that we can append to
    clean_db_conn <- dbConnect(RSQLite::SQLite(), output_sqlite_file)

    # Establish variables for how many rows have currently been read, and each records primary key
    rows_read <- 0
    record_primary_key <- 1

    tryCatch({
      while(TRUE){
        #s <- proc.time()
        gc()

        # Skip the header if it's not the first iteration
        if (rows_read == 0 && dataset_requires_header == 1) {
          chunk_read <- read_sas(file_path, n_max = chunk_size, skip = rows_read + 1)
          rows_read <- 1
        }
        else{
          chunk_read <- read_sas(file_path, n_max = chunk_size, skip = rows_read)
        }

        # Call garbage collector
        gc()

        # Extract the fields we need for linkage and pass the rest off.
        chunk <- select(chunk_read, column_numbers)

        #Get the number of fields that the data SHOULD have
        fields_found <- ncol(chunk)
        if(flag_lookup_table["debug_mode"] == "on"){
          print(paste("We found [", fields_found, "] fields."))
          cat("\n")
        }

        # If the number of fields differs, throw an error
        if(fields_found != num_of_expected_fields){
          stop(paste("Error In pre_process_chunks(), number of columns read does ",
                     "not match the expected number of columns. ", cat("\n"),
                     "Expected [",num_of_expected_fields, "] columns, found ",
                     "[", fields_found, "]", sep=""))
        }

        # Grab the column names from the metadata
        column_names <- output_fields$source_field_name
        colnames(chunk) <- column_names

        # Add a sequential record primary key to the chunk
        chunk[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(chunk) - 1)

        # Clean the data frame using our pre_process_data() function
        clean_dataframe <- pre_process_data(chunk, split_data, dataset_metadata_conn, dataset_id,
                                            standardized_name_lookup, standardized_function_lookup, flag_lookup_table)

        # Replace NA values with ""
        clean_dataframe[is.na(clean_dataframe)] <- ""

        # # Add record_primary_key column
        # clean_dataframe[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(clean_dataframe) - 1)

        # Append the clean_dataframe to the SQLite database
        dbWriteTable(clean_db_conn, "clean_data_table", clean_dataframe, append = TRUE)

        # If we are outputting the program and health data, run another function to grab all the data we didn't need to standardize
        output_health_and_program_data_value <- flag_lookup_table["output_health_and_program_data"]
        if(output_health_and_program_data_value == "yes"){
          # Call function in "flag_standardizing_script.R" to return a dataset containing the health/program data
          df <- compile_health_and_program_data(chunk_read, dataset_metadata_conn, dataset_id)
          df[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(clean_dataframe) - 1)
          dbWriteTable(clean_db_conn2, "clean_data_table", df, append = TRUE)
        }

        # Increment the record primary key counter
        record_primary_key <- record_primary_key + nrow(clean_dataframe)

        # Determine how many rows were read on this iteration, and print the total number read to console
        chunk_rows <- nrow(chunk)
        rows_read <- rows_read + chunk_rows
        print(paste("Rows Read: ", rows_read))

        # Modify the reactive value with how many rows have been read, then send a notification
        total_rows_read(rows_read)
        tryCatch({
          showNotification(paste("Total Rows Read:", as.integer(total_rows_read())), type = "warning", closeButton = FALSE) #put a try/catch around this?
        },
        error = function(e){
          print("Can't send notification without a UI.")
        })

        # Remove the chunk and clean dataframe
        rm(chunk, clean_dataframe)
        gc()

        # If the size of the read chunk is less than our chunk size, break out of the loop
        if(chunk_rows < chunk_size){
          break
        }

      }
    },
    error = function(e){
      errmsg <- geterrmessage()
      if(grepl("skip=", errmsg, fixed = TRUE) == FALSE)
        error_handle(dataset_metadata_conn, errmsg, clean_db_conn, file_path, output_folder)
      else
        print(errmsg)
    })
    print("Finished Reading!")

    # Last check to see if gender imputation is to be done internally
    sex_impute_type  <- flag_lookup_table["impute_sex_type"]
    impute_sex_value <- flag_lookup_table["impute_sex"]
    if(sex_impute_type == "internal" && impute_sex_value == "yes"){
      source_df <- dbReadTable(clean_db_conn, 'clean_data_table')
      final_df  <- impute_sex(source_df, source_df$primary_given_name, source_df$gender, flag_lookup_table)
      dbWriteTable(clean_db_conn, "clean_data_table", final_df, overwrite = TRUE)
    }

    # Determine whether the user wants the file output in a different format
    standardize_file_output(clean_db_conn, output_folder, flag_lookup_table, dataset_code)

    # Determine whether the user wants the non-linkage data in a different format
    if(flag_lookup_table["output_health_and_program_data"] == "yes"){
      standardize_file_output(clean_db_conn2, output_folder, flag_lookup_table, paste0(dataset_code, "_non_linkage"))
    }

    # Close the SQLite database connection
    dbDisconnect(clean_db_conn)

  }
  # If the file_type is a fixed width format, we will handle reading the file this way (READL_LINES & READ_FWF)
  else if (my_file_type == "fwf"){
    # Set our chunking size
    chunk_size <- chunking_size

    # Create the full path to the output SQLite file within the output folder
    output_sqlite_file <- file.path(output_folder, paste0(dataset_code, "_cleaned_dataset_fwf.sqlite"))
    clean_sqlite_path  <- output_sqlite_file

    # Create a new data base (SQLite) that we can append to
    clean_db_conn <- dbConnect(RSQLite::SQLite(), output_sqlite_file)

    rows_read <- 0
    record_primary_key <- 1
    tryCatch({
      while(TRUE){
        if (rows_read == 0 && dataset_requires_header == 1) {
          chunk <- read_lines(file_path, n_max = chunk_size, skip = rows_read + 1)
          rows_read <- 1
        }
        else{
          chunk <- read_lines(file_path, n_max = chunk_size, skip = rows_read)
        }

        # Create an empty dataframe for the chunked portion of the data
        result_df <- data.frame()

        # Grab the column widths and names from the metadata
        data_set_widths <- output_fields$fixed_width_length
        column_names <- output_fields$source_field_name

        # Create a temporary vector file to store the read lines
        tmp_file <- tempfile()
        writeLines(chunk, tmp_file)

        # Use read_fwf to get the chunked data frame for cleaning
        chunk_df <- read_fwf(
          file = tmp_file,
          fwf_widths(data_set_widths, col_names = column_names),
          col_types = cols(.default = "c"),
          locale = locale(encoding = "latin1")  # Specify the encoding if needed
        )

        #Get the number of fields that the data SHOULD have
        fields_found <- ncol(chunk_df)

        if(flag_lookup_table["debug_mode"] == "on"){
          print(paste("We found [", fields_found, "] fields."))
          cat("\n")
        }

        if(fields_found != num_of_expected_fields){
          stop(paste("Error In pre_process_chunks(), number of columns read does ",
                     "not match the expected number of columns. ", cat("\n"),
                     "Expected [",num_of_expected_fields, "] columns, found ",
                     "[", fields_found, "]", sep=""))
        }

        # Append the processed chunk data frame to the result data frame
        result_df <- rbind(result_df, chunk_df)

        # Add a sequential record primary key to the chunk
        result_df[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(result_df) - 1)

        # Clean the data frame using our pre_process_data() function
        clean_dataframe <- pre_process_data(result_df, split_data, dataset_metadata_conn, dataset_id,
                                            standardized_name_lookup, standardized_function_lookup, flag_lookup_table)
        # Replace NA values with ""
        clean_dataframe[is.na(clean_dataframe)] <- ""

        # # Add record_primary_key column
        # clean_dataframe[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(clean_dataframe) - 1)

        # Append the clean_dataframe to the SQLite database
        dbWriteTable(clean_db_conn, "clean_data_table", clean_dataframe, append = TRUE)

        # If we are outputting the program and health data, run another function
        # to grab all the data we didn't need to standardize
        output_health_and_program_data_value <- flag_lookup_table["output_health_and_program_data"]
        if(output_health_and_program_data_value == "yes"){
          # Call function in "flag_standardizing_script.R" to return a dataset containing the health/program data
          df <- compile_health_and_program_data(result_df, dataset_metadata_conn, dataset_id)
          df[["record_primary_key"]] <- record_primary_key:(record_primary_key + nrow(clean_dataframe) - 1)
          dbWriteTable(clean_db_conn2, "clean_data_table", df, append = TRUE)
        }

        # Increment the record primary key counter
        record_primary_key <- record_primary_key + nrow(clean_dataframe)

        # Increment how many rows we've read in total
        rows_read <- rows_read + nrow(result_df)
        print(paste("Rows Read: ", rows_read))

        # Modify the reactive value with how many rows have been read, then send a notification
        total_rows_read(rows_read)
        tryCatch({
          showNotification(paste("Total Rows Read:", as.integer(total_rows_read())), type = "warning", closeButton = FALSE) #put a try/catch around this?
        },
        error = function(e){
          print("Can't send notification without a UI.")
        })

        # Get the rows read from current chunk iteration
        chunk_rows <- nrow(chunk_df)

        # Remove the chunk and clean dataframe
        rm(chunk, clean_dataframe)
        gc()

        # If the size of the read chunk is less than our chunk size, break out of the loop
        if(chunk_rows < chunk_size){
          break
        }
      }

    },
    error = function(e){
      print(e)
      errmsg <- geterrmessage()
      if(grepl("skip=", errmsg, fixed = TRUE) == FALSE)
        error_handle(dataset_metadata_conn, errmsg, clean_db_conn, file_path, output_folder)
      else
        print(errmsg)
    })

    print("Finished Reading!")

    # Last check to see if gender imputation is to be done internally
    sex_impute_type  <- flag_lookup_table["impute_sex_type"]
    impute_sex_value <- flag_lookup_table["impute_sex"]
    if(sex_impute_type == "internal" && impute_sex_value == "yes"){
      source_df <- dbReadTable(clean_db_conn, 'clean_data_table')
      final_df  <- impute_sex(source_df, source_df$primary_given_name, source_df$gender, flag_lookup_table)
      dbWriteTable(clean_db_conn, "clean_data_table", final_df, overwrite = TRUE)
    }

    # Determine whether the user wants the file output in a different format
    standardize_file_output(clean_db_conn, output_folder, flag_lookup_table, dataset_code)

    # Determine whether the user wants the non-linkage data in a different format
    if(flag_lookup_table["output_health_and_program_data"] == "yes"){
      standardize_file_output(clean_db_conn2, output_folder, flag_lookup_table, paste0(dataset_code, "_non_linkage"))
    }

    # Close the SQLite database connection
    dbDisconnect(clean_db_conn)
  }

  # Disconnect from the program data database connection if the flag value is "yes"
  output_health_and_program_data_value <- flag_lookup_table["output_health_and_program_data"]
  if(output_health_and_program_data_value == "yes"){
    dbDisconnect(clean_db_conn2)
  }

  # Print how long it took to finish reading and standardizing
  end <- proc.time()
  print("Time From Inside the Chunk Function:")
  print(end - start)

  # Reset the total rows read
  total_rows_read(0)

  # Return the clean file path
  return(clean_sqlite_path)
}

#' Standardize Data
#'
#' The standardize data function will open a database connection to a SQLite file which contains all metadata info
#' of the datasets that will be standardized.It will then fetch a dataset_id using a selected dataset_code and pass
#' it to a chunking script which will read and process the file.
#' @param input_file_path A path to a file.
#' @param input_dataset_code A dataset_code, used in the query statement within the function which will get the desired dataset code. File names meant to be pre-processed should contain a prefix containing the dataset code kept in the metadata, separated by something like a hyphen, underscore, etc.
#' @param input_flags A lookup table containing manually set or default values used by the flag script which will use the values of the flags to determine how to pre-process and output the health data.
#' @param output_folder An output folder that the successfully cleaned data ready for linkage will be placed in once pre-processing is complete.
#' @param metadata_file A path to a metadata file.
#' @return A cleaned data frame containing the cleaned table from pre_process_chunks() returned database (as long as file size is <1GB)
#' @examples
#' clean_df <- standardize_data("path/to/file.txt", "fakecode", flag_lookup, "path/to/folder", "path/to/metadata.sqlite")
#' @export
standardize_data <- function(input_file_path, input_dataset_code, input_flags, output_folder, metadata_file){
  # Establish default lookup flags and replace them if necessary
  #----
  # Construct the flag lookup tables for standardization [Set this to be in a single source file]
  flag_values <- data.frame(
    flag_code = c("convert_name_case", "convert_name_to_ascii", "remove_name_punctuation","compress_name_whitespace", "list_all_curr_given_names", "list_all_curr_surnames", "list_all_curr_names",
                  "impute_sex", "impute_sex_type", "chosen_sex_file",
                  "compress_address_whitespace", "remove_address_punctuation", "convert_address_case", "convert_address_to_ascii", "extract_postal_code",
                  "file_output",
                  "output_health_and_program_data",
                  "chunk_size",
                  "debug_mode"),
    flag_value = c("original", "no", "no", "no", "no", "no", "no",
                   "no", "none", "null",
                   "no", "no", "original", "no", "no",
                   "sqlite",
                   "no",
                   100000,
                   "off")
  )

  flag_lookup <- setNames(flag_values$flag_value, flag_values$flag_code)

  tryCatch({
    if(!is.null(input_flags)){
      for(flag_code in names(input_flags)){
        input_flag_value <- input_flags[flag_code]
        flag_lookup[flag_code] <- input_flag_value
      }
    }
  },
  error = function(e){
    error_handle(NULL, "ERROR: Input flags format is invalid, use setNames(values, codes) as parameter.", NULL, input_file_path, output_folder)
  })
  #----

  # Error handling to ensure the provided SQLite file is actually an SQLite file
  #----#
  if(is.null(metadata_file) || file_ext(metadata_file) != "sqlite"){
    error_handle(NULL, "ERROR: Invalid metadata file provided.", NULL, input_file_path, output_folder)
  }
  #----#

  metadata_connection <- dbConnect(RSQLite::SQLite(), metadata_file)

  # Error handling to ensure that the metadata has all the correct tables
  #----#
  # Define the required tables
  required_tables <- c(
    "categorical_fields", "categorical_values", "compound_field_destinations",
    "compound_field_formats", "compound_field_separators", "compound_fields",
    "datasets", "destination_fields", "file_formats", "numeric_date_destinations",
    "numeric_date_fields", "numeric_date_formats", "record_priority_fields",
    "source_fields", "standardizing_modules"
  )

  # Check if all required tables exist and match exactly
  existing_tables <- dbListTables(metadata_connection)
  missing_tables <- setdiff(required_tables, existing_tables)

  # Do the error check here, if missing_tables is empty, we're good, otherwise throw an error
  if(length(missing_tables) != 0){
    error_handle(metadata_connection, "ERROR: Invalid metadata tables.", NULL, input_file_path, output_folder)
  }

  rm(required_tables, missing_tables, existing_tables)
  #----#

  # Get the dataset_id from the file name
  chosen_fields <- dbSendQuery(metadata_connection, 'SELECT dataset_id FROM datasets WHERE dataset_code = ? AND enabled_for_standardization = 1;')
  dbBind(chosen_fields, list(input_dataset_code))
  output_id <- dbFetch(chosen_fields)
  dbClearResult(chosen_fields)

  # Error handling, if there is more than one enabled data set (somehow) or none enabled with the same data set code, throw an error
  #----#
  if(nrow(output_id) == 0){
    error_handle(metadata_connection, "ERROR: No enabled datasets match the desired code, verify that the dataset to be standardized is enabled and try again.", NULL, input_file_path, output_folder)
  }
  else if(nrow(output_id) > 1){
    error_handle(metadata_connection, "ERROR: More than one of the enabled datasets match the dataset code, disable all but one and try again.", NULL, input_file_path, output_folder)
  }

  if(is.null(input_file_path)){
    error_handle(metadata_connection, "ERROR: File path to unclean dataset not provided, provide a path in the input flags via file_path.", NULL, input_file_path, output_folder)
  }

  chunk_size <- as.integer(flag_lookup["chunk_size"])
  if(is.na(chunk_size) || (chunk_size != 100000 && chunk_size != 200000 && chunk_size != 500000 && chunk_size != 1000000)){
    error_handle(metadata_connection, "ERROR: Invalid chunking size.", NULL, input_file_path, output_folder)
  }
  #----#

  dataset_id <- output_id$dataset_id

  if(flag_lookup["debug_mode"] == "on"){
    print(paste("Dataset Code:", input_dataset_code))
    print(paste("Dataset ID:", dataset_id))
  }

  # Automatically pre-process the chunk from the dropped input file
  clean_file_path <- pre_process_chunks(input_file_path, metadata_connection, dataset_id, flag_lookup, output_folder, input_dataset_code)

  # Disconnect once finished
  dbDisconnect(metadata_connection)
  rm(metadata_connection)
  rm(dataset_id)

  # Open the metadata connection, and return a data frame if the file size is low enough
  file_size <- file.size(clean_file_path)/1000000
  # If its less than 1GB then return a data frame, other wise return nothing.
  if(file_size <= 1000){
    clean_db_conn <- dbConnect(RSQLite::SQLite(), clean_file_path)
    df <- dbReadTable(clean_db_conn, 'clean_data_table')
    dbDisconnect(clean_db_conn)
    return(df)
  }
  else{
    return(data.frame())
  }

}
