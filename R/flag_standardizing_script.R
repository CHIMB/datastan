#' Standardize Names
#'
#' The standardize_names() function serves as a general/abstract function that will
#' use input flags and data standards to standardize fields in the data set relating
#' to a person's name.
#' @param input_names A vector of names, can be primary or secondary given names, primary or secondary surnames, and prior surnames.
#' @param flag_lookup_table A lookup table containing manually set or default values that will determine how persons names are handled specifically by the function.
#' @return A vector of processed names.
#' @examples
#' processed_names <- standardize_names(c("John", "Jane", "Cole"), flag_lookup)
#' @export
standardize_names <- function(input_names, flag_lookup_table){

  curr_names <- input_names

  #-- First - Remove any punctuation: --#
  remove_name_punctuation_value <- flag_lookup_table["remove_name_punctuation"]
  if(remove_name_punctuation_value == "yes"){
    curr_names <- stri_replace_all_regex(curr_names, "[-.'[:punct:]]+", " ")
  }

  #-- Second - Remove any white space: --#
  compress_name_whitespace_value <- flag_lookup_table["compress_name_whitespace"]
  if(compress_name_whitespace_value == "yes"){
    curr_names <- stri_replace_all_regex(curr_names, " ", "")
  }

  #-- Third - Remove accents or diacritics from names: --#
  convert_to_ascii_value <- flag_lookup_table["convert_name_to_ascii"]
  if(convert_to_ascii_value == "yes"){
    curr_names <- stri_trans_general(curr_names, "Latin-ASCII")
  }

  #-- Fourth - Convert name case using flag value --#
  convert_case_value <- flag_lookup_table["convert_name_case"]
  if(convert_case_value == "upper"){
    curr_names <- stri_trans_toupper(curr_names)
  }
  else if (convert_case_value == "lower"){
    curr_names <- stri_trans_tolower(curr_names)
  }

  return(curr_names)
}

#' Standardize Addresses
#'
#' The standardize_addresses() function serves as a general/abstract function that will use input flags
#' and data standards to standardize fields in the data set relating to a person's address or location.
#' @param input_addresses A vector of addresses, can be primary or alternative addresses, countries, cities, or any location based field.
#' @param flag_lookup_table A lookup table containing manually set or default values that will determine how persons addresses are handled specifically by the function.
#' @return A vector of processed address/location based values.
#' @examples
#' processed_locations <- standardize_addresses(c("123 Fake St", "canada", "winniPEG"), flag_lookup)
#' @export
standardize_addresses <- function(input_addresses, flag_lookup_table){
  curr_addresses <- input_addresses

  #-- First - Remove any punctuation: --#
  compress_address_whitespace_value <- flag_lookup_table["remove_address_punctuation"]
  if(compress_address_whitespace_value == "yes"){
    curr_addresses <- stri_replace_all_regex(curr_addresses, "[-.'[:punct:]]+", " ")
  }

  #-- Second - Remove any white space: --#
  compress_address_whitespace_value <- flag_lookup_table["compress_address_whitespace"]
  if(compress_address_whitespace_value == "yes"){
    curr_addresses <- stri_replace_all_regex(curr_addresses, " ", "")
  }

  #-- Third - Remove accents or diacritics from names: --#
  convert_to_ascii_value <- flag_lookup_table["convert_address_to_ascii"]
  if(convert_to_ascii_value == "yes"){
    curr_addresses <- stri_trans_general(curr_addresses, "Latin-ASCII")
  }

  #-- Fourth - Convert name case using flag value --#
  convert_case_value <- flag_lookup_table["convert_address_case"]
  if(convert_case_value == "upper"){
    curr_addresses <- stri_trans_toupper(curr_addresses)
  }
  else if (convert_case_value == "lower"){
    curr_addresses <- stri_trans_tolower(curr_addresses)
  }

  return(curr_addresses)
}

#' Split Compound Field
#'
#' split_compound_field() will take in a vector of single fields that are to be split into separate fields,
#' it will either use separators or indexes based on the source fields metadata.
#' @param compound_field A vector of compound data meant to be split using the objects in “split_objects”, the other argument “split_type” will determine the algorithm/logic for splitting a compound field.
#' @param split_objects A vector of split objects that will either use split logic to separate a compound field based on a specific character or string. Or we will use substr logic to separate a compound field based on integer indexes.
#' @param split_type Splitting a compound field will either be of type separators, or of type indexes.
#' @return A list of split values.
#' @examples
#' split_dates <- split_compound_field(c("1960-01-01", "2024-04-04"), c("-", "-"), "separators")
#' @export
split_compound_field <- function(compound_field, split_objects, split_type){
  if(split_type == "separators"){
    compound_field <- t(compound_field)
    name_split <- vector("list", length(split_objects) + 1)
    name_split[[1]] <- compound_field
    for (i in seq_along(split_objects)){
      name_split[[i]] <- trimws(name_split[[i]], "both")
      split <- str_split_fixed(name_split[[i]], split_objects[i], n = 2)
      name_split[[i]] <- split[,1]
      name_split[[i+1]] <- split[,2]
    }
    return(name_split)
  }
  else{
    compound_field <- t(compound_field)
    name_split <- vector("list", length(split_objects))
    name_split[[1]] <- compound_field
    start_index <- 1
    end_index <- 0
    for(i in seq_along(split_objects)){
      if(i != 1){
        start_index <- end_index + 1
      }
      end_index <- end_index + split_objects[i]
      name_split[[i]] <- substr(compound_field, start_index, end_index)
    }
    return(name_split)
  }
}


#' List All Current Names
#'
#' list_curr_names() will take in a vector of names, the current built up data frame, and append all current names (primary/secondary given names,
#' primary/secondary surnames, and prior surnames) if the user flag is defined as yes before pre-processing.
#' @param names_to_append A vector of names to be concatenated together, separated by a space. Contains all current processed names of the dataset.
#' @param df The current built up and processed data frame, a new column called “all_curr_names” is defined and the processed names are appended to it.
#' @return The original data frame with an additional column called "all_curr_names" with names_to_append pasted to it.
#' @examples
#' df <- list_curr_names(c("James", "Jane", "Cole"), df)
#' @export
list_curr_names <- function(names_to_append, df){
  df[["all_curr_names"]] <- paste(df[["all_curr_names"]], names_to_append, sep = " ")
  return(df)
}

#' List Current Given Names
#'
#' list_curr_given_names() will take in a vector of primary of secondary given names, the current built up data frame,
#' and append all given names if the user flag is defined as yes before pre-processing.
#' @param names_to_append A vector of given names to be concatenated together, separated by a space. Contains all current processed given names of the dataset.
#' @param df The current built up and processed data frame, a new column called “all_curr_given_names” is defined and the processed names are appended to it.
#' @return The original data frame with an additional column called "all_curr_given_names" with names_to_append pasted to it.
#' @examples
#' df <- list_curr_given_names(c("Maria", "Larry", "John"), df)
#' @export
list_curr_given_names <- function(names_to_append, df){
  df[["all_curr_given_names"]] <- paste(df[["all_curr_given_names"]], names_to_append, sep = " ")
  return(df)
}

#' List Current Surnames
#'
#' list_curr_surnames() will take in a vector of primary of secondary surnames, the current built up data frame,
#' and append all surnames if the user flag is defined as yes before pre-processing.
#' @param names_to_append A vector of surnames to be concatenated together, separated by a space. Contains all current processed surnames of the dataset.
#' @param df The current built up and processed data frame, a new column called “all_curr_surnames” is defined and the processed names are appended to it.
#' @return The original data frame with an additional column called "all_curr_surnames" with names_to_append pasted to it.
#' @examples
#' df <- list_curr_surnames(c("Doe", "Doe", "Chuchmach"), df)
#' @export
list_curr_surnames <- function(names_to_append, df){
  df[["all_curr_surnames"]] <- paste(df[["all_curr_surnames"]], names_to_append, sep = " ")
  return(df)
}

#' Impute Sex
#'
#' The impute_sex() function will take in a source data frame, two vectors and flag lookup table, using that
#' table it will identify whether we will be imputing sex using a custom file of first names and cooresponding
#' sexes, otherwise we may attempt to impute sex internally by using the data within the processed file.
#' @param source_df The current processed/cleaned data frame of the input file.
#' @param processed_names The current processed primary given names on this chunk iteration or after processing finishes.
#' @param processed_genders The current processed genders on this chunk iteration or after processing finishes.
#' @param flag_lookup_table Lookup table used to determine what output file format is to be used. Output format can be of type rdata, csv, xlsx or just sqlite.
#' @return The original data frame with an additional column named "imputed_sex".
#' @examples
#' df <- impute_sex(df, c("John", "Jane", "Cole", "John"), c("M", "F", "C", NA), flag_lookup)
#' @export
impute_sex <- function(source_df, processed_names, processed_genders, flag_lookup_table){ # Change wordings a descriptions to inferring sex
  print("Imputing Sex")
  # Get what imputation type we are doing
  imputation_type <- flag_lookup_table["impute_sex_type"]

  if(imputation_type == "default"){
    return(source_df) #THE GENDER PACKAGE WONT INSTALL ON SOME COMPUTERS SO KEEP IT OUT FOR NOW

    if(is.null(processed_genders))
      temporary_gender_imputation_df <- data.frame(names = processed_names, genders = NA)
    else
      temporary_gender_imputation_df <- data.frame(names = processed_names, genders = processed_genders)

    names_to_impute <- temporary_gender_imputation_df[is.na(temporary_gender_imputation_df$genders), ]$names

    # Sample data
    df0 <- data.frame(v1 = names_to_impute, stringsAsFactors = FALSE)

    # Add an index column
    df0$index <- seq_len(nrow(df0))

    # Rename column in df0
    colnames(df0) <- c("name", "index")

    # Merge
    df1 <- merge(df0, gender(unique(df0$name)), by = "name", all.x = TRUE)

    # Sort by index
    df1 <- df1[order(df1$index), ]

    # Remove the index column if needed
    df1 <- df1[, -which(names(df1) == "index")]

    temporary_gender_imputation_df$genders[is.na(temporary_gender_imputation_df$genders)] <- df1$gender
    temporary_gender_imputation_df$genders <- str_replace_all(temporary_gender_imputation_df$genders, "female", "F")
    temporary_gender_imputation_df$genders <- str_replace_all(temporary_gender_imputation_df$genders, "male", "M")

    source_df[["imputed_sex"]] <- paste(source_df[["imputed_sex"]], temporary_gender_imputation_df$genders, sep = " ")
    source_df[is.na(source_df)] <- "" #figure out how to get rid of these NA values, they are annoying.
    source_df[["imputed_sex"]] <- trimws(source_df[["imputed_sex"]])
    return(source_df)
  }
  else if (imputation_type == "custom"){

    gender_file_path <- flag_lookup_table[["chosen_gender_file"]]
    gender_infer_df <- read_csv(gender_file_path, show_col_types = FALSE)

    # Filter out names with NA genders
    na_indices <- which(is.na(processed_genders) | processed_genders == "NA" | processed_genders == "")
    processed_names_na <- processed_names[na_indices]
    print(processed_names_na)
    # Step 1: Find the indices where the names exist in the first names list
    indicies_list <- match(processed_names_na, gender_infer_df$primary_given_name)

    # Step 2: Get the majority sex for each name
    majority_sex <- gender_infer_df[indicies_list, "sex"]

    # Step 3: Un-list the sexes
    processed_genders[na_indices] <- unlist(majority_sex)

    # Step 4: Add the imputed sex to a new column
    source_df[["imputed_sex"]] <- paste(processed_genders)
    source_df[["imputed_sex"]] <- trimws(source_df[["imputed_sex"]])

    # Remove the data frame and clean everything up
    rm(gender_infer_df)
    gc()

    return(source_df)
  }
  else if (imputation_type == "internal"){
    # Filter out names with NA genders
    na_indices <- which(is.na(processed_genders) | processed_genders == "NA" | processed_genders == "")

    processed_names_na <- processed_names[na_indices]

    # Step 1: Use vectorized operations for gender imputation, create a named vector for faster look-up
    first_names_lookup <- setNames(source_df$gender, source_df$primary_given_name)

    # Step 2: Find the indices where the names exist in the first names list
    indices_list <- lapply(processed_names_na, function(name) which(names(first_names_lookup) == name))

    # Step 3: Create a function to determine the majority gender for each name
    get_majority_sex <- function(indices) {
      if (length(indices) == 0) {
        return(NA)  # Return NA if the name doesn't exist in the first names list
      }
      # Filter out NA and empty strings
      valid_genders <- na.omit(source_df$gender[unlist(indices)])
      valid_genders <- valid_genders[valid_genders != ""]  # Filter out empty strings

      if (length(valid_genders) == 0) {
        return(NA)  # Return NA if there are no valid genders
      }

      gender_counts <- table(valid_genders)
      majority_sex <- names(gender_counts)[which.max(gender_counts)]
      return(majority_sex)
    }

    # Step 4: Create a list to store majority genders for each name
    majority_sexes <- lapply(indices_list, get_majority_sex)

    # Step 5: Create a vector of majority genders corresponding to processed_names_na
    processed_sexes_na <- unlist(majority_sexes)

    # Step 6: Replace NA genders in processed_genders with imputed genders
    processed_genders[na_indices] <- processed_sexes_na

    # Step 7: Replace NA genders in processed_genders with imputed genders
    temporary_sex_imputation_df <- data.frame(names = processed_names, genders = processed_genders)

    # Step 8: Add the new column
    source_df[["imputed_sex"]] <- paste(source_df[["imputed_sex"]], temporary_sex_imputation_df$genders, sep = " ")
    source_df[["imputed_sex"]] <- trimws(source_df[["imputed_sex"]])

    return(source_df)
  }

}

#' Standardize File Output
#'
#' Takes in a connection to the database, the output path folder, and the flag lookup table. Uses the value from
#' the flag lookup table when searching for “file_output” to determine what format the output file will be.
#' @param db_conn A connection to the metadata database, used to read the cleaned table after processing has finished.
#' @param output_folder_path The output_folder_path will be used to determine where the standardized file be placed.
#' @param flag_lookup_table Lookup table used to determine what output file format is to be used. Output format can be of type rdata, csv, xlsx or just sqlite.
#' @param input_dataset_code Used as prefix for the cleaned file to help differentiate amongst other cleaned files.
#' @examples
#' standardize_file_output(db, "path/to/folder", flag_lookup, "fakecode")
#' @export
standardize_file_output <- function(clean_df, output_folder_path, flag_lookup_table, input_dataset_code){

  # # Read the table into a data frame
  # clean_dataframe_chunked <- dbReadTable(db_conn, "clean_data_table")

  output_file_type <- flag_lookup_table["file_output"]
  if(output_file_type == "rds"){

    # Create the full path to the output RData file within the output folder
    output_file <- file.path(output_folder_path, paste0(input_dataset_code, "_cleaned_dataset.Rds"))

    # Load in the old file if it exists, otherwise we will need to save for the first time
    if(file.exists(output_file)){
      curr_df <- readRDS(output_file)

      # Bind and save the data
      saveRDS(rbind(curr_df, clean_df), output_file)
    }
    else{
      # Save the Rdata file
      saveRDS(clean_df, output_file)
    }
  }
  else if (output_file_type == "csv"){
    # Create the full path to the output RData file within the output folder
    output_file <- file.path(output_folder_path, paste0(input_dataset_code, "_cleaned_dataset.csv"))

    # Save the csv file
    fwrite(clean_df, file = output_file, append = TRUE)
  }
  # else if (output_file_type == "excel"){
  #   # Create the full path to the output RData file within the output folder
  #   output_file <- file.path(output_folder_path, paste0(input_dataset_code, "_cleaned_dataset.xlsx"))
  #
  #   # Save the excel file
  #   write.xlsx(clean_df, output_file, append = TRUE)
  #   #write_xlsx(clean_df, path = output_file)
  # }
}

#' Extract Postal Codes
#'
#' Takes in a vector of values that may potentially have postal codes embedded in them, regular expressions are used
#' to extract the values and append them to a new column in the current processed data frame called “alt_postal_code”.
#' @param to_extract A vector of data which may contain concatenated postal codes which should be extracted.
#' @param df The current built up and processed data frame, a new column called “alt_postal_code” is defined and the extracted postal codes are appended to it.
#' @return A list of two values, one being the original field under "orig_field", the other being the extracted postal codes under "p_codes"
#' @examples
#' return_list <- extract_postal_codes(c("123 Fake Address A1A1A1", "No Postal Code", "B1B 1B1 321 Fake Address"), source_df)
#' @export
extract_postal_codes <- function(to_extract, df){
  # Create a variable to track postal code count
  postal_code_count <- 0

  # Try getting postal codes of the format CNCNCN
  postal_codes <- stri_extract_first_regex(to_extract, "[A-Za-z]{1}[0-9]{1}[A-Za-z]{1}[0-9]{1}[A-Za-z]{1}[0-9]{1}")

  if(!all(is.na(postal_codes))){
    # Increment postal code count
    postal_code_count <- postal_code_count + length(postal_codes[!is.na(postal_codes)])

    postal_codes[is.na(postal_codes)] <- ""
    to_extract <- stri_replace_all_regex(to_extract, "[A-Za-z]{1}[0-9]{1}[A-Za-z]{1}[0-9]{1}[A-Za-z]{1}[0-9]{1}", "")
    postal_codes <- str_replace_all(postal_codes, " ", "")
    df[["alt_postal_code"]] <- paste(df[["alt_postal_code"]], postal_codes, sep = " ")
  }
  # Try getting postal codes of the format CNC NCN
  postal_codes <- stri_extract_first_regex(to_extract, "[A-Za-z]{1}[0-9]{1}[A-Za-z] {1}[0-9]{1}[A-Za-z]{1}[0-9]{1}")

  if(!all(is.na(postal_codes))){
    # Increment postal code count
    postal_code_count <- postal_code_count + length(postal_codes[!is.na(postal_codes)])

    postal_codes[is.na(postal_codes)] <- ""
    to_extract <- stri_replace_all_regex(to_extract, "[A-Za-z]{1}[0-9]{1}[A-Za-z] {1}[0-9]{1}[A-Za-z]{1}[0-9]{1}", "")
    postal_codes <- str_replace_all(postal_codes, " ", "")
    df[["alt_postal_code"]] <- paste(df[["alt_postal_code"]], postal_codes, sep = " ")

  }

  print(paste("Postal Codes Extracted:", postal_code_count))

  return(list("orig_field" = to_extract, "p_codes" = df[["alt_postal_code"]]))
}

#' Concatenate Postal Codes
#'
#' Modifies the alt_postal_code column of the data frame, by splitting the data and
#' removing any duplicate postal codes that it may contain in a single row.
#' @param df The current built up and processed data frame containing the column alt_postal_code.
#' @param alt_postal_codes A vector of alternative postal codes that will first be added to the data frame column before de-duplication takes place.
#' @return The original data frame with an additional column called "alt_postal_codes" containing the de-duplicated extracted codes.
#' @examples
#' df <- concat_postal_codes(df, c("A1A1A1", NA, "B1B1B1"))
#' @export
concat_postal_codes <- function(df, alt_postal_codes){

  df[["alt_postal_code"]] <- paste(df[["alt_postal_code"]], alt_postal_codes, sep = " ")

  df <- df %>%
    mutate(alt_postal_code = str_split(alt_postal_code, " ")) %>%
    mutate(alt_postal_code = map_chr(alt_postal_code, ~paste(unique(.x), collapse = " ")))

  df[["alt_postal_code"]] <- trimws(df[["alt_postal_code"]])

  # Convert all postal codes to uppercase
  df[["alt_postal_code"]] <- str_to_upper(df[["alt_postal_code"]])

  return(df)
}

#' Compile Health and Program Data
#'
#' Fields that were not included in the processed data frame that is to be used during linkage, as well as some additional fields
#' that may be included such as postal code, gender, birthday, are also included in the output program and health data.
#' @param source_data_frame The source data frame, containing both the health and program data we’re interested in extracting, as well as the valid linkage fields found by checking the metadata database with the dataset_id.
#' @param db_conn A connection to the metadata database, used with the dataset_id argument to determine what fields are valid to also include in the health and program data output.
#' @param dataset_id The dataset_id of the dataset we’re currently cleaning.
#' @return A processed data frame containing the non-linkage fields in their original format.
#' @examples
#' non_linkage_df <- compile_health_and_program_data(source_df, db, 1)
#' @export
compile_health_and_program_data <- function(source_data_frame, db_conn, dataset_id){

  # Query to get the fields from the chosen dataset, that we DO standardize that is NOT IDENTIFIABLE
  standardized_data_query <- paste("SELECT distinct source_field_name, field_order",
                                     "FROM source_fields sf",
                                     "JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id",
                                     "WHERE output_program_data = 0 and dataset_id = ", dataset_id)
  standardized_data_output <- dbSendQuery(db_conn, standardized_data_query)
  standardized_data <- dbFetch(standardized_data_output)
  standardized_field_orders <- standardized_data$field_order
  dbClearResult(standardized_data_output)

  processed_data_frame <- subset(source_data_frame, select = -standardized_field_orders) #INVERSE SELECT

  rm(standardized_field_orders)
  return(processed_data_frame)
}
