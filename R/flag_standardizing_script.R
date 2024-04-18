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
#' processed_locations <- standardize_locations(c("123 Fake St", "canada", "winniPEG"), flag_lookup)
#' @export
standardize_locations <- function(input_locations, flag_lookup_table){
  curr_locations <- input_locations

  #-- First - Remove any punctuation: --#
  remove_location_punctuation_value <- flag_lookup_table["remove_location_punctuation"]
  if(remove_location_punctuation_value == "yes"){
    curr_locations <- stri_replace_all_regex(curr_locations, "[-.'[:punct:]]+", " ")
  }

  #-- Second - Remove any white space: --#
  compress_location_whitespace_value <- flag_lookup_table["compress_location_whitespace"]
  if(compress_location_whitespace_value == "yes"){
    curr_locations <- stri_replace_all_regex(curr_locations, " ", "")
  }

  #-- Third - Remove accents or diacritics from names: --#
  convert_to_ascii_value <- flag_lookup_table["convert_location_to_ascii"]
  if(convert_to_ascii_value == "yes"){
    curr_locations <- stri_trans_general(curr_locations, "Latin-ASCII")
  }

  #-- Fourth - Convert name case using flag value --#
  convert_case_value <- flag_lookup_table["convert_location_case"]
  if(convert_case_value == "upper"){
    curr_locations <- stri_trans_toupper(curr_locations)
  }
  else if (convert_case_value == "lower"){
    curr_locations <- stri_trans_tolower(curr_locations)
  }

  return(curr_locations)
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
#' @param processed_sexes The current processed sexes on this chunk iteration or after processing finishes.
#' @param flag_lookup_table Lookup table used to determine what output file format is to be used. Output format can be of type rdata, csv, xlsx or just sqlite.
#' @return The original data frame with an additional column named "imputed_sex".
#' @examples
#' df <- impute_sex(df, c("John", "Jane", "Cole", "John"), c("M", "F", "C", NA), flag_lookup)
#' @export
impute_sex <- function(source_df, processed_names, processed_sexes, flag_lookup_table){
  # Get what imputation type we are doing
  imputation_type <- flag_lookup_table["impute_sex_type"]

  if(imputation_type == "default"){
    # Create a data.frame based on results from the gender package
    sex_results <- gender(unique(tolower(processed_names)))
    sex_summary <- data.frame(
      primary_given_name = sex_results$name,
      sex = sex_results$gender
    )

    # Remove sex_results and call garbage collector
    rm(sex_results)
    gc()

    # Replace male and female with M and F
    sex_summary$sex <- str_replace_all(sex_summary$sex, "female", "F")
    sex_summary$sex <- str_replace_all(sex_summary$sex, "male", "M")

    # Filter out names with NA genders
    na_indices <- which(is.na(processed_sexes) | processed_sexes == "NA" | processed_sexes == "")

    processed_names_na <- processed_names[na_indices]

    # Step 1: Find the indices where the names exist in the first names list
    indicies_list <- match(tolower(processed_names_na), tolower(sex_summary$primary_given_name))

    # Step 2: Get the majority sex for each name
    majority_sex <- sex_summary[indicies_list, "sex"]

    # Step 3: Un-list the sexes
    processed_sexes[na_indices] <- unlist(majority_sex)

    # Step 4: Add the imputed sex to a new column
    source_df[["imputed_sex"]] <- paste(processed_sexes)
    source_df[["imputed_sex"]] <- trimws(source_df[["imputed_sex"]])

    # Remove the sex summary data frame
    rm(sex_summary)
    gc()

    return(source_df)

    # if(is.null(processed_sexes))
    #   temporary_sex_imputation_df <- data.frame(names = processed_names, sex = NA)
    # else
    #   temporary_sex_imputation_df <- data.frame(names = processed_names, sex = processed_sexes)
    #
    # names_to_impute <- temporary_sex_imputation_df[is.na(temporary_sex_imputation_df$sex), ]$names
    #
    # # Sample data
    # df0 <- data.frame(v1 = names_to_impute, stringsAsFactors = FALSE)
    #
    # # Add an index column
    # df0$index <- seq_len(nrow(df0))
    #
    # # Rename column in df0
    # colnames(df0) <- c("name", "index")
    #
    # # Merge
    # df1 <- merge(df0, gender(unique(df0$name)), by = "name", all.x = TRUE)
    #
    # # Sort by index
    # df1 <- df1[order(df1$index), ]
    #
    # # Remove the index column if needed
    # df1 <- df1[, -which(names(df1) == "index")]
    #
    # temporary_sex_imputation_df$sex[is.na(temporary_sex_imputation_df$sex)] <- df1$gender
    # temporary_sex_imputation_df$sex <- str_replace_all(temporary_sex_imputation_df$sex, "female", "F")
    # temporary_sex_imputation_df$sex <- str_replace_all(temporary_sex_imputation_df$sex, "male", "M")
    #
    # source_df[["imputed_sex"]] <- paste(source_df[["imputed_sex"]], temporary_sex_imputation_df$sex, sep = " ")
    # source_df[is.na(source_df)] <- ""
    # source_df[["imputed_sex"]] <- trimws(source_df[["imputed_sex"]])
    #
    # return(source_df)
  }
  else if (imputation_type == "custom"){
    sex_imputation_file <- flag_lookup_table[["chosen_sex_file"]]
    sex_infer_df <- read_csv(sex_imputation_file, show_col_types = FALSE)

    # # Group by name and calculate the majority gender for each name
    # sex_summary <- sex_infer_df %>%
    #   group_by(primary_given_name) %>%
    #   summarise(sex = names(which.max(table(sex))))

    # # Remove the initial sex imputation data frame
    # rm(sex_infer_df)

    # Filter out names with NA genders
    na_indices <- which(is.na(processed_sexes) | processed_sexes == "NA" | processed_sexes == "")
    processed_names_na <- processed_names[na_indices]
    # Step 1: Find the indices where the names exist in the first names list
    indicies_list <- match(tolower(processed_names_na), tolower(sex_infer_df$primary_given_name))

    # Step 2: Get the majority sex for each name
    majority_sex <- sex_infer_df[indicies_list, "sex"]

    # Step 3: Un-list the sexes
    processed_sexes[na_indices] <- unlist(majority_sex)

    # Step 4: Add the imputed sex to a new column
    source_df[["imputed_sex"]] <- paste(processed_sexes)
    source_df[["imputed_sex"]] <- trimws(source_df[["imputed_sex"]])

    # Remove the sex summary data frame
    rm(sex_infer_df)
    gc()

    return(source_df)
  }
  else if (imputation_type == "internal"){
    # Create a sex imputation data frame out of the processed names and sexes
    sex_impuation_df <- data.frame(
      primary_given_name = source_df[["primary_given_name"]],
      sex = source_df[["gender"]]
    )

    # Group by name and calculate the majority gender for each name
    sex_summary <- sex_impuation_df %>%
      group_by(primary_given_name) %>%
      summarise(sex = names(which.max(table(sex))))

    # Remove the initial sex imputation data frame
    rm(sex_impuation_df)

    # Filter out names with NA genders
    na_indices <- which(is.na(processed_sexes) | processed_sexes == "NA" | processed_sexes == "")

    processed_names_na <- processed_names[na_indices]

    # Step 1: Find the indices where the names exist in the first names list
    indicies_list <- match(tolower(processed_names_na), tolower(sex_summary$primary_given_name))

    # Step 2: Get the majority sex for each name
    majority_sex <- sex_summary[indicies_list, "sex"]

    # Step 3: Un-list the sexes
    processed_sexes[na_indices] <- unlist(majority_sex)

    # Step 4: Add the imputed sex to a new column
    source_df[["imputed_sex"]] <- paste(processed_sexes)
    source_df[["imputed_sex"]] <- trimws(source_df[["imputed_sex"]])

    # Remove the sex summary data frame
    rm(sex_summary)
    gc()

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
#' non_linkage_df <- compile_non_linkage_data(source_df, db, 1)
#' @export
compile_non_linkage_data <- function(source_data_frame, db_conn, dataset_id){

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

#' Create Standardizing Options Lookup
#'
#' Using this function the user can supply any number of flag options they want (not all are required, missing entries will be given a default
#' value) which will be used as additional standardization rules.
#' @param convert_name_case Convert the capitalization of a person's name. (Options: "upper", "lower", "default")
#' @param convert_name_to_ascii Remove diacritics of a person's name. (Options: "yes", "no")
#' @param remove_name_punctuation Remove any symbols or punctuation from a person's name. (Options: "yes", "no")
#' @param compress_name_whitespace Replace name white space with an empty string symbol. (Options: "yes", "no")
#' @param list_all_curr_given_names Combine all given names of a person into an additional column. (Options: "yes", "no")
#' @param list_all_curr_surnames Combine all surnames of a person into an additional column. (Options: "yes", "no")
#' @param list_all_curr_names Combine all names of a person into an additional column. (Options: "yes", "no")
#' @param impute_sex Impute missing values in sex fields. (Options: "yes", "no")
#' @param impute_sex_type How should the sex imputation take place? (Options: "default" - Must have the gender and genderdata packages installed, "custom" - Must be a .csv file with two columns [primary_given_name] & [sex], "internal" - Use sex values from the source data set)
#' @param chosen_sex_file If the custom type is chosen, supply an input file.
#' @param compress_location_whitespace Replace location based fields white space with an empty string symbol. (Options: "yes", "no")
#' @param remove_location_punctuation Remove any symbols or punctuation from location based fields. (Options: "yes", "no")
#' @param convert_location_case Convert the capitalization of a location based field. (Options: "upper", "lower", "default")
#' @param convert_location_to_ascii Remove diacritics of a location based field. (Options: "yes", "no")
#' @param extract_postal_code Attempt to extract postal codes from location based fields and place an additional column. (Options: "yes", "no")
#' @param file_output What file output format is desired. (Options: "csv", "rds", "sqlite")
#' @param output_non_linkage_fields Output non-linkage fields in a separate file? (Options: "yes", "no")
#' @param chunk_size How many rows should be read at a time when reading the source file in chunks? (Options: 10000-1000000)
#' @param debug_mode Print additional information to the console in case of potential bugs? (Options: "on", "off")
#' @param max_file_size_output What is the max file size that a data frame can be before it isn't returned? (Options: integer in Mega-bytes)
#' @param read_mode Read delimited data using the base file path, or using shell commands? (Options: "path", "cmd")
#' @return A lookup table consisting of chosen flags and the assigned default options for bad inputs or undefined choices.
#' @examples
#' flags <- create_standardizing_options_lookup(convert_name_case = "upper", impute_sex = "yes", chunk_size = 15000, max_file_size_output = 200)
#' @export
create_standardizing_options_lookup <- function(convert_name_case, convert_name_to_ascii, remove_name_punctuation, compress_name_whitespace,
                                                list_all_curr_given_names, list_all_curr_surnames, list_all_curr_names,
                                                impute_sex, impute_sex_type, chosen_sex_file,
                                                compress_location_whitespace, remove_location_punctuation, convert_location_case, convert_location_to_ascii, extract_postal_code,
                                                file_output, output_non_linkage_fields, chunk_size, debug_mode, max_file_size_output, read_mode){

  # NAMES
  #----------------------------------------------------------------------------#
  # Convert Name Case
  if(missing(convert_name_case) || (convert_name_case != "upper" && convert_name_case != "lower"))
    convert_name_case <- "default"

  # Convert Name to ASCII
  if(missing(convert_name_to_ascii) || (convert_name_to_ascii != "yes" && convert_name_to_ascii != "no"))
    convert_name_to_ascii <- "no"

  # Remove Name Punctuation
  if(missing(remove_name_punctuation) || (remove_name_punctuation != "yes" && remove_name_punctuation != "no"))
    remove_name_punctuation <- "no"

  # Compress Name White space
  if(missing(compress_name_whitespace) || (compress_name_whitespace != "yes" && compress_name_whitespace != "no"))
    compress_name_whitespace <- "no"

  # List Given Names
  if(missing(list_all_curr_given_names) || (list_all_curr_given_names != "upper" && list_all_curr_given_names != "lower"))
    list_all_curr_given_names <- "default"

  # List Surnames
  if(missing(list_all_curr_surnames) || (list_all_curr_surnames != "upper" && list_all_curr_surnames != "lower"))
    list_all_curr_surnames <- "default"

  # List All Names
  if(missing(list_all_curr_names) || (list_all_curr_names != "upper" && list_all_curr_names != "lower"))
    list_all_curr_names <- "default"
  #----------------------------------------------------------------------------#

  # SEX
  #----------------------------------------------------------------------------#
  # Impute Sex
  if(missing(impute_sex) || (impute_sex != "yes" && impute_sex != "no"))
    impute_sex <- "no"

  # Impute Sex Type
  if(missing(impute_sex_type) || (impute_sex_type != "custom" && impute_sex_type != "internal"))
    impute_sex_type <- "default"

  # Chosen Sex Imputation File
  if(missing(chosen_sex_file) || (!file.exists(chosen_sex_file)))
    chosen_sex_file <- "none"
  #----------------------------------------------------------------------------#

  # LOCATIONS
  #----------------------------------------------------------------------------#
  # Compress Location Based Fields White space
  if(missing(compress_location_whitespace) || (compress_location_whitespace != "yes" && compress_location_whitespace != "no"))
    compress_location_whitespace <- "no"

  # Remove Location Based Fields Punctuation
  if(missing(remove_location_punctuation) || (remove_location_punctuation != "yes" && remove_location_punctuation != "no"))
    remove_location_punctuation <- "no"

  # Convert Location Based Fields Case
  if(missing(convert_location_case) || (convert_location_case != "upper" && convert_location_case != "lower"))
    convert_location_case <- "default"

  # Convert Location Based Fields to ASCII
  if(missing(convert_location_to_ascii) || (convert_location_to_ascii != "yes" && convert_location_to_ascii != "no"))
    convert_location_to_ascii <- "no"

  # Convert Location Based Fields to ASCII
  if(missing(extract_postal_code) || (extract_postal_code != "yes" && extract_postal_code != "no"))
    extract_postal_code <- "no"
  #----------------------------------------------------------------------------#

  # OUTPUT & FILE READING
  #----------------------------------------------------------------------------#
  # Convert Location Based Fields to ASCII
  if(missing(file_output) || (file_output != "csv" && file_output != "rds"))
    file_output <- "sqlite"

  # Convert Location Based Fields to ASCII
  if(missing(output_non_linkage_fields) || (output_non_linkage_fields != "yes" && output_non_linkage_fields != "no"))
    output_non_linkage_fields <- "no"

  # Convert Location Based Fields to ASCII
  if(missing(chunk_size) || (chunk_size < 10000 || chunk_size > 1000000))
    chunk_size <- 100000

  # Convert Location Based Fields to ASCII
  if(missing(debug_mode) || (debug_mode != "on" && debug_mode != "off"))
    debug_mode <- "off"

  # Convert Location Based Fields to ASCII
  if(missing(max_file_size_output) || (max_file_size_output <= 0))
    max_file_size_output <- "null"

  if(missing(read_mode) || (read_mode != "cmd"))
    read_mode <- "path"
  #----------------------------------------------------------------------------#

  # Construct the flag lookup tables for standardization [Set this to be in a single source file]
  flag_values <- data.frame(
    flag_code = c("convert_name_case", "convert_name_to_ascii", "remove_name_punctuation","compress_name_whitespace", "list_all_curr_given_names", "list_all_curr_surnames", "list_all_curr_names",
                  "impute_sex", "impute_sex_type", "chosen_sex_file",
                  "compress_location_whitespace", "remove_location_punctuation", "convert_location_case", "convert_location_to_ascii", "extract_postal_code",
                  "file_output","output_non_linkage_fields", "chunk_size", "max_file_size_output", "debug_mode", "read_mode"),
    flag_value = c(convert_name_case, convert_name_to_ascii, remove_name_punctuation, compress_name_whitespace, list_all_curr_given_names, list_all_curr_surnames, list_all_curr_names,
                   impute_sex, impute_sex_type, chosen_sex_file,
                   compress_location_whitespace, remove_location_punctuation, convert_location_case, convert_location_to_ascii, extract_postal_code,
                   file_output, output_non_linkage_fields, chunk_size, max_file_size_output, debug_mode, read_mode)
  )

  # Create the lookup table
  flag_lookup <- setNames(flag_values$flag_value, flag_values$flag_code)

  # Return the lookup table
  return(flag_lookup)
}
