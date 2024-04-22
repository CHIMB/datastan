#' Create New Metadata
#'
#' The create_new_metadata() function will run all the insert and create queries to build
#' the tables and provide some initial metadata values. The function takes two parameters,
#' one for the file name that the user will want it to be called, and one for where the
#' file will be output.
#' @param file_name The file name for what the new .sqlite metadata file will be called.
#' @param output_folder A path to the output folder, where the new metadata file will be output.
#' @examples
#' create_new_metadata("my_new_metadata", "path/to/folder")
#' @export
create_new_metadata <- function(file_name, output_folder){
  # Error handling to ensure the file name and output folder is all valid
  #----
  if(is.null(file_name) || is.null(output_folder) || is.na(file_name) || is.na(output_folder)){
    stop("ERROR: Invalid file name or output folder.")
  }
  #----

  # Create the metadata connection
  my_db <- dbConnect(RSQLite::SQLite(), paste0(output_folder, "/", file_name, ".sqlite"))

  # Run the CREATE TABLE queries so that the metadata file has all that it needs
  # for the metadata UI and data standardization.
  #----
  dbExecute(my_db, "
  CREATE TABLE file_formats (
    file_format_id INTEGER PRIMARY KEY,
    file_format_label VARCHAR(255),
    file_extension_code VARCHAR(255)
  );
")

  dbExecute(my_db, "
  CREATE TABLE datasets (
    dataset_id INTEGER PRIMARY KEY,
    dataset_code VARCHAR(255),
    dataset_name VARCHAR(255),
    has_header INTEGER,
    file_format_id INTEGER REFERENCES file_formats(file_format_id),
    enabled_for_standardization INTEGER
  );
")

  dbExecute(my_db, "
  CREATE TABLE destination_fields (
    destination_field_id INTEGER PRIMARY KEY,
    destination_field_name VARCHAR(255),
    destination_field_description VARCHAR(255)
  );
")

  dbExecute(my_db, "
  CREATE TABLE standardizing_modules (
    standardizing_module_id INTEGER PRIMARY KEY,
    standardizing_module_name VARCHAR(255),
    description VARCHAR(255),
    destination_field_id INTEGER REFERENCES destination_fields(destination_field_id),
    output_program_data INTEGER,
    standardizing_module_type INTEGER
  );
")

  dbExecute(my_db, "
  CREATE TABLE source_fields (
    dataset_id INTEGER REFERENCES datasets(dataset_id),
    source_field_id INTEGER PRIMARY KEY,
    source_field_name VARCHAR(255),
    field_order INTEGER,
    fixed_width_length INTEGER,
    standardizing_module_id INTEGER REFERENCES standardizing_modules(standardizing_module_id)
  );
")

  dbExecute(my_db, "
  CREATE TABLE record_priority_fields (
    source_field_id INTEGER REFERENCES source_fields(source_field_id),
    source_value VARCHAR(255),
    priority INTEGER,
    PRIMARY KEY (source_field_id, source_value)
  );
")

  dbExecute(my_db, "
  CREATE TABLE categorical_fields (
    source_field_id INTEGER REFERENCES source_fields(source_field_id),
    source_value VARCHAR(255),
    standardized_value_id INTEGER,
    PRIMARY KEY (source_field_id, source_value)
  );
")

  dbExecute(my_db, "
  CREATE TABLE categorical_values (
    standardized_value_id INTEGER REFERENCES categorical_fields(standardized_value_id),
    standardized_value VARCHAR(255),
    PRIMARY KEY (standardized_value_id)
  );
")

  dbExecute(my_db, "
  CREATE TABLE numeric_date_formats (
    numeric_date_format_id INTEGER PRIMARY KEY,
    numeric_date_format_label VARCHAR(255),
    origin_date VARCHAR(255),
    units_label VARCHAR(255)
  );
")

  dbExecute(my_db, "
  CREATE TABLE numeric_date_destinations (
    numeric_destination_type INTEGER,
    destination_mapping_order INTEGER,
    destination_field_id INTEGER REFERENCES destination_fields(destination_field_id),
    PRIMARY KEY (numeric_destination_type, destination_mapping_order)
  );
")

  dbExecute(my_db, "
  CREATE TABLE numeric_date_fields (
    source_field_id INTEGER REFERENCES source_fields(source_field_id),
    numeric_date_format_id INTEGER REFERENCES numeric_date_formats(numeric_date_format_id),
    numeric_destination_type INTEGER,
    PRIMARY KEY (source_field_id)
  );
")


  dbExecute(my_db, "
  CREATE TABLE compound_field_formats (
    compound_field_format_id INTEGER PRIMARY KEY,
    compound_format VARCHAR(255),
    format_description VARCHAR(255)
  );
")

  dbExecute(my_db, "
  CREATE TABLE compound_fields (
    source_field_id INTEGER REFERENCES source_fields(source_field_id),
    compound_field_format_id INTEGER REFERENCES compound_field_formats(compound_field_format_id),
    PRIMARY KEY (source_field_id)
  );
")

  dbExecute(my_db, "
  CREATE TABLE compound_field_separators (
    compound_field_format_id INTEGER REFERENCES compound_field_formats(compound_field_format_id),
    separator_order INTEGER,
    separator VARCHAR(255),
    substring_index INTEGER,
    PRIMARY KEY (compound_field_format_id, separator_order)
  );
")

  dbExecute(my_db, "
  CREATE TABLE compound_field_destinations (
    compound_field_format_id INTEGER REFERENCES compound_field_formats(compound_field_format_id),
    destination_mapping_order INTEGER,
    destination_field_id INTEGER REFERENCES destination_fields(destination_field_id),
    PRIMARY KEY (compound_field_format_id, destination_mapping_order)
  );
")
  #----

  # Insert Statements for Standardizing File Formats
  #----
  new_entry_query <- paste('INSERT INTO file_formats (file_format_id, file_format_label, file_extension_code)',
                           'VALUES(1, "SAS", "sas7bdat");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO file_formats (file_format_id, file_format_label, file_extension_code)',
                           'VALUES(2, "Fixed Width", "fwf");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO file_formats (file_format_id, file_format_label, file_extension_code)',
                           'VALUES(3, "Regular Text", "txt");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO file_formats (file_format_id, file_format_label, file_extension_code)',
                           'VALUES(4, "Comma Seperated Values", "csv");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO file_formats (file_format_id, file_format_label, file_extension_code)',
                           'VALUES(5, "Database", "db");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)
  #----

  # Insert Statements for Destination Fields
  #----
  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(1, "record_primary_key", "Uniquely identifies records");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(2, "individual_id", "Uniquely identifies individuals");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(3, "phin", "Manitoba Health Personal Health Identification Number (PHIN)");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(4, "registration_no", "Manitoba Health Registration Number");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(5, "primary_given_name", "Primary (first) given name");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(6, "secondary_given_names", "Secondary given names (i.e., middle name)");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(7, "primary_surname", "Primary last name / family name");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(8, "prior_surname", "Previous surname (e.g., maiden name)");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(9, "secondary_surnames", "Secondary surnames not listed in prior_surname field");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(10, "birth_year", "Birth year");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(11, "birth_month", "Birth month");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(12, "birth_day", "Birth day");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(13, "gender", "Gender");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(14, "address1", "Residential address 1");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(15, "address2", "Residential address 2");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(16, "city", "City, town, or village");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(17, "province", "Canadian province");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(18, "country", "Country");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(19, "postal_code", "Canadian postal code");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(20, "acquisition_year", "Year of capture");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(21, "acquisition_month", "Month of capture");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(22, "acquisition_day", "Day of capture");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO destination_fields (destination_field_id, destination_field_name, destination_field_description)',
                           'VALUES(23, "record_priority", "Priority of record (1 being the highest)");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)
  #----

  # Insert Statements for Standardizing Modules
  #----
  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(1, "record_primary_key", "The key used to identify a specific record in the dataset.", 1, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(2, "individual_id", "The key used to identify a specific individual in the dataset.", 2, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(3, "phin", "The field containing the Personal Health Identification Number.", 3, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(4, "registration_no", "The field containing the Registration Number of a person.", 4, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(5, "primary_given_name", "The field containing a persons Primary Given name.", 5, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(6, "secondary_given_name", "The field containing a persons Secondary Given name.", 6, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(7, "primary_surname", "The field containing a persons Primary Surname.", 7, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(8, "prior_surname", "The field containing a persons Prior Surname.", 8, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(9, "secondary_surname", "The field containing a persons Secondary Surname.", 9, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(10, "birth_date", "The field containing the compounded birth date of a person. This will require additional information in the form of a compound format.", NULL, 1, 2);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(11, "birth_year", "The field containing the birth year of a person on its own.", 10, 1, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(12, "birth_month", "The field containing the birth month of a person on its own.", 11, 1, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(13, "birth_day", "The field containing the birth day of a person on its own.", 12, 1, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(14, "gender", "The field containing the gender of a person. This will require additional information in the form of categorical fields.", 13, 1, 3);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(15, "address1", "The field containing the primary address of a person.", 14, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(16, "address2", "The field containing the secondary addresses of a person.", 15, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(17, "city", "The field containing the city the person lives in.", 16, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(18, "province", "The field containing the province the person lives in.", 17, 0, 3);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(19, "country", "The field containing the country the person lives in.", 18, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(20, "postal_code", "The field containing a persons postal codes.", 19, 1, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(21, "acquisition_date", "The field containing the compounded acquisiton date of this record. This will require additional information in the form of a compound format.", NULL, 0, 2);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(22, "acquisition_year", "The field containing the acquisition year of the record on its own.", 20, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(23, "acquisition_month", "The field containing the acquisition month of the record on its own.", 21, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(24, "acquisition_day", "The field containing the acquisition day of the record on its own.", 22, 0, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(25, "compound_name", "The field containing the compounded name of a person. This will require additional information in the form of a compound format.", NULL, 0, 2);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(26, "numeric_date", "The field containing the birthdate of a person or acqusition date of the record in a date-time format. This will require additional information in the form of a numeric format.", NULL, 1, 4);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(27, "pass_through_field", "The field that is used to perform linkages in a way where the values of the field do not need to be standardized.", NULL, 1, 1);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO standardizing_modules (standardizing_module_id, standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)',
                           'VALUES(28, "record_priority", "The field that is used to break ties, replaces values with priorities ranging from 1 -> N where 1 is the highest priority and N is lowest.", 23, 0, 5);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)
  #----

  # Insert Statements for Categorical Values
  #----
  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(1, "M");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(2, "F");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(3, "X");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(4, "NL");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(5, "PE");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(6, "NS");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(7, "NB");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(8, "QC");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(9, "ON");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(10, "MB");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(11, "SK");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(12, "AB");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(13, "BC");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(14, "YT");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(15, "NT");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO categorical_values (standardized_value_id, standardized_value)',
                           'VALUES(16, "NU");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)
  #----

  # Insert Statements for Numeric Date Destinations
  #----
  new_entry_query <- paste('INSERT INTO numeric_date_destinations (numeric_destination_type, destination_mapping_order, destination_field_id)',
                           'VALUES(1, 1, 10);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO numeric_date_destinations (numeric_destination_type, destination_mapping_order, destination_field_id)',
                           'VALUES(1, 2, 11);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO numeric_date_destinations (numeric_destination_type, destination_mapping_order, destination_field_id)',
                           'VALUES(1, 3, 12);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO numeric_date_destinations (numeric_destination_type, destination_mapping_order, destination_field_id)',
                           'VALUES(2, 1, 20);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO numeric_date_destinations (numeric_destination_type, destination_mapping_order, destination_field_id)',
                           'VALUES(2, 2, 21);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO numeric_date_destinations (numeric_destination_type, destination_mapping_order, destination_field_id)',
                           'VALUES(2, 3, 22);')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  #----

  # Insert Statements for Numeric Date Formats
  #----
  new_entry_query <- paste('INSERT INTO numeric_date_formats (numeric_date_format_id, numeric_date_format_label, origin_date, units_label)',
                           'VALUES(1, "SAS/Stata", "1960-01-01", "Days");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO numeric_date_formats (numeric_date_format_id, numeric_date_format_label, origin_date, units_label)',
                           'VALUES(2, "UNIX", "1970-01-01", "Seconds");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO numeric_date_formats (numeric_date_format_id, numeric_date_format_label, origin_date, units_label)',
                           'VALUES(3, "SPSS", "1582-10-14", "Seconds");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_entry_query <- paste('INSERT INTO numeric_date_formats (numeric_date_format_id, numeric_date_format_label, origin_date, units_label)',
                           'VALUES(4, "Windows OLE", "1899-12-30", "Days");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)
  #----

  # Insert Statements for Starter Compound Formats
  #----

  #~~~~
  new_entry_query <- paste('INSERT INTO compound_field_formats (compound_field_format_id, compound_format, format_description)',
                           'VALUES(1, "yyyy-mm-dd", "Birthdate separated by hyphens in the order year, month, day.");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                           'VALUES(1, 1, "-", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(1, 2, "-", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                               'VALUES(1, 1, 10);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(1, 2, 11);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(1, 3, 12);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)
  #~~~~

  #~~~~
  new_entry_query <- paste('INSERT INTO compound_field_formats (compound_field_format_id, compound_format, format_description)',
                           'VALUES(2, "mm/dd/yyyy", "Birthdate separated by back slashes in the order month, day, year.");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(2, 1, "/", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(2, 2, "/", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(2, 1, 11);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(2, 2, 12);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(2, 3, 10);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)
  #~~~~

  #~~~~
  new_entry_query <- paste('INSERT INTO compound_field_formats (compound_field_format_id, compound_format, format_description)',
                           'VALUES(3, "dd/mm/yyyy", "Birthdate separated by back slashes in the order day, month, year.");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(3, 1, "/", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(3, 2, "/", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(3, 1, 12);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(3, 2, 11);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(3, 3, 10);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)
  #~~~~

  #~~~~
  new_entry_query <- paste('INSERT INTO compound_field_formats (compound_field_format_id, compound_format, format_description)',
                           'VALUES(4, "DDMMYYYY", "Birthdate without separators, in the form of a 2-digit day, 2-digit month and 4-digit year.");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(4, 1, NULL, 2);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(4, 2, NULL, 2);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(4, 3, NULL, 4);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(4, 1, 12);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(4, 2, 11);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(4, 3, 10);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)
  #~~~~

  #~~~~
  new_entry_query <- paste('INSERT INTO compound_field_formats (compound_field_format_id, compound_format, format_description)',
                           'VALUES(5, "yyyy-mm-dd", "Acquisition date separated by hyphens in the order year, month, day, with a space separating hours and minutes.");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(5, 1, "-", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(5, 2, "-", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(5, 1, 20);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(5, 2, 21);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(5, 3, 22);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)
  #~~~~

  #~~~~
  new_entry_query <- paste('INSERT INTO compound_field_formats (compound_field_format_id, compound_format, format_description)',
                           'VALUES(6, "LN,FN MN", "Compound name separated by a comma, then space, in the form last name, first name, then middle.");')
  new_entry <- dbSendStatement(my_db, new_entry_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(6, 1, ",", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_separator_query <- paste('INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)',
                               'VALUES(6, 2, " ", NULL);')
  new_entry <- dbSendStatement(my_db, new_separator_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(6, 1, 7);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(6, 2, 5);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)

  new_destination_query <- paste('INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)',
                                 'VALUES(6, 3, 6);')
  new_entry <- dbSendStatement(my_db, new_destination_query)
  dbClearResult(new_entry)
  #~~~~
  #----

  # Finally disconnect
  dbDisconnect(my_db)
}
