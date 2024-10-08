data_standardization_ui <- fluidPage(
  # We will use shinyJS
  useShinyjs(),

  # Tags
  #----
  tags$head(
    tags$style(
      HTML("#shiny-notification-panel {
              top: 0;
              bottom: unset;
              left: 0;
              right: 0;
              margin-left: auto;
              margin-right: auto;
              width: 100%;
              max-width: 450px;
              opacity: 1;
            }"
      ),
      HTML(".shiny-notification-message {
              background-color:#0ec116;
              color:#000000;
              opacity: 1;
              text-align: center;
           }"
      ),
      HTML(".shiny-notification-warning {
              color:#000000;
              opacity: 1;
              text-align: center;
           }"
      ),
      HTML(".shiny-notification-error {
              background-color:#C90000;
              color:#000000;
              opacity: 1;
              text-align: center;
           }"
      ),
      HTML(
        ".dataTables_wrapper caption {
          font-size: 16px;
          font-weight: bold;
          color: black;
          text-align: center;
        }"
      ),
      HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }"
      ),
      HTML(
        ".popover {
          max-width: 50%;
        }"
      ),
    )
  ),
  #----

  # Welcome message
  #----
  fluidRow(
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
      HTML("<b>Data Standardization App</b>")), align = "center"
    ),
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
      HTML(paste("Welcome to the Data Standardization App, here you can upload the raw datasets for standardization once",
                 "you are done adding or modifying any rules for a specific dataset.<br><br>",
                 "To begin, upload a file below and ensure that the first prefix of file name contains the <b>dataset_code</b>",
                 "of the file that you want standardized. For example, the file should be <b>dataset_code</b>, some form",
                 "of separator such as [ _ ], [ - ], [ . ], etc., and then the remaining file preamble.",
                 "(<b>E.g., datasetcode_mh_2024.csv</b>)<br><br>",
                 "Once the file has been uploaded, you may additionally change any of the processing flags from their default values",
                 "using the information boxes to determine how the data will be further processed.<br><br>",
                 "When you are ready to begin standardizing, press the large green button near the bottom and then let",
                 "the file process. Fully processed/clean files will appear in the output folder, and if any issues arise during",
                 "standardization, an <b>ERRORS_README.txt</b> file will appear respectively in a new error folder."))),
      align = "center"
    ),
  ),
  #----
  HTML("<br><br><br>"),
  # Upload Source File
  #----
  fluidRow(
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
      HTML("<b>Upload Source Data File</b>")), align = "center"
    ),
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
      #fileInput("source_file", "Upload a File:"),
        fluidRow(
          column(12, actionButton("choose_file", label = "Select Source File", class = "btn-primary"), align = "center"),
          HTML("<br><br><br>"),
          uiOutput("chosen_file")
        ),
        fluidRow(
          column(12, actionButton("choose_folder", label = "Select Output Folder", class = "btn-primary"), align = "center"),
          HTML("<br><br><br>"),
          uiOutput("chosen_folder")
        ),
        fluidRow(
          column(12, actionButton("choose_metadata", label = "Select Metadata File", class = "btn-primary"), align = "center"),
          HTML("<br><br><br>"),
          uiOutput("chosen_metadata")
        ),
      ),
      align = "center"
    ),
  ),
  #----
  HTML("<br><br><br>"),
  # Name Flag Options
  #----
  fluidRow(
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
      HTML("<b>Given Name & Surname Fields Flag Options</b>")), align = "center"
    ),
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("convert_name_case", label = "What Case Should Names Convert To?",
                        choices = list("Upper Case" = "upper",
                                       "Lower Case" = "lower",
                                       "Original Case" = "original"),
                        selected = "original", width = validateCssUnit(600)),
              bsButton("convert_name_case_help", label = "", icon = icon("question"), style = "info"),
              bsPopover(id = "convert_name_case_help", title = "Convert Name Case - Help",
                        content = paste("Convert Name Case will use the input option to either convert all given names, surnames, or prior surnames",
                                        "in the dataset to <b>Upper Case</b>, <b>Lower Case</b>, or it will keep the <b>Original Case</b> derived from the data."),
                        placement = "right", trigger = "hover",
                        options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("compress_name_whitespace", label = "Should White Space Between Names Become Compressed?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "no", width = validateCssUnit(600)),
            bsButton("compress_name_whitespace_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "compress_name_whitespace_help", title = "Compress Name Whitespace - Help",
                      content = paste("Compress Name Whitespace will either remove all white space characters between names <b>(E.g., John Doe --> JohnDoe)</b>.",
                                      "Or it will leave spaces between names in."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("remove_titles_and_suffix", label = "Should Titles or Suffix like Dr, Mr, Ms, Jr, Sr, III be removed?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "no", width = validateCssUnit(600)),
            bsButton("remove_titles_and_suffix_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "remove_titles_and_suffix_help", title = "Remove Titles and Suffix - Help",
                      content = paste("Remove titles or suffix will replace them with an empty string <b>(Dr John Doe --> John Doe</b>.",
                                      "Or it will leave the names as is."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("extract_middle_initial", label = "Extract the Middle Initial from Names?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "no", width = validateCssUnit(600)),
            bsButton("extract_middle_initial_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "extract_middle_initial_help", title = "Remove Titles and Suffix - Help",
                      content = paste("Extracts the middle initials from names and places in a separate column <b>(David --> D</b>.",
                                      "Or it will leave the names as is."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("remove_name_punctuation", label = "Remove Punctuation From Names?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "no", width = validateCssUnit(600)),
            bsButton("remove_name_punctuation_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "remove_name_punctuation_help", title = "Compress Name Whitespace - Help",
                      content = paste("Remove Name Punctuation will remove any kind of symbols from a name such as <b>slashes, apostrophes, periods, commas, hyphens",
                                      "etc.</b> Or it will leave them in."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("convert_name_to_ascii", label = "Convert Persons Name to ASCII?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "no", width = validateCssUnit(600)),
            bsButton("convert_name_to_ascii_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "convert_name_to_ascii_help", title = "Convert Name to ASCII - Help",
                      content = paste("Convert Name to ASCII will remove any accents or diacritics of a persons name, or not if the user chooses <b>NO</b>."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("list_all_curr_given_names", label = "Add Output Column Containing All Given Names of a Person?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "no", width = validateCssUnit(600)),
            bsButton("list_all_curr_given_names_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "list_all_curr_given_names_help", title = "List Given Names - Help",
                      content = paste("List Given Names will add an additional column to the output file which contains all given names of a single person, for each",
                                      "record. Or will not if the option is <b>NO</b>."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("list_all_curr_surnames", label = "Add Output Column Containing All Surnames of a Person?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "no", width = validateCssUnit(600)),
            bsButton("list_all_curr_surnames_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "list_all_curr_surnames_help", title = "List Surnames - Help",
                      content = paste("List Surnames will add an additional column to the output file which contains all surnames of a single person, for each",
                                      "record. Or will not if the option is <b>NO</b>."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("list_all_curr_names", label = "Add Output Column Containing All Given Names & Surnames of a Person?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "no", width = validateCssUnit(600)),
            bsButton("list_all_curr_names_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "list_all_curr_names_help", title = "List Names - Help",
                      content = paste("List Surnames will add an additional column to the output file which contains all given names AND surnames of a single person, for each",
                                      "record. Or will not if the option is <b>NO</b>."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        )
      ),
    align = "center"
    ),
  ),
  #----
  HTML("<br><br><br>"),
  # Location Flag Options
  #----
  fluidRow(
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
      HTML("<b>Location Fields Flag Options:</b>")), align = "center"
    ),
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("convert_location_case", label = "What Case Should Locations Convert To?",
                      choices = list("Upper Case" = "upper",
                                     "Lower Case" = "lower",
                                     "Original Case" = "original"),
                      selected = "original", width = validateCssUnit(600)),
          bsButton("convert_location_case_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "convert_location_case_help", title = "Convert Location Case - Help",
                    content = paste("Convert Location Case will use the input option to either convert all location fields",
                                    "in the dataset to <b>Upper Case</b>, <b>Lower Case</b>, or it will keep the <b>Original Case</b> derived from the data."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("compress_location_whitespace", label = "Should White Space Between Locations Become Compressed?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("compress_location_whitespace_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "compress_location_whitespace_help", title = "Compress Location Whitespace - Help",
                    content = paste("Compress Location Whitespace will either remove all white space characters between locations <b>(E.g., 123 Street -> 123Street)</b>.",
                                    "Or it will leave spaces between addresses in."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("remove_location_punctuation", label = "Remove Punctuation From Locations?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("remove_location_punctuation_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "remove_location_punctuation_help", title = "Compress Location Whitespace - Help",
                    content = paste("Remove Location Punctuation will remove any kind of symbols from a location such as <b>slashes, apostrophes, periods, commas, hyphens",
                                    "etc.</b> Or it will leave them in."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("convert_location_to_ascii", label = "Convert Locations to ASCII?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("convert_location_to_ascii_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "convert_location_to_ascii_help", title = "Convert Location to ASCII - Help",
                    content = paste("Convert Location to ASCII will remove any accents or diacritics of a location, or not if the user chooses <b>NO</b>."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("convert_location_abbreviations", label = "Expand Location Abbreviations?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("convert_location_abbreviations_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "convert_location_abbreviations_help", title = "Expand Location Abbreviations - Help",
                    content = paste("Expand location abbrevations will convert all common abbreviations like <b>St</b> or <b>Rd</b> to their",
                                    "full form like <b>Street</b> and <b>Road</b>, or not if the user chooses <b>NO</b>."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("extract_postal_codes", label = "Extract Alternative Postal Codes from Location Fields?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("extract_postal_codes_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "extract_postal_codes_help", title = "Extract Alternate Postal Codes - Help",
                    content = paste("This will attempt to extract postal codes that may be missing from the postal code field and place them in an alternative column",
                                    "called alt_postal_code. This will attempt to extract from various location fields such as Addresses, Cities, and Countries."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
    ),
    align = "center"
    ),
  ),
  #----
  HTML("<br><br><br>"),
  # Sex Flag options
  #----
  fluidRow(
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
      HTML("<b>Sex Flag Options:</b>")), align = "center"
    ),
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("impute_gender", label = "Impute Missing Sex?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("impute_gender_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "impute_gender_help", title = "Impute Sex - Help",
                    content = paste("Attempt to predict the missing sex values in the dataset using the gender package in R,",
                                    "make note of imputation values and whether or not we computed them."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("impute_gender_type", label = "How Should Sex be Imputed?",
                      choices = list("Gender & Genderdata Package" = "default",
                                     "Custom File Imputation" = "custom",
                                     "Internal Imputation" = "internal"),
                      selected = "default", width = validateCssUnit(600)),
          bsButton("impute_gender_type_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "impute_gender_type_help", title = "Impute Sex Type - Help",
                    content = paste("The impute gender function will either use an internal method by using the source data to",
                                    "try and infer sex based on a first name and its matching gender.<br><br>",
                                    "Otherwise, a custom file may be submitted which consists of only <b>2 COLUMNS</b>, the first",
                                    "column being called <b>primary_given_name</b> and second being called <b>sex</b>."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))

      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          actionButton("choose_gender_file", label = "Select Sex Imputation File", class = "btn-primary")
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          uiOutput("chosen_gender_file")
      )
    ),
    align = "center"
    ),
  ),
  #----
  HTML("<br><br><br>"),
  # Output Flag options
  #----
  fluidRow(
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
      HTML("<b>Output Flag Options:</b>")), align = "center"
    ),
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("file_output", label = "What is the File Output Format?",
                      choices = list("SQLite (.sqlite)" = "sqlite",
                                     "Comma Separated Values (.csv)" = "csv",
                                     "RData (.Rds)" = "rds"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("file_output_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "file_output_help", title = "File Output - Help",
                    content = paste("File Output will determine what the output file format is after the file is finished being cleaned and processed."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("output_non_linkage_fields", label = "Output Non-linkage Variables in Another File?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("output_non_linkage_fields_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "output_non_linkage_fields_help", title = "Output Non-linkage Fields - Help",
                    content = paste("Remove any identifying information from the standardized dataset, keeping only the non-linkage fields, in",
                                    "addition to <b>some</b> linkage fields (postal code, birthdate, and gender)."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      )
    ),
    align = "center"
    ),
  ),
  #----
  HTML("<br><br><br>"),
  # Reading Options
  #----
  fluidRow(
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
      HTML("<b>Chunked Reading Flag Options:</b>")), align = "center"
    ),
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
      div(style = "display: flex; justify-content: center; align-items: center;",
          numericInput("chunking_size", label = "How many rows should be read at a time?", value = 100000, width = validateCssUnit(600)),
          bsButton("chunking_size_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "chunking_size_help", title = "Chunk Size - Help",
                    content = paste("The chunking size is used to determine how many rows to read in at a time when processing data, you may enter",
                                    "an integer >= 10000 or <= 1000000."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("read_mode", label = "What is the File Output Format?",
                      choices = list("File Path" = "path",
                                     "Shell Commands" = "cmd"),
                      selected = "path", width = validateCssUnit(600)),
          bsButton("read_mode_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "read_mode_help", title = "Read Mode - Help",
                    content = paste("Read the data using the normal file path, or use shell commands to help with processing."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      fluidRow(
        column(12, actionButton("choose_imputation_output", label = "Select Imputation Output Path", class = "btn-primary"), align = "center"),
        HTML("<br><br><br>"),
        uiOutput("imputation_output")
      ),
    ),
    align = "center"
    ),
  ),
  #----
  HTML("<br><br><br>"),
  fluidRow(
    column(12, actionButton("standardize_data", "Standardize Data", class = "btn-success btn-lg"), align = "center")
  ),
  HTML("<br><br><br>")
  #----

)


data_standardization_server <- function(input, output, session){

  file_path <- reactiveValues(
    path=NULL
  )

  gender_file_path <- reactiveValues(
    path=NULL
  )

  folder_path <- reactiveValues(
    path=NULL
  )

  imputation_path <- reactiveValues(
    path=NULL
  )

  metadata_path <- reactiveValues(
    path=NULL
  )

  observe({
    type <- input$impute_gender_type

    if((!is.null(type) && type != "custom") || input$impute_gender == "no"){
      disable("choose_gender_file")
    }else{
      enable("choose_gender_file")
    }
  })

  observeEvent(input$choose_file,{
    tryCatch({
      file_path$path <- file.choose()
    },
    error = function(e){
      file_path$path <- NULL
    })
  })

  observeEvent(input$choose_folder,{
    tryCatch({
      folder_path$path <- choose.dir()
    },
    error = function(e){
      folder_path$path <- NULL
    })

    if(is.na(folder_path$path)){
      folder_path$path <- NULL
    }
  })

  observeEvent(input$choose_imputation_output,{
    tryCatch({
      imputation_path$path <- choose.dir()
    },
    error = function(e){
      imputation_path$path <- NULL
    })

    if(is.na(imputation_path$path)){
      imputation_path$path <- NULL
    }
  })

  observeEvent(input$choose_metadata,{
    tryCatch({
      metadata_path$path <- file.choose()
    },
    error = function(e){
      metadata_path$path <- NULL
    })
  })

  observeEvent(input$choose_gender_file,{
    tryCatch({
      gender_file_path$path <- file.choose()
    },
    error = function(e){
      gender_file_path$path <- NULL
    })
  })

  observe({
    chosen_file <- file_path$path
    chosen_gender_file <- gender_file_path$path
    chosen_folder <- folder_path$path
    chosen_metadata <- metadata_path$path
    imputation_output <- imputation_path$path

    #Input File
    if(is.null(chosen_file)){
      output$chosen_file <- renderUI({
        column(12, HTML("No File Chosen"), align = "center")
      })
    }
    else{
      output$chosen_file <- renderUI({
        column(12, HTML(paste("<b>File Selected:</b>", basename(file_path$path))), align = "center")
      })
    }

    # Sex File
    if(is.null(chosen_gender_file)){
      output$chosen_gender_file <- renderUI({
        column(12, HTML("No File Chosen"), align = "center")
      })
    }
    else{
      output$chosen_gender_file <- renderUI({
        column(12, HTML(paste("<b>File Selected:</b>", basename(gender_file_path$path))), align = "center")
      })
    }

    # Folder
    if(is.null(chosen_folder)){
      output$chosen_folder <- renderUI({
        column(12, HTML("No Folder Chosen"), align = "center")
      })
    }
    else{
      output$chosen_folder <- renderUI({
        column(12, HTML(paste("<b>Folder Selected:</b>", basename(folder_path$path))), align = "center")
      })
    }

    # Folder
    if(is.null(imputation_output)){
      output$imputation_output <- renderUI({
        column(12, HTML("No Folder Chosen"), align = "center")
      })
    }
    else{
      output$imputation_output <- renderUI({
        column(12, HTML(paste("<b>Folder Selected:</b>", basename(imputation_path$path))), align = "center")
      })
    }

    # Metadata File
    if(is.null(chosen_metadata)){
      output$chosen_metadata <- renderUI({
        column(12, HTML("No Metadata File Chosen"), align = "center")
      })
    }
    else{
      output$chosen_metadata <- renderUI({
        column(12, HTML(paste("<b>Metadata File Selected:</b>", basename(metadata_path$path))), align = "center")
      })
    }
  })

  observeEvent(input$standardize_data, {
    disable("standardize_data")

    # Get the variables from the select input
    convert_name_case <- input$convert_name_case
    convert_name_to_ascii <- input$convert_name_to_ascii
    remove_name_punctuation <- input$remove_name_punctuation
    compress_name_whitespace <- input$compress_name_whitespace
    remove_titles_and_suffix <- input$remove_titles_and_suffix
    extract_middle_initial <- input$extract_middle_initial
    list_all_curr_given_names <- input$list_all_curr_given_names
    list_all_curr_surnames <- input$list_all_curr_surnames
    list_all_curr_names <- input$list_all_curr_names

    impute_gender <- input$impute_gender
    impute_gender_type <- input$impute_gender_type
    chosen_gender_file <- gender_file_path$path

    compress_location_whitespace <- input$compress_location_whitespace
    remove_location_punctuation <- input$remove_location_punctuation
    convert_location_case <- input$convert_location_case
    convert_location_to_ascii <- input$convert_location_to_ascii
    convert_location_abbreviations <- input$convert_location_abbreviations
    extract_postal_codes <- input$extract_postal_codes

    file_output <- input$file_output
    output_non_linkage_fields <- input$output_non_linkage_fields
    chunking_size <- input$chunking_size
    read_mode <- input$read_mode

    # Get the file path
    input_file <- file_path$path

    # Get the folder output path
    chosen_folder <- folder_path$path

    # Get the metadata file
    chosen_metadata <- metadata_path$path

    # Get the imputation metadata output path
    imputation_metadata_path <- imputation_path$path

    #----# Error Handling
    if(is.null(input_file)){
      showNotification("Failed to Standardize Dataset - Missing Input File", type = "error", closeButton = FALSE)
      enable("standardize_data")
      return()
    }

    if(is.null(chosen_gender_file) && impute_gender_type == "custom" && impute_gender == "yes"){
      showNotification("Failed to Standardize Dataset - Custom Sex Imputation File is Missing", type = "error", closeButton = FALSE)
      enable("standardize_data")
      return()
    }

    if(is.null(imputation_metadata_path)){
      imputation_metadata_path <- "null"
    }

    if(is.null(chosen_folder)){
      showNotification("Failed to Standardize Dataset - Missing Output Folder", type = "error", closeButton = FALSE)
      enable("standardize_data")
      return()
    }

    if(is.null(chosen_gender_file)){
      chosen_gender_file <- "NULL"
    }

    if(is.null(chosen_metadata) || file_ext(chosen_metadata) != "sqlite"){
      showNotification("Failed to Standardize Dataset - Invalid SQLite File", type = "error", closeButton = FALSE)
      enable("standardize_data")
      return()
    }

    if(is.na(chunking_size) || is.null(chunking_size) || (chunking_size < 10000 || chunking_size > 1000000)){
      showNotification("Failed to Standardize Dataset - Invalid chunking size. Must be >= 10k and <= 1M", type = "error", closeButton = FALSE)
      enable("standardize_data")
      return()
    }
    #----#

    # Construct the flag lookup tables for standardization
    flag_values <- data.frame(
      flag_code = c("convert_name_case", "convert_name_to_ascii", "remove_name_punctuation","compress_name_whitespace", "list_all_curr_given_names", "list_all_curr_surnames", "list_all_curr_names", "remove_titles_and_suffix", "extract_middle_initial",
                    "impute_sex", "impute_sex_type", "chosen_sex_file",
                    "compress_location_whitespace", "remove_location_punctuation", "convert_location_case", "convert_location_to_ascii", "convert_location_abbreviations", "extract_postal_code",
                    "file_output", "output_non_linkage_fields", "chunk_size", "read_mode", "imputation_metadata_path"),
      flag_value = c(convert_name_case, convert_name_to_ascii, remove_name_punctuation, compress_name_whitespace, list_all_curr_given_names, list_all_curr_surnames, list_all_curr_names, remove_titles_and_suffix, extract_middle_initial,
                     impute_gender, impute_gender_type, chosen_gender_file,
                     compress_location_whitespace, remove_location_punctuation, convert_location_case, convert_location_to_ascii, convert_location_abbreviations, extract_postal_codes,
                     file_output,output_non_linkage_fields, chunking_size, read_mode, imputation_metadata_path)
    )
    #print(flag_values)
    flag_lookup <- setNames(flag_values$flag_value, flag_values$flag_code)

    # Get the File path
    file_path <- input_file

    # Get the dataset_code
    file_name <- basename(input_file)
    split_file_name <- unlist(strsplit(file_name, "[[:punct:]]"))
    dataset_code <- split_file_name[1]

    # Get the file extension and do some error handling
    file_extension <- tools::file_ext(input_file)
    #print(file_extension)
    if(file_extension != "csv" && file_extension != "txt" && file_extension != "sas7bdat"){
      showNotification("Failed to Standardize Dataset - Invalid File Format, Select a New File", type = "error", closeButton = FALSE)
      enable("standardize_data")
      return()
    }

    # Get the path to the output folder
    output_path <- folder_path$path

    tryCatch({
      standardize_data(file_path, dataset_code, flag_lookup, output_path, chosen_metadata)
      showNotification("Standardize Data Successfully Completed - Check Output Folder for File", type = "message", closeButton = FALSE)
    },
    error = function(e){
      print(e)
      showNotification(paste("Failed to Standardize Data -", geterrmessage()), type = "error", closeButton = FALSE)
    })

    gc() # Call garbage collector after we finish processing
    Sys.sleep(5)
    enable("standardize_data")

  })
}

#' Start Standardization UI
#'
#' Will start the local shiny app upon call, from here the user can use the standardizing function
#' in a user friendly way by way of the GUI.
#' @examples
#' startDataStandardizationUI()
#' @export
startDataStandardizationUI <- function(){
  # Start App ----
  shinyApp(ui = data_standardization_ui, server = data_standardization_server,
           onStart = function() {
             useShinyjs()
             cat("App Started")
             onStop(function() {
               cat("App Ended")
             })
           })
}
