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
      HTML("<b>Given Names & Surnames Flag Options</b>")), align = "center"
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
                        selected = "Yes", width = validateCssUnit(600)),
            bsButton("compress_name_whitespace_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "compress_name_whitespace_help", title = "Compress Name Whitespace - Help",
                      content = paste("Compress Name Whitespace will either remove all white space characters between names <b>(E.g., John Doe --> JohnDoe)</b>.",
                                      "Or it will leave spaces between names in."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("remove_name_punctuation", label = "Remove Punctuation From Names?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "Yes", width = validateCssUnit(600)),
            bsButton("remove_name_punctuation_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "remove_name_punctuation_help", title = "Compress Name Whitespace - Help",
                      content = paste("Remove Name Punctuation will remove any kind of symbols from a name such as <b>slashes, apostrophes, periods, commas, hyphens",
                                      "etc.</b> Or it will leave them in."),
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
        ),
        div(style = "display: flex; justify-content: center; align-items: center;",
            selectInput("convert_name_to_ascii", label = "Convert Persons Name to ASCII?",
                        choices = list("Yes" = "yes",
                                       "No" = "no"),
                        selected = "Yes", width = validateCssUnit(600)),
            bsButton("convert_name_to_ascii_help", label = "", icon = icon("question"), style = "info"),
            bsPopover(id = "convert_name_to_ascii_help", title = "Convert Name to ASCII - Help",
                      content = paste("Convert Name to ASCII will remove any accents or diacritics of a persons name, or not if the user chooses <b>NO</b>."),
                      placement = "right", trigger = "hover",
                      options = list(container = "body"))
        ),
      ),
    align = "center"
    ),
  ),
  #----
  HTML("<br><br><br>"),
  # Address Flag Options
  #----
  fluidRow(
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
      HTML("<b>Address and Locations Flag Options:</b>")), align = "center"
    ),
    column(12, div(
      style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("convert_address_case", label = "What Case Should Addresses Convert To?",
                      choices = list("Upper Case" = "upper",
                                     "Lower Case" = "lower",
                                     "Original Case" = "original"),
                      selected = "original", width = validateCssUnit(600)),
          bsButton("convert_address_case_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "convert_address_case_help", title = "Convert Address Case - Help",
                    content = paste("Convert Address Case will use the input option to either convert all primary or secondary addresses",
                                    "in the dataset to <b>Upper Case</b>, <b>Lower Case</b>, or it will keep the <b>Original Case</b> derived from the data."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("compress_address_whitespace", label = "Should White Space Between Addresses Become Compressed?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "Yes", width = validateCssUnit(600)),
          bsButton("compress_address_whitespace_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "compress_address_whitespace_help", title = "Compress Address Whitespace - Help",
                    content = paste("Compress Address Whitespace will either remove all white space characters between address <b>(E.g., 123 Street -> 123Street)</b>.",
                                    "Or it will leave spaces between addresses in."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("remove_address_punctuation", label = "Remove Punctuation From Addresses?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "Yes", width = validateCssUnit(600)),
          bsButton("remove_address_punctuation_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "remove_address_punctuation_help", title = "Compress Address Whitespace - Help",
                    content = paste("Remove Address Punctuation will remove any kind of symbols from an address such as <b>slashes, apostrophes, periods, commas, hyphens",
                                    "etc.</b> Or it will leave them in."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("convert_address_to_ascii", label = "Convert Addresses to ASCII?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "Yes", width = validateCssUnit(600)),
          bsButton("convert_address_to_ascii_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "convert_address_to_ascii_help", title = "Convert Address to ASCII - Help",
                    content = paste("Convert Address to ASCII will remove any accents or diacritics of a persons name, or not if the user chooses <b>NO</b>."),
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
                      choices = list("Custom File Imputation" = "custom",
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
                                     "RData (.rdata)" = "rdata",
                                     "Excel (.xlsx)" = "xlsx"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("file_output_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "file_output_help", title = "File Output - Help",
                    content = paste("File Output will determine what the output file format is after the file is finished being cleaned and processed."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
      ),
      div(style = "display: flex; justify-content: center; align-items: center;",
          selectInput("output_health_and_program_data", label = "Output Non-linkage Variables in Another File?",
                      choices = list("Yes" = "yes",
                                     "No" = "no"),
                      selected = "no", width = validateCssUnit(600)),
          bsButton("output_health_and_program_data_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "output_health_and_program_data_help", title = "Output Health and Program Data - Help",
                    content = paste("Remove any identifying information from the standardized dataset, keeping only the health and program fields in",
                                    "addition to some fields such as postal code, birthdate, and gender."),
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
          # selectInput("chunking_size", label = "What size chunks should the file be read in?",
          #             choices = list("100,000 Rows" = 100000,
          #                            "200,000 Rows" = 200000,
          #                            "500,000 Rows" = 500000,
          #                            "1,000,000 Rows" = 1000000),
          #             selected = 100000, width = validateCssUnit(600)),
          numericInput("chunking_size", label = "How many rows should be read at a time?", value = NULL, width = validateCssUnit(600)),
          bsButton("chunking_size_help", label = "", icon = icon("question"), style = "info"),
          bsPopover(id = "chunking_size_help", title = "Chunk Size - Help",
                    content = paste("The chunking size is used to determine how many rows to read in at a time when processing data, you may select",
                                    "to read in 100k rows, 200k rows, 500k rows, or 1 million rows at a time."),
                    placement = "right", trigger = "hover",
                    options = list(container = "body"))
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
    list_all_curr_given_names <- input$list_all_curr_given_names
    list_all_curr_surnames <- input$list_all_curr_surnames
    list_all_curr_names <- input$list_all_curr_names

    impute_gender <- input$impute_gender
    impute_gender_type <- input$impute_gender_type
    chosen_gender_file <- gender_file_path$path

    compress_address_whitespace <- input$compress_address_whitespace
    remove_address_punctuation <- input$remove_address_punctuation
    convert_address_case <- input$convert_address_case
    convert_address_to_ascii <- input$convert_address_to_ascii
    extract_postal_codes <- input$extract_postal_codes

    file_output <- input$file_output
    output_health_and_program_data <- input$output_health_and_program_data
    chunking_size <- input$chunking_size

    # Get the file path
    input_file <- file_path$path

    # Get the folder output path
    chosen_folder <- folder_path$path

    # Get the metadata file
    chosen_metadata <- metadata_path$path

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
      flag_code = c("convert_name_case", "convert_name_to_ascii", "remove_name_punctuation","compress_name_whitespace", "list_all_curr_given_names", "list_all_curr_surnames", "list_all_curr_names",
                    "impute_sex", "impute_sex_type", "chosen_sex_file",
                    "compress_address_whitespace", "remove_address_punctuation", "convert_address_case", "convert_address_to_ascii", "extract_postal_code",
                    "file_output",
                    "output_health_and_program_data",
                    "chunk_size"),
      flag_value = c(convert_name_case, convert_name_to_ascii, remove_name_punctuation, compress_name_whitespace, list_all_curr_given_names, list_all_curr_surnames, list_all_curr_names,
                     impute_gender, impute_gender_type, chosen_gender_file,
                     compress_address_whitespace, remove_address_punctuation, convert_address_case, convert_address_to_ascii, extract_postal_codes,
                     file_output,
                     output_health_and_program_data,
                     chunking_size)
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
