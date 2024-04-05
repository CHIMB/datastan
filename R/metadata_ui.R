# UI ----
dataset_ui <- fluidPage(
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
      HTML(
        "body { word-wrap: break-word; }"
      )
    )
  ),
  #----

  titlePanel("Automated Data Standardization"),
  sidebarLayout(
    sidebarPanel(
      span(textOutput("selected_variable"), style = "font-size: 28px; font-family: Arial; color: black;")
    ),
    mainPanel(
      fluidRow(
        column(3, div(style = "display: flex; align-items: center; display: none;",
                      selectInput("modification_form", label = "Select Standardization Page:",
                              choices = list("Home" = "Home",
                                             "View Standardization Rules" = "View Standardization Rules",
                                             "Add and Update Databases" = "Add and Update Databases",
                                             "Manage Database Standardization Rules" = "Manage Database Standardization Rules",
                                             "Enable and Disable Databases" = "Enable and Disable Databases",
                                             "Add Standardizing Module" = "Add Standardizing Module",
                                             "Add Destination Field" = "Add Destination Field"),
                              selected = "Home", width = validateCssUnit(600)),
                      conditionalPanel(
                        condition = "input.modification_form == 'Home'",
                        bsButton("modification_form_help_home", label = "", icon = icon("question"), style = "info"),
                        bsPopover(id = "modification_form_help_home", title = "Welcome Page - Help",
                                  content = paste("The welcome page serves as a main description for the overall goal of the UI.",
                                                  "When selecting a modification form, you may hover over the question mark to learn more about it.",
                                                  "Otherwise, you may choose from the assortment of buttons below to get to your destination.", sep = "<br><br>"),
                                  placement = "right", trigger = "hover",
                                  options = list(container = "body"))
                      ),
                      conditionalPanel(
                        condition = "input.modification_form == 'View Standardization Rules'",
                        bsButton("modification_form_help_view", label = "", icon = icon("question"), style = "info"),
                        bsPopover(id = "modification_form_help_view", title = "View Standardization Rules - Help",
                                  content = paste("The View Standardization Rules page allows you to view all the tables in the database and limit by a specific dataset.",
                                                  "This page is useful for verifying that your desired standardization rules are inplace before pre-processing a dataset.",
                                                  sep = "<br><br>"),
                                  placement = "right", trigger = "hover",
                                  options = list(container = "body"))
                      ),
                      conditionalPanel(
                        condition = "input.modification_form == 'Add and Update Databases'",
                        bsButton("modification_form_help_add_and_update", label = "", icon = icon("question"), style = "info"),
                        bsPopover(id = "modification_form_help_add_and_update", title = "Add and Update Databases - Help",
                                  content = paste("The Add and Update Databases page allows you to either create a new database that can be managed on the",
                                                  "Manage Database Standardization Rules page, or you may select a row from the table which will allow you",
                                                  "to change the dataset name, code, file format, and header information."),
                                  placement = "right", trigger = "hover",
                                  options = list(container = "body"))
                      ),
                      conditionalPanel(
                        condition = "input.modification_form == 'Manage Database Standardization Rules'",
                        bsButton("modification_form_help_manage", label = "", icon = icon("question"), style = "info"),
                        bsPopover(id = "modification_form_help_manage", title = "Manage Database Standardization Rules - Help",
                                  content = paste("The Manage Database Standardization Rules page allows you to either create or update the standardization rules",
                                                  "used to pre-process the unclean datasets.", HTML("<br><br>"), "You will be able to create new Source Fields, Compound Formats,",
                                                  "Categorical Fields, Cateogorical Values, and Numeric Formats. Or you may update existing Source Fields,",
                                                  "Compound Formats, Categorical Fields and Numeric Formats."),
                                  placement = "right", trigger = "hover",
                                  options = list(container = "body"))
                      ),
                      conditionalPanel(
                        condition = "input.modification_form == 'Enable and Disable Databases'",
                        bsButton("modification_form_help_enable_and_disable", label = "", icon = icon("question"), style = "info"),
                        bsPopover(id = "modification_form_help_enable_and_disable", title = "Enable and Disable Databases - Help",
                                  content = paste("The Enable and Disable Databases page allows you to enable and disable specific versions of databases",
                                                  "for standardization, only one database with the same dataset code can be enabled at a time. <br><br>",
                                                  "Note that each dataset with a same dataset code will get its own versioning number to make",
                                                  "identifying which version of the database to use."),
                                  placement = "right", trigger = "hover",
                                  options = list(container = "body"))
                      )

        )),
        column(3, offset = 0, actionButton("return_home", "Home", class = "btn-info btn-lg")),
        column(3, bsButton("app_settings", label = "", icon = icon("gear"), style = "default"), align = "right")
      )
    )
  ),
  fluidPage(
    uiOutput("standardizing_module_type_create"),
    uiOutput("standardizing_module_type_update"),
    uiOutput("created_new_dataset"),
    uiOutput("updated_dataset"),
    uiOutput("created_new_compound_format_new_sf"),
    uiOutput("created_new_compound_format_update_sf"),
    uiOutput("created_new_numeric_format_new"),
    uiOutput("created_new_numeric_format_update"),

    #--------------------------------------------#

    # "Home" conditional panel
    #----
    conditionalPanel(
      condition = "input.modification_form == 'Home'",

      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Welcome Message</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("Welcome to the Automated Data Standardization Metadata app! Within this app you are able to create and modify both the",
                     "datasets and fields within those datasets such that the automatic standardizing script can use the standardizing rules you enforce. <br><br>",
                     "You may use the drop down/select bar above to transition between pages, using the numerous <b>[?]</b> boxes to provide extra descriptions,",
                     "instructions, and rules that you may be unclear about. Otherwise, you may scroll down further and use any of the quick links provided to",
                     "to get to a page quicker."))),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>View Standardization Rules</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you would like to view the metadata standardization rules, datasets, source fields, and everything that goes into",
                     "what the standardizing script would make use of, you may press the button below to transition to that page.<br><br>")),
          actionButton("link_to_view_standardization_rules", "View Standardization Rules?", class = "btn-success btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Add or Update Databases</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you need to create a new database for a new source file, or make changes to an exisiting data bases code, name, file format",
                     "or header information, then you may click the button below which will move you to the correct page.<br><br>",
                     "Otherwise, if you need to enable or disable datasets for standardizing purposes, you may click the other button below which",
                     "will move you to the correct page.<br><br>")),
          actionButton("link_to_add_databases", "Add or Update Databases?", class = "btn-success btn-lg"),
          actionButton("link_to_enable_databases", "Enable or Disable Databases?", class = "btn-danger btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Add or Update Source Fields</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you need to create new source fields for a newly created or pre-existing database, then you may click",
                     "the green button which will directly transition you to the add page.<br><br>",
                     "Otherwise, if you would like to update or delete source fields from an exisiting database, you may click",
                     "the yellow button to transition to the update page.<br><br>")),
          actionButton("link_to_add_source_fields", "Add Source Fields?", class = "btn-success btn-lg"),
          actionButton("link_to_update_source_fields", "Update or Delete Source Fields?", class = "btn-warning btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Add or Update Compound Formats</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you are looking to create new compound formats for dates or names, you may use the green button to transition",
                     "to the correct page.<br><br>",
                     "Otherwise, if you are looking to update an exisiting compound format not currently used by any database, then",
                     "you may press the yellow button to transition yourself to the update page.<br><br>")),
          actionButton("link_to_add_compound_formats", "Add Compound Formats?", class = "btn-success btn-lg"),
          actionButton("link_to_update_compound_formats", "Update Compound Formats?", class = "btn-warning btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Add to or Update Categorical Fields</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you are wanting to add new categorical values to a categorical field that exists in a database already,",
                     "then you may press the green button to transition to the corresponding page.<br><br>",
                     "Otherwise, you may press the yellow button to update or delete existing categorical values of a field which",
                     "will transition to the correct page to do so.<br><br>")),
          actionButton("link_to_add_to_categorical_fields", "Add Categorical Fields?", class = "btn-success btn-lg"),
          actionButton("link_to_update_categorical_fields", "Update or Delete Categorical Fields?", class = "btn-warning btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Add to or Update Record Priority Fields</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you are wanting to add or modify priority values of a dataset then you may use the buttons below to",
                     "choose a database and select a record priority field. <br><br>",
                     "The record priority is used to break ties during the linkage process by choosing the option with the",
                     "higher priority where <b>[1]</b> is the highest, and the higher the number gets, the less of a priority",
                     "it has when it comes down to tie breaks.<br><br>")),
          actionButton("link_to_add_to_record_priority_fields", "Add Record Priority Fields?", class = "btn-success btn-lg"),
          actionButton("link_to_update_record_priority_fields", "Update or Delete Record Priority Fields?", class = "btn-warning btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Add Categorical Values</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If there is a categorical value that you would like to add such that source categorical values have the option",
                     "of standardizing to that value, you may do so by pressing the green button below to transition to that page.<br><br>")),
          actionButton("link_to_add_categorical_values", "Add Categorical Values?", class = "btn-success btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Add or Update Numeric Formats</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you would like to create a new numeric format to convert date-time formats, then you may press the green button below",
                     "to transition to the correct page.<br><br>",
                     "Otherwise, you may update an existing numeric format not currently being used by a database by pressing the yellow",
                     "button below.<br><br>")),
          actionButton("link_to_add_numeric_formats", "Add Numeric Formats?", class = "btn-success btn-lg"),
          actionButton("link_to_update_numeric_formats", "Update Numeric Formats?", class = "btn-warning btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Create a New Destination Field</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you would like to create a new destination field that can be used when creating new standardizing modules or when",
                     "you are creating a new compound format, press the button below.<br><br>")),
          actionButton("link_to_create_new_destination_fields", "Create New Destination Field?", class = "btn-success btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
      fluidRow(
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #9c9c9c; font-size: 28px",
          HTML("<b>Create a New Standardizing Module</b>")), align = "center"
        ),
        column(12, div(
          style = "border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb; font-size: 16px",
          HTML(paste("If you would like to create your own standardizing module for processing source fields, you may press the button below",
                     "and create a new standardizing module that may be used for current and future source fields.<br><br>")),
          actionButton("link_to_create_new_standardizing_module", "Create New Standardizing Module?", class = "btn-success btn-lg")),
          align = "center"
        ),
      ),
      HTML("<br><br>"),
    ),
    #----

    # "View Datasets" conditional panel
    #----
    conditionalPanel(
      condition = "input.modification_form == 'View Standardization Rules'",

      # Placeholder for dynamic UI generation
      fluidRow(
        column(width = 3, div(style = "display: flex; align-items: center;",
                              uiOutput("view_dataset_table_ui"),
                              bsButton("view_rules_help_table", label = "", icon = icon("question"), style = "info"),
                              bsPopover(id = "view_rules_help_table", title = "Select a Dataset - Help",
                                        content = paste("From the dropdown, select a table to view the records of.",
                                                        "NOTE: Some tables require a selection from the other column to limit searches.", sep = "<br><br>"),
                                        placement = "right", trigger = "hover",
                                        options = list(container = "body"))
        )),
        column(width = 3, div(style = "display: flex; align-items: center;",
                              uiOutput("view_limited_dataset"),
                              bsButton("view_rules_help_limit", label = "", icon = icon("question"), style = "info"),
                              bsPopover(id = "view_rules_help_limit", title = "Limit by a Dataset - Help",
                                        content = paste("From the dropdown, select the dataset name that youre interested in seeing the records of."),
                                        placement = "right", trigger = "hover",
                                        options = list(container = "body"))
        ))
      ),

      # View the selected table
      conditionalPanel(
        condition = "input.view_dataset_table != 'null'",
        hr(),
        dataTableOutput("view_selected_table"),
        hr()
      )

    ),
    #----

    # "Add Database" conditional panel
    #----
    conditionalPanel(
      condition = "input.modification_form == 'Add and Update Databases'",

      hr(),
      dataTableOutput("view_current_created_datasets"),
      conditionalPanel(
        condition = "input.link_to_manage_datasets == 1",

        fluidRow(
          column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                         actionButton("go_to_manage_database", "Manage Newly Created Database",
                                  class = "btn-success"), align = 'center',
                         bsButton("go_to_manage_database_help", label = "", icon = icon("question"), style = "info"),
                         bsPopover(id = "go_to_manage_database_help", title = "Manage New Database - Help",
                                   content = paste("Now that you created a new database, using this button will directly link you to the Manage Databases page and place",
                                                   "you on the page to add new source fields. Otherwise, once you are done with adding or updating datasets, you may",
                                                   "use the drop down at the top of the page to transition between pages."),
                                   placement = "right", trigger = "hover",
                                   options = list(container = "body"))
          ))
        )
      ),
      conditionalPanel(
        condition = "input.link_to_manage_datasets_update == 1",

        fluidRow(
          column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                         actionButton("go_to_manage_database_update", "Manage Updated Database",
                                      class = "btn-success"), align = 'center',
                         bsButton("go_to_manage_database_update_help", label = "", icon = icon("question"), style = "info"),
                         bsPopover(id = "go_to_manage_database_update_help", title = "Manage Updated Database - Help",
                                   content = paste("Now that you updated a database, using this button will directly link you to the Manage Databases page and place",
                                                   "you on the page to update source fields. Otherwise, once you are done with adding or updating datasets, you may",
                                                   "use the drop down at the top of the page to transition between pages."),
                                   placement = "right", trigger = "hover",
                                   options = list(container = "body"))
          ))
        )
      ),
      hr(),

      # If the user does not select a row, then they can create a database
      #----
      conditionalPanel(
        condition = "input.view_current_created_datasets_rows_selected <= 0",

        fluidRow(
          column(width = 3, div(style = "display: flex; align-items: center;",
                                textAreaInput("new_record_dataset_code", label = "Dataset Code:", value = "",
                                              width = validateCssUnit(500), resize = "none"),
                                bsButton("new_record_dataset_code_help", label = "", icon = icon("question"), style = "info"),
                                bsPopover(id = "new_record_dataset_code_help", title = "Dataset Code - Help",
                                          content = paste("Dataset Code should be an abbreviation of the full dataset name.",
                                                          "NOTE: Dataset has a max character count of [20]", sep = "<br><br>"),
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body"))
          )),
          #column(width = 1, bsButton("new_record_dataset_code_help", label = "", icon = icon("question"), style = "info")),
          column(width = 3, div(style = "display: flex; align-items: center;",
                                textAreaInput("new_record_dataset_name", label = "Dataset Name:", value = "",
                                              width = validateCssUnit(500), resize = "none"),
                                bsButton("new_record_dataset_name_help", label = "", icon = icon("question"), style = "info"),
                                bsPopover(id = "new_record_dataset_name_help", title = "Dataset Name - Help",
                                          content = paste("Dataset name should include the full name of the dataset in its non-abbreviated form."),
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body"))
          )),
          column(width = 3, div(style = "display: flex; align-items: center;",
                                selectInput("new_record_has_header", label = "Does File Have a Header Row?",
                                            choices = c(" " = 'null',
                                                        "Yes" = 1,
                                                        "No" = 0),
                                            selected = "null", width = validateCssUnit(500)),
                                bsButton("new_record_dataset_has_header_help", label = "", icon = icon("question"), style = "info"),
                                bsPopover(id = "new_record_dataset_has_header_help", title = "Has Header Name - Help",
                                          content = paste("Does the dataset have a row at the top of the file containing the column names of the dataset being added?"),
                                          placement = "left", trigger = "hover",
                                          options = list(container = "body"))
          )),
          column(width = 3, div(style = "display: flex; align-items: center;",
                                selectInput("new_record_file_extension", label = "File Format:",
                                            choices = c(" " = 'null',
                                                        "SAS (.sas7bdat)" = 1,
                                                        "Fixed Width" = 2,
                                                        "Text (.txt)" = 3,
                                                        "Comma Separated Values (.csv)" = 4),
                                            selected = "null", width = validateCssUnit(500)),
                                bsButton("new_record_dataset_file_extension_help", label = "", icon = icon("question"), style = "info"),
                                bsPopover(id = "new_record_dataset_file_extension_help", title = "File Format - Help",
                                          content = paste("What is the file format of the raw source file that is being cleaned?",
                                                          "NOTE: Fixed width files can also have the .txt format, the difference is that a fixed width file is not delimited.",
                                                          sep = "<br><br>"),
                                          placement = "left", trigger = "hover",
                                          options = list(container = "body"))
          ))
        ),

        conditionalPanel(
          condition = "input.new_record_dataset_code != '' &&
                     input.new_record_dataset_name != '' &&
                     input.new_record_has_header  != 'null' &&
                     input.new_record_file_extension != 'null'",

          fluidRow(
            style = "margin-top: 20px;",
            column(12, actionButton("submit_new_dataset_record", "Create Database", class = "btn-primary btn-lg"), align = "center")
          )
        )
      ),
      #----

      # If user selects a data set, they may update it
      #----
      conditionalPanel(
        condition = "input.view_current_created_datasets_rows_selected > 0",

        fluidRow(
          column(width = 3, div(style = "display: flex; align-items: center;",
                                textAreaInput("update_dataset_code", label = "Update Dataset Code:", value = "",
                                              width = validateCssUnit(500), resize = "none"),
                                bsButton("update_dataset_code_help", label = "", icon = icon("question"), style = "info"),
                                bsPopover(id = "update_dataset_code_help", title = "Dataset Code - Help",
                                          content = paste("Dataset code should be an abbreviation of the source dataset",
                                                          "name. 20 CHARACTERS MAX."),
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body"))
          )),
          #column(width = 1, bsButton("new_record_dataset_code_help", label = "", icon = icon("question"), style = "info")),
          column(width = 3, div(style = "display: flex; align-items: center;",
                                textAreaInput("update_dataset_name", label = "Update Dataset Name:", value = "",
                                              width = validateCssUnit(500), resize = "none"),
                                bsButton("update_dataset_name_help", label = "", icon = icon("question"), style = "info"),
                                bsPopover(id = "update_dataset_name_help", title = "Dataset Name - Help",
                                          content = paste("Dataset name should include the full name of the dataset in",
                                                          "its non-abbreviated form."),
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body"))
          )),
          column(width = 3, div(style = "display: flex; align-items: center;",
                                selectInput("update_has_header", label = "Does File Have a Header Row?",
                                            choices = c(" " = 'null',
                                                        "Yes" = 1,
                                                        "No" = 0),
                                            selected = "null", width = validateCssUnit(500)),
                                bsButton("update_dataset_has_header_help", label = "", icon = icon("question"), style = "info"),
                                bsPopover(id = "update_dataset_has_header_help", title = "Has Header Name - Help",
                                          content = paste("Does the dataset have a row at the top of the file containing",
                                                          "the column names of the dataset being added?"),
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body"))
          )),
          column(width = 3, div(style = "display: flex; align-items: center;",
                                selectInput("update_file_extension", label = "Updated File Format:",
                                            choices = c(" " = 'null',
                                                        "SAS (.sas7bdat)" = 1,
                                                        "Fixed Width" = 2,
                                                        "Text (.txt)" = 3,
                                                        "Comma Separated Values (.csv)" = 4),
                                            selected = "null", width = validateCssUnit(500)),
                                bsButton("update_dataset_file_extension_help", label = "", icon = icon("question"), style = "info"),
                                bsPopover(id = "update_dataset_file_extension_help", title = "File Format - Help",
                                          content = paste("What is the file format of the raw source file that is being cleaned?", cat("\n\n\n"),
                                                          "NOTE: Fixed width files can also have the .txt format, the difference",
                                                          "is that a fixed width file is not delimited."),
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body"))
          ))
        ),
        conditionalPanel(
          condition = "input.update_dataset_code != '' &&
                     input.update_dataset_name != '' &&
                     input.update_has_header  != 'null' &&
                     input.update_file_extension != 'null'",

          fluidRow(
            column(12, actionButton("update_dataset_record", "Update Database", class = "btn-primary btn-lg"), align = "center")
          )
        )
      ),
      #----
      HTML("<br><br><br><br>")
    ),

    #----

    # "Add Standardization Rules" conditional panel
    #----
    conditionalPanel(
      condition = "input.modification_form == 'Manage Database Standardization Rules'",

      fluidRow(
        column(width = 3, div(style = "display: flex; align-items: center; display: none;",
                              selectInput("update_type", label = "Add or Update Standardization Rules?",
                                      choices = c(" " = "null", "Add Rules" = 1,
                                                  "Update Rules" = 2),
                                      selected = "null")
        )),
        conditionalPanel(
          condition = "input.update_type == 1",

          column(width = 3, div(style = "display: flex; align-items: center; display: none;",
                                selectInput("table_to_add_to", label = "What Standardizing Rules are you Adding?",
                                        choices = c(" " = "null", "Source Fields" = "source_fields",
                                                    "Compound Formats" = "compound_field_formats",
                                                    "Categorical Fields" = "categorical_fields",
                                                    "Categorical Values" = "categorical_values",
                                                    "Numeric Date Formats" = "numeric_date_formats",
                                                    "Record Priority Fields" = "record_priority"),
                                        selected = "null")
          ))
        ),
        conditionalPanel(
          condition = "input.update_type == 2",

          column(width = 3, div(style = "display: flex; align-items: center; display: none;",
                                selectInput("table_to_update", label = "What Standardizing Rules are you Updating?",
                                        choices = c(" " = "null", "Source Fields" = "source_fields_update",
                                                    "Compound Formats" = "compound_field_formats_update",
                                                    "Categorical Fields" = "categorical_fields_update",
                                                    "Numeric Formats" = "numeric_formats_update",
                                                    "Record Priority Fields" = "record_priority_update"),
                                        selected = "null")
          ))
        ),
      ),

      # Conditional panel for "Creating New Records"
      #----
      conditionalPanel(
        condition = "input.update_type == 1",

        # Conditional panel for adding new records to "source_fields"
        #----
        conditionalPanel(
          condition = "input.table_to_add_to == 'source_fields'",

          hr(),
          fluidRow(

            column(width = 6, div(dataTableOutput("dataset_to_update"), style = "font-size: 0.7vw; width: 48vw")),
            conditionalPanel(
              condition = "input.dataset_to_update_rows_selected > 0",
              column(width = 6, div(dataTableOutput("dataset_to_update_source_fields"), style = "font-size: 0.7vw; width: 48vw"))
            )
          ),
          hr(),

          # Input fields for the new record
          conditionalPanel(
            condition = "input.dataset_to_update_rows_selected > 0",
            fluidRow(
              column(width = 3, div(style = "display: flex; align-items: center;",
                                    textInput("new_source_field_name", label = "Source Field Name:", value = "", width = validateCssUnit("20vw")),
                                    bsButton("new_source_field_name_help", label = "", icon = icon("question"), style = "info"),
                                    bsPopover(id = "new_source_field_name_help", title = "Source Field Name - Help",
                                              content = paste("What is the raw source field name/column name in the source dataset?"),
                                              placement = "right", trigger = "hover",
                                              options = list(container = "body"))
              )),
              column(width = 3, div(style = "display: flex; align-items: center;",
                                    uiOutput("new_field_order_max_add"),
                                    bsButton("new_field_order_help", label = "", icon = icon("question"), style = "info"),
                                    bsPopover(id = "new_field_order_help", title = "Field Order - Help",
                                              content = paste("What is the column number/order of the source field being entered?"),
                                              placement = "right", trigger = "hover",
                                              options = list(container = "body"))
              )),
              column(width = 3, div(style = "display: flex; align-items: center;",
                                    numericInput("new_fixed_width_length", label = "Fixed Width Length:", value = "", min = 0, width = validateCssUnit("20vw")),
                                    bsButton("new_fixed_width_help", label = "", icon = icon("question"), style = "info"),
                                    bsPopover(id = "new_fixed_width_help", title = "Fixed Width - Help",
                                              content = paste("If the dataset the source field being created for is a Fixed-Width file type, then the source field being added should include",
                                                              "a value for the width of the variable so that the pre-processing script knows when to stop reading."),
                                              placement = "left", trigger = "hover",
                                              options = list(container = "body"))
              )),
              column(width = 3, div(style = "display: flex; align-items: center;",
                                    uiOutput("select_standardizing_module"),
                                    bsButton("standardizing_module_help", label = "", icon = icon("question"), style = "info"),
                                    bsPopover(id = "standardizing_module_help", title = "Standardizing Module - Help",
                                              content = paste("The standardizing module of a source field is used to determine what specific function to call when pre-processing",
                                                              "the dataset for a singular source field. Select a standardizing module to learn more about what inputs it expects."),
                                              placement = "left", trigger = "hover",
                                              options = list(container = "body"))
              ))
            ),

            fluidRow(
              column(12, htmlOutput("standardizing_module_description"), align = "center")
            ),

            # Conditional panel for if the user selects a compound field
            conditionalPanel(
              condition = "input.standardizing_type_create_source_field == 2",

              hr(),
              fluidRow(
                column(12, div(dataTableOutput("compound_format_types"), style = "font-size: 100%; width: 100%; align: center"))
              ),
              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               actionButton("add_compound_format_for_new_sf", "Create New Compound Format",
                                        class = "btn-success"), align = 'center',
                               bsButton("add_compound_format", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "add_compound_format", title = "New Compound Format - Help",
                                         content = paste("If there isnt a compound format that fits the format of your source dataset, using this button will link you to creating a new compound format"),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              ),
              hr()
            ),
            # Conditional panel for if the user selected a categorical field
            conditionalPanel(
              condition = "input.standardizing_type_create_source_field == 3",

              hr(),

              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               numericInput("number_of_categorical_fields", label = "Number of Categorical Fields in the Dataset:", value = 1, min = 1), align = "center",
                               bsButton("num_categorical_fields", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "num_categorical_fields", title = "Number of Categorical Fields - Help",
                                         content = paste("How many categorical values does this source field have?<br><br>",
                                                         "Dependent on how many values you entered, you must fill in that many input fields by entering",
                                                         "the datasets source values, and what standardizing value it should map to."),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              ),
              fluidRow(
                column(12, uiOutput("categorical_inputs"), align = "center")
              ),

              hr()

            ),
            # Conditional panel for if the user selected a record priority field
            conditionalPanel(
              condition = "input.standardizing_type_create_source_field == 5",

              hr(),

              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               numericInput("number_of_record_priority_fields", label = "Number of Record Priority Fields in the Dataset:", value = 1, min = 1), align = "center",
                               bsButton("num_record_priority_fields", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "num_record_priority_fields", title = "Number of Record Priority Fields - Help",
                                         content = paste("How many record priority values does this source field have?<br><br>",
                                                         "Dependent on how many values you entered, you must fill in that many input fields by entering",
                                                         "the datasets source values, and what record priority it should have with <b>[1]</b> being",
                                                         "highest, <b>[N - number of values]</b> being the least"),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              ),
              fluidRow(
                column(12, uiOutput("record_priority_inputs"), align = "center")
              ),

              hr()

            ),
            conditionalPanel(
              condition = "input.standardizing_type_create_source_field == 4",

              hr(),
              fluidRow(
                column(12, div(dataTableOutput("numeric_format_types"), style = "font-size: 100%; width: 100%; align: center"))
              ),
              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               actionButton("add_numeric_format_for_new_sf", "Create New Numeric Format",
                                            class = "btn-success"), align = 'center',
                               bsButton("add_numeric_format", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "add_numeric_format", title = "New Numeric Format - Help",
                                         content = paste("If there isnt a numeric format that fits the format of your source dataset, using this button will link you to creating a new numeric format"),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              ),
              hr(),
              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               selectInput("numeric_destination_type_new_sf", label = "What is the Numeric Date Type?",
                                       choices = c("Birthdate" = 1,
                                                   "Acquisition Date" = 2),
                                       selected = NULL), align = "center",
                               bsButton("numeric_destination_type", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "numeric_destination_type", title = "New Numeric Format - Help",
                                         content = paste("Is the date-time format tracking a birthdate or acquisition date?"),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              )
            ),
            conditionalPanel(
              condition = paste("(input.standardizing_type_create_source_field == 1",
                                "&& input.dataset_to_update_rows_selected > 0",
                                "&& input.new_source_field_name != ''",
                                "&& input.new_field_order >= 1)",
                                "||",
                                "(input.standardizing_type_create_source_field == 2",
                                "&& input.dataset_to_update_rows_selected > 0",
                                "&& input.new_source_field_name != ''",
                                "&& input.new_field_order >= 1",
                                "&& input.compound_format_types_rows_selected > 1)",
                                "||",
                                "(input.standardizing_type_create_source_field == 2",
                                "&& input.dataset_to_update_rows_selected > 0",
                                "&& input.new_source_field_name != ''",
                                "&& input.new_field_order >= 1",
                                "&& input.compound_format_types_rows_selected == 1",
                                "&& input.new_compound_format != ''",
                                "&& input.new_compound_format_description != '')",
                                "||",
                                "(input.standardizing_type_create_source_field == 3",
                                "&& input.dataset_to_update_rows_selected > 0",
                                "&& input.new_source_field_name != ''",
                                "&& input.new_field_order >= 1)",
                                "||",
                                "(input.standardizing_type_create_source_field == ''",
                                "&& input.dataset_to_update_rows_selected > 0",
                                "&& input.new_source_field_name != ''",
                                "&& input.new_field_order >= 1)",
                                "||",
                                "(input.numeric_format_types_rows_selected > 0",
                                "&& input.new_source_field_name != ''",
                                "&& input.new_field_order >= 1",
                                "&& input.standardizing_type_create_source_field == 4)",
                                "||",
                                "(input.standardizing_type_create_source_field == 5",
                                "&& input.dataset_to_update_rows_selected > 0",
                                "&& input.new_source_field_name != ''",
                                "&& input.new_field_order >= 1)"),

              fluidRow(
                HTML("<br>"),
                column(12, actionButton("submit_new_dataset_field", "Enter Source Field", class = "btn-primary btn-lg"), align = 'center'),
                HTML("<br><br><br>")
              )
            ),
            HTML("<br><br><br><br><br><br><br><br>")
            #actionButton("submit_new_dataset_field", "Submit")
          )

        ),
        #----

        # Conditional panel for adding new records to "compound_field_formats"
        #----
        conditionalPanel(
          condition = "input.table_to_add_to == 'compound_field_formats'",

          fluidRow(
            column(width = 6, div(dataTableOutput("add_compound_field_formats"), style = "font-size: 0.7vw; width: 48vw",
                                  fluidRow(
                                    column(6, actionButton("see_separator_example", "Separator Example", class = "btn-success btn-lg"), align = "right"),
                                    column(6, actionButton("see_index_example", "Index Example", class = "btn-success btn-lg"), align = "left")
                                  )
            )),
            column(width = 6, fluidPage(

              fluidRow(
                column(width = 3, div(style = "display: flex; align-items: center;",
                                      textAreaInput("new_compound_format_to_add", label = "Compound Format:", value = "", height = validateCssUnit(100)),
                                      bsButton("compound_format_help", label = "", icon = icon("question"), style = "info"),
                                      bsPopover(id = "compound_format_help", title = "Compound Format - Help",
                                                content = paste("The compound format is how the compounded format would look in your dataset. When adding",
                                                                "a new format, please use these shorthands for replacing things like names and dates: <br><br>",
                                                                "<b>yyyy/YYYY:</b> Used to represent years for birthdates or acquisition dates. <br>",
                                                                "<b>mm/mmm/MM/MMM:</b> Used to represent months for birthdates or acquisition dates (Can be in integer or named month form). <br>",
                                                                "<b>dd/DD:</b> Used to represent days for birthdates or acquisition dates. <br><br>",
                                                                "<b>FN/PGN:</b> Used to represent first names/primary given names of a person. <br>",
                                                                "<b>MN/SGN:</b> Used to represent middle names/secondary given names of a person. <br>",
                                                                "<b>LN/PSN:</b> Used to represent last names/primary surnames of a person. <br>",
                                                                "<b>ALN/SSN:</b> Used to represent alternative last names/secondary surnames of a person. <br>",
                                                                "<b>PS:</b> Used to represent prior surnames of a person. <br>"),
                                                placement = "right", trigger = "hover",
                                                options = list(container = "body"))
                )),
                column(width = 3,
                       div(
                         selectInput("separator_or_indexes", label = "Choose Split Method:", choices = c("Separators", "Indexes"), selected = "Separators"),
                         div(style = "display: flex; justify-content: center; align-items: center;",
                             bsButton("separator_type_and_count", label = "", icon = icon("question"), style = "info", align = "center"),
                             bsPopover(id = "separator_type_and_count", title = "Split Type - Help",
                                       content = paste("The split method is used to indicate whether splitting a compound field will be done using",
                                                       "character separators, or substrings using indexes equal to the length of each component. The",
                                                       "descriptions are as follows:<br><br>",
                                                       "<b>Separators:</b> Splitting using separators will ask you to enter how many character separators are found in",
                                                       "the compound field. Afterwards, you will be given a list of input fields containing destination fields, for where each",
                                                       "split will go, and a textbox for you to enter what the separator character is.",
                                                       "See the <b>Separator Example</b> under the formats for extra clarification.<br><br>",
                                                       "<b>Indexes:</b> Splitting using indexes will ask you how many indexes will be part of the split.",
                                                       "Afterwards, you will be given a list input fields containing destination fields paired with an input for the length",
                                                       "of the index. See the <b>Index Example</b> under the formats for extra clarification."),
                                       placement = "top", trigger = "hover",
                                       options = list(container = "body"))
                             ),
                         conditionalPanel(
                           condition = "input.separator_or_indexes == 'Separators'",
                           numericInput("number_of_separators_to_add", label = "Number of Separators:", value = 1)
                         ),
                         conditionalPanel(
                           condition = "input.separator_or_indexes == 'Indexes'",
                           numericInput("number_of_indexes_to_add", label = "Number of Indexes:", value = 2)
                         ),

                )),
                column(width = 6, div(style = "display: flex; align-items: center;",
                                      textAreaInput("new_compound_format_description_to_add",
                                                label = "Compound Format Description:",
                                                value = "",
                                                height = validateCssUnit(100),
                                                width = validateCssUnit(400)),
                                      bsButton("compound_format_description", label = "", icon = icon("question"), style = "info", align = "center"),
                                      bsPopover(id = "compound_format_description", title = "Compound Format Description - Help",
                                                content = paste("The compound format description is useful for future users and yourself in determing",
                                                                "how exactly the date is processed and using what form. When writing your own description",
                                                                "please be sure to include the following information:<br><br>",
                                                                "<b>(1)</b> What type of information the compound field contains.<br>",
                                                                "<b>(2)</b> What separators or indexes are used in the splitting process."),
                                                placement = "left", trigger = "hover",
                                                options = list(container = "body"))
                ))
              ),


              uiOutput("separator_inputs_to_add"),

              conditionalPanel(
                condition = paste("input.new_compound_format_to_add != ''",
                                  "&& input.new_compound_format_description_to_add != ''"),

                fluidRow(
                  HTML("<br>"),
                  column(12, actionButton("submit_new_compound_format", "Enter Compound Format", class = "btn-primary btn-lg"), align = "center"),
                  HTML("<br><br><br>")
                )
              ),
            ))
          ),
        ),
        #----

        # Conditional panel for adding new records to "categorical_fields"
        #----
        conditionalPanel(
          condition = "input.table_to_add_to == 'categorical_fields'",

          fluidRow(
            hr(),
            column(width = 6, div(dataTableOutput("categorical_fields_datasets"), style = "font-size: 0.7vw; width: 48vw")),
            conditionalPanel(
              condition = "input.categorical_fields_datasets_rows_selected > 0",
              column(width = 6, div(dataTableOutput("categorical_source_fields_to_add"), style = "font-size: 0.7vw; width: 48vw"))
            ),
          ),
          hr(),
          fluidRow(
            conditionalPanel(
              condition = "input.categorical_source_fields_to_add_rows_selected > 0",
              column(width = 6, div(dataTableOutput("categorical_source_fields_to_add_curr"), style = "font-size: 0.7vw; width: 48vw")),
              column(width = 6, fluidPage(

                fluidRow(
                  column(10, div(style = "display: flex; justify-content: center; align-items: center;",
                                 numericInput("number_of_categorical_fields_to_add", label = "Number of Categorical Fields to Add:", value = 1, min = 1), align = "center",
                                 bsButton("num_categorical_fields_2", label = "", icon = icon("question"), style = "info"),
                                 bsPopover(id = "num_categorical_fields_2", title = "Number of Categorical Fields - Help",
                                           content = paste("How many categorical values does this source field have?<br><br>",
                                                           "Dependent on how many values you entered, you must fill in that many input fields by entering",
                                                           "the datasets source values, and what standardizing value it should map to."),
                                           placement = "right", trigger = "hover",
                                           options = list(container = "body"))
                  ))
                ),
                fluidRow(
                  column(width = 12, uiOutput("categorical_fields_to_add"))
                ),
              ))
            ),
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.categorical_source_fields_to_add_rows_selected > 0",
              HTML("<br><br><br>"),
              column(12, actionButton("add_new_categorical_fields", "Enter Categorical Fields", class = "btn-primary btn-lg"), align = "center"),
              HTML("<br><br><br>")
            )
          )
        ),
        #----

        # Conditional panel for adding new records to "categorical_values"
        #----
        conditionalPanel(
          condition = "input.table_to_add_to == 'categorical_values'",

          hr(),
          dataTableOutput("curr_categorical_values_add"),
          hr(),

          fluidRow(
            column(12, textInput("new_categorical_value", label = "New Categorical Value:", value = ""), align = "center")
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.new_categorical_value != ''",
              column(12, actionButton("add_new_categorical_values", "Enter Categorical Value", class = "btn-primary btn-lg"), align = "center")
            )
          )
        ),
        #----

        # Conditional panel for adding new records to "numeric_date_formats"
        #----
        conditionalPanel(
          condition = "input.table_to_add_to == 'numeric_date_formats'",

          hr(),
          fluidRow(
            column(width = 12, div(dataTableOutput("add_numeric_date_formats"), style = "font-size: 100%; width: 100%")),
          ),
          hr(),
          fluidRow(
            column(4, div(style = "display: flex; align-items: center;",
                          textAreaInput("new_numeric_format_label", label = "Numeric Date Format Label:", width = validateCssUnit("30vw"), resize = "none"),
                          bsButton("numeric_format_label_help", label = "", icon = icon("question"), style = "info"),
                          bsPopover(id = "numeric_format_label_help", title = "Numeric Date Format Label - Help",
                                    content = paste("The numeric format label is used to represent the format type the date-time numeric value is coming from."),
                                    placement = "top", trigger = "hover",
                                    options = list(container = "body"))
            )),
            column(4, div(style = "display: flex; align-items: center;",
                          dateInput("new_origin_date", label = "Origin Date:", value = NULL, width = validateCssUnit("30vw")),
                          bsButton("origin_date_help", label = "", icon = icon("question"), style = "info"),
                          bsPopover(id = "origin_date_help", title = "Origin Date - Help",
                                    content = paste("Numeric date-time values are normally recorded as a certain numbers integer away (seconds, hours, days, etc.) from an origin date,",
                                                    "this field asks you to pick a date using the provided calendar, or by typing in a date in the format <b>YYYY-MM-DD</b>."),
                                    placement = "top", trigger = "hover",
                                    options = list(container = "body"))
            )),
            column(4, div(style = "display: flex; align-items: center;",
                          selectInput("new_unit_label", label = "Unit of Measurement:",
                                  choices = c("Days" = "Days",
                                              "Hours" = "Hours",
                                              "Seconds" = "Seconds"),
                                  selected = NULL,
                                  width = validateCssUnit("30vw")),
                          bsButton("unit_label_help", label = "", icon = icon("question"), style = "info"),
                          bsPopover(id = "unit_label_help", title = "Unit Label - Help",
                                    content = paste("What is the unit of measurement used in the datasets date-time value? The unit label is used as the distance from an origin date."),
                                    placement = "top", trigger = "hover",
                                    options = list(container = "body"))
            ))
          ),
          fluidRow(
            conditionalPanel(
              condition = paste("(input.new_numeric_format_label != '' &&",
                                "input.new_origin_date != '')"),
              HTML("<br>"),
              column(12, actionButton("submit_new_numeric_date_format", "Enter Numeric Date Format", class = "btn-primary btn-lg"), align = "center"),
              HTML("<br><br><br>")
            )
          )
        ),
        #----

        # Conditional panel for adding new records to "record_priority"
        #----
        conditionalPanel(
          condition = "input.table_to_add_to == 'record_priority'",

          hr(),
          fluidRow(
            column(width = 6, div(dataTableOutput("record_priority_datasets"), style = "font-size: 0.7vw; width: 48vw")),
            conditionalPanel(
              condition = "input.record_priority_datasets_rows_selected > 0",
              column(width = 6, div(dataTableOutput("record_priority_fields_to_add"), style = "font-size: 0.7vw; width: 48vw"))
            ),
          ),
          hr(),
          fluidRow(
            conditionalPanel(
              condition = "input.record_priority_fields_to_add_rows_selected > 0",
              column(width = 6, div(dataTableOutput("record_priority_fields_to_add_curr"), style = "font-size: 0.7vw; width: 48vw")),
              column(width = 6, fluidPage(

                fluidRow(
                  column(10, div(style = "display: flex; justify-content: center; align-items: center;",
                                 numericInput("number_of_record_priorities_to_add", label = "Number of Extra Record Priorities:", value = 1, min = 1), align = "center",
                                 bsButton("num_record_priority_fields_3", label = "", icon = icon("question"), style = "info"),
                                 bsPopover(id = "num_record_priority_fields_3", title = "Number of Record Priorities - Help",
                                           content = paste("How many record priority values does this source field have?<br><br>",
                                                           "Dependent on how many values you entered, you must fill in that many input fields by entering",
                                                           "the datasets source values, and what record priority it should have with <b>[1]</b> being",
                                                           "highest, <b>[N - number of values]</b> being the least"),
                                           placement = "right", trigger = "hover",
                                           options = list(container = "body"))
                  ))
                ),
                fluidRow(
                  column(width = 12, uiOutput("record_priorities_to_add"))
                ),
              ))
            ),
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.record_priority_fields_to_add_rows_selected > 0",
              HTML("<br><br><br>"),
              column(12, actionButton("add_new_record_priorities", "Enter Record Priorities", class = "btn-primary btn-lg"), align = "center"),
              HTML("<br><br><br>")
            )
          )
        )
        #----

      ),

      #----
      # Conditional panel for "Updating Existing Records"
      conditionalPanel(
        condition = "input.update_type == 2",

        # Conditional panel for updating existing "source_fields"
        #----
        conditionalPanel(
          condition = "input.table_to_update == 'source_fields_update'",

          fluidRow(
            hr(),
            column(width = 6, div(dataTableOutput("source_field_to_update_datasets"), style = "font-size: 0.7vw; width: 48vw")),
            conditionalPanel(
              condition = "input.source_field_to_update_datasets_rows_selected > 0",
              column(width = 6, div(dataTableOutput("source_field_to_update"), style = "font-size: 0.7vw; width: 48vw")),
            ),
            hr(),
          ),

          conditionalPanel(
            condition = "input.source_field_to_update_rows_selected > 0 && input.source_field_to_update_datasets_rows_selected > 0",

            fluidRow(
              column(width = 3, div(style = "display: flex; align-items: center;",
                                    textInput("updated_source_field_name", label = "Source Field Name:", value = "", width = validateCssUnit("20vw")),
                                    bsButton("new_source_field_name_help_2", label = "", icon = icon("question"), style = "info"),
                                    bsPopover(id = "new_source_field_name_help_2", title = "Source Field Name - Help",
                                              content = paste("What is the raw source field name/column name in the source dataset?"),
                                              placement = "right", trigger = "hover",
                                              options = list(container = "body"))
              )),
              column(width = 3, div(style = "display: flex; align-items: center;",
                                    uiOutput("updated_numeric_field_order"),
                                    bsButton("new_field_order_help_2", label = "", icon = icon("question"), style = "info"),
                                    bsPopover(id = "new_field_order_help_2", title = "Field Order - Help",
                                              content = paste("What is the column number/order of the source field being entered?"),
                                              placement = "right", trigger = "hover",
                                              options = list(container = "body"))
              )),
              column(width = 3, div(style = "display: flex; align-items: center;",
                                    numericInput("updated_fixed_width_length", label = "Fixed Width Length (if required):", value = "", min = 0, width = validateCssUnit("20vw")),
                                    bsButton("new_fixed_width_help_2", label = "", icon = icon("question"), style = "info"),
                                    bsPopover(id = "new_fixed_width_help_2", title = "Fixed Width - Help",
                                              content = paste("If the dataset the source field being created for is a Fixed-Width file type, then the source field being added should include",
                                                              "a value for the width of the variable so that the pre-processing script knows when to stop reading."),
                                              placement = "left", trigger = "hover",
                                              options = list(container = "body"))
              )),
              column(width = 3, div(style = "display: flex; align-items: center;",
                                    uiOutput("select_updated_standardizing_module"),
                                    bsButton("standardizing_module_help_2", label = "", icon = icon("question"), style = "info"),
                                    bsPopover(id = "standardizing_module_help_2", title = "Standardizing Module - Help",
                                              content = paste("The standardizing module of a source field is used to determine what specific function to call when pre-processing",
                                                              "the dataset for a singular source field. Select a standardizing module to learn more about what inputs it expects."),
                                              placement = "left", trigger = "hover",
                                              options = list(container = "body"))
              ))
            ),

            fluidRow(
              column(12, htmlOutput("standardizing_module_description_update"), align = "center")
            ),

            conditionalPanel(
              condition = "input.standardizing_type_update_source_field == 2",

              hr(),
              fluidRow(
                column(12, div(dataTableOutput("updated_compound_format_type"), style = "font-size: 100%; width: 100%; align: center"))
              ),
              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               actionButton("add_compound_format_for_updated_sf", "Create New Compound Format",
                                            class = "btn-success"), align = 'center',
                               bsButton("add_compound_format_2", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "add_compound_format_2", title = "New Compound Format - Help",
                                         content = paste("If there isnt a compound format that fits the format of your source dataset, using this button will link you to creating a new compound format"),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              ),
              hr(),

            ),
            conditionalPanel(
              condition = "input.standardizing_type_update_source_field == 3",

              hr(),

              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               numericInput("updated_number_of_categorical_fields", label = "Number of Categorical Fields in the Dataset:", value = 1, min = 1), align = "center",
                               bsButton("num_categorical_fields_3", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "num_categorical_fields_3", title = "Number of Categorical Fields - Help",
                                         content = paste("How many categorical values does this source field have?<br><br>",
                                                         "Dependent on how many values you entered, you must fill in that many input fields by entering",
                                                         "the datasets source values, and what standardizing value it should map to."),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              ),
              fluidRow(
                column(12, uiOutput("updated_categorical_inputs"), align = "center")
              ),
            ),
            conditionalPanel(
              condition = "input.standardizing_type_update_source_field == 4",

              hr(),
              fluidRow(
                column(12, div(dataTableOutput("numeric_format_types_update"), style = "font-size: 100%; width: 100%; align: center"))
              ),
              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               actionButton("add_numeric_format_for_updated_sf", "Create New Numeric Format",
                                            class = "btn-success"), align = 'center',
                               bsButton("add_numeric_format_2", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "add_numeric_format_2", title = "New Numeric Format - Help",
                                         content = paste("If there isnt a numeric format that fits the format of your source dataset, using this button will link you to creating a new numeric format"),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              ),
              hr(),
              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               selectInput("numeric_destination_type_updated_sf", label = "What is the Numeric Date Type?",
                                           choices = c("Birthdate" = 1,
                                                       "Acquisition Date" = 2),
                                           selected = NULL), align = "center",
                               bsButton("numeric_destination_type_2", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "numeric_destination_type_2", title = "New Numeric Format - Help",
                                         content = paste("Is the date-time format tracking a birthdate or acquisition date?"),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              )
            ),
            conditionalPanel(
              condition = "input.standardizing_type_update_source_field == 5",

              hr(),

              fluidRow(
                column(12, div(style = "display: flex; justify-content: center; align-items: center;",
                               numericInput("updated_number_of_record_priority_fields", label = "Number of Record Priority Fields in the Dataset:", value = 1, min = 1), align = "center",
                               bsButton("num_record_priority_fields_2", label = "", icon = icon("question"), style = "info"),
                               bsPopover(id = "num_record_priority_fields_2", title = "Number of Record Priority Fields - Help",
                                         content = paste("How many record priority values does this source field have?<br><br>",
                                                         "Dependent on how many values you entered, you must fill in that many input fields by entering",
                                                         "the datasets source values, and what record priority it should have with <b>[1]</b> being",
                                                         "highest, <b>[N - number of values]</b> being the least"),
                                         placement = "right", trigger = "hover",
                                         options = list(container = "body"))
                ))
              ),
              fluidRow(
                column(12, uiOutput("updated_record_priority_inputs"), align = "center")
              ),
            ),
            conditionalPanel(
              condition = paste("(input.standardizing_type_update_source_field == 1",
                                "&& input.source_field_to_update_rows_selected > 0",
                                "&& input.updated_source_field_name != ''",
                                "&& input.updated_field_order >= 1)",
                                "||",
                                "(input.standardizing_type_update_source_field == 2",
                                "&& input.source_field_to_update_rows_selected > 0",
                                "&& input.updated_source_field_name != ''",
                                "&& input.updated_field_order >= 1",
                                "&& input.updated_compound_format_type_rows_selected > 1)",
                                "||",
                                "(input.standardizing_type_update_source_field == 2",
                                "&& input.source_field_to_update_rows_selected > 0",
                                "&& input.updated_source_field_name != ''",
                                "&& input.updated_field_order >= 1",
                                "&& input.updated_compound_format_type_rows_selected == 1",
                                "&& input.updated_compound_format != ''",
                                "&& input.updated_compound_format_description != '')",
                                "||",
                                "(input.standardizing_type_update_source_field == 3",
                                "&& input.source_field_to_update_rows_selected > 0",
                                "&& input.updated_source_field_name != ''",
                                "&& input.updated_field_order >= 1)",
                                "||",
                                "(input.standardizing_type_update_source_field == ''",
                                "&& input.source_field_to_update_rows_selected > 0",
                                "&& input.updated_source_field_name != ''",
                                "&& input.updated_field_order >= 1)",
                                "||",
                                "(input.numeric_format_types_update_rows_selected > 0",
                                "&& input.updated_source_field_name != ''",
                                "&& input.updated_field_order >= 1",
                                "&& input.standardizing_type_update_source_field == 4)",
                                "||",
                                "(input.standardizing_type_update_source_field == 5",
                                "&& input.source_field_to_update_rows_selected > 0",
                                "&& input.updated_source_field_name != ''",
                                "&& input.updated_field_order >= 1)"),

              fluidRow(
                HTML("<br>"),
                column(6, actionButton("update_source_field", "Update Source Field", class = "btn-primary btn-lg"), align = "right"),
                column(6, actionButton("delete_source_field", "Delete Source Field", class = "btn-danger btn-lg"), align = "left"),
                HTML("<br><br><br>")
              )
            ),
            HTML("<br><br><br><br><br><br><br><br>")
          ),
        ),
        #----

        # Conditional panel for updating existing "compound_field_formats"
        #----
        conditionalPanel(
          condition = "input.table_to_update == 'compound_field_formats_update'",

          fluidRow(
            hr(),
            column(width = 6, div(dataTableOutput("updatable_compound_formats"), style = "font-size: 0.7vw; width: 48vw",
                                  fluidRow(
                                    column(6, actionButton("see_separator_example_2", "Separator Example", class = "btn-success btn-lg"), align = "right"),
                                    column(6, actionButton("see_index_example_2", "Index Example", class = "btn-success btn-lg"), align = "left")
                                  )

            )),
            column(width = 6, fluidPage(
              conditionalPanel(
                condition = "input.updatable_compound_formats_rows_selected > 0",

                fluidRow(
                  column(width = 3, div(style = "display: flex; align-items: center;",
                                        textAreaInput("new_updatable_compound_format", label = "Compound Format:", value = "", height = validateCssUnit(100)),
                                        bsButton("compound_format_help_2", label = "", icon = icon("question"), style = "info"),
                                        bsPopover(id = "compound_format_help_2", title = "Compound Format - Help",
                                                  content = paste("The compound format is how the compounded format would look in your dataset. When adding",
                                                                  "a new format, please use these shorthands for replacing things like names and dates: <br><br>",
                                                                  "<b>yyyy/YYYY:</b> Used to represent years for birthdates or acquisition dates. <br>",
                                                                  "<b>mm/mmm/MM/MMM:</b> Used to represent months for birthdates or acquisition dates (Can be in integer or named month form). <br>",
                                                                  "<b>dd/DD:</b> Used to represent days for birthdates or acquisition dates. <br><br>",
                                                                  "<b>FN/PGN:</b> Used to represent first names/primary given names of a person. <br>",
                                                                  "<b>MN/SGN:</b> Used to represent middle names/secondary given names of a person. <br>",
                                                                  "<b>LN/PSN:</b> Used to represent last names/primary surnames of a person. <br>",
                                                                  "<b>ALN/SSN:</b> Used to represent alternative last names/secondary surnames of a person. <br>",
                                                                  "<b>PS:</b> Used to represent prior surnames of a person. <br>"),
                                                  placement = "right", trigger = "hover",
                                                  options = list(container = "body"))
                  )),
                  column(width = 3,
                         div(
                           selectInput("separator_or_indexes_update", label = "Choose Split Method:", choices = c("Separators", "Indexes"), selected = "Separators"),
                           div(style = "display: flex; justify-content: center; align-items: center;",
                               bsButton("separator_type_and_count_2", label = "", icon = icon("question"), style = "info", align = "center"),
                               bsPopover(id = "separator_type_and_count_2", title = "Split Type - Help",
                                         content = paste("The split method is used to indicate whether splitting a compound field will be done using",
                                                         "character separators, or substrings using indexes equal to the length of each component. The",
                                                         "descriptions are as follows:<br><br>",
                                                         "<b>Separators:</b> Splitting using separators will ask you to enter how many character separators are found in",
                                                         "the compound field. Afterwards, you will be given a list of input fields containing destination fields, for where each",
                                                         "split will go, and a textbox for you to enter what the separator character is.",
                                                         "See the <b>Separator Example</b> under the formats for extra clarification.<br><br>",
                                                         "<b>Indexes:</b> Splitting using indexes will ask you how many indexes will be part of the split.",
                                                         "Afterwards, you will be given a list input fields containing destination fields paired with an input for the length",
                                                         "of the index. See the <b>Index Example</b> under the formats for extra clarification."),
                                         placement = "top", trigger = "hover",
                                         options = list(container = "body"))
                           ),
                           conditionalPanel(
                             condition = "input.separator_or_indexes_update == 'Separators'",
                             numericInput("new_updatable_number_of_separators", label = "Number of Separators:", value = 1)
                           ),
                           conditionalPanel(
                             condition = "input.separator_or_indexes_update == 'Indexes'",
                             numericInput("new_updatable_number_of_indexes", label = "Number of Indexes:", value = 2)
                           ),

                  )),
                  column(width = 6, div(style = "display: flex; align-items: center;",
                                        textAreaInput("new_updatable_compound_format_description",
                                                      label = "Compound Format Description:",
                                                      value = "",
                                                      height = validateCssUnit(100),
                                                      width = validateCssUnit(400)),
                                        bsButton("compound_format_description_2", label = "", icon = icon("question"), style = "info", align = "center"),
                                        bsPopover(id = "compound_format_description_2", title = "Compound Format Description - Help",
                                                  content = paste("The compound format description is useful for future users and yourself in determing",
                                                                  "how exactly the date is processed and using what form. When writing your own description",
                                                                  "please be sure to include the following information:<br><br>",
                                                                  "<b>(1)</b> What type of information the compound field contains.<br>",
                                                                  "<b>(2)</b> What separators or indexes are used in the splitting process."),
                                                  placement = "left", trigger = "hover",
                                                  options = list(container = "body"))
                  ))
                ),
                uiOutput("updatable_separator_inputs"),

                conditionalPanel(
                  condition = paste("input.new_updatable_compound_format != ''",
                                    "&& input.new_updatable_compound_format_description != ''"),

                  fluidRow(
                    HTML("<br>"),
                    column(12, actionButton("update_exisiting_compound_format", "Update Compound Format", class = "btn-primary btn-lg"), align = "center"),
                    HTML("<br><br><br>")
                  )
                ),
              )
            )),
            hr(),
          ),
        ),
        #----

        # Conditional panel for updating existing "categorical_fields"
        #----
        conditionalPanel(
          condition = "input.table_to_update == 'categorical_fields_update'",
          hr(),
          fluidRow(
            column(width = 6, div(dataTableOutput("dataset_to_update_cfs"), style = "font-size: 0.7vw; width: 48vw")),

            # Conditional panel means user has chosen a dataset to edit the categorical_fields of.
            conditionalPanel(
              condition = "input.dataset_to_update_cfs_rows_selected > 0",
              column(width = 6, div(dataTableOutput("categorical_field_to_update"), style = "font-size: 0.7vw; width: 48vw"))
            ),
          ),
          hr(),
          conditionalPanel(
            condition = "input.categorical_field_to_update_rows_selected > 0 && input.dataset_to_update_cfs_rows_selected > 0",
            fluidRow(
              column(6, textInput("new_source_value_update", label = "Source Categorical Value:"), align = "right"),
              column(6, uiOutput("updated_standardizied_value"), align = "left")
            ),
            fluidRow(
              column(6, actionButton("submit_updated_categorical_field", "Update Categorical Field", class = "btn-primary btn-lg"), align = "right"),
              column(6, actionButton("delete_categorical_field", "Delete Categorical Field", class = "btn-danger btn-lg"), align = "left")
            ),
            HTML("<br><br><br>")
          )
        ),
        #----

        # Conditional panel for updating existing "numeric_formats"
        #----
        conditionalPanel(
          condition = "input.table_to_update == 'numeric_formats_update'",

          fluidRow(
            hr(),
            column(width = 12, div(dataTableOutput("updatable_numeric_formats"), style = "font-size: 100%; width: 100%")),
            hr()
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.updatable_numeric_formats_rows_selected > 0",
              column(4, div(style = "display: flex; align-items: center;",
                            textAreaInput("updated_numeric_format_label", label = "Numeric Date Format Label:", width = validateCssUnit("30vw"), resize = "none"),
                            bsButton("numeric_format_label_help_2", label = "", icon = icon("question"), style = "info"),
                            bsPopover(id = "numeric_format_label_help_2", title = "Numeric Date Format Label - Help",
                                      content = paste("The numeric format label is used to represent the format type the date-time numeric value is coming from."),
                                      placement = "top", trigger = "hover",
                                      options = list(container = "body"))
              )),
              column(4, div(style = "display: flex; align-items: center;",
                            dateInput("updated_origin_date", label = "Origin Date:", value = NULL, width = validateCssUnit("30vw")),
                            bsButton("origin_date_help_2", label = "", icon = icon("question"), style = "info"),
                            bsPopover(id = "origin_date_help_2", title = "Origin Date - Help",
                                      content = paste("Numeric date-time values are normally recorded as a certain numbers integer away (seconds, hours, days, etc.) from an origin date,",
                                                      "this field asks you to pick a date using the provided calendar, or by typing in a date in the format <b>YYYY-MM-DD</b>."),
                                      placement = "top", trigger = "hover",
                                      options = list(container = "body"))
              )),
              column(4, div(style = "display: flex; align-items: center;",
                            selectInput("updated_units_label", label = "Unit of Measurement:",
                                        choices = c("Days" = "Days",
                                                    "Hours" = "Hours",
                                                    "Seconds" = "Seconds"),
                                        selected = NULL,
                                        width = validateCssUnit("30vw")),
                            bsButton("unit_label_help_2", label = "", icon = icon("question"), style = "info"),
                            bsPopover(id = "unit_label_help_2", title = "Unit Label - Help",
                                      content = paste("What is the unit of measurement used in the datasets date-time value? The unit label is used as the distance from an origin date."),
                                      placement = "top", trigger = "hover",
                                      options = list(container = "body"))
              ))
            ),
            fluidRow(
              HTML("<br>"),
              conditionalPanel(
                condition = paste("input.updated_numeric_format_label != ''",
                                "&& input.new_origin_date != ''",
                                "&& input.updatable_numeric_formats_rows_selected > 0"),
                column(12, actionButton("update_exisiting_numeric_format", "Update Numeric Format", class = "btn-primary btn-lg"), align = "center"),
              ),
              HTML("<br><br><br>")
            )
          )
        ),
        #----

        # Conditional panel for updating existing "record_priorities"
        #----
        conditionalPanel(
          condition = "input.table_to_update == 'record_priority_update'",

          hr(),
          fluidRow(
            column(width = 6, div(dataTableOutput("dataset_to_update_record_priority"), style = "font-size: 0.7vw; width: 48vw")),

            # Conditional panel means user has chosen a dataset to edit the categorical_fields of.
            conditionalPanel(
              condition = "input.dataset_to_update_record_priority_rows_selected > 0",
              column(width = 6, div(dataTableOutput("record_priorities_to_update"), style = "font-size: 0.7vw; width: 48vw"))
            ),
          ),
          hr(),
          conditionalPanel(
            condition = "input.record_priorities_to_update_rows_selected > 0",
            fluidRow(
              column(6, textInput("new_record_priority_source_value_update", label = "Source Field Value:"), align = "right"),
              column(6, numericInput("updated_record_priority", label = "Record Priority:", value = 1), align = "left")
            ),
            fluidRow(
              column(6, actionButton("submit_updated_record_priority", "Update Record Priority", class = "btn-primary btn-lg"), align = "right"),
              column(6, actionButton("delete_record_priority", "Delete Record Priority", class = "btn-danger btn-lg"), align = "left")
            ),
            HTML("<br><br><br>")
          )
        )
        #----
      ),
    ),
    #----

    # "Enable and Disable Databases" conditional panel
    #----
    conditionalPanel(
      condition = "input.modification_form == 'Enable and Disable Databases'",

      fluidRow(
        column(6, fluidRow(
            column(12, div(dataTableOutput("databases_disable"), style = "font-size: 0.7vw; width: 48vw")),
            conditionalPanel(
              condition = "input.databases_disable_rows_selected > 0",
              column(12, actionButton("disable_selected_database", "Disable Database", class = "btn-primary btn-lg"), align = "center")
            )
          )
        ),
        column(6, fluidRow(
            column(12, div(dataTableOutput("databases_enable"), style = "font-size: 0.7vw; width: 48vw")),
            conditionalPanel(
              condition = "input.databases_enable_rows_selected > 0",
              column(12, actionButton("enable_selected_database", "Enable Database", class = "btn-primary btn-lg"), align = "center")
            )
          )
        ),
      ),
    ),
    #----

    # "Create New Standardizing Module conditional panel"
    #----
    conditionalPanel(
      condition = "input.modification_form == 'Add Standardizing Module'",

      hr(),
      dataTableOutput("curr_standardizing_modules"),
      hr(),

      fluidRow(
        column(width = 2, div(style = "display: flex; align-items: center;",
                              textInput("new_module_name", label = "Standardizing Module Name:", value = "", width = validateCssUnit("20vw")),
                              bsButton("new_module_name_help", label = "", icon = icon("question"), style = "info"),
                              bsPopover(id = "new_module_name_help", title = "New Module Name - Help",
                                        content = paste("What is the standardizing module name that follows the <b>pre_process_</b> module prefix?"),
                                        placement = "right", trigger = "hover",
                                        options = list(container = "body"))
        )),
        column(width = 4, div(style = "display: flex; align-items: center;",
                              textAreaInput("new_module_desc", label = "Standardizing Module Description", value = "", width = validateCssUnit("25vw"), resize = "none"),
                              bsButton("new_module_desc_help", label = "", icon = icon("question"), style = "info"),
                              bsPopover(id = "new_module_desc_help", title = "New Module Description - Help",
                                        content = paste("Provide a short description of what kind of fields would be used by this standardizing module for yourself",
                                                        "and future users."),
                                        placement = "right", trigger = "hover",
                                        options = list(container = "body"))
        )),
        column(width = 4, div(style = "display: flex; align-items: center;",
                              uiOutput("new_module_dest"),
                              bsButton("new_module_dest_help", label = "", icon = icon("question"), style = "info"),
                              bsPopover(id = "new_module_dest_help", title = "New Module Destination - Help",
                                        content = paste("Select a destination field that the column name will become after processing finishes, or create a new field",
                                                        "and then select it here."),
                                        placement = "left", trigger = "hover",
                                        options = list(container = "body"))
        )),
        column(width = 2, div(style = "display: flex; align-items: center;",
                              selectInput("new_module_output_data", label = "Include in Non-Linkage File?",
                                          choices = c(" " = 'null',
                                                      "Yes" = 1,
                                                      "No" = 0),
                                          selected = 0, width = validateCssUnit("20vw")),
                              bsButton("new_module_output_data_help", label = "", icon = icon("question"), style = "info"),
                              bsPopover(id = "new_module_output_data_help", title = "New Module Output Data - Help",
                                        content = paste("Should this field also be included in the file containing the non-linkage fields while also being in the",
                                                        "cleaned data file?"),
                                        placement = "left", trigger = "hover",
                                        options = list(container = "body"))
        ))
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.new_module_name != '' && input.new_module_desc != ''",
          column(12, actionButton("create_module", "Create New Standardizing Module", class = "btn-primary btn-lg"), align = "center")
        )
      ),
      HTML("<br><br><br>")
    ),
    #----

    # "Create New Destination Field conditional panel"
    #----
    conditionalPanel(
      condition = "input.modification_form == 'Add Destination Field'",

      hr(),
      dataTableOutput("curr_destination_fields"),
      hr(),

      fluidRow(
        column(width = 6, div(style = "display: flex; align-items: center;",
                              textInput("new_dest_field_name", label = "Destination Field Name:", value = "", width = validateCssUnit("40vw")),
                              bsButton("new_dest_field_name_help", label = "", icon = icon("question"), style = "info"),
                              bsPopover(id = "new_dest_field_name_help", title = "New Destination Field Name - Help",
                                        content = paste("What is the standardizied column name that the field column will be renamed to following processing?"),
                                        placement = "right", trigger = "hover",
                                        options = list(container = "body"))
        )),
        column(width = 6, div(style = "display: flex; align-items: center;",
                              textAreaInput("new_dest_field_desc", label = "Destination Field Description:", value = "", width = validateCssUnit("40vw"), resize = "none"),
                              bsButton("new_dest_field_desc_help", label = "", icon = icon("question"), style = "info"),
                              bsPopover(id = "new_dest_field_desc_help", title = "New Destination Field Description - Help",
                                        content = paste("Provide a short description of what this field represents following processing"),
                                        placement = "right", trigger = "hover",
                                        options = list(container = "body"))
        ))
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.new_dest_field_name != '' && input.new_dest_field_desc != ''",
          column(12, actionButton("create_dest_field", "Create New Destination Field", class = "btn-primary btn-lg"), align = "center")
        )
      ),
      HTML("<br><br><br>")
    )
    #----

  ),
)

# Script/Server ----
dataset_server <- function(input, output, session, metadata_connection){
  curr_table <- "none"

  # Function to execute when the session ends
  onSessionEnded(function() {
    # Your code here to execute when the window is closed
    # For example, you can print a message to the console
    #remove_messages()
    print("Window closed.")
    dbDisconnect(metadata_connection)
    stopApp()

  }, session)

  # Modification Form
  #----
  # Reactive expression to get the selected variable name
  selected_variable <- reactive({
    if(input$modification_form == "null") {
      return(NULL)
    } else {
      return(input$modification_form)
    }
  })

  # Render the selected variable name
  output$selected_variable <- renderText({
    selected_variable()
  })

  observeEvent(input$exit_program, {
    stopApp()
  })

  reset_view_metadata_widgets <- function(){
    updateSelectInput(session, "view_dataset_table", selected = "null")
    updateSelectInput(session, "view_dataset_selection_sf", selected = "null")
  }

  reset_add_dataset_widgets <- function(){
    updateTextInput(session, "new_record_dataset_code", value = "")
    updateTextInput(session, "new_record_dataset_name", value = "")
    updateSelectInput(session, "new_record_has_header", selected = "null")
    updateSelectInput(session, "new_record_file_extension", selected = "null")

    output$view_selected_table <- renderTable({
      dataset_id <- input$view_dataset_selection_sf
      metadata_table <- input$view_dataset_table

      if(!is.null(metadata_table) && (metadata_table == "datasets" || metadata_table == "standardizing_modules" ||
                                      metadata_table == "destination_fields" || metadata_table == "file_formats" ||
                                      metadata_table == "categorical_values" || metadata_table == "compound_field_formats" ||
                                      metadata_table == "compound_field_destinations" || metadata_table == "compound_field_separators" ||
                                      metadata_table == "numeric_date_formats")){
        df <- dbGetQuery(metadata_connection, paste('SELECT * from ', metadata_table))
      }
      else if (!is.null(metadata_table) && metadata_table == "source_fields"){
        if(!is.null(dataset_id) && dataset_id != "null"){
          df <- dbGetQuery(metadata_connection, paste('SELECT * from ', metadata_table, ' where dataset_id = ', dataset_id))
        }
        else{
          df <- dbGetQuery(metadata_connection, paste('SELECT * from ', metadata_table, " LIMIT 0,50")) #figure out multiple pages later?
        }
      }
      else if (!is.null(metadata_table) && metadata_table == "standardizing_modules"){
        df <- dbGetQuery(metadata_connection, paste('SELECT * from ', metadata_table))
      }
      else if (!is.null(metadata_table) && metadata_table == "categorical_fields"){
        if(!is.null(dataset_id) && dataset_id != "null"){
          df <- dbGetQuery(metadata_connection, paste('SELECT distinct sf.source_field_id, sf.source_field_name, source_value, standardized_value_id
                                                    from source_fields sf',
                                                      'JOIN categorical_fields cf on cf.source_field_id = sf.source_field_id',
                                                      ' where dataset_id = ', dataset_id))
        }
        else{
          df <- dbGetQuery(metadata_connection, paste('SELECT * from ', metadata_table, " LIMIT 0,50")) #figure out multiple pages later?
        }
      }
      else if (!is.null(metadata_table) && metadata_table == "compound_fields"){
        if(!is.null(dataset_id) && dataset_id != "null"){
          df <- dbGetQuery(metadata_connection, paste('SELECT distinct sf.source_field_id, sf.source_field_name, compound_format, format_description
                                                    from source_fields sf',
                                                      'JOIN compound_fields cf on cf.source_field_id = sf.source_field_id',
                                                      'JOIN compound_field_formats cff on cff.compound_field_format_id = cf.compound_field_format_id',
                                                      ' where dataset_id = ', dataset_id))
        }
        else{
          df <- dbGetQuery(metadata_connection, paste('SELECT * from ', metadata_table, " LIMIT 0,50")) #figure out multiple pages later?
        }
      }
      else{
        return(data.frame())
      }
    })
  }

  reset_update_entry_widgets <- function(){
    updateSelectInput(session, "update_type", selected = "null")
    updateSelectInput(session, "table_to_update", selected = "null")
    updateSelectInput(session, "dataset_to_add_records", selected = "null")

    updateTextInput(session, "new_source_field_name", value = "")
    updateNumericInput(session, "new_field_order", value = 1)
    updateNumericInput(session, "new_fixed_width_length", value = "")
    updateSelectInput(session, "selected_standardizing_module", selected = "null")

  }

  reset_ui_renders <- function(){
    # Re-render the "View Page" UI
    output$view_dataset_table_ui <- renderUI({

      # Create select input with dynamic choices
      return(selectInput("view_dataset_table",
                         choices = list(" " = "null",
                                        "Datasets" = "datasets",
                                        "Source Fields" = "source_fields",
                                        "Standardizing Modules" = "standardizing_modules",
                                        "Destination Fields" = "destination_fields",
                                        "Categorical Fields" = "categorical_fields",
                                        "Categorical Values" = "categorical_values",
                                        "Compound Fields" = "compound_fields",
                                        "Compound Field Formats" = "compound_field_formats",
                                        "Compound Field Separators" = "compound_field_separators",
                                        "Compound Field Destinations" = "compound_field_destinations",
                                        "Numeric Date Destinations" = "numeric_date_destinations",
                                        "Numeric Date Fields" = "numeric_date_fields",
                                        "Numeric Date Format" = "numeric_date_formats",
                                        "File Formats" = "file_formats"),
                         selected = "null", label = "Select a Dataset Table:",
                         width = validateCssUnit(300)))
    })
    output$view_limited_dataset <- renderUI({
      # Perform query using metadata_connection
      query_result <- dbGetQuery(metadata_connection, "SELECT dataset_name, dataset_id FROM datasets")

      # Extract columns from query result
      choices <- setNames(query_result$dataset_id, query_result$dataset_name)

      # Add the additional look up value where the first choice is 0
      choices <- c(" " = "null", choices)

      # Create select input with dynamic choices
      span(selectInput("view_dataset_selection_sf", label = "Select a Dataset:", choices = choices), style = "width: 500px;")
    })

    # Re-render the "Add Database" UI

    # Re-render the "Add Standardization Rules" UI
    output$select_standardizing_module <- renderUI({
      # Perform query using metadata_connection
      query_result <- dbGetQuery(metadata_connection, "SELECT standardizing_module_name, standardizing_module_id FROM standardizing_modules")

      # Extract columns from query result
      choices <- setNames(query_result$standardizing_module_id, query_result$standardizing_module_name)

      # Add the additional look up value where the first choice is 0
      choices <- c("Not Applicable" = "null", choices)

      # Create select input with dynamic choices
      span(selectInput("selected_standardizing_module", label = "Select Standardizing Module:", choices = choices), style = "width: 500px;")
    })

    output$select_updated_standardizing_module <- renderUI({
      # Perform query using metadata_connection
      query_result <- dbGetQuery(metadata_connection, "SELECT standardizing_module_name, standardizing_module_id FROM standardizing_modules")

      # Extract columns from query result
      choices <- setNames(query_result$standardizing_module_id, query_result$standardizing_module_name)

      # Add the additional look up value where the first choice is 0
      choices <- c("Not Applicable" = "null", choices)

      # Create select input with dynamic choices
      return(span(selectInput("selected_updated_standardizing_module", label = "Select Standardizing Module:", choices = choices), style = "width: 500px;"))
    })

    # Re-render the "Add and Update Compound Format" UI
    output$updatable_separator_inputs <- renderUI({
      num_separators <- input$new_updatable_number_of_separators
      num_indexes <- input$new_updatable_number_of_indexes

      # Retrieve pre-populated values from reactive values
      separators <- prepopulated_values$separators
      indexes <- prepopulated_values$indexes
      destination_ids <- prepopulated_values$destination_fields

      if((is.nan(num_separators) || is.na(num_separators) || num_separators <= 0) && input$separator_or_indexes_update == 'Separators'){
        showNotification("Error - Invalid Number of Separators", type = "error", closeButton = FALSE)
        return()
      }

      if((is.nan(num_indexes) || is.na(num_indexes) || num_indexes <= 1) && input$separator_or_indexes_update != 'Separators'){
        showNotification("Error - Invalid Number of Indexes", type = "error", closeButton = FALSE)
        return()
      }

      if(input$separator_or_indexes_update == 'Separators'){
        separator_inputs_list <- lapply(1:((num_separators * 2) + 1), function(i) {
          if (i %% 2 == 1) {  # Odd index, create selectInput for destination field
            # Perform query using metadata_connection
            query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

            # Extract columns from query result
            choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

            # Add the additional look up value where the first choice is 0
            choices <- c("Not Applicable" = "null", choices)
            selectInput(inputId = paste0("updatable_separator_destination_field_", (i + 1) %/% 2),
                        label = paste0("Destination Field ", (i + 1) %/% 2, ":"),
                        choices = choices,
                        selected = destination_ids[(i + 1) %/% 2],
                        width = validateCssUnit("20vw"))
          } else {  # Even index, create textInput for separator
            textInput(inputId = paste0("updatable_separator_", (i + 1) %/% 2),
                      label = paste0("Separator ", (i + 1) %/% 2, ":"),
                      value = separators[(i + 1) %/% 2],
                      width = validateCssUnit("20vw"))
          }
        })

        tagList(separator_inputs_list)
      }
      else{
        index_inputs_list <- lapply(1:((num_indexes)), function(i) {
          # Perform query using metadata_connection
          query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

          # Extract columns from query result
          choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

          # Add the additional look up value where the first choice is 0
          choices <- c("Not Applicable" = "null", choices)
          fluidRow(
            column(6, div(style = "width: 20vw;",
                          selectInput(inputId = paste0("updatable_index_destination_field_", (i)),
                                      label = paste0("Destination Field ", i, ":"),
                                      choices = choices,
                                      selected = destination_ids[i]))),
            column(6, div(style = "width: 20vw;",
                          numericInput(inputId = paste0("updatable_index_", i),
                                       label = paste0("Index ", i, ":"),
                                       value = indexes[i])))
          )
        })

        tagList(index_inputs_list)
      }
    })

    output$separator_inputs_to_add <- renderUI({
      num_separators <- input$number_of_separators_to_add
      num_indexes <- input$number_of_indexes_to_add
      if((is.nan(num_separators) || is.na(num_separators) || num_separators <= 0) && input$separator_or_indexes == 'Separators'){
        showNotification("Error - Invalid Number of Separators", type = "error", closeButton = FALSE)
        return()
      }

      if((is.nan(num_indexes) || is.na(num_indexes) || num_indexes <= 1) && input$separator_or_indexes != 'Separators'){
        showNotification("Error - Invalid Number of Indexes", type = "error", closeButton = FALSE)
        return()
      }

      if(input$separator_or_indexes == 'Separators'){
        separator_inputs_list <- lapply(1:((num_separators * 2) + 1), function(i) {
          if (i %% 2 == 1) {  # Odd index, create selectInput for destination field
            # Perform query using metadata_connection
            query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

            # Extract columns from query result
            choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

            # Add the additional look up value where the first choice is 0
            choices <- c("Not Applicable" = "null", choices)
            selectInput(inputId = paste0("separator_destination_field_to_add_", (i + 1) %/% 2),
                        label = paste0("Destination Field ", (i + 1) %/% 2, ":"),
                        choices = choices,
                        selected = "null",
                        width = validateCssUnit("20vw"))
          } else {  # Even index, create textInput for separator
            textInput(inputId = paste0("separator_to_add_", (i + 1) %/% 2),
                      label = paste0("Separator ", (i + 1) %/% 2, ":"),
                      value = "",
                      width = validateCssUnit("20vw"))
          }
        })

        tagList(separator_inputs_list)
      }
      else{
        index_inputs_list <- lapply(1:((num_indexes)), function(i) {
          # Perform query using metadata_connection
          query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

          # Extract columns from query result
          choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

          # Add the additional look up value where the first choice is 0
          choices <- c("Not Applicable" = "null", choices)
          fluidRow(
            column(6, div(style = "width: 20vw",
                          selectInput(inputId = paste0("index_destination_field_to_add_", (i)),
                                      label = paste0("Destination Field ", i, ":"),
                                      choices = choices,
                                      selected = "null",
                                      width = validateCssUnit("20vw")))),
            column(6, div(style = "width: 20vw",
                          numericInput(inputId = paste0("index_to_add_", i),
                                       label = paste0("Index ", i, ":"),
                                       value = NULL,
                                       width = validateCssUnit("20vw"))))
          )
        })

        tagList(index_inputs_list)
      }

    })

    # Re-render the "Categorical Fields" UI
    output$updated_standardizied_value <- renderUI({
      query_result <- dbGetQuery(metadata_connection, "SELECT * FROM categorical_values")

      # Extract columns from query result
      choices <- setNames(query_result$standardized_value_id, query_result$standardized_value)

      selectInput(inputId = paste0("standardized_categorical_value_updated"),
                  label = paste0("Select Standardized Categorical Value:"),
                  choices = choices,
                  selected = NULL)

    })



  }

  rerender_data_tables <- function(){
    # Re-render "Add Database" data tables
    output$view_current_created_datasets <- renderDataTable({
      df <- dbGetQuery(metadata_connection, paste('SELECT dataset_code, dataset_name, has_header, file_extension_code from datasets d',
                                                  'JOIN file_formats ff on ff.file_format_id = d.file_format_id',
                                                  'where enabled_for_standardization = 1'))
      datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select a Row Below To update OR Create a New Entry Below")
    })

    output$dataset_to_update <- renderDataTable({
      df <- dbGetQuery(metadata_connection, paste('SELECT dataset_code, dataset_name FROM datasets'))
      datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select a Dataset to Add Standardizing Rules To:")
    })

    output$view_selected_table <- renderDataTable({
      df <- data.frame()
      datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE))
    })

    output$categorical_fields_datasets <- renderDataTable({
      df <- dbGetQuery(metadata_connection, paste("SELECT dataset_code, dataset_name FROM datasets"))
      datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select A Table to Add Categorical Fields To:")
    })

    output$source_field_to_update_datasets <- renderDataTable({
      df <- dbGetQuery(metadata_connection, paste('SELECT dataset_code, dataset_name FROM datasets'))
      datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select the Dataset Containing the Source Fields to be Updated:")
    })
  }

  rerender_data_tables_manage_datasets <- function(){

    #-- Add Source Field --#
    observe({
      selected_row <- input$dataset_to_update_rows_selected
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))

      if(!is.null(selected_row)){
        selected_dataset_id <- df[selected_row, "dataset_id"]

        df <- dbGetQuery(metadata_connection, paste('SELECT source_field_id, source_field_name, field_order, fixed_width_length, standardizing_module_name FROM source_fields sf',
                                                    'LEFT JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                                                    'WHERE dataset_id =', selected_dataset_id,
                                                    "ORDER by field_order asc"))
        output$dataset_to_update_source_fields <- renderDataTable({
          datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
                    caption = "Current Dataset Source Fields:")
        })
      }
      else{
        datatable(data.frame(), selection = "none", rownames = FALSE, options = list(lengthChange = FALSE))
      }

    })

    output$compound_format_types <- renderDataTable({
      # Fetch data from the database
      df <- dbGetQuery(metadata_connection, 'SELECT * FROM compound_field_formats')

      # Render the table with single row selection
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select A Compound Format To Use OR Create A New Format Using The Button")
    })

    output$numeric_format_types <- renderDataTable({
      # Fetch data from the database
      df <- dbGetQuery(metadata_connection, 'SELECT * FROM numeric_date_formats')

      # Render the table with single row selection
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select A Numeric Format To Use OR Create A New Format Using The Button")
    })
    #----------------------#

    #-- Add Compound Format --#
    output$add_compound_field_formats <- renderDataTable({
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM compound_field_formats'))
      datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Current Compound Field Formats")
    })
    #-------------------------#

    #-- Add Categorical Field --#
    output$categorical_source_fields_to_add <- renderDataTable({
      if (!is.null(input$categorical_fields_datasets_rows_selected)) {
        selected_row <- input$categorical_fields_datasets_rows_selected
        df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))
        selected_dataset_id <- df[selected_row, "dataset_id"]
        dataset_id_categorical_field_add <<- selected_dataset_id
        query <- paste('SELECT dataset_id, source_field_id, source_field_name  FROM source_fields sf',
                       'JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                       'where dataset_id =', dataset_id_categorical_field_add, 'and standardizing_module_type = 3')
        df <- dbGetQuery(metadata_connection, query)
        datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                  caption = "Select a Categorical Source Field to Add Values To:")

      } else {
        # Return an empty table or a message indicating no row is selected
        datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
      }
    })

    output$add_compound_field_formats <- renderDataTable({
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM compound_field_formats'))
      datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Current Compound Field Formats")
    })
    #---------------------------#

    #-- Add Numeric Date Format --#
    output$add_numeric_date_formats <- renderDataTable({
      df <- dbGetQuery(metadata_connection, 'SELECT * from numeric_date_formats')
      datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Current Numeric Date Formats:")
    })
    #-----------------------------#

    #-- Add to Record Priorities --#
    output$record_priority_datasets <- renderDataTable({
      df <- dbGetQuery(metadata_connection, paste("SELECT dataset_code, dataset_name FROM datasets ORDER BY dataset_id ASC"))
      datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select A Table to Add Record Priorities To:")
    })
    #------------------------------#

    #-- Update Source Field --#
    output$source_field_to_update <- renderDataTable({
      if (!is.null(input$source_field_to_update_datasets_rows_selected)) {
        selected_row <- input$source_field_to_update_datasets_rows_selected
        df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))
        selected_dataset_id <- df[selected_row, "dataset_id"]
        update_source_field_ds_id <<- selected_dataset_id
        query <- paste('SELECT source_field_id, source_field_name, field_order, fixed_width_length, standardizing_module_name FROM source_fields sf',
                       'LEFT JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                       'where dataset_id = ', update_source_field_ds_id,
                       "ORDER by field_order asc")
        df <- dbGetQuery(metadata_connection, query)
        datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                  caption = "Select a Source Field to Update:")
      } else {
        # Return an empty table or a message indicating no row is selected
        datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
      }
    })

    output$updated_compound_format_type <- renderDataTable({
      # Fetch data from the database
      df <- dbGetQuery(metadata_connection, 'SELECT * FROM compound_field_formats')

      # Render the table with single row selection
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select A Compound Format To Use OR Create A New Format Using The Button")
    })

    output$numeric_format_types_update <- renderDataTable({
      # Fetch data from the database
      df <- dbGetQuery(metadata_connection, 'SELECT * FROM numeric_date_formats')

      # Render the table with single row selection
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select A Numeric Format To Use OR Create A New Format Using The Button")
    })
    #-------------------------#

    #-- Update Compound Format --#
    output$updatable_compound_formats <- renderDataTable({
      query <- paste('SELECT cff.compound_field_format_id, compound_format, format_description, count(source_field_id) as Number_Of_Usages FROM compound_field_formats cff',
                     'LEFT JOIN compound_fields cf on cf.compound_field_format_id = cff.compound_field_format_id',
                     'GROUP BY cff.compound_field_format_id',
                     'HAVING Number_Of_Usages = 0')
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select a Compound Format to Update:")
    })
    #----------------------------#

    #-- Update Categorical Field --#
    output$dataset_to_update_cfs <- renderDataTable({
      query <- paste('SELECT dataset_code, dataset_name from datasets')
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select the Dataset Containing the Desired Categorical Field:")
    })
    #------------------------------#

    #-- Update Numeric Format --#
    output$updatable_numeric_formats <- renderDataTable({
      query <- paste('SELECT ndf.numeric_date_format_id, numeric_date_format_label, origin_date, units_label, count(source_field_id) as Number_Of_Usages FROM numeric_date_formats ndf',
                     'LEFT JOIN numeric_date_fields ndf2 on ndf2.numeric_date_format_id = ndf.numeric_date_format_id',
                     'GROUP BY ndf.numeric_date_format_id',
                     'HAVING Number_Of_Usages = 0')
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select a Numeric Format to Update:")
    })
    #---------------------------#

    #-- Update Record Priorities --#
    output$dataset_to_update_record_priority <- renderDataTable({
      query <- paste('SELECT dataset_code, dataset_name from datasets ORDER BY dataset_id ASC')
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select the Dataset Containing the Desired Record Priority:")
    })
    #------------------------------#

    #-- Enable and Disable Databases --#
    output$databases_disable <- renderDataTable({
      df <- dbGetQuery(metadata_connection, 'SELECT dataset_code, dataset_name from datasets WHERE enabled_for_standardization = 1;')
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE, scrollX = TRUE),
                caption = "Select a Dataset to Disable:")
    })

    output$databases_enable <- renderDataTable({
      df <- dbGetQuery(metadata_connection, 'SELECT dataset_code, dataset_name from datasets WHERE enabled_for_standardization = 0;')
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE, scrollX = TRUE),
                caption = "Select a Dataset to Enable:")
    })
    #----------------------------------#

  }

  observeEvent(input$app_settings, {
    showModal(modalDialog(
      title = "App Settings",
      fluidRow(
        column(6, numericInput("font_size_user", "New Font Size:", min = 16, value = 16), align = "right"),
        column(6, actionButton("submit_new_font_size", "Close", class = "btn-lg"), align = "left")
      ),
      footer = NULL
    ))
  })
  #----

  #----------------------------------HOME--------------------------------------#

  # Render the text boxes
  #----

  # Look for changes in the user chosen font size
  observeEvent(input$submit_new_font_size, {
    new_font_size <- input$font_size_user

    if(is.na(new_font_size) || is.null(new_font_size) || new_font_size < 16)
      new_font_size <- 16
    else if(is.na(new_font_size) || is.null(new_font_size) || new_font_size > 24)
      new_font_size <- 24

    removeModal()

    rerender_home_page_boxes(new_font_size)
  })

  rerender_home_page_boxes <- function(new_font_size){

    font_size_string <- paste0("font-size: ", new_font_size, "px;")

    output$welcome_box_content <- renderUI({
      column(12, div(
        style = paste("border: 5px solid #ccc; padding: 10px; width: 1000px; background-color: #Bbbbbb;", font_size_string),
        HTML(welcome_message)), align = "center"
      )
    })
  }

  # Call the function initially with default font size of 16
  rerender_home_page_boxes(16)

  #----

  # Transition between pages
  #----

  # Return to home page
  observeEvent(input$return_home, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Home")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
    reset_ui_renders()
  })

  # Move to view standardization rules
  observeEvent(input$link_to_view_standardization_rules, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "View Standardization Rules")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to add or update databases
  observeEvent(input$link_to_add_databases, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Add and Update Databases")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to enable or disable databases
  observeEvent(input$link_to_enable_databases, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Enable and Disable Databases")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to add source fields
  observeEvent(input$link_to_add_source_fields, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "source_fields")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to update or delete source fields
  observeEvent(input$link_to_update_source_fields, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 2)
    updateSelectInput(session, "table_to_update", selected = "source_fields_update")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to add compound formats
  observeEvent(input$link_to_add_compound_formats, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "compound_field_formats")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to update compound formats
  observeEvent(input$link_to_update_compound_formats, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 2)
    updateSelectInput(session, "table_to_update", selected = "compound_field_formats_update")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to add categorical fields
  observeEvent(input$link_to_add_to_categorical_fields, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "categorical_fields")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to update or delete categorical fields
  observeEvent(input$link_to_update_categorical_fields, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 2)
    updateSelectInput(session, "table_to_update", selected = "categorical_fields_update")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to add categorical values
  observeEvent(input$link_to_add_categorical_values, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "categorical_values")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to add numeric formats
  observeEvent(input$link_to_add_numeric_formats, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "numeric_date_formats")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  # Move to update numeric formats
  observeEvent(input$link_to_update_numeric_formats, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 2)
    updateSelectInput(session, "table_to_update", selected = "numeric_formats_update")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  observeEvent(input$link_to_add_to_record_priority_fields, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "record_priority")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  observeEvent(input$link_to_update_record_priority_fields, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 2)
    updateSelectInput(session, "table_to_update", selected = "record_priority_update")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  observeEvent(input$link_to_create_new_standardizing_module, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Add Standardizing Module")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })

  observeEvent(input$link_to_create_new_destination_fields, {
    # Transition pages by updating the selected page
    updateSelectInput(session, "modification_form", selected = "Add Destination Field")

    # Re-render tables
    rerender_data_tables()
    rerender_data_tables_manage_datasets()
  })
  #----


  #----------------------------------VIEW--------------------------------------#

  # Select Data set to View
  #----
  output$view_dataset_table_ui <- renderUI({

    # Create select input with dynamic choices
    return(selectInput("view_dataset_table",
                choices = list(" " = "null",
                               "Datasets" = "datasets",
                               "Source Fields" = "source_fields",
                               "Standardizing Modules" = "standardizing_modules",
                               "Destination Fields" = "destination_fields",
                               "Record Priority Fields" = "record_priority_fields",
                               "Categorical Fields" = "categorical_fields",
                               "Categorical Values" = "categorical_values",
                               "Compound Fields" = "compound_fields",
                               "Compound Field Formats" = "compound_field_formats",
                               "Compound Field Separators" = "compound_field_separators",
                               "Compound Field Destinations" = "compound_field_destinations",
                               "Numeric Date Destinations" = "numeric_date_destinations",
                               "Numeric Date Fields" = "numeric_date_fields",
                               "Numeric Date Format" = "numeric_date_formats",
                               "File Formats" = "file_formats"),
                selected = "null", label = "Select a Dataset Table:",
                width = validateCssUnit(300)))
  })

  output$view_limited_dataset <- renderUI({
    # Perform query using metadata_connection
    query_result <- dbGetQuery(metadata_connection, "SELECT dataset_name, dataset_id FROM datasets")

    # Extract columns from query result
    choices <- setNames(query_result$dataset_id, query_result$dataset_name)

    # Add the additional look up value where the first choice is 0
    choices <- c(" " = "null", choices)

    # Create select input with dynamic choices
    span(selectInput("view_dataset_selection_sf", label = "Select a Dataset to Limit Search By:",
                     choices = choices, width = validateCssUnit(500)))
  })

  # Observes for any change in what table and dataset we are viewing
  observe({
    selected_table   <- input$view_dataset_table
    selected_dataset <- input$view_dataset_selection_sf

    if(!is.null(selected_table) && selected_table != "null"){
      query <- ""
      if(selected_table == "source_fields" && !is.null(selected_dataset)){
        query <- paste("SELECT * from ", selected_table, " where dataset_id = ", selected_dataset)
      }
      else if((selected_table == "categorical_fields" || selected_table == "compound_fields" ||
               selected_table == "numeric_date_fields" || selected_table == "record_priority_fields") && !is.null(selected_dataset)){
        query <- paste("SELECT dataset_id, source_field_name, ct.* from source_fields sf JOIN ", selected_table,
                       " ct ON sf.source_field_id = ct.source_field_id
                       WHERE dataset_id = ", selected_dataset)
      }
      else{
        query <- paste("SELECT * FROM", selected_table)
      }
      output$view_selected_table <- renderDataTable({
        df <- dbGetQuery(metadata_connection, query)
        datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE)) # THIS IS HOW WE DISABLE "ROW NAMES" AND "SHOW ENTRIES"
      })
    }
  })
  #----

  #----------------------------------CREATE------------------------------------#

  # Create new data set
  #----
  output$created_new_dataset <- renderUI({
    div(
      style = "display: none;",
      selectInput("link_to_manage_datasets",
                  choices = c("no" = 0,
                              "yes" = 1),
                  selected = 0,
                  label = "")
    )
  })

  output$updated_dataset <- renderUI({
    div(
      style = "display: none;",
      selectInput("link_to_manage_datasets_update",
                  choices = c("no" = 0,
                              "yes" = 1),
                  selected = 0,
                  label = "")
    )
  })

  output$view_current_created_datasets <- renderDataTable({
    df <- dbGetQuery(metadata_connection, paste('SELECT dataset_code, dataset_name, has_header, file_extension_code from datasets d',
                                                'JOIN file_formats ff on ff.file_format_id = d.file_format_id',
                                                'where enabled_for_standardization = 1'))
    datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select a Row Below To update OR Create a New Entry Below")
  })

  # Observes what row the user selects to update and will pre-populate the fields
  observe({
    row_selected <- input$view_current_created_datasets_rows_selected

    # Get the entire dataframe for datasets so that we can get the info from the
    # row the user selected.
    df <- dbGetQuery(metadata_connection, paste('SELECT * from datasets where enabled_for_standardization = 1'))
    dataset_code <- df[row_selected, "dataset_code"]
    dataset_name <- df[row_selected, "dataset_name"]
    has_header <- df[row_selected, "has_header"]
    file_format_id <- df[row_selected, "file_format_id"]

    # Now update the input fields
    updateTextAreaInput(session, "update_dataset_code",    value = dataset_code)
    updateTextAreaInput(session, "update_dataset_name",    value = dataset_name)
    updateSelectInput(session,   "update_has_header",      selected = has_header)
    updateSelectInput(session,   "update_file_extension",  selected = file_format_id)
  })

  observeEvent(input$submit_new_dataset_record, {
    dataset_code   <- input$new_record_dataset_code
    dataset_name   <- input$new_record_dataset_name
    has_header     <- input$new_record_has_header
    file_format_id <- input$new_record_file_extension

    # Error checking to ensure that the new data set being added isn't using an existing dataset_code
    #----#
    get_query <- dbSendQuery(metadata_connection, 'SELECT * FROM datasets WHERE dataset_code = ? AND enabled_for_standardization = 1;')
    dbBind(get_query, list(dataset_code))
    output_df <- dbFetch(get_query)
    num_of_databases <- nrow(output_df)
    dbClearResult(get_query)
    if(num_of_databases != 0){
      showNotification("Failed to Add Dataset - Dataset Code Already in Use", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    new_entry_query <- paste("INSERT INTO datasets (dataset_code, dataset_name, has_header, file_format_id, enabled_for_standardization)",
                             "VALUES(?, ?, ?, ?, 1);")
    new_entry <- dbSendQuery(metadata_connection, new_entry_query)
    dbBind(new_entry, list(dataset_code, dataset_name, has_header, file_format_id))
    dbClearResult(new_entry)

    updateTextAreaInput(session, "new_record_dataset_code", value = "")
    updateTextAreaInput(session, "new_record_dataset_name", value = "")
    updateSelectInput(session, "new_record_has_header", selected = "null")
    updateSelectInput(session, "new_record_file_extension", selected = "null")
    updateSelectInput(session, "link_to_manage_datasets", selected = 1)
    updateSelectInput(session, "link_to_manage_datasets_update", selected = 0)

    reset_update_entry_widgets()
    reset_view_metadata_widgets()

    # Re-render the UI select inputs and data tables that use the sqlite data base.
    reset_ui_renders()
    rerender_data_tables()

    showNotification("Dataset Successfully Added", type = "message", closeButton = FALSE)
  })

  observeEvent(input$update_dataset_record, {
    dataset_code   <- input$update_dataset_code
    dataset_name   <- input$update_dataset_name
    has_header     <- input$update_has_header
    file_format_id <- input$update_file_extension
    selected_row   <- input$view_current_created_datasets_rows_selected

    df <- dbGetQuery(metadata_connection, paste('SELECT * from datasets where enabled_for_standardization = 1'))

    selected_dataset_id <- df[selected_row, "dataset_id"]

    # Error handling to make sure update runs smoothly
    #----#

    # Verify that the new dataset being added isn't using an existing dataset_code
    get_query <- dbSendQuery(metadata_connection, 'SELECT * FROM datasets WHERE dataset_id != ? AND dataset_code = ? AND enabled_for_standardization = 1;')
    dbBind(get_query, list(selected_dataset_id, dataset_code))
    output_df <- dbFetch(get_query)
    num_of_databases <- nrow(output_df)
    dbClearResult(get_query)
    if(num_of_databases != 0){
      showNotification("Failed to Add Dataset - Dataset Code Already in Use", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    # Create a query for updating the dataset
    update_query <- paste("UPDATE datasets
                              SET dataset_code = ?, dataset_name = ?, has_header = ?, file_format_id = ?
                              WHERE dataset_id = ?")
    update <- dbSendQuery(metadata_connection, update_query)
    dbBind(update, list(dataset_code, dataset_name, has_header, file_format_id, selected_dataset_id))
    dbClearResult(update)

    updateTextAreaInput(session, "update_dataset_code", value = "")
    updateTextAreaInput(session, "update_dataset_name", value = "")
    updateSelectInput(session, "update_has_header", selected = NULL)
    updateSelectInput(session, "update_file_extension", selected = NULL)
    updateSelectInput(session, "link_to_manage_datasets", selected = 0)
    updateSelectInput(session, "link_to_manage_datasets_update", selected = 1)

    reset_update_entry_widgets()
    reset_view_metadata_widgets()

    # Re-render the data tables and update the select input UI
    rerender_data_tables()
    reset_ui_renders()

    showNotification("Dataset Successfully Updated", type = "message", closeButton = FALSE)
  })

  # The dataset code may only be 20 characters, this observe will shorten it if too long
  observe({
    dataset_code <- input$new_record_dataset_code
    updated_dataset_code <- input$update_dataset_code

    if(nchar(dataset_code) > 20){
      updateTextAreaInput(session, "new_record_dataset_code", value = substr(dataset_code, 1, 20))
    }

    if(nchar(updated_dataset_code) > 20){
      updateTextAreaInput(session, "update_dataset_code", value = substr(updated_dataset_code, 1, 20))
    }
  })

  observeEvent(input$go_to_manage_database, {
    # Update the select inputs and link the user to creating new source fields.
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "source_fields")
  })

  observeEvent(input$go_to_manage_database_update, {
    # Update the select inputs and link the user to creating new source fields.
    updateSelectInput(session, "modification_form", selected = "Manage Database Standardization Rules")
    updateSelectInput(session, "update_type", selected = 2)
    updateSelectInput(session, "table_to_update", selected = "source_fields_update")
  })
  #----

  #----------------------------------UPDATE------------------------------------#

  # TO-DO: WHEN WE INSERT A FIELD, ADJUST THE ORDER_ID ACCORDINGLY!!!
  #-- Add to Source Fields --#
  #----
  output$select_standardizing_module <- renderUI({
    # Perform query using metadata_connection
    query_result <- dbGetQuery(metadata_connection, "SELECT standardizing_module_name, standardizing_module_id, description FROM standardizing_modules")

    # Extract columns from query result
    choices <- setNames(query_result$standardizing_module_id, query_result$standardizing_module_name)

    # Add the additional look up value where the first choice is 0
    choices <- c("Not Applicable" = "null", choices)

    # Create select input with dynamic choices
    span(selectInput("selected_standardizing_module", label = "Standardizing Module:", choices = choices, width = validateCssUnit("20vw")))
  })

  output$standardizing_module_description <- renderPrint({
    selected_module_id <- input$selected_standardizing_module

    if(!is.null(selected_module_id) && selected_module_id != "null"){
      standardizing_type_query <- paste0("SELECT standardizing_module_name, description FROM standardizing_modules WHERE standardizing_module_id = ", selected_module_id)
      df <- dbGetQuery(metadata_connection, standardizing_type_query)

      module_name <- df$standardizing_module_name
      description <- df$description

      div(
        style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px;",
        HTML(paste0("<strong>", module_name, "</strong>: ", description))
      )
    }
    else{
      div(
        style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px;",
        HTML(paste0("<strong>Not Applicable</strong>: Not Applicable fields are ignored when processing linkage ",
                    "fields and are only including in the health and program data afterwards."))
      )
    }
  })

  output$standardizing_module_type_create <- renderUI({
    div(
      style = "display: none;",
      selectInput("standardizing_type_create_source_field",
                  choices = c("health/program" = "", "normal" = 1,
                              "compound" = 2,
                              "categorical" = 3,
                              "numerical" = 4,
                              "priority" = 5),
                  selected = "",
                  label = "")
    )
  })

  observeEvent(input$selected_standardizing_module, {

    # Get the selected standardizing module ID
    selected_module_id <- input$selected_standardizing_module

    # Check if a standardizing module is selected
    if (!is.null(selected_module_id) && selected_module_id != "null") {
      # Perform another query to check for standardizing type using selected_module_id
      standardizing_type_query <- paste0("SELECT standardizing_module_type FROM standardizing_modules WHERE standardizing_module_id = ", selected_module_id)
      standardizing_type <- dbGetQuery(metadata_connection, standardizing_type_query)$standardizing_module_type

      # Update the choices in standardizing_type_update_source_field based on standardizing type
      updateSelectInput(session, "standardizing_type_create_source_field", selected = standardizing_type)
    }
    else{
      updateSelectInput(session, "standardizing_type_create_source_field", selected = "")
    }
  })

  output$compound_format_types <- renderDataTable({
    # Fetch data from the database
    df <- dbGetQuery(metadata_connection, 'SELECT * FROM compound_field_formats')

    # Render the table with single row selection
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select A Compound Format To Use OR Create A New Format Using The Button")
  })

  output$numeric_format_types <- renderDataTable({
    # Fetch data from the database
    df <- dbGetQuery(metadata_connection, 'SELECT * FROM numeric_date_formats')

    # Render the table with single row selection
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select A Numeric Format To Use OR Create A New Format Using The Button")
  })

  output$separator_inputs <- renderUI({
    # V2
    #----
    num_separators <- input$number_of_separators

    separator_inputs_list <- lapply(1:((num_separators * 2) + 1), function(i) {
      if (i %% 2 == 1) {  # Odd index, create selectInput for destination field
        # Perform query using metadata_connection
        query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

        # Extract columns from query result
        choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

        # Add the additional look up value where the first choice is 0
        choices <- c("Not Applicable" = "null", choices)
        selectInput(inputId = paste0("separator_destination_field_", (i + 1) %/% 2),
                    label = paste0("Destination Field For Split ", (i + 1) %/% 2, ":"),
                    choices = choices,
                    selected = "null")
      } else {  # Even index, create textInput for separator
        textInput(inputId = paste0("separator_", (i + 1) %/% 2),
                  label = paste0("Separator ", (i + 1) %/% 2, ":"),
                  value = "")
      }
    })

    tagList(separator_inputs_list)
    #----

  })

  output$categorical_inputs <- renderUI({
    num_fields <- input$number_of_categorical_fields

    if(is.nan(num_fields) || is.na(num_fields) || num_fields <= 0){
      showNotification("Error - Invalid Number of Categorical Fields", type = "error", closeButton = FALSE)
      return()
    }

    separator_inputs_list <- lapply(1:(num_fields), function(i) {
      query_result <- dbGetQuery(metadata_connection, "SELECT * FROM categorical_values")

      # Extract columns from query result
      choices <- setNames(query_result$standardized_value_id, query_result$standardized_value)

      fluidRow(
        column(width = 6, textInput(inputId = paste0("categorical_input_value_", i),
                                    label = paste0("Source Categorical Value ", i),
                                    value = ""), align = "right"),
        column(width = 6, selectInput(inputId = paste0("categorical_output_value_", i),
                                      label = paste0("Standardized Categorical Value ", i, ":"),
                                      choices = choices,
                                      selected = NULL), align = "left")
      )
    })
  })

  output$record_priority_inputs <- renderUI({
    num_fields <- input$number_of_record_priority_fields

    if(is.nan(num_fields) || is.na(num_fields) || num_fields <= 0){
      showNotification("Error - Invalid Number of Record Priority Fields", type = "error", closeButton = FALSE)
      return()
    }

    separator_inputs_list <- lapply(1:(num_fields), function(i) {
      query_result <- dbGetQuery(metadata_connection, "SELECT * FROM categorical_values")

      fluidRow(
        column(width = 6, textInput(inputId = paste0("record_priority_input_value_", i),
                                    label = paste0("Source Field Value ", i, ":"),
                                    value = ""), align = "right"),
        column(width = 6, numericInput(inputId = paste0("record_priority_output_value_", i),
                                      label = paste0("Record Priority ", i, ":"),
                                      value = NULL), align = "left")
      )
    })
  })

  output$dataset_to_update <- renderDataTable({
    df <- dbGetQuery(metadata_connection, paste('SELECT dataset_code, dataset_name FROM datasets'))
    datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select a Dataset to Add Standardizing Rules To:")
  })

  # This observe function will look for what row the user selects to add a new
  # source field for the dataset.
  observe({
    selected_row <- input$dataset_to_update_rows_selected
    df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))

    if(!is.null(selected_row)){
      selected_dataset_id <- df[selected_row, "dataset_id"]

      df <- dbGetQuery(metadata_connection, paste('SELECT source_field_id, source_field_name, field_order, fixed_width_length, standardizing_module_name FROM source_fields sf',
                                                  'LEFT JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                                                  'WHERE dataset_id =', selected_dataset_id,
                                                  "ORDER by field_order asc"))
      output$dataset_to_update_source_fields <- renderDataTable({
        datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
                  caption = "Current Dataset Source Fields:")
      })

      output$new_field_order_max_add <- renderUI({
        max_field_order <- nrow(df) + 1
        numericInput("new_field_order", "Field Order:", value = 1, min = 1, max = max_field_order, width = validateCssUnit("20vw"))
      })
    }
    else{
      output$new_field_order_max_add <- renderUI({
        numericInput("new_field_order", "Field Order:", value = 1, min = 1, width = validateCssUnit("20vw"))
      })
      datatable(data.frame(), selection = "none")
    }

  })

  observeEvent(input$submit_new_dataset_field, {
    # Query to get dataset_id
    df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))
    dataset_selected_row        <- input$dataset_to_update_rows_selected
    dataset_id                  <- df[dataset_selected_row, "dataset_id"]
    source_field_name           <- input$new_source_field_name
    field_order                 <- input$new_field_order
    fixed_width_length          <- input$new_fixed_width_length
    standardizing_module        <- input$selected_standardizing_module
    if(standardizing_module == "null")
      standardizing_module <- NA
    standardizing_module_type   <- input$standardizing_type_create_source_field
    compound_field_selected_row <- input$compound_format_types_rows_selected
    numeric_field_selected_row  <- input$numeric_format_types_rows_selected

    # Error handling
    #----#
    if(field_order <= 0){
      showNotification("Failed to Add Source Field - Invalid Field Order", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    if(!is.null(standardizing_module) && standardizing_module_type == 2){
      # Fetch data from the database
      df <- dbGetQuery(metadata_connection, 'SELECT * FROM compound_field_formats')

      # Get the compound format id using the selected row
      compound_format_id <- df[compound_field_selected_row, "compound_field_format_id"]

      if(is.na(compound_format_id)){
        num_separators     <- input$number_of_separators
        format_description <- input$new_compound_format_description
        format             <- input$new_compound_format
        separators <- c()
        # indexes <- c()
        destination_field_ids <- c()
        for(i in seq_len(num_separators + 1)){
          if(i <= num_separators){
            separator_input_id <- paste0("separator_", i)
            separator_value <- input[[separator_input_id]]
            separators <- c(separators, separator_value)
          }

          destination_field_input_id <- paste0("separator_destination_field_", i)
          destination_field_value <- input[[destination_field_input_id]]
          if(destination_field_value == "null"){
            destination_field_value <- NA
          }
          destination_field_ids <- c(destination_field_ids, destination_field_value)
        }

        # Error handling
        #----#
        for(i in seq_along(separators)){
          if(separators[i] == ""){
            showNotification("Failed to Add Source Field - Some Compound Format Inputs are Missing", type = "error", closeButton = FALSE)
            return()
          }
        }
        #----#

        # Create a query for inserting a new compound format into compound_field_formats
        new_entry_query <- paste("INSERT INTO compound_field_formats (compound_format, format_description)",
                                 "VALUES(?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(format, format_description))
        dbClearResult(new_entry)

        # Get the most recently inserted compound_field_format_id value
        compound_field_format_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS compound_field_format_id;")$compound_field_format_id

        # Loop for adding the separators into the metadata
        for(i in seq_along(separators)){
          # Create a query for inserting a new compound separator entry into compound_field_separators
          new_entry_query <- paste("INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)",
                                   "VALUES(?, ?, ?, ?);")
          new_entry <- dbSendQuery(metadata_connection, new_entry_query)
          dbBind(new_entry, list(compound_field_format_id, i, separators[i], NA)) #NA for now, change it later!
          dbClearResult(new_entry)
        }

        # Loop for adding the compound field destinations into the metadata
        for(i in seq_along(destination_field_ids)){

          # Create a query for inserting a new compound separator entry into compound_field_destinations
          new_entry_query <- paste("INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)",
                                   "VALUES(?, ?, ?);")
          new_entry <- dbSendQuery(metadata_connection, new_entry_query)
          dbBind(new_entry, list(compound_field_format_id, i, destination_field_ids[i]))
          dbClearResult(new_entry)
        }

        # Increment the field_order of existing entries where field_order >= new_field_order
        update_query <- paste("UPDATE source_fields SET field_order = field_order + 1 where field_order >= ? and dataset_id = ?")
        update_statement <- dbSendQuery(metadata_connection, update_query)
        dbBind(update_statement, list(field_order, dataset_id))
        dbClearResult(update_statement)

        # Create a query for inserting a new source field into source_fields
        new_entry_query <- paste("INSERT INTO source_fields (dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module_id)",
                                 "VALUES(?, ?, ?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module))
        dbClearResult(new_entry)

        # Get the most recently inserted source_field_id value
        source_field_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS source_field_id;")$source_field_id

        # Finally, create a query for inserting a new compound field into compound_fields
        new_entry_query <- paste("INSERT INTO compound_fields (source_field_id, compound_field_format_id)",
                                 "VALUES(?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(source_field_id, compound_field_format_id))
        dbClearResult(new_entry)
      }
      else{
        # Increment the field_order of existing entries where field_order >= new_field_order
        update_query <- paste("UPDATE source_fields SET field_order = field_order + 1 where field_order >= ? and dataset_id = ?")
        update_statement <- dbSendQuery(metadata_connection, update_query)
        dbBind(update_statement, list(field_order, dataset_id))
        dbClearResult(update_statement)

        # Create a query for inserting a new source field into source_fields
        new_entry_query <- paste("INSERT INTO source_fields (dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module_id)",
                                 "VALUES(?, ?, ?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module))
        dbClearResult(new_entry)

        # Get the most recently inserted source_field_id value
        source_field_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS source_field_id;")$source_field_id

        # Create a query for inserting a new compound field into compound_fields
        new_entry_query <- paste("INSERT INTO compound_fields (source_field_id, compound_field_format_id)",
                                 "VALUES(?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(source_field_id, compound_format_id))
        dbClearResult(new_entry)
      }
    }
    else if (!is.null(standardizing_module) && standardizing_module_type == 3){
      num_categorical_fields <- input$number_of_categorical_fields

      # Error handling to ensure adding a source field goes smoothly
      #----#
      if(is.nan(num_categorical_fields) || is.na(num_categorical_fields) || num_categorical_fields == 0){
        showNotification("Error - Invalid Number of Categorical Fields", type = "error", closeButton = FALSE)
        return()
      }

      for(i in seq_len(num_categorical_fields)){
        categorical_input_value_id <- paste0("categorical_input_value_", i)
        categorical_input_value <- input[[categorical_input_value_id]]

        categorical_standardized_value_id <- paste0("categorical_output_value_", i)
        categorical_standardized_value <- input[[categorical_standardized_value_id]]

        if(categorical_input_value == ""){
          showNotification("Failed to Add Source Field - Some Categorical Inputs are Missing", type = "error", closeButton = FALSE)
          return()
        }
      }
      #----#

      # Increment the field_order of existing entries where field_order >= new_field_order
      update_query <- paste("UPDATE source_fields SET field_order = field_order + 1 where field_order >= ? and dataset_id = ?")
      update_statement <- dbSendQuery(metadata_connection, update_query)
      dbBind(update_statement, list(field_order, dataset_id))
      dbClearResult(update_statement)

      # Create a query for inserting a new source field into source_fields
      new_entry_query <- paste("INSERT INTO source_fields (dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module_id)",
                               "VALUES(?, ?, ?, ?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module))
      dbClearResult(new_entry)

      # Get the most recently inserted compound_field_format_id value
      source_field_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS source_field_id;")$source_field_id

      for(i in seq_len(num_categorical_fields)){
        categorical_input_value_id <- paste0("categorical_input_value_", i)
        categorical_input_value <- input[[categorical_input_value_id]]

        categorical_standardized_value_id <- paste0("categorical_output_value_", i)
        categorical_standardized_value <- input[[categorical_standardized_value_id]]

        # Create a query for inserting a new categorical field into categorical_fields
        new_entry_query <- paste("INSERT INTO categorical_fields (source_field_id, source_value, standardized_value_id)",
                                 "VALUES(?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(source_field_id, categorical_input_value, categorical_standardized_value))
        dbClearResult(new_entry)
      }

      updateNumericInput(session, "number_of_categorical_fields", value = 2)
      updateNumericInput(session, "number_of_categorical_fields", value = 1)
    }
    else if (!is.null(standardizing_module) && standardizing_module_type == 4){
      # Increment the field_order of existing entries where field_order >= new_field_order
      update_query <- paste("UPDATE source_fields SET field_order = field_order + 1 where field_order >= ? and dataset_id = ?")
      update_statement <- dbSendQuery(metadata_connection, update_query)
      dbBind(update_statement, list(field_order, dataset_id))
      dbClearResult(update_statement)

      # Create a query for inserting a new source field into source_fields
      new_entry_query <- paste("INSERT INTO source_fields (dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module_id)",
                               "VALUES(?, ?, ?, ?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module))
      dbClearResult(new_entry)

      # Get the most recently inserted source_field_id value
      source_field_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS source_field_id;")$source_field_id

      # Get the input variables
      df <- dbGetQuery(metadata_connection, 'SELECT * FROM numeric_date_formats')
      numeric_format_id <- df[numeric_field_selected_row, "numeric_date_format_id"]
      numeric_dest_type <- input$numeric_destination_type_new_sf

      # Create a query for inserting a new compound field into compound_fields
      new_entry_query <- paste("INSERT INTO numeric_date_fields (source_field_id, numeric_date_format_id, numeric_destination_type)",
                               "VALUES(?, ?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(source_field_id, numeric_format_id, numeric_dest_type))
      dbClearResult(new_entry)
    }
    else if (!is.null(standardizing_module) && standardizing_module_type == 5){
      num_record_priority_fields <- input$number_of_record_priority_fields

      # Error handling to ensure adding a source field goes smoothly
      #----#
      if(is.nan(num_record_priority_fields) || is.na(num_record_priority_fields) || num_record_priority_fields == 0){
        showNotification("Error - Invalid Number of Record Priority Fields", type = "error", closeButton = FALSE)
        return()
      }

      for(i in seq_len(num_record_priority_fields)){
        categorical_input_value_id <- paste0("record_priority_input_value_", i)
        categorical_input_value <- input[[categorical_input_value_id]]

        categorical_standardized_value_id <- paste0("record_priority_output_value_", i)
        categorical_standardized_value <- input[[categorical_standardized_value_id]]

        if(categorical_input_value == "" || is.na(categorical_standardized_value) || categorical_standardized_value <= 0){
          showNotification("Failed to Add Source Field - Some Record Priority Inputs are Missing", type = "error", closeButton = FALSE)
          return()
        }
      }

      #----#

      # Increment the field_order of existing entries where field_order >= new_field_order
      update_query <- paste("UPDATE source_fields SET field_order = field_order + 1 where field_order >= ? and dataset_id = ?")
      update_statement <- dbSendQuery(metadata_connection, update_query)
      dbBind(update_statement, list(field_order, dataset_id))
      dbClearResult(update_statement)

      # Create a query for inserting a new source field into source_fields
      new_entry_query <- paste("INSERT INTO source_fields (dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module_id)",
                               "VALUES(?, ?, ?, ?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module))
      dbClearResult(new_entry)

      # Get the most recently inserted compound_field_format_id value
      source_field_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS source_field_id;")$source_field_id

      for(i in seq_len(num_record_priority_fields)){
        categorical_input_value_id <- paste0("record_priority_input_value_", i)
        categorical_input_value <- input[[categorical_input_value_id]]

        categorical_standardized_value_id <- paste0("record_priority_output_value_", i)
        categorical_standardized_value <- input[[categorical_standardized_value_id]]

        # Create a query for inserting a new categorical field into categorical_fields
        new_entry_query <- paste("INSERT INTO record_priority_fields (source_field_id, source_value, priority)",
                                 "VALUES(?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(source_field_id, categorical_input_value, categorical_standardized_value))
        dbClearResult(new_entry)
      }

      updateNumericInput(session, "number_of_record_priority_fields", value = 2)
      updateNumericInput(session, "number_of_record_priority_fields", value = 1)
      #----#
    }
    else if (!is.null(standardizing_module)){
      # Basic create statement that doesn't have any other tables that use the
      # source field id in it.

      # Increment the field_order of existing entries where field_order >= new_field_order
      update_query <- paste("UPDATE source_fields SET field_order = field_order + 1 where field_order >= ? and dataset_id = ?")
      update_statement <- dbSendQuery(metadata_connection, update_query)
      dbBind(update_statement, list(field_order, dataset_id))
      dbClearResult(update_statement)

      # Create a query for inserting a new source field into source_fields
      new_entry_query <- paste("INSERT INTO source_fields (dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module_id)",
                               "VALUES(?, ?, ?, ?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(dataset_id, source_field_name, field_order, fixed_width_length, standardizing_module))
      dbClearResult(new_entry)

    }
    cat()

    # Re-render some data sets
    rerender_data_tables_manage_datasets()


    # Update the widgets on this page
    updateTextInput(session, "new_source_field_name", value = "")
    updateNumericInput(session, "new_field_order", value = "")
    updateNumericInput(session, "new_fixed_width_length", value = "")
    updateSelectInput(session, "selected_standardizing_module", selected = "null")
    updateSelectInput(session, "standardizing_type_create_source_field", selected = "")

    # Reset the widgets on other pages:
    reset_add_dataset_widgets()
    reset_view_metadata_widgets()

    showNotification("Source Field Successfully Added", type = "message", closeButton = FALSE)
  })

  observeEvent(input$add_compound_format_for_new_sf, {
    updateSelectInput(session, "table_to_add_to", selected = "compound_field_formats")
    updateSelectInput(session, "create_new_compound_format_new_sf", selected = 1)
    updateSelectInput(session, "create_new_compound_format_update_sf", selected = 0)
  })

  observeEvent(input$add_numeric_format_for_new_sf, {
    updateSelectInput(session, "table_to_add_to", selected = "numeric_date_formats")
    updateSelectInput(session, "create_new_numeric_format_new", selected = 1)
    updateSelectInput(session, "create_new_numeric_format_update", selected = 0)
  })

  #----

  #-- Add to Compound Formats --#
  #----
  output$created_new_compound_format_new_sf <- renderUI({
    div(
      style = "display: none;",
      selectInput("create_new_compound_format_new_sf",
                  choices = c("no" = 0,
                              "yes" = 1),
                  selected = 0,
                  label = ""),
    )
  })

  output$created_new_compound_format_update_sf <- renderUI({
    div(
      style = "display: none;",
      selectInput("create_new_compound_format_update_sf",
                  choices = c("no" = 0,
                              "yes" = 1),
                  selected = 0,
                  label = ""),
    )
  })

  output$add_compound_field_formats <- renderDataTable({
    df <- dbGetQuery(metadata_connection, paste('SELECT * FROM compound_field_formats'))
    datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Current Compound Field Formats")
  })

  output$separator_inputs_to_add <- renderUI({
    num_separators <- input$number_of_separators_to_add
    num_indexes <- input$number_of_indexes_to_add
    if((is.nan(num_separators) || is.na(num_separators) || num_separators <= 0) && input$separator_or_indexes == 'Separators'){
      showNotification("Error - Invalid Number of Separators", type = "error", closeButton = FALSE)
      return()
    }

    if((is.nan(num_indexes) || is.na(num_indexes) || num_indexes <= 1) && input$separator_or_indexes != 'Separators'){
      showNotification("Error - Invalid Number of Indexes", type = "error", closeButton = FALSE)
      return()
    }

    if(input$separator_or_indexes == 'Separators'){
      separator_inputs_list <- lapply(1:((num_separators * 2) + 1), function(i) {
        if (i %% 2 == 1) {  # Odd index, create selectInput for destination field
          # Perform query using metadata_connection
          query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

          # Extract columns from query result
          choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

          # Add the additional look up value where the first choice is 0
          choices <- c("Not Applicable" = "null", choices)
          selectInput(inputId = paste0("separator_destination_field_to_add_", (i + 1) %/% 2),
                      label = paste0("Destination Field ", (i + 1) %/% 2, ":"),
                      choices = choices,
                      selected = "null",
                      width = validateCssUnit("20vw"))
        } else {  # Even index, create textInput for separator
          textInput(inputId = paste0("separator_to_add_", (i + 1) %/% 2),
                    label = paste0("Separator ", (i + 1) %/% 2, ":"),
                    value = "",
                    width = validateCssUnit("20vw"))
        }
      })

      tagList(separator_inputs_list)
    }
    else{
      index_inputs_list <- lapply(1:((num_indexes)), function(i) {
          # Perform query using metadata_connection
          query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

          # Extract columns from query result
          choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

          # Add the additional look up value where the first choice is 0
          choices <- c("Not Applicable" = "null", choices)
          fluidRow(
            column(6, div(style = "width: 20vw",
                          selectInput(inputId = paste0("index_destination_field_to_add_", (i)),
                                  label = paste0("Destination Field ", i, ":"),
                                  choices = choices,
                                  selected = "null",
                                  width = validateCssUnit("20vw")))),
            column(6, div(style = "width: 20vw",
                          numericInput(inputId = paste0("index_to_add_", i),
                                   label = paste0("Index ", i, ":"),
                                   value = NULL,
                                   width = validateCssUnit("20vw"))))
          )
      })

      tagList(index_inputs_list)
    }

  })

  observeEvent(input$submit_new_compound_format, {
    num_separators     <- input$number_of_separators_to_add
    num_indexes        <- input$number_of_indexes_to_add
    format_description <- input$new_compound_format_description_to_add
    format             <- input$new_compound_format_to_add
    split_type         <- input$separator_or_indexes
    separators <- c()
    indexes <- c()
    destination_field_ids <- c()

    # Error handling pt. 1
    #----#
    if(is.nan(num_separators) || is.na(num_separators) || num_separators <= 0){
      showNotification("Error - Invalid Number of Separators", type = "error", closeButton = FALSE)
      return()
    }

    if(is.nan(num_indexes) || is.na(num_indexes) || num_indexes <= 1){
      showNotification("Error - Invalid Number of Indexes", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    if(split_type == "Separators"){
      for(i in seq_len(num_separators + 1)){
        if(i <= num_separators){
          separator_input_id <- paste0("separator_to_add_", i)
          separator_value <- input[[separator_input_id]]
          separators <- c(separators, separator_value)
        }

        destination_field_input_id <- paste0("separator_destination_field_to_add_", i)
        destination_field_value <- input[[destination_field_input_id]]
        if(destination_field_value == "null"){
          destination_field_value <- NA
        }
        destination_field_ids <- c(destination_field_ids, destination_field_value)
      }

      # Error handling pt. 2
      #----#
      for(i in seq_along(separators)){
        if(separators[i] == ""){
          showNotification("Failed to Create Compound Format - Some Compound Format Inputs are Missing", type = "error", closeButton = FALSE)
          return()
        }
      }
      #----#

      # Create a query for inserting a new compound format into compound_field_formats
      new_entry_query <- paste("INSERT INTO compound_field_formats (compound_format, format_description)",
                               "VALUES(?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(format, format_description))
      dbClearResult(new_entry)

      # Get the most recently inserted compound_field_format_id value
      compound_field_format_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS compound_field_format_id;")$compound_field_format_id

      # Loop for adding the separators into the metadata
      for(i in seq_along(separators)){
        # Create a query for inserting a new compound separator entry into compound_field_separators
        new_entry_query <- paste("INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)",
                                 "VALUES(?, ?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(compound_field_format_id, i, separators[i], NA)) #NA for now, change it later!
        dbClearResult(new_entry)
      }

      # Loop for adding the compound field destinations into the metadata
      for(i in seq_along(destination_field_ids)){
        # Create a query for inserting a new compound separator entry into compound_field_destinations
        new_entry_query <- paste("INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)",
                                 "VALUES(?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(compound_field_format_id, i, destination_field_ids[i]))
        dbClearResult(new_entry)
      }
    }
    else{
      for(i in seq_len(num_indexes)){
        index_input_id <- paste0("index_to_add_", i)
        index_value <- input[[index_input_id]]
        indexes <- c(indexes, index_value)

        destination_field_input_id <- paste0("index_destination_field_to_add_", i)
        destination_field_value <- input[[destination_field_input_id]]

        if(destination_field_value == "null"){
          destination_field_value <- NA
        }
        destination_field_ids <- c(destination_field_ids, destination_field_value)
      }

      # Error handling pt. 2
      #----#
      for(i in seq_along(indexes)){
        if(is.na(indexes[i]) || is.null(indexes[i]) || indexes[i] <= 0){
          showNotification("Failed to Create Compound Format - Some Compound Format Inputs are Missing", type = "error", closeButton = FALSE)
          return()
        }
      }
      #----#

      # Create a query for inserting a new compound format into compound_field_formats
      new_entry_query <- paste("INSERT INTO compound_field_formats (compound_format, format_description)",
                               "VALUES(?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(format, format_description))
      dbClearResult(new_entry)

      # Get the most recently inserted compound_field_format_id value
      compound_field_format_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS compound_field_format_id;")$compound_field_format_id

      # Loop for adding the separators into the metadata
      for(i in seq_along(indexes)){
        # Create a query for inserting a new compound separator entry into compound_field_separators
        new_entry_query <- paste("INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)",
                                 "VALUES(?, ?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(compound_field_format_id, i, NA, indexes[i])) #NA for now, change it later!
        dbClearResult(new_entry)
      }

      # Loop for adding the compound field destinations into the metadata
      for(i in seq_along(destination_field_ids)){
        # Create a query for inserting a new compound separator entry into compound_field_destinations
        new_entry_query <- paste("INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)",
                                 "VALUES(?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(compound_field_format_id, i, destination_field_ids[i]))
        dbClearResult(new_entry)
      }
    }

    rerender_data_tables_manage_datasets()

    updateTextAreaInput(session, "new_compound_format_to_add", value = "")
    updateTextAreaInput(session, "new_compound_format_description_to_add", value = "")
    updateNumericInput(session, "number_of_separators_to_add", value = 2)
    updateNumericInput(session, "number_of_separators_to_add", value = 1)
    updateNumericInput(session, "number_of_indexes_to_add", value = 3)
    updateNumericInput(session, "number_of_indexes_to_add", value = 2)

    if(input$create_new_compound_format_new_sf == 1){
      updateSelectInput(session, "table_to_add_to", selected = "source_fields")
      updateSelectInput(session, "create_new_compound_format_new_sf", selected = 0)
    }
    else if (input$create_new_compound_format_update_sf == 1){
      updateSelectInput(session, "update_type", selected = 2)
      updateSelectInput(session, "table_to_update", selected = "source_fields_update")
      updateSelectInput(session, "create_new_compound_format_new_sf", selected = 0)
    }

    reset_view_metadata_widgets()
    reset_add_dataset_widgets()

    showNotification("Compound Format Successfully Added", type = "message", closeButton = FALSE)
  })

  observeEvent(input$see_separator_example, {
    showModal(modalDialog(
      title = "Separator Example",
      tags$div(
        style = "max-height: 80vh; overflow-y: auto;",
        tags$img(src='separator_example.png', width = "100%")
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$see_index_example, {
    showModal(modalDialog(
      title = "Index Example",
      tags$div(
        style = "max-height: 80vh; overflow-y: auto;",
        tags$img(src='index_example.png', width = "100%")
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  #----

  #-- Add to Categorical Fields --#
  #----
  # Categorical Field Variables
  dataset_id_categorical_field_add <- 0
  source_id_categorical_field_add  <- 0

  output$categorical_fields_datasets <- renderDataTable({
    df <- dbGetQuery(metadata_connection, paste("SELECT dataset_code, dataset_name FROM datasets"))
    datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select A Table to Add Categorical Fields To:")
  })

  output$categorical_source_fields_to_add <- renderDataTable({
    if (!is.null(input$categorical_fields_datasets_rows_selected)) {
      #input$categorical_source_fields_to_add_rows_selected <- NULL
      selected_row <- input$categorical_fields_datasets_rows_selected
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))
      selected_dataset_id <- df[selected_row, "dataset_id"]
      dataset_id_categorical_field_add <<- selected_dataset_id
      query <- paste('SELECT dataset_id, source_field_id, source_field_name  FROM source_fields sf',
                     'JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                     'where dataset_id =', dataset_id_categorical_field_add, 'and standardizing_module_type = 3')
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select a Categorical Source Field to Add Values To:")

    } else {
      # Return an empty table or a message indicating no row is selected
      datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
    }
  })

  output$categorical_source_fields_to_add_curr <- renderDataTable({
    if (!is.null(input$categorical_source_fields_to_add_rows_selected)) {
      # Next, limit by the previous query
      selected_row <- input$categorical_source_fields_to_add_rows_selected
      query <- paste('SELECT dataset_id, source_field_id, source_field_name  FROM source_fields sf',
                      'JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                      'where dataset_id =', dataset_id_categorical_field_add, 'and standardizing_module_type = 3')
      df <- dbGetQuery(metadata_connection, query)
      source_id_categorical_field_add <<- df[selected_row, "source_field_id"]

      # Finally, get which categorical field we will be adding a new value to!
      if(is.na(source_id_categorical_field_add)){
        return()
      }
      query <- paste('SELECT source_field_id, source_value, standardized_value FROM categorical_fields cf',
                     'JOIN categorical_values cv on cf.standardized_value_id = cv.standardized_value_id',
                     'where source_field_id = ', source_id_categorical_field_add)
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Current Categorical Values of Selected Source Field:")
    } else {
      # Return an empty table or a message indicating no row is selected
      datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
    }
  })

  output$categorical_fields_to_add <- renderUI({

    num_fields <- input$number_of_categorical_fields_to_add

    if(is.nan(num_fields) || is.na(num_fields) || num_fields == 0){
      showNotification("Error - Invalid Number of Categorical Fields", type = "error", closeButton = FALSE)
      return()
    }

    separator_inputs_list <- lapply(1:(num_fields), function(i) {
      query_result <- dbGetQuery(metadata_connection, "SELECT * FROM categorical_values")

      # Extract columns from query result
      choices <- setNames(query_result$standardized_value_id, query_result$standardized_value)

      fluidRow(
        column(width = 6, textInput(inputId = paste0("categorical_field_input_value_", i),
                                    label = paste0("Categorical Value ", i, ":"),
                                    value = "")),
        column(width = 6, selectInput(inputId = paste0("categorical_field_output_value_", i),
                                      label = paste0("Standardized Categorical Value ", i, ":"),
                                      choices = choices,
                                      selected = NULL))
      )
    })
  })

  observeEvent(input$add_new_categorical_fields, {
    num_categorical_fields <- input$number_of_categorical_fields_to_add

    # Error handling
    #----#
    if(is.nan(num_categorical_fields) || is.na(num_categorical_fields) || num_categorical_fields == 0){
      showNotification("Error - Invalid Number of Categorical Fields", type = "error", closeButton = FALSE)
      return()
    }

    for(i in seq_len(num_categorical_fields)){
      categorical_input_value_id <- paste0("categorical_field_input_value_", i)
      categorical_input_value <- input[[categorical_input_value_id]]

      categorical_standardized_value_id <- paste0("categorical_field_output_value_", i)
      categorical_standardized_value <- input[[categorical_standardized_value_id]]

      if(categorical_input_value == ""){
        showNotification("Failed to Add to Categorical Field - Some Categorical Inputs are Missing", type = "error", closeButton = FALSE)
        return()
      }
    }
    #----#

    for(i in seq_len(num_categorical_fields)){
      categorical_input_value_id <- paste0("categorical_field_input_value_", i)
      categorical_input_value <- input[[categorical_input_value_id]]

      categorical_standardized_value_id <- paste0("categorical_field_output_value_", i)
      categorical_standardized_value <- input[[categorical_standardized_value_id]]

      # Create a query for inserting a new categorical field into categorical_fields
      new_entry_query <- paste("INSERT INTO categorical_fields (source_field_id, source_value, standardized_value_id)",
                               "VALUES(?, ?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(source_id_categorical_field_add, categorical_input_value, categorical_standardized_value))
      dbClearResult(new_entry)
    }

    # Re-render table
    output$categorical_source_fields_to_add_curr <- renderDataTable({
      if (!is.null(input$categorical_source_fields_to_add_rows_selected)) {
        # Next, limit by the previous query
        selected_row <- input$categorical_source_fields_to_add_rows_selected
        query <- paste('SELECT dataset_id, source_field_id, source_field_name  FROM source_fields sf',
                       'JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                       'where dataset_id =', dataset_id_categorical_field_add, 'and standardizing_module_type = 3')
        df <- dbGetQuery(metadata_connection, query)
        source_id_categorical_field_add <<- df[selected_row, "source_field_id"]

        # Finally, get which categorical field we will be adding a new value to!
        if(is.na(source_id_categorical_field_add)){
          return()
        }
        query <- paste('SELECT source_field_id, source_value, standardized_value FROM categorical_fields cf',
                       'JOIN categorical_values cv on cf.standardized_value_id = cv.standardized_value_id',
                       'where source_field_id = ', source_id_categorical_field_add)
        df <- dbGetQuery(metadata_connection, query)
        datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE),
                  caption = "Current Categorical Values of Selected Source Field:")
      } else {
        # Return an empty table or a message indicating no row is selected
        datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
      }
    })

    updateNumericInput(session, "number_of_categorical_fields_to_add", value = 2)
    updateNumericInput(session, "number_of_categorical_fields_to_add", value = 1)

    showNotification("Categorical Field Successfully Added", type = "message", closeButton = FALSE)
  })
  #----

  #-- Add to Categorical Values --#
  #----
  output$curr_categorical_values_add <- renderDataTable({
    df <- dbGetQuery(metadata_connection, paste('SELECT * from categorical_values'))
    datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Current Standardized Categorical Values:")
  })

  observeEvent(input$add_new_categorical_values, {
    new_categorical_val <- input$new_categorical_value

    if(!is.null(new_categorical_val) && new_categorical_val != ""){
      # Create a query for inserting a new categorical field into categorical_fields
      new_entry_query <- paste("INSERT INTO categorical_values (standardized_value)",
                               "VALUES(?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(new_categorical_val))
      dbClearResult(new_entry)
    }
    else{
      showNotification("Failed to Add Categorical Value - Invalid Input", type = "message", closeButton = FALSE)
      return()
    }

    output$curr_categorical_values_add <- renderDataTable({
      df <- dbGetQuery(metadata_connection, paste('SELECT * from categorical_values'))
      datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Current Standardized Categorical Values:")
    })

    showNotification("Categorical Value Successfully Added", type = "message", closeButton = FALSE)
  })
  #----

  #-- Add to Numeric Date Formats --#
  #----
  output$created_new_numeric_format_new <- renderUI({
    div(
      style = "display: none;",
      selectInput("create_new_numeric_format_new",
                  choices = c("no" = 0,
                              "yes" = 1),
                  selected = 0,
                  label = ""),
    )
  })

  output$created_new_numeric_format_update <- renderUI({
    div(
      style = "display: none;",
      selectInput("create_new_numeric_format_update",
                  choices = c("no" = 0,
                              "yes" = 1),
                  selected = 0,
                  label = ""),
    )
  })

  output$add_numeric_date_formats <- renderDataTable({
    df <- dbGetQuery(metadata_connection, 'SELECT * from numeric_date_formats')
    datatable(df, selection = "none", rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Current Numeric Date Formats:")
  })

  observeEvent(input$submit_new_numeric_date_format, {
    format_label <- input$new_numeric_format_label
    origin_date  <- as.character(input$new_origin_date)
    units_label  <- input$new_unit_label

    # Error Handling
    #----#
    if(is.na(format_label) || is.null(format_label) || format_label == ""){
      showNotification("Adding Numeric Format Failed - Invalid Format Label", type = "error", closeButton = FALSE)
      return()
    }

    if(identical(origin_date, character(0)) || is.na(origin_date) || is.null(origin_date) || origin_date == ""){
      showNotification("Adding Numeric Format Failed - Invalid Origin Date", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    # Create a query for inserting a new numeric format
    new_entry_query <- paste("INSERT INTO numeric_date_formats (numeric_date_format_label, origin_date, units_label)",
                             "VALUES(?, ?, ?);")
    new_entry <- dbSendQuery(metadata_connection, new_entry_query)
    dbBind(new_entry, list(format_label, origin_date, units_label))
    dbClearResult(new_entry)

    # Re-render tables
    rerender_data_tables_manage_datasets()

    # Reset inputs
    updateTextAreaInput(session, "new_numeric_format_label", value = "")
    updateDateInput(session, "new_origin_date", value = NULL)
    updateSelectInput(session, "new_unit_label", selected = NULL)

    if(input$create_new_numeric_format_new == 1){
      updateSelectInput(session, "table_to_add_to", selected = "source_fields")
      updateSelectInput(session, "create_new_numeric_format_new", selected = 0)
    }
    else if (input$create_new_numeric_format_update == 1){
      updateSelectInput(session, "update_type", selected = 2)
      updateSelectInput(session, "table_to_update", selected = "source_fields_update")
      updateSelectInput(session, "create_new_numeric_format_update", selected = 0)
    }

    showNotification("Numeric Format Successfully Added", type = "message", closeButton = FALSE)
  })
  #----

  #-- Add to Record Priorities --#
  #----
  dataset_id_record_priority_add <- 0
  source_id_record_priority_add  <- 0

  output$record_priority_datasets <- renderDataTable({
    df <- dbGetQuery(metadata_connection, paste("SELECT dataset_code, dataset_name FROM datasets ORDER BY dataset_id ASC"))
    datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select A Table to Add Record Priorities To:")
  })

  output$record_priority_fields_to_add <- renderDataTable({
    if (!is.null(input$record_priority_datasets_rows_selected)) {
      selected_row <- input$record_priority_datasets_rows_selected
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets ORDER BY dataset_id ASC'))

      selected_dataset_id <- df[selected_row, "dataset_id"]
      dataset_id_record_priority_add <<- selected_dataset_id

      query <- paste('SELECT source_field_id, source_field_name  FROM source_fields sf',
                     'JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                     'where dataset_id =', dataset_id_record_priority_add, 'and standardizing_module_type = 5')
      df <- dbGetQuery(metadata_connection, query)

      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select a Record Priority Field to Add Extra Priorities To:")

    } else {
      # Return an empty table or a message indicating no row is selected
      datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
    }
  })

  output$record_priority_fields_to_add_curr <- renderDataTable({
    if (!is.null(input$record_priority_fields_to_add_rows_selected)) {
      # Next, limit by the previous query
      selected_row <- input$record_priority_fields_to_add_rows_selected
      query <- paste('SELECT source_field_id, source_field_name  FROM source_fields sf',
                     'JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                     'where dataset_id =', dataset_id_record_priority_add, 'and standardizing_module_type = 5')
      df <- dbGetQuery(metadata_connection, query)
      source_id_record_priority_add <<- df[selected_row, "source_field_id"]

      # Finally, get which categorical field we will be adding a new value to!
      if(is.na(source_id_record_priority_add)){
        return()
      }
      query <- paste('SELECT * FROM record_priority_fields where source_field_id = ', source_id_record_priority_add, 'ORDER BY priority ASC')
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Current Record Priorities of Selected Source Field:")
    } else {
      # Return an empty table or a message indicating no row is selected
      datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
    }
  })

  output$record_priorities_to_add <- renderUI({
    num_fields <- input$number_of_record_priorities_to_add

    if(is.nan(num_fields) || is.na(num_fields) || num_fields == 0){
      showNotification("Error - Invalid Number of Record Priorities", type = "error", closeButton = FALSE)
      return()
    }

    priority_inputs_list <- lapply(1:(num_fields), function(i) {

      fluidRow(
        column(width = 6, textInput(inputId = paste0("record_priority_field_input_value_", i),
                                    label = paste0("Source Field Value ", i, ":"),
                                    value = ""), align = "right"),
        column(width = 6, numericInput(inputId = paste0("record_priority_field_output_value_", i),
                                       label = paste0("Record Priority ", i, ":"),
                                       value = NULL), align = "left")
      )
    })
  })

  observeEvent(input$add_new_record_priorities, {
    num_record_priority_fields <- input$number_of_record_priorities_to_add

    # Error handling
    #----#
    if(is.nan(num_record_priority_fields) || is.na(num_record_priority_fields) || num_record_priority_fields == 0){
      showNotification("Error - Invalid Number of Record Priority Fields", type = "error", closeButton = FALSE)
      return()
    }

    for(i in seq_len(num_record_priority_fields)){
      categorical_input_value_id <- paste0("record_priority_field_input_value_", i)
      categorical_input_value <- input[[categorical_input_value_id]]

      categorical_standardized_value_id <- paste0("record_priority_field_output_value_", i)
      categorical_standardized_value <- input[[categorical_standardized_value_id]]

      if(categorical_input_value == "" || is.na(categorical_standardized_value) || categorical_standardized_value <= 0){
        showNotification("Failed to Add Record Priorities - Some Record Priority Inputs are Missing", type = "error", closeButton = FALSE)
        return()
      }
    }
    #----#

    for(i in seq_len(num_record_priority_fields)){
      categorical_input_value_id <- paste0("record_priority_field_input_value_", i)
      categorical_input_value <- input[[categorical_input_value_id]]

      categorical_standardized_value_id <- paste0("record_priority_field_output_value_", i)
      categorical_standardized_value <- input[[categorical_standardized_value_id]]

      # Create a query for inserting a new categorical field into categorical_fields
      new_entry_query <- paste("INSERT INTO record_priority_fields (source_field_id, source_value, priority)",
                               "VALUES(?, ?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(source_id_record_priority_add, categorical_input_value, categorical_standardized_value))
      dbClearResult(new_entry)
    }

    # Re-render tables
    output$record_priority_fields_to_add_curr <- renderDataTable({
      if (!is.null(input$record_priority_fields_to_add_rows_selected)) {
        # Next, limit by the previous query
        selected_row <- input$record_priority_fields_to_add_rows_selected
        query <- paste('SELECT source_field_id, source_field_name  FROM source_fields sf',
                       'JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                       'where dataset_id =', dataset_id_record_priority_add, 'and standardizing_module_type = 5')
        df <- dbGetQuery(metadata_connection, query)
        source_id_record_priority_add <<- df[selected_row, "source_field_id"]

        # Finally, get which categorical field we will be adding a new value to!
        if(is.na(source_id_record_priority_add)){
          return()
        }
        query <- paste('SELECT * FROM record_priority_fields where source_field_id = ', source_id_record_priority_add, 'ORDER BY priority ASC')
        df <- dbGetQuery(metadata_connection, query)
        datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE),
                  caption = "Current Record Priorities of Selected Source Field:")
      } else {
        # Return an empty table or a message indicating no row is selected
        datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
      }
    })

    updateNumericInput(session, "number_of_record_priorities_to_add", value = 2)
    updateNumericInput(session, "number_of_record_priorities_to_add", value = 1)

    showNotification("Record Priorities Successfully Added", type = "message", closeButton = FALSE)
  })
  #----

  #-- Update Source Fields --#
  #----
  # Source Field Variables
  update_source_field_ds_id <- 0
  update_source_field_sf_id <- 0
  update_source_field_fo    <- 0

  output$source_field_to_update_datasets <- renderDataTable({
    df <- dbGetQuery(metadata_connection, paste('SELECT dataset_code, dataset_name FROM datasets'))
    datatable(df, selection = "single", rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select the Dataset Containing the Source Fields to be Updated:")
  })

  output$source_field_to_update <- renderDataTable({
    if (!is.null(input$source_field_to_update_datasets_rows_selected)) {
      selected_row <- input$source_field_to_update_datasets_rows_selected
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))
      selected_dataset_id <- df[selected_row, "dataset_id"]
      update_source_field_ds_id <<- selected_dataset_id
      query <- paste('SELECT source_field_id, source_field_name, field_order, fixed_width_length, standardizing_module_name FROM source_fields sf',
                     'LEFT JOIN standardizing_modules sm on sm.standardizing_module_id = sf.standardizing_module_id',
                     'where dataset_id = ', update_source_field_ds_id,
                     "ORDER by field_order asc")
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select a Source Field to Update:")
    } else {
      # Return an empty table or a message indicating no row is selected
      datatable(data.frame(), options = list(searching = FALSE, paging = FALSE, ordering = FALSE))
    }
  })

  output$updated_compound_format_type <- renderDataTable({
    # Fetch data from the database
    df <- dbGetQuery(metadata_connection, 'SELECT * FROM compound_field_formats')

    # Render the table with single row selection
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select A Compound Format To Use OR Create A New Format Using The Button")
  })

  output$numeric_format_types_update <- renderDataTable({
    # Fetch data from the database
    df <- dbGetQuery(metadata_connection, 'SELECT * FROM numeric_date_formats')

    # Render the table with single row selection
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select A Numeric Format To Use OR Create A New Format Using The Button")
  })

  output$select_updated_standardizing_module <- renderUI({
    # Perform query using metadata_connection
    query_result <- dbGetQuery(metadata_connection, "SELECT standardizing_module_name, standardizing_module_id FROM standardizing_modules")

    # Extract columns from query result
    choices <- setNames(query_result$standardizing_module_id, query_result$standardizing_module_name)

    # Add the additional look up value where the first choice is 0
    choices <- c("Not Applicable" = "null", choices)

    # Create select input with dynamic choices
    return(span(selectInput("selected_updated_standardizing_module", label = "Select Standardizing Module:", choices = choices, width = validateCssUnit("20vw"))))
  })

  output$standardizing_module_description_update <- renderPrint({
    selected_module_id <- input$selected_updated_standardizing_module

    if(!is.null(selected_module_id) && selected_module_id != "null"){
      standardizing_type_query <- paste0("SELECT standardizing_module_name, description FROM standardizing_modules WHERE standardizing_module_id = ", selected_module_id)
      df <- dbGetQuery(metadata_connection, standardizing_type_query)

      module_name <- df$standardizing_module_name
      description <- df$description

      div(
        style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px;",
        HTML(paste0("<strong>", module_name, "</strong>: ", description))
      )
    }
    else{
      div(
        style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px;",
        HTML(paste0("<strong>Not Applicable</strong>: Not Applicable fields are ignored when processing linkage ",
        "fields and are only including in the health and program data afterwards."))
      )
    }
  })

  output$standardizing_module_type_update <- renderUI({
    div(
      style = "display: none;",
      selectInput("standardizing_type_update_source_field",
                  choices = c("health/program" = "", "normal" = 1,
                              "compound" = 2,
                              "categorical" = 3,
                              "numerical" = 4,
                              "priority" = 5),
                  selected = "null",
                  label = "")
    )
  })

  observeEvent(input$selected_updated_standardizing_module, {

    # Get the selected standardizing module ID
    selected_module_id <- input$selected_updated_standardizing_module

    # Check if a standardizing module is selected
    if (!is.null(selected_module_id) && selected_module_id != "null") {
      # Perform another query to check for standardizing type using selected_module_id
      standardizing_type_query <- paste0("SELECT standardizing_module_type FROM standardizing_modules WHERE standardizing_module_id = ", selected_module_id)
      standardizing_type <- dbGetQuery(metadata_connection, standardizing_type_query)$standardizing_module_type

      # Update the choices in standardizing_type_update_source_field based on standardizing type
      updateSelectInput(session, "standardizing_type_update_source_field", selected = standardizing_type)
    }
    else{
      updateSelectInput(session, "standardizing_type_update_source_field", selected = "")
    }
  })

  observeEvent(input$source_field_to_update_rows_selected, {
    selected_row <- input$source_field_to_update_rows_selected

    if (!is.null(selected_row)) {
      query <- paste('SELECT * FROM source_fields where dataset_id = ', update_source_field_ds_id,
                     'ORDER by field_order asc')
      df <- dbGetQuery(metadata_connection, query)
      # Retrieve the chosen_source_field_id of the selected row
      selected_source_field_id     <- df[selected_row, "source_field_id"]
      selected_source_field_name   <- df[selected_row, "source_field_name"]
      selected_field_order         <- df[selected_row, "field_order"]
      selected_fixed_width         <- df[selected_row, "fixed_width_length"]
      selected_standardizing_field <- df[selected_row, "standardizing_module_id"]

      if(is.na(selected_standardizing_field) || selected_standardizing_field == ""){
        selected_standardizing_field <- "null"
      }

      update_source_field_sf_id <<- selected_source_field_id
      update_source_field_fo    <<- selected_field_order

      # Update/pre-populate the input fields
      updateTextAreaInput(session, "updated_source_field_name",           value = selected_source_field_name)
      output$updated_numeric_field_order <- renderUI({
        numericInput("updated_field_order", label = "Field Order:", value = selected_field_order, width = validateCssUnit("20vw"))
      })
      updateNumericInput(session, "updated_fixed_width_length",           value = selected_fixed_width)
      updateSelectInput(session, "selected_updated_standardizing_module", selected = selected_standardizing_field)
    }
  })

  output$updated_categorical_inputs <- renderUI({
    num_fields <- input$updated_number_of_categorical_fields

    if(is.nan(num_fields) || is.na(num_fields) || num_fields <= 0){
      showNotification("Error - Invalid Number of Categorical Fields", type = "error", closeButton = FALSE)
      return()
    }

    separator_inputs_list <- lapply(1:(num_fields), function(i) {
      query_result <- dbGetQuery(metadata_connection, "SELECT * FROM categorical_values")

      # Extract columns from query result
      choices <- setNames(query_result$standardized_value_id, query_result$standardized_value)

      fluidRow(
        column(width = 6, textInput(inputId = paste0("updated_categorical_input_value_", i, ":"),
                                    label = paste0("Source Categorical Value ", i),
                                    value = ""), align = "right"),
        column(width = 6, selectInput(inputId = paste0("updated_categorical_output_value_", i, ":"),
                                      label = paste0("Standardized Categorical Value ", i, ":"),
                                      choices = choices,
                                      selected = NULL), align = "left")
      )
    })
  })

  output$updated_record_priority_inputs <- renderUI({
    num_fields <- input$updated_number_of_record_priority_fields

    if(is.nan(num_fields) || is.na(num_fields) || num_fields <= 0){
      showNotification("Error - Invalid Number of Record Priority Fields", type = "error", closeButton = FALSE)
      return()
    }

    separator_inputs_list <- lapply(1:(num_fields), function(i) {

      fluidRow(
        column(width = 6, textInput(inputId = paste0("updated_categorical_input_value_", i),
                                    label = paste0("Source Field Value ", i, ":"),
                                    value = ""), align = "right"),
        column(width = 6, numericInput(inputId = paste0("updated_categorical_output_value_", i),
                                      label = paste0("Record Priority ", i, ":"),
                                      value = NULL), align = "left")
      )
    })
  })

  observeEvent(input$update_source_field, {
    dataset_id                 <- update_source_field_ds_id
    source_field_id_to_update  <- update_source_field_sf_id
    old_field_order            <- update_source_field_fo
    new_source_field_name      <- input$updated_source_field_name
    new_field_order            <- input$updated_field_order
    new_fixed_width_length     <- input$updated_fixed_width_length
    new_standardizing_module   <- input$selected_updated_standardizing_module
    if(new_standardizing_module == "null")
      new_standardizing_module <- NA
    standardizing_module_type   <- input$standardizing_type_update_source_field
    compound_field_selected_row <- input$updated_compound_format_type_rows_selected
    numeric_format_selected_row <- input$numeric_format_types_update_rows_selected

    # Error handling
    #----#
    if(new_field_order <= 0){
      showNotification("Failed to Update Source Field - Invalid Field Order", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    if(!is.null(new_standardizing_module) && standardizing_module_type == 2){

      # Fetch data from the database
      df <- dbGetQuery(metadata_connection, 'SELECT * FROM compound_field_formats')

      # Using the selected compound format row, determine whether we need to make
      # a new format
      selected_row <- input$updated_compound_format_type_rows_selected
      compound_format_id <- df[selected_row, "compound_field_format_id"]

      # Branch for if we need to create a new compound format for the updated source field
      if(is.na(compound_format_id)){
        num_separators     <- input$updated_number_of_separators
        format_description <- input$updated_compound_format_description
        format             <- input$updated_compound_format
        separators <- c()
        # indexes <- c()
        destination_field_ids <- c()
        for(i in seq_len(num_separators + 1)){
          if(i <= num_separators){
            separator_input_id <- paste0("updated_separator_", i)
            separator_value <- input[[separator_input_id]]
            separators <- c(separators, separator_value)
          }

          destination_field_input_id <- paste0("updated_separator_destination_field_", i)
          destination_field_value <- input[[destination_field_input_id]]
          destination_field_ids <- c(destination_field_ids, destination_field_value)
        }

        # Create a query for inserting a new compound format into compound_field_formats
        new_entry_query <- paste("INSERT INTO compound_field_formats (compound_format, format_description)",
                                 "VALUES(?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(format, format_description))
        dbClearResult(new_entry)

        # Get the most recently inserted compound_field_format_id value
        compound_field_format_id <- dbGetQuery(metadata_connection, "SELECT last_insert_rowid() AS compound_field_format_id;")$compound_field_format_id

        # Loop for adding the separators into the metadata
        for(i in seq_along(separators)){
          # Create a query for inserting a new compound separator entry into compound_field_separators
          new_entry_query <- paste("INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)",
                                   "VALUES(?, ?, ?, ?);")
          new_entry <- dbSendQuery(metadata_connection, new_entry_query)
          dbBind(new_entry, list(compound_field_format_id, i, separators[i], NA)) #NA for now, change it later!
          dbClearResult(new_entry)
        }

        # Loop for adding the compound field destinations into the metadata
        for(i in seq_along(destination_field_ids)){
          # Do we want this to fill it in with an NA incase of "null"? We can decide later
          if(destination_field_ids[i] != "null"){
            # Create a query for inserting a new compound separator entry into compound_field_destinations
            new_entry_query <- paste("INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)",
                                     "VALUES(?, ?, ?);")
            new_entry <- dbSendQuery(metadata_connection, new_entry_query)
            dbBind(new_entry, list(compound_field_format_id, i, destination_field_ids[i]))
            dbClearResult(new_entry)
          }
        }

        #Update field order of the source field and update the old one if necessary
        update_query <- paste("UPDATE source_fields SET field_order = ? where field_order = ? and dataset_id = ?")
        update_statement <- dbSendQuery(metadata_connection, update_query)
        dbBind(update_statement, list(old_field_order, new_field_order, dataset_id))
        dbClearResult(update_statement)


        # Create a query for updating the source field
        update_query <- paste("UPDATE source_fields
                              SET source_field_name = ?, field_order = ?, fixed_width_length = ?, standardizing_module_id = ?
                              WHERE source_field_id = ?")
        update <- dbSendQuery(metadata_connection, update_query)
        dbBind(update, list(new_source_field_name, new_field_order, new_fixed_width_length, new_standardizing_module, source_field_id_to_update))
        dbClearResult(update)

        # Create a query for deleting the records that use the same source field in
        # other tables
        delete_query <- paste("DELETE from compound_fields",
                              "WHERE source_field_id = ?")
        delete <- dbSendQuery(metadata_connection, delete_query)
        dbBind(delete, list(source_field_id_to_update))
        dbClearResult(delete)

        delete_query <- paste("DELETE from categorical_fields",
                              "WHERE source_field_id = ?")
        delete <- dbSendQuery(metadata_connection, delete_query)
        dbBind(delete, list(source_field_id_to_update))
        dbClearResult(delete)

        delete_query <- paste("DELETE from numeric_date_fields",
                              "WHERE source_field_id = ?")
        delete <- dbSendQuery(metadata_connection, delete_query)
        dbBind(delete, list(source_field_id_to_update))
        dbClearResult(delete)

        delete_query <- paste("DELETE from record_priority_fields",
                              "WHERE source_field_id = ?")
        delete <- dbSendQuery(metadata_connection, delete_query)
        dbBind(delete, list(source_field_id_to_update))
        dbClearResult(delete)

        # Create a query for inserting a new compound field into compound_fields
        new_entry_query <- paste("INSERT INTO compound_fields (source_field_id, compound_field_format_id)",
                                 "VALUES(?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(source_field_id_to_update, compound_field_format_id))
        dbClearResult(new_entry)
      }
      # Branch for if we are using an existing compound format for this source field.
      else{
        #Update field order of the source field and update the old one if necessary
        update_query <- paste("UPDATE source_fields SET field_order = ? where field_order = ? and dataset_id = ?")
        update_statement <- dbSendQuery(metadata_connection, update_query)
        dbBind(update_statement, list(old_field_order, new_field_order, dataset_id))
        dbClearResult(update_statement)

        # Create a query for updating the source field
        update_query <- paste("UPDATE source_fields
                              SET source_field_name = ?, field_order = ?, fixed_width_length = ?, standardizing_module_id = ?
                              WHERE source_field_id = ?")
        update <- dbSendQuery(metadata_connection, update_query)
        dbBind(update, list(new_source_field_name, new_field_order, new_fixed_width_length, new_standardizing_module, source_field_id_to_update))
        dbClearResult(update)

        # Create a query for deleting the records that use the same source field in
        # other tables
        delete_query <- paste("DELETE from compound_fields",
                              "WHERE source_field_id = ?")
        delete <- dbSendQuery(metadata_connection, delete_query)
        dbBind(delete, list(source_field_id_to_update))
        dbClearResult(delete)

        delete_query <- paste("DELETE from categorical_fields",
                              "WHERE source_field_id = ?")
        delete <- dbSendQuery(metadata_connection, delete_query)
        dbBind(delete, list(source_field_id_to_update))
        dbClearResult(delete)

        delete_query <- paste("DELETE from numeric_date_fields",
                              "WHERE source_field_id = ?")
        delete <- dbSendQuery(metadata_connection, delete_query)
        dbBind(delete, list(source_field_id_to_update))
        dbClearResult(delete)

        delete_query <- paste("DELETE from record_priority_fields",
                              "WHERE source_field_id = ?")
        delete <- dbSendQuery(metadata_connection, delete_query)
        dbBind(delete, list(source_field_id_to_update))
        dbClearResult(delete)

        # Create a query for inserting a new compound field into compound_fields
        new_entry_query <- paste("INSERT INTO compound_fields (source_field_id, compound_field_format_id)",
                                 "VALUES(?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(source_field_id_to_update, compound_format_id))
        dbClearResult(new_entry)
      }
    }
    else if (!is.null(new_standardizing_module) && standardizing_module_type == 3){
      num_categorical_fields <- input$updated_number_of_categorical_fields

      # Error handling to ensure adding a source field goes smoothly
      #----#
      if(is.nan(num_categorical_fields) || is.na(num_categorical_fields) || num_categorical_fields == 0){
        showNotification("Error - Invalid Number of Categorical Fields", type = "error", closeButton = FALSE)
        return()
      }

      for(i in seq_len(num_categorical_fields)){
        categorical_input_value_id <- paste0("updated_categorical_input_value_", i)
        categorical_input_value <- input[[categorical_input_value_id]]

        categorical_standardized_value_id <- paste0("updated_categorical_output_value_", i)
        categorical_standardized_value <- input[[categorical_standardized_value_id]]

        if(categorical_input_value == ""){
          showNotification("Failed to Update Source Field - Some Categorical Inputs are Missing", type = "error", closeButton = FALSE)
          return()
        }
      }
      #----#

      #Update field order of the source field and update the old one if necessary
      update_query <- paste("UPDATE source_fields SET field_order = ? where field_order = ? and dataset_id = ?")
      update_statement <- dbSendQuery(metadata_connection, update_query)
      dbBind(update_statement, list(old_field_order, new_field_order, dataset_id))
      dbClearResult(update_statement)

      # Create a query for updating the source field
      update_query <- paste("UPDATE source_fields
                              SET source_field_name = ?, field_order = ?, fixed_width_length = ?, standardizing_module_id = ?
                              WHERE source_field_id = ?")
      update <- dbSendQuery(metadata_connection, update_query)
      dbBind(update, list(new_source_field_name, new_field_order, new_fixed_width_length, new_standardizing_module, source_field_id_to_update))
      dbClearResult(update)

      # Create a query for deleting the records that use the same source field in
      # other tables
      delete_query <- paste("DELETE from compound_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from categorical_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from numeric_date_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from record_priority_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      for(i in seq_len(num_categorical_fields)){
        categorical_input_value_id <- paste0("updated_categorical_input_value_", i)
        categorical_input_value <- input[[categorical_input_value_id]]

        categorical_standardized_value_id <- paste0("updated_categorical_output_value_", i)
        categorical_standardized_value <- input[[categorical_standardized_value_id]]

        # Create a query for inserting a new categorical field into categorical_fields
        new_entry_query <- paste("INSERT INTO categorical_fields (source_field_id, source_value, standardized_value_id)",
                                 "VALUES(?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(source_field_id_to_update, categorical_input_value, categorical_standardized_value))
        dbClearResult(new_entry)
      }
    }
    else if (!is.null(new_standardizing_module) && standardizing_module_type == 4){

      #Update field order of the source field and update the old one if necessary
      update_query <- paste("UPDATE source_fields SET field_order = ? where field_order = ? and dataset_id = ?")
      update_statement <- dbSendQuery(metadata_connection, update_query)
      dbBind(update_statement, list(old_field_order, new_field_order, dataset_id))
      dbClearResult(update_statement)

      # Create a query for updating the source field
      update_query <- paste("UPDATE source_fields
                              SET source_field_name = ?, field_order = ?, fixed_width_length = ?, standardizing_module_id = ?
                              WHERE source_field_id = ?")
      update <- dbSendQuery(metadata_connection, update_query)
      dbBind(update, list(new_source_field_name, new_field_order, new_fixed_width_length, new_standardizing_module, source_field_id_to_update))
      dbClearResult(update)

      # Get the input variables
      df <- dbGetQuery(metadata_connection, 'SELECT * FROM numeric_date_formats')
      numeric_format_id <- df[numeric_format_selected_row, "numeric_date_format_id"]
      numeric_dest_type <- input$numeric_destination_type_new_sf

      # Create a query for deleting the records that use the same source field in
      # other tables
      delete_query <- paste("DELETE from compound_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from categorical_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from numeric_date_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from record_priority_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      # Create a query for inserting a new compound field into compound_fields
      new_entry_query <- paste("INSERT INTO numeric_date_fields (source_field_id, numeric_date_format_id, numeric_destination_type)",
                               "VALUES(?, ?, ?);")
      new_entry <- dbSendQuery(metadata_connection, new_entry_query)
      dbBind(new_entry, list(source_field_id_to_update, numeric_format_id, numeric_dest_type))
      dbClearResult(new_entry)
    }
    else if (!is.null(new_standardizing_module) && standardizing_module_type == 5){
      num_categorical_fields <- input$updated_number_of_record_priority_fields

      # Error handling to ensure adding a source field goes smoothly
      #----#
      if(is.nan(num_categorical_fields) || is.na(num_categorical_fields) || num_categorical_fields == 0){
        showNotification("Error - Invalid Number of Record Priority Fields", type = "error", closeButton = FALSE)
        return()
      }

      for(i in seq_len(num_categorical_fields)){
        categorical_input_value_id <- paste0("updated_categorical_input_value_", i)
        categorical_input_value <- input[[categorical_input_value_id]]

        categorical_standardized_value_id <- paste0("updated_categorical_output_value_", i)
        categorical_standardized_value <- input[[categorical_standardized_value_id]]

        if(categorical_input_value == "" || is.na(categorical_standardized_value)){
          showNotification("Failed to Update Source Field - Some Record Priority Inputs are Missing", type = "error", closeButton = FALSE)
          return()
        }
      }
      #----#

      #Update field order of the source field and update the old one if necessary
      update_query <- paste("UPDATE source_fields SET field_order = ? where field_order = ? and dataset_id = ?")
      update_statement <- dbSendQuery(metadata_connection, update_query)
      dbBind(update_statement, list(old_field_order, new_field_order, dataset_id))
      dbClearResult(update_statement)

      # Create a query for updating the source field
      update_query <- paste("UPDATE source_fields
                              SET source_field_name = ?, field_order = ?, fixed_width_length = ?, standardizing_module_id = ?
                              WHERE source_field_id = ?")
      update <- dbSendQuery(metadata_connection, update_query)
      dbBind(update, list(new_source_field_name, new_field_order, new_fixed_width_length, new_standardizing_module, source_field_id_to_update))
      dbClearResult(update)

      # Create a query for deleting the records that use the same source field in
      # other tables
      delete_query <- paste("DELETE from compound_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from categorical_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from numeric_date_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from record_priority_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      for(i in seq_len(num_categorical_fields)){
        categorical_input_value_id <- paste0("updated_categorical_input_value_", i)
        categorical_input_value <- input[[categorical_input_value_id]]

        categorical_standardized_value_id <- paste0("updated_categorical_output_value_", i)
        categorical_standardized_value <- input[[categorical_standardized_value_id]]

        # Create a query for inserting a new categorical field into categorical_fields
        new_entry_query <- paste("INSERT INTO record_priority_fields (source_field_id, source_value, priority)",
                                 "VALUES(?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(source_field_id_to_update, categorical_input_value, categorical_standardized_value))
        dbClearResult(new_entry)
      }
    }
    else if (!is.null(new_standardizing_module)){
      # Basic create statement that doesn't have any other tables that use the
      # source field id in it.

      # Create a query for deleting the records that use the same source field in
      # other tables
      delete_query <- paste("DELETE from compound_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from categorical_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from numeric_date_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      delete_query <- paste("DELETE from record_priority_fields",
                            "WHERE source_field_id = ?")
      delete <- dbSendQuery(metadata_connection, delete_query)
      dbBind(delete, list(source_field_id_to_update))
      dbClearResult(delete)

      #Update field order of the source field and update the old one if necessary
      update_query <- paste("UPDATE source_fields SET field_order = ? where field_order = ? and dataset_id = ?")
      update_statement <- dbSendQuery(metadata_connection, update_query)
      dbBind(update_statement, list(old_field_order, new_field_order, dataset_id))
      dbClearResult(update_statement)

      # Create a query for updating the source field
      update_query <- paste("UPDATE source_fields
                              SET source_field_name = ?, field_order = ?, fixed_width_length = ?, standardizing_module_id = ?
                              WHERE source_field_id = ?")
      update <- dbSendQuery(metadata_connection, update_query)
      dbBind(update, list(new_source_field_name, new_field_order, new_fixed_width_length, new_standardizing_module, source_field_id_to_update))
      dbClearResult(update)
    }

    # Re-render the data tables
    rerender_data_tables_manage_datasets()

    # Update the widgets on this page
    updateTextInput(session, "updated_source_field_name", value = "")
    updateNumericInput(session, "updated_field_order", value = 1)
    updateNumericInput(session, "updated_fixed_width_length", value = "")
    updateSelectInput(session, "selected_updated_standardizing_module", selected = "null")

    showNotification("Source Field Successfully Updated", type = "message", closeButton = FALSE)

  })

  # This observe is meant to deal with manual field order entries, so that they can't
  # go over the max value
  observe({
    field_order_update <- input$updated_field_order
    field_order_create <- input$new_field_order
    selected_row_update <- input$source_field_to_update_datasets_rows_selected
    selected_row_create <- input$dataset_to_update_rows_selected

    if(!is.null(field_order_update) && !is.na(field_order_update) && !is.null(selected_row_update)){
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))
      selected_dataset_id <- df[selected_row_update, "dataset_id"]
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM source_fields WHERE dataset_id =', selected_dataset_id))
    }
    if(!is.null(field_order_create) && !is.na(field_order_create) && !is.null(selected_row_create)){
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM datasets'))
      selected_dataset_id <- df[selected_row_create, "dataset_id"]
      df <- dbGetQuery(metadata_connection, paste('SELECT * FROM source_fields WHERE dataset_id =', selected_dataset_id))
    }
  })

  # User wants to create a new compound format when updating a source field
  observeEvent(input$add_compound_format_for_updated_sf, {
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "compound_field_formats")
    updateSelectInput(session, "create_new_compound_format_update_sf", selected = 1)
    updateSelectInput(session, "create_new_compound_format_new_sf", selected = 0)
  })

  observeEvent(input$add_numeric_format_for_updated_sf, {
    updateSelectInput(session, "update_type", selected = 1)
    updateSelectInput(session, "table_to_add_to", selected = "numeric_date_formats")
    updateSelectInput(session, "create_new_numeric_format_new", selected = 0)
    updateSelectInput(session, "create_new_numeric_format_update", selected = 1)
  })

  # This runs when user initially clicks the button to delete a source field
  observeEvent(input$delete_source_field, {
    showModal(modalDialog(
      title="Delete Source Field?",
      footer = tagList(actionButton("delete_source_field_confirm", "Delete", class = "btn-danger btn-lg"),
                       modalButton("Cancel")
      )
    ))
  })

  # This runs when user confirms to deleting a source field
  observeEvent(input$delete_source_field_confirm, {
    # Get the selected rows from the user
    source_field_selected_row <- input$source_field_to_update_rows_selected

    # Grab the source_field_id using the dataset_id
    df <- dbGetQuery(metadata_connection, paste('SELECT * from source_fields where dataset_id =', update_source_field_ds_id,
                                                'ORDER BY field_order ASC'))
    source_field_id <- df[source_field_selected_row, "source_field_id"]
    field_order     <- df[source_field_selected_row, "field_order"]

    # Increment the field_order of existing entries where field_order >= new_field_order
    update_query <- paste("UPDATE source_fields SET field_order = field_order - 1 where field_order > ? AND dataset_id = ?")
    update_statement <- dbSendQuery(metadata_connection, update_query)
    dbBind(update_statement, list(field_order, update_source_field_ds_id))
    dbClearResult(update_statement)

    # Create a query for deleting the source_field
    delete_query <- paste("DELETE from source_fields",
                          "WHERE source_field_id = ?")
    delete <- dbSendQuery(metadata_connection, delete_query)
    dbBind(delete, list(source_field_id))
    dbClearResult(delete)

    # Create a query for deleting the records that use the same source field in
    # other tables
    delete_query <- paste("DELETE from compound_fields",
                          "WHERE source_field_id = ?")
    delete <- dbSendQuery(metadata_connection, delete_query)
    dbBind(delete, list(source_field_id))
    dbClearResult(delete)

    delete_query <- paste("DELETE from categorical_fields",
                          "WHERE source_field_id = ?")
    delete <- dbSendQuery(metadata_connection, delete_query)
    dbBind(delete, list(source_field_id))
    dbClearResult(delete)

    delete_query <- paste("DELETE from numeric_date_fields",
                          "WHERE source_field_id = ?")
    delete <- dbSendQuery(metadata_connection, delete_query)
    dbBind(delete, list(source_field_id))
    dbClearResult(delete)

    delete_query <- paste("DELETE from record_priority_fields",
                          "WHERE source_field_id = ?")
    delete <- dbSendQuery(metadata_connection, delete_query)
    dbBind(delete, list(source_field_id))
    dbClearResult(delete)

    rerender_data_tables_manage_datasets()
    removeModal()
    showNotification("Source Field Successfully Deleted", type = "message", closeButton = FALSE)
  })
  #----

  #-- Update Compound Formats --#
  #----
  # Compound Format Variables #
  selected_compound_field_format_id <- 0
  prepopulated_values <- reactiveValues()

  output$updatable_compound_formats <- renderDataTable({
    query <- paste('SELECT cff.compound_field_format_id, compound_format, format_description, count(source_field_id) as Number_Of_Usages FROM compound_field_formats cff',
                   'LEFT JOIN compound_fields cf on cf.compound_field_format_id = cff.compound_field_format_id',
                   'GROUP BY cff.compound_field_format_id',
                   'HAVING Number_Of_Usages = 0')
    df <- dbGetQuery(metadata_connection, query)
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select a Compound Format to Update:")
  })

  observeEvent(input$updatable_compound_formats_rows_selected, {

    selected_row <- input$updatable_compound_formats_rows_selected

    if (!is.null(selected_row)) {
      query <- paste('SELECT cff.compound_field_format_id, compound_format, format_description, count(source_field_id) as Number_Of_Usages FROM compound_field_formats cff',
                     'LEFT JOIN compound_fields cf on cf.compound_field_format_id = cff.compound_field_format_id',
                     'GROUP BY cff.compound_field_format_id',
                     'HAVING Number_Of_Usages <= 0')
      df <- dbGetQuery(metadata_connection, query)
      # Retrieve the chosen_source_field_id of the selected row
      selected_compound_field_format_id <<- df[selected_row, "compound_field_format_id"]
      compound_format <- df[selected_row, "compound_format"]
      format_description <- df[selected_row, "format_description"]

      # Store pre-populated values in reactive values
      prepopulated_values$compound_format <- compound_format
      prepopulated_values$format_description <- format_description

      # Run a query to get the separators and destinations
      df_separators <- dbGetQuery(metadata_connection, paste("SELECT * from compound_field_separators where compound_field_format_id =", selected_compound_field_format_id,
                                                             "ORDER BY separator_order ASC"))
      num_of_separators <- nrow(df_separators)
      separators <- df_separators$separator
      indexes    <- df_separators$substring_index

      # Store pre-populated values in reactive values
      prepopulated_values$num_of_separators <- num_of_separators
      prepopulated_values$separators <- separators
      prepopulated_values$indexes <- indexes

      df_destinations <- dbGetQuery(metadata_connection, paste("SELECT * from compound_field_destinations where compound_field_format_id =", selected_compound_field_format_id,
                                                               "ORDER BY destination_mapping_order ASC"))
      destination_fields <- df_destinations$destination_field_id
      prepopulated_values$destination_fields <- destination_fields

      # Update the inputs based on what row the user selected
      updateTextAreaInput(session, "new_updatable_compound_format", value = compound_format)
      updateTextAreaInput(session, "new_updatable_compound_format_description", value = format_description)

      if(all(is.na(indexes) | indexes == "")){
        updateSelectInput(session, "separator_or_indexes_update", selected = "Separators")
        updateNumericInput(session, "new_updatable_number_of_separators", value = num_of_separators)
      }
      else{
        updateSelectInput(session, "separator_or_indexes_update", selected = "Indexes")
        updateNumericInput(session, "new_updatable_number_of_indexes", value = num_of_separators)
      }

    }

  })

  output$updatable_separator_inputs <- renderUI({
    num_separators <- input$new_updatable_number_of_separators
    num_indexes <- input$new_updatable_number_of_indexes

    # Retrieve pre-populated values from reactive values
    separators <- prepopulated_values$separators
    indexes <- prepopulated_values$indexes
    destination_ids <- prepopulated_values$destination_fields

    if((is.nan(num_separators) || is.na(num_separators) || num_separators <= 0) && input$separator_or_indexes_update == 'Separators'){
      showNotification("Error - Invalid Number of Separators", type = "error", closeButton = FALSE)
      return()
    }

    if((is.nan(num_indexes) || is.na(num_indexes) || num_indexes <= 1) && input$separator_or_indexes_update != 'Separators'){
      showNotification("Error - Invalid Number of Indexes", type = "error", closeButton = FALSE)
      return()
    }

    if(input$separator_or_indexes_update == 'Separators'){
      separator_inputs_list <- lapply(1:((num_separators * 2) + 1), function(i) {
        if (i %% 2 == 1) {  # Odd index, create selectInput for destination field
          # Perform query using metadata_connection
          query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

          # Extract columns from query result
          choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

          # Add the additional look up value where the first choice is 0
          choices <- c("Not Applicable" = "null", choices)
          selectInput(inputId = paste0("updatable_separator_destination_field_", (i + 1) %/% 2),
                      label = paste0("Destination Field ", (i + 1) %/% 2, ":"),
                      choices = choices,
                      selected = destination_ids[(i + 1) %/% 2],
                      width = validateCssUnit("20vw"))
        } else {  # Even index, create textInput for separator
          textInput(inputId = paste0("updatable_separator_", (i + 1) %/% 2),
                    label = paste0("Separator ", (i + 1) %/% 2, ":"),
                    value = separators[(i + 1) %/% 2],
                    width = validateCssUnit("20vw"))
        }
      })

      tagList(separator_inputs_list)
    }
    else{
      index_inputs_list <- lapply(1:((num_indexes)), function(i) {
        # Perform query using metadata_connection
        query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

        # Extract columns from query result
        choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

        # Add the additional look up value where the first choice is 0
        choices <- c("Not Applicable" = "null", choices)
        fluidRow(
          column(6, div(style = "width: 20vw;",
                        selectInput(inputId = paste0("updatable_index_destination_field_", (i)),
                                label = paste0("Destination Field ", i, ":"),
                                choices = choices,
                                selected = destination_ids[i]))),
          column(6, div(style = "width: 20vw;",
                        numericInput(inputId = paste0("updatable_index_", i),
                                 label = paste0("Index ", i, ":"),
                                 value = indexes[i])))
        )
      })

      tagList(index_inputs_list)
    }
  })

  observeEvent(input$update_exisiting_compound_format, {
    num_separators     <- input$new_updatable_number_of_separators
    num_indexes        <- input$new_updatable_number_of_indexes
    format_description <- input$new_updatable_compound_format_description
    format             <- input$new_updatable_compound_format
    update_type        <- input$separator_or_indexes_update

    separators <- c()
    indexes <- c()
    destination_field_ids <- c()

    # Error handling pt. 1
    #----#
    if(is.nan(num_separators) || is.na(num_separators) || num_separators <= 0){
      showNotification("Error - Invalid Number of Separators", type = "error", closeButton = FALSE)
      return()
    }

    if(is.nan(num_indexes) || is.na(num_indexes) || num_indexes <= 1){
      showNotification("Error - Invalid Number of Indexes", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    if(update_type == "Separators"){
      for(i in seq_len(num_separators + 1)){
        if(i <= num_separators){
          separator_input_id <- paste0("updatable_separator_", i)
          separator_value <- input[[separator_input_id]]
          separators <- c(separators, separator_value)
        }

        destination_field_input_id <- paste0("updatable_separator_destination_field_", i)
        destination_field_value <- input[[destination_field_input_id]]
        if(destination_field_value == "null"){
          destination_field_value <- NA
        }
        destination_field_ids <- c(destination_field_ids, destination_field_value)
      }

      # Error handling pt. 2
      #----#
      for(i in seq_along(separators)){
        if(separators[i] == ""){
          showNotification("Failed to Create Compound Format - Some Compound Format Inputs are Missing", type = "error", closeButton = FALSE)
          return()
        }
      }
      #----#

      # Delete all the previous separators and destinations
      dbSendQuery(metadata_connection, paste('DELETE FROM compound_field_separators WHERE compound_field_format_id = ', selected_compound_field_format_id))
      dbSendQuery(metadata_connection, paste('DELETE FROM compound_field_destinations WHERE compound_field_format_id = ', selected_compound_field_format_id))

      # Create a query for inserting a new compound format into compound_field_formats
      update_query <- paste("UPDATE compound_field_formats
                          SET compound_format = ?, format_description = ?
                          WHERE compound_field_format_id = ?")
      update <- dbSendQuery(metadata_connection, update_query)
      dbBind(update, list(format, format_description, selected_compound_field_format_id))
      dbClearResult(update)

      # Loop for adding the separators into the metadata
      for(i in seq_along(separators)){
        # Create a query for inserting a new compound separator entry into compound_field_separators
        new_entry_query <- paste("INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)",
                                 "VALUES(?, ?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(selected_compound_field_format_id, i, separators[i], NA))
        dbClearResult(new_entry)
      }

      # Loop for adding the compound field destinations into the metadata
      for(i in seq_along(destination_field_ids)){
        # Create a query for inserting a new compound separator entry into compound_field_destinations
        new_entry_query <- paste("INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)",
                                 "VALUES(?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(selected_compound_field_format_id, i, destination_field_ids[i]))
        dbClearResult(new_entry)
      }
    }
    else{
      for(i in seq_len(num_indexes)){
        index_input_id <- paste0("updatable_index_", i)
        index_value <- input[[index_input_id]]
        indexes <- c(indexes, index_value)

        destination_field_input_id <- paste0("updatable_index_destination_field_", i)
        destination_field_value <- input[[destination_field_input_id]]
        if(destination_field_value == "null"){
          destination_field_value <- NA
        }
        destination_field_ids <- c(destination_field_ids, destination_field_value)
      }

      # Error handling pt. 2
      #----#
      for(i in seq_along(indexes)){
        if(is.na(indexes[i]) || is.null(indexes[i]) || indexes[i] <= 0){
          showNotification("Failed to Create Compound Format - Some Compound Format Inputs are Missing", type = "error", closeButton = FALSE)
          return()
        }
      }

      #----#

      # Delete all the previous separators and destinations
      dbSendQuery(metadata_connection, paste('DELETE FROM compound_field_separators WHERE compound_field_format_id = ', selected_compound_field_format_id))
      dbSendQuery(metadata_connection, paste('DELETE FROM compound_field_destinations WHERE compound_field_format_id = ', selected_compound_field_format_id))

      # Create a query for inserting a new compound format into compound_field_formats
      update_query <- paste("UPDATE compound_field_formats
                          SET compound_format = ?, format_description = ?
                          WHERE compound_field_format_id = ?")
      update <- dbSendQuery(metadata_connection, update_query)
      dbBind(update, list(format, format_description, selected_compound_field_format_id))
      dbClearResult(update)


      # Loop for adding the separators into the metadata
      for(i in seq_along(indexes)){
        # Create a query for inserting a new compound separator entry into compound_field_separators
        new_entry_query <- paste("INSERT INTO compound_field_separators (compound_field_format_id, separator_order, separator, substring_index)",
                                 "VALUES(?, ?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(selected_compound_field_format_id, i, NA, indexes[i])) #NA for now, change it later!
        dbClearResult(new_entry)
      }

      # Loop for adding the compound field destinations into the metadata
      for(i in seq_along(destination_field_ids)){
        # Create a query for inserting a new compound separator entry into compound_field_destinations
        new_entry_query <- paste("INSERT INTO compound_field_destinations (compound_field_format_id, destination_mapping_order, destination_field_id)",
                                 "VALUES(?, ?, ?);")
        new_entry <- dbSendQuery(metadata_connection, new_entry_query)
        dbBind(new_entry, list(selected_compound_field_format_id, i, destination_field_ids[i]))
        dbClearResult(new_entry)
      }
    }

    # Re-render the tables
    rerender_data_tables_manage_datasets()

    # Update the widgets on this page
    updateTextInput(session, "new_updatable_compound_format", value = "")
    updateTextInput(session, "new_updatable_compound_format_description", value = "")

    showNotification("Compound Format Successfully Updated", type = "message", closeButton = FALSE)
  })

  observeEvent(input$see_separator_example_2, {
    showModal(modalDialog(
      title = "Separator Example",
      tags$div(
        style = "max-height: 80vh; overflow-y: auto;",
        tags$img(src='separator_example.png', width = "100%")
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$see_index_example_2, {
    showModal(modalDialog(
      title = "Index Example",
      tags$div(
        style = "max-height: 80vh; overflow-y: auto;",
        tags$img(src='index_example.png', width = "100%")
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  #----

  #-- Update Categorical Fields --#
  #----

  # Update Categorical Fields variables
  dataset_id_categorical_field_update      <- 0
  source_field_id_categorical_field_update <- 0
  source_value_categorical_field_update    <- ""

  output$dataset_to_update_cfs <- renderDataTable({
    query <- paste('SELECT dataset_code, dataset_name from datasets')
    df <- dbGetQuery(metadata_connection, query)
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select the Dataset Containing the Desired Categorical Field:")
  })

  observeEvent(input$dataset_to_update_cfs_rows_selected, {
    selected_row <- input$dataset_to_update_cfs_rows_selected

    if (!is.null(selected_row)) {
      query <- paste('SELECT * from datasets')
      df <- dbGetQuery(metadata_connection, query)
      # Retrieve the dataset_id of the selected row
      dataset_id_categorical_field_update <<- df[selected_row, "dataset_id"]

      output$categorical_field_to_update <- renderDataTable({
        query <- paste('SELECT sf.source_field_id, source_field_name, source_value, standardized_value
                   FROM source_fields sf
                   JOIN categorical_fields cf on cf.source_field_id = sf.source_field_id
                   JOIN categorical_values cv on cf.standardized_value_id = cv.standardized_value_id
                   WHERE dataset_id =', dataset_id_categorical_field_update)
        df <- dbGetQuery(metadata_connection, query)
        datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                  caption = "Select The Categorical Field To Update:")
      })
    }
  })

  output$updated_standardizied_value <- renderUI({
    query_result <- dbGetQuery(metadata_connection, "SELECT * FROM categorical_values")

    # Extract columns from query result
    choices <- setNames(query_result$standardized_value_id, query_result$standardized_value)

    selectInput(inputId = paste0("standardized_categorical_value_updated"),
                label = paste0("Select Standardized Categorical Value:"),
                choices = choices,
                selected = NULL)

  })

  observeEvent(input$categorical_field_to_update_rows_selected, {
    selected_row <- input$categorical_field_to_update_rows_selected

    if(!is.null(selected_row)){
      query <- paste('SELECT sf.source_field_id, source_field_name, source_value, standardized_value, cf.standardized_value_id
                   FROM source_fields sf
                   JOIN categorical_fields cf on cf.source_field_id = sf.source_field_id
                   JOIN categorical_values cv on cf.standardized_value_id = cv.standardized_value_id
                   WHERE dataset_id =', dataset_id_categorical_field_update)
      df <- dbGetQuery(metadata_connection, query)
      source_field_id_categorical_field_update <<- df[selected_row, "source_field_id"]
      source_value_categorical_field_update    <<- df[selected_row, "source_value"]
      updateSelectInput(session, "standardized_categorical_value_updated", selected = df[selected_row, "standardized_value_id"])
      updateTextAreaInput(session, "new_source_value_update", value = source_value_categorical_field_update)
    }

  })

  observeEvent(input$submit_updated_categorical_field, {
    source_field_id                    <- source_field_id_categorical_field_update
    source_categorical_value           <- source_value_categorical_field_update
    new_source_categorical_value       <- input$new_source_value_update
    new_standardized_categorical_value <- input$standardized_categorical_value_updated

    # Error handling
    #----#
    if(is.na(new_source_categorical_value) || is.null(new_source_categorical_value) || new_source_categorical_value == ""){
      showNotification("Failed to Update Categorical Field - Some Inputs are Empty", type = "error", closeButton = FALSE)
      return()
    }

    #----#


    # Create a query for inserting a new compound format into compound_field_formats
    update_query <- paste("UPDATE categorical_fields
                          SET source_value = ?, standardized_value_id = ?
                          WHERE source_field_id = ? AND source_value = ?")
    update <- dbSendQuery(metadata_connection, update_query)
    dbBind(update, list(new_source_categorical_value, new_standardized_categorical_value,
                        source_field_id, source_categorical_value))
    dbClearResult(update)

    # Re-render tables
    output$categorical_field_to_update <- renderDataTable({
      query <- paste('SELECT sf.source_field_id, source_field_name, source_value, standardized_value
                   FROM source_fields sf
                   JOIN categorical_fields cf on cf.source_field_id = sf.source_field_id
                   JOIN categorical_values cv on cf.standardized_value_id = cv.standardized_value_id
                   WHERE dataset_id =', dataset_id_categorical_field_update)
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select The Categorical Field To Update:")
    })

    # Update input boxes
    updateSelectInput(session, "standardized_categorical_value_updated", selected = NULL)
    updateTextInput(session, "new_source_value_update", value = "")

    showNotification("Categorical Field Successfully Updated", type = "message", closeButton = FALSE)

  })

  # This runs when user initially clicks the button to delete a categorical field
  observeEvent(input$delete_categorical_field, {
    showModal(modalDialog(
      title="Delete Categorical Field?",
      footer = tagList(actionButton("delete_categorical_field_confirm", "Delete", class = "btn-danger btn-lg"),
                       modalButton("Cancel")
      )
    ))
  })

  # Runs if user confirms deleting a categorical field
  observeEvent(input$delete_categorical_field_confirm, {
    dataset_id      <- dataset_id_categorical_field_update
    source_field_id <- source_field_id_categorical_field_update
    source_value    <- source_value_categorical_field_update

    # Create a query for deleting the categorical field
    delete_query <- paste("DELETE from categorical_fields",
                          "WHERE source_field_id = ? and source_value = ?")
    delete <- dbSendQuery(metadata_connection, delete_query)
    dbBind(delete, list(source_field_id, source_value))
    dbClearResult(delete)

    # Re-render tables
    output$categorical_field_to_update <- renderDataTable({
      query <- paste('SELECT sf.source_field_id, source_field_name, source_value, standardized_value
                   FROM source_fields sf
                   JOIN categorical_fields cf on cf.source_field_id = sf.source_field_id
                   JOIN categorical_values cv on cf.standardized_value_id = cv.standardized_value_id
                   WHERE dataset_id =', dataset_id_categorical_field_update)
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select The Categorical Field To Update:")
    })

    removeModal()

    showNotification("Categorical Field Successfully Deleted", type = "message", closeButton = FALSE)
  })
  #----

  #-- Update Numeric Format --#
  #----
  numeric_date_format_id_to_update <- 0
  output$updatable_numeric_formats <- renderDataTable({
    query <- paste('SELECT ndf.numeric_date_format_id, numeric_date_format_label, origin_date, units_label, count(source_field_id) as Number_Of_Usages FROM numeric_date_formats ndf',
                   'LEFT JOIN numeric_date_fields ndf2 on ndf2.numeric_date_format_id = ndf.numeric_date_format_id',
                   'GROUP BY ndf.numeric_date_format_id',
                   'HAVING Number_Of_Usages = 0')
    df <- dbGetQuery(metadata_connection, query)
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select a Numeric Format to Update:")
  })

  observeEvent(input$updatable_numeric_formats_rows_selected, {
    selected_row <- input$updatable_numeric_formats_rows_selected

    if (!is.null(selected_row)) {
      query <- paste('SELECT ndf.numeric_date_format_id, numeric_date_format_label, origin_date, units_label, count(source_field_id) as Number_Of_Usages FROM numeric_date_formats ndf',
                     'LEFT JOIN numeric_date_fields ndf2 on ndf2.numeric_date_format_id = ndf.numeric_date_format_id',
                     'GROUP BY ndf.numeric_date_format_id',
                     'HAVING Number_Of_Usages = 0')
      df <- dbGetQuery(metadata_connection, query)
      # Retrieve the chosen_source_field_id of the selected row
      numeric_date_format_id_to_update <<- df[selected_row, "numeric_date_format_id"]
      numeric_date_format_label        <- df[selected_row, "numeric_date_format_label"]
      origin_date                      <- df[selected_row, "origin_date"]
      units_label                      <- df[selected_row, "units_label"]

      # Update/pre-populate the input fields
      updateTextAreaInput(session, "updated_numeric_format_label", value = numeric_date_format_label)
      updateDateInput(session, "updated_origin_date", value = origin_date)
      updateSelectInput(session, "updated_units_label", selected = units_label)
    }
  })

  observeEvent(input$update_exisiting_numeric_format, {
    format_label <- input$updated_numeric_format_label
    origin_date  <- as.character(input$updated_origin_date)
    units_label  <- input$updated_units_label

    # Error Handling
    #----#
    if(is.na(format_label) || is.null(format_label) || format_label == ""){
      showNotification("Updating Numeric Format Failed - Invalid Format Label", type = "error", closeButton = FALSE)
      return()
    }

    if(identical(origin_date, character(0)) || is.na(origin_date) || is.null(origin_date) || origin_date == ""){
      showNotification("Updating Numeric Format Failed - Invalid Origin Date", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    # Create a query for updating the numeric_date_format
    update_query <- paste("UPDATE numeric_date_formats
                          SET numeric_date_format_label = ?, origin_date = ?, units_label = ?
                          WHERE numeric_date_format_id = ?")
    update <- dbSendQuery(metadata_connection, update_query)
    dbBind(update, list(format_label, origin_date, units_label, numeric_date_format_id_to_update))
    dbClearResult(update)

    # Re-render tables
    rerender_data_tables_manage_datasets()

    showNotification("Numeric Format Successfully Updated", type = "message", closeButton = FALSE)
  })
  #----

  #-- Update Record Priorities --#
  #----
  dataset_id_record_priority_update      <- 0
  source_field_id_record_priority_update <- 0
  source_value_record_priority_update  <- ""

  output$dataset_to_update_record_priority <- renderDataTable({
    query <- paste('SELECT dataset_code, dataset_name from datasets ORDER BY dataset_id ASC')
    df <- dbGetQuery(metadata_connection, query)
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
              caption = "Select the Dataset Containing the Desired Record Priority:")
  })

  observeEvent(input$dataset_to_update_record_priority_rows_selected, {
    selected_row <- input$dataset_to_update_record_priority_rows_selected

    if (!is.null(selected_row)) {
      query <- paste('SELECT * from datasets ORDER BY dataset_id ASC')
      df <- dbGetQuery(metadata_connection, query)
      # Retrieve the dataset_id of the selected row
      dataset_id_record_priority_update <<- df[selected_row, "dataset_id"]

      output$record_priorities_to_update <- renderDataTable({
        query <- paste('SELECT sf.source_field_id, source_field_name, source_value, priority
                   FROM source_fields sf
                   JOIN record_priority_fields rpf on rpf.source_field_id = sf.source_field_id
                   WHERE dataset_id =', dataset_id_record_priority_update, 'ORDER BY priority ASC')
        df <- dbGetQuery(metadata_connection, query)
        datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                  caption = "Select The Record Priority To Update:")
      })
    }
  })

  observeEvent(input$record_priorities_to_update_rows_selected, {
    selected_row <- input$record_priorities_to_update_rows_selected

    if(!is.null(selected_row)){
      query <- paste('SELECT sf.source_field_id, source_field_name, source_value, priority
                   FROM source_fields sf
                   JOIN record_priority_fields rpf on rpf.source_field_id = sf.source_field_id
                   WHERE dataset_id =', dataset_id_record_priority_update, 'ORDER BY priority ASC')
      df <- dbGetQuery(metadata_connection, query)

      source_field_id_record_priority_update <<- df[selected_row, "source_field_id"]
      source_value_priority                  <<- df[selected_row, "priority"]
      source_value_record_priority_update    <<- df[selected_row, "source_value"]

      updateNumericInput(session, "updated_record_priority", value = source_value_priority)
      updateTextInput(session, "new_record_priority_source_value_update", value = source_value_record_priority_update)
    }

  })

  observeEvent(input$submit_updated_record_priority, {
    source_field_id        <- source_field_id_record_priority_update
    source_priority_value  <- source_value_record_priority_update
    new_source_value       <- input$new_record_priority_source_value_update
    new_record_priority    <- input$updated_record_priority

    # Error handling
    #----#
    if(is.na(new_record_priority) || new_record_priority <= 0 || new_source_value == ""){
      showNotification("Failed to Update Record Priority - Some Inputs are Invalid", type = "error", closeButton = FALSE)
    }
    #----#

    # Create a query for inserting a new compound format into compound_field_formats
    update_query <- paste("UPDATE record_priority_fields
                          SET source_value = ?, priority = ?
                          WHERE source_field_id = ? AND source_value = ?")
    update <- dbSendQuery(metadata_connection, update_query)
    dbBind(update, list(new_source_value, new_record_priority,
                        source_field_id, source_priority_value))
    dbClearResult(update)

    # Re-render the table
    output$record_priorities_to_update <- renderDataTable({
      query <- paste('SELECT sf.source_field_id, source_field_name, source_value, priority
                   FROM source_fields sf
                   JOIN record_priority_fields rpf on rpf.source_field_id = sf.source_field_id
                   WHERE dataset_id =', dataset_id_record_priority_update, 'ORDER BY priority ASC')
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select The Record Priority To Update:")
    })

    # Reset UI Inputs
    updateNumericInput(session, "updated_record_priority", value = NULL)
    updateTextInput(session, "new_record_priority_source_value_update", value = "")

    # Send success message
    showNotification("Record Priority Successfully Updated", type = "message", closeButton = FALSE)
  })

  # This runs when user initially clicks the button to delete a record priority
  observeEvent(input$delete_record_priority, {
    showModal(modalDialog(
      title="Delete Record Priority?",
      footer = tagList(actionButton("delete_record_priority_confirm", "Delete", class = "btn-danger btn-lg"),
                       modalButton("Cancel")
      )
    ))
  })

  # This runs when user confirms to delete the record priority
  observeEvent(input$delete_record_priority_confirm, {
    dataset_id      <- dataset_id_record_priority_update
    source_field_id <- source_field_id_record_priority_update
    source_value    <- source_value_record_priority_update

    # Create a query for deleting the categorical field
    delete_query <- paste("DELETE from record_priority_fields",
                          "WHERE source_field_id = ? and source_value = ?")
    delete <- dbSendQuery(metadata_connection, delete_query)
    dbBind(delete, list(source_field_id, source_value))
    dbClearResult(delete)

    # Re-render the table
    output$record_priorities_to_update <- renderDataTable({
      query <- paste('SELECT dataset_id, sf.source_field_id, source_field_name, source_value, priority
                   FROM source_fields sf
                   JOIN record_priority_fields rpf on rpf.source_field_id = sf.source_field_id
                   WHERE dataset_id =', dataset_id_record_priority_update)
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE),
                caption = "Select The Record Priority To Update:")
    })

    removeModal()

    showNotification("Record Priority Successfully Deleted", type = "message", closeButton = FALSE)
  })

  #----

  #----------------------------ENABLE AND DISABLE------------------------------#

  # Enable and Disable Databases
  #----
  output$databases_disable <- renderDataTable({
    df <- dbGetQuery(metadata_connection, 'SELECT dataset_code, dataset_name from datasets WHERE enabled_for_standardization = 1;')
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE, scrollX = TRUE),
              caption = "Select a Dataset to Disable:")
  })

  output$databases_enable <- renderDataTable({
    df <- dbGetQuery(metadata_connection, 'SELECT dataset_code, dataset_name from datasets WHERE enabled_for_standardization = 0;')
    datatable(df, selection = 'single', rownames = FALSE, options = list(lengthChange = FALSE, scrollX = TRUE),
              caption = "Select a Dataset to Enable:")
  })

  observeEvent(input$disable_selected_database, {
    # Get the selected row
    selected_row <- input$databases_disable_rows_selected

    # Get the data frame so that we can identify the selected dataset
    df <- dbGetQuery(metadata_connection, 'SELECT * from datasets WHERE enabled_for_standardization = 1;')
    dataset_id <- df[selected_row, "dataset_id"]

    # Update the enabled value by setting it to 0 (disabled)
    update_query <- paste("UPDATE datasets
                          SET enabled_for_standardization = 0
                          WHERE dataset_id = ?")
    update <- dbSendQuery(metadata_connection, update_query)
    dbBind(update, list(dataset_id))
    dbClearResult(update)

    # Re-render the tables
    rerender_data_tables_manage_datasets()
    rerender_data_tables_manage_datasets()

    showNotification("Database Successfully Disabled", type = "message", closeButton = FALSE)
  })

  observeEvent(input$enable_selected_database, {
    # Get the selected row
    selected_row <- input$databases_enable_rows_selected

    # Get the data frame so that we can identify the selected dataset
    df <- dbGetQuery(metadata_connection, 'SELECT * from datasets WHERE enabled_for_standardization = 0;')
    dataset_id   <- df[selected_row, "dataset_id"]
    dataset_code <- df[selected_row, "dataset_code"]

    # Error handling - don't allow user to have two databases enabled with the same data set code
    #----#
    get_query <- dbSendQuery(metadata_connection, 'SELECT * FROM datasets WHERE dataset_code = ? AND enabled_for_standardization = 1;')
    dbBind(get_query, list(dataset_code))
    output_df <- dbFetch(get_query)
    enabled_databases <- nrow(output_df)
    dbClearResult(get_query)

    if(is.na(enabled_databases) || is.null(enabled_databases) || enabled_databases != 0){
      showNotification("Failed to Enable Database - Database with the same dataset code is already enabled", type = "error", closeButton = FALSE)
      return()
    }
    #----#

    # Update the enabled value by setting it to 0 (disabled)
    update_query <- paste("UPDATE datasets
                          SET enabled_for_standardization = 1
                          WHERE dataset_id = ?")
    update <- dbSendQuery(metadata_connection, update_query)
    dbBind(update, list(dataset_id))
    dbClearResult(update)

    # Re-render tables
    rerender_data_tables_manage_datasets()
    rerender_data_tables()

    showNotification("Database Successfully Enabled", type = "message", closeButton = FALSE)
  })
  #----

  #----------------------------CREATE NEW MODULE--------------------------------#
  # Create a New Standardizing Module
  #----
  output$curr_standardizing_modules <- renderDataTable({
    query <- paste("SELECT standardizing_module_name, description, destination_field_name, output_program_data from standardizing_modules sm",
                   "LEFT JOIN destination_fields df on sm.destination_field_id = df.destination_field_id",
                   "ORDER BY standardizing_module_id asc")
    df <- dbGetQuery(metadata_connection, query)
    datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE, scrollX = TRUE),
              caption = "Current Standardizing Modules:")
  })

  output$new_module_dest <- renderUI({
    query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

    # Extract columns from query result
    choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

    # Add the additional look up value where the first choice is 0
    choices <- c("No Destination Field" = "null", choices)

    selectInput(inputId = paste0("new_module_destination_field"),
                label = paste0("Select Destination Field:"),
                choices = choices,
                selected = NULL,
                width = validateCssUnit("25vw"))

  })

  observeEvent(input$create_module, {
    module_name <- input$new_module_name
    module_desc <- input$new_module_desc
    module_dest <- input$new_module_destination_field
    module_outp <- input$new_module_output_data

    if(module_dest == "null")
      module_dest <- NA

    # Run an insert query to create a new standardizing module!
    # Create a query for inserting a new categorical field into categorical_fields
    new_entry_query <- paste("INSERT INTO standardizing_modules (standardizing_module_name, description, destination_field_id, output_program_data, standardizing_module_type)",
                             "VALUES(?, ?, ?, ?, ?);")
    new_entry <- dbSendQuery(metadata_connection, new_entry_query)
    dbBind(new_entry, list(module_name, module_desc, module_dest, module_outp, 1))
    dbClearResult(new_entry)

    showNotification("Standardizing Module Successfully Created", type = "message", closeButton = FALSE)

    # Re-render table
    output$curr_standardizing_modules <- renderDataTable({
      query <- paste("SELECT standardizing_module_name, description, destination_field_name, output_program_data from standardizing_modules sm",
                     "LEFT JOIN destination_fields df on sm.destination_field_id = df.destination_field_id",
                     "ORDER BY standardizing_module_id asc")
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE, scrollX = TRUE),
                caption = "Current Standardizing Modules:")
    })

    # Re-render UI
    updateTextInput(session, "new_module_name", value = "")
    updateTextAreaInput(session, "new_module_desc", value = "")
    reset_ui_renders()
  })
  #----

  # Create a New Destination Field
  #----
  output$curr_destination_fields <- renderDataTable({
    query <- paste("SELECT destination_field_name, destination_field_description from destination_fields",
                   "ORDER BY destination_field_id asc")
    df <- dbGetQuery(metadata_connection, query)
    datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE, scrollX = TRUE),
              caption = "Current Destination Fields:")
  })

  observeEvent(input$create_dest_field, {
    destination_field_name <- input$new_dest_field_name
    destination_field_desc <- input$new_dest_field_desc

    # Run an insert query to create a new standardizing module!
    # Create a query for inserting a new categorical field into categorical_fields
    new_entry_query <- paste("INSERT INTO destination_fields (destination_field_name, destination_field_description)",
                             "VALUES(?, ?);")
    new_entry <- dbSendQuery(metadata_connection, new_entry_query)
    dbBind(new_entry, list(destination_field_name, destination_field_desc))
    dbClearResult(new_entry)

    showNotification("Destination Field Successfully Created", type = "message", closeButton = FALSE)

    # Re-render table
    output$curr_destination_fields <- renderDataTable({
      query <- paste("SELECT destination_field_name, destination_field_description from destination_fields",
                     "ORDER BY destination_field_id asc")
      df <- dbGetQuery(metadata_connection, query)
      datatable(df, selection = 'none', rownames = FALSE, options = list(lengthChange = FALSE, scrollX = TRUE),
                caption = "Current Destination Fields:")
    })

    # Re-render UI
    output$new_module_dest <- renderUI({
      query_result <- dbGetQuery(metadata_connection, "SELECT * FROM destination_fields")

      # Extract columns from query result
      choices <- setNames(query_result$destination_field_id, query_result$destination_field_name)

      # Add the additional look up value where the first choice is 0
      choices <- c("No Destination Field" = "null", choices)

      selectInput(inputId = paste0("new_module_destination_field"),
                  label = paste0("Select Destination Field:"),
                  choices = choices,
                  selected = NULL,
                  width = validateCssUnit("25vw"))

    })
    updateTextInput(session, "new_dest_field_name", value = "")
    updateTextAreaInput(session, "new_dest_field_desc", value = "")
    reset_ui_renders()
  })
  #----
}

#' Start Metadata UI
#'
#' Upon call, and passing it a valid metadata file, the application will run using the metadata file as the file that the user can
#' make modifications to, this involves viewing entries, adding new entries, updating them, or deleting them.
#' @param metadata_file_path A path to the sqlite metadata file.
#' @examples
#' startMetadataUI("path/to/metadata.sqlite")
#' @export
startMetadataUI <- function(metadata_file_path){

  # Error handling to ensure passed an actual sqlite file
  #----#
  if(is.null(metadata_file_path) || file_ext(metadata_file_path) != "sqlite"){
    stop("ERROR: Input file provided was not an SQLite file.")
  }
  #----#

  # Establish the metadata connection
  metadata_connection <- dbConnect(RSQLite::SQLite(), metadata_file_path)

  # Error handling to ensure the metadata file provided is valid
  #----#
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
    dbDisconnect(metadata_connection)
    stop("ERROR: Invalid SQLite file.")
  }

  rm(required_tables, missing_tables, existing_tables)
  #----#

  # Set a busy timeout here to help when more than one user is writing or reading
  dbExecute(metadata_connection, "PRAGMA busy_timeout = 10000")

  # Start App ----
  shinyApp(ui = dataset_ui,
           server = function(input, output, session) {
             dataset_server(input, output, session, metadata_connection)
           },
           onStart = function() {
             cat("App Started")
             onStop(function() {
               cat("App Ended")
             })
           })
}


