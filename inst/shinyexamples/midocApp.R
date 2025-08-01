# Set up ------------------------------------------------------------------

# install.packages("midoc")
# install.packages("shiny")
# install.packages("devtools")

# library(devtools) #  allows the next line of code
# install_github("elliecurnow/midoc", dependencies = TRUE) # link to github version instead of CRAN

library(midoc)
library(shiny) # makes embedded apps

# Reactive values
uploaded_data <- reactiveValues(
  df = NULL,
  data_source = NULL,
  dag_text = NULL
)

# data upload app ---------------------------------------------------------

# USER INTERFACE - data upload app
data_ui <- sidebarLayout(

  # App title
  sidebarPanel(
    selectInput("data_source", "Choose Data Source:",
                choices = c("Upload CSV" = "upload",
                            "BMI dataset"= "bmi",
                            "RTC dataset" = "qol",
                            "ADR dataset" = "adr")),

    conditionalPanel(
      condition = "input.data_source == 'upload'",
      fileInput("file", "Upload .csv file", accept = ".csv")
    )
  ),

  mainPanel(
    tableOutput("preview"),
    uiOutput("post_ouput_text_upload_data")
  )
)

# SERVER - data upload app
data_server <- function(input, output, session) {

  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    uploaded_data$df <- df             # Store uploaded data
    uploaded_data$data_source <- "upload"  # record source as upload
  })

  observeEvent(input$data_source, {
    if (input$data_source == "bmi") {
      uploaded_data$df <- midoc::bmi   # Store BMI dataframe
      uploaded_data$data_source <- "bmi"  # record source as bmi
    }

    if (input$data_source == "qol") {
      uploaded_data$df <- midoc::qol  # store qol dataframe
      uploaded_data$data_source <- "qol" # record source as qol
    }

    if (input$data_source == "adr") {
      uploaded_data$df <- midoc::adr  #record source as adr
      uploaded_data$data_source <- "adr" # record source as adr
    }
  })

  output$preview <- renderTable({
    req(uploaded_data$df)
    head(uploaded_data$df, 10)
  })

  output$post_ouput_text_upload_data <- renderUI({
    req(uploaded_data$df)  # Make sure data is loaded before showing the text
    tags$p("The above table shows a preview of the selected or uploaded data set")
  })
}

# dag specification app ---------------------------------------------------

# USER INTERFACE  - Draw DAG app
drawDAG_ui <- fluidPage(

  # app title
  titlePanel("Draw the specified missingness directed acyclic graph (mDAG)"),

  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Use arrows to specify causal relationships. Make sure to include a space before and ",
           "after the arrow. Use a new line for each causal relationship.")),
    p(HTML("Example input: <code>variable -> variable</code>")),
    p(HTML("<code>variable -> variable</code>"), style = "text-indent: 95px;"),
    p(HTML("<code>variable -> variable</code>"), style = "text-indent: 95px;")
  ),
  hr(),

  sidebarLayout(
    sidebarPanel(
      textAreaInput(inputId = "mdag_drawDAG",
                    label = "Directed acyclic graph",
                    value = ""),
      actionButton(inputId = "go_drawDAG",
                   label = "Draw mDAG")
    ),

    mainPanel(
      plotOutput("mdagplot"),
      uiOutput("post_output_text_drawDAG")
    )
  )
)

# SERVER - Draw DAG app
drawDAG_server <- function(input, output, session) {

  # bmi autofill dag input
  bmi_dag_text <- paste(
    "matage -> bmi7",
    "mated -> matage",
    "mated -> bmi7",
    "sep_unmeas -> mated",
    "sep_unmeas -> r",
    "pregsize -> bmi7",
    "pregsize -> bwt",
    "sep_unmeas -> bwt",
    sep = "\n"
  )

  # RCT autofill dag input
  qol_dag_text <- paste(
    "", # add qol dag here
    sep = "\n"
  )

  # ADR autofill dag input
  adr_dag_text <- paste(
    "log_income -> gcse_score
    log_income -> ks2_score
    mated -> log_income
    mated -> gcse_score
    mated -> ks2_score
    ks2_score -> gcse_score
    mated -> r_cra
    ks2_score -> r_cra",
    sep = "\n"
  )

  # Reactive flag to track data changes and reset plot
  data_changed <- reactiveVal(TRUE)  # tracks whether new data/DAG was uploaded

  # if data source is package dataset, autofill dag to input
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark that data has changed (so plot should clear)

    if (uploaded_data$data_source == "bmi") {
      updateTextAreaInput(session, "mdag_drawDAG", value = bmi_dag_text)
      uploaded_data$dag_text <- bmi_dag_text
    } else if (uploaded_data$data_source == "qol") {
      updateTextAreaInput(session, "mdag_drawDAG", value = qol_dag_text)
      uploaded_data$dag_text <- qol_dag_text
    } else if (uploaded_data$data_source == "adr") {
      updateTextAreaInput(session, "mdag_drawDAG", value = adr_dag_text)
      uploaded_data$dag_text <- adr_dag_text
    } else if (uploaded_data$data_source == "upload") {
      updateTextAreaInput(session, "mdag_drawDAG", value = "")
      uploaded_data$dag_text <- ""
    }
  })

  # Set data_changed to TRUE when DAG input changes, to block output until button clicked
  observeEvent(input$mdag_drawDAG, {
    data_changed(TRUE)
  })

  # Clear the data_changed flag when button is clicked
  observeEvent(input$go_drawDAG, {
    uploaded_data$dag_text <- input$mdag_drawDAG
    data_changed(FALSE)
  })

  # Reactive DAG specification - run only after button click
  dag_spec <- eventReactive(input$go_drawDAG, {
    req(input$mdag_drawDAG)
    paste('dag {', input$mdag_drawDAG, '}')
  })

  # Draw DAG plot only after "draw DAG" button is clicked and data unchanged
  output$mdagplot <- renderPlot({
    if (data_changed()) {
      # Data has changed but draw button not clicked yet — clear plot
      return(NULL)
    }
    req(dag_spec())  # only run after button click
    plot(dagitty::dagitty(dag_spec(), layout = TRUE))
  })

  #conditional text under output for draw DAG app
  output$post_output_text_drawDAG <- renderUI({
    if (data_changed()) {
      # Clear UI output if data changed but not drawn yet
      return(NULL)
    }

    req(input$go_drawDAG)
    req(uploaded_data$data_source)  # Make sure data_source exists

    if (uploaded_data$data_source == "bmi") {
      tagList(
        hr(),
        div(
          # bmi specific post output text
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("Visually check that the relationships are specified as you intended. ",
            "This dag will carry over to the function apps."),
          p(HTML('The mDAG has been drawn using “dagitty”. Go to the',
          '<a href="https://dagitty.net" target="_blank">dagitty website</a>',
                 'to find out more about using "dagitty" to draw mDAGs.' ))
        )
      )
    } else {
      tagList(
        hr(),
        div(
          # generic post output text
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("Visually check that the relationships are specified as you intended. ",
            "This dag will carry over to the function apps."),
          p(HTML('The mDAG has been drawn using “dagitty”. Go to the dagitty [website](https://www.dagitty.net/)',
                 'to find out more about using "dagitty" to draw mDAGs.' ))
        )
      )
    }
  })
}

# descMissData() function app tab 1 ---------------------------------------------

# USER INTERFACE - descMissData() function app
descMissData_ui <- tagList(

  # App title
  titlePanel("List missing data patterns"),

  # text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p("Your analysis model outcome/variable of primary interest is the variable with",
      "missing data."),
    p(HTML("When listing covariates, seperate them by a space. Example input:",
           "<code>covariate_1 covariate_2 covariate_3</code>.")),
    p("Make sure variables entered are spelt the same as in the dataset.")
  ),
  hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      textInput(inputId = "y_descMissData",
                label = "Analysis model outcome/variable of primary interest",
                value = ""),

      textInput(inputId = "covs_descMissData",
                label = "Analysis model covariates/other variables, separated by a space",
                value = ""),

      actionButton(inputId = "go_descMissData",
                   label = "List missing data patterns")

    ),

    # Main panel for displaying outputs
    mainPanel(
      verbatimTextOutput(outputId = "descmissdata_print"), #  text output
      plotOutput(outputId = "descmissdata"), # plot output
      uiOutput("post_output_text_descMissData")  # dynamic text below the output
    )

  )
)


# SERVER - descMissData() function app

descMissData_server <- function(input, output, session) {

  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a data set.")
    )
    uploaded_data$df
  })

  # Reactive flag to track data changes and reset plot
  data_changed <- reactiveVal(FALSE)

  # if data source is bmi, autofill input, otherwise, leave them blank
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark that data has changed (so plot should clear)

    if (uploaded_data$data_source == "bmi") {
      updateTextInput(session, "y_descMissData", value = "bmi7")
      updateTextInput(session, "covs_descMissData", value = "matage mated pregsize bwt")
    } else {
      updateTextInput(session, "y_descMissData", value = "")
      updateTextInput(session, "covs_descMissData", value = "")
    }
  })

  # Set data_changed TRUE when relevant inputs change, to block output until button clicked
  observeEvent(c(input$y_descMissData, input$covs_descMissData), {
    data_changed(TRUE)
  })

  # Save DAG input when Draw DAG button is clicked
  observeEvent(input$go_descMissData, {
    data_changed(FALSE)  # Clear the data changed flag so plot can render
  })

  # descMissData() function for text output
  output$descmissdata_print <- renderPrint({
    req(input$go_descMissData)
    if (data_changed()) {
      return(invisible())
    }
    req(data())
    midoc::descMissData(
      y = isolate(input$y_descMissData),
      covs = isolate(input$covs_descMissData),
      data = data(),
      plot = FALSE
    )
  })

  # descMissData() function for plot output
  output$descmissdata <- renderPlot({
    req(input$go_descMissData)
    if (data_changed()) {
      return(invisible())
    }
    req(data())
    midoc::descMissData(
      y = isolate(input$y_descMissData),
      covs = isolate(input$covs_descMissData),
      data = data(),
      plot = TRUE
    )
  })

  # conditional text under output
  output$post_output_text_descMissData <- renderUI({
    if (data_changed()) {
      # Clear UI output if data changed but not drawn yet
      return(NULL)
    }
    req(input$go_descMissData)
    req(uploaded_data$data_source)  # Make sure data_source exists

    if (uploaded_data$data_source == "bmi") {
      tagList(
        hr(),
        div(
          # bmi specific post output text
          style = "margin-top: 15px; font-size: 12px; color: #333;",
          p("A complete records indicator of 1 = observed data and 0 = incomplete data"),
          p("Here there are two missing data patterns: either all variables are observed,",
            "or BMI at age 7 years is missing and all other variables are observed. There are",
            "592 (59%) individuals with no missing data.")
        )
      )
    } else {
      tagList(
        hr(),
        div(
          # generic post output text
          style = "margin-top: 15px; font-size: 12px; color: #333;",
          p("A complete records indicator of 1 = complete and 0 = incomplete data")
        )
      )
    }
  })
}

# exploreDAG() function app tab 2 -----------------------------------------------

# USER INTERFACE - exploreDAG() function app
exploreDAG_ui <- tagList(

  # App title
  titlePanel("Compare data with proposed DAG"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Use the <code>midoc</code> function <code>exploreDAG()</code> to explore",
           "whether the observed relationships ",
           "in the dataset are consistent with your proposed mDAG")),
    p("The DAG is carried over from the draw DAG app.")
  ),
  hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      textAreaInput(inputId = "mdag_exploreDAG",
                    label = "Directed acyclic graph",
                    value = ""),

      actionButton(inputId = "go_exploreDAG",
                   label = "Compare data with DAG")

    ),

    # Main panel for displaying outputs
    mainPanel(

      # Output: Print result
      verbatimTextOutput(outputId = "exploredag"),
      uiOutput("post_output_text_exploreDAG")

    )
  )
)

# SERVER explorDAG() function app
exploreDAG_server <- function(input, output, session) {

  # Reactive dataset
  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a data set.")
    )
    uploaded_data$df
  })

  # Reactive DAG text with validation for non-empty text
  dag_text <- reactive({
    validate(
      need(!is.null(uploaded_data$dag_text) && nzchar(trimws(uploaded_data$dag_text)),
           "Please specify DAG")
    )
    uploaded_data$dag_text
  })

  # Reactive flag to track data changes and reset output
  data_changed <- reactiveVal(TRUE)  # tracks whether new data/DAG was uploaded

  # autofill dag from draw dag app
  observe({
    if (!is.null(uploaded_data$dag_text)) {
      updateTextAreaInput(session, "mdag_exploreDAG", value = uploaded_data$dag_text)
    }
  })

  # Reset output if new data or DAG is loaded
  observeEvent(uploaded_data$df, {
    data_changed(TRUE)
  })
  observeEvent(uploaded_data$dag_text, {
    data_changed(TRUE)
  })

  # Reset output if DAG input is altered
  observeEvent(input$mdag_exploreDAG, {
    data_changed(TRUE)
  })

  # Clear the data_changed flag when button is clicked
  observeEvent(input$go_exploreDAG, {
    data_changed(FALSE)
  })

  # exploreDAG() function
  exploredag_result <- eventReactive(input$go_exploreDAG, {
    req(data())
    req(dag_text())
    testthat::evaluate_promise(midoc::exploreDAG(dag_text(), data()))$messages
  })

  # OexploreDAG function ouput
  output$exploredag <- renderPrint({
    if (data_changed()) return(invisible())  # prevent output if data changed but button not clicked
    req(exploredag_result())  # only run after button click
    cat(strwrap(paste(exploredag_result(), collapse = "\n")), sep = "\n")
  })

  # conditional text below output
  output$post_output_text_exploreDAG <- renderUI({
    if (data_changed()) return(NULL)  # prevent UI text if data changed but button not clicked
    req(uploaded_data$data_source)  # Make sure data_source exists

    if (uploaded_data$data_source == "bmi") {
      tagList(
        hr(),
        div(
          # bmi specifc post output text
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("There is little evidence against the null hypothesis that the stated ",
            "variables are (conditionally) independent.")
        )
      )
    } else {
      tagList(
        hr(),
        div(
          # generic post output text
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("") # this is blank currently because the output explains how to interpret it well on its own
        )
      )
    }
  })
}

# checkCRA() function app  tab 3-------------------------------------------------

# USER INTERFACE - checkCRA() function app
checkCRA_ui <- tagList(

  # App title
  titlePanel("Inspect complete records analysis model"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Use the <code>midoc</code> function <code>checkCRA()</code> to check whether",
           "a complete records analysis is valid.")),
    p("Enter covariates seperated by a space. Ensure variable names match column headings."),
    p(HTML("Example input: <code>covariate_1 covariate_2 covariate_3</code>."))
  ),
  hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      textInput(inputId = "y_checkCRA",
                label = "Analysis model outcome",
                value = ""),

      textInput(inputId = "covs_checkCRA",
                label = "Analysis model covariates",
                value = ""),

      textInput(inputId = "r_checkCRA",
                label = "Complete record indicator",
                value = ""),

      textAreaInput(inputId = "mdag_checkCRA",
                    label = "Directed acyclic graph",
                    value = ""),

      actionButton(inputId = "go_checkCRA",
                   label = "Check CRA is valid")
    ),

    # Main panel for displaying outputs
    mainPanel(

      # Output: Print result
      verbatimTextOutput(outputId = "checkcra"), # CRA ouput
      uiOutput("post_output_text_checkCRA") # post output text

    )
  )
)

# SERVER - checkCRA() function app
checkCRA_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)

  # Autofill DAG from uploaded_data$dag_text
  observe({
    dag_text <- uploaded_data$dag_text
    updateTextAreaInput(session, "mdag_checkCRA", value = if (is.null(dag_text) || dag_text == "") ""
                        else dag_text)
  })

  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)

    if (uploaded_data$data_source == "bmi") {
      updateTextInput(session, "y_checkCRA", value = "bmi7")
      updateTextInput(session, "covs_checkCRA", value = "matage mated")
      updateTextInput(session, "r_checkCRA", value = "r")
      # Also update DAG input with current dag_text or clear if missing
      updateTextAreaInput(session, "mdag_checkCRA",
                          value = if (is.null(uploaded_data$dag_text) || uploaded_data$dag_text == "") ""
                          else uploaded_data$dag_text)
    } else {
      updateTextInput(session, "y_checkCRA", value = "")
      updateTextInput(session, "covs_checkCRA", value = "")
      updateTextInput(session, "r_checkCRA", value = "")
      # Clear DAG input on other data sources
      updateTextAreaInput(session, "mdag_checkCRA",
                          value = if (is.null(uploaded_data$dag_text) || uploaded_data$dag_text == "") ""
                          else uploaded_data$dag_text)
    }
  })

  # Set data_changed TRUE when any relevant input changes, to block output until button clicked
  observeEvent(
    c(input$y_checkCRA, input$covs_checkCRA, input$r_checkCRA, input$mdag_checkCRA),
    {
      data_changed(TRUE)
    }
  )

  observeEvent(input$go_checkCRA, {
    data_changed(FALSE)
  })

  checkcra <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a data set.")
    )
    testthat::evaluate_promise(
      midoc::checkCRA(
        input$y_checkCRA,
        input$covs_checkCRA,
        input$r_checkCRA,
        input$mdag_checkCRA
      )
    )$messages
  })

  # checkCRA function output
  output$checkcra <- renderPrint({
    if (data_changed()) {
      return(invisible())
    }
    req(input$go_checkCRA)
    cat(strwrap(paste(cat(checkcra(), "\n", fill = TRUE), collapse = "\n")))
  })

  output$post_output_text_checkCRA <- renderUI({
    if (data_changed()) {
      return(NULL)
    }
    req(input$go_checkCRA)
    req(uploaded_data$data_source)

    if (uploaded_data$data_source == "bmi") {
      tagList(
        hr(),
        div(
          # bmi specific post text output
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("The results indicate that CRA is valid in principle, conditional on ",
            "covariates mother’s age and education level.")
        )
      )
    } else {
      tagList(
        hr(),
        div(
          # generic post text output
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("") # blank for now as output explains itself well
        )
      )
    }
  })

}

# checkMI()  function app tab 4 --------------------------------------------


#  USER INTERFACE - checkMI() function app
checkMI_ui <- tagList(

  # App title
  titlePanel("Inspect multiple imputation model"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Enter covariates, seperated by a space. Ensure variables match column headings.",
           "Example input: <code>covariate_1 covariate_2 covariate_3</code>.")),
    p("The DAG is carried over from the draw DAG app.")
  ),
  hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      textInput(inputId = "dep_checkMI",
                label = "Partially observed variable",
                value = ""),

      textInput(inputId = "preds_checkMI",
                label = "Imputation model predictors, separated by a space",
                value = ""),

      textInput(inputId = "r_dep_checkMI",
                label = "Partially observed variable's missingness indicator",
                value = ""),

      textAreaInput(inputId = "mdag_checkMI",
                    label = "Directed acyclic graph",
                    value = ""),

      actionButton(inputId = "go_checkMI",
                   label = "Check MI is valid")
    ),

    # Main panel for displaying outputs
    mainPanel(

      # Output: Print result
      verbatimTextOutput(outputId = "checkMI"),
      uiOutput("post_output_text_checkMI")

    )
  )
)

checkMI_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)  # Tracks if data has changed and output should reset

  # Autofill dag from uploaded_data$dag_text anytime it changes
  observe({
    req(uploaded_data$dag_text)
    updateTextAreaInput(session, "mdag_checkMI", value = uploaded_data$dag_text)
  })

  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark that data has changed

    if (uploaded_data$data_source == "bmi") {
      updateTextInput(session, "dep_checkMI", value = "bmi7")
      updateTextInput(session, "preds_checkMI", value = "matage mated pregsize")
      updateTextInput(session, "r_dep_checkMI", value = "r")
      updateTextAreaInput(session, "mdag_checkMI", value = uploaded_data$dag_text)
    } else {
      updateTextInput(session, "dep_checkMI", value = "")
      updateTextInput(session, "preds_checkMI", value = "")
      updateTextInput(session, "r_dep_checkMI", value = "")
      updateTextAreaInput(session, "mdag_checkMI", value = uploaded_data$dag_text)
    }
  })

  # check formula input — if it has changed, reset output and wait for button click
  observeEvent(
    list(input$dep_checkMI, input$preds_checkMI, input$r_dep_checkMI),
    {
      data_changed(TRUE)  # Mark outputs should reset because inputs changed
    }
  )

  # Reset output if user edits DAG input
  observeEvent(input$mdag_checkMI, {
    data_changed(TRUE)
  })

  observeEvent(input$go_checkMI, {
    data_changed(FALSE)  # Clear the flag so outputs can render
  })

  checkmi <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a data set.")
    )
    testthat::evaluate_promise(
      midoc::checkMI(
        input$dep_checkMI,
        input$preds_checkMI,
        input$r_dep_checkMI,
        input$mdag_checkMI
      )
    )$messages
  })

  # checkMI function output
  output$checkMI <- renderPrint({
    if (data_changed()) {
      return(invisible())  # clear output if data has changed but button not clicked yet
    }
    req(input$go_checkMI)  # Wait for button click to show output
    cat(strwrap(paste(cat(checkmi(), "\n", fill = TRUE), collapse = "\n")))
  })

  output$post_output_text_checkMI <- renderUI({
    if (data_changed()) {
      return(NULL)  # clear UI output if data has changed but the button hasn't been clicked yet
    }
    req(input$go_checkMI)
    req(uploaded_data$data_source)

    # the following makes the post text output when bmi is selected dependent on input
    if (uploaded_data$data_source == "bmi") {
      preds_vec <- strsplit(input$preds_checkMI, "\\s+")[[1]]

      if (identical(preds_vec, c("matage", "mated", "pregsize"))) {
        tagList(
          hr(),
          div(
            # bmi and 'matage mated pregsize' input specific post text output
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p("The results indicate that MI is valid in principle (the missing data is random),",
              "conditional on predictors mother’s age, mother’s education level, and pregnancy size."),
            p(HTML("Try changing the model predictors to <code>matage mated bwt</code>"))
          )
        )
      } else if (identical(preds_vec, c("matage", "mated", "bwt"))) {
        tagList(
          hr(),
          div(
            # bmi and 'matage mated bwt' input specific post text output
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p("The results indicate that MI is not valid in principle (birth weight is",
              "not independent of bmi7. The missing data is no random), conditional on predictors",
              "mother’s age, mother’s education level, and birth weight."),
            p(HTML("This is because birth weight is a “collider”. It shares a common cause with",
                   "both BMI at age 7y and its missingness indicator, <code>r</code>. Hence conditioning on birth",
                   "weight opens a path between BMI at age 7 and its missingness."))
          )
        )
      } else {
        tagList(
          hr(),
          div(
            # bmi, misc input specific post text output
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p("")  # Blank text output for BMI other predictors
          )
        )
      }
    } else {
      tagList(
        hr(),
        div(
          # generic post text output
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("")  # Blank text output for other data sources
        )
      )
    }
  })

}

# checkModSpec() function app tab 5 ---------------------------------------------

#  USER INTERFACE - checkModSpec() function app
checkModSpec_ui <- tagList(

  # App title
  titlePanel("Inspect parametric model specification"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("If CRA and MI are valid in principle, the <code>midoc</code> function",
           "<code>checkModSpec()</code> can be used to",
           "explore the specification of the imputation model.")),
    p(HTML("The imputation model formula input should follow the style: ",
           "<code>partially missing variable ~ formula containing covariates</code>.")),
    p(HTML("Example input if partially missing variable has a linear relationship with covariate_1: ",
           "<code>partially missing variable ~ covariate_1 + covariate_2 + covariate_3</code>"))
  ),
  hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      textAreaInput(inputId = "formula_checkModSpec",
                    label = "Imputation Model Formula",
                    value = ""),

      selectInput(inputId = "family_checkModSpec",
                  label = "Imputation Model Family",
                  choices = c("gaussian(identity)","binomial(logit)"),
                  selected = ""),

      shiny::actionButton(inputId = "go_checkModSpec",
                          label = "inspect model")

    ),

    # Main panel for displaying outputs
    mainPanel(

      # plotOutput(outputId = "checkmodspec_plot"),        # plot output
      verbatimTextOutput(outputId = "checkmodspec"), # text output
      uiOutput("post_output_text_checkModSpec") # post output text

    )
  )
)

# SERVER - checkModSpec fucntion app
checkModSpec_server <- function(input, output, session) {

  data_changed <- reactiveVal(TRUE)  # Start TRUE, so output is hidden initially

  # Autofill formula and family when data source changes
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark data changed so outputs reset

    if (uploaded_data$data_source == "bmi") {
      updateTextAreaInput(session, "formula_checkModSpec",
                          value = "bmi7 ~ matage + mated + pregsize")  # autofill formula
      updateSelectInput(session, "family_checkModSpec",
                        selected = "gaussian(identity)")
    } else {
      updateTextAreaInput(session, "formula_checkModSpec", value = "")
      updateSelectInput(session, "family_checkModSpec", selected = "")
    }
  })

  # check formula or family inputs — if changed, reset output and wait for button click
  observeEvent(list(input$formula_checkModSpec, input$family_checkModSpec), {
    data_changed(TRUE)  # Mark outputs should reset because inputs changed
  })

  # ReactiveVal to store checkmodspec output - ONLY updated on button click
  checkmodspec_result <- reactiveVal(NULL)

  # ReactiveVal to store formula and family at button click
  stored_formula <- reactiveVal(NULL)
  stored_family <- reactiveVal(NULL)

  # When button clicked, update result by isolating inputs to prevent early firing
  observeEvent(input$go_checkModSpec, {
    data_changed(FALSE)  # Clear flag to allow output rendering

    # Run checkModSpec inside isolate to avoid auto reactivity before button click
    isolate({
      validate(
        need(!is.null(uploaded_data$df), "Please upload a data set.")
      )
      stored_formula(input$formula_checkModSpec)
      stored_family(input$family_checkModSpec)

      res <- testthat::evaluate_promise(
        midoc::checkModSpec(
          input$formula_checkModSpec,
          input$family_checkModSpec,
          uploaded_data$df,
          plot = FALSE
        )
      )$messages

      checkmodspec_result(res)  # Store result reactively
    })
  })

  # Render model check output after clicking button, else show nothing
  output$checkmodspec <- renderPrint({
    req(!data_changed())       # Only run if data_changed is FALSE (button clicked)
    req(input$go_checkModSpec) # And button must be clicked

    res <- checkmodspec_result()
    if (is.null(res)) return(invisible())

    cat(strwrap(paste(cat(res, "\n", fill = TRUE), collapse = "\n")))
  })

  # checkModSpec function output
  output$post_output_text_checkModSpec <- renderUI({
    req(!data_changed())
    req(input$go_checkModSpec)
    req(uploaded_data$data_source)

    if (uploaded_data$data_source == "bmi") {
      formula_trimmed <- gsub("\\s+", " ", trimws(stored_formula()))

      if (formula_trimmed == "bmi7 ~ matage + mated + pregsize") {
        tagList(
          hr(),
          div(
            # bmi 'bmi7 ~ matage + mated + pregsize' input specific post text output
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p("There is strong evidence that the imputation model is mis-specified."),
            p(HTML("<code>bmi7 ~ matage + mated + pregsize</code> assumes that child’s BMI",
                   "at age 7y has a linear relationship with mother’s age. Try changing the formula",
                   "to <code>bmi7 ~ matage + I(matage^2) + mated + pregsize</code> which assumes a",
                   "quadratic relationship."))
          )
        )
      } else if (formula_trimmed == "bmi7 ~ matage + I(matage^2) + mated + pregsize") {
        tagList(
          hr(),
          div(
            # bmi 'bmi7 ~ matage + I(matage^2) + mated + pregsize' input specific post text output
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p("There is little evidence that the imputation model is mis-specified (MI is valid).",
              "Note that we must account for the non-linear relationship between BMI at age 7y and",
              "mother’sage in all other imputation models."),
            p("For example, the imputation model for pregnancy size would need to include BMI",
              "at age 7y, mother’s education level, and a quadratic form of mother’s age.",
              "This non-linear relationship is induced by conditioning on BMI at age 7y.")
          )
        )
      } else {
        tagList(
          hr(),
          div(
            # bmi misc input specific post test output
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p("")  # blank text for any other formula when data_source is bmi
          )
        )
      }
    } else {
      tagList(
        hr(),
        div(
          # misc post text output
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("")  # blank text for non-bmi data sources
        )
      )
    }
  })

}

#  proposeMI() function tab 6 ---------------------------------------------

# USER INTERFACE - proposeMI() function app
proposeMI_ui <- tagList(

  # App title
  titlePanel("Suggest multiple imputation options"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",

    p(HTML("The imputation model formula input should follow the style: ",
           "<code>partially missing variable ~ formula containing covariates</code>.")),
    p(HTML("Example input if partially missing variable has a linear relationship with covariate_1: ",
           "<code>partially missing variable ~ covariate_1 + covariate_2 + covariate_3</code>")),
    p("Every word and symbol is separated by a single space.")
  ),
  hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      textAreaInput(inputId = "formula_proposeMI",
                    label = "Imputation Model formula",
                    value = ""),

      selectInput(inputId = "family_proposeMI",
                  label = "Imputation Model family",
                  choices = c("gaussian(identity)","binomial(logit)"),
                  selected = "gaussian(identity)"),

      actionButton(inputId = "go_proposeMI",
                   label = "Propose MI options")

    ),

    # Main panel for displaying outputs
    mainPanel(

      # Output: Print result
      verbatimTextOutput(outputId = "proposemi"),
      uiOutput("post_output_text_proposeMI")

    )
  )
)

# SERVER - proposeMI() function app
proposeMI_server <- function(input, output, session) {

  data_changed <- reactiveVal(TRUE)  # Start TRUE, so output hidden initially

  # autofill formula input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$formula_checkModSpec)) {
      updateSelectInput(session, "formula_proposeMI", selected = uploaded_data$formula_checkModSpec)
    }
  })

  # autofill family input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$family_checkModSpec)) {
      updateSelectInput(session, "family_proposeMI", selected = uploaded_data$family_checkModSpec)
    }
  })

  # Autofill inputs on data_source change, reset output
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark data changed to clear output/UI

    if (uploaded_data$data_source == "bmi") {
      updateTextAreaInput(session, "formula_proposeMI",
                          value = "bmi7 ~ matage + mated + pregsize")
      updateSelectInput(session, "family_proposeMI",
                        selected = "gaussian(identity)")
    } else {
      updateTextAreaInput(session, "formula_proposeMI", value = "")
      updateSelectInput(session, "family_proposeMI", selected = "")
    }
  })

  # check formula or family inputs — if changed, reset output and wait for button click
  observeEvent(list(input$formula_proposeMI, input$family_proposeMI), {
    data_changed(TRUE)  # Mark outputs should reset because inputs changed
  })

  # save formula input for auto input to later apps
  observeEvent(input$go_checkModSpec, {
    uploaded_data$formula_checkModSpec <- input$formula_checkModSpec
    data_changed(FALSE)
  })

  # save family input for auto input to later apps
  observeEvent(input$go_checkModSpec, {
    uploaded_data$family_checkModSpec <- input$family_checkModSpec
    data_changed(FALSE)
  })

  # ReactiveVal to store formula and family at button click
  stored_formula <- reactiveVal(NULL)
  stored_family <- reactiveVal(NULL)

  # Reset data_changed flag when "propose model" button clicked so output renders
  observeEvent(input$go_proposeMI, {
    data_changed(FALSE)

    # Store current inputs at button click
    stored_formula(input$formula_proposeMI)
    stored_family(input$family_proposeMI)
  })

  # Reactive dataset
  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a data set.")
    )
    uploaded_data$df
  })

  # CheckModSpec reactive (using stored inputs)
  mimod <- reactive({
    req(uploaded_data$df)
    req(stored_formula())
    req(stored_family())
    midoc::checkModSpec(stored_formula(), stored_family(), data(),
                        plot = FALSE, message = FALSE)
  })

  # ProposeMI reactive
  proposemi <- reactive({
    req(uploaded_data$df)
    req(mimod())
    testthat::evaluate_promise(
      midoc::proposeMI(mimod(), data(), plot = FALSE)
    )$messages
  })

  # Output the proposeMI output, only after go button clicked & if data not changed
  output$proposemi <- renderPrint({
    if (data_changed()) {
      return(invisible())  # Clear output if data changed but button not clicked
    }
    req(input$go_proposeMI)
    msgs <- proposemi()
    if (is.null(msgs) || length(msgs) == 0) {
      cat("No messages to display.")
    } else {
      cat(strwrap(paste(cat(msgs, "\n", fill = TRUE), collapse = "\n")))
    }
  })

  # Conditional post-output text, only after go button clicked & if data not changed
  output$post_output_text_proposeMI <- renderUI({
    if (data_changed()) {
      return(NULL)  # Clear UI if data changed but button not clicked
    }
    req(input$go_proposeMI)
    req(uploaded_data$data_source)

    if (uploaded_data$data_source != "bmi") {
      tagList(
        hr(),
        div(
          # generic post output text
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("Try fitting both linear and quadratic relationships between the partially",
            "missing variable and covariates to see how the results differ.")
        )
      )
    } else {
      formula_trimmed <- gsub("\\s+", " ", trimws(stored_formula()))

      if (formula_trimmed == "bmi7 ~ matage + mated + pregsize") {
        tagList(
          hr(),
          div(
            # bmi 'bmi7 ~ matage + mated + pregsize' input specific post text output
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p(HTML("<code>bmi7 ~ matage + mated + pregsize</code> assumes that child’s BMI",
                   "at age 7y has a linear relationship with mother’s age. Try changing the formula",
                   "to <code>bmi7 ~ matage + I(matage^2) + mated + pregsize</code> which assumes a",
                   "quadratic relationship."))
          )
        )
      } else if (formula_trimmed == "bmi7 ~ matage + I(matage^2) + mated + pregsize") {
        tagList(
          hr(),
          div(
            # bmi 'bmi7 ~ matage + I(matage^2) + mated + pregsize' input specific post output text
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p( "")
          )
        )
      } else {
        tagList(
          hr(),
          div(
            # bmi misc input post output text
            style = "margin-top: 15px; font-size: 14px; color: #333;",
            p("Try fitting both linear and quadratic relationships between the partially",
              "missing variable and covariates to see how the results differ.")
          )
        )
      }
    }
  })
}

# doMImice() function app tab 7 -------------------------------------------------

# USER INTERFACE - doMImice() function app
doMImice_ui <- tagList(

  # App title
  titlePanel("Perform multiple imputation using 'mice'"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",

    p(HTML("The imputation model formula input should follow the style: ",
           "<code>partially missing variable ~ formula containing covariates</code>.")),
    p(HTML("Example input if partially missing variable has a linear relationship with covariate_1: ",
           "<code>partially missing variable ~ covariate_1 + covariate_2 + covariate_3</code>")),
    p("Every word and symbol is separated by a single space. The substantive model will autofill from",
      "the imputation model formula.")
  ),
  hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      textAreaInput(inputId = "impformula",
                    label = "Imputation Model formula",
                    value = ""),

      selectInput(inputId = "impfamily",
                  label = "Imputation Model family",
                  choices = c("gaussian(identity)","binomial(logit)"),
                  selected = "gaussian(identity)"),

      textAreaInput(inputId = "substmod",
                    label = "Substantive Model",
                    value = "lm()"),

      numericInput(inputId = "seed",
                   label = "Set the seed of the 'mice' call",
                   value = 123),

      actionButton(inputId = "go_doMImice",
                   label = "Perform MI")

    ),

    # Main panel for displaying outputs
    mainPanel(verbatimTextOutput(outputId = "domimice"), # domomice output
              uiOutput("post_output_text_doMImice") # post output text
    )
  )
)

# SERVER - doMImice function app
doMImice_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)  # Track data changes to reset outputs

  # autofill formula input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$formula_checkModSpec)) {
      updateSelectInput(session, "formula_proposeMI", selected = uploaded_data$formula_checkModSpec)
    }
  })

  # autofill family input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$family_checkModSpec)) {
      updateSelectInput(session, "family_proposeMI", selected = uploaded_data$family_checkModSpec)
    }
  })

  # if data source is bmi, autofill input, otherwise leave them blank
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # mark data changed to clear output/UI

    if (uploaded_data$data_source == "bmi") {
      updateTextAreaInput(session, "impformula",
                          value = "bmi7 ~ matage + I(matage^2) + mated + pregsize")
      updateSelectInput(session, "impfamily",
                        selected = "gaussian(identity)")
      updateTextAreaInput(session, "substmod",
                        value = "lm(bmi7 ~ matage + I(matage^2) + mated + pregsize)")
    } else {
      updateTextAreaInput(session, "impformula", value = "")
      updateSelectInput(session, "impfamily", selected = "")
      updateTextAreaInput(session, "substmod", value = "")
      updateNumericInput(session, "seed", value = NULL)
    }
  })

  # Reset output if these inputs change
  observeEvent(list(input$impformula, input$impfamily, input$substmod, input$seed), {
    data_changed(TRUE)
  })

  # Clear data_changed flag when go button clicked
  observeEvent(input$go_doMImice, {
    data_changed(FALSE)
  })

  # prompt if no data
  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a data set.")
    )
    uploaded_data$df
  })

  #checkMedSpec() function
  mimod <- reactive(midoc::checkModSpec(
    input$impformula, input$impfamily, data(), plot = FALSE, message = FALSE
  ))

  # proposeMI() function
  miprop <- reactive(midoc::proposeMI(
    mimod(), data(), plot = FALSE, message = FALSE
  ))

  # doMImice() function
  domimice <- reactive({
    testthat::evaluate_promise(
      midoc::doMImice(miprop(), input$seed, input$substmod)
    )$messages
  })

  #output
  output$domimice <- renderPrint({
    if (data_changed()) {
      return(invisible())  # clear output if data changed but button not clicked
    }
    req(input$go_doMImice)
    cat(strwrap(paste(cat(domimice(), "\n", fill = TRUE), collapse = "\n")))
  })

  # conditional text under the output
  output$post_output_text_doMImice <- renderUI({
    if (data_changed()) {
      return(NULL)  # clear UI if data changed but button not clicked
    }
    req(input$go_doMImice)
    req(uploaded_data$data_source)

    if (uploaded_data$data_source == "bmi") {
      tagList(
        hr(),
        div(
          # bmi specific post text output
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("CRA and MI estimates (fitting a quadratic relationship between BMI at age 7",
            "years and maternal age in the imputation model) are similar.")
        )
      )
    } else {
      tagList(
        hr(),
        div(
          # generic post text output
          style = "margin-top: 15px; font-size: 14px; color: #333;",
          p("Try fitting both linear and quadratic relationships between the partially",
            "missing variable and covariates to see how the results differ.")
        )
      )
    }
  })
}

# master UI ---------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-size: 12pt;
      }
      code, code.r, pre {
        background-color: #white;
        color: black;
        font-family: monospace;
        padding: 2px 4px;
        border-radius: 4px;
      }
      caption {
        color: black;
        font-weight: bold;
      }
      a {
        color: #337ab7;
      }
      #toc {
        position: fixed;
        top: 100px;
        left: 10px;
        width: 220px;
        background: white;
        border: 1px solid #ddd;
        padding: 10px 15px;
        border-radius: 4px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.1);
        font-size: 13pt;
        max-height: 80vh;
        overflow-y: auto;
        color: black;
      }
      #main-content {
        padding: 10px 30px 30px 30px;
      }
      #toc h4 {
        margin-top: 0;
        font-weight: bold;
      }
      #toc ul {
        padding-left: 15px;
      }
      #toc ul li {
        margin-bottom: 8px;
      }
      #main-content p.authors {
        font-size: 13pt;
      }
    "))
  ),

  # #floating TOC headers wiht links
  # div(id = "toc",
  #     tags$ul(
  #       style = "list-style-type: none; padding-left: 0; margin: 0;",
  #       tags$li(a(href = "#about-midoc", "About midoc")),
  #       tags$li(a(href = "#how-to-use", "How to use these apps")),
  #       tags$li(a(href = "#interactive-midoc", "Interactive midoc apps"))
  #     )
  # ),

  div(id = "main-content",

      tags$h1(
        HTML('Multiple Imputation DOCtor (<a href="https://elliecurnow.github.io/midoc/" target="_blank">midoc</a>)')
      ),

      tags$p(
        HTML('
        Elinor Curnow, Jon Heron, Rosie Cornish, Kate Tilling, James Carpenter, and
        Holly Taylor')
      ),

      tags$hr(),

      tags$h2(id = "about-midoc", "About midoc"),
      tags$p("A guidance system for analysis with missing data. It incorporates expert, up-to-date",
      "methodology to help researchers choose the most appropriate analysis approach when some data",
      "are missing. "),

      tags$p(
        HTML(
          ' <code>midoc</code> follows the framework for the treatment and reporting of missing data',
          'in observational studies <a href="https://doi.org/10.1016/j.jclinepi.2021.01.008"',
          'target="_blank">TARMOS</a>).')
      ),

      tags$h2(id = "how-to-use", "How to use these apps"),
      tags$p(HTML('First, specify the dataset you want to analyse. Select one of the three',
                  'datasets included in the <code>midoc</code> package (body mass index – BMI;',
                  'randomised controlled trial – RCT; administrative data – ADR)  or upload your own data.' ,
                  'All uploaded datasets must be in .csv format.')),

         p(HTML('<strong>Note:</strong> This application is hosted on shiny.io, a third-party platform.',
                'Do not upload confidential or sensitive data. To analyse data that is not publicly ',
                'available, run the <code>midoc</code> apps locally in R, download <code>midoc</code> ',
                'version <code>1.0.0</code> from <a href="https://cloud.r-project.org/web/packages/midoc/index.html" ',
                'target="_blank">CRAN</a> and run command <code>midocVignette()</code>')),

      tags$p(HTML('Next, specify your "missingness" directed acyclic graph (mDAG) for the analysis model as',
                  'if there were no missing data.',
                  'You may find <a href="https://doi.org/10.1038/s41390-018-0071-3" target="_blank">this</a> ',
                  'introduction to DAGs useful. Then add missingness indicator(s) to your DAG.',
                  ' If you have multiple variables with mising data, start by including just ',
                  'the complete records indicator in your DAG.')),

      p(HTML('For guidance on how toidentify variables related to missingness, consult the,',
      '<a href="https://elliecurnow.github.io/midoc/" target="_blank"><code>midoc</code> website</a>.')),

      # data upload app UI
      tags$hr(),
      tags$h3("Upload Dataset"),
      data_ui,

      # dag app UI
      tags$hr(),
      drawDAG_ui,

      # Tabbed section for interactive apps
      tags$h2(id = "interactive-midoc", "Interactive midoc apps"),

      tabsetPanel(type = "pills", id = "midoc_tabs",
                  tabPanel("descMissData" , descMissData_ui),
                  tabPanel("exploreDAG", exploreDAG_ui),
                  tabPanel("checkCRA", checkCRA_ui),
                  tabPanel("checkMI", checkMI_ui),
                  tabPanel("checkModSpec", checkModSpec_ui),
                  tabPanel("proposeMI", proposeMI_ui),
                  tabPanel("doMImice", doMImice_ui)
      ),
      br()
  ))



# master server -----------------------------------------------------------

server <- function(input, output, session) {

  # data upload app server
  data_server(input, output, session)

  # DAG app server
  drawDAG_server(input, output, session)

  #descMissData() function app
  descMissData_server(input, output, session)

  # exploreDAG() function app
  exploreDAG_server(input, output, session)

  # checkCRA() function app
  checkCRA_server(input, output, session)

  # checkMI() function app
  checkMI_server(input, output, session)

  # checkModSpec() function app
  checkModSpec_server(input, output, session)

  # proposeMI() function app
  proposeMI_server(input, output, session)

  # doMImice() function app
  doMImice_server(input,output, session)

}

# App function -----------------------------------------------------------
shinyApp(ui, server)




# run app -----------------------------------------------------------------


shiny::runApp(midocApp)

# To do next --------------------------------------------

#add explanation/worked example for other dataset in package, similar to bmi
#
# upload to shiny.io so anyone with a link can open

# Done -----------------------------------------------------------------

# add text above apps about format for inputs
#
#compile all apps into one html
#
# put apps in multitab format
#
# change tabe name, app title, button text etc.
#
#change theme of toc to be more streamlined
#
# add some linnks to titles and names
#
#make data upload app a dropdown menu for upload, bmi or other package dataset
#
#figure out how to call package dataset (e.g. bmi) instead of having to upload it.
# and add this as an option alongside uploading data
#
# Add an app that allows the data set to be uploaded/selected once and input into the following apps.
#
# change font to sans serif for accessibility
#
# fix consecutive data uploads from causign ouput erros in function apps.
#
# test apps with new data set
#
# add dag input
#
#create app script
#
#transfer text and setup to app scritps
#
#add floating toc to app script
#
# add css to make app text , title, toc etc look like a webpage
#
# add data upload app to app script
#
# add sepcify dag app to app script
#
# add tabs ot app script
#
# add ddescmiss data to app script
#
# add explore dag app to app script
#
# change all post output text to app specific names now theyre in the same chunk
#
#make midoc run from github verison so new dataset can be used
#
# figure out how to get rid of scrolling for embedded tab apps.
#
#figure out what happened to dag post ouput text
#
# descmissdata is generating outut before button is clicked when bmi input is autofilled. stop that.
#
# wipe all inputs and outputs in subsequent apps when a new dataset is uploaded or selected
#
# fix titles messed up by app script
#
# Add text that shows up when you run a data set. if a new data set, make it an explanation
# of how to interpret the output. If one of the example data sets, make it an in
# depth explanation of how the function generated that output from the data
#
# last app dag issues
#
#make sure all apps reset when input is changed
#
#tidy comments
#
# remove substanive model autofill
#
# string wrap explore dag output to remove scrolling

