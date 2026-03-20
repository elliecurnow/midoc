# Set up ------------------------------------------------------------------

# install.packages("shiny")
# install.packages("devtools")

# library(devtools) #  allows the next line of code
# install_github("elliecurnow/midoc", dependencies = TRUE) # link to github version instead of CRAN

library(midoc)
library(shiny) # makes embedded apps

# Reactive values
uploaded_data <- reactiveValues(
  #uploaded_data$
    df = NULL,
  #uploaded_data$
  data_source = NULL,
  #uploaded_data$
  dag_text = NULL
)

uploaded_data$df <- midoc::qol  # store qol dataframe
uploaded_data$data_source <- "qol" # record source as qol

#uploaded_data$dag_text <- # RCT autofill dag input
  dag_text <- paste(
    "dag {",
    "age0 -> qol0",
    "group -> qol12",
    "group -> qol3",
    'qol0 -> qol12 [pos="-0.559,-1.300"]',
    "qol0 -> qol3",
    "qol0 -> r",
    "qol3 -> qol12",
    "qol3 -> r",
    'age0 [pos="-2.174,-1.081"]',
    'group [pos="-2.117,1.021"]',
    'qol0 [pos="-2.174,-0.587"]',
    'qol12 [pos="0.381,-0.587"]',
    'qol3 [pos="-0.891,-0.587"]',
    'r [pos="0.381,1.021"]',
    '}',
    sep = "\n"
  )

# descMissData() function app tab 1 ---------------------------------------------

# USER INTERFACE - descMissData() function app
descMissData_ui <- fluidPage(tagList(

  # App title
  titlePanel("List missing data patterns"),

  # text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Use the <strong>midoc</strong> function <strong>descMissData</strong> to list",
           "the missing data patterns ",
           "in the dataset.")),
    hr(),
    p(HTML('<strong>Example R code:</strong> <br>midoc::descMissData(y=“qol12", covs=“age0 qol0 qol3", data=qol)'))
  #  p("Your analysis model outcome/variable of primary interest."),
  #  p(HTML("When listing covariates, seperate them by a space. Example input:",
  #         "<code>covariate_1 covariate_2 covariate_3</code>.")),
  #  p("Make sure variables entered are spelt the same as in the dataset.")
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,

      textInput(inputId = "y_descMissData",
                label = "Analysis model outcome/variable of primary interest",
                value = " "),

      textInput(inputId = "covs_descMissData",
                label = "Analysis model covariates/other variables, separated by a space",
                value = " "),

      textInput(inputId = "group_descMissData",
                label = "Treatment group variable",
                value = " "),

      actionButton(inputId = "go_descMissData",
                   label = "List missing data patterns")

    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      uiOutput("pre_output_text_descMissData"),  # dynamic text above text output
      verbatimTextOutput(outputId = "descmissdata_print"), #  text output
      uiOutput("post_output_text_descMissData"),  # dynamic text below text output
      plotOutput(outputId = "descmissdata", width="80%"), # plot output

      uiOutput("pre_output_text_descMissDataby"),  # dynamic text above text output
      verbatimTextOutput(outputId = "descmissdata_printby"), #  text output
      #plotOutput(outputId = "descmissdata0", width="80%"), # plot output
    )
  )
))

# SERVER - descMissData() function app

descMissData_server <- function(input, output, session) {

    #reactive({
  #  validate(
  #    need(!is.null(uploaded_data$df), "Please upload a dataset")
  #  )
  #  uploaded_data$df
  #})

  # Reactive flag to track data changes and reset plot
  data_changed <- reactiveVal(FALSE)

  # if data source is bmi, autofill input, otherwise, leave them blank
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark that data has changed (so plot should clear)

  #  if (uploaded_data$data_source == "bmi") {
  #    updateTextInput(session, "y_descMissData", value = "bmi7")
  #    updateTextInput(session, "covs_descMissData", value = "matage mated pregsize bwt")
  #  } else {
      updateTextInput(session, "y_descMissData", value = "qol12")
      updateTextInput(session, "covs_descMissData", value = "age0 qol0 qol3")
      updateTextInput(session, "group_descMissData", value = "group")
 #   }
  })

  # Set data_changed TRUE when relevant inputs change, to block output until button clicked
  observeEvent(c(input$y_descMissData, input$covs_descMissData, input$group_descMissData), {
    data_changed(TRUE)
  })

  # Save input when button is clicked
  observeEvent(input$go_descMissData, {
    data_changed(FALSE)  # Clear the data changed flag so plot can render
  })

  #Pre output text
  output$pre_output_text_descMissData <- renderUI({
    if (data_changed()) {
      # Clear UI output if data changed but not drawn yet
      return(NULL)
    }
    req(input$go_descMissData)
    req(uploaded_data$data_source)  # Make sure data_source exists

    #Only print plot and postscript if no error
    tryCatch({midoc::descMissData(
      y = input$y_descMissData,
      covs = input$covs_descMissData,
      data = uploaded_data$df,
      plot = FALSE
    )

      tagList(
        #hr(),
        div(
          # generic post output text for descMissData
          style = "margin-top: 0px; font-size: 14px; color: #333; font-weight: bold;",
          p("Overall")
        )
      )
    },
    error = function(e) {""}
    )
  })

  output$pre_output_text_descMissDataby <- renderUI({
    if (data_changed()) {
      # Clear UI output if data changed but not drawn yet
      return(NULL)
    }
    req(input$go_descMissData)
    req(input$group_descMissData)
    req(uploaded_data$data_source)  # Make sure data_source exists

    #Only print plot and postscript if no error
    tryCatch({midoc::descMissData(
      y = input$y_descMissData,
      covs = input$covs_descMissData,
      by = input$group_descMissData,
      data = uploaded_data$df,
      plot = FALSE
    )

    tagList(
        #hr(),
        div(
          # generic post output text for descMissData
          style = "margin-top: 0px; font-size: 14px; color: #333; font-weight: bold;",
          p("By treatment group")
        )
      )
    },
    error = function(e) {""}
    )
  })



  # descMissData() function for text output
output$descmissdata_print <- renderPrint({
  req(input$go_descMissData)
  if (data_changed()) {
    return(invisible())
  }
  #req(data())
  tryCatch({
    midoc::descMissData(
      y = input$y_descMissData,
      covs = input$covs_descMissData,
      data = uploaded_data$df,
      plot = FALSE)
  }, error = function(e) {
    e$message
  })
})

  output$descmissdata_printby <- renderPrint({
    req(input$go_descMissData)
    req(input$group_descMissData)
    if (data_changed()) {
      return(invisible())
    }
    #req(data())
    tryCatch({
      midoc::descMissData(
        y = input$y_descMissData,
        covs = input$covs_descMissData,
        by = input$group_descMissData,
        data = uploaded_data$df,
        plot = FALSE)
    }, error = function(e) {
      e$message
    })
  })

  # descMissData() function for plot output
  output$descmissdata <- renderPlot({
    req(input$go_descMissData)
    if (data_changed()) {
      return(invisible())
    }
    #req(data())

    #Only print plot and postscript if no error
    tryCatch(midoc::descMissData(
      y = input$y_descMissData,
      covs = input$covs_descMissData,
      data = uploaded_data$df,
      plot = TRUE
    ),
    error = function(e) {""}
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

    #Only print plot and postscript if no error
    tryCatch({midoc::descMissData(
      y = input$y_descMissData,
      covs = input$covs_descMissData,
      data = uploaded_data$df,
      plot = FALSE
    )

      tagList(
        #hr(),
        div(
          # generic post output text for descMissData
          style = "margin-top: 0px; font-size: 14px; color: #333; font-style: normal;",
          p("1 = observed and 0 = missing")
        )
      )
    },
    error = function(e) {""}
    )
  })
}

# summMissData() function app tab 2 ---------------------------------------------

# USER INTERFACE - descMissData() function app
summMissData_ui <- fluidPage(tagList(

  # App title
  titlePanel("Summarise missing data"),

  # text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Summarise the data by treatment group and missing data pattern.",
           "The mean and standard deviation of the observed data",
           #"as well as the number and percentage of missing data ",
           "are displayed.")),
    hr(),
    p(HTML('<strong>Example R code:</strong>',
    '<br>aggregate(qol[,c("age0", "qol0", "qol3", "qol12")], by=list(Response_12m=qol[,"r"],Group=qol[,"group"]),',
    '<br>function(x) c(Mean = round(mean(x,na.rm=T)), SD = round(sd(x,na.rm=T))))'))
    #  p("Your analysis model outcome/variable of primary interest."),
    #  p(HTML("When listing covariates, separate them by a space. Example input:",
    #         "<code>covariate_1 covariate_2 covariate_3</code>.")),
    #  p("Make sure variables entered are spelled the same as in the dataset.")
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,
      textInput(inputId = "y_summMissData",
                label = "Analysis model outcome/variable of primary interest",
                value = " "),

      textInput(inputId = "covs_summMissData",
                label = "Analysis model covariates/other variables, separated by a space",
                value = " "),

      textInput(inputId = "group_summMissData",
                label = "Treatment group variable",
                value = " "),

      textInput(inputId = "r_summMissData",
                label = "Complete record indicator",
                value = ""),

      actionButton(inputId = "go_summMissData",
                   label = "Summarise missing data")

    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      verbatimTextOutput(outputId = "summmissdata"), #  text output
    )
  )
))

# SERVER - summMissData() function app

summMissData_server <- function(input, output, session) {

  #reactive({
  #  validate(
  #    need(!is.null(uploaded_data$df), "Please upload a dataset")
  #  )
  #  uploaded_data$df
  #})

  # Reactive flag to track data changes and reset plot
  data_changed <- reactiveVal(FALSE)

  # if data source is bmi, autofill input, otherwise, leave them blank
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark that data has changed (so plot should clear)

    #  if (uploaded_data$data_source == "bmi") {
    #    updateTextInput(session, "y_descMissData", value = "bmi7")
    #    updateTextInput(session, "covs_descMissData", value = "matage mated pregsize bwt")
    #  } else {
    updateTextInput(session, "y_summMissData", value = "qol12")
    updateTextInput(session, "covs_summMissData", value = "age0 qol0 qol3")
    updateTextInput(session, "group_summMissData", value = "group")
    updateTextInput(session, "r_summMissData", value = "r")
    #   }
  })

  # Set data_changed TRUE when relevant inputs change, to block output until button clicked
  observeEvent(c(input$y_summMissData, input$covs_summMissData, input$group_summMissData, input$r_summMissData), {
    data_changed(TRUE)
  })

  # Save input when button is clicked
  observeEvent(input$go_summMissData, {
    data_changed(FALSE)  # Clear the data changed flag so plot can render
  })

  # summMissData() text output
  output$summmissdata <- renderPrint({
    req(input$go_summMissData)
    if (data_changed()) {
      return(invisible())
    }
    #req(data())
    tryCatch({
      stats::aggregate(uploaded_data$df[,c(unlist(strsplit(input$covs_summMissData," ")),input$y_summMissData)],
                by=list(Response_12m=uploaded_data$df[,input$r_summMissData],Group=uploaded_data$df[,input$group_summMissData]),
                function(x) c(Mean = round(mean(x,na.rm=T)), SD = round(sd(x,na.rm=T))))
                #miss.n=round(sum(is.na(x))), miss.pct=round(100*sum(is.na(x))/sum(x))))
    }, error = function(e) {
      e$message
    })
  })
}

# drawDAG() function app tab 4 ---------------------------------------------------

# USER INTERFACE  - Draw DAG app
drawDAG_ui <- fluidPage(

# app title
titlePanel("Specify mDAG"),

# text above the UI
div(
  style = "margin-bottom: 20px; font-size: 14px; color: #333;",
  tags$p(HTML('Specify the assumed "missingness" directed acyclic graph (mDAG).',
              '<br> <br>There are two ways to do this:',
              '<br> 1. Draw your mDAG at the <a href="https://dagitty.net/dags.html" target="_blank">dagitty website</a>',
              'and copy the "Model code" on the right-hand side of the screen into the box below.',
              '<br>2. Write the assumed causal relationships between variables in the box',
              'below using "dagitty" syntax.',
              '<br>Example input: the code for "(treatment) group causes qol12m" is  <code>group -> qol12m</code>.',
              'You do not need to include positional (pos[ ]) statements, which control the layout of the mDAG.',
              'If omitted, a random layout will be used.',

              "<br> <br>Your mDAG should include all analysis model variables, plus",
              "any other variables that are related to the analysis model variables",
              'or their "missingness" (i.e. their probability of being missing).',

              'For more tips, see the <strong>midoc</strong>',
              '<a href="https://elliecurnow.github.io/midoc/articles/midoc.html" target="_blank">vignette</a>.',

              '<br> <br> Note your mDAG specification will be copied over to the other functions in this app.'))),

  hr(),

  sidebarLayout(
    sidebarPanel(
      textAreaInput(inputId = "mdag_drawDAG",
                    label = "mDAG",
                    value = " "),
      selectInput(inputId = "mdag_layout",
                  label = "Use random layout?",
                  choices = c("TRUE","FALSE"),
                  selected = "FALSE"),
      actionButton(inputId = "go_drawDAG",
                   label = "Draw mDAG")
    ),

    mainPanel(
      plotOutput("mdagplot", width="80%"),
      uiOutput("post_output_text_drawDAG")
    )
  )
)

# SERVER - Draw DAG app
drawDAG_server <- function(input, output, session) {

# Reactive flag to track data changes and reset plot
 data_changed <- reactiveVal(FALSE)  # tracks whether new data/DAG was uploaded

# if data source is package dataset, autofill dag to input
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark that data has changed (so plot should clear)

#    if (uploaded_data$data_source == "bmi") {
#     updateTextAreaInput(session, "mdag_drawDAG", value = bmi_dag_text)
#      uploaded_data$dag_text <- bmi_dag_text
#    }
#    else if (uploaded_data$data_source == "qol") {
      updateTextAreaInput(session, "mdag_drawDAG", value = dag_text)
      uploaded_data$dag_text <- dag_text
#    } else if (uploaded_data$data_source == "adr") {
#      updateTextAreaInput(session, "mdag_drawDAG", value = adr_dag_text)
#      uploaded_data$dag_text <- adr_dag_text
#    } else {
#      #if (uploaded_data$data_source == "upload") {
#      updateTextAreaInput(session, "mdag_drawDAG", value = gen_dag_text)
#      uploaded_data$dag_text <- gen_dag_text
#    }
  })

# Set data_changed to TRUE when DAG input changes, to block output until button clicked
  observeEvent(input$mdag_drawDAG, {
    data_changed(TRUE)
  })
  observeEvent(input$mdag_layout, {
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
      dagitty::dagitty(input$mdag_drawDAG, layout=input$mdag_layout)
  })

# Draw DAG plot only after "draw DAG" button is clicked and data unchanged
  output$mdagplot <- renderPlot({
    if (data_changed()) {
#      # Data has changed but draw button not clicked yet — clear plot
      return(NULL)
    }
    req(dag_spec())  # only run after button click

#Plot and postscript only if no error
    tryCatch({
        plot(dag_spec())
      }, error = function(e) {""}
    )
  })

  #observeEvent(input$go_drawDAG, {
  #  withCallingHandlers(
  #    plot(dag_spec()),
  #    message = function(m) output$post_output_text_drawDAG <- renderUI({
  #      if (data_changed()) {
          #Clear UI output if data changed but not drawn yet
  #        return(NULL)
  #      }
  #      m$message
  #      })
  #  )
  #})

#conditional text under output for draw DAG app
  output$post_output_text_drawDAG <- renderUI({
    if (data_changed()) {
#Clear UI output if data changed but not drawn yet
      return(NULL)
    }

    req(input$go_drawDAG)
    req(uploaded_data$data_source)  # Make sure data_source exists#

    tryCatch({
      plot(dag_spec())
      tagList(
        div(
          # generic post output text for Draw DAG
          style = "margin-top: 15px; font-size: 14px; color: #333;",

          p(HTML("Visually check that the relationships are specified as you intended."))))
    }, error = function(e) {""}, message = function(m) {"Plot coordinates for graph not supplied! Generating coordinates..."}, warning = function(w) {""})
  })
}

# exploreDAG() function app tab 4 -----------------------------------------------

# USER INTERFACE - exploreDAG() function app
exploreDAG_ui <- fluidPage(tagList(

  # App title
  titlePanel("Explore the proposed DAG"),

  # add text above the UI
  div(
    style = "auto; word-wrap:break-word; margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Use the <strong>midoc</strong> function <strong>exploreDAG</strong> to explore",
           "the specification of your proposed mDAG.",
           "Optionally, you can also use the supplied dataset to explore whether the observed relationships",
           "in the dataset are consistent with your proposed mDAG.")),
    hr(),
    p(HTML('<strong>Example R code:</strong>',
            '<br>qoldag <- "group -> qol12 group -> qol3 age0 -> qol0 qol0 -> qol3',
           'qol0 -> qol12 qol3 -> qol12 qol0 -> r qol3 -> r"',
           '<br>midoc::exploreDAG(mdag=qoldag, data=qol)'))

  #  p("The DAG is carried over from the draw DAG app.")
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=3,

      textAreaInput(inputId = "mdag_exploreDAG",
                    label = "mDAG",
                    value = ""),

      selectInput(inputId = "data_exploreDAG",
                  label = "Use data?",
                  choices = c("TRUE","FALSE"),
                  selected = "TRUE"),

      actionButton(inputId = "go_exploreDAG",
                   #style = "height: 60px",
                   #width = "100%",
                   label = "Compare data with mDAG")

    ),

    # Main panel for displaying outputs
    mainPanel(

      #div(style = "word-wrap:break-word; margin-bottom: 0 auto;"),

      width = 12,

      # Output: Print result
      verbatimTextOutput(outputId = "exploredag"),
      #uiOutput("post_output_text_exploreDAG")#not required

    )
  )
))

# SERVER exploreDAG() function app
exploreDAG_server <- function(input, output, session) {

  # Reactive flag to track data changes and reset output
  data_changed <- reactiveVal(FALSE)  # tracks whether new data/DAG was uploaded

  # Reactive dataset
  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a dataset")
    )
    uploaded_data$df
  })

  # Reactive DAG text with validation for non-empty text
  #dag_text <- reactive({
  #  validate(
  #    need(!is.null(uploaded_data$dag_text) && nzchar(trimws(uploaded_data$dag_text)),
  #         "Please specify mDAG")
  #  )
  #  uploaded_data$dag_text
  #})


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

  observeEvent(input$data_exploreDAG, {
    data_changed(TRUE)
  })

  # Clear the data_changed flag when button is clicked
  observeEvent(input$go_exploreDAG, {
    data_changed(FALSE)
  })

  # exploreDAG() function
  exploredag_result <- eventReactive(input$go_exploreDAG, {
    #req(data())
    req(dag_text())

    if (input$data_exploreDAG=="TRUE"){
      tryCatch({
        testthat::evaluate_promise(midoc::exploreDAG(input$mdag_exploreDAG, data()))$messages
      }, error = function(e) {
        e$message
      })
    } else {
      tryCatch({
        testthat::evaluate_promise(midoc::exploreDAG(input$mdag_exploreDAG))$messages
      }, error = function(e) {
        e$message
      })
    }
  })

  # exploreDAG function output
  output$exploredag <- renderPrint({
    if (data_changed()) return(invisible())  # prevent output if data changed but button not clicked
    req(exploredag_result())  # only run after button click
    cat(strwrap(paste(cat(exploredag_result(),"\n",fill=TRUE), collapse = "\n")))
  })

  # conditional text below output
  #output$post_output_text_exploreDAG <- renderUI({
    #if (data_changed()) return(NULL)  # prevent UI text if data changed but button not clicked
    #req(uploaded_data$data_source)  # Make sure data_source exists

    #if (uploaded_data$data_source == "bmi") {
    #  tagList(
    #    r(),
    #    div(
          # bmi specifc post output text exploreDAG
    #      style = "margin-top: 0px; font-size: 14px; color: #333;",
    #      p("There is little evidence against the null hypothesis that the stated ",
    #        "variables are (conditionally) independent.")
    #    )
    #  )
    #} else {
      #tagList(
        #hr(),
        #div(
          # generic post output text for exploreDAG
          #style = "margin-top: 15px; font-size: 14px; color: #333;",
          #p("") # this is blank currently because the output explains how to interpret it well on its own
        #)
     # )
    #}
  #})
}

# checkCRA() function app  tab 5-------------------------------------------------

# USER INTERFACE - checkCRA() function app
checkCRA_ui <- fluidPage(tagList(

  # App title
  titlePanel("Inspect complete records analysis model"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Use the <strong>midoc</strong> function <strong>checkCRA</strong> to explore whether",
           "a complete records analysis is valid.")),
    hr(),
    p(HTML('<strong>Example R code:</strong>',
           '<br>qoldag <- "group -> qol12 group -> qol3 age0 -> qol0 qol0 -> qol3',
           'qol0 -> qol12 qol3 -> qol12 qol0 -> r qol3 -> r"',
           '<br>midoc::checkCRA(y="qol12", covs="group age0 qol0", r_cra="r", mdag=qoldag)'))

  #  p("Enter covariates seperated by a space. Ensure variable names match column headings."),
  #  p(HTML("Example input: <code>covariate_1 covariate_2 covariate_3</code>."))
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,

      textInput(inputId = "y_checkCRA",
                label = "Analysis model outcome(s), separated by a space",
                value = ""),

      textInput(inputId = "covs_checkCRA",
                label = "Analysis model covariate(s), separated by a space",
                value = ""),

      textInput(inputId = "r_checkCRA",
                label = "Complete record indicator",
                value = ""),

      textAreaInput(inputId = "mdag_checkCRA",
                    label = "mDAG",
                    value = ""),

      actionButton(inputId = "go_checkCRA",
                   label = "Check CRA is valid")
    ),

    # Main panel for displaying outputs
    mainPanel(

      width = 12,

      # Output: Print result
      verbatimTextOutput(outputId = "checkcra"), # CRA ouput
      #uiOutput("post_output_text_checkCRA") # post output text

    )
  )
))

# SERVER - checkCRA() function app
checkCRA_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)

  # Autofill DAG from uploaded_data$dag_text
  observe({
    if (!is.null(uploaded_data$dag_text)) {
      updateTextAreaInput(session, "mdag_checkCRA", value = uploaded_data$dag_text)
    }
  })
  #observe({
  #  dag_text <- uploaded_data$dag_text
  #  updateTextAreaInput(session, "mdag_checkCRA", value = if (is.null(dag_text) || dag_text == "") ""
  #                      else dag_text)
  #})

  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)

    #if (uploaded_data$data_source == "bmi") {
    #  updateTextInput(session, "y_checkCRA", value = "bmi7")
    #  updateTextInput(session, "covs_checkCRA", value = "matage mated")
    #  updateTextInput(session, "r_checkCRA", value = "r")
      # Also update DAG input with current dag_text or clear if missing
    #  updateTextAreaInput(session, "mdag_checkCRA",
    #                      value = if (is.null(uploaded_data$dag_text) || uploaded_data$dag_text == "") ""
    #                      else uploaded_data$dag_text)
    #} else {
      updateTextInput(session, "y_checkCRA", value = "qol12")
      updateTextInput(session, "covs_checkCRA", value = "group age0 qol0")
      updateTextInput(session, "r_checkCRA", value = "r")
      # Clear DAG input on other data sources
      updateTextAreaInput(session, "mdag_checkCRA",
                          value = if (is.null(uploaded_data$dag_text) || uploaded_data$dag_text == "") ""
                          else uploaded_data$dag_text)
   # }
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

  # checkCRA function
  checkcra <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a dataset")
    )
    tryCatch({
      testthat::evaluate_promise(
        midoc::checkCRA(
          input$y_checkCRA,
          input$covs_checkCRA,
          input$r_checkCRA,
          input$mdag_checkCRA
        )
      )$messages
    }, error = function(e) {
      e$message
    })
  })

  # checkCRA function output
  output$checkcra <- renderPrint({
    if (data_changed()) {
      return(invisible())
    }
    req(input$go_checkCRA)
    cat(strwrap(paste(cat(checkcra(), "\n", fill = TRUE), collapse = "\n")))
  })

  #output$post_output_text_checkCRA <- renderUI({
  #  if (data_changed()) {
  #    return(NULL)
  #  }
  #  req(input$go_checkCRA)
  #  req(uploaded_data$data_source)

    #if (uploaded_data$data_source == "bmi") {
    #  tagList(
    #    hr(),
    #    div(
          # bmi specific post text output  for checkCRA
    #      style = "margin-top: 15px; font-size: 14px; color: #333;",
    #      p("The results indicate that CRA is valid in principle, conditional on ",
    #        "covariates mother’s age and education level.")
    #    )
    #  )
    #} else {
    #  tagList(
    #    hr(),
    #    div(
          # generic post text output for checkCRA
     #     style = "margin-top: 15px; font-size: 14px; color: #333;",
     #     p("") # blank for now as output explains itself well
     #   )
     # )
    #}
  #})

}

# checkMI()  function app tab 6 --------------------------------------------

#  USER INTERFACE - checkMI() function app
checkMI_ui <- fluidPage(tagList(

  # App title
  titlePanel("Inspect multiple imputation model"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Use the <strong>midoc</strong> function <strong>checkMI</strong> to explore",
           "whether multiple imputation is valid. ")),
    hr(),
    p(HTML('<strong>Example R code:</strong>',
           '<br>qoldag <- "group -> qol12 group -> qol3 age0 -> qol0 qol0 -> qol3',
           'qol0 -> qol12 qol3 -> qol12 qol0 -> r qol3 -> r"',
           '<br>midoc::checkMI(dep="qol12", preds="group age0 qol0 qol3", r_cra="r", mdag=qoldag)'))

  #  p(HTML("Enter covariates, seperated by a space. Ensure variables match column headings.",
  #         "Example input: <code>covariate_1 covariate_2 covariate_3</code>.")),
  #  p("The DAG is carried over from the draw DAG app.")
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,

      textInput(inputId = "dep_checkMI",
                label = "Partially observed variable(s), separated by a space",
                value = ""),

      textInput(inputId = "preds_checkMI",
                label = "Imputation model predictor(s), separated by a space",
                value = ""),

      textInput(inputId = "r_cra_checkMI",
                label = "Complete record indicator",
                value = ""),

      textAreaInput(inputId = "mdag_checkMI",
                    label = "mDAG",
                    value = ""),

      actionButton(inputId = "go_checkMI",
                   label = "Check MI is valid")
    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      # Output: Print result
      verbatimTextOutput(outputId = "checkMI"),
      #uiOutput("post_output_text_checkMI")

    )
  )
))

# SERVER - checkMI() function app
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

   # if (uploaded_data$data_source == "bmi") {
   #  updateTextInput(session, "dep_checkMI", value = "bmi7")
   #   updateTextInput(session, "preds_checkMI", value = "matage mated pregsize")
   #    updateTextInput(session, "r_cra_checkMI", value = "r")
   #   updateTextAreaInput(session, "mdag_checkMI", value = uploaded_data$dag_text)
   #  } else {
      updateTextInput(session, "dep_checkMI", value = "qol12")
      updateTextInput(session, "preds_checkMI", value = "group age0 qol0 qol3")
      updateTextInput(session, "r_cra_checkMI", value = "r")
      updateTextAreaInput(session, "mdag_checkMI", value = uploaded_data$dag_text)
#    }
  })

  # check formula input. If it has changed, reset output and wait for button click.
  observeEvent(
    list(input$dep_checkMI, input$preds_checkMI, input$r_cra_checkMI),
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
      need(!is.null(uploaded_data$df), "Please upload a dataset")
    )
    tryCatch({
      testthat::evaluate_promise(
        midoc::checkMI(
          input$dep_checkMI,
          input$preds_checkMI,
          input$r_cra_checkMI,
          input$mdag_checkMI
        )
      )$messages
    }, error = function(e) {
      e$message
    })
  })

  # checkMI function output
  output$checkMI <- renderPrint({
    if (data_changed()) {
      return(invisible())  # clear output if data has changed but button not clicked yet
    }
    req(input$go_checkMI)  # Wait for button click to show output
    cat(strwrap(paste(cat(checkmi(), "\n", fill = TRUE), collapse = "\n")))
  })

  #output$post_output_text_checkMI <- renderUI({
  #  if (data_changed()) {
  #    return(NULL)  # clear UI output if data has changed but the button hasn't been clicked yet
  #  }
  #  req(input$go_checkMI)
  #  req(uploaded_data$data_source)

    # the following makes the post text output when bmi is selected dependent on input
  #  if (uploaded_data$data_source == "bmi") {
  #    preds_vec <- strsplit(input$preds_checkMI, "\\s+")[[1]]

  #    if (identical(preds_vec, c("matage", "mated", "pregsize"))) {
  #      tagList(
  #        hr(),
  #        div(
  #          # bmi and 'matage mated pregsize' input specific post text output  for checkMI
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p("The results indicate that MI is valid in principle (the missing data is random),",
  #            "conditional on predictors mother’s age, mother’s education level, and pregnancy size."),
  #          p(HTML("Try changing the model predictors to <code>matage mated bwt</code>"))
  #        )
  #      )
  #    } else if (identical(preds_vec, c("matage", "mated", "bwt"))) {
  #      tagList(
  #        hr(),
  #        div(
  #          # bmi and 'matage mated bwt' input specific post text output  for checkMI
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p("The results indicate that MI is not valid in principle (birth weight is",
  #            "not independent of bmi7. The missing data is no random), conditional on predictors",
  #            "mother’s age, mother’s education level, and birth weight."),
  #          p(HTML("This is because birth weight is a “collider”. It shares a common cause with",
  #                 "both BMI at age 7y and its missingness indicator, <code>r</code>. Hence conditioning on birth",
  #                 "weight opens a path between BMI at age 7 and its missingness."))
  #        )
  #      )
  #    } else {
  #      tagList(
  #        hr(),
  #        div(
  #          # bmi, misc input specific post text output  for checkMI
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p("")  # Blank text output for BMI other predictors
  #        )
  #      )
  #    }
  #  } else {
  #    tagList(
  #      hr(),
  #      div(
  #        # generic post text output for checkMI
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("")  # Blank text output for other data sources
  #      )
  #    )
  #  }
  #})

}

# checkModSpec() function app tab 7 ---------------------------------------------

#  USER INTERFACE - checkModSpec() function app
checkModSpec_ui <- fluidPage(tagList(

  # App title
  titlePanel("Inspect parametric model specification"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("If MI is valid in principle, the <strong>midoc</strong> function",
           "<strong>checkModSpec</strong> can be used to",
           "specify a parametric model.",
           "Optionally, you can also use the supplied dataset to explore",
           "whether the observed relationships",
           "are consistent with the proposed model.")),
    hr(),
    p(HTML('<strong>Example R code:</strong> <br>midoc::checkModSpec(formula="qol12 ~ factor(group) + age0 + qol0 + qol3", family="gaussian(identity)", data=qol)'))

  #  p(HTML("The model formula input should follow the style: ",
   #        "<code>partially missing variable ~ formula containing covariates</code>.")),
  #  p(HTML("Example input if partially missing variable has a linear relationship with covariate_1: ",
   #        "<code>partially missing variable ~ covariate_1 + covariate_2 + covariate_3</code>"))
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,

      textAreaInput(inputId = "formula_checkModSpec",
                    label = "Imputation Model Formula",
                    value = ""),

      selectInput(inputId = "family_checkModSpec",
                  label = "Imputation Model Family",
                  choices = c("gaussian(identity)","binomial(logit)"),
                  selected = ""),

      selectInput(inputId = "data_checkModSpec",
                  label = "Use data?",
                  choices = c("TRUE","FALSE"),
                  selected = "TRUE"),

      shiny::actionButton(inputId = "go_checkModSpec",
                          label = "Inspect model")

    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      verbatimTextOutput(outputId = "checkmodspec"), # text output
      plotOutput(outputId = "checkmodspec_plot", width="80%"),        # plot output
      #uiOutput("post_output_text_checkModSpec") # post output text

    )
  )
))

# SERVER - checkModSpec function app
checkModSpec_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)

  # Autofill formula and family when data source changes
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # Mark data changed so outputs reset

    #if (uploaded_data$data_source == "bmi") {
    #  updateTextAreaInput(session, "formula_checkModSpec",
    #                      value = "bmi7 ~ matage + mated + pregsize")  # autofill formula
    #  updateSelectInput(session, "family_checkModSpec",
    #                    selected = "gaussian(identity)")
    #} else {
      updateTextAreaInput(session, "formula_checkModSpec", value = "qol12 ~ factor(group) + age0 + qol0 + qol3")
      updateSelectInput(session, "family_checkModSpec", selected = "gaussian(identity)")
    #}
  })

  # check formula or family inputs — if changed, reset output and wait for button click
  observeEvent(list(input$formula_checkModSpec, input$family_checkModSpec, input$data_checkModSpec), {
    data_changed(TRUE)  # Mark outputs should reset because inputs changed
  })

  # ReactiveVal to store checkmodspec output - ONLY updated on button click
  #checkmodspec_result <- reactiveVal(NULL)

  # ReactiveVal to store formula and family at button click
  #stored_formula <- reactiveVal(NULL)
  #stored_family <- reactiveVal(NULL)

  # When button clicked, update result by isolating inputs to prevent early firing
  observeEvent(input$go_checkModSpec, {
    data_changed(FALSE)  # Clear flag to allow output rendering
  })

  # checkModSpec function
  checkmodspec<- reactive({#isolate({
      validate(
        need(!is.null(uploaded_data$df), "Please upload a dataset")
      )
      #stored_formula(input$formula_checkModSpec)
      #stored_family(input$family_checkModSpec)

      #res <- NULL
      if (input$data_checkModSpec=="TRUE"){
        tryCatch({
          #eval_result <-
          testthat::evaluate_promise(
            midoc::checkModSpec(
              input$formula_checkModSpec,
              input$family_checkModSpec,
              uploaded_data$df,
              plot = FALSE
            )
          )$messages
          #res <- eval_result$messages
        }, error = function(e) {
          e$message
        })
      } else {
        tryCatch({
          #eval_result <-
          testthat::evaluate_promise(
            midoc::checkModSpec(
              input$formula_checkModSpec,
              input$family_checkModSpec,
              plot = FALSE
            )
          )$messages
          #res <- eval_result$messages
        }, error = function(e) {
          e$message
        })
      }

      #checkmodspec_result(res)  # Store result reactively
    })
  #})

  # Render model check output after clicking button, else show nothing
  output$checkmodspec <- renderPrint({
    #req(!data_changed())       # Only run if data_changed is FALSE (button clicked)

    #res <- checkmodspec_result()
    #if (is.null(res)) return(invisible())
    if (data_changed()) {
      return(invisible())
    }

    req(input$go_checkModSpec) # And button must be clicked

    cat(strwrap(paste(cat(checkmodspec(), "\n", fill = TRUE), collapse = "\n")))
  })

  # checkModSpec() function for plot output
  output$checkmodspec_plot <- renderPlot({
    req(input$go_checkModSpec)
    if (data_changed()) {
      return(invisible())
    }
    req(data())

    #Only print plot if no error and data=TRUE
    if (input$data_checkModSpec=="TRUE"){
      tryCatch(midoc::checkModSpec(
        input$formula_checkModSpec,
        input$family_checkModSpec,
        uploaded_data$df,
        message = FALSE,
        plot = TRUE
      ),
      error = function(e) {""}
      )
      }
    })
  # checkModSpec function output
  #output$post_output_text_checkModSpec <- renderUI({
  #  req(!data_changed())
  #  req(input$go_checkModSpec)
  #  req(uploaded_data$data_source)

  #  if (uploaded_data$data_source == "bmi") {
  #    formula_trimmed <- gsub("\\s+", " ", trimws(stored_formula()))

  #    if (formula_trimmed == "bmi7 ~ matage + mated + pregsize") {
  #      tagList(
  #        hr(),
  #        div(
  #          # bmi 'bmi7 ~ matage + mated + pregsize' input specific post text output for checkModSpec
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p("There is strong evidence that the imputation model is mis-specified."),
  #          p(HTML("<code>bmi7 ~ matage + mated + pregsize</code> assumes that child’s BMI",
  #                 "at age 7y has a linear relationship with mother’s age. Try changing the formula",
  #                 "to <code>bmi7 ~ matage + I(matage^2) + mated + pregsize</code> which assumes a",
  #                 "quadratic relationship."))
  #        )
  #      )
  #   } else if (formula_trimmed == "bmi7 ~ matage + I(matage^2) + mated + pregsize") {
  #      tagList(
  #        hr(),
  #        div(
  #          # bmi 'bmi7 ~ matage + I(matage^2) + mated + pregsize' input specific post text output for checkModSpec
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p("There is little evidence that the imputation model is mis-specified (MI is valid).",
  #            "Note that we must account for the non-linear relationship between BMI at age 7y and",
  #            "mother’sage in all other imputation models."),
  #          p("For example, the imputation model for pregnancy size would need to include BMI",
  #            "at age 7y, mother’s education level, and a quadratic form of mother’s age.",
  #            "This non-linear relationship is induced by conditioning on BMI at age 7y.")
  #        )
  #      )
  #    } else {
  #      tagList(
  #        hr(),
  #        div(
  #          # bmi misc input specific post test output for checkModSpec
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p("")  # blank text for any other formula when data_source is bmi
  #        )
  #      )
  #    }
  #  } else {
  #    tagList(
  #      hr(),
  #      div(
  #        # misc post text output for checkModSpec
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("")  # blank text for non-bmi data sources
  #      )
  #    )
  #  }
  #})

}

#  proposeMI() function tab 8 ---------------------------------------------

# USER INTERFACE - proposeMI() function app
proposeMI_ui <- fluidPage(tagList(

  # App title
  titlePanel("Suggest multiple imputation options"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("Use the <strong>midoc</strong> function <strong>proposeMI</strong> to suggest",
           "multiple imputation options when using the <strong>mice</strong> package, based on ",
           "the proposed imputation model. The proportion of complete records",
           "can be specified a priori or calculated using the supplied dataset.")),
    hr(),
    p(HTML('<strong>Example R code:</strong>',
    '<br>mimod <- midoc::checkModSpec(formula="qol12 ~ factor(group) + age0 + qol0 + qol3", family="gaussian(identity)", data=qol)',
    '<br>midoc::proposeMI(mimodobj=mimod, data=qol)'))

  #  p(HTML("The imputation model formula input should follow the style: ",
  #         "<code>partially missing variable ~ formula containing covariates</code>.")),
  #  p(HTML("Example input if partially missing variable has a linear relationship with covariate_1: ",
  #         "<code>partially missing variable ~ covariate_1 + covariate_2 + covariate_3</code>")),
  #  p("Every word and symbol is separated by a single space.")
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,

      textAreaInput(inputId = "formula_proposeMI",
                    label = "Imputation Model Formula",
                    value = ""),

      selectInput(inputId = "family_proposeMI",
                  label = "Imputation Model Family",
                  choices = c("gaussian(identity)","binomial(logit)"),
                  selected = ""),

      numericInput(inputId = "propcomplete_proposeMI",
                  label = "Proportion of complete records",
                  value = NA),

      selectInput(inputId = "data_proposeMI",
                  label = "Use data?",
                  choices = c("TRUE","FALSE"),
                  selected = "TRUE"),

      actionButton(inputId = "go_proposeMI",
                   label = "Propose MI options")

    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      # Output: Print result
      verbatimTextOutput(outputId = "proposemi"),
      plotOutput(outputId = "proposemi_plot", width="80%"),        # plot output
      #uiOutput("post_output_text_proposeMI")

    )
  )
))

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

    #if (uploaded_data$data_source == "bmi") {
    #  updateTextAreaInput(session, "formula_proposeMI",
    #                      value = "bmi7 ~ matage + mated + pregsize")
    #  updateSelectInput(session, "family_proposeMI",
    #                    selected = "gaussian(identity)")
    #} else {
      updateTextAreaInput(session, "formula_proposeMI", value = "qol12 ~ factor(group) + age0 + qol0 + qol3")
      updateSelectInput(session, "family_proposeMI", selected = "gaussian(identity)")
    #}
  })

  # check formula or family inputs — if changed, reset output and wait for button click
  observeEvent(list(input$formula_proposeMI, input$family_proposeMI, input$data_proposeMI, input$propcomplete_proposeMI), {
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
      need(!is.null(uploaded_data$df), "Please upload a dataset")
    )
    uploaded_data$df
  })

  # CheckModSpec reactive (using stored inputs)
  mimod <- reactive({
    req(uploaded_data$df)
    req(stored_formula())
    req(stored_family())

    if (input$data_proposeMI=="TRUE"){
      tryCatch({
      midoc::checkModSpec(stored_formula(), stored_family(), uploaded_data$df,
                          plot = FALSE, message = FALSE)
      }, error = function(e) {
        e$message
      })
    } else {
      tryCatch({
        midoc::checkModSpec(stored_formula(), stored_family(),
                            plot = FALSE, message = FALSE)
      }, error = function(e) {
        e$message
      })
    }
  })

  # ProposeMI reactive
  proposemi <- reactive({
  #  req(uploaded_data$df)
  #  req(mimod())

   if (input$data_proposeMI=="TRUE"){
      tryCatch({
        testthat::evaluate_promise(
          midoc::proposeMI(mimodobj=mimod(), data=data(), plot = FALSE)
        )$messages
      }, error = function(e) {
        e$message
      })
    } else {
      tryCatch({
        testthat::evaluate_promise(
          midoc::proposeMI(mimodobj=mimod(), prop_complete=input$propcomplete_proposeMI, plot = FALSE)
        )$messages
      }, error = function(e) {
        e$message
      })
    }
  })

  # Output the proposeMI output, only after go button clicked & if data not changed
  output$proposemi <- renderPrint({
    if (data_changed()) {
      return(invisible())  # Clear output if data changed but button not clicked
    }
    req(input$go_proposeMI)
    #msgs <- proposemi()
    #if (is.null(msgs) || length(msgs) == 0) {
    #  cat("No messages to display.")
    #} else {
      cat(strwrap(paste(cat(proposemi(), "\n", fill = TRUE), collapse = "\n")))
    #}
  })

  # proposeMI() function for plot output
  output$proposemi_plot <- renderPlot({
    req(input$go_proposeMI)
    if (data_changed()) {
      return(invisible())
    }
    req(data())

    #Only print plot if no error and if data==TRUE
    if (input$data_proposeMI=="TRUE"){
      tryCatch(midoc::proposeMI(mimodobj=mimod(),
                                data=uploaded_data$df,
                                message = FALSE,
                                plot = TRUE,
                                plotprompt = FALSE
      ),
      error = function(e) {""}
      )
    }
  })

  # Conditional post-output text, only after go button clicked & if data not changed
  #output$post_output_text_proposeMI <- renderUI({
  #  if (data_changed()) {
  #    return(NULL)  # Clear UI if data changed but button not clicked
  #  }
  #  req(input$go_proposeMI)
  #  req(uploaded_data$data_source)

  #  if (uploaded_data$data_source != "bmi") {
  #    tagList(
  #      hr(),
  #      div(
          # generic post output text for proposeMI
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("Try fitting both linear and quadratic relationships between the partially",
  #          "missing variable and covariates to see how the results differ.")
  #      )
  #    )
  #  } else {
  #    formula_trimmed <- gsub("\\s+", " ", trimws(stored_formula()))

  #    if (formula_trimmed == "bmi7 ~ matage + mated + pregsize") {
  #      tagList(
  #        hr(),
  #        div(
            # bmi 'bmi7 ~ matage + mated + pregsize' input specific post text output for proposeMI
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p(HTML("<code>bmi7 ~ matage + mated + pregsize</code> assumes that child’s BMI",
  #                "at age 7y has a linear relationship with mother’s age. Try changing the formula",
  #                 "to <code>bmi7 ~ matage + I(matage^2) + mated + pregsize</code> which assumes a",
  #                 "quadratic relationship."))
  #        )
  #      )
  #    } else if (formula_trimmed == "bmi7 ~ matage + I(matage^2) + mated + pregsize") {
  #      tagList(
  #        hr(),
  #        div(
  #          # bmi 'bmi7 ~ matage + I(matage^2) + mated + pregsize' input specific post output text for proposeMI
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p( "")
  #        )
  #      )
  #    } else {
  #      tagList(
  #        hr(),
  #        div(
            # bmi misc input post output text for proposeMI
  #          style = "margin-top: 15px; font-size: 14px; color: #333;",
  #          p("Try fitting both linear and quadratic relationships between the partially",
  #            "missing variable and covariates to see how the results differ.")
  #        )
  #      )
  #    }
  #  }
  #})
}

# doMImice() function app tab 9 -------------------------------------------------

# USER INTERFACE - doMImice() function app
doMImice_ui <- fluidPage(tagList(

  # App title
  titlePanel("Perform multiple imputation using the mice package"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("The <strong>midoc</strong> function",
           "<strong>doMImice</strong> can be used to",
           "perform multiple imputation using the <strong>mice</strong> package.")),
    hr(),
    p(HTML('<strong>Example R code:</strong>',
           '<br>mimod <- midoc::checkModSpec(formula="qol12 ~ factor(group) + age0 + qol0 + qol3", family="gaussian(identity)", data=qol)',
           '<br>miprop <- midoc::proposeMI(mimodobj=mimod, data=qol)',
           '<br>midoc::doMImice(mipropobj=miprop, seed=123, substmod = "lm(qol12 ~ factor(group) + age0 + qol0)")'))

  #  p(HTML("The imputation model formula input should follow the style: ",
  #         "<code>partially missing variable ~ formula containing covariates</code>.")),
  #  p(HTML("Example input if partially missing variable has a linear relationship with covariate_1: ",
  #         "<code>partially missing variable ~ covariate_1 + covariate_2 + covariate_3</code>")),
  #  p("Every word and symbol is separated by a single space. The substantive model will autofill from",
  #    "the imputation model formula.")
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,

      textAreaInput(inputId = "impformula",
                    label = "Imputation Model Formula",
                    value = ""),

      selectInput(inputId = "impfamily",
                  label = "Imputation Model Family",
                  choices = c("gaussian(identity)","binomial(logit)"),
                  selected = ""),

      #textInput(inputId = "group_domimice",
      #          label = "Treatment group variable",
      #          value = " "),


      textAreaInput(inputId = "substmod",
                    label = "Substantive (Analysis) Model Expression",
                    value = ""),

      numericInput(inputId = "seed",
                   label = "Set the seed of the 'mice' call",
                   value = 123),

      actionButton(inputId = "go_doMImice",
                   label = "Perform MI")

    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      verbatimTextOutput(outputId = "domimice"), # domomice output
      #uiOutput("post_output_text_doMImice") # post output text
    )
  )
))

# SERVER - doMImice function app
doMImice_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)  # Track data changes to reset outputs

  # autofill formula input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$formula_checkModSpec)) {
      updateSelectInput(session, "impformula", selected = uploaded_data$formula_checkModSpec)
    }
  })

  # autofill family input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$family_checkModSpec)) {
      updateSelectInput(session, "impfamily", selected = uploaded_data$family_checkModSpec)
    }
  })

  # if data source is bmi, autofill input, otherwise leave them blank
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # mark data changed to clear output/UI

    #if (uploaded_data$data_source == "bmi") {
    #  updateTextAreaInput(session, "impformula",
    #                      value = "bmi7 ~ matage + I(matage^2) + mated + pregsize")
    #  updateSelectInput(session, "impfamily",
    #                    selected = "gaussian(identity)")
    #  updateTextAreaInput(session, "substmod",
    #                    value = "lm(bmi7 ~ matage + I(matage^2) + mated + pregsize)")
    #} else {
      updateTextAreaInput(session, "impformula", value = "qol12 ~ factor(group) + age0 + qol0 + qol3")
      updateSelectInput(session, "impfamily", selected = "gaussian(identity)")
      #updateTextAreaInput(session, "group_domimice", value = "group")
      updateTextAreaInput(session, "substmod", value = "lm(qol12 ~ factor(group) + age0 + qol0)")
      updateNumericInput(session, "seed", value = NULL)
    #}
  })

  # Reset output if these inputs change
  observeEvent(list(input$impformula, input$impfamily, #input$group_domimice,
                    input$substmod, input$seed), {
    data_changed(TRUE)
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
  stored_impformula <- reactiveVal(NULL)
  stored_impfamily <- reactiveVal(NULL)

  # Clear data_changed flag when go button clicked
  observeEvent(input$go_doMImice, {
    data_changed(FALSE)

    # Store current inputs at button click
    stored_impformula(input$impformula)
    stored_impfamily(input$impfamily)
  })

  # prompt if no data
  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a dataset")
    )
    uploaded_data$df
  })

  # CheckModSpec reactive (using stored inputs)
  mimod_domi <- reactive({
    req(uploaded_data$df)
    req(stored_impformula())
    req(stored_impfamily())
    tryCatch({
    midoc::checkModSpec(stored_impformula(), stored_impfamily(), data(),
                        plot = FALSE, message = FALSE)
    }, error = function(e) {
      e$message
    })
  })

  # proposeMI() function
  miprop_domi <- reactive({
    req(uploaded_data$df)
    req(mimod_domi())
    tryCatch({
    midoc::proposeMI(mimodobj=mimod_domi(), data=data(), plot = FALSE, message = FALSE)
    }, error = function(e) {
      e$message
      })
  })


  # doMImice() function
  domimice <- reactive({
    req(miprop_domi())
    tryCatch({
        testthat::evaluate_promise(
          midoc::doMImice(miprop_domi(), input$seed, input$substmod)
        )$messages
    }, error = function(e) {
      e$message
    })
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
  #output$post_output_text_doMImice <- renderUI({
  #  if (data_changed()) {
  #    return(NULL)  # clear UI if data changed but button not clicked
  #  }
  #  req(input$go_doMImice)
  #  req(uploaded_data$data_source)

  #  if (uploaded_data$data_source == "bmi") {
  #    tagList(
  #      hr(),
  #      div(
          # bmi specific post text output for doMImice
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("CRA and MI estimates (fitting a quadratic relationship between BMI at age 7",
  #          "years and maternal age in the imputation model) are similar.")
  #      )
  #    )
  #  } else {
  #    tagList(
  #      hr(),
  #      div(
          # generic post text output for doMImice
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("Try fitting both linear and quadratic relationships between the partially",
  #          "missing variable and covariates to see how the results differ.")
  #      )
  #    )
  #  }
  #})
}

# doMNARMImice() function app tab 8 -------------------------------------------------

# USER INTERFACE - doMNARMImice() function app
doMNARMImice_ui <- fluidPage(tagList(

  # App title
  titlePanel("Perform not-at-random multiple imputation using the mice package"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("The <strong>midoc</strong> function",
           "<strong>doMNARMImice</strong> can be used to",
           'perform <a href="https://doi.org/10.1002/sim.7643" target="_blank">missing not-at-random (MNAR) multiple imputation</a> using the <strong>mice</strong> package.',
           'MNAR multiple imputation (also referred to as "delta-based" or "pattern-mixture" modelling)',
           'involves "shifting" the imputed values by a specified amount (delta).',
           'Delta can vary across individuals and/or subgroups and reflects systematic departures from the',
           'values imputed under the missing at random (MAR) assumption.',
           '<br><br>In this example, we specify delta to be -10 for individuals in the placebo group and',
           '-5 for those in the treatment group.')),
    hr(),
    p(HTML('<strong>Example R code:</strong>',
           '<br>mimod <- midoc::checkModSpec(formula="qol12 ~ factor(group) + age0 + qol0 + qol3", family="gaussian(identity)", data=qol)',
           '<br>miprop <- midoc::proposeMI(mimodobj=mimod, data=qol)',
           '<br>midoc::doMNARMImice(mipropobj=miprop, mnardep="qol12", mnardelta="-10 + 5*factor(group)2", seed=123, substmod = "lm(qol12 ~ factor(group) + age0 + qol0)")'))

    #  p(HTML("The imputation model formula input should follow the style: ",
    #         "<code>partially missing variable ~ formula containing covariates</code>.")),
    #  p(HTML("Example input if partially missing variable has a linear relationship with covariate_1: ",
    #         "<code>partially missing variable ~ covariate_1 + covariate_2 + covariate_3</code>")),
    #  p("Every word and symbol is separated by a single space. The substantive model will autofill from",
    #    "the imputation model formula.")
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,

      textAreaInput(inputId = "mnarimpformula",
                    label = "Imputation Model Formula",
                    value = ""),

      selectInput(inputId = "mnarimpfamily",
                  label = "Imputation Model Family",
                  choices = c("gaussian(identity)","binomial(logit)"),
                  selected = ""),

      textAreaInput(inputId = "mnardelta",
                    label = "Missing not at random mechanism (delta)",
                    value = ""),

      textAreaInput(inputId = "mnardep",
                    label = "Missing not at random variable",
                    value = ""),

      textAreaInput(inputId = "mnarsubstmod",
                    label = "Substantive (Analysis) Model Expression",
                    value = ""),

      numericInput(inputId = "mnarseed",
                   label = "Set the seed of the 'mice' call",
                   value = 123),

      actionButton(inputId = "go_doMNARMImice",
                   label = "Perform MNAR MI")

    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      verbatimTextOutput(outputId = "domnarmimice"), # domomice output
      #uiOutput("post_output_text_doMImice") # post output text
    )
  )
))

# SERVER - doMNARMImice function app
doMNARMImice_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)  # Track data changes to reset outputs

  # autofill formula input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$formula_checkModSpec)) {
      updateSelectInput(session, "mnarimpformula", selected = uploaded_data$formula_checkModSpec)
    }
  })

  # autofill family input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$family_checkModSpec)) {
      updateSelectInput(session, "mnarimpfamily", selected = uploaded_data$family_checkModSpec)
    }
  })

  # if data source is bmi, autofill input, otherwise leave them blank
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # mark data changed to clear output/UI

    #if (uploaded_data$data_source == "bmi") {
    #  updateTextAreaInput(session, "impformula",
    #                      value = "bmi7 ~ matage + I(matage^2) + mated + pregsize")
    #  updateSelectInput(session, "impfamily",
    #                    selected = "gaussian(identity)")
    #  updateTextAreaInput(session, "substmod",
    #                    value = "lm(bmi7 ~ matage + I(matage^2) + mated + pregsize)")
    #} else {
    updateTextAreaInput(session, "mnarimpformula", value = "qol12 ~ factor(group) + age0 + qol0 + qol3")
    updateSelectInput(session, "mnarimpfamily", selected = "gaussian(identity)")
    updateTextAreaInput(session, "mnardelta", value = "-10 + 5*factor(group)2")
    updateTextAreaInput(session, "mnardep", value = "qol12")
    updateTextAreaInput(session, "mnarsubstmod", value = "lm(qol12 ~ factor(group) + age0 + qol0)")
    updateNumericInput(session, "mnarseed", value = NULL)
    #}
  })

  # Reset output if these inputs change
  observeEvent(list(input$mnarimpformula, input$mnarimpfamily, input$mnardelta, input$mnardep, input$mnarsubstmod, input$mnarseed), {
    data_changed(TRUE)
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
  stored_impformula <- reactiveVal(NULL)
  stored_impfamily <- reactiveVal(NULL)

  # Clear data_changed flag when go button clicked
  observeEvent(input$go_doMNARMImice, {
    data_changed(FALSE)

    # Store current inputs at button click
    stored_impformula(input$mnarimpformula)
    stored_impfamily(input$mnarimpfamily)
  })

  # prompt if no data
  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a dataset")
    )
    uploaded_data$df
  })

  # CheckModSpec reactive (using stored inputs)
  mimod_domnarmi <- reactive({
    req(uploaded_data$df)
    req(stored_impformula())
    req(stored_impfamily())
    tryCatch({
      midoc::checkModSpec(stored_impformula(), stored_impfamily(), data(),
                          plot = FALSE, message = FALSE)
    }, error = function(e) {
      e$message
    })
  })

  # proposeMI() function
  miprop_domnarmi <- reactive({
    req(uploaded_data$df)
    req(mimod_domnarmi())
    tryCatch({
      midoc::proposeMI(mimodobj=mimod_domnarmi(), data=data(), plot = FALSE, message = FALSE)
    }, error = function(e) {
      e$message
    })
  })

  # doMNARMImice() function
  domnarmimice <- reactive({
    req(miprop_domnarmi())
    tryCatch({
      testthat::evaluate_promise(
        midoc::doMNARMImice(miprop_domnarmi(), mnardep=input$mnardep, mnardelta=input$mnardelta, input$mnarseed, input$mnarsubstmod)
      )$messages
    }, error = function(e) {
      e$message
    })
  })

  #output
  output$domnarmimice <- renderPrint({
    if (data_changed()) {
      return(invisible())  # clear output if data changed but button not clicked
    }
    req(input$go_doMNARMImice)
    cat(strwrap(paste(cat(domnarmimice(), "\n", fill = TRUE), collapse = "\n")))
  })

  # conditional text under the output
  #output$post_output_text_doMImice <- renderUI({
  #  if (data_changed()) {
  #    return(NULL)  # clear UI if data changed but button not clicked
  #  }
  #  req(input$go_doMImice)
  #  req(uploaded_data$data_source)

  #  if (uploaded_data$data_source == "bmi") {
  #    tagList(
  #      hr(),
  #      div(
  # bmi specific post text output for doMImice
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("CRA and MI estimates (fitting a quadratic relationship between BMI at age 7",
  #          "years and maternal age in the imputation model) are similar.")
  #      )
  #    )
  #  } else {
  #    tagList(
  #      hr(),
  #      div(
  # generic post text output for doMImice
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("Try fitting both linear and quadratic relationships between the partially",
  #          "missing variable and covariates to see how the results differ.")
  #      )
  #    )
  #  }
  #})
}

# doRefBasedMI() function app tab 9 -------------------------------------------------

# USER INTERFACE - doRefBasedMI() function app
doRefBasedMI_ui <- fluidPage(tagList(

  # App title
  titlePanel("Perform reference-based multiple imputation using the RefBasedMI package"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("The <strong>midoc</strong> function",
           "<strong>doRefBasedMI</strong> can be used to",
           'perform <a href="https://doi.org/10.1002/sim.8569" target="_blank">reference-based multiple imputation</a> using the <strong>RefBasedMI</strong> package.',
           'Reference-based multiple imputation uses observed data from one group - the reference group -',
           'to impute missing values in other groups.',
           'Available reference-based methods are "jump-to-reference" (J2R), "copy reference" (CR)',
           'and "copy increments in reference" (CIR). <br><br>J2R assumes that the distribution of outcomes for individuals who',
           'drop out "jumps to" the distribution observed in the reference group,',
           'following their last observed time point.',
           '<br>CR assumes individuals who drop out behave as if they are in the specified',
           'reference arm for the full duration of the trial.',
           '<br>CIR assumes that the distribution of outcomes for individuals who drop out ',
           'follows the mean increments observed in the reference arm, following their last observed time point')),

    hr(),
    p(HTML('<strong>Example R code:</strong>',
           '<br>mimod <- midoc::checkModSpec(formula="qol12 ~ factor(group) + age0 + qol0 + qol3", family="gaussian(identity)", data=qol)',
           '<br>miprop <- midoc::proposeMI(mimodobj=mimod, data=qol)',
           '<br>midoc::doRefBasedMI(mipropobj=miprop, covs="age0 qol0", depvar="qol3 qol12", treatvar="group", idvar="id",',
           'method="J2R", reference=1, seed=123, substmod = "lm(qol12 ~ factor(group) + age0 + qol0)")'))

    #  p(HTML("The imputation model formula input should follow the style: ",
    #         "<code>partially missing variable ~ formula containing covariates</code>.")),
    #  p(HTML("Example input if partially missing variable has a linear relationship with covariate_1: ",
    #         "<code>partially missing variable ~ covariate_1 + covariate_2 + covariate_3</code>")),
    #  p("Every word and symbol is separated by a single space. The substantive model will autofill from",
    #    "the imputation model formula.")
  ),
  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,
      textAreaInput(inputId = "refmiimpformula",
                    label = "Imputation Model Formula",
                    value = ""),

      selectInput(inputId = "refmiimpfamily",
                  label = "Imputation Model Family",
                  choices = c("gaussian(identity)","binomial(logit)"),
                  selected = ""),

      textAreaInput(inputId = "refmicovs",
                    label = "Analysis model covariates, separated by a space",
                    value = ""),

      textAreaInput(inputId = "refmideps",
                    label = "Longitudinal outcome variables, separated by a space",
                    value = ""),

      textAreaInput(inputId = "refmigroup",
                    label = "Treatment group variable",
                    value = ""),

      textAreaInput(inputId = "refmiid",
                    label = "Participant identifier variable",
                    value = ""),

      selectInput(inputId = "refmimethod",
                  label = "Reference-based imputation method",
                  choices = c("J2R","CR","CIR"),
                  selected = ""),

      numericInput(inputId = "refmiref",
                   label = "Reference treatment group",
                   value = ""),

      textAreaInput(inputId = "refmisubstmod",
                    label = "Substantive (Analysis) Model Expression",
                    value = ""),

      numericInput(inputId = "refmiseed",
                   label = "Set the seed of the 'RefBasedMI' call",
                   value = 123),

      actionButton(inputId = "go_doRefBasedMI",
                   label = "Perform ref-based MI")

    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      verbatimTextOutput(outputId = "dorefmi"), # dorefmi output
      #uiOutput("post_output_text_doMImice") # post output text
    )
  )
))

# SERVER - doRefBasedMI function app
doRefBasedMI_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)  # Track data changes to reset outputs

  # autofill formula input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$formula_checkModSpec)) {
      updateSelectInput(session, "refmiimpformula", selected = uploaded_data$formula_checkModSpec)
    }
  })

  # autofill family input from the checkmodspec app input
  observe({
    if (!is.null(uploaded_data$family_checkModSpec)) {
      updateSelectInput(session, "refmiimpfamily", selected = uploaded_data$family_checkModSpec)
    }
  })

  # if data source is bmi, autofill input, otherwise leave them blank
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # mark data changed to clear output/UI

    #if (uploaded_data$data_source == "bmi") {
    #  updateTextAreaInput(session, "impformula",
    #                      value = "bmi7 ~ matage + I(matage^2) + mated + pregsize")
    #  updateSelectInput(session, "impfamily",
    #                    selected = "gaussian(identity)")
    #  updateTextAreaInput(session, "substmod",
    #                    value = "lm(bmi7 ~ matage + I(matage^2) + mated + pregsize)")
    #} else {
    updateTextAreaInput(session, "refmiimpformula", value = "qol12 ~ factor(group) + age0 + qol0 + qol3")
    updateSelectInput(session, "refmiimpfamily", selected = "gaussian(identity)")
    updateTextAreaInput(session, "refmicovs", value = "age0 qol0")
    updateTextAreaInput(session, "refmideps", value = "qol3 qol12")
    updateTextAreaInput(session, "refmigroup", value = "group")
    updateTextAreaInput(session, "refmiid", value = "id")
    updateSelectInput(session, "refmimethod", selected = "J2R")
    updateNumericInput(session, "refmiref", value = 1)
    updateTextAreaInput(session, "refmisubstmod", value = "lm(qol12 ~ factor(group) + age0 + qol0)")
    updateNumericInput(session, "refmiseed", value = NULL)
    #}
  })

  # Reset output if these inputs change
  observeEvent(list(input$refmiimpformula, input$refmiimpfamily, input$refmicovs,
                    input$refmideps, input$refmigroup, input$refmiid, input$refmimethod,
                    input$refmiref, input$refmisubstmod, input$refmiseed), {
    data_changed(TRUE)
  })

  # save formula input for auto input to later apps
  #observeEvent(input$go_checkModSpec, {
  #  uploaded_data$formula_checkModSpec <- input$formula_checkModSpec
  #  data_changed(FALSE)
  #})

  # save family input for auto input to later apps
  observeEvent(input$go_checkModSpec, {
    uploaded_data$family_checkModSpec <- input$family_checkModSpec
    data_changed(FALSE)
  })

  # ReactiveVal to store formula and family at button click
  stored_impformula <- reactiveVal(NULL)
  stored_impfamily <- reactiveVal(NULL)

  # Clear data_changed flag when go button clicked
  observeEvent(input$go_doRefBasedMI, {
    data_changed(FALSE)

    # Store current inputs at button click
    stored_impformula(input$refmiimpformula)
    stored_impfamily(input$refmiimpfamily)
  })

  # prompt if no data
  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a dataset")
    )
    uploaded_data$df
  })

  # CheckModSpec reactive (using stored inputs)
  mimod_dorefmi <- reactive({
    req(uploaded_data$df)
    req(stored_impformula())
    req(stored_impfamily())
    tryCatch({
      midoc::checkModSpec(stored_impformula(), stored_impfamily(), data(),
                          plot = FALSE, message = FALSE)
    }, error = function(e) {
      e$message
    })
  })

  # proposeMI() function
  miprop_dorefmi <- reactive({
    req(uploaded_data$df)
    req(mimod_dorefmi())
    tryCatch({
      midoc::proposeMI(mimodobj=mimod_dorefmi(), data=data(), plot = FALSE, message = FALSE)
    }, error = function(e) {
      e$message
    })
  })

  # doRefBasedMI() function
  dorefbasedmi <- reactive({
    req(miprop_dorefmi())
    tryCatch({
      testthat::evaluate_promise(
        midoc::doRefBasedMI(miprop_dorefmi(), input$refmicovs,
                            input$refmideps, input$refmigroup, input$refmiid, input$refmimethod,
                            input$refmiref, input$refmiseed, input$refmisubstmod)
      )$messages
    }, error = function(e) {
      e$message
    })
  })

  #output
  output$dorefmi <- renderPrint({
    if (data_changed()) {
      return(invisible())  # clear output if data changed but button not clicked
    }
    req(input$go_doRefBasedMI)
    cat(strwrap(paste(cat(dorefbasedmi(), "\n", fill = TRUE), collapse = "\n")))
  })

  # conditional text under the output
  #output$post_output_text_doMImice <- renderUI({
  #  if (data_changed()) {
  #    return(NULL)  # clear UI if data changed but button not clicked
  #  }
  #  req(input$go_doMImice)
  #  req(uploaded_data$data_source)

  #  if (uploaded_data$data_source == "bmi") {
  #    tagList(
  #      hr(),
  #      div(
  # bmi specific post text output for doMImice
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("CRA and MI estimates (fitting a quadratic relationship between BMI at age 7",
  #          "years and maternal age in the imputation model) are similar.")
  #      )
  #    )
  #  } else {
  #    tagList(
  #      hr(),
  #      div(
  # generic post text output for doMImice
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("Try fitting both linear and quadratic relationships between the partially",
  #          "missing variable and covariates to see how the results differ.")
  #      )
  #    )
  #  }
  #})
}



# doCRA() function app tab 10 -------------------------------------------------

# USER INTERFACE - doCRA() function app
doCRA_ui <- fluidPage(tagList(

  # App title
  titlePanel("Perform complete records analysis"),

  # add text above the UI
  div(
    style = "margin-bottom: 20px; font-size: 14px; color: #333;",
    p(HTML("<strong>Note:</strong> A <strong>midoc</strong> function is not required",
          "because complete records analysis is performed by default in most software.")),
  hr(),
  p(HTML('<strong>Example R code:</strong>',
         '<br>lm(qol12 ~ factor(group) + age0 + qol0)'))
  ),

  #hr(), # line to divide text from output

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      #width=2,

      textAreaInput(inputId = "substmod_cra",
                    label = "Substantive (Analysis) Model Expression",
                    value = ""),

      actionButton(inputId = "go_doCRA",
                   label = "Perform CRA")

    ),

    # Main panel for displaying outputs
    mainPanel(

      width=12,

      verbatimTextOutput(outputId = "docra"), # docra output
      verbatimTextOutput(outputId = "docra_ci"), # docra CI output
      #uiOutput("post_output_text_docra") # post output text
    )
  )
))

# SERVER - doCRA function app
doCRA_server <- function(input, output, session) {

  data_changed <- reactiveVal(FALSE)  # Track data changes to reset outputs

  # autofill substmod input from the doMImice app input
  observe({
    if (!is.null(uploaded_data$substmod)) {
      updateSelectInput(session, "substmod_cra", selected = uploaded_data$substmod)
    }
  })

  # if data source is bmi, autofill input, otherwise leave blank
  observeEvent(uploaded_data$data_source, {
    req(uploaded_data$data_source)
    data_changed(TRUE)  # mark data changed to clear output/UI

    #if (uploaded_data$data_source == "bmi") {
    #  updateTextAreaInput(session, "substmod_cra",
    #                      value = "lm(bmi7 ~ matage + I(matage^2) + mated + pregsize)")
    #} else {
      updateTextAreaInput(session, "substmod_cra", value = "lm(qol12 ~ factor(group) + age0 + qol0)")
    #}
  })

  # Reset output if these inputs change
  observeEvent(list(input$substmod_cra), {
    data_changed(TRUE)
  })

  # save formula input for auto input to later apps
  observeEvent(input$go_doMImice, {
    uploaded_data$substmod <- input$substmod
    data_changed(FALSE)
  })

  # ReactiveVal to store substmod at button click
  stored_substmod_cra <- reactiveVal(NULL)

  # Clear data_changed flag when go button clicked
  observeEvent(input$go_doCRA, {
    data_changed(FALSE)

    # Store current inputs at button click
    stored_substmod_cra(input$substmod_cra)
  })

  # prompt if no data
  data <- reactive({
    validate(
      need(!is.null(uploaded_data$df), "Please upload a dataset")
    )
    uploaded_data$df
  })

  # doCRA() output
  output$docra <- renderPrint({
    if (data_changed()) {
      return(invisible())  # clear output if data changed but button not clicked
    }
    req(input$go_doCRA)

    tryCatch({
      summary(eval(parse(text=input$substmod_cra, keep.source=FALSE), env=data()))
    }, error = function(e) {
      e$message
    })
  })

  output$docra_ci <- renderPrint({
    if (data_changed()) {
      return(invisible())  # clear output if data changed but button not clicked
    }
    req(input$go_doCRA)
    #Print if no error
    tryCatch({
      confint(eval(parse(text=input$substmod_cra, keep.source=FALSE), env=data()))
    },
    error = function(e) {return(invisible())}
    )
  })

  # conditional text under the output
  #output$post_output_text_doCRA <- renderUI({
  #  if (data_changed()) {
  #    return(NULL)  # clear UI if data changed but button not clicked
  #  }
  #  req(input$go_doCRA)
  #  req(uploaded_data$data_source)

  #  if (uploaded_data$data_source == "bmi") {
  #    tagList(
  #      hr(),
  #      div(
  # bmi specific post text output for doMImice
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("CRA and MI estimates (fitting a quadratic relationship between BMI at age 7",
  #          "years and maternal age in the imputation model) are similar.")
  #      )
  #    )
  #  } else {
  #    tagList(
  #      hr(),
  #      div(
  # generic post text output for doMImice
  #        style = "margin-top: 15px; font-size: 14px; color: #333;",
  #        p("Try fitting both linear and quadratic relationships between the partially",
  #          "missing variable and covariates to see how the results differ.")
  #      )
  #    )
  #  }
  #})
}

# master UI ---------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-size: 12pt;
      }
      code, code.r, pre {
        font-size: 10pt;
        background-color: white;
        color: black;
        font-family: monospace;
        #padding: 2px 4px;
        #border-radius: 4px;
      }
      pre{
        background-color: white;
        font-color: black;
        font-size: 12pt;
        font-weight: bold;
        font-family: monospace;
      }
      caption {
        color: black;
        font-weight: bold;
      }
      .main-container {
        font-size: 12pt;
        #max-width: 90% !important;
        margin: auto;
      }
      .md-tabs {
        font-size: 12pt;
      }
      .selectize-input {
        font-size: 10pt !important;
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
      table {
      style: width:100%;
      td {
          padding: 5px;
      }
      display: table;
      #table-layout:fixed;
      #border-collapse: separate;
      white-space: normal;
      line-height: normal;
      font-family: monospace;
      font-weight: normal;
      font-size: large;
      font-style: normal;
      color: black;
      text-align: start;
      #border-spacing: 2px;
      border-color: white;
      font-variant: normal;
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

  title = 'midoc RCT Shiny app',

  div(id = "main-content",

      tags$h1(
        HTML('midoc - a Shiny app for missing data analysis')
      ),

      tags$p(
        HTML('
        Elinor Curnow, Jon Heron, Rosie Cornish, Holly Sachdeva, Kate Tilling, and James Carpenter
        ')
      ),

      #tags$hr(),

      tags$h2(id = "about-midoc", "About midoc"),
      tags$p(
        HTML('The Multiple Imputation DOCtor (<a href="https://elliecurnow.github.io/midoc/" target="_blank">midoc</a>) R package',
        'is a guidance system for analysis with missing data. It incorporates expert, up-to-date',
      'methodology to help researchers choose the most appropriate analysis approach when some data',
      'are missing.',
      ' <strong>midoc</strong> follows the framework for the treatment and reporting of missing data',
          'in observational studies (<a href="https://doi.org/10.1016/j.jclinepi.2021.01.008"',
          'target="_blank">TARMOS</a>).',
      'In this Shiny app, we use <strong>midoc</strong> to show how the TARMOS approach can also be applied to
      randomised controlled trials (RCTs) with missing data.')
      ),

      tags$h2(id = "how-to-use", "How to use this app"),
      tags$p(HTML('<br><strong>1. Analyse an example RCT dataset</strong>',
                  '<br>With this app, you can analyse the simulated RCT dataset',
                  'included in the <strong>midoc</strong> package.',
                  'Simulated trial participants are adults with Parkinson’s disease,',
                  'randomised to receive either placebo (group=1) or active treatment (group=2)',
                  'for 12 months post-randomisation.',
                  'Interest is in the effect of treatment (vs. placebo) on quality of life (QoL) at 12 months,',
                  ' where QoL is measured at randomisation, 3 months, and 12 months post-randomisation.',
                  'Some participants experience intercurrent events (ICEs), either stopping trial treatment early',
                  'or receiving rescue treatment. Some QoL data are missing after ICEs. We assume we are targeting',
                  'a <a href="https://doi.org/10.1136/bmj-2023-076316" target="_blank">treatment policy estimand</a>,',
                  'so we want to include participant outcomes in our analysis, regardless of whether they experienced an ICE.',
                  '<br><br>A preview of the dataset is shown below, where "id" denotes the participant identifier,',
                  '"group" is the treatment (randomisation) group, "age0" is age at randomisation,',
                  '"qol0/3/12" are QoL at randomisation, 3 months, and 12 months post-randomisation, and',
                  '"r" indicates whether the participant record is complete, i.e. no missing data, (r=1) or not (r=0).'
                  #'randomised controlled trial (RCT), or administrative data (ADR) -  or upload your own data.' ,
                  #'All uploaded datasets must be in .csv format.',
                  #'<strong>Tip:</strong> For a full demo of the package, choose the BMI dataset.',
                  #'Then the <strong>midoc</strong> functions below will be pre-populated.'
                  )),
      tags$table(border = 1,
                           tags$tbody(
                             tags$tr(
                               tags$td(align = "center", strong("id")),
                               tags$td(align = "center", strong("group")),
                               tags$td(align = "center", strong("age0")),
                               tags$td(align = "center", strong("qol0")),
                               tags$td(align = "center", strong("qol3")),
                               tags$td(align = "center", strong("qol12")),
                               tags$td(align = "center", strong("r"))
                             ),
                             tags$tr(
                               tags$td(align = "center", "1"),
                               tags$td(align = "center", "1"),
                               tags$td(align = "center", "61"),
                               tags$td(align = "center", "69"),
                               tags$td(align = "center", "67"),
                               tags$td(align = "center", "72"),
                               tags$td(align = "center", "1")
                             ),
                             tags$tr(
                               tags$td(align = "center", "4"),
                               tags$td(align = "center", "2"),
                               tags$td(align = "center", "59"),
                               tags$td(align = "center", "76"),
                               tags$td(align = "center", "75"),
                               tags$td(align = "center", "86"),
                               tags$td(align = "center", "1")
                             ),
                             tags$tr(
                               tags$td(align = "center", "12"),
                               tags$td(align = "center", "1"),
                               tags$td(align = "center", "75"),
                               tags$td(align = "center", "55"),
                               tags$td(align = "center", "58"),
                               tags$td(align = "center", "NA"),
                               tags$td(align = "center", "0")
                             )
                           )),

      tags$p(HTML('<br>You can perform a missing data analysis of the simulated RCT dataset',
                  'using the interactive <strong>midoc</strong> functions below.',
                  'We suggest using them in the order shown, i.e. starting with <strong>specDAG</strong>.',
      )),
      tags$hr(),
      tags$p(HTML('<strong>2. Plan a missing data analysis</strong>',
                  '<br>This app can also help you decide on the best missing data',
                  'approach a priori (i.e. without using the data).',
                  'Specify your missing data assumptions (using the prompts to help you) using the interactive midoc functions',
                  '<strong>specDAG</strong>,',
                  '<strong>exploreDAG</strong>, <strong>checkCRA</strong>, <strong>checkMI</strong>, <strong>checkModSpec</strong>,',
                  'and <strong>proposeMI</strong>.',
                  'You can plan your approach for either the <strong>midoc</strong> example RCT, or your own study.',
                  )),

      # data upload app UI
      #tags$hr(),
      #tags$h2("Specify Dataset"),
      #data_ui,

      tags$hr(),

      #tags$p(HTML('Here is our assumed "missingness" directed acyclic graph (mDAG).',
                  #'You may find <a href="https://doi.org/10.1038/s41390-018-0071-3" target="_blank">this</a> ',
                  #'introduction to DAGs useful. Then add missingness indicator(s) to your DAG.',
                  #' If you have multiple variables with mising data, start by including just ',
                  #'the complete records indicator in your DAG.')),
                  #'For tips on specifying an mDAG, see the <strong>midoc</strong>',
      #'<a href="https://elliecurnow.github.io/midoc/articles/midoc.html" target="_blank">vignette</a>.',
      #'The mDAG has been drawn using “dagitty”.',
      #'Go to the <a href="https://dagitty.net" target="_blank">dagitty website</a>',
      #'to find out more.')),


    #img(src = "qol_mdag.png", width = "80%"),

      #tags$p(HTML('<strong>Note:</strong> This application is hosted on',
      #       '<a href="https://www.shinyapps.io/" target="_blank">shinyapps.io</a>, a third-party platform.',
      #       'Do not upload confidential or sensitive data. To analyse data that are not publicly ',
      #       'available, run <strong>midoc</strong> functions locally in R.')),
      #by download <code>midoc</code> ',
      #'version <code>1.0.0</code> from <a href="https://cloud.r-project.org/web/packages/midoc/index.html" ',
      #'target="_blank">CRAN</a> and run command <code>midocVignette()</code>')),

      # dag app UI
      #tags$h2("Specify mDAG"),
      #drawDAG_ui,

      #tags$hr(),

      tags$p(HTML('This application is hosted on <a href="https://www.shinyapps.io/" target="_blank">shinyapps.io</a>,',
                  'a third-party platform. As such, it should not be used to analyse confidential or sensitive data.',
                  'To analyse your own data, run <strong>midoc</strong> functions locally in R.')),

      # Tabbed section for interactive apps
      tags$h2(id = "interactive-midoc", "Interactive midoc functions"),

      tabsetPanel(type = "pills", id = "midoc_tabs",
      # Re-ordered so functions that can be used a priori are listed first
                  tabPanel("specDAG" , drawDAG_ui),
                  tabPanel("exploreDAG", exploreDAG_ui),
                  tabPanel("checkCRA", checkCRA_ui),
                  tabPanel("checkMI", checkMI_ui),
                  tabPanel("checkModSpec", checkModSpec_ui),
                  tabPanel("proposeMI", proposeMI_ui),
                  tabPanel("descMissData" , descMissData_ui),
                  tabPanel("summMissData", summMissData_ui),
                  tabPanel("doMImice", doMImice_ui),
                  tabPanel("doMNARMImice", doMNARMImice_ui),
                  tabPanel("doRefBasedMI", doRefBasedMI_ui),
                  tabPanel("doCRA", doCRA_ui)
      ),
      #br()
  ))

# master server -----------------------------------------------------------
server <- function(input, output, session) {

  # data upload app server
  #data_server(input, output, session)

  # DAG app server
  drawDAG_server(input, output, session)

  #descMissData() function app
  descMissData_server(input, output, session)

  #summMissData() function app
  summMissData_server(input, output, session)

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

  # doMNARMImice() function app
  doMNARMImice_server(input,output, session)

  # doMNARMImice() function app
  doRefBasedMI_server(input,output, session)

  # doCRA() function app
  doCRA_server(input,output, session)

}

# App function -----------------------------------------------------------
shinyApp(ui, server)

# run app -----------------------------------------------------------------
#shiny::runApp(midocApp)

