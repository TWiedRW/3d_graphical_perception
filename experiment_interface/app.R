library(shiny)
library(shinyWidgets)
library(rgl)
library(RSQLite)
library(tidyverse)
library(rayshader)
library(shinyjs)
library(markdown)



datasets <- load('data/set85data.Rdata')
kits <- load('data/kits.Rdata')

# Read in p1, p2, p3 demo plots
source("demo-plots.R")

# Code to generate completion codes and save all codes to a file
source("completion-code.R")

# Functions to generate experiment charts
source("plot-code.R")

jsCode <- "
shinyjs.disableTab = function() {
    var tabs = $('#tabs').find('li:not(.active) a');
    tabs.bind('click.tab', function(e) {
        e.preventDefault();
        return false;
    });
    tabs.addClass('disabled');
}
shinyjs.enableTab = function(param) {
    var tab = $('#tabs').find('li:not(.active):nth-child(' + param + ') a');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
}
"

set85id_colors <- tibble(set85id = c(1, 2, 3, 4, 5, 6, 9), 
                         print_color = c("Teal", "Green", "Red", "Yellow", "Blue", "Orange", "Purple"))

# Database ----------------------------------------------------------------

#Create database if does not exist
con <- dbConnect(SQLite(), '20230209-graphicsGroup.db')

dbTables <- dbListTables(con)

# #Create user space
# if(!('user' %in% dbTables)){
#   dbWriteTable(con, 'users', data.frame(
#     userAppStartTime = NA,
#     consent = NA,
#     nickname = NA,
#     age = NA, 
#     gender = NA,
#     education = NA
#   ))
# }
# 
# #Create result space
# if(!('results' %in% dbTables)){
#   dbWriteTable(con, 'results', data.frame(
#     plot = NA,
#     ratio = NA,
#     type = NA,
#     file = NA,
#     fileID = NA,
#     set85id = NA,
#     graphtype = NA,
#     userStart = NA,
#     kit = NA,
#     nickname = NA,
#     appStartTime = NA,
#     plotStartTime = NA,
#     plotEndTime = NA,
#     whichIsSmaller = NA,
#     byHowMuch = NA,
#     graphCorrecter = NA
#   ))
# }

dbDisconnect(con)


# Pages -------------------------------------------------------------------

consent_form <- {fluidPage(
  fluidRow(
    column(
      width = 8, offset = 2,
      includeMarkdown("graphics-consent.qmd"),
      radioButtons("consent", label = "I have read the informed consent document and agree to participate in this experiment", 
                   choiceNames = c("I agree. You may save my data.", "I do not agree. Please do not save my data."), choiceValues = c(TRUE, FALSE), width = "100%"),
      
      actionButton("toDemographics", "Next")
    )
  )
)}

demographicsUI <- {fluidPage(
  fluidRow(
    column(
      width = 8, offset = 2,
      h4('Demographic Information'),
      textInput("nickname", "Please enter a nickname to be used as your identifier: "),
      selectizeInput("age", "Age Range",
                     choices = c("", "Under 19", "19-25", "26-30",
                                 "31-35", "36-40", "41-45", "46-50",
                                 "51-55", "56-60", "Over 60",
                                 "Prefer not to answer")),
      selectizeInput("gender", "Gender Identity",
                     choices = c('', "Female", "Male",
                                 "Variant/Nonconforming",
                                 "Prefer not to answer")),
      selectizeInput("education",
                     "Highest Education Level",
                     choices = c("", "High School or Less",
                                 "Some Undergraduate Courses",
                                 "Undergraduate Degree",
                                 "Some Graduate Courses",
                                 "Graduate Degree",
                                 "Prefer not to answer")),
      # Submit button only shows when demographic data is provided
      uiOutput("demographicsSubmit") 
    )
  )
)}

practiceUI <- {fluidPage(
  conditionalPanel(
    condition = '!input.beginPractice',
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("Welcome"),
        p(
          "In this survey, a series of graphs will be given to you. Each graph will have 10 bars; two of the bars will be identified with a circle and a triangle.",
          "First, we will ask you to identify which bar (circle or triangle) is smaller.",
          "Then, we will ask you to estimate what size the smaller bar is in comparison to the larger bar. That is, if the large bar is 1, what fraction of 1 is the smaller bar?"
        ),
        
        p(
          "In some cases, the graphs will be displayed on screen.",
          "In other cases, we will ask you to get one of the graphs from a ziploc bag you should have received before starting this study.",
          "For these 3D-printed graphs, we will ask you to tell us which graph you selected using the ID code engraved on the bottom of the chart."
        ),
        
        p(
          'The next screen will display a few sample graphs.',
          'The answers for each of these graphs are located underneath the sample.'
        )
      )
    ),
    fluidRow(
      column(8, offset = 2, align = 'center',
             actionButton('beginPractice', 'Next')
      )
    )
  ),
  conditionalPanel(
    condition = 'input.beginPractice',
    fluidRow(
      column(12,
             align = 'center',
             h3('Sample Graphs with Solutions')
      )
    ),
    fluidRow(
      useShinyjs(),
      column(width = 4,  align = "center",
             plotOutput('prac2'),
             selectizeInput('dummy5', 'Which bar is smaller?',
                            choices = c('', 'Circle (●)', 'Triangle (▲)'),
                            selected = 'Triangle (▲)'),
             sliderInput('dummy2', label = 'What size is the smaller bar in comparison to the larger bar?',
                         min = 0, max = 100, value = 66,
                         step = 0.1, ticks = F),
             sliderInput('dummy8', label = 'Correct answer:',
                         min = 0, max = 100, value = 75,
                         step = 0.1, ticks = F)
      ),
      column(width = 4,  align = "center",
             plotOutput('prac1'),
             selectizeInput('dummy4', 'Which bar is smaller?',
                            choices = c('', 'Circle (●)', 'Triangle (▲)'),
                            selected = 'Triangle (▲)'),
             sliderInput('dummy1', label = 'What size is the smaller bar in comparison to the larger bar?',
                         min = 0, max = 100, value = 33,
                         step = 0.1, ticks = F),
             sliderInput('dummy7', label = 'Correct answer:',
                         min = 0, max = 100, value = 50,
                         step = 0.1, ticks = F)
      ),
      column(width = 4, align = "center",
             plotOutput('prac3'),
             selectizeInput('dummy6', 'Which bar is smaller?',
                            choices = c('', 'Circle (●)', 'Triangle (▲)'),
                            selected = 'Triangle (▲)'),
             sliderInput('dummy3', label = 'What size is the smaller bar in comparison to the larger bar?',
                         min = 0, max = 100, value = 33,
                         step = 0.1, ticks = F),
             sliderInput('dummy9', label = 'Correct answer:',
                         min = 0, max = 100, value = 25,
                         step = 0.1, ticks = F)
      )

    ),
    fluidRow(align = 'center',
             helpText("These plots are for practice. Your responses will not be saved."),
             actionButton(inputId = 'toInstructions', label = 'Continue'),
             br()
    )
  )
)}

instructions <- {fluidPage(
  fluidRow(
    column(8, offset = 2,
           h2('Instructions'),
           align = 'center'
    )
  ),
  fluidRow(
    column(8, offset = 2, align = 'left',
           p('Thank you for participating in our experiment on perceptual judgments in different graphical mediums.',
             'You will see a series of 15 charts in this experiment',
             'On each screen, you will see either a rendered chart (in 2D or 3D), or a prompt to choose a 3D printed chart from your kit'),
           br(),
           p("Before you start, please enter your kit number so that we know what charts you have"),
           selectizeInput('kitID', 'Kit ID: ', choices = c(1:21, "Other"), width = '15%',   
                          # https://stackoverflow.com/questions/24175997/force-no-default-selection-in-selectinput
                          options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }')
                          )),
           br(),
           
           h4("Chart Selection"),
           p("If you are instructed to use a 3D chart, you will pick one of the charts from your kit and select the ID code on the bottom of the chart. ",
             "Otherwise, use the graph which shows up on the screen."),
           h4("Interactivity"),
           p("You will be able to use your mouse to interact with 3D plots shown on your screen."),
           h4("Task Steps"),
           p("First, select which marked bar, circle or triangle, is smaller.", 
             "Then, visually estimate how big the smaller bar is in comparison to the larger bar."),
           br(),
           p("When you are ready, click 'Begin'"),
           uiOutput('toExpBtn')
    )
  )
)}

experimentUI <- {fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput('printed_graph_choice'),
      br(),
      selectizeInput('smaller', 'Which bar is smaller?',
                     choices = c('', 'Circle (●)', 'Triangle (▲)'),
                     selected = NA),
      sliderInput('ratio', label = 'Approximately what size is the smaller bar in comparison to the larger bar?',
                  min = 0, max = 100, value = 50,
                  step = 0.1, ticks = F),
      br(),
      uiOutput("expNextBtn"),
      width = 4
    ),
    mainPanel(
      uiOutput('expPlot'),
      width = 8
    )
  )
)}

exitUI <- {fluidPage(
  useShinyjs(),
  fluidRow(
    column(8, offset = 2, align = 'center',
           h3('Thank You'),
           p('Your response has been submitted. 
             Please return the graphs to the kit bag and reload the page to reset the application for the next user.'),
           h5("Completion code"),
           textOutput("completion_code"),
           helpText("If you are a Stat 218 student, save this code and submit it to Canvas to complete your Stat 218 assignment."),
           br(),
           actionButton('reset', 'New Submission')
    )
  )
)}




# Page Navigation ---------------------------------------------------------

ui <- navbarPage(
  'Perceptual Judgments Experiment',
  # Common header for all panels
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  id = 'nav',
  tabPanel('Consent', consent_form),
  tabPanel('Demographics', uiOutput("demographics")),
  tabPanel('Practice', uiOutput("practice")),
  tabPanel('Instructions', instructions),
  tabPanel('Experiment', experimentUI),
  tabPanel('Finishing Up', exitUI)
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
  # disable tabs on page load
  shinyjs::disable(selector = '.navbar-nav a[data-value="Demographics"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Practice"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Instructions"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Experiment"]')
  shinyjs::disable(selector = '.navbar-nav a[data-value="Finishing Up"]')
  
  
  # Keep track of experiment time milestones
  timing <- reactiveValues()
  
  # Move from Consent page to Demographics page
  observeEvent(
    input$toDemographics,
    updateNavbarPage(inputId = 'nav', selected = 'Demographics')
  )
  
  output$demographics <- renderUI({
    validate(need(input$toDemographics > 0, "Please select a consent option and then hit 'Next' to proceed to this page"))
    demographicsUI
  })
  
  output$demographicsSubmit <- renderUI({
    if (input$consent == "FALSE") {
      list(
        actionButton("toPracticeInstructions", "Submit Demographics"),
        helpText("Demo Mode: Because you did not agree to participate in the experiment, your data will not be saved.")
      )
    } else { 
      # Only enforce conditions if data is saved
      validate(need(input$nickname != "", "Please select a nickname to continue"))
      validate(need(input$age != "", "Please select your age to continue"))
      validate(need(input$gender != "", "Please select your gender identity to continue"))
      validate(need(input$education != "", "Please select your education level to continue"))
      actionButton("toPracticeInstructions", "Submit Demographics")
    }
  })
  
  # Move from Demographics page to Practice page
  observeEvent(
    input$toPracticeInstructions, {
      updateNavbarPage(inputId = 'nav', selected = 'Practice')
      
      # Only write to DB if consent
      if (input$consent == "TRUE") {
        con <- dbConnect(SQLite(), "20230209-graphicsGroup.db" )
        
        #User start time with the app, use with nickname for unique ID
        timing$startExp <- Sys.time()
        
        #Demographic dataset
        demographics <- data.frame(
          userAppStartTime = timing$startExp,
          consent = input$consent,
          nickname = input$nickname,
          age = input$age,
          gender = input$gender,
          education = input$education
        )
        
        dbWriteTable(con, 'user', demographics, append = T)
        dbDisconnect(con)
      }
    })
  
  output$practice <- renderUI({
    validate(need(input$toPracticeInstructions > 0, "Please fill in demographic information and then click 'Submit Demographics' to proceed to this page"))
    practiceUI
  })
  
  # Render practice plots
  output$prac1 <- renderPlot({p1})
  output$prac2 <- renderPlot({p2})
  output$prac3 <- renderPlot({p3})
  
  # Disable "target" numeric inputs on practice page
  observeEvent(input$beginPractice, {
    shinyjs::disable('dummy7')
    shinyjs::disable('dummy8')
    shinyjs::disable('dummy9')
  })
  
  # Move from Practice to Instructions
  observeEvent(
    input$toInstructions, 
    updateNavbarPage(inputId = 'nav', selected = 'Instructions')
  )
  
  # Validation of ID - don't provide button to move to experiment until ID is valid
  output$toExpBtn <- renderUI({
    validate(need(input$kitID != "", "Please provide kit ID to continue"))
    validate(need(!is.na(as.numeric(input$kitID)), "Please provide kit ID to continue"))
    
    actionButton('toExp', 'Begin')
  })
  
  output$expNextBtn <- renderUI({
    if (input$consent == "FALSE") {
      list(
        actionButton('expNext', 'Next'),
        helpText("Demo Mode: Because you did not agree to participate in the experiment, your data will not be saved.")
      )
    } else { 
      # Only enforce conditions if data is saved
      if (is3dtrial()) {
        validate(need(input$plotID3d != "--Select--", "Please choose a 3d plot ID to continue"))
      }
      validate(need(input$smaller != "", "Please identify the smaller bar to continue"))
      validate(need(input$ratio != "0.50", "Please show the size of the small bar relative to the large bar to continue"))
      actionButton('expNext', 'Next')
    }
  })
  
  
  # Get plots in a particular kit
  plots_in_kit <- reactive({
    validate(need(!is.na(as.numeric(input$kitID)), "Please provide kit ID to continue"))
    kitID_num <- as.numeric(input$kitID)
    if (!is.na(kitID_num)) {
      # Filter data for given kit
      kits[[kitID_num]] %>% 
        bind_rows(.id = 'plot') %>% 
        mutate(file = paste0('data/pilot/Set85/', gsub('.csv', '', file)),
               kit = kitID_num) %>%
        ungroup() %>%
        # Randomize order of kit
        sample_frac(1) %>%
        # Set trial order (ish)
        mutate(trial = 1:n())
    }
  })
  
  plots_trial <- reactive({
    tmp <- plots_in_kit()
    
    # Remove info from 3d print since the user picks out graphs from kit at random
    tmp[tmp$plot == '3dPrint', setdiff(names(tmp), c('plot', 'kit', 'trial'))] <- NA
    
    tmp
  })
  
  plots_3d <- reactive({
    tmp <- plots_in_kit()
    filter(tmp, plot == "3dPrint")
  })
  
  # Initialize reactive values for trial information
  trial_data <- reactiveValues(
    trialID = NA,
    max_trials = 15,
    plots_3d_options = NULL,
    plots_3d_used = NULL,
    remaining_3d = NULL,
    info = NULL,
    full_info = FALSE,
    df = NULL,
    startTime = NULL,
    endTime = NULL,
    whichIsSmaller = NULL,
    byHowMuch = NULL
  )
  
  is3dtrial <- reactive({ # Define a reactive variable that is just is it a 3d plot
    tmp <- isolate(plots_trial()$plot)
    if (length(tmp) > 0) {
      tmp[pmin(trial_data$max_trials, trial_data$trialID)] == '3dPrint'
    } else {
      FALSE
    }
  })
  
  # Move from Instructions to Experiment and initialize values
  observeEvent(input$toExp, {
    updateNavbarPage(inputId = 'nav', selected = 'Experiment')
    
    #First plot start time
    timing$startExp <- Sys.time()
    
    # set trial ID
    trial_data$trialID <- 1
    trial_data$plots_3d_options <- plots_3d()$file
    trial_data$remaining_3d <- plots_3d()$file
    trial_data$max_trials <- max(plots_in_kit()$trial)
  })
  
  observeEvent(trial_data$plots_3d_used, {
    # Update remaining plots automatically when a 3d plot is used
    trial_data$remaining_3d <- setdiff(trial_data$plots_3d_options, 
                                       trial_data$plots_3d_used)
  })
  
  output$printed_graph_choice <- renderUI({
    
    sel_opts <- c('-- Select ID --', trial_data$remaining_3d, 'Other')
    
    if (is3dtrial()) {
      list(
        selectizeInput('plotID3d', 'What is the identifier on the bottom of the graph?',
                       choices = sel_opts),
        conditionalPanel(
          'input.plotID3d=="Other"',
          textInput('incorrectGraph', 'If the identifier on the bottom of the plot does not match any of the available options, please enter the identifier here: ')
        )
      )
    }
  })
  
  # Set dataset and file ID when trial ID is set
  observeEvent(trial_data$trialID, {
    # First set info := current trial row from plots_in_kit()
    trial_data$info <- filter(plots_trial(), trial == trial_data$trialID)
    trial_data$full_info <- !is3dtrial()
  })
  
  # Update dataset when plotID3d is set
  observeEvent(input$`plotID3d`, {
    actual_trial_info <- filter(plots_3d(), file == input$`plotID3d`)
    if (nrow(actual_trial_info) > 0) {
      idx <- which(names(trial_data$info) %in% c("plot", "kit", "trial"))
      trial_data$info[,-idx] <- actual_trial_info[,-idx]
      trial_data$full_info <- TRUE
    }
  })

  # Once file ID is filled in in the info df, we can start the trial
  observeEvent(trial_data$full_info, {
    if (trial_data$full_info) {
      # Get corresponding dataset
      trial_data$df <- datasets$data[[trial_data$info$fileID]]
      
      trial_data$startTime <- Sys.time()
    }
  })
  
  # Output related to selected dataset
  output$dataset <- renderTable(trial_data$df)
  output$text <- renderText(nrow(trial_data$df))
  
  output$bar2d <- renderPlot({
    Bar2D(trial_data$df)
  })
  
  output$print3d <- renderPlot({
    print3DPlot
  })
  
  output$bar3d <- renderRglwidget({
    Bar3D(trial_data$df)
    rglwidget()
  })
  
  output$expPlot <- renderUI({
    validate(need(!is.na(as.numeric(input$kitID)), "Please provide kit ID to continue"))
    
    if (trial_data$trialID) {} # Debugging
    message(sprintf("Trial: %d is a %s", trial_data$trialID, trial_data$info$plot))
    switch(
      as.character(trial_data$info$plot),
      'refresh' = plotOutput('refresh'),
      '2dDigital' = plotOutput('bar2d', width = '70%'),
      '3dPrint' = plotOutput('print3d', width = '70%'),
      '3dDigital' = rglwidgetOutput('bar3d', width = '100%')
    )
  })

  observeEvent(input$expNext, {
    
    # Update data values
    trial_data$endTime <- Sys.time()
    trial_data$whichIsSmaller <- input$smaller
    trial_data$byHowMuch <- input$ratio
    
    # Write data to database
    if (input$consent == "TRUE") {
      con <- dbConnect(SQLite(), '20230209-graphicsGroup.db')
      
      message("Results written to database")
      
      #Save data
      results <- trial_data$info %>% 
        select(-trial) %>%
        mutate(nickname = input$nickname,
               appStartTime = timing$startExp,
               plotStartTime = trial_data$startTime,
               plotEndTime = trial_data$endTime,
               whichIsSmaller = trial_data$whichIsSmaller,
               byHowMuch = trial_data$byHowMuch,
               file = ifelse(is.na(file), input$`plotID3d`, file),
               graphCorrecter = ifelse(plot == '3dPrint', input$incorrectGraph, NA))
      dbWriteTable(con, 'results', results, append = T)
      dbDisconnect(con)
    } else {
      message("Results not written to database - no consent")
    }
    
    try(close3d())
    
    if (is3dtrial()) {
      trial_data$plots_3d_used <- c(trial_data$plots_3d_used, trial_data$info$file)
    }
    
    # Refresh plot
    trial_data$info$plot <- "refresh"
    
    # set trial ID
    trial_data$trialID <- trial_data$trialID + 1
    trial_data$full_info <- FALSE
    
    #Reset plot information
    updateTextInput(inputId = 'incorrectGraph', value = NA)
    updateSelectizeInput(inputId = 'smaller', selected = NA)
    updateSliderInput(inputId = 'ratio', value = 50)
    
    output$completion_code <- renderText(generate_completion_code())
    
    #To exit screen
    if (trial_data$trialID > trial_data$max_trials + 1) {
      updateNavbarPage(inputId = 'nav', selected = 'Finishing Up')
      
      # hideTab(inputId = 'nav', target = 'Experiment')
    }
  })
  
  # Exit screen
  observeEvent(input$reset, {
    refresh()
    updateNavbarPage(inputId = 'nav', selected = 'Research Consent')
    updateNumericInput(inputId = 'kitID', value = NA)
    updateCheckboxInput(inputId = 'consent', value = NA)
    updateTextInput(inputId = 'nickname', value = NA)
    updateNumericInput(inputId = 'age', value = NA)
    updateSelectizeInput(inputId = 'gender', selected = NA)
    updateSelectizeInput(inputId = 'education', selected = NA)
  })

} #End server

# Run the application 
shinyApp(ui = ui, server = server)

