library(shiny)
library(shinyWidgets)
library(tidyverse)

datasets <- readRDS('/Users/tylerwiederich/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Research/3d_graphical_perception/data/pilot/set85data.Rdata')
kits <- readRDS('/Users/tylerwiederich/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Research/3d_graphical_perception/data/pilot/kits.Rdata')


# Pages -------------------------------------------------------------------

#### Instructions ####
#
# Provide clear instructions on step and to record
# the ID provided on the bag of plots
#

instructions <- fluidPage(

    # Application title
    fluidRow(
      column(8, offset = 2,
        h3('Instructions'),
        align = 'center'
    )),
    fluidRow(
      column(8, offset = 2, align = 'left',
      p('Thank you for participating in our experiment on the perceptual judgements on different graphical mediums. You will be presented with a series of graphs and asked to select which of the identified bars are smaller than the other for each graph. You will then be asked to estimate the ratio of the smaller bar to the larger bar. One bar is marked with a circle and the other with a triangle. For each graph, make a quick assessment and do not use anything other than your own judgment for estimating each ratio. The 3D printed graphs will trigger a prompt for you to remove a chart from your assigned box and record the identifier located on the bottom of the graph.'),
      p('Before you start, please enter your identification number into the entry box below and then click “Begin”.')
    )),
    
    fluidRow(
      numericInput('userID', 'ID: ', value = NA, width = '15%',
                   min = 1, max = 21, step = 1),
      align = 'center'),
    fluidRow(
      actionButton('toPracticeScreen', 'Begin'),
      align = 'center'
    )
)


#### Practice Page starting screen ####

ui2 <- fluidPage(
  
  fluidRow(
    column(8, offset = 2, align = 'center',
           h2('Practice Graphs'))
  ),
  fluidRow(
    column(8, offset = 2, align = 'left',
           p('Here are a few practice graphs for you to get acquainted with the testing procedure. Click “Begin Practice” to begin the practice graphs.'))
  ),
  fluidRow(
    column(8, offset = 2, align = 'center',
           actionButton('beginPractice', 'Begin Practice'))
  )
)




#### Begin Experiment screen ####
ui3 <- fluidPage(
  
  fluidRow(
    column(8, offset = 2, align = 'center',
           h2(''))
  ),
  fluidRow(
    column(8, offset = 2, align = 'left',
           p('You are now about to begin the experiment. Please remember to use quick judgements. Click “Begin Experiment” to begin the experiment.'))
  ),
  fluidRow(
    column(8, offset = 2, align = 'center',
           actionButton('beginExp', 'Begin Experiment'))
  )
)


#### Experiment ####
ui4 <- fluidPage(
  
  # fluidRow(
  #   column(8, offset = 2, align = 'center',
  #          plotOutput('testGraph'))
  # ),
  fluidRow(
    tags$head(tags$style(HTML('.irs-single {
            visibility: hidden !important;
    }'))),
    column(3),
    column(3, offset = 0, align = 'center',
           sliderInput('ratio', 'Ratio of smaller bar to larger bar (%)',
                       min = 0, max = 100, value = 0,
                       step = 0.1, ticks = F)),
    column(3, offset = 0, align = 'center',
           numericInput('ratioN', 'Numeric Input (%)',
                        min = 0, max = 100, value = 0))
  ),
  fluidRow(
    column(4, offset = 4, align = 'center',
           selectizeInput('3dID', 'What is the identifier on the bottom of the graph?',
                          choices = c('-- SELECT ID --', paste('Graph', 1:7))))
  ),
  fluidRow(
    sliderTextInput('test', 'Text Input', choices = c('Smaller', seq(0, 1, by = 0.01), 'Larger'),
                    selected = '0.5', grid = F,
                    from_min = 0, from_max = 1,
                    to_min = 0, to_max = 1)
  ),
  fluidRow(
    sliderInput('ratio', label = div(style='width:300px;', 
                                     div(style='float:left;', 'Smaller'), 
                                     div(style='float:right;', 'Larger')),
                min = 0, max = 100, value = 0,
                step = 0.1, ticks = F), align = 'right'
  ),
  fluidRow(tableOutput('data'))
  
)

# https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels




# Experiment UI -----------------------------------------------------------

experimentUI <- fluidPage(
  
  #Plot
  fluidRow(
    column(8, offset = 2, align = 'center',
           #renderRglwidget('plot3Digital'),
           renderPlot('plot'),
           tableOutput('data')),
    ),
  #Which is smaller
  fluidRow(
    column(4, offset = 4, align = 'center',
           radioButtons('smaller', 'Which bar is smaller?',
                        choices = c('Circle (●)', 'Triangle (▲)')))
  ),
  
  #Size
  fluidRow(tags$head(tags$style(HTML('.irs-single {
            visibility: hidden !important;
    }'))),
    column(4, offset = 4, align = 'center',
           sliderInput('ratio', label = div(style='width:300px;', 
                                            div(style='float:left;', 'Smaller'), 
                                            div(style='float:right;', 'Larger')),
                       min = 0, max = 100, value = 0,
                       step = 0.1, ticks = F))
  ), 
  fluidRow(
    column(6, offset = 3, align = 'center',
           selectizeInput('3dID', 'What is the identifier on the bottom of the graph?',
                          choices = c('-- SELECT ID --', paste('Graph', 1:7))),
           textInput('incorrectGraph', 'If the identifier on the bottom of the plot does not match any of the available options, please enter the identifier here: '))
  ),
  fluidRow(
    column(4, offset = 4, align = 'center',
           actionButton('expNext', 'Next'))),
  fluidRow(
    tableOutput('react'), tableOutput('dataset')
  )
)





# Thank you page ----------------------------------------------------------

exitUI <- fluidPage(
  
  # Application title
  fluidRow(
    column(8, offset = 2,
           h3('Thank You'),
           align = 'center'
    )),
  fluidRow(
    column(8, offset = 2, align = 'left',
           p('Your response has been submitted. Please click the "New Submission" button to reset the application for the next user.'),
    )),
  fluidRow(
    actionButton('reset', 'New Submission'),
    align = 'center'
  )
)




# Page Navigation ---------------------------------------------------------

ui <- navbarPage('Perceptual Judgements Experiment',
                 id = 'nav',
                 tabPanel('Instructions', instructions),
                 tabPanel('Practice Screen', ui2),
                 tabPanel('Practice', 'SECTION FOR PRACTICE GRAPHS'),
                 tabPanel('Experiment Screen', ui3),
                 tabPanel('Experiment', experimentUI),
                 tabPanel('Exit Screen', exitUI))








# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  #Initialize reactive kits and data
  reactiveKit <- reactiveValues(df = NA)
  reactiveData <- reactiveValues(df = NA)
  
  #Start experiment from instruction screen
  observeEvent(input$toPracticeScreen, {
    #Record start time
    subjectStartTime <- Sys.time()
    
    #Filter data for given kit
    subjectKit <- kits[[input$userID]] %>% 
      bind_rows(.id = 'plot') %>% 
      mutate(file = paste0('data/pilot/Set85/', gsub('.csv', '', file)),
             userStart = subjectStartTime,
             kit = input$userID)
    
    #Randomize order of kit
    subjectKit <- subjectKit[sample(1:15),] %>% 
      ungroup() %>% 
      mutate(rownumber = row_number())
    
    #Get list of 3D printed identifiers
    printedPlots <- sort(subjectKit$file[subjectKit$plot == '3dPrint'])
    updateSelectizeInput(inputId = '3dID', choices = c('-- Select ID --', printedPlots, 'Other'))
    
    #Display data for test purposes
    output$data <- renderTable(subjectKit)
    
    #Reactive data
    reactiveKit$df <- subjectKit

  })
  
  
  # hideTab(inputId = 'nav', target = 'Instructions')
  # hideTab(inputId = 'nav', target = 'Practice Screen')
  # hideTab(inputId = 'nav', target = 'Practice')
  # hideTab(inputId = 'nav', target = 'Experiment Screen')
  # hideTab(inputId = 'nav', target = 'Experiment')
  
  #Instructions to Practice Screen
  observeEvent(input$toPracticeScreen, {
    updateNavbarPage(inputId = 'nav', selected = 'Practice Screen')
    showTab(inputId = 'nav', target = 'Practice Screen')
    showTab(inputId = 'nav', target = 'Practice')
    #hideTab(inputId = 'nav', target = 'Instructions')
  })
  
  #Practice Screen to Practice
  observeEvent(input$beginPractice, {
    updateNavbarPage(inputId = 'nav', selected = 'Practice')
  })
  
  #Practice to Experiment Screen
  observeEvent(input$beginPractice, {
    updateNavbarPage(inputId = 'nav', selected = 'Practice')
  })
  
  #Experiment Screen to Experiment, initialize first plot
  observeEvent(input$beginExp, {
    updateNavbarPage(inputId = 'nav', selected = 'Experiment')
    # hideTab(inputId = 'nav', target = 'Instructions')
    # hideTab(inputId = 'nav', target = 'Practice Screen')
    # hideTab(inputId = 'nav', target = 'Practice')
    # hideTab(inputId = 'nav', target = 'Experiment Screen')
    
    reactiveData$df <- unnest(datasets[as.numeric(reactiveKit$df[1,'fileID']), 'data'])
    output$dataset <- renderTable(reactiveData$df)
  })
  
  
  #Removing first row of dataframe
  observeEvent(input$expNext, {
    temp <- reactiveKit$df[-1,]
    reactiveKit$df <- temp
  })
  
  output$react <- renderTable(reactiveKit$df)

  #https://stackoverflow.com/questions/39136385/delete-row-of-dt-data-table-in-shiny-app
  
  #Exit screen
  observeEvent(input$reset, {
    updateNavbarPage(inputId = 'nav', selected = 'Instructions')
    updateNumericInput(inputId = 'userID', value = NA)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

