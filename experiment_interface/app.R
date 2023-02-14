library(shiny)
library(shinyWidgets)
library(rgl)
library(RSQLite)
library(tidyverse)
library(rayshader)
library(shinyjs)

datasets <- readRDS('data/set85data.Rdata')
kits <- readRDS('data/kits.Rdata')



# Plotting Functions ------------------------------------------------------

#### 2D Bar Chart ####
Bar2D = function(data, mark_height = 5){
  ggplot(data, mapping = aes(x = GroupOrder, y = Height)) +
    facet_grid(.~Group, switch = 'x') + 
    geom_col(color = 'black',
             fill = NA,
             width = 1) +
    geom_point(data = filter(data, IDchr != ""),
               mapping = aes(x = GroupOrder, y = mark_height, shape = IDchr),
               size = 3) +
    scale_x_discrete() +
    ylim(0, 100) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3.3,
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 20),
          strip.text = element_text(size = 20),
          legend.position = 'none')
}

#### 3D Printed (Choose from kit) ####
print3DPlot <- ggplot(mapping = aes(x = 1, y = 1)) +
  geom_text(aes(label = 'Please randomly select a chart\nfrom your kit'),
            size = 6) +
  theme_void() +
  theme(aspect.ratio = 4/3.3)

#### 3D Bar Chart ####
Bar3D = function(samp, output_style = '3D', scale = 1096.935){

  if(output_style == '3D'){
    emboss <- 0
  } else {
    emboss <- 0.0125
  }

  require(ggplot2)
  require(rayshader)

  p = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                                 fill = Height
  )) +
    facet_grid(.~Group, switch = 'x') +
    geom_tile(color = 'black') +

    scale_fill_gradient(low = 'grey80', high = 'grey80',
                        limits = c(0, 100)) +

    coord_equal() +
    theme(
      legend.position = 'none',
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      panel.background = element_rect(fill = 'white',
                                      color = 'white'),
      plot.background = element_rect(fill = 'white',
                                     color = 'white'),
      strip.text = element_text(size = 20, face = 'bold'),
      panel.grid = element_blank(),
      axis.text = element_blank()
    )



  #Height of bars of interest
  samp[samp[,'Identifier'] == 'random','Height'] <- NA
  p2 = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                                  color = Height + 2)) +
    facet_grid(.~Group, switch = 'x') +
    geom_tile(fill = NA, color = NA) +
    geom_point(mapping = aes(x = GroupOrder, shape = IDchr),
               na.rm = T,
               size = 6) +
    scale_color_gradient(low = '#000000', high = '#000000',
                         limits = c(0, 100)) +
    coord_equal() +
    theme(
      legend.position = 'none',
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      panel.background = element_rect(fill = 'white',
                                      color = 'white'),
      plot.background = element_rect(fill = 'white',
                                     color = 'white'),
      strip.text = element_text(size = 20, face = 'bold'),
      panel.grid = element_blank(),
      axis.text = element_blank()
    )


  plot_gg(p, width = 4.125, height = 1, raytrace = F, scale = scale, multicore = F,
          shadow_intensity = 0,
          emboss_text = emboss,
          #emboss_text = 0,
          preview = F,
          offset_edges = 0.000001,
          units = 'in',
          theta = 20,
          phi = 15,
          soliddepth = -5/100,
          solidcolor = 'grey80',
          background = 'white',
          solidlinecolor = 'grey80',
          shadow = FALSE)


  #THIS ADDS THE POINTS! DO NOT DELETE
  plot_gg(p2, width = 4.125, height = 1, raytrace = F, scale = scale, multicore = F,
          shadow_intensity = 0,
          emboss_text = emboss,
          preview = F,
          units = 'in',
          theta = 20,
          phi = 15,
          soliddepth = -5/100,
          solidcolor = 'grey80',
          background = 'white',
          solidlinecolor = 'grey80',
          shadow = FALSE)
}

















# Plots for Instructions --------------------------------------------------

p1 = ggplot(mapping = aes(x = 1:2, y = c(80, 40))) +
  geom_col(width = 1,
           fill = NA,
           color = 'black') +
  geom_point(aes(y = 5), shape = c(16,17),
             size = 2) +
  ylim(0, 100) +
  labs(title = 'Smaller bar is 50% the \nsize of the larger bar') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 20),
        legend.position = 'none',
        aspect.ratio = 4/3.3)



p2 = ggplot(mapping = aes(x = 1:2, y = c(60, 80))) +
  geom_col(width = 1,
           fill = NA,
           color = 'black') +
  geom_point(aes(y = 5), shape = c(16,17),
             size = 2) +
  ylim(0, 100) +
  labs(title = 'Smaller bar is 75% the \nsize of the larger bar') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 20),
        legend.position = 'none',
        aspect.ratio = 4/3.3)

p3 = ggplot(mapping = aes(x = 1:2, y = c(80, 20))) +
  geom_col(width = 1,
           fill = NA,
           color = 'black') +
  geom_point(aes(y = 5), shape = c(16,17),
             size = 2) +
  ylim(0, 100) +
  labs(title = 'Smaller bar is 25% the \nsize of the larger bar') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 20),
        legend.position = 'none',
        aspect.ratio = 4/3.3)















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



#### Research Acknowledgement ####

acknowledgement <-
  fluidPage(
    fluidRow(
      column(width = 8, offset = 2,
      conditionalPanel(
        condition = "!input.toDemographics || !input.consent",
        h2("Welcome"),
        helpText(
          "In this survey, a series of graphs will be given to you with two bars identified with either a circle or a triangle.",
          "We would like you to respond to the following questions for each graph."),
        helpText("1. Which of the identified bars is smaller?"),
        helpText("2. Approximately what size is the smaller bar in comparison to the larger bar?"),
        helpText("3. For the 3D printed bar charts, what is the identifier on the bottom of the chart?"),
        helpText(
          "Finally we would like to collect some information about you.",
          "(age category, education and gender)"),
        helpText(
          "Your response is voluntary and any information we collect from you will be kept confidential.",
          "Please read the informed consent document (click the button below) before you decide whether to participate."),
        
        a("Show Informed Consent Document", href = "informed_consent.html", target = "_blank"),
        
        checkboxInput("consent","I have read the informed consent document and agree.", width = "100%"),
        
        actionButton("toDemographics", "Next")
      ),
      
      conditionalPanel(
        condition = 'input.consent && input.toDemographics',
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
        
        actionButton("submitdemo", "Submit Demographics")
      )
      
      )
    ))
















#### Instructions ####
#
# Provide clear instructions on step and to record
# the ID provided on the bag of plots
#

instructions <- fluidPage(

    # Application title
    fluidRow(
      column(8, offset = 2,
        h2('Instructions'),
        align = 'center'
    )),
    fluidRow(
      column(8, offset = 2, align = 'left',
      p('Thank you for participating in our experiment on the perceptual judgements on different graphical mediums. 
        You will be presented with a series of graphs and asked to select which of the identified bars are smaller than the other for each graph. 
        You will then be asked to estimate the size of the smaller bar in relation to the larger bar. 
        One bar is marked with a circle and the other with a triangle. 
        For each graph, make a quick assessment and do not use anything other than your own judgment for estimating each ratio. 
        The 3D printed graphs will trigger a prompt for you to remove a chart from your assigned box and record the identifier located on the bottom of the graph.
        For the 3D digital graphs, you are able to rotate the plot using your computer mouse or trackpad'),
      p('Before you start, please enter the number on your kit into the entry box below and then click “Begin”.')
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

practiceScreenUI <- fluidPage(
  
  fluidRow(
    column(8, offset = 2, align = 'center',
           h2('Sample Graphs'))
  ),
  fluidRow(
    column(8, offset = 2, align = 'left',
           p('The next screen will display a few sample graphs. 
             Each graph has the answer to which bar is smaller and what the size of of the smaller bar is in relation to the larger bar.
             '),
           p('When you are finished on the next page, click on the "Continue to Experiment" button to move to the next page.'))
  ),
  fluidRow(
    column(8, offset = 2, align = 'center',
           actionButton('beginPractice', 'Next'))
  )
)







#### Practice Graphs #### 


instructPlots <- fluidPage(
  fluidRow(
    column(12,
      align = 'center',
      h3('Sample Graphs with Solutions')
    )
  ),
  br(),
  fluidRow(
    column(width = 4, offset = 0, plotOutput('prac1')),
    column(width = 4, offset = 0, plotOutput('prac2')),
    column(width = 4, offset = 0, plotOutput('prac3'))
  ),
  fluidRow(
    column(width = 4, offset = 0, selectizeInput('dummy4', 'Which bar is smaller?',
                                                 choices = c('', 'Circle (●)', 'Triangle (▲)'),
                                                 selected = 'Triangle (▲)')),
    column(width = 4, offset = 0, selectizeInput('dummy5', 'Which bar is smaller?',
                                                 choices = c('', 'Circle (●)', 'Triangle (▲)'),
                                                 selected = 'Circle (●)')),
    column(width = 4, offset = 0, selectizeInput('dummy6', 'Which bar is smaller?',
                                                 choices = c('', 'Circle (●)', 'Triangle (▲)'),
                                                 selected = 'Triangle (▲)'))
  ),
  fluidRow(
    tags$head(tags$style(HTML(".irs-single {
            visibility: hidden !important;
            position: fixed;
            }
            
            .irs-min {
            visibility: hidden !important;
            }
            .irs-min:before {
            visibility: visible;
            content: 'Smaller';
            position: left;
            }
            
            .irs-max {
            visibility: hidden !important;
            }
            .irs-max:after {
            visibility: visible;
            content: 'Larger';
            position: left;
            }

            "))),
    column(width = 4, offset = 0, sliderInput('dummy1', label = 'Approximately what size is the smaller bar in comparison to the larger bar?',
                                              min = 0, max = 100, value = 50,
                                              step = 0.1, ticks = F)),
    column(width = 4, offset = 0, sliderInput('dummy2', label = 'Approximately what size is the smaller bar in comparison to the larger bar?',
                                              min = 0, max = 100, value = 75,
                                              step = 0.1, ticks = F)),
    column(width = 4, offset = 0, sliderInput('dummy3', label = 'Approximately what size is the smaller bar in comparison to the larger bar?',
                                              min = 0, max = 100, value = 25,
                                              step = 0.1, ticks = F))
  ),
  fluidRow(align = 'center',
    actionButton(inputId = 'toExpScreen', label = 'Continue to \nExperiment')
  )
)












#### Begin Experiment screen ####
expScreenUI <- fluidPage(
  
  fluidRow(
    column(8, offset = 2, align = 'center',
           h2('Experiment Screen'))
  ),
  fluidRow(
    column(8, offset = 2, align = 'left',
           p('You are now about to begin the experiment. 
             Please remember to use quick judgements. 
             Click “Begin Experiment” to begin the experiment.'))
  ),
  fluidRow(
    column(8, offset = 2, align = 'center',
           actionButton('beginExp', 'Begin Experiment'))
  )
)




















#### Experiment UI ####
#
#
#
#
# 
# experimentUI <- fluidPage(
#   fluidRow(
#     column(
#       4, 
#       tags$head(tags$style(HTML('.irs-single {visibility: hidden !important;}'))),
#       uiOutput('printed_graph_choice'),
#       uiOutput('printed_writein'),
#       br(),
#       selectizeInput('smaller', 'Which bar is smaller?',
#                      choices = c('', 'Circle (●)', 'Triangle (▲)'),
#                      selected = NA),
#       sliderInput('ratio', label = div(style='width:300px;', 
#                                        div(style='float:left;', 'Smaller'), 
#                                        div(style='float:right;', 'Larger')),
#                   min = 0, max = 100, value = 50,
#                   step = 0.1, ticks = F),
#       br(),
#       actionButton('expNext', 'Next')
#     ),
#     column(8, uiOutput('expPlot'))
#     
#   )
# )
experimentUI <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(HTML(".irs-single {
            visibility: hidden !important;
            position: fixed;
            }
            
            .irs-min {
            visibility: hidden !important;
            }
            .irs-min:before {
            visibility: visible;
            content: 'Smaller';
            position: left;
            }
            
            .irs-max {
            visibility: hidden !important;
            }
            .irs-max:after {
            visibility: visible;
            content: 'Total = size of larger bar';
            position: left;
            }

            "))),
      
      uiOutput('printed_graph_choice'),
      uiOutput('printed_writein'),
      br(),
      
      selectizeInput('smaller', 'Which bar is smaller?',
                     choices = c('', 'Circle (●)', 'Triangle (▲)'),
                     selected = NA),
      sliderInput('ratio', label = 'Approximately what size is the smaller bar in comparison to the larger bar?',
                  min = 0, max = 100, value = 50,
                  step = 0.1, ticks = F),
      br(),
      actionButton('expNext', 'Next'),
      width = 4
    ),
    mainPanel(
      uiOutput('expPlot'),
      width = 8
    )
  )
)

# Thank you page ----------------------------------------------------------

exitUI <- fluidPage(
  
  useShinyjs(),
  
  # Application title
  fluidRow(
    column(8, offset = 2,
           h3('Thank You'),
           align = 'center'
    )),
  fluidRow(
    column(8, offset = 2, align = 'left',
           p('Your response has been submitted. Please return the graphs to the kit bag and click the "New Submission" button to reset the application for the next user.'),
    )),
  fluidRow(
    actionButton('reset', 'New Submission'),
    align = 'center'
  )
)




# Page Navigation ---------------------------------------------------------

ui <- navbarPage(
  'Perceptual Judgements Experiment',
  id = 'nav',
  tabPanel('Research Acknowledgement', acknowledgement),
  tabPanel('Instructions', instructions),
  tabPanel('Instructions for Graphs', practiceScreenUI),
  tabPanel('Sample Graphs', instructPlots),
  tabPanel('Experiment Screen', expScreenUI),
  tabPanel('Experiment', experimentUI),
  tabPanel('Exit Screen', exitUI)
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  #### Tab Display/Hide ####
  
  #Initially hide all but research acknowledgement
  hideTab(inputId = 'nav', target = 'Instructions')
  hideTab(inputId = 'nav', target = 'Instructions for Graphs')
  hideTab(inputId = 'nav', target = 'Sample Graphs')
  hideTab(inputId = 'nav', target = 'Experiment Screen')
  hideTab(inputId = 'nav', target = 'Experiment')
  hideTab(inputId = 'nav', target = 'Exit Screen')
  
  
  #### Render sample plots ###
  output$prac1 <- renderPlot({p1})
  output$prac2 <- renderPlot({p2})
  output$prac3 <- renderPlot({p3})
  
  #### Initialize reactive kits and data ####
  reactiveKit <- reactiveValues(df = NA)
  reactiveData <- reactiveValues(df = NA)
  printedPlots <- reactiveValues(vals = NA)
  plotStartTime <- reactiveValues(time = NA)
  plotEndTime <- reactiveValues(time = NA)
  appStartTime <- reactiveValues(time = NA)
  endMarker <- reactiveValues(val = 0)
  plotType <- reactiveValues(val = NA)
  
  
  #### Demographics to Instructions, write demographic info to table ####
  observeEvent(input$submitdemo, {
    
    con <- dbConnect(SQLite(), "20230209-graphicsGroup.db" )
    
    #User start time with the app, use with nickname for unique ID
    appStartTime$time <- Sys.time()
    
    #Demographic dataset
    demographics <- data.frame(
      userAppStartTime = appStartTime$time,
      consent = input$consent,
      nickname = input$nickname,
      age = input$age,
      gender = input$gender,
      education = input$education
    )
    dbWriteTable(con, 'user', demographics, append = T)
    dbDisconnect(con)
    
    
    #Tab updates
    showTab(inputId = 'nav', target = 'Instructions')
    showTab(inputId = 'nav', target = 'Exit Screen')
    
    updateNavbarPage(inputId = 'nav', selected = 'Instructions')
    hideTab(inputId = 'nav', target = 'Research Acknowledgement')
    
  })
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  #### Start experiment from instruction screen ####
  observeEvent(input$toPracticeScreen, {
    
    #-------Validate that user inputted ID
  
    #Filter data for given kit
    subjectKit <- kits[[input$userID]] %>% 
      bind_rows(.id = 'plot') %>% 
      mutate(file = paste0('data/pilot/Set85/', gsub('.csv', '', file)),
             userStart = appStartTime$time,
             kit = input$userID)
    
    #Randomize order of kit
    subjectKit <- subjectKit[sample(1:15),] %>% 
      ungroup()
    
    #Get list of 3D printed identifiers
    printedPlots$vals <- sort(subjectKit$file[subjectKit$plot == '3dPrint'])
    
    #Remove info from 3d print since the user picks out graphs from kit at random
    subjectKit[subjectKit[,'plot'] == '3dPrint', setdiff(names(subjectKit), c('plot', 'kit'))] <- NA
    
    
    #Display data for test purposes
    output$data <- renderTable(subjectKit)
    
    #Reactive data
    reactiveKit$df <- subjectKit
    
    #Tab updates
    showTab(inputId = 'nav', target = 'Instructions')
    showTab(inputId = 'nav', target = 'Exit Screen')
    
    updateNavbarPage(inputId = 'nav', selected = 'Instructions')
    hideTab(inputId = 'nav', target = 'Research Acknowledgement')

  })
  
  
  
  
  
  
  
  
  
  #### Instructions to Practice Screen ####
  observeEvent(input$toPracticeScreen, {
    showTab(inputId = 'nav', target = 'Instructions for Graphs')
    updateNavbarPage(inputId = 'nav', selected = 'Instructions for Graphs')
    hideTab(inputId = 'nav', target = 'Instructions')
  })
  
  
  
  
  
  
  
  
  #### Practice Screen to Practice ####
  observeEvent(input$beginPractice, {
    showTab(inputId = 'nav', target = 'Sample Graphs')
    updateNavbarPage(inputId = 'nav', selected = 'Sample Graphs')
    hideTab(inputId = 'nav', target = 'Instructions for Graphs')
  })
  
  
  
  
  
  #### Practice to Experiment Screen ####
  observeEvent(input$toExpScreen, {
    showTab(inputId = 'nav', target = 'Experiment Screen')
    updateNavbarPage(inputId = 'nav', selected = 'Experiment Screen')
    hideTab(inputId = 'nav', target = 'Sample Graphs')
  })
  
  
  
  
  

  #### Experiment Screen to Experiment ####
  observeEvent(input$beginExp, {
    showTab(inputId = 'nav', target = 'Experiment')
    updateNavbarPage(inputId = 'nav', selected = 'Experiment')
    # hideTab(inputId = 'nav', target = 'Practice Screen')
    # hideTab(inputId = 'nav', target = 'Practice')
    hideTab(inputId = 'nav', target = 'Experiment Screen')
    
    #First plot start time
    plotStartTime$time <- Sys.time()
    
    #Gathering values from dataset list and extracting only the dataset
    reactiveData$df <- unnest(datasets[as.numeric(reactiveKit$df[1,'fileID']), 'data'], cols = c(data))
    output$dataset <- renderTable(reactiveData$df)
    
  })
  
  
  
  

  
  #### Plots ####
  
  #----- I suspect there that these might be causing the errors
  
  output$bar2d <- renderPlot({Bar2D(reactiveData$df)})
  output$print3d <- renderPlot({print3DPlot})
  output$bar3d <- renderRglwidget({
    Bar3D(reactiveData$df)
    rglwidget()
    })

  output$expPlot <- renderUI({
    
    switch(
      as.character(reactiveKit$df[1,'plot']),
      '2dDigital' = plotOutput('bar2d', width = '70%'),
      '3dPrint' = plotOutput('print3d', width = '70%'),
      '3dDigital' = rglwidgetOutput('bar3d', width = '100%')
    )
      
  })
  
  
  
  
  
  
  
  
  #### UI for 3D plot identifier, conditional on if the plot is 3D printed ####
  output$printed_graph_choice <- renderUI({
    if(reactiveKit$df[1,'plot'] == '3dPrint' & nrow(reactiveKit$df) > 0){
      selectizeInput('3dID', 'What is the identifier on the bottom of the graph?',
                     choices = c('-- Select ID --', printedPlots$vals, 'Other'))
    }
    else{
      return()
    }
    
  })
  
  #### UI for manual 3D plot identifier entry ####
  output$printed_writein <- renderUI({
    if(reactiveKit$df[1,'plot'] == '3dPrint' & nrow(reactiveKit$df) > 0){
      textInput('incorrectGraph', 'If the identifier on the bottom of the plot does not match any of the available options, please enter the identifier here: ')
    }
    else {
      return()
    }
  })
  
  
  
  
  
  output$text <- renderText({nrow(reactiveKit$df)})
  
  output$dataset <- renderTable({reactiveData$df})
  
  
  
  
  #### Save data and update next plot ####
  observeEvent(input$expNext, {
    
    plotEndTime$time <- Sys.time()  
    
    try(close3d())
    
    con <- dbConnect(SQLite(), '20230209-graphicsGroup.db')
    
    #Save data
    results <- reactiveKit$df[1,] %>% 
      mutate(nickname = input$nickname,
             appStartTime = appStartTime$time,
             plotStartTime = plotStartTime$time,
             plotEndTime = plotEndTime$time,
             whichIsSmaller = input$smaller,
             byHowMuch = input$ratio,
             file = ifelse(is.na(file), input$`3dID`, file),
             graphCorrecter = ifelse(plot == '3dPrint', input$incorrectGraph, NA))
    dbWriteTable(con, 'results', results, append = T)
    dbDisconnect(con)
    
    #Remove first row from data
    reactiveKit$df <- reactiveKit$df[-1,]
    
    
    
    #Update 3D printed graph list
    printedPlots$vals <- setdiff(c('-- Select ID --', printedPlots$vals, 'Other'),
                                 ifelse(input$`3dID` %in% c('-- Select ID --', 'Other'),
                                        NA, input$`3dID`))
    updateSelectizeInput(inputId = '3dID', choices = printedPlots$vals, selected = '')

    #Updating data for the next dataset
    reactiveData$df <- unnest(datasets[as.numeric(reactiveKit$df[1,'fileID']), 'data'], cols = c(data))
    output$dataset <- renderTable(reactiveData$df)
    
    #Update time for next plot
    plotStartTime$time <- Sys.time()
    
    #Reset plot information
    updateTextInput(inputId = 'incorrectGraph', value = NA)
    updateSelectizeInput(inputId = 'smaller', selected = NA)
    updateSliderInput(inputId = 'ratio', value = 50)
    
    #To exit screen
    if(nrow(reactiveKit$df) == 0){
      updateNavbarPage(inputId = 'nav', selected = 'Exit Screen')
      hideTab(inputId = 'nav', target = 'Experiment')
    }
    else{
    }
  })
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$react <- renderTable(reactiveKit$df)

  #https://stackoverflow.com/questions/39136385/delete-row-of-dt-data-table-in-shiny-app
  
  #### Exit screen ####
  observeEvent(input$reset, {
    
    refresh()
    
    updateNavbarPage(inputId = 'nav', selected = 'Research Acknowledgement')
    updateNumericInput(inputId = 'userID', value = NA)
    updateCheckboxInput(inputId = 'consent', value = NA)
    updateTextInput(inputId = 'nickname', value = NA)
    updateNumericInput(inputId = 'age', value = NA)
    updateSelectizeInput(inputId = 'gender', selected = NA)
    updateSelectizeInput(inputId = 'education', selected = NA)

    
  })
  
  

  
  
  
  
  
  
} #End server

# Run the application 
shinyApp(ui = ui, server = server)

