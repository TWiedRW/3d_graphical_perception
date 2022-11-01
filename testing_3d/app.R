## ---------------------------
##
## Script name: app.R
##
## Purpose of script: Testing 3D bar charts in Shiny
##
## Author: Tyler Wiederich
##
## Date Created: 2022-10-20
##
## ---------------------------
##
## Notes:
##   Cannot read local file????
##
## ---------------------------

library(shiny)
library(rgl)
source('https://raw.githubusercontent.com/TWiedRW/3d_graphical_perception/main/Bar3D.R')


# Define UI for application that draws a histogram
ui <- fluidPage(
    

      sidebarPanel(
        h1('Sidebar'),
        p('Probably will put the testing stuff here. Stuff like instructions, selection of smaller bar, and a numeric input for size.')
      ),
    # Application title
    titlePanel("Test of 3D graphics"),
        mainPanel(
           rglwidgetOutput("rglPlot"),
           imageOutput('plot2')
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  test_plot = normalizePath('plots/Set 1/2D/Type 1/type1_dataset1.jpeg')
  
  output$plot2 = renderImage({
    list(src = test_plot, width="516px", height = "auto")
  }, deleteFile = FALSE)
  
  output$rglPlot <- renderRglwidget({
    plot_dat = read.csv('data/Set 1/2D/Type 1/type1_dataset1.csv')
    Bar3D(plot_dat)
    rglwidget()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)




