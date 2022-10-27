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



# Define UI for application that draws a histogram
ui <- fluidPage(
    

      sidebarPanel(
        h1('Sidebar'),
        p('Probably will put the testing stuff here. Stuff like instructions, selection of smaller bar, and a numeric input for size.')
      ),
    # Application title
    titlePanel("Test of 3D graphics"),
        mainPanel(
           #rglwidgetOutput("rglPlot", width = '516px', height = '516px'),
           plotOutput('plot2.2', width = '100px', height = '100px')
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #samp = read.csv('data/type1_dataset54_56.23_compared_to_26.1.csv')
  samp = read.csv('https://raw.githubusercontent.com/TWiedRW/3d_graphical_perception/main/data/type1_dataset54_56.23_compared_to_26.1.csv')
  samp[,'Marker'] <- rep(1:5, 2)
  
  samp[is.na(samp[,'Identifier']),'Marker'] <- NA
  
  
  require(ggplot2)
  require(rayshader)
  
  p = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                                 fill = Height)) +
    facet_grid(.~Group, switch = 'x') + 
    geom_tile() +
    geom_point(mapping = aes(x = Marker), na.rm = T) +
    scale_fill_gradient(low = 'grey20', high = 'grey80',
                        limits = c(0, 100)) +
    coord_equal()
  
  p3 = plot_gg(p, 
               width = 4.125, 
               height = 1, 
               raytrace = F, 
               scale = 500*1.171, 
               multicore = F,
          shadow_intensity = 0,
          units = 'in',
          offset_edges = T,
          theta = 0,
          phi = 0)  
  
  
  
  
  
  output$rglPlot <- renderRglwidget({
    p3
    aspect3d(4.125, 5, 1)
    rglwidget()
  })
  
  output$plot2 <- renderUI({
    img(src = '/Users/tylerwiederich/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Research/3d_graphical_perception/plots/type1_dataset54_56.23_compared_to_26.1.csv.jpeg')
  })
  
  output$plot2.2 <- renderImage({
    list(src = '/Users/tylerwiederich/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Research/3d_graphical_perception/plots/type1_dataset54_56.23_compared_to_26.1.csv.jpeg')
  }, deleteFile = F)
  
}

# Run the application 
shinyApp(ui = ui, server = server)




