# Set language to English
Sys.setlocale(category = "LC_CTYPE", locale = "en_US.UTF-8")

# Specify required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "renv", "shiny")

# Install missing packages
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load required packages
lapply(required_packages, library, character.only = TRUE)

#renv::restore()



# Shiny App
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Tab1", "whatever"),
    tabPanel("Tab2", "blabla"),
    tabPanel("Tab3", "blabla"),
    tabPanel("Tab4", "blabla"),
    tabPanel("Tab5", "blabla")
  )
  
)

server <- function(input, output) 
  
shiny::shinyApp(ui, server)



