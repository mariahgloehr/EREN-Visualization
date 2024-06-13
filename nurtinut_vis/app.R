#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries
library(shiny)
library(tidyverse)
library(plotly)
library(ggtree)
library(packcircles)

#load data
taxon <- read.delim("taxon.txt") %>%
  na.omit() %>%
  pivot_longer(
    cols = X1:X103,
    names_to = "SampleID",
    values_to = "abondance"
  ) %>%
  mutate(SampleID = as.integer(str_sub(SampleID, 2))) %>%
  mutate(Percentage = abondance/100)

metadata <- read.delim("metadata.txt") %>%
  na.omit()

full_data <- metadata %>% 
  full_join(taxon, by = "SampleID") %>%
  group_by(SampleID) %>%
  arrange(desc(abondance), .by_group = T)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
