#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(DT)

## load data sets
taxon <- read.delim("taxon.txt") %>%
  na.omit() %>%
  pivot_longer(
    cols = X1:X103,
    names_to = "SampleID",
    values_to = "Abondance"
  ) %>%
  mutate(SampleID = as.integer(str_sub(SampleID, 2))) %>%
  mutate(Percentage = Abondance/100)

metadata <- read.delim("metadata.txt") %>%
  na.omit()

full_data <- metadata %>%
  full_join(taxon, by = "SampleID") %>%
  group_by(SampleID) %>%
  arrange(desc(Abondance), .by_group = TRUE)

id = 1

phylum_data <- full_data %>%
  filter(SampleID == id) %>%
  filter(str_detect(clade_name, "p_")) %>%
  filter(!str_detect(clade_name, "c_|o_|f_|g_|s_|_unclassified")) %>%
  mutate(Phylum = str_sub(str_extract(clade_name, "p_.*"), 4)) %>%
  ungroup()

family_data <- full_data %>%
  filter(SampleID == id) %>%
  filter(str_detect(clade_name, "f_")) %>%
  filter(!str_detect(clade_name, "g_|s_|_unclassified")) %>%
  mutate(Family = str_sub(str_extract(clade_name, "f_.*"), 4)) %>%
  ungroup()

genre_data <- full_data %>%
  filter(SampleID == id) %>%
  filter(str_detect(clade_name, "g_")) %>%
  filter(!str_detect(clade_name, "s_|_unclassified")) %>%
  mutate(Genre = str_sub(str_extract(clade_name, "g_.*"), 4)) %>%
  ungroup()


# table_test <- full_data %>%
#   filter(SampleID == id) %>%
#   separate_wider_delim(col = clade_name,
#                        delim = "|",
#                        names = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genre", "Species", "T"),
#                        too_few = "align_start")

# Define UI  
ui <- fluidPage(
  fluidRow(
    column(4, dataTableOutput("phylum_table")),
    column(4, dataTableOutput("family_table")),
    column(4, dataTableOutput("genre_table"))
  )
)

#   tabsetPanel(
#   tabPanel("Phylum",
#            dataTableOutput("phylum_table")),
#   tabPanel("Family",
#            dataTableOutput("family_table")),
#   tabPanel("Genre",
#            dataTableOutput("genre_table"))
# )

#   fluidPage(
#   fluidRow(
#     column(4, dataTableOutput("phylum_table")),
#     column(4, dataTableOutput("family_table")),
#     column(4, dataTableOutput("genre_table"))
#   )
# )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$phylum_table <- phylum_data %>%
    select(Phylum, Abondance) %>%
    renderDataTable()
  
  output$family_table <- family_data %>%
    select(Family, Abondance) %>%
    renderDataTable()
  
  output$genre_table <- genre_data %>%
    select(Genre, Abondance) %>%
    renderDataTable()
}

# Run the application
shinyApp(ui = ui, server = server)