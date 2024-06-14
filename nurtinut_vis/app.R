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
    values_to = "Abundance"
  ) %>%
  mutate(SampleID = as.integer(str_sub(SampleID, 2))) %>%
  mutate(Percentage = Abundance/100)

metadata <- read.delim("metadata.txt") %>%
  na.omit()

full_data <- metadata %>%
  full_join(taxon, by = "SampleID") %>%
  group_by(SampleID) %>%
  arrange(desc(Abundance), .by_group = TRUE)

id = 1

phylum_data <- full_data %>%
  filter(SampleID == id) %>%
  filter(str_detect(clade_name, "p_|UNCLASSIFIED|_unclassified")) %>%
  filter(!str_detect(clade_name, "c_|o_|f_|g_|s_")) %>%
  mutate(Phylum = ifelse(clade_name == "UNCLASSIFIED"|str_detect(clade_name, "_unclassified"),
                         "UNCLASSIFIED", str_sub(str_extract(clade_name, "p_.*"), 4))) %>%
  group_by(Phylum = ifelse(Phylum == "UNCLASSIFIED", "UNCLASSIFIED", Phylum)) %>%
  summarise(across(c(Abundance, Percentage), sum)) %>%
  ungroup()

family_data <- full_data %>%
  filter(SampleID == id) %>%
  filter(str_detect(clade_name, "f_|UNCLASSIFIED|_unclassified")) %>%
  filter(!str_detect(clade_name, "g_|s_")) %>%
  mutate(Family = ifelse(clade_name == "UNCLASSIFIED"|str_detect(clade_name, "_unclassified"),
                         "UNCLASSIFIED", str_sub(str_extract(clade_name, "f_.*"), 4))) %>%
  group_by(Family = ifelse(Family == "UNCLASSIFIED", "UNCLASSIFIED", Family)) %>%
  summarise(across(c(Abundance, Percentage), sum)) %>%
  ungroup()

genre_data <- full_data %>%
  filter(SampleID == id) %>%
  filter(str_detect(clade_name, "g_|UNCLASSIFIED|_unclassified")) %>%
  filter(!str_detect(clade_name, "s_")) %>%
  mutate(Genre = ifelse(clade_name == "UNCLASSIFIED"|str_detect(clade_name, "_unclassified"),
                        "UNCLASSIFIED", str_sub(str_extract(clade_name, "g_.*"), 4))) %>%
  group_by(Genre = ifelse(Genre == "UNCLASSIFIED", "UNCLASSIFIED", Genre)) %>%
  summarise(across(c(Abundance, Percentage), sum)) %>%
  ungroup()


# table_test <- full_data %>%
#   filter(SampleID == id) %>%
#   separate_wider_delim(col = clade_name,
#                        delim = "|",
#                        names = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genre", "Species", "T"),
#                        too_few = "align_start")

# Define UI  
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Phylum",
             dataTableOutput("phylum_table")),
    tabPanel("Family",
             dataTableOutput("family_table")),
    tabPanel("Genre",
             dataTableOutput("genre_table"))
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
    select(Phylum, Abundance) %>%
    filter(Abundance != 0) %>%
    renderDataTable()
  
  output$family_table <- family_data %>%
    select(Family, Abundance) %>%
    filter(Abundance != 0) %>%
    renderDataTable()
  
  output$genre_table <- genre_data %>%
    select(Genre, Abundance) %>%
    filter(Abundance != 0) %>%
    renderDataTable()
}

# Run the application
shinyApp(ui = ui, server = server)