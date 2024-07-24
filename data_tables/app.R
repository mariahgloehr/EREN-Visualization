# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

## load data sets
#we want the taxon data in a form where we have the following columns: "clade_name", "SampleID", "Abundance"
taxon <- read.delim("taxon.txt") %>%   #read.csv() if a csv file
  na.omit() %>%
  pivot_longer(         #this function takes the selected columns and makes them rows in one column
    cols = X1:X103,     #change: select all the SampleID columns
    names_to = "SampleID",
    values_to = "Abundance"
  ) %>%
  mutate(SampleID = as.integer(str_sub(SampleID, 2))) #edits SampleID entries to be singular numbers i.e. "1" instead of "X1"

metadata <- read.delim("metadata.txt") %>% #load in metadata, want columns: "SampleID", "Gender", "Age" AND/OR "Age_class", "Region"
  na.omit()

## create new datasets
#create full data set with all taxon data and metadata
full_data <- metadata %>%
  full_join(taxon, by = "SampleID") %>% #join taxon data and metadata by aligning SampleIDs
  mutate(Gender = ifelse(Gender == 1, "Homme", "Femme")) %>% #for labels, in Gender column edit "0" -> "Femme" etc
  mutate(full_name = clade_name) %>%
  separate_wider_delim(col = full_name,
                       delim = "|",
                       names = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genre", "Species", "T"),
                       too_few = "align_start") %>%
  mutate(Phylum = ifelse(str_detect(str_sub(str_extract(Phylum, "p_.*"), 4), "_"),
                         str_replace(str_sub(str_extract(Phylum, "p_.*"), 4), "_", " "),
                         str_sub(str_extract(Phylum, "p_.*"), 4)),
         Family = ifelse(str_detect(str_sub(str_extract(Family, "f_.*"), 4), "_"),
                         str_replace(str_sub(str_extract(Family, "f_.*"), 4), "_", " "),
                         str_sub(str_extract(Family, "f_.*"), 4)),
         Genre = ifelse(str_detect(str_sub(str_extract(Genre, "g_.*"), 4), "_"),
                        str_replace(str_sub(str_extract(Genre, "g_.*"), 4), "_", " "),
                        str_sub(str_extract(Genre, "g_.*"), 4)),
         Species = str_sub(str_extract(Species, "s_.*"), 4)) %>%
  select(!c("T")) %>%
  group_by(SampleID) %>%
  arrange(desc(Abundance), .by_group = TRUE) %>%
  filter(Abundance != 0) %>%
  ungroup()

##table datasets

unclassified_data <- full_data %>%
  filter(Kingdom == 'UNCLASSIFIED') %>%
  mutate(Microorganisme = Kingdom) %>%
  mutate(Rank = "UNCLASSIFIED") %>%
  select(1:5, Microorganisme, Abundance, Rank, clade_name)

phylum_data <- full_data %>%
  filter(!is.na(Phylum)) %>%
  filter(!str_detect(Phylum, "unclassified")) %>%
  filter(is.na(Class)) %>%
  filter(is.na(Order)) %>%
  filter(is.na(Family)) %>%
  filter(is.na(Genre)) %>%
  filter(is.na(Species)) %>%
  mutate(Microorganisme = Phylum) %>%
  mutate(Rank = 'Phylum') %>%
  select(1:5, Rank, Microorganisme, Abundance, clade_name)


family_data <- full_data %>%
  filter(!is.na(Family)) %>%
  filter(!str_detect(Family, "unclassified")) %>%
  filter(is.na(Genre)) %>%
  filter(is.na(Species)) %>%
  mutate(Microorganisme = Family) %>%
  mutate(Rank = 'Family') %>%
  select(1:5, Rank, Microorganisme, Abundance, clade_name)


genre_data <- full_data %>%
  filter(!is.na(Genre)) %>%
  filter(!str_detect(Genre, "unclassified")) %>%
  filter(is.na(Species)) %>%
  mutate(Microorganisme = Genre) %>%
  mutate(Rank = 'Genre') %>%
  select(1:5, Rank, Microorganisme, Abundance, clade_name)

data_table <- rbind(unclassified_data, phylum_data, family_data, genre_data)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("gender",
                         "Genre",
                         choices = unique(data_table$Gender),
                         selected = c("Homme", "Femme")),
      checkboxGroupInput("age",
                         "Age",
                         choices = unique(arrange(data_table, Age_class)$Age_class),
                         selected = "30-39"),
      checkboxGroupInput("region",
                         "Region",
                         choices = unique(arrange(data_table, Region)$Region),
                         selected = "A")
    ),
    mainPanel(
      span(textOutput("total"), style = "color:black;font-size:30px;font-family:arial;font-weight:bold"),
      tabsetPanel(
        tabPanel("Phylum",
                 dataTableOutput("phylum_table")
        ),
        tabPanel("Famille",
                 dataTableOutput("family_table")),
        tabPanel("Genre",
                 dataTableOutput("genre_table"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(digits=4)
  table_data <- reactive(
    data_table %>%
      filter(Gender %in% input$gender) %>%
      filter(Age_class %in% input$age) %>%
      filter(Region %in% input$region) %>%
      group_by(Microorganisme) %>%
      mutate(Moyenne = sprintf(mean(Abundance), fmt = '%.6f')) %>%
      mutate(Prevalence = n()) %>%
      ungroup() %>%
      mutate(Prevalence = paste0(Prevalence, "/", n_distinct(SampleID),
                                 "(", sprintf((Prevalence/n_distinct(SampleID))*100, fmt = '%.3f'), "%)")) %>%
      select(!Abundance)
  )
  
  output$total <- renderText({
    paste(n_distinct(table_data()$SampleID), "Participants")
  })  
  
  output$phylum_table <- renderDataTable({
    unique(select(filter(table_data(), Rank %in% c("Phylum", "UNCLASSIFIED")),
                  Microorganisme, Moyenne, Prevalence, clade_name))[,c(1,4,2,3)]
  })
  
  output$family_table <- renderDataTable({
    unique(select(filter(table_data(), Rank %in% c("Family", "UNCLASSIFIED")),
                  Microorganisme, Moyenne, Prevalence, clade_name))[,c(1,4,2,3)]
  })
  
  output$genre_table <- renderDataTable({
    unique(select(filter(table_data(), Rank %in% c("Genre", "UNCLASSIFIED")),
                  Microorganisme, Moyenne, Prevalence, clade_name))[,c(1,4,2,3)]
  })
}

# Run the application
shinyApp(ui = ui, server = server)
