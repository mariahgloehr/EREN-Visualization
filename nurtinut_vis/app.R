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
library(plotly)
library(viridis)
library(packcircles)


## assign ID
id = 3

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

## create datasets
full_data <- metadata %>%
  full_join(taxon, by = "SampleID") %>%
  group_by(SampleID) %>%
  arrange(desc(Abundance), .by_group = TRUE) %>%
  mutate(Gender = ifelse(Gender == 1, "Homme", "Femme"))

### chart functions ###
#phylum donut function
phylum_donut <- function(id){
  phylum_data <- full_data %>%
    filter(SampleID == as.character(id)) %>%
    filter(str_detect(clade_name, "p_")) %>%
    filter(!str_detect(clade_name, "c_|o_|f_|g_|s_|_unclassified")) %>%
    mutate(Phylum = str_sub(str_extract(clade_name, "p_.*"), 4))
  
  return(phylum_data %>%
           plot_ly(labels = ~Phylum, values = ~Percentage,
                   text = ifelse(phylum_data$Abundance < 0.5, "", paste(phylum_data$Phylum)),
                   textposition = "outside",
                   textinfo = "text",
                   hoverinfo = "label+percent")%>%
           add_pie(hole = 0.6) %>%
           layout(title = paste(id, "Phylum Chart"),  showlegend = FALSE,
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
}

##family donut function
family_donut <- function(id){
  family_data <- full_data %>%
    filter(SampleID == as.character(id)) %>%
    filter(str_detect(clade_name, "f_")) %>%
    filter(!str_detect(clade_name, "g_|s_|_unclassified"))%>%
    group_by(clade_name = ifelse(row_number() < 10, clade_name, "Autres")) %>%
    summarise(across(c(Abundance, Percentage), sum)) %>%
    mutate(Family = ifelse(clade_name == "Autres", "Autres", str_sub(str_extract(clade_name, "f_.*"), 4)))
  
  return(family_data %>%
           plot_ly(labels = ~Family, values = ~Percentage,
                   textposition = "outside",
                   textinfo = "label",
                   hoverinfo = "label+percent")%>%
           add_pie(hole = 0.6) %>%
           layout(title = paste(id, "Family Chart"),  showlegend = FALSE,
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
}

##genre donut function
genre_donut <- function(id){
  genre_data <- full_data %>%
    filter(SampleID == as.character(id)) %>%
    filter(str_detect(clade_name, "g_")) %>%
    filter(!str_detect(clade_name, "s_|_unclassified"))%>%
    group_by(clade_name = ifelse(row_number() < 20, clade_name, "Autres")) %>%
    summarise(across(c(Abundance, Percentage), sum)) %>%
    mutate(Genre = ifelse(clade_name == "Autres", "Autres", str_sub(str_extract(clade_name, "g_.*"), 4)))
  
  return(genre_data %>%
           plot_ly(labels = ~Genre, values = ~Percentage,
                   textposition = "outside",
                   textinfo = "label",
                   hoverinfo = "label+percent")%>%
           add_pie(hole = 0.6) %>%
           layout(title = paste(id, "Genre Chart"),  showlegend = FALSE,
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
}
### barchart function ###
bar_data <- function(id) {
  phylum_data_full <- full_data %>%
    filter(str_detect(clade_name, "p_")) %>%
    filter(!str_detect(clade_name, "c_|o_|f_|g_|s_|_unclassified")) %>%
    mutate(Phylum = str_sub(str_extract(clade_name, "p_.*"), 4))
  
  data_id <- phylum_data_full %>%
    filter(SampleID == as.character(id))
  
  phylum_data_pop <- phylum_data_full %>%
    group_by(Phylum) %>%
    summarise(across(Abundance, sum)) %>%
    mutate(Abundance = (Abundance/sum(Abundance))*100) %>%
    mutate(Group = "Total Population")
  
  phylum_data_ID <- phylum_data_full %>%
    filter(SampleID == as.character(id)) %>%
    group_by(Phylum) %>%
    summarise(across(Abundance, sum)) %>%
    mutate(Abundance = (Abundance/sum(Abundance))*100) %>%
    mutate(Group = "Vous")
  
  phylum_data_bar <- rbind(phylum_data_ID, phylum_data_pop)
  
  for (group in c("Gender", "Age_class", "Region")) {
    phylum_data_bar <- phylum_data_full %>%
      filter_at(group, ~ . == data_id[[group]][[1]]) %>%
      group_by(Phylum) %>%
      summarise(across(Abundance, sum)) %>%
      mutate(Abundance = (Abundance/sum(Abundance))*100) %>%
      mutate(Group = data_id[[group]][[1]]) %>%
      rbind(phylum_data_bar)
  }
  return(phylum_data_bar %>%
           mutate(Group = fct_relevel(Group, c("Vous", "Total Population",
                                               data_id[["Gender"]][[1]], data_id[["Age_class"]][[1]], data_id[["Region"]][[1]]))))
}


## bubble chart function
bubble_chart <- function(ID){
  genre_data <- full_data %>%
    filter(SampleID == as.character(ID)) %>%
    fitler(str_detect(clade_name, "g_")) %>%
    filter(!str_detect(clade_name, "s_|_unclassified")) %>%
    mutate(Genre = str_sub(str_extract(clade_name, "g_.*"), 4)) %>%
    filter(Abundance != 0)
  
  packing <- circleProgressiveLayout(genre_data$Abundance, sizetype = "area")
  bubble_data <- cbind(genre_data, packing)
  plotcard <- circleLayoutVertices(packing, npoints = 50)
  
  plotcard$Abundance <- rep(bubble_data$Abundance, each = 51)
  plotcard$Genre <- rep(bubble_data$Genre, each = 51)
  
  bubble <- ggplot(data = plotcard, aes(x = x, y = y)) +
    geom_text(data = filter(bubble_data, Abundance >= 0.5), 
              mapping = aes(x,y, size = Abundance, label = Genre)) +
    geom_polygon(data = plotcard, aes(x,y, group = id, fill = Abundance,
                                      text = paste(Genre, "\n", str_sub(Abundance, 1, 4), "%")),
                 color = "black", alpha = 0.6) +
    scale_fill_viridis() +
    scale_size_continuous(range = c(1,4)) +
    theme_void() +
    theme(legend.position = "none") +
    coord_equal() +
    labs(title = paste("Vos", n_distinct(plotcard$Genre), "Microbiotes"))
  
  return(ggplotly(bubble, tooltip = "test") %>%
           style(hoverinfo = "none", traces = 1))
}

### table datasets ###
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
             plotlyOutput("phylum_donut"),
             dataTableOutput("phylum_table")
    ),
    tabPanel("Family",
             plotlyOutput("family_donut"),
             dataTableOutput("family_table")),
    tabPanel("Genre",
             plotlyOutput("genre_donut"),
             dataTableOutput("genre_table")),
    tabPanel("Comparison",
             plotlyOutput("barplot")),
    tabPanel("Bubble Chart",
             plotlyOutput("bubble_chart"))
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
  
  output$bubble_chart <- renderPlotly({
    bubble_chart(id)
  })
  
  output$phylum_donut <- renderPlotly({
    phylum_donut(id)
  })  
  
  output$family_donut <- renderPlotly({
    family_donut(id)
  })  
  
  output$genre_donut <- renderPlotly({
    genre_donut(id)
  })  
  
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
  
  output$barplot <- renderPlotly({
    all_bars <- bar_data(id) %>%
      ggplot(aes(fill = Phylum, y = Abundance, x = Group,
                 text = paste(Phylum, "\n", str_sub(Abundance, 1, 4), "%"))) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_viridis(option = "plasma", discrete = T) +
      labs(title = paste(id,"'s Phylum Comparison"))
    
    ggplotly(all_bars, tooltip = "text")
  })
}

# Run the application
shinyApp(ui = ui, server = server)