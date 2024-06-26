#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# install.packages("shiny")
# install.packages("tidyverse")
# install.packages("plotly")
# install.packages("viridis")
# install.packages("DT")
# install.packages("packcircles")
# install.packages("shinyWidgets")
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(packcircles)
# library(shinyWidgets)


## assign ID
id = 10

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

## color palette test
my_colors <- c(
  "#75b70b",
  "#e2007a",
  "#2C73D2",
  "#F9F871",
  "#CAA8F5",
  "#92ceea",
  "#38A3A5",
  "#DF97AD",
  "#FFA347",
  "#FF9DE8",
  "#00c1FF",
  "#B744BE",
  "#025400",
  "#008EF5"
)

## create datasets
full_data <- metadata %>%
  full_join(taxon, by = "SampleID") %>%
  group_by(SampleID) %>%
  arrange(desc(Abundance), .by_group = TRUE) %>%
  mutate(Gender = ifelse(Gender == 1, "Homme", "Femme"))

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
  ungroup() %>%
  filter(Abundance != 0)

genre_data <- full_data %>%
  filter(SampleID == id) %>%
  filter(str_detect(clade_name, "g_|UNCLASSIFIED|_unclassified")) %>%
  filter(!str_detect(clade_name, "s_")) %>%
  mutate(Genre = ifelse(clade_name == "UNCLASSIFIED"|str_detect(clade_name, "_unclassified"),
                        "UNCLASSIFIED", str_sub(str_extract(clade_name, "g_.*"), 4))) %>%
  group_by(Genre = ifelse(Genre == "UNCLASSIFIED", "UNCLASSIFIED", Genre)) %>%
  summarise(across(c(Abundance, Percentage), sum)) %>%
  ungroup()

species_data <- full_data %>%
  filter(SampleID == id) %>%
  filter(str_detect(clade_name, "s_|UNCLASSIFIED|_unclassified")) %>%
  mutate(Species = ifelse(clade_name == "UNCLASSIFIED"|str_detect(clade_name, "_unclassified"),
                          "UNCLASSIFIED", str_sub(str_extract(clade_name, "s_.*"), 4))) %>%
  group_by(Species = ifelse(Species == "UNCLASSIFIED", "UNCLASSIFIED", Species)) %>%
  summarise(across(c(Abundance, Percentage), sum)) %>%
  ungroup() %>%
  filter(Abundance != 0)

### chart functions ###
#phylum donut function
phylum_donut <- function(id){
  phylum_data <- full_data %>%
    filter(SampleID == as.character(id)) %>%
    filter(str_detect(clade_name, "p_")) %>%
    filter(!str_detect(clade_name, "c_|o_|f_|g_|s_|_unclassified")) %>%
    mutate(Phylum = str_sub(str_extract(clade_name, "p_.*"), 4)) %>%
    filter(Abundance != 0)
  
  return(phylum_data %>%
           plot_ly(labels = ~Phylum, values = ~Percentage,
                   text = ifelse(phylum_data$Abundance < 0.5, "", paste(phylum_data$Phylum)),
                   textposition = "outside",
                   textinfo = "text",
                   hoverinfo = "label+percent",
                   type='pie',
                   hole=0.5,
                   marker = list(colors = ~my_colors))%>%
           add_pie(hole = 0.5) %>%
           layout(title = list(text = paste(n_distinct(phylum_data$Phylum), "Phylums"),
                               font = list(color = "#e2007a")),
                  showlegend = FALSE,
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
}

##family donut function
family_donut <- function(id){
  family_donut_data <- full_data %>%
    filter(SampleID == as.character(id)) %>%
    filter(str_detect(clade_name, "f_")) %>%
    filter(!str_detect(clade_name, "g_|s_|_unclassified"))%>%
    group_by(clade_name = ifelse(row_number() < 10, clade_name, "Autres")) %>%
    summarise(across(c(Abundance, Percentage), sum)) %>%
    mutate(Family = ifelse(clade_name == "Autres", "Autres", str_sub(str_extract(clade_name, "f_.*"), 4))) %>%
    ungroup()
  
  return(family_donut_data %>%
           plot_ly(labels = ~Family, values = ~Percentage,
                   text = paste(family_donut_data$Family),
                   textposition = "outside",
                   textinfo = "text",
                   hoverinfo = "label+percent",
                   type='pie',
                   hole=0.5,
                   marker = list(colors = ~my_colors))%>%
           add_pie(hole = 0.5) %>%
           layout(title = list(text = paste(n_distinct(family_data$Family), "Familles"),
                               font = list(color = "#e2007a")),
                  showlegend = FALSE,
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
}

### barchart function ###
bar_data <- function(id) {
  phylum_data_full <- full_data %>%
    filter(str_detect(clade_name, "p_")) %>%
    filter(!str_detect(clade_name, "c_|o_|f_|g_|s_|_unclassified")) %>%
    mutate(Phylum = str_sub(str_extract(clade_name, "p_.*"), 4)) %>%
    filter(Abundance != 0)
  
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
    filter(str_detect(clade_name, "g_")) %>%
    filter(!str_detect(clade_name, "s_|_unclassified")) %>%
    mutate(Genre = str_sub(str_extract(clade_name, "g_.*"), 4)) %>%
    filter(Abundance != 0)
  
  
  packing <- circleProgressiveLayout(genre_data$Abundance, sizetype = 'area')
  bubble_data = cbind(genre_data, packing)
  plotcard <- circleLayoutVertices(packing, npoints=50)
  
  plotcard$Abundance <- rep(bubble_data$Abundance, each=51)
  plotcard$Genre <- rep(bubble_data$Genre, each=51)
  
  bubble <- ggplot(data = plotcard, aes(x=x, y=y)) +
    geom_text(data = filter(bubble_data, Abundance >= 0.5), mapping =
                aes(x, y, size = Abundance, label= Genre)) +
    geom_polygon(data = plotcard, aes(x, y, group = id, fill = Abundance,
                                      text = paste(Genre, "\n", str_sub(Abundance, 1, 4), "%")),
                 color = "black", alpha = 0.6) +
    scale_fill_gradientn(colours=c( "#75b70b", "#b50062", "#e2007a")) +
    # scale_fill_viridis() +
    scale_size_continuous(range = c(1,4)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),
          plot.title = element_text(hjust = 0.5, colour = "#e2007a")) +
    coord_equal() +
    labs(title = paste(n_distinct(plotcard$Genre), "Genres"))
  
  return(ggplotly(bubble, tooltip = "text", width = 1000, height = 600) %>%
           style(hoverinfo = "none", traces = 1)
  )
}

# Define UI  
ui <- fluidPage(
  titlePanel(h1(paste("Vos", n_distinct(species_data$Species), "Microbiotes"),
                align = "center",
                style={'font-size: 40px;
                    font-style: bold;
                    color: #e51a87;
                    background-color:#9ecd54;
                        margin-left: -15px;
                        margin-right: -15px;
                        margin-top: -20px;
                        padding-top: 20px;
                        padding-bottom: 20px;'})),
  fluidRow(
    column(2),
    column(4, plotlyOutput("phylum_donut"),
           align = "center"
           # dataTableOutput("phylum_table")
    ),
    column(4, plotlyOutput("family_donut"),
           align = "center"
           # dataTableOutput("family_table"
    ),
    column(2)),
  # plotlyOutput("genre_donut"),
  #            # dataTableOutput("genre_table")),
  fluidRow(align="center",
           plotlyOutput("bubble_chart", height = "50%")
  ),
  fluidRow(align="center",
           plotlyOutput("barplot")
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
  
  # output$genre_donut <- renderPlotly({
  #   genre_donut(id)
  # })  
  
  # output$phylum_table <- phylum_data %>%
  #     select(Phylum, Abundance) %>%
  #     filter(Abundance != 0) %>%
  #     renderDataTable()
  #  
  # output$family_table <- family_data %>%
  #   select(Family, Abundance) %>%
  #   filter(Abundance != 0) %>%
  #   renderDataTable()
  #
  # output$genre_table <- genre_data %>%
  #   select(Genre, Abundance) %>%
  #   filter(Abundance != 0) %>%
  #   renderDataTable()
  
  output$barplot <- renderPlotly({
    all_bars <- bar_data(id) %>%
      ggplot(aes(fill = Phylum, y = Abundance, x = Group,
                 text = paste(Phylum, "\n", str_sub(Abundance, 1, 4), "%"))) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values= my_colors)+
      # scale_fill_viridis(option = "plasma", discrete = T) +
      labs(title = paste("Vos Phylums vs ___ Nutrinauts")) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, colour = "#e2007a"))
    
    ggplotly(all_bars, tooltip = "text", width = 1100, height = 700)
  })
}

# Run the application
shinyApp(ui = ui, server = server)