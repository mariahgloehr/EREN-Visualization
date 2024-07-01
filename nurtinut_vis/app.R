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
# install.packages("DT")
# install.packages("packcircles")
library(shiny)
library(tidyverse)
library(plotly)
# library(DT)
library(packcircles)


## assign ID
id = 15

## load data sets
#we want the taxon data in a form where we have the following columns: "clade_name", "SampleID", "Abundance", "Percentage"
taxon <- read.delim("taxon.txt") %>%   #read.csv() if a csv file
  na.omit() %>%
  pivot_longer(         #this function takes the selected columns and makes them rows in one column
    cols = X1:X103,     #change: select all the SampleID columns
    names_to = "SampleID",
    values_to = "Abundance"
  ) %>%
  mutate(SampleID = as.integer(str_sub(SampleID, 2))) %>%  #edits SampleID entries to be singular numbers i.e. "1" instead of "X1"
  mutate(Percentage = Abundance/100) #creates Percentage column

metadata <- read.delim("metadata.txt") %>% #load in metadata, want columns: "SampleID", "Gender", "Age" AND/OR "Age_class", "Region"
  na.omit()

## create color palette based on nutrinet colors, do not change
my_colors <- c(
  "Firmicutes" = "#75b70b",
  "Bacteroidetes" = "#e2007a",
  "Tenericutes" = "#2C73D2",
  "Euryarchaeota" = "#F9F871",
  "Candidatus_Melainabacteria" = "#CAA8F5",
  "Actinobacteria" = "#92ceea",
  "Thaumarchaeota" = "#38A3A5",
  "Fusobacteria" = "#DF97AD",
  "Proteobacteria" = "#FFA347",
  "Elusimicrobia" = "#FF9DE8",
  "Lentisphaerae" = "#00c1FF",
  "Verrucomicrobia" = "#B744BE",
  "Synergistetes" = "#025400",
  "Candidatus_Thermoplasmatota" = "#008EF5",
  "Candidatus_Saccharibacteria" = "#C35355",
  "Spirochaetes" = "#965A4A",
  "Cyanobacteria" = "#FF7378"
)

phylum_colors <- function(x){
  cols <- c(x)
  my_colors[cols]
}

family_colors <- function(x){
  cols <- c(x)
  my_colors[cols]
}

## create new datasets
#create full data set with all taxon data and metadata
full_data <- metadata %>%
  full_join(taxon, by = "SampleID") %>% #join taxon data and metadata by aligning SampleIDs
  mutate(Gender = ifelse(Gender == 1, "Homme", "Femme")) %>% #for labels, in Gender column edit "0" -> "Femme" etc
  separate_wider_delim(col = clade_name,
                       delim = "|",
                       names = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genre", "Species", "T"),
                       too_few = "align_start") %>%
  mutate(Phylum = ifelse(str_detect(str_sub(str_extract(Phylum, "p_.*"), 4), "_"),
                         str_replace(str_sub(str_extract(Phylum, "p_.*"), 4), "_", " "),
                         str_sub(str_extract(Phylum, "p_.*"), 4)),
         Family = str_sub(str_extract(Family, "f_.*"), 4),
         Genre = str_sub(str_extract(Genre, "g_.*"), 4),
         Species = str_sub(str_extract(Species, "s_.*"), 4)) %>%
  select(!c("T")) %>%
  group_by(SampleID) %>%
  arrange(desc(Abundance), .by_group = TRUE) %>%
  filter(Abundance != 0)

### chart functions ###
#phylum donut function
phylum_donut <- function(id){
  phylum_data <- full_data %>%
    filter(SampleID == as.character(id)) %>%
    filter(!is.na(Phylum)) %>%
    filter(is.na(Class)) %>%
    filter(is.na(Order)) %>%
    filter(is.na(Family)) %>%
    filter(is.na(Genre)) %>%
    filter(is.na(Species)) %>%
    filter(!str_detect(Phylum, "_unclassified"))
  
  fig1 <- phylum_data %>%
    plot_ly(labels = ~Phylum, values = ~Percentage,
            text = ifelse(phylum_data$Abundance < 0.5, "", paste(phylum_data$Phylum)),
            textposition = "outside",
            textinfo = "text",
            hoverinfo = "label+percent",
            type='pie',
            hole=0.5,
            marker = list(colors = ~phylum_colors(Phylum)))%>%
    add_pie(hole = 0.5) %>%
    layout(
      title = list(text = paste(n_distinct(phylum_data$Phylum), "Phylums"),
                   font = list(color = "#e2007a", weight = "bold")),
      showlegend = FALSE,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(fig1
  )
}

##family donut function
family_donut <- function(id){
  family_data <- full_data %>%
    filter(SampleID == as.character(id)) %>%
    filter(!is.na(Family)) %>%
    filter(is.na(Genre)) %>%
    filter(is.na(Species))%>%
    filter(!str_detect(Family, "_unclassified"))
  
  family_donut_data <- family_data %>%
    group_by(Family = ifelse(row_number() < 10, Family, "Autres")) %>%
    summarise(across(c(Abundance, Percentage), sum)) %>%
    ungroup() %>%
    mutate(Phylum = ifelse(Family == "Autres", NA, family_data$Phylum))
  
  return(family_donut_data %>%
           plot_ly(labels = ~Family, values = ~Percentage,
                   text = paste(family_donut_data$Family),
                   textposition = "outside",
                   textinfo = "text",
                   hoverinfo = "label+percent",
                   type='pie',
                   hole=0.5,
                   marker = list(colors = ~family_colors(Phylum)))%>%
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
    filter(!is.na(Phylum)) %>%
    filter(is.na(Class)) %>%
    filter(is.na(Order)) %>%
    filter(is.na(Family)) %>%
    filter(is.na(Genre)) %>%
    filter(is.na(Species)) %>%
    filter(!str_detect(Phylum, "_unclassified"))
  
  data_id <- phylum_data_full %>%
    filter(SampleID == as.character(id))
  
  phylum_data_pop <- phylum_data_full %>%
    group_by(Phylum) %>%
    summarise(across(Abundance, sum)) %>%
    mutate(Abundance = (Abundance/sum(Abundance))*100) %>%
    mutate(Group = "Population Totale")
  
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
           mutate(Group = fct_relevel(Group, c("Vous", "Population Totale",
                                               data_id[["Gender"]][[1]], data_id[["Age_class"]][[1]], data_id[["Region"]][[1]]))))
}

## bubble chart function
bubble_chart <- function(ID){
  genre_data <- full_data %>%
    filter(SampleID == as.character(ID)) %>%
    filter(!is.na(Genre)) %>%
    filter(is.na(Species)) %>%
    filter(!str_detect(Genre, "_unclassified"))
  
  
  packing <- circleProgressiveLayout(genre_data$Abundance, sizetype = 'area')
  bubble_data = cbind(genre_data, packing)
  plotcard <- circleLayoutVertices(packing, npoints=50)
  
  plotcard$Abundance <- rep(bubble_data$Abundance, each=51)
  plotcard$Genre <- rep(bubble_data$Genre, each=51)
  plotcard$Phylum <- rep(bubble_data$Phylum, each = 51)
  
  bubble <- ggplot(data = plotcard, aes(x=x, y=y)) +
    geom_text(data = filter(bubble_data, Abundance >= 0.5), mapping =
                aes(x, y, size = Abundance, label= Genre)) +
    geom_polygon(data = plotcard, aes(x, y, group = id, fill = Abundance,
                                      text = paste0(Genre, " (",Phylum,")", "\n", str_sub(Abundance, 1, 4), "%")),
                 color = "black", alpha = 0.6) +
    scale_fill_gradientn(colours=c( "#75b70b", "#b50062", "#e2007a")) +
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
  titlePanel(h1(img(src = "logo-nutrinet.jpg",
                    width = "130px",
                    height = "130px"),
                paste("Votre Microbiote"),
                align = "center",
                style={'font-size: 40px;
                    font-style: bold;
                    color: #e51a87;
                    background-color:#9ecd54;
                        margin-left: -15px;
                        margin-right: -15px;
                        margin-top: -20px;
                        padding-top: 10px;
                        padding-bottom: 10px;'})),
  fluidRow(
    column(6, plotlyOutput("phylum_donut"),
           align = "center"
    ),
    column(6, plotlyOutput("family_donut"),
           align = "center"
    )),
  fluidRow(align="center",
           plotlyOutput("bubble_chart", height = "50%")
  ),
  fluidRow(align="center",
           plotlyOutput("barplot")
  )
)


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
  
  output$barplot <- renderPlotly({
    all_bars <- bar_data(id) %>%
      ggplot(aes(fill = Phylum, y = Abundance, x = Group,
                 text = paste(Phylum, "\n", str_sub(Abundance, 1, 4), "%"))) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = phylum_colors(bar_data(id)$Phylum)) +
      labs(title = paste("Vos Phylums vs ___ Nutrinauts")) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, colour = "#e2007a"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
    
    ggplotly(all_bars, tooltip = "text", width = 1100, height = 700)
  })
}

# Run the application
shinyApp(ui = ui, server = server)