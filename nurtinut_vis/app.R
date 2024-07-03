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
# install.packages("packcircles")
library(shiny)
library(tidyverse)
library(plotly)
library(packcircles)


## assign ID
id = 100

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

## create color palette based on nutrinet colors, do not change
my_phylum_colors <- c(
  "Firmicutes" = "#75b70b",
  "Bacteroidetes" = "#e2007a",
  "Tenericutes" = "#2C73D2",
  "Euryarchaeota" = "#F9F871",
  "Candidatus Melainabacteria" = "#CAA8F5",
  "Actinobacteria" = "#92ceea",
  "Thaumarchaeota" = "#38A3A5",
  "Fusobacteria" = "#DF97AD",
  "Proteobacteria" = "#FFA347",
  "Elusimicrobia" = "#FF9DE8",
  "Lentisphaerae" = "#00c1FF",
  "Verrucomicrobia" = "#B744BE",
  "Synergistetes" = "#025400",
  "Candidatus Thermoplasmatota" = "#008EF5",
  "Candidatus Saccharibacteria" = "#C35355",
  "Spirochaetes" = "#965A4A",
  "Cyanobacteria" = "#FF7378"
)

phylum_colors <- function(Phylum){
  cols <- c(Phylum)
  my_phylum_colors[cols]
}

my_family_colors <- c(
  "Autres" = "#A7AE9C",
  "Firmicutes" = "#75b70bFF",
  "Firmicutes" = "#75b70bAA",
  "Firmicutes" = "#75b70b88",
  "Firmicutes" = "#75b70b77",
  "Firmicutes" = "#75b70b66",
  "Firmicutes" = "#75b70b55",
  "Firmicutes" = "#75b70b44",
  "Firmicutes" = "#75b70b33",
  "Firmicutes" = "#75b70b22",
  "Bacteroidetes" = "#e2007aFF",
  "Bacteroidetes" = "#e2007aAA",
  "Bacteroidetes" = "#e2007a88",
  "Bacteroidetes" = "#e2007a77",
  "Bacteroidetes" = "#e2007a66",
  "Bacteroidetes" = "#e2007a55",
  "Bacteroidetes" = "#e2007a44",
  "Bacteroidetes" = "#e2007a33",
  "Bacteroidetes" = "#e2007a22",
  "Tenericutes" = "#2C73D2EE",
  "Tenericutes" = "#2C73D2CC",
  "Tenericutes" = "#2C73D2AA",
  "Tenericutes" = "#2C73D288",
  "Tenericutes" = "#2C73D277",
  "Tenericutes" = "#2C73D266",
  "Tenericutes" = "#2C73D255",
  "Tenericutes" = "#2C73D244",
  "Tenericutes" = "#2C73D233",
  "Tenericutes" = "#2C73D222",
  "Euryarchaeota" = "#F9F871FF",
  "Euryarchaeota" = "#F9F871AA",
  "Euryarchaeota" = "#F9F87188",
  "Euryarchaeota" = "#F9F87177",
  "Euryarchaeota" = "#F9F87166",
  "Euryarchaeota" = "#F9F87155",
  "Euryarchaeota" = "#F9F87144",
  "Candidatus Melainabacteria" = "#CAA8F5FF",
  "Candidatus Melainabacteria" = "#CAA8F5EE",
  "Candidatus Melainabacteria" = "#CAA8F5CC",
  "Candidatus Melainabacteria" = "#CAA8F5AA",
  "Candidatus Melainabacteria" = "#CAA8F599",
  "Candidatus Melainabacteria" = "#CAA8F588",
  "Candidatus Melainabacteria" = "#CAA8F577",
  "Candidatus Melainabacteria" = "#CAA8F566",
  "Candidatus Melainabacteria" = "#CAA8F555",
  "Candidatus Melainabacteria" = "#CAA8F544",
  "Actinobacteria" = "#92ceeaFF",
  "Actinobacteria" = "#92ceea88",
  "Actinobacteria" = "#92ceea77",
  "Actinobacteria" = "#92ceea66",
  "Actinobacteria" = "#92ceea55",
  "Actinobacteria" = "#92ceea44",
  "Thaumarchaeota" = "#38A3A5EE",
  "Thaumarchaeota" = "#38A3A5CC",
  "Thaumarchaeota" = "#38A3A5AA",
  "Thaumarchaeota" = "#38A3A588",
  "Thaumarchaeota" = "#38A3A577",
  "Thaumarchaeota" = "#38A3A566",
  "Thaumarchaeota" = "#38A3A555",
  "Thaumarchaeota" = "#38A3A544",
  "Thaumarchaeota" = "#38A3A533",
  "Thaumarchaeota" = "#38A3A522",
  "Fusobacteria" = "#DF97ADFF",
  "Fusobacteria" = "#DF97ADEE",
  "Fusobacteria" = "#DF97ADCC",
  "Fusobacteria" = "#DF97ADAA",
  "Fusobacteria" = "#DF97AD99",
  "Fusobacteria" = "#DF97AD88",
  "Fusobacteria" = "#DF97AD77",
  "Fusobacteria" = "#DF97AD66",
  "Fusobacteria" = "#DF97AD55",
  "Fusobacteria" = "#DF97AD44",
  "Proteobacteria" = "#FFA347FF",
  "Proteobacteria" = "#FFA347AA",
  "Proteobacteria" = "#FFA34788",
  "Proteobacteria" = "#FFA34777",
  "Proteobacteria" = "#FFA34766",
  "Proteobacteria" = "#FFA34755",
  "Proteobacteria" = "#FFA34744",
  "Elusimicrobia" = "#FF9DE8FF",
  "Elusimicrobia" = "#FF9DE8DD",
  "Elusimicrobia" = "#FF9DE8BB",
  "Elusimicrobia" = "#FF9DE8AA",
  "Elusimicrobia" = "#FF9DE899",
  "Elusimicrobia" = "#FF9DE888",
  "Elusimicrobia" = "#FF9DE877",
  "Elusimicrobia" = "#FF9DE866",
  "Elusimicrobia" = "#FF9DE855",
  "Elusimicrobia" = "#FF9DE844",
  "Lentisphaerae" = "#00c1FFEE",
  "Lentisphaerae" = "#00c1FFCC",
  "Lentisphaerae" = "#00c1FFAA",
  "Lentisphaerae" = "#00c1FF88",
  "Lentisphaerae" = "#00c1FF77",
  "Lentisphaerae" = "#00c1FF66",
  "Lentisphaerae" = "#00c1FF55",
  "Lentisphaerae" = "#00c1FF44",
  "Lentisphaerae" = "#00c1FF33",
  "Lentisphaerae" = "#00c1FF22",
  "Verrucomicrobia" = "#B744BEEE",
  "Verrucomicrobia" = "#B744BECC",
  "Verrucomicrobia" = "#B744BEAA",
  "Verrucomicrobia" = "#B744BE88",
  "Verrucomicrobia" = "#B744BE77",
  "Verrucomicrobia" = "#B744BE66",
  "Verrucomicrobia" = "#B744BE55",
  "Verrucomicrobia" = "#B744BE44",
  "Verrucomicrobia" = "#B744BE33",
  "Verrucomicrobia" = "#B744BE22",
  "Synergistetes" = "#025400EE",
  "Synergistetes" = "#025400CC",
  "Synergistetes" = "#025400AA",
  "Synergistetes" = "#02540088",
  "Synergistetes" = "#02540077",
  "Synergistetes" = "#02540066",
  "Synergistetes" = "#02540055",
  "Synergistetes" = "#02540044",
  "Synergistetes" = "#02540033",
  "Synergistetes" = "#02540022",
  "Candidatus Thermoplasmatota" = "#008EF5EE",
  "Candidatus Thermoplasmatota" = "#008EF5CC",
  "Candidatus Thermoplasmatota" = "#008EF5AA",
  "Candidatus Thermoplasmatota" = "#008EF588",
  "Candidatus Thermoplasmatota" = "#008EF577",
  "Candidatus Thermoplasmatota" = "#008EF566",
  "Candidatus Thermoplasmatota" = "#008EF555",
  "Candidatus Thermoplasmatota" = "#008EF544",
  "Candidatus Thermoplasmatota" = "#008EF533",
  "Candidatus Thermoplasmatota" = "#008EF522",
  "Candidatus Saccharibacteria" = "#C35355EE",
  "Candidatus Saccharibacteria" = "#C35355CC",
  "Candidatus Saccharibacteria" = "#C35355AA",
  "Candidatus Saccharibacteria" = "#C3535588",
  "Candidatus Saccharibacteria" = "#C3535577",
  "Candidatus Saccharibacteria" = "#C3535566",
  "Candidatus Saccharibacteria" = "#C3535555",
  "Candidatus Saccharibacteria" = "#C3535544",
  "Candidatus Saccharibacteria" = "#C3535533",
  "Candidatus Saccharibacteria" = "#C3535522",
  "Spirochaetes" = "#965A4AEE",
  "Spirochaetes" = "#965A4ACC",
  "Spirochaetes" = "#965A4AAA",
  "Spirochaetes" = "#965A4A88",
  "Spirochaetes" = "#965A4A77",
  "Spirochaetes" = "#965A4A66",
  "Spirochaetes" = "#965A4A55",
  "Spirochaetes" = "#965A4A44",
  "Spirochaetes" = "#965A4A33",
  "Spirochaetes" = "#965A4A22",
  "Cyanobacteria" = "#FF7378EE",
  "Cyanobacteria" = "#FF7378CC",
  "Cyanobacteria" = "#FF7378AA",
  "Cyanobacteria" = "#FF737888",
  "Cyanobacteria" = "#FF737877",
  "Cyanobacteria" = "#FF737866",
  "Cyanobacteria" = "#FF737855",
  "Cyanobacteria" = "#FF737844",
  "Cyanobacteria" = "#FF737833",
  "Cyanobacteria" = "#FF737822"
)

family_donut_colors <- character(11)
x = 0
family_colors <- function(Phylum){
  cols <- c(Phylum)
  for (i in cols){
    x = x + 1
    family_donut_colors[x] <- my_family_colors[i]
    my_family_colors <- my_family_colors[!my_family_colors == my_family_colors[i]]
  }
  return(family_donut_colors)
}

# family_colors <- function(Phylum){
#   color <- my_family_colors[Phylum]
#   my_family_colors <- my_family_colors[!my_family_colors == my_family_colors[Phylum]]
#   return(color)
# }


# family_colors(family_donut_data$Phylum)


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
  filter(Abundance != 0)

### chart functions ###
#phylum donut function
phylum_donut <- function(id){
  phylum_data <- full_data %>%
    filter(SampleID == as.character(id)) %>%
    filter(!is.na(Phylum)) %>%
    filter(!str_detect(Phylum, "unclassified")) %>%
    filter(is.na(Class)) %>%
    filter(is.na(Order)) %>%
    filter(is.na(Family)) %>%
    filter(is.na(Genre)) %>%
    filter(is.na(Species))
  
  fig1 <- phylum_data %>%
    plot_ly(labels = ~Phylum, values = ~Abundance,
            text = ifelse(phylum_data$Abundance < 0.5, "", paste(phylum_data$Phylum)),
            textposition = "outside",
            textinfo = "text",
            hoverinfo = "label+percent",
            type='pie',
            hole=0.5,
            marker = list(colors = ~phylum_colors(Phylum),
                          line = list(color = 'white', width = 1))) %>%
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
    filter(!str_detect(Family, "unclassified")) %>%
    filter(is.na(Genre)) %>%
    filter(is.na(Species)) %>%
    arrange(desc(Abundance))
  
  family_donut_data <- family_data %>%
    group_by(Family = ifelse(row_number() <= 10, Family, "Autres")) %>%
    summarise(across(Abundance, sum)) %>%
    ungroup() %>%
    arrange(desc(Abundance)) %>%
    left_join(family_data) %>%
    select(c("Family", "Abundance", "Phylum")) %>%
    mutate(Phylum = ifelse(Family == "Autres", "Autres", Phylum))
  
  
  return(family_donut_data %>%
           plot_ly(labels = ~Family, values = ~Abundance,
                   text = ifelse(family_donut_data$Family == "Autres", "Autres",
                                 paste0(family_donut_data$Family," (",family_donut_data$Phylum,")")),
                   textposition = "outside",
                   textinfo = "label",
                   hoverinfo = "text+percent",
                   type='pie',
                   hole=0.5,
                   marker = list(colors = ~family_colors(Phylum),
                                 line = list(color = 'white', width = 1)))%>%
           add_pie(hole = 0.5) %>%
           layout(title = list(text = paste(n_distinct(family_data$Family), "Familles"),
                               font = list(color = "#e2007a")),
                  showlegend = FALSE,
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  
  # return(list(family_colors(family_donut_data$Phylum)))
}

# family_donut("1")

### barchart function ###
bar_data <- function(id) {
  phylum_data_full <- full_data %>%
    filter(!is.na(Phylum)) %>%
    filter(!str_detect(Phylum, "unclassified")) %>%
    filter(is.na(Class)) %>%
    filter(is.na(Order)) %>%
    filter(is.na(Family)) %>%
    filter(is.na(Genre)) %>%
    filter(is.na(Species))
  
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
    filter(!str_detect(Genre, "unclassified")) %>%
    filter(is.na(Species))
  
  
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