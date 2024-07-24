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
id = 1

## load data sets
#we want the taxon data in a form where we have the following columns: "clade_name", "SampleID", "Abundance"
metadata <- read.delim("metadata.txt") %>% #load in metadata, want columns: "SampleID", "Gender", "Age" AND/OR "Age_class", "Region"
  na.omit()

taxon <- read.delim("taxon.txt") %>%   #read.csv() if a csv file
  na.omit() %>%
  pivot_longer(         #this function takes the selected columns and makes them rows in one column
    cols = X1:X103,     #change: select all the SampleID columns
    names_to = "SampleID",
    values_to = "Abundance"
  ) %>%
  mutate(SampleID = as.integer(str_sub(SampleID, 2))) #edits SampleID entries to be singular numbers i.e. "1" instead of "X1"

## create new datasets
#create full data set with all taxon data and metadata
full_data <- metadata %>%
  full_join(taxon, by = "SampleID") %>% #join taxon data and metadata by aligning SampleIDs
  mutate(Gender = ifelse(Gender == 0, "Homme", "Femme")) %>% #for labels, in Gender column edit "0" -> "Femme" etc
  # split clade_name into multiple rank columns, separating the string at every "|"
  separate_wider_delim(col = clade_name,
                       delim = "|",
                       names = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genre", "Species", "T"),
                       too_few = "align_start") %>%
  #edit names of each microbacteria, get rid of every p_, f_ etc
  #i.e. "g__Candidatus__Aristotella" becomes "Candidatus Aristotella"
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
  select(!c("T")) %>% # get rid of "T" column
  group_by(SampleID) %>%                            # arrange dataset by most SampleID and
  arrange(desc(Abundance), .by_group = TRUE) %>%   # within SampleID by Abundance
  filter(Abundance != 0) %>%   # will only graph present bacteria so get rid of all entries where Abundance = 0
  ungroup()

## create dataframe that only includes classified phylum data
phylum_data_full <- full_data %>%
  filter(!is.na(Phylum)) %>%
  filter(!str_detect(Phylum, "unclassified")) %>%
  #select rows where the only information is Phylum
  #i.e. only selecting k_bacteria|p_firmicutes, and not k_bacteria|p_firmicutes|g_calendria
  #(otherwise we double count abundance )
  filter(is.na(Class)) %>%
  filter(is.na(Order)) %>%
  filter(is.na(Family)) %>%
  filter(is.na(Genre)) %>%
  filter(is.na(Species))

## create dataframe that only includes classified family data (same process as above)
family_data_full <- full_data %>%
  filter(!is.na(Family)) %>%
  filter(!str_detect(Family, "unclassified")) %>%
  filter(is.na(Genre)) %>%
  filter(is.na(Species)) %>%
  arrange(desc(Abundance))

## create dataframe that only includes classified genre data (same process as above)
genre_data_full <- full_data %>%
  filter(!is.na(Genre)) %>%
  filter(!str_detect(Genre, "unclassified")) %>%
  filter(is.na(Species))


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

##function for generating phylum donut chart colors
phylum_colors <- function(Phylum){
  cols <- c(Phylum)
  my_phylum_colors[cols]
}

## create gradient color palette for family colors based on their phylum
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

##function for generating family donut chart gradient colors (for 11 pieces)
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


### chart functions ###
#create function phylum_donut() to generate phylum donut chart for selected ID
phylum_donut <- function(id){
  #filter full phylum dataframe for just the information of the selected SampleID
  phylum_data <- phylum_data_full %>%
    filter(SampleID == as.character(id))
  
  #create donut plot
  fig1 <- phylum_data %>%
    plot_ly(labels = ~Phylum, values = ~Abundance,
            # "text" is the label for each piece of the donut
            #ifelse() function so the label is only showed if piece is big enough text displayed is in paste()
            text = ifelse(phylum_data$Abundance < 0.5, "", paste(phylum_data$Phylum)),
            textposition = "outside",
            textinfo = "text",
            hoverinfo = "label+percent", #interactive information
            type='pie',
            hole=0.5,
            # marker information deals with aesthetics, color of pieces and lines between pieces
            marker = list(colors = ~phylum_colors(Phylum),
                          #use phylum_colors function defined above to get colors
                          #generate white lines between the pieces
                          line = list(color = 'white', width = 1))) %>%
    #layout information includes full graph aesthetics
    layout(
      title = list(text = "Vos Phylums", #title text
                   font = list(color = "#e2007a", weight = "bold")), #title color, bold
      showlegend = FALSE,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), #blank graph background
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) #no legend, lines etc
  
  
  return(fig1
  )
}

##create family donut chart function, very similar process to above phylum chart
family_donut <- function(id){
  family_data <- family_data_full %>%
    filter(SampleID == as.character(id))
  
  family_donut_data <- family_data %>%
    group_by(Family = ifelse(row_number() <= 10, Family, "Autres")) %>% #show top 10 family, otherwise "Other"
    summarise(across(Abundance, sum)) %>% #sum rows to get total abundance of "Others"
    ungroup() %>%
    arrange(desc(Abundance)) %>%
    left_join(family_data) %>%  # summarise function gets rid of the below columns,
    #so join with family_data to get this information back
    select(c("Family", "Abundance", "Phylum")) %>%
    mutate(Phylum = ifelse(Family == "Autres", "Autres", Phylum)) #need Phylum column for coloring
  
  #create family donut
  return(family_donut_data %>%
           plot_ly(labels = ~Family, values = ~Abundance,
                   #text here is the hoverinfo, note hoverinfo = "text+percent" versus in phylum function
                   #hoverinfo = "label+percent"
                   #display both Family and Phylum for clarity of the coloring
                   text = ifelse(family_donut_data$Family == "Autres", "Autres",
                                 paste0(family_donut_data$Family," (",family_donut_data$Phylum,")")),
                   textposition = "outside",
                   textinfo = "label",
                   hoverinfo = "text+percent",
                   type='pie',
                   hole=0.5,
                   marker = list(colors = ~family_colors(Phylum), #use previously defined function family_colors() to get colors
                                 line = list(color = 'white', width = 1)))%>%
           layout(title = list(text = "Vos Familles",
                               font = list(color = "#e2007a")),
                  showlegend = FALSE,
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
}

### barchart function ###
barchart <- function(id) {
  #filter full phylum data to just selected participant (id)
  data_id <- phylum_data_full %>%
    filter(SampleID == as.character(id))
  
  #create new dataset that shows total abundance of phylums in total population
  phylum_data_pop <- phylum_data_full %>%
    group_by(Phylum) %>%
    summarise(across(Abundance, sum)) %>% #sum abundance of each phylum
    ungroup() %>%
    mutate(Abundance = (Abundance/sum(Abundance))*100) %>% #restandardize abundance to be percentage of classified microbacteria present in dataset
    mutate(Group = "Population Totale") #create group column for graphing by grouping
  
  #create new dataset that shows total abundance of phylums in participant
  phylum_data_ID <- phylum_data_full %>%
    filter(SampleID == as.character(id)) %>%
    group_by(Phylum) %>%
    summarise(across(Abundance, sum)) %>%
    ungroup() %>%
    mutate(Abundance = (Abundance/sum(Abundance))*100) %>%
    mutate(Group = "Vous")
  
  #bind total population and participant datasets into one dataset
  phylum_data_bar <- rbind(phylum_data_ID, phylum_data_pop)
  
  #using iteration, create new datasets that show total abundance of phylums
  #within the participant's gender, age, and region group
  for (group in c("Gender", "Age_class", "Region")) {        
    phylum_data_bar <- phylum_data_full %>%
      filter_at(group, ~ . == data_id[[group]][[1]]) %>% #filter for Gender, age_class, or region
      group_by(Phylum) %>%
      summarise(across(Abundance, sum)) %>% #sum abundance of each phylum
      ungroup() %>%
      mutate(Abundance = (Abundance/sum(Abundance))*100) %>% #restandardize
      mutate(Group = data_id[[group]][[1]]) %>% #add group title, i.e. "Homme" or "50-59"
      rbind(phylum_data_bar) #bind each dataset to the full bar dataset created above
    #in order to plot each grouping as a bar together
  }
  
  bar_data <- phylum_data_bar %>%
    #manually set order that columns should go in: participant, total population, Gender, Age, Region
    mutate(Group = fct_relevel(Group, c("Vous", "Population Totale",
                                        data_id[["Gender"]][[1]], data_id[["Age_class"]][[1]],
                                        data_id[["Region"]][[1]])))
  all_bars <- bar_data %>%
    ggplot(aes(fill = Phylum, y = Abundance, x = Group,
               text = paste(Phylum, "\n", str_sub(Abundance, 1, 4), "%"))) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(values = phylum_colors(bar_data$Phylum)) +
    labs(title = paste("Vos Phylums vs Les Autres Nutrinauts")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, colour = "#e2007a"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  #make ggplot interactive
  return(ggplotly(all_bars, tooltip = "text", height = 700, width = 1100))
}

## bubble chart function ##
bubble_chart <- function(ID){
  #filter full genre data to just selected participant (ID)
  genre_data <- genre_data_full %>%
    filter(SampleID == as.character(ID))
  
  # Generate the layout, packing dataframe creates data information to create circles based on area
  packing <- circleProgressiveLayout(genre_data$Abundance, sizetype = 'area')
  # full data to create bubbles with taxon information
  bubble_data = cbind(genre_data, packing)
  # 50 point circles
  plotcard <- circleLayoutVertices(packing, npoints=50)
  
  # add taxon data to plotcard dataframe
  plotcard$Abundance <- rep(bubble_data$Abundance, each=51)
  plotcard$Genre <- rep(bubble_data$Genre, each=51)
  plotcard$Phylum <- rep(bubble_data$Phylum, each = 51)
  
  # plot bubble chart
  bubble <- ggplot(data = plotcard, aes(x=x, y=y)) +  # plug in data with circle geometric info
    # text labels only applied to big enough circles
    geom_text(data = filter(bubble_data, Abundance >= 0.5), mapping =
                aes(x, y, size = Abundance, label= Genre)) +  
    # plot the circles, text input is the desired hover text
    geom_polygon(data = plotcard, aes(x, y, group = id, fill = Abundance,
                                      text = paste0(Genre, " (",Phylum,")", "\n", str_sub(Abundance, 1, 4), "%")),
                 color = "black", alpha = 0.6) +
    # desired gradient nutrinet colors
    scale_fill_gradientn(colours=c( "#75b70b", "#b50062", "#e2007a")) +
    scale_size_continuous(range = c(1,4)) +
    # totally blank graph background and add title color
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),
          plot.title = element_text(hjust = 0.5, colour = "#e2007a")) +
    coord_equal() +
    # desired title
    labs(title = "Vos Genres")
  
  # ggplotly turns plot interactive, tooltip is hover text (set to pre-assigned text above), set desired size
  return(ggplotly(bubble, tooltip = "text", width = 1000, height = 600) %>%
           style(hoverinfo = "none", traces = 1)
         # style = no hoverinfo for the text object, just when hovering over the circles
  )
}

# Define UI  
ui <- fluidPage(
  # image file must be within a "www" folder within the folder that contains the app
  # can change pixel width and height of the image
  # code within paste() is the displayed text
  # code after "style = " changes aesthetic of the text, all these can be changed
  # margins and padding set so the text box extends across the entire screen
  titlePanel(h1(img(src = "logo-nutrinet.png",
                    width = "220px",          
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
  ## p() creates a paragraph, text is in quotations, br() indicates a line break
  ## code after "style = " changes aesthetic of the text and paragraph, all these can be changed
  p(
    "Cher(e) Nutrinaute,",
    br("Merci de votre participation à l'étude NutriNet-Santé ! Afin de vous remercier,
       nous vous présente un brève aperçue de votre microbiote intestinal."),
    style = "font-size: 24px;
    text-align:center"
  ),
  
  ## text within strong() will be bolded, can change color
  ## padding-bottom controls size of space between text box and surrounding objects
  p(
    strong("N.B. :", style = "color:#e2007a"), "Chaque personne a une composition microbienne unique.
    Cette variation est normale et saine. Ce rapport a vocation à encourager la curiosité,",
    strong("il ne s’agit pas d’un avis médical.", style = "color:#e2007a"),
    style = "font-size: 15px;
    text-align:center;
    padding-bottom:10px" # can change space between this text box and following text box
  ),
  ## n_distinct() calculates number of distinct species/families/phylums within the dataframe
  p(
    "À partir de votre échantillon, nous avons analysé un microbiote intestinal diversifié
    composé de",
    strong(n_distinct(filter(full_data, SampleID == as.character(id))$Species), "espèces",
           style = "color:#e2007a"),
    "connus de micro-organisme. Ces micro-organismes sont issus de",
    strong(n_distinct(filter(phylum_data_full, SampleID == as.character(id))$Phylum), "phylums",
           style = "color:#e2007a"),
    "et", strong(n_distinct(filter(family_data_full, SampleID == as.character(id))$Family), "familles",
                 style = "color:#e2007a"),
    ". Les graphiques ci-dessous montrent l'abondance relative de chacun de vos phylums etde
    vos dix familles les plus abondantes. Les familles sont colorées en fonction de leur phylum.",
    br("P.S. tous les graphiques sont interactifs, n'hesitez pas à explorer !"),
    style = "font-size: 17px;
    text-align:center;
    background-color: #75b70b55;
    padding:10px"
  ),
  #creates new row for with two equal sections for each donut chart
  fluidRow(
    column(6, plotlyOutput("phylum_donut"),
           align = "center"
    ),
    column(6, plotlyOutput("family_donut"),
           align = "center"
    )),
  p(
    "Votre microbiote contient aussi",
    strong(n_distinct(filter(genre_data_full, SampleID == as.character(id))$Genre), "genres",
           style = "color:#e2007a"),
    "de micro-organismes, qui sont représentés dans le graphique ci-dessous.
    Chaque cercle est un genre, et la taille traduit l'abondance relative.",
    style = "font-size: 17px;
    text-align:center;
    background-color: #75b70b55;
    padding:10px"
  ),
  #new row for bubble chart
  fluidRow(align="center",
           plotlyOutput("bubble_chart", height = "50%")
  ),
  p(
    "Enfin, l'histogramme suivant compare votre composition de phylums avec celle
    de milliers d'autres nutrinautes. Les barres successives représentent votre composition, la composition moyenne
    de tous les nutrinautes, ainsi que la composition moyenne des nutrinautes appartenant au même sexe,
    catégorie d'âge et région que vous.
    Pour rappelle, un écart par rapport à ces moyennes est normal en raison de
    la variation naturelle du microbiote intestinal.",
    style = "font-size: 17px;
    text-align:center;
    background-color: #75b70b55;
    padding:10px"
  ),
  #new row for bar chart
  fluidRow(align="center",
           plotlyOutput("barplot", height = "50%")
  ),
  p(
    strong("Notice technique :", style = "font-size: 15px"),
    br("Les données utilisées pour créer ce rapport ont été
       obtenues grâce à l'étude NutriGut-Santé et à ses participants volontaires.
       Toutes les données restent strictement confidentielles."),
    style = "font-size: 12px;
    text-align:center;
    background-color: #2C73D255;
    width:800px;
    margin:auto;
    padding:10px;
    margin-bottom:10px
    "
  ),
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  #generate outputs for each chart
  output$bubble_chart <- renderPlotly({
    bubble_chart(id) #call bubble_chart function with set id
  })
  
  output$phylum_donut <- renderPlotly({
    phylum_donut(id) #call phylum_donut function with set id
  })  
  
  output$family_donut <- renderPlotly({
    family_donut(id)  #call family_donut function with set id
  })  
  
  output$barplot <- renderPlotly({
    #create barchart function with set id
    barchart(id)
  })
}

# Run the application
shinyApp(ui = ui, server = server)