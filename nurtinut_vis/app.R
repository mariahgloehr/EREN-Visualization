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
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(packcircles)


## assign ID
id = 7
fig1 <- phylum_data %>%
  plot_ly(labels = ~Phylum, values = ~Percentage,
          text = ifelse(phylum_data$Abundance < 0.5, "", paste(phylum_data$Phylum)),
          textposition = "outside",
          textinfo = "text",
          hoverinfo = "label+percent",
          type='pie',
          hole=0.5,
          marker = list(colors = ~my_colors))%>%
  add_pie(hole = 0.5) %>%
  layout(
    title = list(text = paste(n_distinct(phylum_data$Phylum), "Phylums"),
                 font = list(color = "#e2007a", weight = "bold")),
    showlegend = FALSE,
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# fig2 <- phylum_data %>%
#   plot_ly(labels = ~Phylum, values = ~Percentage,
#           textposition = "inside",
#           textinfo = "percent",
#           hoverinfo = "label+percent",
#           type='pie',
#           hole=0.5,
#           marker = list(colors = ~my_colors))%>%
#   add_pie(hole = 0.5) %>%
#   layout(title = list(text = paste(n_distinct(phylum_data$Phylum), "Phylums"),
#                       font = list(color = "#e2007a", weight = "bold")),
#          showlegend = FALSE,
#          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#
# return(subplot(fig1, fig2)
return(fig1
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
  
  fig1 <- family_donut_data %>%
    plot_ly(labels = ~Family, values = ~Percentage,
            text = paste(family_donut_data$Family),
            textposition = "outside",
            textinfo = "text",
            hoverinfo = "label+percent",
            type='pie',
            hole=0.5,
            marker = list(colors = ~my_colors))%>%
    add_pie(hole = 0.5) %>%
    layout(
      # title = list(text = paste(n_distinct(family_data$Family), "Familles"),
      #                   font = list(color = "#e2007a")),
      showlegend = FALSE,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig2 <- family_donut_data %>%
    plot_ly(labels = ~Family, values = ~Percentage,
            textposition = "inside",
            textinfo = "percent",
            hoverinfo = "label+percent",
            type='pie',
            hole=0.5,
            marker = list(colors = ~my_colors))%>%
    add_pie(hole = 0.5) %>%
    layout(title = list(text = paste(n_distinct(family_data$Family), "Familles"),
                        font = list(color = "#e2007a", weight = "bold")),
           showlegend = FALSE,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(subplot(fig1, fig2)
         titlePanel(h1(img(src = "logo-nutrinet.jpg",
                           width = "130px",
                           height = "130px"),
                       paste("Vos", n_distinct(species_data$Species), "Microbiotes Classés"),
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
                  # dataTableOutput("phylum_table")
           ),
           column(6, plotlyOutput("family_donut"),
                  align = "center"
                  # dataTableOutput("family_table"
           )),
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
        theme(plot.title = element_text(hjust = 0.5, colour = "#e2007a"),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())
      
      ggplotly(all_bars, tooltip = "text", width = 1100, height = 700)
    })
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)