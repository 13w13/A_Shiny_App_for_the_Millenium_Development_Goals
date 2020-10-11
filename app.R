##Exercice : Shiny Deaths from Covid-19

#rajouter region : simon

#Mettre menu selection sur le haut : alexandra 

#histro : aggregation proporitonelle avec les indicateurs (virer pays)
#map : antoine et alexandra. Choroleptre

#(a la fin) Side panel : couper a pays pour mettre a droite. 


#library
library(shiny)
library(magrittr)
library(data.table)
library(ggplot2)
library(readr)
library(httr)
library(readxl)
library(stringr)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(shinythemes)
install.packages("mapproj")
library(mapproj)
 #global_scope
selected_country <- unique(PlotDT$Country_Name)
selected_topic <- unique(goalD$Topic)
selected_subtopic_1 <- list()
selected_subtopic_2 <- list()
selected_subtopic_3 <- list()
selected_indicator <- list()
selected_aggregation<-unique(colnames(Country_break[,2:5]))
Plot_choices<-PlotDT_Region#Initizalise

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
  #CSS
  #tags$head(
    #tags$style(HTML("
      #h1 {
      #font-family: 'Lobster', cursive;
      #font-weight: 500;
      #line-height: 1.1;
      #color: green;
      #}
      
      #h2 {
      #font-family: 'Lobster', cursive;
      #font-weight: 500;
      #line-height: 1.1;
      #color: green;
      #}
      
      #body {
      #background-color: #fff;
      #}
      
      #.selectize-input {
      #min-height: 20px;
      #border: 0;
      #padding: 4px;
      #font-family: 'Lobster', cursive;
      #}
      
    #"))
  #),
  
  titlePanel(
    # app title/description
    h1("Millenium Development Goals"),
  ),
  sidebarLayout(
    sidebarPanel(
      helpText("Here you can find some graphical information
                     about World Development Goals"),
      br(), 
      helpText("First, choose the World Development Indicators."),
      br(), 
      
      # inputs
      selectInput("topic", 
                  h2("Choose a topic", align = "center"),
                  selected_topic),
      
      br(), 
      selectInput("subtopic_1", 
                  h2("Choose a subtopic 1", align = "center"),
                  selected_subtopic_1, 
                  choices = NULL),
      
      br(), 
      selectInput("subtopic_2", 
                  h2("Choose a subtopic 2", align = "center"),
                  selected_subtopic_2, 
                  choices = NULL),
      
      br(), 
      selectInput("subtopic_3", 
                  h2("Choose a subtopic 3", align = "center"),
                  selected_subtopic_3, 
                  choices = NULL),
      
      br(), 
      selectInput("indicator", 
                  h2("Choose a indicator", align = "center"),
                  selected_indicator, 
                  choices = NULL),
      
      br(), 
      
      selectInput("country", 
                  h2("Choose a country", align = "center"),
                  selected_country, 
                  "France"),
      br(), 
      selectInput("aggregation", 
                  h2("Choose an aggregation view", align = "center"),
                  selected_aggregation, 
                  "Region"),

      br(),
      radioButtons("y_axis_choice", 
                   h2("Axis :", align = "center"), 
                   c("linear", "logarithmic")), 
      
      br(), 
      dateRangeInput("date_choice", 
                     h2("Choose a date range :", align="center"),
                     format = "yyyy",
                     start="1972"),
      
      br(),
      p(strong("Full data is available just below."), style="strong"),
      br(),
      a(strong("DATA AVAILABLE HERE"), href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"),
      br(),
      img(src="https://i1.wp.com/www.un.org/sustainabledevelopment/wp-content/uploads/2015/12/english_SDG_17goals_poster_all_languages_with_UN_emblem_1.png?fit=728%2C451&ssl=1", height = 72, width = 72, style="margin-left:80px"),
      br(), 
  
    ),
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel(h3("GGPLOT"), (plotlyOutput("displot"))),
                  tabPanel(h3("HISTO"), (plotlyOutput("displot2"))),
                  tabPanel(h3("MAP"), plotOutput("displot3"))
      )
    )
  )
)


#  Define a server for the Shiny app
server <- function(input, output, session) {
  
  observeEvent(input$topic, {
    #remise a zero de l'indicator
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = ''
      )
      #
    
      #look if there is something in SubTopic1
      choices <- unique(goalD[goalD$Topic == input$topic, list(SubTopic1)])
      
      #change the value of subtopic_1 in function of the value of topic  
      updateSelectInput(
      session,
      inputId = "subtopic_1",
      choices = choices
      )
  })
  
  observeEvent(input$subtopic_1, {
    #look if there is something in SubTopic2
    choices <- unique(goalD[goalD$SubTopic1 == input$subtopic_1, list(SubTopic2)])
    
    not_value_subtopic_2 = as.vector(is.na(choices[1]))
  
    #change the value of subtopic_2 in function of the value of topic
    if(not_value_subtopic_2) {
      updateSelectInput(
        session,
        inputId = "subtopic_2",
        choices = ''
      )
      
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = ''
      )
      
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic1 == input$subtopic_1, list(Series_Name.x)])
      )
      #
      
    }
    else {
      updateSelectInput(
        session,
        inputId = "subtopic_2",
        choices = choices
      )
    }
      
  })
  
  observeEvent(input$subtopic_2, {
    if (input$subtopic_2 != '') {
    #look if there is something in SubTopic3
    choices <- unique(goalD[goalD$SubTopic2 == input$subtopic_2, list(SubTopic3)])
    
    not_value_subtopic_3 = as.vector(is.na(choices[1]))
    
    #change the value of subtopic_2 in function of the value of topic
    if(not_value_subtopic_3) {
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = ''
      )
      
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic2 == input$subtopic_2, list(Series_Name.x)])
      )
      #
      
    }
    else {
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = choices
      )
    }
    }
    
  })
  
  observeEvent(input$subtopic_3, {
    if (input$subtopic_3 != '') {
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic3 == input$subtopic_3, list(Series_Name.x)])
      )
      #
    }
  })
  
  
  #Store aggregation selected
  #Try whit ObservedEvent and stock change in new datatable
  
  #observeEvent(input$aggregation, {
  #  isolate({
  #  if(input$aggregation =="Region") {
  #    Plot_choices <- PlotDT_Region
  #  } else if(input$aggregation =="Income_group") {
  #    Plot_choices <- PlotDT_Income_group
  #  } else {
  #    Plot_choices <- PlotDT_Other
  #  }
  #  })
  #}
  #)
  
  #add condition if selected : aggregation view or single view (donc 2 groupe de plot)

  output$displot <- renderPlotly({
    
    p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
    
    Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                           "Income_group"=PlotDT_Income_group,
                           "Lending_category"=PlotDT_Lending_category, 
                           "Other"=PlotDT_Other)
    
    color <- switch(input$aggregation,"Region"=PlotDT_Region,
                    "Income_group"=PlotDT_Income_group,
                    "Lending_category"=PlotDT_Lending_category, 
                    "Other"=PlotDT_Other)
  
    q<-ggplot() +
      geom_line(data=PlotDT[`Country_Name`==input$country &`Series_Name.x`==input$indicator], 
                aes(x=Date, y=Value,colour=input$country))+ 
      geom_line(data=Plot_choices[`Series_Name.x`==input$indicator], 
                aes_string("Date", "Value", colour = input$aggregation)) + 
      xlab("Dates")+
      ylab(input$indicator)+
      p
  
    ggplotly(q)

})
  
  
  output$displot2 <- renderPlotly({
    p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
    
    Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                           "Income_group"=PlotDT_Income_group,
                           "Lending_Category"=PlotDT_Lending_category, 
                           "Other"=PlotDT_Other)
    
     q <- ggplot() + geom_bar(data=PlotDT[`Country_Name`==input$country 
                                         &`Series_Name.x`==input$indicator], 
                             aes(x=Date,y=Value, colour=input$country), stat="identity")+
      geom_bar(data=Plot_choices[`Series_Name.x`==input$indicator], 
               aes(x=Date,y=Value, colour=input$aggregation), stat="identity")
    q
    
    
  })
  
  
  output$displot3 <- renderPlot({
    WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
    
    df <- data.frame(region=input$country, 
                     value=11, 
                     stringsAsFactors=FALSE)
    
    p <- ggplot() +
      geom_map(data = WorldData, map = WorldData,
               aes(x = long, y = lat, group = group, map_id=region),
               fill = "white", colour = "#7f7f7f", size=0.5) + 
      geom_map(data = df, map=WorldData,
               aes(fill=value, map_id=region),
               colour="#7f7f7f", size=0.5) +
      coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
      scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
      scale_y_continuous(breaks=c()) +
      scale_x_continuous(breaks=c()) +
      labs(fill="legend", title="Title", x="", y="") +
      theme_bw() +
      ggtitle(paste("You choose : ", input$country))
    
    p 
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
