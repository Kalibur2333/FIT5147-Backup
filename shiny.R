library(lubridate)
library(dplyr)
library(hms)
library(ggplot2)
library(shiny)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(mapdata)
library(reshape2)
library(plotly)


data = read.csv("coutries.csv")
data_grp_MAP = data %>% group_by(region, Year_2019) %>%
  summarise(.groups = "drop")

dataAsia <- data_grp_MAP %>% filter(region %in% c("China","Bahrain","Iran","Israel","Jordan","Kuwait","Lebanon","Oman","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen","Myanmar","Cambodia","Laos","Thailand","Vietnam","Brunei Darussalam","Indonesia","Malaysia","Philippines","Singapore","Timor-Leste","Mongolia","Taiwan","Japan","Korea Democratic People's Republic of (North)","Korea Republic","Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Sri Lanka","Afghanistan","Armenia","Azerbaijan","Georgia","Kazakhstan","Kyrgyzstan","Tajikistan","Uzbekistan"))
dataEurope <- data_grp_MAP %>% filter(region %in% c("UK","Ireland","Austria","Belgium","France","Germany","Liechtenstein","Luxembourg","Monaco",'Netherlands',"Switzerland",'Denmark',"Finland","Iceland","Norway","Sweden","Andorra","Italy","Malta","Portugal","San Marino","Spain","Albania","Bulgaria","Croatia","Cyprus","North Macedonia","Greece","Moldova","Romania","Slovenia","Montenegro","Serbia","Kosovo","Belarus","Czech Republic","Estonia","Hungary","Latvia","Lithuania","Poland","Russian Federation","Slovakia","Ukraine"))
dataOceania <- data_grp_MAP %>% filter(region %in% c("Australia","New Zealand", "Papua New Guinea", "Solomon Islands", "Vanuatu", "Kiribati", "Marshall Islands", "Nauru", "Fiji", "Samoa", "Tonga", "Tuvalu"))
dataAmerica <- data_grp_MAP %>% filter(region %in% c("Canada", "USA", "Argentina", "Bolivia Plurinational State of", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela Bolivarian republic of", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Antigua and Barbuda", "Barbados", "Cuba", "Dominican Republic", "Haiti", "Jamaica", "St Kitts and Nevis", "St Lucia", "Trinidad and Tobago"))
dataAfrica <- data_grp_MAP %>% filter(region %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo Republic of", "Congo Democratic Republic of", "Côte d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"))

mapdata <- map_data("world")
mapdata <- left_join(mapdata,data_grp_MAP,by = "region")
mapdata <- mapdata %>%
  mutate(Year_2019 = as.numeric(Year_2019),
         groupcode = group)
mapdataAsia <- mapdata %>% filter(region %in% c("China","Bahrain","Iran","Israel","Jordan","Kuwait","Lebanon","Oman","Qatar","Saudi Arabia","Syria","Turkey","United Arab Emirates","Yemen","Myanmar","Cambodia","Laos","Thailand","Vietnam","Brunei Darussalam","Indonesia","Malaysia","Philippines","Singapore","Timor-Leste","Mongolia","Taiwan","Japan","Korea Democratic People's Republic of (North)","Korea Republic","Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Sri Lanka","Afghanistan","Armenia","Azerbaijan","Georgia","Kazakhstan","Kyrgyzstan","Tajikistan","Uzbekistan"))
mapdataEurope <- mapdata %>% filter(region %in% c("UK","Ireland","Austria","Belgium","France","Germany","Liechtenstein","Luxembourg","Monaco",'Netherlands',"Switzerland",'Denmark',"Finland","Iceland","Norway","Sweden","Andorra","Italy","Malta","Portugal","San Marino","Spain","Albania","Bulgaria","Croatia","Cyprus","North Macedonia","Greece","Moldova","Romania","Slovenia","Montenegro","Serbia","Kosovo","Belarus","Czech Republic","Estonia","Hungary","Latvia","Lithuania","Poland","Russian Federation","Slovakia","Ukraine"))
mapdataOceania <- mapdata %>% filter(region %in% c("Australia","New Zealand", "Papua New Guinea", "Solomon Islands", "Vanuatu", "Kiribati", "Marshall Islands", "Nauru", "Fiji", "Samoa", "Tonga", "Tuvalu"))
mapdataAmerica <- mapdata %>% filter(region %in% c("Canada", "USA", "Argentina", "Bolivia Plurinational State of", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela Bolivarian republic of", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Antigua and Barbuda", "Barbados", "Cuba", "Dominican Republic", "Haiti", "Jamaica", "St Kitts and Nevis", "St Lucia", "Trinidad and Tobago"))
mapdataAfrica <- mapdata %>% filter(region %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo Republic of", "Congo Democratic Republic of", "Côte d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"))

mapAsia <- ggplot(mapdataAsia, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = Year_2019, group = groupcode), color = "black")
mapAsia

mapAsiaShow <- mapAsia + scale_fill_gradient(name = "Visitors", 
                                     low = "#011b85", high = "#8ffd01", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
mapAsiaShow


mapEurope <- ggplot(mapdataEurope, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = Year_2019, group = groupcode), color = "black")
mapEurope

mapEuropeShow <- mapEurope + scale_fill_gradient(name = "Visitors", 
                                             low = "#011b85", high = "#8ffd01", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
mapEuropeShow

mapOceania <- ggplot(mapdataOceania, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = Year_2019, group = groupcode), color = "black")
mapOceania

mapOceaniaShow <- mapOceania + scale_fill_gradient(name = "Visitors", 
                                                 low = "#011b85", high = "#8ffd01", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
mapOceaniaShow

mapAmerica <- ggplot(mapdataAmerica, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = Year_2019, group = groupcode), color = "black")
mapAmerica

mapAmericaShow <- mapAmerica + scale_fill_gradient(name = "Visitors", 
                                                   low = "#011b85", high = "#8ffd01", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
mapAmericaShow

mapAfrica <- ggplot(mapdataAfrica, aes(x = long, y = lat)) +
  geom_polygon(aes(fill = Year_2019, group = groupcode), color = "black")
mapAfrica

mapAfricaShow <- mapAfrica + scale_fill_gradient(name = "Visitors", 
                                                   low = "#011b85", high = "#8ffd01", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
mapAfricaShow

data3 <- data.frame(
  type = c("Vocational Education", "Higher Education", "Student other", 
           "Temporary work skilled", "Visitor", "Working Holiday"),
  count = c(6280, 41180, 8840, 8470, 25550, 13180)
)

data3$percent <- data3$count / sum(data3$count) * 100
data3$angle <- 2 * pi * cumsum(data3$percent) - 0.5 * pi

ggplot(data3, aes(x = "", y = percent, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#9467BD")) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  theme_void() +
  labs(title = "Temporary Visas (2014-2019)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0, "lines"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

data4 <- data.frame(
  type = c("New Zealand Citizen", "Permanent Visas", "Temporary Visas"),
  count = c(8950, 26850, 197890)
)

data4$percent <- data4$count / sum(data4$count) * 100
data4$angle <- 2 * pi * cumsum(data4$percent) - 0.5 * pi

ggplot(data4, aes(x = "", y = percent, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  theme_void() +
  labs(title = "Visa Groups (2014-2019") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0, "lines"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

dataMoney <- data.frame(
  Year = c(2014, 2015, 2016, 2017, 2018, 2019),
  China = c(1357, 2173, 2360, 2639, 3270, 3383),
  India = c(193, 249, 298, 376, 543, 508),
  New_Zealand = c(352, 424, 433, 422, 402, 394),
  USA = c(231, 285, 339, 327, 353, 377),
  UK = c(315, 409, 401, 376, 394, 373),
  Malaysia = c(327, 349, 391, 424, 450, 359),
  Singapore = c(233, 312, 281, 314, 311, 319),
  Indonesia = c(190, 179, 219, 221, 270, 269),
  Japan = c(87, 122, 146, 198, 198, 231),
  Korea = c(121, 129, 122, 142, 134, 170)
)
dataMoneyMelted <- melt(dataMoney, id.vars = "Year", variable.name = "Country", value.name = "Value")

ui <- dashboardPage(
  dashboardHeader(title = "Visitor Performance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Toursim performance by country", tabName = "Map", icon = icon("map")),
      menuItem("Visitor Visas Distribution", tabName = "Visa", icon = icon("passport")),
      menuItem("Expenditure of Visitors", tabName = "Money", icon = icon("dollar-sign"))
    )
  ),
  dashboardBody(
    tabItems
    (
      tabItem("Map", 
              fluidRow(
                column(width = 12, h1("International Visitors Performance of Victoria")),
                column(width = 12, h3("Visitor Movements")),
                column(width = 8, plotlyOutput("Map")),
                column(width = 4, selectInput("Region", "Select a region", c("Europe", "Asia", "Oceania", "America", "Africa" ))),
                column(width = 12, "Visitors from Different countries over the world")
              ),
              fluidRow(
                column(width = 8, plotlyOutput("Barplot")),
                column(width = 4, selectInput("Region2", "Select a region", c("Europe", "Asia", "Oceania", "America", "Africa" ))),
                column(width = 12, "Visitors form top countries by continents")
              )
      ),
      tabItem("Visa",
              fluidRow(
                column(width = 12, h1("International Visitors Performance of Victoria")),
                column(width = 12, h3("Visas Distribution")),
                column(width = 6, plotlyOutput("VisaPlot")),
                column(width = 6, plotlyOutput("VisaPlot2")),
                column(width = 4, "Distribution of Temporary Visas and All Visa Types")
              ),
              fluidRow(
                column(width = 6, plotlyOutput("VisaBarPlot")),
                column(width = 6, plotlyOutput("VisaBarPlot2")),
                column(width = 4, "Numbers of Temporary Visas and All Visa Types")
              )
      ),
      tabItem("Money",
              fluidRow(
                column(width = 12, h1("International Visitors Performance of Victoria")),
                column(width = 12, h3("Expenditures from International Visitors")),
                column(width = 12, plotlyOutput("Money")),
                column(width = 4, selectInput("Country", "Select a country", choices = c("China", "India", "New_Zealand", "USA", "UK", "Malaysia", "Singapore", "Indonesia", "Japan", "Korea"))),
                column(width = 4, "Visitors' Expenditure(2014-2019)")
              ),
              fluidRow(
                column(width = 12, plotlyOutput("MoneyBarPlot")),
                column(width = 4, selectInput("Year", "Select a year", choices = c(2014:2019))),
                column(width = 4, "Top Expenditures by Country")
              )
      )
    )
  )
)

server <- function(input, output) {
  p1 <- mapAsia + scale_fill_gradient(name = "Visitors", 
                                  low = "#011b85", high = "#8ffd01", na.value = "grey50")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank())
  
  p2 <- mapEurope + scale_fill_gradient(name = "Visitors", 
                                    low = "#011b85", high = "#8ffd01", na.value = "grey50")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank())
  
  p3 <- mapOceania + scale_fill_gradient(name = "Visitors", 
                                     low = "#011b85", high = "#8ffd01", na.value = "grey50")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank())
  
  p4 <- mapAmerica + scale_fill_gradient(name = "Visitors", 
                                     low = "#011b85", high = "#8ffd01", na.value = "grey50")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank())

  p5 <- mapAfrica + scale_fill_gradient(name = "Visitors", 
                                    low = "#011b85", high = "#8ffd01", na.value = "grey50")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank())
  
  output$Map <- renderPlotly({
    if (length(input$Region) > 0){
      if (input$Region == "Europe") {
        ggplotly(p2)
      } else if (input$Region == "Asia") {
        ggplotly(p1)
      } else if (input$Region == "Oceania") {
        ggplotly(p3)
      } else if (input$Region == "America") {
        ggplotly(p4)
      } else if (input$Region == "Africa") {
        ggplotly(p5)
      }
    }
  })
  
  output$Barplot <- renderPlotly({
    if (length(input$Region) > 0){
      if (input$Region2 == "Europe") {
        p <- dataEurope %>%
          arrange(Year_2019) %>%
          slice_tail(n = 10) %>%
          mutate(region = factor(region, levels = region)) %>%
          ggplot(aes(region, Year_2019, fill = region)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() + 
          theme(legend.position = "none") +
          labs(x = "", y = "")
        ggplotly(p, tooltip = c("fill", "y"))
      } else if (input$Region2 == "Asia") {
        p <- dataAsia %>%
          arrange(Year_2019) %>%
          slice_tail(n = 10) %>%
          mutate(region = factor(region, levels = region)) %>%
          ggplot(aes(region, Year_2019, fill = region)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() + 
          theme(legend.position = "none") +
          labs(x = "", y = "")
        ggplotly(p, tooltip = c("fill", "y"))
      } else if (input$Region2 == "Oceania") {
        p <- dataOceania %>%
          arrange(Year_2019) %>%
          slice_tail(n = 10) %>%
          mutate(region = factor(region, levels = region)) %>%
          ggplot(aes(region, Year_2019, fill = region)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() + 
          theme(legend.position = "none") +
          labs(x = "", y = "")
        ggplotly(p, tooltip = c("fill", "y"))
      } else if (input$Region2 == "America") {
        p <- dataAmerica %>%
          arrange(Year_2019) %>%
          slice_tail(n = 10) %>%
          mutate(region = factor(region, levels = region)) %>%
          ggplot(aes(region, Year_2019, fill = region)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() + 
          theme(legend.position = "none") +
          labs(x = "", y = "")
        ggplotly(p, tooltip = c("fill", "y"))
      } else if (input$Region2 == "Africa") {
        p <- dataAfrica %>%
          arrange(Year_2019) %>%
          slice_tail(n = 10) %>%
          mutate(region = factor(region, levels = region)) %>%
          ggplot(aes(region, Year_2019, fill = region)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_minimal() + 
          theme(legend.position = "none") +
          labs(x = "", y = "")
        ggplotly(p, tooltip = c("fill", "y"))
      }
    }
  })
  
  output$VisaPlot <- renderPlotly({
    data3 <- data.frame(
      type = c("Vocational Education", "Higher Education", "Student other", 
               "Temporary work skilled", "Visitor", "Working Holiday"),
      count = c(6280, 41180, 8840, 8470, 25550, 13180)
    )
    
    data3$percent <- data3$count / sum(data3$count) * 100
    data3$angle <- 2 * pi * cumsum(data3$percent) - 0.5 * pi
    
    plot_ly(data3, labels = ~type,values = ~count, type = "pie") |> 
      layout(title = "Temporary Visas (2014-2019)",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
  })
  
  output$VisaPlot2 <- renderPlotly({
    data4 <- data.frame(
      type = c("New Zealand Citizen", "Permanent Visas", "Temporary Visas"),
      count = c(8950, 26850, 197890)
    )
    
    data4$percent <- data4$count / sum(data4$count) * 100
    data4$angle <- 2 * pi * cumsum(data4$percent) - 0.5 * pi
    
    plot_ly(data4, labels = ~type,values = ~count, type = "pie") |> 
      layout(title = "Visa Types",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
  })
  
  output$VisaBarPlot <- renderPlotly({
    data3 <- data.frame(
      type = c("Vocational Education", "Higher Education", "Student other", 
               "Temporary work skilled", "Visitor", "Working Holiday"),
      count = c(6280, 41180, 8840, 8470, 25550, 13180)
    )
    
    p <- data3 %>%
      arrange(desc(count)) %>%
      mutate(type = factor(type, levels = type)) %>%
      ggplot(aes(type ,count, fill = type)) +
        geom_bar(stat = "identity", width = 0.5) +
        theme_bw() + 
        theme(legend.position = "none", 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Temporary Visas (2014-2019)")
   ggplotly(p, tooltip = c("fill", "y"))   
  })
  
  output$VisaBarPlot2 <- renderPlotly({
    data4 <- data.frame(
      type = c("New Zealand Citizen", "Permanent Visas", "Temporary Visas"),
      count = c(8950, 26850, 197890)
    )
    p <- data4 %>%
      arrange(desc(count)) %>%
      mutate(type = factor(type, levels = type)) %>%
      ggplot(aes(type ,count, fill = type)) +
      geom_bar(stat = "identity", width = 0.5) +
      theme_bw() + 
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Visa Types")
    ggplotly(p, tooltip = c("fill", "y"))   
  })
  
  output$Money <- renderPlotly({
    dataMoneyMeltedSubset <- dataMoneyMelted[dataMoneyMelted$Country == input$Country, ]
    p <- ggplot(dataMoneyMeltedSubset, aes(x = Year, y = Value, color = Country)) +
      geom_line() +
      labs(title = paste("Money Graph(", input$Country, ")", sep = ""), x = "Year", y = "Value") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$MoneyBarPlot <- renderPlotly({
    dataMoneyMeltedSubset1 <- dataMoneyMelted[dataMoneyMelted$Year == as.numeric(input$Year), ]
    dataMoneyMeltedSubset1 <- dataMoneyMeltedSubset1 %>%
      arrange(desc(Value)) %>%
      mutate(Country = factor(Country, levels = Country))
    p <- ggplot(dataMoneyMeltedSubset1, aes(x = Country, y = Value, fill = Country)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = Value)) +
      labs(title = paste("Money Graph(", input$Year, ")", sep = ""), x = "Year", y = "Value") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = c("fill", "y"))
  })
}

shinyApp(ui, server)
