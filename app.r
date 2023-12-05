library(shiny)
library(bslib)
library(ggplot2)
library(echarts4r)
library(reshape)
library(leaflet)
library(shinyjs)
library(scroller)
library(tidyverse)
library(crosstalk)
library(plotly)
library(RColorBrewer)


num_colors <- 10
color_palette <- brewer.pal(n = num_colors, name = "Paired")



results_df <- read.csv(file="data/results_all_per_year_with_code.csv",
                       head=TRUE, fileEncoding='UTF-8-BOM', stringsAsFactors=FALSE)

df_population <- read.csv(file="data/population over ages.csv",
                            head=TRUE, fileEncoding='UTF-8-BOM')

df_gdp <- read.csv(file="data/gdp_data2.csv",
                         head=TRUE, fileEncoding='UTF-8-BOM')

total_available_medals <- 10237

population_medals <- results_df %>% 
  filter(year >= 1950) %>% 
  filter( year!= "All") %>% 
  mutate(year=as.numeric(year)) %>% 
  inner_join(df_population, by = join_by(year == Year, country == Country))

population_medals <- population_medals %>% 
  mutate(Population=Population/1000000) 

gdp_medals <- results_df %>% 
  filter(year >= 1962) %>% 
  filter( year!= "All") %>% 
  mutate(year=as.numeric(year)) %>% 
  inner_join(df_gdp, by = join_by(year == year, country == Country))

stadiums <-  read.csv(file="data/country_total_medals.csv",
                      head=TRUE, fileEncoding='UTF-8-BOM')

ui <- shinyUI(
  
  tags$div(id = "homePage",
           navbarPage(theme = bs_theme(version = 4,
                                       primary = "rgba(0, 0, 0, 0.5)", 
                                       secondary = "#000000", 
                                       success = "#000000", 
                                       info = "#000000", 
                                       base_font = font_google("Montserrat"), 
                                       code_font = font_google("Montserrat"), 
                                       heading_font = font_google("Montserrat"), 
                                       font_scale = NULL, 
                                       `enable-gradients` = TRUE, 
                                       bootswatch = "flatly"),
                      tabsetPanel(),
                      
                      tags$section(id="Home",
                                   fluidPage(
                                     column(12,
                                            div(id="intro-head",
                                                h1("The Commonwealth Games Visualisation"),
                                                p(style="text-shadow: -1px -1px 0 white, 1px -1px 0 white, -1px 1px 0 white, 1px 1px 0 white;",strong("An in-depth analysis using interactive visualisations to find the effects of GDP and Population on a Country's performance.")),
                                            ),
                                            div(class = "overview",
                                                h3("About the Applicaion"),
                                                p("This app offers a captivating visual journey into the history, achievements, and fascinating insights of the Commonwealth Games, the renowned international sporting event that brings together nations from across the Commonwealth."), 
                                                p("The project has the following panels:"),
                                                tags$ol(
                                                  tags$li(strong("Growth of the Games"), "- shows how the games has grown in numbers since its inception"),
                                                  tags$li(strong("Results: Medal Tally"), "- shows the medal results for a selected year or for all years"),
                                                  tags$li(strong("Map View"), "- shows the participant countries and medal winnings on a map"),
                                                  tags$li(strong("Country Profiles"), "- shows details about a participating country, including medals won and in which sports"),
                                                  tags$li(strong("Population and GDP"), "- shows details about the effects of population and gdp on medals won by country"),
                                                  tags$li(strong("Stadiums"), "- shows details about the number of stadiums in the participating country and it's effect on the performance"),
                                                ),
                                            ),
                                     ),
                                     div(class = "footer-content"),
                                     div(
                                     )
                                   ),
                      ),
                      
                      tags$hr(style="margin-top: 10px;
                         margin-bottom: 80px;"),
                      
                      tags$section(id="Growth of Games",
                                   fluidPage(
                                     tags$h2(class = "growth-heading" ,"Growth throughout the years"),
                                     column(class = "grwoth-line-legend" ,12,
                                            wellPanel(
                                              radioButtons("growthProp", label="Select category to view: ",
                                                           choices=c('Number of Participating Countries', 'Number of Sports Played', 'Number of Events Contested', 'Number of Participating Athletes'), 
                                                           selected='Number of Participating Countries')
                                            ),
                                            tags$div(class = "plot-style" , echarts4rOutput("growthPlot")),
                                     ),
                                     
                                     div(class="overview",style="padding-top:0px;margin-bottom:80px;padding-bottom=100px;",

                                         tags$h6(style="text-align:justify","The expansion and development of the Commonwealth Games are crucial to comprehending its impact over time. The growing number of participating nations reflects the rising cooperation between Commonwealth nations and provides opportunities for smaller or less developed nations to compete on the international sporting stage. The introduction of new sports reflects global trends, promotes cultural exchange, and facilitates the development of skills. The total number of athletes demonstrates the magnitude of the event and demonstrates the expansion of athletic talent within the Commonwealth, highlighting the significance of the Commonwealth Games as a platform for athletes to pursue their ambitions and inspire their communities.")
                                         
                                         )
                                   )
                      ),
                      
                      tags$hr(style="margin-top: 80px;
                         margin-bottom: 80px;"),
                      
                      tags$section(id="Results: Medal Tally",
                                   fluidPage(
                                     tags$h1(class = "growth-heading" ,"Medal Tally throughout the years"),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;","This section examines the competitive performance of Commonwealth Games participants throughout the games' history. This interactive visualisation allows you to select specific years or regions to examine the performance of different countries in various editions of the games."),
                                     
                                     fluidRow(
                                       tags$div(style = "width: 50%" ,
                                                wellPanel( style = "width: 50% !important; margin: 0 10px auto auto; background-color: bisque;",
                                                           selectInput("resultsYearInput", "Select Year", "All")
                                                )
                                       ),
                                       tags$div(style = "width: 50%" ,
                                                wellPanel(style = "width: 50% !important; margin: 0 0 auto 10px; background-color: bisque;",
                                                          selectInput("resultsRegionInput", "Select Region", "All")
                                                )
                                                
                                       ),
                                     ),
                                     tags$div(class = "table-class",
                                              htmlOutput("resultsTally")),
                                     tags$h6(style="width:75%;margin-top:60px;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "The Top 3 overall position is occupied by the most developed countries."),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "South Africa has always been the best African Nation."),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "Canada outperforms all the American nations by a hefty margin."),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "The most populous country in the world India is the best performing nation from Asia "),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "Except Jamaica. none of the nation from the Caribbean has made a significant impact on the games "),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "European nations as a whole have been brilliant in the competetion."),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "Australia, the best performing nation in the games is the torch bearer of Oceania")
                                     
                                     )
                      ),
                      
                      tags$hr(style="margin-top: 80px;
                         margin-bottom: 80px;"),
                      
                      tags$section(id="Map View",
                                   fluidPage(
                                     htmlOutput("mapViewTitle"),
                                     
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "Use the interactive map to investigate the distribution of success across participating nations and gain insights into their notable performances.
                                             The Participant Countries view displays all the nations that have taken part in the Commonwealth Games over the years.
                                             The Medals view showcases which nations have won gold, silver, and bronze medals.
                                             Hover on the pin to view the country name."),
                                     div(id="leaflet-map-container",
                                         tags$div(class = "map-radio",
                                                  wellPanel(class = "map-well",
                                                            selectInput("mapYearInput", "Select Year", "All"),
                                                            selectInput("mapRegionInput", "Select Region", "All")
                                                  ),
                                                  wellPanel(class = "map-well",
                                                            radioButtons("mapViewInput", "Select Map View",
                                                                         choices=c('Participant Countries',
                                                                                   'Countries that have won Gold Medals', 
                                                                                   'Countries that have won Silver Medals', 
                                                                                   'Countries that have won Bronze Medals'),
                                                                         selected='Participant Countries'
                                                            )
                                                  )
                                         ),
                                         leafletOutput("mapLeaflet")
                                     ),
                                     htmlOutput("mapViewCaption")
                                   )
                      ),
                      
                      tags$hr(style="margin-top: 80px;
                         margin-bottom: 80px;"),
                      
                      tags$section(id="Country Profile",
                                   fluidPage(
                                     div(class = "growth-heading", "Growth of Countries throughout the Years"),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "Examine the performance of each country in the Commonwealth Games in detail. 
                                             Here, you can select specific regions and countries to learn more about their intriguing participation 
                                             history and remarkable medal achievements."),
                                     tags$div(class = "countrypage",
                                              wellPanel( style = "width: 25% !important" ,
                                                         selectInput("countryRegionInput", "Select Region", "All"),
                                                         selectInput("countryNameInput", "Select Country", "All")
                                              ),
                                              plotOutput("countryYearResults"),
                                     ),
                                     tags$div(
                                       fluidRow(
                                         column(width=12,class = "countrydetails",
                                                htmlOutput("countryProfile"))
                                        
                                       )
                                     ),
                                     
                                   )
                      ),
                      
                      tags$hr(style="margin-top: 80px;
                         margin-bottom: 80px;"),
                      
                      
                      
                      tags$section(id="Medals and Population",
                                   fluidPage(
                                     div(style="width:55%",class = "growth-heading", "Population and GDP versus the performance of a country"),
                                     titlePanel(""),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "This section explores the intriguing relationship between a nation's population, GDP, and performance in the Commonwealth Games for the top 10 countries. As we investigate the effects of these variables, we obtain valuable insights into the dynamics that shape the international sporting success of a nation. First let's look at the 10 countries chosen for analysis.
                                        "),
                                    
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "All the three plots are linked with each other. You can hover on one to get the insights about all three plots."),
                                     sidebarLayout(
                                       
                                       sidebarPanel( width = 12,
                                         selectInput("country", "Select a country:", choices = c("Australia","Canada","England","India","Kenya","New Zealand","Nigeria","Scotland","South Africa","Wales"))
                                       ),
                                       fluidRow(
                                        
                                         column(width=12,leafletOutput("map2",width="1200px"),style="margin-left:120px;"),
                                         
                                         column(width = 6,
                                                echarts4rOutput("plot1")
                                         ),
                                         column(width = 6,
                                                echarts4rOutput("plot2")
                                         ),
                                         column(width = 12,style="margin-left=25%",
                                                echarts4rOutput("plot3")
                                         )
                                       )
                                     ),
                                     tags$ul(style="width:70%;margin-left:15%;margin-top:60px;",
                                       tags$li("Population and GDP can play significant roles in a country's sporting success. Larger populations may have a larger talent pool to draw from, while higher GDP can potentially provide better training facilities and resources for athletes."),
                                       tags$li("Countries with large populations like India and Nigeria have shown impressive growth in medal tallies over the years, demonstrating the impact of population size on sporting achievements."),
                                       tags$li("We can also see the impact of host nation. India scored the highest medals in 2010 when the games were held in it's capital state Delhi. The similar effect can also be observed for Australia in 2006, Scotland in 2014 and Canada in 1994."),
                                       tags$li("We observe that countries like Australia and Canada, despite having smaller populations, have consistently performed exceptionally well, showcasing the influence of other factors like sports culture, investment, and development programs."),
                                       tags$li("GDP can also influence a country's performance. Nations with higher GDP, such as Australia and Canada, have historically been among the top medal-winning countries, underlining the significance of financial investments in sports."),
                                       tags$li("However, outliers exist, with some countries surpassing expectations despite challenges related to population or GDP, showcasing the determination and passion of their athletes."),
                                       tags$li("It is essential to recognize that while population and GDP can offer advantages, the spirit of sportsmanship, dedication, and strategic planning are equally vital in achieving success in the Commonwealth Games.")
                                     ),
                                     tags$h6(style="font-style:italic;font-size:10px;width:75%;padding-left:23%;margin-right:5%;text-align:justify;",
                                             "*The empty bars in gdp and population plots indicate that the country did not take part in 
                                             the games during that year."),
                                     tags$h6(style="font-style:italic;font-size:10px;width:75%;padding-left:23%;margin-right:5%;text-align:justify;",
                                             "**The reason for choosing top 10 countries is that the accurate data for these countries can be found on the web."),
                                     tags$h6(style="font-style:italic;font-size:10px;width:75%;padding-left:23%;margin-right:5%;text-align:justify;",
                                             "***The gdp data was available after 1962.")
                                   )
                      ),
                      
                      tags$hr(style="margin-top: 80px;
                         margin-bottom: 80px;"),
                      
                      tags$section(id="Map_View2",
                                   fluidPage(
                                     div(style="width:55%",class = "growth-heading", "Sports facilities versus Medals"),
                                     tags$h6(style="width:75%;padding-left:23%;margin-right:5%;text-align:justify;margin-bottom:40px;",
                                             "We will investigate the intriguing correlation between a country's number of stadiums and its performance at the Commonwealth Games. 
                                             Hover on the map and the barplot to get the data regarding the medals and stadiums."),
                                     titlePanel(""),
                                     
                                   fluidRow(
                                     
                                     column(width=12,plotlyOutput("barPlot",width="800px",height="400px"),style="width:70%;margin-top:120px;margin-left:350px;margin-bottom:100px") 
                                   ),
                                   tags$ul(style="width:70%;margin-left:15%;",
                                           tags$li("Although having 179 sports facilities, India still is not able to make it to the top 3 nations. The reason being that the number of stadiums are relatively less if we look at India's growing population."),
                                           tags$li("Although New Zealand may have a lesser number of stadiums than some other nations, the quality of these facilities may be exceptional. Having well-maintained stadiums can provide an ideal environment for training and competitions for athletes."),
                                           ),
                                   tags$h6(style="font-style:italic;font-size:10px;width:75%;padding-left:23%;margin-right:5%;text-align:justify;",
                                           "*The stadiums include only those facilities that have coaches and accommodation facilites.")
                                   ),div(class = "footer-content","DATA5002 PROJECT",style="margin-top:100px;"),
                                   div(class = "footer",
                                       img(src = "cwlogo.png", alt = "commonwealth logo", width = "200"),
                                       img(src = "cwflogo.png", alt = "commonwealth federation logo", width = '180'),
                                   )
                      
                      ),
                      
                   
                      
                      tags$head(
                        tags$script(src = "scripts.js"),
                        tags$link(rel="stylesheet", type="text/css", href="styles.css"),
                        tags$link(rel="stylesheet" ,href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta2/css/all.min.css",integrity="sha512-YWzhKL2whUzgiheMoBFwW8CKV4qpHQAEuvilg9FAn5VJUDwKZZxkJNuGM4XkWuk94WCrrwslk8yWNGmY1EduTA==",crossorigin="anonymous",referrerpolicy="no-referrer"),
                      )
           ))
)

growth.df <- read.csv(file="data/growth_of_the_games.csv",
                      head=TRUE, fileEncoding='UTF-8-BOM')

results_df <- read.csv(file="data/results_all_per_year_with_code.csv",
                       head=TRUE, fileEncoding='UTF-8-BOM', stringsAsFactors=FALSE)



df_population <- read.csv(file="data/population over ages.csv",
                            head=TRUE, fileEncoding='UTF-8-BOM')

# years
uniqueYears <- as.list(sort(unique(results_df$year), decreasing=T)) # decreasing



# regions/continents
uniqueRegions <- as.list(sort(unique(results_df$continent)))
uniqueRegions <- c('All', uniqueRegions)

# changed to local subdir under www
flagBaseUrl <- 'flags/'


server <- function(input,output,session){
  
  
  ###################################
  # Growth data
  ###################################
  
  growth.misc.data <- reactive({
    if (input$growthProp=='Number of Participating Countries') {
      y <- growth.df$participating.countries
      y.name <- 'participating.countries'
      subtitle <- 'Number of Participating Countries'
      color <- 'red'
      styles <- list(countries=paste0('color:', color, '; font-style:italic'))
    } else if (input$growthProp=='Number of Sports Played') {
      y <- growth.df$sports
      y.name <- 'sports'
      subtitle <- 'Number of Sports Played'
      color <- 'blue'
      styles <- list(sports=paste0('color:', color, '; font-style:italic'))
    } else if (input$growthProp=='Number of Events Contested') {
      y <- growth.df$events
      y.name <- 'events'
      subtitle <- 'Number of Events Contested'
      color <- 'green'
      styles <- list(events=paste0('color:', color, '; font-style:italic'))
    } else {
      y <- growth.df$athletes
      y.name <- 'athletes'
      subtitle <- 'Number of Participating Athletes'
      color <- 'orange'
      styles <- list(athletes=paste0('color:', color, '; font-style:italic'))
    }
    
    # save to a list
    list(y=y, y.name=y.name, subtitle=subtitle, color=color, styles=styles)
  })
  
  output$growthPlot <- renderEcharts4r({ 
    growth.df$year = as.character(growth.df$year)
    if(growth.misc.data()[['y.name']] == 'participating.countries'){
      growth.df |>
        e_chart( x = year ) |>
        e_line(serie = participating.countries , name = "Number of Countries Participating"  ) |>
        e_tooltip(trigger = 'axis') |>
        e_datazoom(x_index = c(0, 1))
    } else if (growth.misc.data()[['y.name']] == 'sports'){
      growth.df |>
        e_chart( x = year ) |>
        e_line(serie = sports , name = "Number of Sports Played"  ) |>
        e_tooltip(trigger = 'axis') |>
        e_datazoom(x_index = c(0, 1))
    } else if (growth.misc.data()[['y.name']] == 'events'){
      growth.df |>
        e_chart( x = year ) |>
        e_line(serie = events , name = "Number of Events Contested"  ) |>
        e_tooltip(trigger = 'axis') |>
        e_datazoom(x_index = c(0, 1))
    } else {
      growth.df |>
        e_chart( x = year) |>
        e_line(serie = athletes , name = "Number of Participating Athletes"  ) |>
        e_tooltip(trigger = 'axis') |>
        e_datazoom(x_index = c(0, 1))
    }
  })
  
  ###################################
  # Results Tally
  ###################################
  
  # update input dropdowns
  updateSelectInput(session, "resultsYearInput", choices=uniqueYears)
  updateSelectInput(session, "resultsRegionInput", choices=uniqueRegions)
  
  results.data <- reactive({
    data <- results_df
    # filters
    data <- data[data$year == input$resultsYearInput, ] 
    if (input$resultsRegionInput != 'All') {
      data <- data[data$continent == input$resultsRegionInput, ]
    }
    # sort: gold, silver, bronze
    data <- data[with(data, order(-gold, -silver, -bronze)), ]
    data
  })
  
  output$resultsTally <- renderUI({
    if (nrow(results.data()) != 0) {
      
      resultsTable <- tags$table( style = "width:100%",
                                  tags$tr(class="results-header",
                                          tags$th(class="results-rank", "Rank"), 
                                          tags$th(class="results-flag", "Flag"),
                                          tags$th(class="results-country", "Country"),
                                          tags$th(class="results-gold", "Gold"),
                                          tags$th(class="results-silver", "Silver"),
                                          tags$th(class="results-bronze", "Bronze"),
                                          tags$th(class="results-total", "Total")
                                  )
      )
      
      for(i in 1:nrow(results.data())) {
        row <- results.data()[i,]
        resultsTable <- tagAppendChild(resultsTable,
                                       tags$tr(class="results-row",
                                               tags$td(class="results-rank", i),
                                               tags$td(class="results-flag", 
                                                       img(src=paste0(flagBaseUrl, row[2], '.png'),
                                                           alt=paste0(row[3], ' flag'),
                                                           width=80)),
                                               tags$td(class="results-country", row[3]),
                                               tags$td(class="results-gold", row[7]),
                                               tags$td(class="results-silver", row[8]),
                                               tags$td(class="results-bronze", row[9]),
                                               tags$td(class="results-total", row[11])
                                       )
        )        
      }      
      
      if (input$resultsYearInput == 'All') {
        medalTallyTitle <- "Medal Tally (All years)"
      } else {
        medalTallyTitle <- paste0("Medal Tally in ", input$resultsYearInput) 
      }
      
      div(
        h2(id="medal-tally-title", medalTallyTitle),
        div(id="results-tally", style = "width: 100%", resultsTable)
      )
    }
  })  
  ###################################
  # Results Map
  ###################################  
  
  # update input dropdowns
  updateSelectInput(session, "mapYearInput", choices=uniqueYears)
  updateSelectInput(session, "mapRegionInput", choices=uniqueRegions)
  
  map.data <- reactive({
    data <- results_df
    # filters
    data <- data[data$year == input$mapYearInput, ] 
    if (input$mapRegionInput != 'All') {
      data <- data[data$continent == input$mapRegionInput, ]
    }
    data
  })
  
  map.medals.data <- reactive({
    data <- map.data()
    if (nrow(data) > 0) {
      if (grepl('Gold', input$mapViewInput)) {
        data$popup <- paste0(data$gold, ' medals')
        data <- data[data$gold > 0, ]
      } else if (grepl('Silver', input$mapViewInput)) {
        data$popup <- paste0(data$silver, ' medals')
        data <- data[data$silver > 0, ]
      } else {
        data$popup <- paste0(data$bronze, ' medals')
        data <- data[data$bronze > 0, ]
      }
    }
    data
  })
  
  output$mapLeaflet <- renderLeaflet({
    leaflet.map <- leaflet(data=map.data()) %>%
      addTiles()
    
    if (nrow(map.data()) > 0) {
      
      if (input$mapViewInput=='Participant Countries') {
        customIcons <- awesomeIcons(
          icon='flag',
          iconColor='black',
          library='glyphicon'
          #markerColor=getRegionColor(map.data())
        )
        leaflet.map <- leaflet.map %>%
          addAwesomeMarkers(~long, ~lat, label=~country, icon=customIcons)
        
        leaflet.map <- leaflet.map
        
      } else {
        m.data <- map.medals.data()
        
        if (nrow(m.data) > 0) {
          leaflet.map <- leaflet(data=m.data) %>%
            addTiles()
          
          if (grepl('Gold', input$mapViewInput)) {
            icon.url <- 'gold-medal-64.png'
          } else if (grepl('Silver', input$mapViewInput)) {
            icon.url <- 'silver-medal-64.png'
          } else {
            icon.url <- 'bronze-medal-64.png'
          }
          
          # set icon size based on value
          getIconSize <- function(m.data) {
            sapply(m.data$total, function(total) {
              if(total <= 10) {
                16
              } else if(total <= 50) {
                24
              } else if (total <= 100) {
                32
              } else {
                48
              }
            })
          }        
          
          medalIcons <- icons(iconUrl=icon.url, iconWidth=getIconSize(m.data), iconHeight=getIconSize(m.data))
          
          leaflet.map <- leaflet.map %>%
            addMarkers(~long, ~lat, label=~country, 
                       icon=medalIcons, 
                       popup=~popup)
        }
      }
      
    }
    
    leaflet.map
  })
  
  output$mapViewTitle <- renderUI({
    if (input$mapYearInput == 'All') {
      leafletSubtitle <- "(All years)"
    } else {
      hostData <- growth.df[growth.df$year==input$mapYearInput, ]
      leafletSubtitle <- paste0(input$mapYearInput, " Games in ", hostData[1, "city"], ", ", hostData[1, "country"])
    }
    
    div(class = "map-title",
        h1(id="leaflet-map-title", input$mapViewInput),
        h2(id="leaflet-map-subtitle", leafletSubtitle)
    )
  })  
  
  output$mapViewCaption <- renderUI({
    if (nrow(map.data()) == 0) {
      p("No data available for the selected filters")
    } else {
      if (grepl('Medals', input$mapViewInput)) {
        p(class="plot-note", "Click on a medal to find out how many were won")
      }
    }
  })
  ###################################
  # Country Details data
  ###################################  
  
  updateSelectInput(session, "countryRegionInput", choices=uniqueRegions)
  
  region.countries <- reactive({
    if (input$countryRegionInput == 'All') {
      as.list(sort(unique(results_df$country)))
    } else {
      as.list(sort(unique(results_df[results_df$continent==input$countryRegionInput, ]$country)))
    }
  })
  
  observe({
    defaultCountry = NULL
    if (input$countryRegionInput == 'All') {
      defaultCountry='Australia' # default
    }
    updateSelectInput(session, "countryNameInput", choices=region.countries(), selected=defaultCountry)
  })
  
  country.data <- reactive({
    results_df[results_df$country==input$countryNameInput, ]
  })
  
  country.misc.data <- reactive({
    country.df <- country.data()
    country.code <- country.df[1, ]$country.code
    continent <- country.df[1, ]$continent
    years.joined <- nrow(country.df) - 1 # deduct 'All' entry
    first.join.year <- min(country.df$year)
    total.gold <- country.df[country.df$year=='All', ]$gold
    total.silver <- country.df[country.df$year=='All', ]$silver
    total.bronze <- country.df[country.df$year=='All', ]$bronze
    total.medals <- country.df[country.df$year=='All', ]$total
    
    list(country.code=country.code,
         continent=continent,
         years.joined=years.joined,
         first.join.year=first.join.year,
         total.gold=total.gold,
         total.silver=total.silver,
         total.bronze=total.bronze,
         total.medals=total.medals)
  })
  
  output$countryProfile <- renderUI({
    div(id="country-profile-details",
        img(id="country-profile-flag", 
            src=paste0(flagBaseUrl, country.misc.data()[['country.code']], '.png'),
            alt=paste0(input$countryNameInput, ' flag'),
            width=200, style = "margin: 0 auto;"),
        div(class = "country-table",div(id="country-profile-name", input$countryNameInput),
            tags$table(id="country-profile-table", border="1",
                       tags$tr(tags$td(class="country-profile-label", "Region"), tags$td(class="country-profile-value", country.misc.data()[['continent']])),
                       tags$tr(tags$td(class="country-profile-label", "# Years joined"), tags$td(class="country-profile-value", country.misc.data()[['years.joined']])),
                       tags$tr(tags$td(class="country-profile-label", "Year first joined"), tags$td(class="country-profile-value", country.misc.data()[['first.join.year']])),
                       tags$tr(tags$td(class="country-profile-label", "Gold medals"), tags$td(class="country-profile-value", country.misc.data()[['total.gold']])),
                       tags$tr(tags$td(class="country-profile-label", "Silver medals"), tags$td(class="country-profile-value", country.misc.data()[['total.silver']])),        
                       tags$tr(tags$td(class="country-profile-label", "Bronze medals"), tags$td(class="country-profile-value", country.misc.data()[['total.bronze']])),        
                       tags$tr(tags$td(class="country-profile-label", "Total medals"), tags$td(class="country-profile-value", country.misc.data()[['total.medals']]))        
            )  
        ))
  })
  
  output$countryYearResults <- renderPlot({
    year.df <- country.data()
    year.df <- year.df[year.df$year!='All', ]
    year.df <- year.df[, c(1, 7, 8, 9)]
    #cat(file=stderr(), str(year.df))
    year.df <- melt(year.df, id='year')
    names(year.df) <- c('year', 'medal.class', 'medal.count')
    #cat(file=stderr(), str(year.df))
    my_color <- c("#1B9E77", "#D95F02" ,"#7570B3")
    
    ggplot(year.df, aes(x=year, y=medal.count, fill=medal.class)) + 
      geom_bar(stat='identity', color='black') +
      labs(x='Year', y='Medals won',
           title=paste0('Medals won by ', input$countryNameInput, ' by Year')) +
      theme(plot.title=element_text(size=22, hjust=0.5, face="bold")) +
      scale_fill_manual("legend", values=c('gold'='#FFD700', 'silver'='#c0c0c0', 'bronze'='#cd7f32')) +
      scale_y_continuous(labels=function(x) { floor(x) })
  })
  

  
  # POPULATION AND MEDALS #
  selected_data <- reactive({
    population_medals %>% 
      filter(country == input$country)  
  })
  
  selected_data_2 <- reactive({
    gdp_medals %>% 
      filter(country == input$country)  
  })
  
  # Create the interactive echarts4r plot
  output$plot1 <- renderEcharts4r({
    
    data <- selected_data()
    
    plot1 <- data %>%
      e_chart(x = year) %>%
      e_bar(serie = Population, name = "Population in Million", y_axis_index = 0) %>%
      e_tooltip(trigger = 'axis') %>%
      e_x_axis(type = "value", min = 1946, max = 2022, interval=4) %>%
      e_y_axis(y_axis_index = 0) %>%
      e_datazoom(x_index = c(0, 1)) %>% 
      e_group("grp")
    plot1
  })
  
  output$plot2 <- renderEcharts4r({
    
    data2 <- selected_data_2()
    
    plot2 <- data2 %>%
      e_chart(x = year) %>%
      e_bar(serie = gdp_million_usd, name = "GDP (Million USD)", y_axis_index = 0) %>%
      e_tooltip(trigger = 'axis') %>%
      e_x_axis(type = "value", min = 1946, max = 2022, interval=4) %>%
      e_y_axis(y_axis_index = 0) %>%
      e_datazoom(x_index = c(0, 1)) %>% 
      e_group("grp")
    plot2
  })
  
  output$plot3 <- renderEcharts4r({
    
    data <- selected_data()
    
    plot3 <- data %>%
      e_chart(x = year) %>%
      e_line(serie = total, name = "Total Medals", y_axis_index = 1) %>%
      e_tooltip(trigger = 'axis') %>%
      e_x_axis(type = "value", min = 1946, max = 2022, interval=4) %>%
      e_y_axis(y_axis_index = 1) %>%
      e_datazoom(x_index = c(0, 1)) %>% 
      e_group("grp") %>%   # assign group
      e_connect_group("grp")
    plot3
  })
  
  output$map2 <- renderLeaflet({
    leaflet(stadiums) %>%
      addTiles() %>%
      addMarkers(
        lat = ~lat,
        lng = ~long,
        label = ~paste0("Country:", country),
        options = markerOptions(tooltipAnchor = c(0, 0))
      )
  })
  
 
  
  output$barPlot <- renderPlotly({
    colors <- color_palette[as.numeric(factor(stadiums$country))]
    p <- plot_ly(stadiums, x = ~stadiums, y = ~total, text = ~country, hoverinfo = "text",
                 type = "scatter", marker = list(color = colors),width=0.5,showlegend=TRUE)
    
    p <- p %>% add_trace(type = "scatter",
                         mode = "markers",
                         marker = list(color = colors, size = 10),
                         hoverinfo = "text",
                         text = ~country,
                         name = ~country
                         )  # Set the country names for the legend
    
    p <- p %>% layout(xaxis = list(title = "Number of Stadiums",
                                   linecolor = "black",  # Set axis line color to black
                                   linewidth = 2,        # Set axis line width to 2
                                   showgrid = TRUE,     # Show grid lines
                                   gridcolor = "lightgray",  # Set grid line color
                                   gridwidth = 1,
                                   dtick=25,
                                   range=c(0,200)),      # Set grid line width
                      yaxis = list(title = "Total Medals Won",
                                   linecolor = "black",  # Set axis line color to black
                                   linewidth = 1,        # Set axis line width to 2
                                   showgrid = TRUE,     # Show grid lines
                                   gridcolor = "lightgray",  # Set grid line color
                                   gridwidth = 1),      # Set grid line width
                      title = "Medals Won by Country and Number of Stadiums",
                      showlegend = TRUE,
                      colorway = colors,
                      hoverlabel = list(bgcolor = "white", font = list(size = 12)),
                      hovermode = "closest",
                      plot_bgcolor = "#f4f4f4",
                      paper_bgcolor = "#f4f4f4",
                      font = list(family = "Arial, sans-serif"),
                      margin = list(l = 60, r = 30, t = 80, b = 60)
                      
    )
    
    p
  })
 
  
}

shinyApp(ui,server)


