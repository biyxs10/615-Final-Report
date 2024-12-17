library(shiny)
library(shinydashboard)
library(leaflet)
library(WDI)
library(ggplot2)
library(dplyr)
library(shiny)
library(rmarkdown)
library(scales)
library(WDI)

# 1. Palau Data Preparation
# 1. indicate country and data
indicators <- c(
  "SP.POP.TOTL",       # Population
  "NY.GDP.MKTP.CD",    # GDP ($)
  "ST.INT.ARVL"        # International Tourist Arrivals
)
# Palau ID
country <- "PLW"

palau_data <- WDI(country = country, indicator = indicators, start = 2010, end = 2023)

# rename column
colnames(palau_data) <- c("country","iso2c","iso3c","year","Population","GDP","Visitors")

# transfer gdp into million
palau_data$GDP <- palau_data$GDP / 1e6


# 2. Palau weather data
#Monthly Precipitation, 2012 - 2021
rain<-read.csv("Precipitation_Palau.csv")

#Monthly Average Temperatures, 2012 - 2021  (Degrees Fahrenheit)
temp<-read.csv("temperature.csv")

# 3. Fiji Data
library(dplyr)
library(tidyr)
# Fiji Data
fiji_data <- WDI(
  country = "FJ",
  indicator = c(
    "NY.GDP.MKTP.CD",  # GDP
    "SP.POP.TOTL",     # Population
    "ST.INT.ARVL"      # Visitors
  ),
  start = 2012,
  end = 2023
)


#rename column names
colnames(fiji_data) <- c("country","iso2c","iso3c","year","GDP","Population","Visitors")

# transfer gdp into million
fiji_data$GDP <- fiji_data$GDP / 1e6

# merge
compare_data <- bind_rows(palau_data, fiji_data)



# !Define UI for application
ui <- dashboardPage(
  dashboardHeader(
    title = "Palau",
    tags$li(
      class = "dropdown",
      style = "position: absolute; right: 10px; top: 10px;",
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/commons/4/48/Flag_of_Palau.svg",
        height = "30px",
        alt = "Palau Flag"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General Description", tabName = "general", icon = icon("info-circle")),
      menuItem("Key Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Comparative Analysis", tabName = "comparative", icon = icon("chart-bar")),
      menuItem("SWOT Analysis", tabName = "swot", icon = icon("clipboard")),
      menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  
#BODY 
  dashboardBody(
    tags$style(HTML("
    .main-sidebar {
        background-color: #000000 !important; /* Dark yellow */
      }
    /* Header background color */
      .main-header .navbar {
        background-color: #2471A3 !important; /* Custom color for header */
      }
      .main-header .logo {
        background-color: #005A90 !important; /* Darker color for the logo section */
      }
      /* Modify text color in the header */
      .main-header .logo,
      .main-header .navbar .navbar-custom-menu > .navbar-nav > li > a {
        color: #E4EFFF  !important; /* Set text color to gold */
      }")),
    tabItems(
#General
      tabItem(
        tabName = "general",
        h2("General Description"),
        tabsetPanel(
          tabPanel(
            "Map of Palau",
            leafletOutput("palauMap", height = 500)
          ),
          tabPanel(
            "Global Location",
            leafletOutput("globalMap", height = 500)
          ),
          tabPanel(
            "Key Facts",
            h3("Key Facts"),
            selectInput(
              inputId = "keyFactsSelect",
              label = "Select a category:",
              choices = c("Government", "Economy","Nature environment", "Culture", "Climate"),
              selected = "Geography"
            ),
            uiOutput("keyFactsContent")
          ),
          tabPanel(
            "Narrative Description",
            h3("Narrative Description"),
            p("Palau is located in the Micronesia region of the Western Pacific Ocean. It is an archipelago of about 340 islands and reefs located in the northwestern part of the Philippine Sea. Palau's largest island is Babeldaob, where the country's capital, Engelmad, is located. The entire country covers an area of approximately 459 square kilometers, including several volcanic and coral reef islands, and the terrain is dominated by tropical islands and oceanic landscapes. Due to its unique location, Palau has a rich marine ecosystem and is known as one of the world's last natural paradises.")
          )
        )
      ),

#Demographics
    tabItem(
      tabName = "demographics",
      fluidRow(
        # GDP CARD
        box(
          title = strong("GDP (in million_$)"), 
          status = "danger", # color
          solidHeader = TRUE,
          width = 6,
          plotOutput("gdpPlot", height = "400px")
        ),
        # Population CARD
        box(
          title = strong("Population"), 
          status = "success", 
          solidHeader = TRUE,
          width = 6,
          plotOutput("populationPlot", height = "300px")
        )
      ),
      fluidRow(
        # Visitors CARD
        box(
          title = strong("International Visitors"), 
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          plotOutput("visitorsPlot", height = "300px")
        ),
        # Weather CARD
        box(
          title = strong("Weather Data"), 
          status = "info",
          solidHeader = TRUE,
          width = 6,
          selectInput(
            "weatherMetric", 
            "Select Metric:",
            choices = c("Monthly Precipitation" = "precip", 
                        "Average Temperatures" = "temp")
          ),
          plotOutput("weatherPlot", height = "300px")
        )
      )
    ),
  
#Comparative
    tabItem(
      tabName = "comparative",
      h2("Comparative Analysis"),
      div(
        style = "margin-bottom: 20px; background-color: #f9f9f9; padding: 15px; border-left: 5px solid #0073e6;",
        h4(strong("Why Fiji as a Comparison?")),
        p("Fiji is selected for comparison due to its geographic proximity to Palau, similar reliance on tourism, 
       and shared environmental and economic challenges. Both countries are known for their marine biodiversity, 
       making them exemplary cases to explore regional dynamics and sustainability efforts in the Pacific.")
      ),
      fluidRow(
        # Fiji Map Card
        box(
          title = "Map of Fiji", 
          status = "primary", 
          solidHeader = TRUE, 
          width = 6,
          leafletOutput("fijiMap", height = 300)
        ),
        # Fiji Global Location Card 
        box(
          title = "Global Location of Fiji",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          leafletOutput("fijiGlobalMap", height = 300)
        ),
        
        box(
          title = "Island Comparison",
          solidHeader = TRUE,
          status = "warning",
          width = 12,
          selectInput(
            inputId = "comparisonMetric",
            label = "Select a metric to compare:",
            choices = c("GDP", "Population", "Visitors")
          ),
          uiOutput("compareCard")
        )
      )
    ),
#SWOT
  tabItem(
    tabName = "swot",
    h2("SWOT Analysis"),
    
    fluidRow(
      # Strengths Card
      box(
        title = "Strengths", status = "success", solidHeader = TRUE, width = 6,
        h2("Unique natural environment"),
        p("Palau has a unique and pristine natural environment. It is home to rare and endemic species that cannot be found anywhere else. The lush forests, untouched marine biodiversity, and world-class diving sites make it a paradise for nature lovers."),
        h2("Conservation pioneers"),
        p(" Ol'au Palau project (started from 2022): emphasize responsible tourism behavior and attract environmentally friendly tourists.")
      ),
  
      
      # Weaknesses Card
      box(
        title = "Weaknesses", status = "danger", solidHeader = TRUE, width = 6,
        h2("Land and resource constraints"),
        p("small in size, lacks exploitable resources, and receives fewer tourists than other popular islands such as Fiji."),
        h2("Economy highly dependent on tourism"),
        p("Other industries such as agriculture and fishing are small, limiting the potential for economic diversification.")
      )
    ),
    
    fluidRow(
      # Opportunities Card
      box(
        title = "Opportunities", status = "info", solidHeader = TRUE, width = 6,
        h2("Special-interest tourism market"),
        p("Pristine natural environments and location can attract tourists who are ecologically friendly and want to be away from crowd."),
        h2("Unique culture"),
        p(" Palau is rich in traditional culture"),
        h2("International cooperation and investment"),
        p("working with international environmental organizations and national governments to promote Marine conservation and sustainable development. Seek outside investment to build infrastructure and tourism facilities.")
      ),
      
      # Threats Card
      box(
        title = "Threats", status = "warning", solidHeader = TRUE, width = 6,
        h2("Climate change"),
        p("Palau‘s special Unique geographical features makes it vulnerable to weather hazards caused by climate change, such as extreme high tides, coastal erosion and seal-level rise."),
        h2("Global economic fluctuations"),
        p("As Palau is a highly tourism-dependent economy, a global economic downturn or crisis can directly affect visitor numbers and revenues. The impact of the global pandemic on Palau's tourism and GDP in 2019-2021 is one example.")
      )
    )
  ),

#Refer
      tabItem(
        tabName = "references",
        h2("References"),
        p("About Palau - Office of the Special Prosecutor | Republic of Palau. (2019, June 3). Office of the Special Prosecutor | Republic of Palau. https://www.palauosp.org/about-palau/"),
        p("Climate Statistics – PalauGov.pw. (n.d.). PalauGov.pw. https://www.palaugov.pw/executive-branch/ministries/finance/budgetandplanning/climate-statistics/"),
        p("Home - Pristine. (n.d.). https://pristineparadisepalau.com/"),
        p("Palau | Culture, History, & People. (2019). In Encyclopædia Britannica. https://www.britannica.com/place/Palau"),
        p("Wikipedia Contributors. (2024, November 21). Palau. Wikipedia; Wikimedia Foundation. https://en.wikipedia.org/wiki/Palau"),
        p("World Bank Climate Change Knowledge Portal. (n.d.). Climateknowledgeportal.worldbank.org. https://climateknowledgeportal.worldbank.org/country/palau/vulnerability"),
        p("World Bank. (2024). World Development Indicators. R package WDI. Retrieved from https://data.worldbank.org")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
 #MAP
  output$palauMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = 134.5825, lat = 7.3417, zoom = 9) %>% 
      addMarkers(lng = 134.5825, lat = 7.3417, popup = "Palau")
  })
  
  output$globalMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = 134.5825, lat = 7.3417, zoom = 2) %>% 
      addMarkers(lng = 134.5825, lat = 7.3417, popup = "Palau")
  })
  
 #Demo
  # GDP visualization
  output$gdpPlot <- renderPlot({
    ggplot(palau_data, aes(x = year, y = GDP)) +
      geom_line(color = "tan1", size = 1) +
      geom_point(color = "indianred1", size = 2) +
      scale_x_continuous(breaks = seq(min(palau_data$year), max(palau_data$year), by = 1)) +
      geom_text(aes(label = paste0(round(GDP, 1), "M")), vjust = -1.5, size = 3) +
      labs(title = "GDP (in millions_$)", x = "Year", y = "GDP (in millions_$)") +
      theme_minimal()
  })
  
  # Population visualization
  output$populationPlot <- renderPlot({
    ggplot(palau_data, aes(x = year, y = Population)) +
      geom_line(color = "aquamarine2", size = 1) +
      geom_point(color = "aquamarine4", size = 2) +
      geom_text(aes(label = Population), vjust = -0.5, color = "grey60", size = 3.5) +
      scale_x_continuous(breaks = seq(min(palau_data$year), max(palau_data$year), by = 1)) +
      labs(title = "Population (2013-2023)", x = "Year", y = "Population") +
      theme_minimal()
  })
  
  # Visitors visualization
  output$visitorsPlot <- renderPlot({
    ggplot(palau_data, aes(x = year, y = Visitors)) +
      geom_line(color = "purple", size = 1) +
      geom_point(color = "blueviolet", size = 2) +
      geom_text(aes(label = Visitors), vjust = -0.5, color = "grey60", size = 3.5) +
      scale_x_continuous(breaks = seq(min(palau_data$year), max(palau_data$year), by = 1)) +
      labs(title = "Number of International Visitors", x = "Year", y = "International Visitors") +
      theme_minimal()
  })
  
  # Weather visualization
  output$weatherPlot <- renderPlot({
    if (input$weatherMetric == "precip") { 
      ggplot(rain, aes(x = Year, y = Average)) +
        geom_bar(stat = "identity", fill = "cornflowerblue", color = "black", alpha = 0.7) +
        geom_text(aes(label = Average), vjust = -0.5, color = "grey60", size = 3.5) +
        scale_x_continuous(breaks = seq(min(rain$Year), max(rain$Year), by = 1)) +
        labs(title = "Average Precipitation", x = "Year", y = "Precipitation (inches)") +
        theme_minimal()

    } else { 
      ggplot(temp, aes(x = Year, y = Average)) +
        geom_bar(stat = "identity", fill = "orange", color = "black",alpha=0.7) +
        geom_text(aes(label = Average), vjust = -0.5, color = "grey60", size = 3.5) +
        scale_x_continuous(breaks = seq(min(temp$Year), max(temp$Year), by = 1)) +
        labs(title = "Average Temperature (°F)", x = "Year", y = "Temperature(°F)") +
        theme_minimal()
      }
  })
  
#key facts
  output$keyFactsContent <- renderUI({
    switch(
      input$keyFactsSelect,
      "Government"=tagList(
        p("Palau is a constitutional presidential republic with a bicameral legislature, the Olbiil Era Kelulau, and an independent judiciary. Its capital is Engelmade. Under the Compact of Free Association with the United States, Palau's defense is the responsibility of the U.S. military. Currently, the country's political system is non-partisan."),
        p("Outside of the national government structure, Palau is divided into 16 states, each with its own constitution and elected political subdivisions, which share a unified national judicial system. Of these 16 states, 10 are located on the country's largest island, Babeldaob, while the remaining 6 are spread across other island groups."),
        img(src="https://www.palauosp.org/wp-content/uploads/2019/05/capitol.jpg", height = "500px", width = "800px"),
        tags$div(
          style = "font-size: 12px; color: black; text-align: left;",
          "Image source:  https://www.palauosp.org/contact/"
        )
      ),
      "Economy"=tagList(
        p("Palau's economy relies on tourism, subsistence agriculture, and fishing."),
        p("Due to the islands' rich marine life, barrier reefs, the key tours include scuba diving and snorkeling, and visit World War II wrecks."),
        p("In April 2022, the country introduced Ol'au Palau, a responsible tourism initiative to safeguard its natural and cultural heritage.   The government, supported significantly by U.S. financial aid, is the largest employer."),
        img(src="https://www.hideawayholidays.com.au/wp-content/uploads/2023/12/Explore_Palau_Blog_Palau_Visitors_Authority_14.jpg", height = "500px", width = "900px"),
        tags$div(
          style = "font-size: 12px; color: black; text-align: left;",
          "Image source:  https://www.hideawayholidays.com.au/travel-blog/palau-adventures-unveiling-paradise-for-thrill-seekers-and-nature-lovers/"
        )
      ),
      "Culture" = tagList(
        h3("Matrilineal"),
        p("The matrilineal traditions of Palauan culture deeply influence the social structure. Here, family lineage and inheritance is passed down through the female line, shaping unique patterns of social relationships and leadership."),
        h3("Respect for Nature"),
        p("Nature occupies a central place in Palauan culture. Traditional beliefs emphasize the harmonious coexistence of man and nature, a concept that is not only reflected in ancient practices, but is also compatible with modern concepts of environmental protection."),
        h3("Oral Tradition"),
        p("Oral traditions are an important way in which Palauan culture is preserved and transmitted. Through the telling of myths, legends and historical stories, Palauans maintain their cultural memory and transmit the values and identity of their communities."),
        h3("Traditional Navigation"),
        p("As a maritime people, Palauans are known for their excellent traditional navigation skills. They navigate the vast Pacific Ocean using the stars, winds, and nature to guide them, and this seafaring tradition, which is closely tied to the sea, is embedded in their cultural DNA."),
        img(src="https://pristineparadisepalau.com/wp-content/uploads/2024/08/DSC06864.png", height = "500px", width = "900px"),
        tags$div(
          style = "font-size: 12px; color: black; text-align: left;",
          "Image source:  https://pristineparadisepalau.com/culture/"
        )
      ),
      "Geography" = tagList(
        p("Palau is home to about 18,000 people. Rechad er Belau, or Palauans, are the indigenous people of Belau, the traditional name of Palau. During your exploration in Palau, you will undoubtedly encounter diverse cultural influences from nations such as Japan and the United States. However, despite these external influences, Palauan culture remains deeply ingrained and resilient."),
        p("Traditional meeting houses called 'bai' are central to community life.")
      ),
      "Climate" = tagList(
        p("Palau has a tropical rainforest climate with an average annual temperature of about 28°C. Rainfall is abundant throughout the year, with 3,800 millimeters per year. The average humidity is 82% and although rainfall is more frequent from June to October, sunshine is still abundant."),
        p("Geographically, Palau is on the edge of the typhoon belt. Tropical cyclones do often form in the vicinity each year, but major tropical cyclones are extremely rare. Only three systems, Mike, Bopha and Haiyan, have ever hit Palau as typhoons in the historical record (Wikipedia Contributors, 2024)."),
        img(src = "https://www.oceanicsociety.org/wp-content/uploads/2021/04/palau-rock-islands-aerial.jpg", height = "500px", width = "900px"),
        tags$div(
          style = "font-size: 12px; color: black; text-align: left;",
          "Image source:  https://www.oceanicsociety.org/expedition/palau-snorkeling-the-rock-islands/"
        )
      ),
      "Nature environment"=tagList(
        h3("For a tiny archipelago with only a 328.14 square km landmass, Palau has an intriguing biodiversity of its own (Home - Pristine, n.d.)."),
        p("Around 75% of Palau's volcanic, coral atoll, and limestone islands are covered in native forests and mangroves. These forests, the most biodiverse in Micronesia, host over 1,400 plant species, including 194 endemic varieties such as 23 unique orchid species."),
        p("Palau also features the iconic Ngerukuid Islands Nature Reserve, or “70 Islands,” known for its maze-like channels and crystal-clear waters. Visitors often encounter sea creatures such as manta rays."),
        img(src="https://pristineparadisepalau.com/wp-content/uploads/2023/06/DJI_0311-1024x768.jpg", height = "500px", width = "900px"),
        tags$div(
          style = "font-size: 12px; color: black; text-align: left;",
           "Image source:  https://pristineparadisepalau.com/environmental-initiatives/"
        )
      )
    )
  })
  
#Comparative 
  output$fijiMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = 178.0650, lat = -17.7134, zoom = 7) %>% 
      addMarkers(lng = 178.0650, lat = -17.7134, popup = "Fiji")
  })
  
  output$fijiGlobalMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = 178.0650, lat = -17.7134, zoom = 2) %>% 
      addMarkers(lng = 178.0650, lat = -17.7134, popup = "Fiji")
  })
  
  output$compareCard <- renderUI({
    selected_metric <- input$comparisonMetric
    
    # select box UI
    plots_and_text <- switch(
      selected_metric,
      "GDP" = list(
        plotOutput("fiji_gdp", height = 300),
        plotOutput("com_gdp", height = 300),
        p("The GDP comparison shows the economic trends of Fiji and Palau over the years. Fiji's economy is significantly larger than Palau's due to its higher population and diversified economic sectors. Additionally, Fiji's GDP also fluctuated significantly overall.")
      ),
      "Population" = list(
        plotOutput("fiji_population", height = 300),
        plotOutput("com_population", height = 300),
        p("The graph is a line chart of population change in Fiji, showing a clear trend of population growth starting in 2021. The comparison on the right highlights that Fiji has a much larger population than Palau, reflecting its larger geographical area and resources.")
      ),
      "Visitors" = list(
        plotOutput("fiji_visitors", height = 300),
        plotOutput("com_visitors", height = 300),
        p("The chart on the left shows trends in tourist arrivals to Fiji. The number of visitors has dropped significantly since 2019, presumably due to the global pandemic. The visitor trends on the right chart show the relative attractiveness of Fiji and Palau to international visitors. While both islands are heavily dependent on tourism, Fiji has a larger visitor base due to its size and infrastructure. However, as a result, the epidemic has caused a greater loss of tourist numbers to Fiji.")
      )
    )
    
    # merge into card
    tagList(
      fluidRow(
        column(6, plots_and_text[[1]]), # first plot
        column(6, plots_and_text[[2]])  # second plot
      ),
      plots_and_text[[3]]               # describe
    )
  })
  
  # ggplot 
  output$fiji_gdp <- renderPlot({
    ggplot(fiji_data, aes(x = year, y = GDP)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "indianred1", size = 2) +
      scale_x_continuous(breaks = seq(min(fiji_data$year), max(fiji_data$year), by = 1)) +
      geom_text(aes(label = paste0(round(GDP, 1), "M")), vjust = -1.5, size = 3) +
      labs(title = "GDP of Fiji(in millions_$)", x = "Year", y = "GDP (in millions_$)") +
      theme_minimal()
  })
  
  output$com_gdp <- renderPlot({
    ggplot(compare_data, aes(x = year, y = GDP, color = country, linetype = country)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Fiji" = "blue", "Palau" = "lightblue")) +
      scale_linetype_manual(values = c("Fiji" = "solid", "Palau" = "dashed")) +
      scale_x_continuous(breaks = seq(min(compare_data$year), max(compare_data$year), by = 1)) +
      labs(title = "GDP Comparison: Fiji vs. Palau",
           x = "Year", y = "GDP (million_$)",
           color = "Country", linetype = "Country") +
      theme_minimal()
  })
  
  output$fiji_population <- renderPlot({
    ggplot(fiji_data, aes(x = year, y = Population)) +
      geom_line(color = "aquamarine2", size = 1) +
      geom_point(color = "green", size = 2) +
      geom_text(aes(label = Population), vjust = -0.5, color = "grey60", size = 3.5) +
      scale_x_continuous(breaks = seq(min(fiji_data$year), max(fiji_data$year), by = 1)) +
      labs(title = "Population of Fiji", x = "Year", y = "Population") +
      theme_minimal()
  })
  
  output$com_population <- renderPlot({
    ggplot(compare_data, aes(x = year, y = Population, color = country, linetype = country)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Fiji" = "green", "Palau" = "lightgreen")) +
      scale_linetype_manual(values = c("Fiji" = "solid", "Palau" = "dashed")) +
      scale_x_continuous(breaks = seq(min(compare_data$year), max(compare_data$year), by = 1)) +
      labs(title = "Population Comparison: Fiji vs. Palau (2012-2023)",
           x = "Year", y = "Population",
           color = "Country", linetype = "Country") +
      theme_minimal()
  })
  output$fiji_visitors <- renderPlot({
    ggplot(fiji_data, aes(x = year, y = Visitors)) +
      geom_line(color = "purple", size = 1) +
      geom_point(color = "tan1", size = 2) +
      geom_text(aes(label = Visitors), vjust = -0.5, color = "grey60", size = 3.5) +
      scale_x_continuous(breaks = seq(min(fiji_data$year), max(fiji_data$year), by = 1)) +
      labs(title = "Number of International Visitors in Fiji", x = "Year", y = "International Visitors") +
      theme_minimal()
  })
  
  output$com_visitors <- renderPlot({
    ggplot(compare_data, aes(x = year, y = Visitors, color = country, linetype = country)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Fiji" = "purple", "Palau" = "plum")) +
      scale_linetype_manual(values = c("Fiji" = "solid", "Palau" = "dashed")) +
      scale_x_continuous(breaks = seq(min(compare_data$year), max(compare_data$year), by = 1)) +
      labs(title = "Visitors Comparison: Fiji vs. Palau (2012-2023)",
           x = "Year", y = "Number of Visitors",
           color = "Country", linetype = "Country") +
      theme_minimal()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
