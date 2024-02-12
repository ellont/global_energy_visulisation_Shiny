# Loading necessary libraries
library(geojsonio)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(plotly)

################## Data Preprocessing #####################
###########################################################

#Loading data sets
power_plant <- read.csv("data/global_power_plant.csv")
sustainable_energy <- read.csv("data/global_sustainable_energy.csv")
countries <- read.csv("data/countries_codes_and_coordinates.csv")
world_sp = geojson_read("data/50m.geojson", what = "sp")

# Pre-processing renewable
# Change column name to lower case
colnames(sustainable_energy) <- tolower(colnames(sustainable_energy))

# Merge with countries
sustainable_energy = merge(sustainable_energy, countries, by = "country")

# Update data/column type
# Set the year column to integer
sustainable_energy$year <- as.integer(sustainable_energy$year)

# Get max and min capacity across all power plants
max_capacity <- max(unique(power_plant$capacity_mw))
min_capacity <- min(unique(power_plant$capacity_mw))


################ Global level aggregated renewable energy ############
######################################################################
# Sum up continuous values
sum_aggregated <- aggregate(
  cbind(renewables..twh., 
        fossil.fuels.twh., 
        renewable.electricity.generating.capacity.per.capita, 
        co2_emissions_kt
        ) ~ year,
  data = sustainable_energy, FUN = sum
)

# Mean for the percentage value columns
mean_aggregated <- aggregate(
  cbind(renewable.in.tot.energy.consump..., 
        electricity.accessibility...,
        gdp_per_capita,
        low.carbon.electricity....electricity.) ~ year,
  data = sustainable_energy, FUN = function(x) round(mean(x), 1)
)

# Merging the two dataframes
sus_aggregated <- merge(sum_aggregated, mean_aggregated, by = "year")

################### Power Plant Treemap Data ############################
#########################################################################

# Grouping the power_plant
grouped_power_plant <- power_plant %>%
  group_by(country, primary_fuel, fuel_type) %>%
  summarise(total_capacity = sum(capacity_mw, na.rm = TRUE)) %>%
  ungroup()

# Add ids and parents for treemap
grouped_power_plant$ids <- paste0(grouped_power_plant$country, "-", grouped_power_plant$primary_fuel, "-", grouped_power_plant$fuel_type)
grouped_power_plant$parents <- paste0(grouped_power_plant$country, "-", grouped_power_plant$primary_fuel)

# Summarizing the grouped_power_plant by grouping by country and primary_fuel
# and calculating total_capacity by summing the total_capacity for each group.
primary_fuel_data <- grouped_power_plant %>%
  group_by(country, primary_fuel) %>%
  summarise(total_capacity = sum(total_capacity, na.rm = TRUE)) %>%
  mutate(ids = paste0(country, "-", primary_fuel),
         parents = country)

# Summarizing the grouped_power_plant dataframe at the country level 
# and calculating the total_capacity by summing the total_capacity for each country.
power_plant_country <- grouped_power_plant %>%
  group_by(country) %>%
  summarise(total_capacity = sum(total_capacity, na.rm = TRUE)) %>%
  mutate(ids = country,
         parents = "",
         primary_fuel = "",
         fuel_type = "")

# Combine all data
power_plant_treemap_data <- bind_rows(grouped_power_plant, primary_fuel_data, power_plant_country)


############# Functions and Variables ##################
########################################################

# Colors
renewColor <- "#50CB86"
fossilColor <- "#4F0A4F"
otherColor <- "#808080"
countryColor <- "#ff851b"
textColor <- "#3A3A3A"

# Assigning colors to fuel types
primary_fuels <- c("Renewable", "Fossil", "Other")
fuel_color <- c(renewColor, fossilColor, otherColor)
names(fuel_color) <- primary_fuels
fuel_pal <- colorFactor(palette = fuel_color, 
                        domain = primary_fuels,
                        ordered = TRUE
                        )


# Setting up the base map
# Bins for legend/coloring
bins = c(0,10,20,30,40,50,60,70,80,90,100)
# Basemap color
basemap_pal <- colorBin("viridis", domain = sustainable_energy$renewable.in.tot.energy.consump..., bins = bins)

# Basemap
basemap <- leaflet(world_sp) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "topright",
    overlayGroups = c("Sustainable Energy", "Power Plants"), # two layers
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Power Plants")) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = basemap_pal, values = ~sustainable_energy$renewable.in.tot.energy.consump...,
            title = "Renewable Energy(%)"#, font=list(family = "Arial", size=12)
            ) %>%
  addLegend("bottomleft", pal = fuel_pal, values = ~power_plant$primary_fuel,
          title = "Primary Fuel",
          group = "Power Plants"
          )


##################
# USER INTERFACE #
##################

ui <- dashboardPage(
  # Main Header for the dashboard
  dashboardHeader(title = tags$p("Renewable Energy Dashboard",style = "font-size: 26px;"), titleWidth = 300),  # Set header color to green
  # Dashboard Sidebar
  dashboardSidebar(
    width = 220,
    # Sub tabs - pages
    sidebarMenu(
      id = "main_page",
      menuItem(text = tags$p("Global Overview", style = "font-size: 16px;"),
               tabName = "renew_map", icon = icon("globe")),
      menuItem(text=tags$p("Country Details", style = "font-size: 16px;"),
               tabName = "country_details", icon = icon("flag"))
    ),
    # Adding spaces between tabs and filters
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    # Year filter (allowing animation)
    sliderInput("mapYear", "Year",
                min = min(unique(sustainable_energy$year)),
                max = max(unique(sustainable_energy$year)),
                value = 2000, step = 1, sep = "",
                animate = animationOptions(interval = 3000, loop = FALSE)),
    # Fuel Type filter (multiple choice)
    selectInput(inputId = "primaryFuelType", "Primary Fuel Type",
                choices = c("All", unique(power_plant$primary_fuel)),
                selected = "All", multiple = TRUE),
    
    # Disclaimer at the bottom
    tags$div(id = "sidebar-disclaimer", 
             "Disclaimer: Used dataset is incomplete and has missing data points.", 
             style = "color: grey; font-size: 12px; font-style: italic; position: absolute; bottom: 10px;")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML( #Setting dashboard body size
        "body{
          min-height: 850px;
          max-height: 850px;
          min-width: 1400px;
          max-width: 1400px;
          margin: auto;
        }
        #sidebar-disclaimer {
        width: 100%;
        padding-left: 15px;
        padding-right: 15px;
        }
        /*Source: https://stackoverflow.com/questions/36080529/r-shinydashboard-customize-box-status-color*/
        /* CSS to adjust colors of sidebar items */
        .skin-blue .main-header .logo {
                              background-color: #42A546;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #75982D;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #42A546;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #003700;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #003700;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #003700;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #75982D;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #75982D;
         }
         .box.box-solid.box-primary>.box-header {
                              color:#fff;
                              height: 35px;
                              background:#42A546
                                                }
          /* box header  */                      
         .box.box-solid.box-primary{
                              border-bottom-color:#42A546;
                              border-left-color:#42A546;
                              border-right-color:#42A546;
                              border-top-color:#42A546;
                              }
        /* background color  */  
        .content-wrapper,
            .right-side {
            background-color: #ffffff;
            }
        "
      ))
    ),
    tabItems(
      # Global Overview tab
      tabItem(tabName = "renew_map",
              fluidRow(
                column(7, 
                       # Global map
                       box(title = "Renewable Energy and Power Plants", status = "primary", solidHeader = TRUE, width = "100%", height = 720,
                    leafletOutput("main_map", height = "660")
                    ),
                    # Power Plant printout
                    uiOutput("power_plant_details", width = 12)

                    ),
                
                column(5,
                       # Scatter plot
                       box(title = "Renewable Energy Capacity for Top 50 CO2 Emitters", status = "primary", solidHeader = TRUE, width = "100%", height = 350,
                           plotlyOutput("topCountryByCO2", height = "300px")
                      
                        ),
                       # Bar + line chart
                        box(title = "Global Trend for Renewable and Fossil Energy", status = "primary", solidHeader = TRUE, width = "100%", height = 350,
                            plotlyOutput("renewVsFossilPlot", height = "300px")
                          )
                       )
                  )
              
      ),
      # Country Details Tab
      tabItem(tabName = "country_details",
              fluidRow(
                # Country filter
                box(style="border-top-color:#42A546;border-bottom-color:#42A546; border-left-color:#42A546;border-right-color:#42A546;",
                    width = 4, height = 100, 
                    selectInput(inputId = "countryInput", "Country",
                                choices = c("Global", unique(sustainable_energy$country)),
                                selected = "Global")),
                infoBoxOutput("gdpBox", width = 4), # value for gdp per capital
                infoBoxOutput("rankBox", width = 4) # value for global ranking
             
              ),
              fluidRow(
                # Radar chart
                box(title = "Matrix", status = "primary", solidHeader = TRUE, width = 6, height = 280,
                    plotlyOutput("country_radarChart", height = "220px")),
                # Bar chart
                box(title = "Country Trend for Renewable and Fossil Energy",status = "primary", solidHeader = TRUE, width = 6, height = 280,
                    plotlyOutput("regional_plot", height = "220px"))
              ),
              fluidRow(
                # Treemap
                box(title = "Power Plants Overview", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("regional_powerplant", height = "310px")
                    ),
                #Display in case no data rendered in treemap
                uiOutput("noDataText")
              )
      )
    )
  )
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
######### Reactive Components #########
  # User Input Year
  mapYear <- reactive({
    input$mapYear
  })
  
  # User Input Country
  countryInput <- reactive({
    input$countryInput
  })
  
  # User Input Fuel type
  primaryFuelType <- reactive({
    input$primaryFuelType
  })
  
  # Reactive Renewable energy data by year
  reactive_sustainable_energy <- reactive({
    sustainable_energy %>%
      filter(year == mapYear())
  })
  
  # Reactive cumulative renewable energy data to date
  reactive_cumulative_sus_eng <- reactive({
    sus_aggregated %>%
      filter(year <= mapYear())
  })
  
  # Reactive power plant for map
  reactive_power_plant <- reactive({
    if ("All" %in% primaryFuelType()){
      power_plant
    } else {
      power_plant %>% filter(primary_fuel %in% primaryFuelType())
    }
  })
  
  
  # Reactive country polygon for sustainable energy
  reactive_polygons <- reactive({
    world_sp[world_sp$ISO_A3 %in% reactive_sustainable_energy()$alpha3, ]
  })
  
  # Reactive country polygon with sustainable energy
  reactive_susPoly_joined <- reactive({
    merged_data <- merge(reactive_polygons(), reactive_sustainable_energy(), by.x = "ISO_A3", by.y = "alpha3")
    merged_data
  })
  
  # Reactive top 50 countries by CO2 emissions
  reactive_top_country_by_co2 <- reactive({
    top_countries <- reactive_sustainable_energy() %>%
      filter(!is.na(renewable.in.tot.energy.consump...)) %>%
      filter(!is.na(co2_emissions_kt)) %>%
      arrange(desc(co2_emissions_kt)) %>%
      head(50)
    top_countries
  })
  
  # Reactive for Country Details by country
  reactive_sus_country <- reactive({
    if (countryInput() == "Global") {
      sus_aggregated
    } else {
      sustainable_energy %>%
      filter(country == countryInput())
    }
  })
  
  # Reactive Radar Chart
  reactive_country_year <- reactive({
    if (countryInput() == "Global") {
      sus_aggregated %>% 
        filter(year == mapYear())
    } else {
      reactive_sustainable_energy() %>%
        filter(country == countryInput())
    }
  })
  
  # Reactive for Country Details bar chart 
  reactive_country_bar <- reactive({
    reactive_sus_country() %>%
      filter(year <= mapYear())

  })
  
  # Reactive for Power Plant treemap
  reactive_pp_country <- reactive({
    data <- power_plant_treemap_data
    
    if ("All" %in% primaryFuelType()){ # if fuel type is not filtered
      if (countryInput() == "Global"){
        data # if country is not filtered, return the entire dataset
      } else { # otherwise, return the dataset with the selected country only
        data %>% 
        filter(country == countryInput())
      }
      # if fuel type is selected
    } else if (countryInput() == "Global"){ # if no country is selected
      data %>% filter(primary_fuel %in% c("",primaryFuelType())) # return global data with selected fuel type
    } else {
      # otherwise, return the data for the selected country and fuel type
      data %>% 
        filter(primary_fuel %in% primaryFuelType()) %>%
        filter(country == countryInput())
    }
  })

  
##################### OutPuts #####################
################# Global Overview #################      
  
  # Global choropleth map
  output$main_map <- renderLeaflet({
    basemap
  })

  # Observe User input / Year filtering and dynamically load map
  # Code Adapted from https://shiny.posit.co/r/gallery/life-sciences/covid19-tracker/
  observeEvent(list(input$mapYear,input$primaryFuelType),{
    data <- reactive_susPoly_joined()
    leafletProxy("main_map")%>% 
      clearMarkers() %>% # clear markers and shapes before loading
      clearShapes() %>%
      
      # Renewable energy share by country
      addPolygons(
        data = data,
        stroke = FALSE,
        smoothFactor = 0.1,
        fillOpacity = 0.5,
        fillColor = ~basemap_pal(renewable.in.tot.energy.consump...),
        highlightOptions = highlightOptions( # highlight the country when hovered over
          weight = 1,
          fillOpacity = 0.7
        ),
        layerId = ~NAME,
        label = ~paste0(NAME,": ",renewable.in.tot.energy.consump...,"% Renewable Energy"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px", "color" = renewColor),
          textsize = "14px", direction = "auto")
        
      ) %>%
      
      # Power Plant markers
      addCircleMarkers(
        data = reactive_power_plant(),# power_plant,
        lat = ~latitude, lng = ~longitude,
        weight = 1, radius = ~ifelse(capacity_mw * 55 / (max_capacity - min_capacity) < 1, 1, capacity_mw * 50 / (max_capacity - min_capacity)),#~capacity_mw * 50 / (max_capacity - min_capacity),#reactive_power_plant()$estimated_gwh * 30 / (max_generated_gwh - min_generated_gwh),
        fillOpacity = 0.8, fillColor = ~fuel_pal(primary_fuel),
        stroke = FALSE, group = "Power Plants",
        label = ~paste("Fuel type: ", fuel_type),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px", "color" = textColor),
          textsize = "14px", direction = "auto"),
        layerId = ~gppd_idnr,
        options = list(category = ~primary_fuel)
      )
    
  })
  
  # Monitor for a click event on the polygons
  observeEvent(input$main_map_shape_click, {
    
    # Extract the clicked country name
    clicked_country <- input$main_map_shape_click$id
    selected_year <- input$mapYear
    
    # Update the country filter
    updateSelectInput(session, "countryInput", selected = clicked_country)
    
    # Switch to the "Country Details" tab
    updateTabItems(session, 'main_page', selected='country_details')
    
  })
  
  # Output for power plant info after clicking marker on main map
  output$power_plant_details <- renderUI({
    power_plant_id <- input$main_map_marker_click$id
    # If no power plant selected, stop
    if(is.null(power_plant_id)) return()
    
    #get the power plant info by gppd_idnr
    power_plant_row <- power_plant %>%
      filter(power_plant$gppd_idnr == power_plant_id)
    power_plant_row <- power_plant_row[1, ]
    
    #print out in table format
    tagList(
      tags$table(style = "border: 1px solid black; width: 100%; border-collapse: collapse; font-family: Arial",
                 tags$tr(
                   tags$td(style = "background-color: #F2F2F2; border-right: 1px solid black; width: 40%","Country:"),
                   tags$td(style = "text-align: left;", power_plant_row$country)
                 ),
                 tags$tr(
                   tags$td(style = "background-color: #F2F2F2; border-right: 1px solid black;", "Power Plant ID:"), 
                   tags$td(style = "text-align: left;", power_plant_row$gppd_idnr)
                 ),
                 tags$tr(
                   tags$td(style = "background-color: #F2F2F2; border-right: 1px solid black;", "Primary Fuel Type:"), 
                   tags$td(style = "text-align: left;", power_plant_row$fuel_type)
                 ),
                 tags$tr(
                   tags$td(style = "background-color: #F2F2F2; border-right: 1px solid black;", "Capacity (MW):"), 
                   tags$td(style = "text-align: left;", power_plant_row$capacity_mw)
                 )
      )
    )
  })
  
  # Scatter Plot for top 50 countries by co2 emission
  output$topCountryByCO2 <- renderPlotly({
    data <- reactive_top_country_by_co2()
    # color matrix
    data$diagonal_metric <- with(data, renewable.in.tot.energy.consump... - co2_emissions_kt)
    # Convert countries to numeric symbols
    symbols <- as.numeric(as.factor(data$country))
    # Tool tip text
    tooltip_text <- paste0(
      "<b>", data$country, "</b></br></br>",
      "<i>GDP per capita ($): </i><i>", format(round(data$gdp_per_capita,0), big.mark = ","),"</i></br>",
      "<i>CO2 Emission (kt): </i><i>", format(round(data$co2_emissions_kt,0), big.mark = ","),"</i></br>",
      "<i>Renewable Energy Share: </i><i>",data$renewable.in.tot.energy.consump...,"%","</i>"
    )
    # set up plot
    p <- plot_ly(data = data,
                 source = "topCountryByCO2",
                 x = ~renewable.in.tot.energy.consump...,
                 y = ~co2_emissions_kt,
                 text = tooltip_text,
                 type = 'scatter',
                 mode = 'markers',
                 color = ~diagonal_metric,
                 colors = colorRampPalette(c("#E10000", "#42A546"))(50),
                 size = ~co2_emissions_kt,
                 marker = list(
                   symbol = symbols
                 ),
                 hoverinfo = "text"
                 
    )
    p <- p %>% layout(
      # setup axis details
      xaxis = list(
        title = list(text = "Renewal Energy Capacity Per Capita", font=list(family = "Arial")),
        zeroline = TRUE,
        # range = c(0, 100),
        titlefont = list(size = 12, family = "Arial"),
        showgrid = FALSE),
      yaxis = list(
        title = list(text="CO2 Emission (kt)", font=list(family="Arial")),
        zeroline = TRUE,
        titlefont = list(size = 12, family = "Arial"),
        showgrid = FALSE)
      
    )
    
    p <- p %>% hide_colorbar()
    p
    
  })
  
  # Monitor for a click event on the ScatterPlot, and take user to country details tab
  observeEvent(event_data("plotly_click", source = "topCountryByCO2"), {
    data <- reactive_top_country_by_co2()
    click_data <- event_data("plotly_click", source = "topCountryByCO2")
    selected_year <- input$mapYear
    
    if(!is.null(click_data$pointNumber)) {
      clicked_country <- data$country[click_data$pointNumber + 1]
      # Update country filter
      updateSelectInput(session, "countryInput", selected = clicked_country)
      
      # Switch to the "Country Details" tab
      updateTabItems(session, 'main_page', selected='country_details')
    }
  })

    
   #Bar + Line chart for Global renewal vs fossil
  output$renewVsFossilPlot <- renderPlotly({
    data <- reactive_cumulative_sus_eng()
    data$year <- as.factor(data$year) #convert year to category
    
    tooltip_text_renewable <- paste0(
      "Renewable Fuel Electricity (TWh): %{y:,.0f}<br>",
      "<extra></extra>"
    )

    tooltip_text_fossil <- paste0(
      "Fossil Fuel Electricity (TWh): %{y:,.0f}<br>",
      "<extra></extra>"
    )
    
    # set up plot
    p <- plot_ly(data=data, source = "renewVsFossilPlot", x=~year)
    p <- p %>%
      add_lines( #lines for renewable
      y = ~renewables..twh.,
      name = 'Renewable',

      hovertemplate = tooltip_text_renewable,
      line = list(color = renewColor)
    )
    p <- p %>%
      add_bars( # bars for fossil
      y = ~fossil.fuels.twh.,
      name = "Fossil",
      hovertemplate = tooltip_text_fossil,
      marker = list(color = fossilColor, opacity = 0.7)
      )
    
    p <- p %>% layout( # Set up axis
                      xaxis = list(
                        title = "Year",
                        tickvals = ~year,
                        titlefont = list(size = 12, family="Arial"),
                        tickfont = list(size = 10, family="Arial"),
                        showgrid = FALSE),
                      yaxis = list(title = "TWh",
                                   titlefont = list(size = 12, family="Arial"),
                                   tickfont = list(size = 10,  family="Arial"),
                                   showgrid = FALSE
                                   )
                      )

    p
  })
  
  
  # Monitor for a click event on the BarChart for primary fuel type
  observeEvent(event_data("plotly_click", source = "renewVsFossilPlot"), {
    click_data <- event_data("plotly_click", source = "renewVsFossilPlot")
    if(!is.null(click_data$curveNumber)) {
      if(click_data$curveNumber == 1){ #curve number 1 for fossil, 0 for renewable
        # update fuel type
        updateSelectInput(session, "primaryFuelType", selected = "Fossil")
      } else {
        updateSelectInput(session, "primaryFuelType", selected = "Renewable")
      }
    }
  })

 
  ###################################################
  ############## Regional Breakdown #################

  #Infobox for GDP
  output$gdpBox <- renderInfoBox({
    data <- reactive_sus_country() %>% filter (year == mapYear())
    
    infoBox(
      title = tags$span("GDP Per Capita", style = "white-space: normal; font-weight: bold; font-size:14px; width: 150px; display: inline-block;"),
      value = tags$p(format(round(data$gdp_per_capita,0),big.mark = ","), 
                        style = "font-weight: bold; font-size:36px;"),
      # value = format(round(data$gdp_per_capita,0),big.mark = ","),
      icon = icon("dollar-sign"),
      color = "orange", 
      fill = TRUE
    )
  })
  
  # Infobox for Country Rank by renewable capacity per capita
  output$rankBox <- renderInfoBox({
    data <- reactive_sustainable_energy() %>% 
      arrange(desc(renewable.electricity.generating.capacity.per.capita)) %>%
      mutate(ranking = row_number()) #create ranking
    
    if(countryInput()=="Global"){
      rank <- "-" # display - if no country is selected
    } else { # otherwise filter by selected country, and get rank
      country_data <- data %>% filter (country == countryInput())
      rank <- country_data$ranking
    }
    
    infoBox(
        title = tags$span("Renewable Capacity Ranking", 
                          style = "white-space: normal; font-weight: bold; font-size:14px;"),
        value = tags$p(rank, 
                          style = "font-weight: bold; font-size:36px;"),
        icon = icon("ranking-star"),
        color = "orange", 
        fill = TRUE
      )
  })
  
  # Bar Chart for Country renewable energy
  output$regional_plot <- renderPlotly({
    data <- reactive_country_bar()
    tooltip_text_renewable <- paste0(
      "<b>%{x:.0f}</b><br>",
      "Renewable Fuel Electricity (TWh): %{y:,.0f}<br>",
      "<extra></extra>"
    )

    tooltip_text_fossil <- paste0(
      "<b>%{x:.0f}</b><br>",
      "Fossil Fuel Electricity (TWh): %{y:,.0f}<br>",
      "<extra></extra>"
    )

  # Setup bar chart
    plot_ly(data = data,
            source = "regional_plot",
            x = ~year, 
            y = ~renewables..twh., 
            type = "bar", 
            name = "Renewable",
            marker = list(color ='rgba(80,203,134,0.7)'), #bar for renewable
            hovertemplate = tooltip_text_renewable
            ) %>%
      add_trace(y = ~fossil.fuels.twh.,  #add another bar for fossil
                name = "Fossil", 
                marker = list(color = 'rgba(79,10,79,0.7)'), 
                hovertemplate = tooltip_text_fossil
                ) %>%
      layout(#set up axis
             xaxis = list(title=list(text="Year", family ="Arial"),
                          showgrid = FALSE,
                          dtick = 2
                          ),
             yaxis = list(title = list(text='TWh', family = "Arial"), 
                          zeroline = TRUE,
                          showgrid = FALSE
                          ),
             barmode = "group",
             margin = list(l = 0.1, r = 0, b = 0, t = 0)  # Remove margins
             )
  })
  
  # Monitor click on BarChart - Country
  observeEvent(event_data("plotly_click", source = "regional_plot"), {
    click_data <- event_data("plotly_click", source = "regional_plot")
    if(!is.null(click_data$curveNumber)) {
      if(click_data$curveNumber == 1){
        updateSelectInput(session, "primaryFuelType", selected = "Fossil")
      } else {
        updateSelectInput(session, "primaryFuelType", selected = "Renewable")
      }
    }
  })
  
  # Radar Chart for sustainable energy data
  output$country_radarChart <- renderPlotly({
    data <- reactive_country_year()
    # get the columns with % values
    subset_data <- data[, c('renewable.in.tot.energy.consump...',
                            'electricity.accessibility...',
                            'low.carbon.electricity....electricity.')]
    theta_values <- c('Renewable %', 'Electricity Accessibility %', 'Low Carbon Electricity %')
    r_values <- as.numeric(subset_data[1,])
    
    p <- plot_ly(
      type = 'scatterpolar',
      r = c(r_values,r_values[1]),
      theta = c(theta_values,theta_values[1]),
      fill = 'toself',
      fillcolor = 'rgba(255, 133, 27,0.7)',
      marker = list(size = 10, color = countryColor),
      line = list(color = countryColor, width = 2),
      hoverinfo = 'theta+r',
      hovertemplate = paste0("<b>%{theta}</b><br>",
                            "%{r:.1f}","<br>", # Display the value with 2 decimal places
                            "<extra></extra>")
    )
    
    p <- p %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100),
            gridcolor = "rgba(128, 128, 128, 0.5)",
            linecolor = "rgba(128, 128, 128, 0.5)",
            gridwidth = 0.3,
            margin = list(l = 0, r = 0, b = 0, t = 0)  # Remove margins
          )
        ),
        showlegend = F
      )
    p
  })

  # Treemap for power plants
  output$regional_powerplant <- renderPlotly({
    data <- reactive_pp_country()

    # text to show
    tooltip_text <- paste0(
      "<br>",
      "<b>",format(data$total_capacity, big.mark = ","), "</b>"," Mw", "<br>"
    )
    
    hover_text <- paste0(
      "<b>",data$ids,"</b><br>",
      "<b>",format(data$total_capacity, big.mark = ","), "</b>"," Mw", "<br>"
    )
    
    # setup colors for fuel types
    custom_colors <- ifelse(grepl("Hydro", data$ids, ignore.case = TRUE), '#7FFFD4',
                            ifelse(grepl("Wave and Tidal", data$ids, ignore.case = TRUE), '#ACFFD2',
                            ifelse(grepl("Geothermal", data$ids, ignore.case = TRUE), '#166C3C',
                            ifelse(grepl("Wind", data$ids, ignore.case = TRUE), '#66BC8C',
                            ifelse(grepl("Solar", data$ids, ignore.case = TRUE), '#DEFFFF',
                            ifelse(grepl("Biomass", data$ids, ignore.case = TRUE), '#8EE4B4', 
                              ifelse(grepl("Oil", data$ids, ignore.case = TRUE), '#945089',
                              ifelse(grepl("Coal", data$ids, ignore.case = TRUE), '#6C2861',
                              ifelse(grepl("Gas", data$ids, ignore.case = TRUE), '#BC78B1',
                              ifelse(grepl("Petcoke", data$ids, ignore.case = TRUE), '#300025',
                               ifelse(grepl("Other-Other", data$ids, ignore.case = TRUE), "#898989",
                               ifelse(grepl("Nuclear", data$ids, ignore.case = TRUE), "#575757",
                               ifelse(grepl("Waste", data$ids, ignore.case = TRUE), "#CFCFCF",
                               ifelse(grepl("Storage", data$ids, ignore.case = TRUE), "#F7F7F7",
                                ifelse(grepl("Cogeneration", data$ids, ignore.case = TRUE), "#2F2F2F", NA)))))))))))))))
    # Set up treemap plot
    p <- plot_ly(
      data = data,
      source = 'regional_powerplant',
      type = 'treemap',
      ids = ~ids,
      labels = ~ids,
      parents = ~parents,
      values = ~total_capacity,
      branchvalues="total",
      text = tooltip_text,
      hovertemplate = hover_text,
      pathbar=list(visible= TRUE),
      marker = list(
        colors = custom_colors,
        colorbar = list(title = "Total Capacity (TWh)")
      )
    ) %>%  layout(
        margin = list(l = 0, r = 0, b = 0, t = 0)  # Remove margins
      
      ) %>%

    hide_colorbar()
    p
  })
  
  # Monitor click on treemap - Country
  observeEvent(event_data("plotly_click", source = "regional_powerplant"), {
    click_data <- event_data("plotly_click", source = "regional_powerplant")
    if(countryInput() == 'Global'){ #update the country filter only when no country is selected
      if(!is.null(click_data$pointNumber)) {
      clicked_country <- power_plant_treemap_data$country[click_data$pointNumber + 1]
      # update the selected country
      updateSelectInput(session, "countryInput", selected = clicked_country)
      }
    }
    
  })
  
  # Print out to let the user know if no power plant data for the selected country 
  output$noDataText <- renderUI({
    if (nrow(reactive_pp_country())==0){
      tags$span(h5(("The selected country has no power plant data available."), style=paste0("color: ", textColor, "; text-align: center; font-weight: bold;")))
    }
    
  })
}

#############
# RUN SHINY #
#############

shinyApp(ui=ui, server=server)