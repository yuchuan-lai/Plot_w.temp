library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(sf)
library(leafpop)
library(shiny)

plot_daily <- function (select.cty, sum.w.temp) {
  # prepare the df for ggplot
  select.city.w.temp <- sum.w.temp %>% filter(., Name == select.cty)
  plot <- ggplot() +
    geom_line(data = select.city.w.temp, aes(x = days, y = Past, color = "#F2BC7B")) +
    geom_line(data = select.city.w.temp, aes(x = days, y = Pres, color = "#FF6467")) +
    geom_line(data = select.city.w.temp, aes(x = days, y = Fcast, color = "#5C1A18")) +
    scale_color_manual(guide = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid"),
                                                                shape = c(NA, NA, NA), size = c(0.7, 0.7, 0.7),
                                                                color = c("#F2BC7B", "#FF6467", "#5C1A18"))),
                       values = c("#5C1A18", "#F2BC7B", "#FF6467"), labels = c(paste0("Historical\n1951", en.dash, "1970"), 
                                                                              paste0("Present\n2001", en.dash, "2020"), 
                                                                              paste0("Forecasted\n2051", en.dash, "2070")),
                       name = "Daily water temperature estimates:") +
    theme_set(theme_bw()) +
    theme(legend.position = "bottom") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(breaks = seq(0, 36, 2)) +
    xlab("Date") + ylab("Temperature (ºC)") +
    ggtitle(paste0("Estimates for ", select.city.w.temp$P.name, ", ", select.city.w.temp$State))
  
  return (plot)
}
plot_map <- function(select.period) {
  
  if (select.period == "rec"){
    w.temp.m.re <- rec.w.temp.m.re
  }
  if (select.period == "past"){
    w.temp.m.re <- past.w.temp.m.re
  }
  if (select.period == "fcast"){
    w.temp.m.re <- fcast.w.temp.m.re
  }
  
  plot.map <- leaflet(w.temp.m.re) %>% addProviderTiles(provider = providers$CartoDB.Positron) %>% 
    setView(lng = -98.585522, lat = 39.833333, zoom = 4.3) %>%  
    addLegend(
      position = "bottomleft",
      title = "Monthly avg. temp (ºC)",
      pal = select.pal, values = w.temp.m.re$Value, opacity = 1) %>% 
    addMinicharts(
      lng = w.temp.m.re$Lon, lat = w.temp.m.re$Lat,
      chartdata = w.temp.m.re$Value,
      type = "polar-area",
      showLabels = FALSE,
      time = w.temp.m.re$Month,
      fillColor = select.pal(w.temp.m.re$Value),
      opacity = 0.9,
      width = 20,
      # popup = popupGraph(graphs = plot_list),
      # popup = popupTable(city.info),
      transitionTime = 0
    )   %>%
    addCircleMarkers(data = city.info, group = "Name", color = "#FFFFFF00", fill = "#FFFFFF00",
                     popup = popupTable(city.info, zcol = c(3, 4, 2, 5, 6),
                                        row.numbers = FALSE, feature.id = FALSE)) 
  # %>%
  #   addPopupGraphs(plot_list, group = "Name", width = 400, height = 300)
  
  return(plot.map)
}  

en.dash <- substr(signs::signs(-2, accuracy = 1), 1, nchar(signs::signs(-2, accuracy = 1)) - 1)

rec.w.temp.m.re <- read.csv(paste0("./Data/Rec_w.temp.monthly.csv"))[-c(1)]
year.month <- format(seq(from = as.Date("2019-01-01"), to = as.Date("2019-12-01"), by = "month"), "%B")
rec.w.temp.m.re$Month <- factor(rec.w.temp.m.re$Month, levels = year.month, ordered = TRUE)
colnames(rec.w.temp.m.re)[3] <- paste0("Annual avg. (1951", en.dash, "1970)")

past.w.temp.m.re <- read.csv(paste0("./Data/Past_w.temp.monthly.csv"))[-c(1)]
year.month <- format(seq(from = as.Date("2019-01-01"), to = as.Date("2019-12-01"), by = "month"), "%B")
past.w.temp.m.re$Month <- factor(past.w.temp.m.re$Month, levels = year.month, ordered = TRUE)
colnames(past.w.temp.m.re)[3] <- paste0("Annual avg. (2001", en.dash, "2020)")

fcast.w.temp.m.re <- read.csv(paste0("./Data/Fcast_w.temp.monthly.csv"))[-c(1)]
year.month <- format(seq(from = as.Date("2019-01-01"), to = as.Date("2019-12-01"), by = "month"), "%B")
fcast.w.temp.m.re$Month <- factor(fcast.w.temp.m.re$Month, levels = year.month, ordered = TRUE)
colnames(fcast.w.temp.m.re)[3] <- paste0("Annual avg. (2051", en.dash, "2070)")

sum.w.temp.m <- merge(merge(rec.w.temp.m.re, past.w.temp.m.re[, c(1:3)], by  = c("Name", "Month")), fcast.w.temp.m.re[, c(1:3)], by  = c("Name", "Month"))

colnames(rec.w.temp.m.re)[3] <- "Value"
colnames(past.w.temp.m.re)[3] <- "Value"
colnames(fcast.w.temp.m.re)[3] <- "Value"

sum.w.temp <- read.csv(paste0("./Data/Sum.w.temp.daily.csv"))[-c(1)]
sum.w.temp$days <- as.Date(sum.w.temp$days)
sum.w.temp <- merge(sum.w.temp, select(rec.w.temp.m.re, - c(Month, Value)), by = "Name", all.x = TRUE)
colnames(sum.w.temp)

color.pal <- colorRampPalette(colors = c("#FECD90FF", "#F56B5CFF", "#AE347BFF", "#5B167EFF"), space="Lab")(20)    
select.pal <- colorNumeric(palette = color.pal, domain = c(rec.w.temp.m.re[, c(3)], past.w.temp.m.re[, c(3)], fcast.w.temp.m.re[, c(3)]))

colnames(sum.w.temp.m)[c(4, 5, 6, 7)] <- c("City","State", "Latitude", "Longitude")
city.info <- sum.w.temp.m %>% group_by(Name) %>% filter(!duplicated(Latitude)) %>% select(., - Month)
city.info <- st_as_sf(city.info, coords = c("Longitude", "Latitude"), crs = 4326)

estimates.type <- data.frame("name" = c("rec", "past", "fcast"),
                             "period" = c(paste0("Recent 2001", en.dash, "2020 level"), 
                                          paste0("Historical 1951", en.dash, "1970 level"), 
                                          paste0("projected 2051", en.dash, "2070 RCP8.5 level")))

state.list <- sort(city.info$State)
city.list <- sort(city.info$City)


ui <- fluidPage(
  titlePanel("Estimates of drinking water temperature at selected U.S. cities"),
  
  shinycssloaders::withSpinner(
    leafletOutput(outputId = "wtempPlot", width = "1000px", height = "500px"),
    hide.ui = FALSE, type = 3, color = "#666666", color.background = "#FFFFFF"),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("period", "Select the estimation period (for plotting the map)", choices = estimates.type$period, selected = estimates.type$period[1]),
      selectInput("state", "Select the state (for plotting daily estimates)", choices = state.list, selected = state.list[1]),
      selectInput("city", "Select the city (for plotting daily estimates)", choices = city.list, selected = city.info$City[city.info$State == "Alabama"][2]),
      width = 3
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("dailyPlot", width = "500px", height = "300px"),
        hide.ui = FALSE, type = 3, color = "#666666", color.background = "#FFFFFF")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$state, {
    x <- input$state
    updateSelectInput(session, "city",
                      choices = city.info$City[city.info$State == x],
                      selected = city.info$City[city.info$State == x][1])
  })
  
  d <- reactive({
    estimates.type$name[estimates.type$period == input$period]   
  }) 
  
  e <- reactive({
    city.info$Name[city.info$City == input$city]   
  }) 
  
  output$wtempPlot <- renderLeaflet({
    plot_map(d())
  })
  
  output$dailyPlot <- renderPlot({
    plot_daily(e(), sum.w.temp)
  })
  
}

shinyApp(ui = ui, server = server)




