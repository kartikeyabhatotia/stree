install.packages("Rtools")
if (!require(devtools))
install.packages("devtools")
devtools::install_github("jcheng5/googleCharts")
#googleCharts is not in CRAN
library(googleCharts)
library(shiny)
library(dplyr)
#fix your directory
data <- read.csv("cashinbox.csv")
data$type <- as.factor(data$type)


xlim <- list(
  min = min(data$CashInBox),
  max = max(data$CashInBox) + 0.3
)
ylim <- list(
  min = min(data$genderperc)- 0.1,
  max = max(data$genderperc)
)

ui <- fluidPage(
  # This line loads the Google Charts JS library
  googleChartsInit(),
  
  # Use the Google webfont "Source Sans Pro"
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
             "body {font-family: 'Source Sans Pro'}"
  ),
  
  h2("Cash in Box x Gender"),
  #fix the chart size here
  googleBubbleChart("chart",
                    width="100%", height = "475px",
                    # Set the default options for this chart; they can be
                    # overridden in server.R on a per-update basis. See
                    # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                    # for option documentation.
                    options = list(
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      # Set axis labels and ranges
                      hAxis = list(
                        title = "cash in rupees",
                        viewWindow = xlim
                      ),
                      vAxis = list(
                        title = "percentage of female enterprises in sector",
                        viewWindow = ylim
                      ),
                      # The default padding is a little too spaced out
                      chartArea = list(
                        top = 50, left = 75,
                        height = "75%", width = "75%"
                      ),
                      # Allow pan/zoom
                      explorer = list(),
                      # Set bubble visual props
                      bubble = list(
                        opacity = 0.5, stroke = "none",
                        # Hide bubble label
                        textStyle = list(
                          color = "none"
                        )
                      ),
                      # Set fonts
                      titleTextStyle = list(
                        fontSize = 16
                      ),
                      tooltip = list(
                        textStyle = list(
                          fontSize = 12
                        )
                      )
                    )
  ),
  fluidRow(
    shiny::column(10, offset = 2, 
                  sidebarLayout(
                    sidebarPanel(
                      h6("There is a strong inverse relationship between feminised sectors
                         and the cash in box, which suggests sectors where women entrepreneurs
                         specialise has lesser working capital")
                      ), 
                    mainPanel(
                      h3(textOutput("Caption"))
            
                    
                    # Show a plot of the generated distribution
                
                    )
                  )
    )
  ))


server <- function(input, output, session) {
  #change colours of bubbles here
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$type)
  )
  
  yearData <- reactive({
    df <- data %>%
     # filter(Year == input$year) %>%
      select(BusinessType, CashInBox, genderperc,type, freq) %>%
      arrange(type)
  })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "feminised sectors vs working capital, %s",
          input$year),
        series = series
      )
    )
  })
}

shinyApp(ui = ui, server = server)
