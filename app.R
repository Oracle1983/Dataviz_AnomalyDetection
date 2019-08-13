# ##### Packages and Functions ###########################################################################################
#
#'shiny' => For building interactive web applications
#'semantic.dashboard' => For adding support for semantic UI dashboard comptiable to shinydashboard functionalities
#'tidyverse' => For ggplot2() function to plot visualisations Using the Grammar of Graphics
#'plotly' => For creating interactive web graphic for parallel plot and control chart
#'latticeExtra' => For horizonplot() function to plot horizon graph
#'openair' => For timeAverage() function to calculate time averages for data frames
#'DataCombine' => For change() function to calculate percentage change from a specified lag, including within groups
#'ggQC' => For stat_QC() function to produce QC charts with gpplot framework
#'xts => For as.xts() function to convert xts objects into environments for horizon graph
#'lubridate' => For functions to work with date-times especially extraction of components of months, days, hours, etc.
#'tibbletime' =>
#'anomalize' =>
#'DT' => For DataTables function to display R data objects (matrices or data frames) as tables.
#'data.table' => For transform function to perform data.table transformation.
#
# ########################################################################################################################

packages <- c('shiny',
              'semantic.dashboard',
              'tidyverse',
              'plotly',
              'latticeExtra',
              'openair',
              'DataCombine',
              'ggQC',
              'xts',
              'lubridate',
              'tibbletime',
              'anomalize',
              'DT',
              'data.table')

for (p in packages) {
  if(!require(p, character.only = T)) {
    install.packages(p)
  }
  library(p, character.only = T)
}

options(shiny.maxRequestSize = 30*1024^2)


# Load processed data (for choices under selectInput)
hourly <- read_csv("./data/hourly.csv", locale=locale(tz="Singapore"))
per_hour <- read_csv("./data/per_hour.csv", locale=locale(tz="Singapore"))


# Extract time range (for min and max under sliderInput)
#mindate = as.POSIXct("2007-07-01 00:00:00","%Y-%m-%d %H:%M:%S", tz="Singapore")
#maxdate = as.POSIXct("2007-08-01 00:00:00","%Y-%m-%d %H:%M:%S")
#mindate = min(as.POSIXct(hourly$date,"%Y-%m-%d %H:%M:%S", tz="Singapore"), na.rm = TRUE)
mindate = min(hourly$date, na.rm = TRUE)
maxdate = max(hourly$date, na.rm = TRUE)
initial <- mindate+9.5*86400 # 86400 secs


horizonOutput <- function(outputId, width = '400%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'horizon', width, height, package = 'horizon')
}


ui <- dashboardPage(
    ##############################################
    # Header-start                               #
    ##############################################
    dashboardHeader(titlePanel(title=div(img(height = 75, width = 1850, src="g11_title_block.png"))),
                    color = 'black', inverted = TRUE
                    ),
    ##############################################
    # Sidebar-start                              #
    ##############################################    
    dashboardSidebar(size = "thin", color = "teal",
                     sidebarMenu(
                         menuItem(tabName = "overview", "Overview", icon=icon("clipboard outline")),
                         menuItem(tabName = "parallelplot", "Parallel Plot", icon=icon("chart bar outline")),
                         menuItem(tabName = "horizongraph", "Horizon Graph",icon=icon("chart area")),
                         menuItem(tabName = "controlchart", "Control Chart",icon=icon("chart line"))
                         )
                     ),
    
    ##############################################
    # Body-start                                 #
    ##############################################
    dashboardBody(
      tabItems(
        selected = 1,
        
        ##########################################
        ############# overview-start #############
        ##########################################
        tabItem(
          tabName = "overview",
          fluidRow(
            h1("Overview")
            )
          ),
        ##########################################
        ########### parallelplot-start ###########
        ##########################################
        tabItem(
          tabName = "parallelplot",
          fluidRow(
            column(3,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:0px;"),
                       selectInput(
                         inputId = 'hFunSys_item_pc',
                         label = 'Function System:',
                         choices = unique(as.vector(per_hour['FunSys_item'])),
                         selected = '40',
                         multiple = FALSE,
                         selectize = FALSE)
                       ),
                     width = 3,
                     title = "Selection of Sensor Groups",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                      )
                  ),
            
            tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: lightgrey}")),
            column(12,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:45px;padding-bottom:15px;"),
                       sliderInput("hsliderDate_pc", 'Range of dates:',
                                   mindate,
                                   maxdate,
                                   c(mindate, maxdate),
                                   step = 300),
                       sliderInput("hsliderDate2_pc", 'Date (reference - min of Day Window):',
                                   mindate,
                                   maxdate,
                                   value = mindate,
                                   step = 300)
                       ),
                     splitLayout(
                       cellArgs = list(style = "padding:0px;padding-bottom:0px;"),
                       selectInput(
                         inputId="hinputDate_start_pc",
                         label = HTML("Exclude date from:"),
                         choices = c('select...'='NULL',unique(as.vector(per_hour['date']))),
                         selected = NULL,
                         multiple = FALSE,
                         selectize = FALSE,
                         width=200),

                       selectInput(
                         inputId="hinputDate_end_pc",
                         label = HTML("to:"),
                         choices = c('select...'='NULL',unique(as.vector(per_hour['date']))),
                         selected = NULL,
                         multiple = FALSE,
                         selectize = FALSE,
                         width=200),
                       
                      
                       selectInput(
                         inputId = 'hrange_pc', 
                         label = 'Day Window:', 
                         choices = c('1 day'=1,
                                     '3 days'=3, 
                                     '5 days'=5,
                                     '7 days'=7,
                                     '14 days'=14,
                                     'All days'=max(unique(day(per_hour$date)))),
                         selected = 1,
                         multiple = FALSE, 
                         selectize = FALSE),
                       
                       selectInput(
                         inputId = 'hinterval_pc',
                         label = 'Time Interval:',
                         choices = '*',
                         multiple = FALSE,
                         selectize = FALSE)
                       
                       ),
                     
                     width = 12,
                     title = "Other Settings",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                      )
                  ),
            column(width = 16,
                   plotlyOutput("parallelplot", height=650)
            )
          )
        ),
        ##########################################
        ########### horizongraph-start ########### 
        ##########################################
        tabItem(
          tabName = "horizongraph",
          fluidRow(
            column(3,align="left",
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:0px;"),
                       selectInput(
                         inputId = 'hFunSys_item', 
                         label = 'Function System:', 
                         choices = unique(as.vector(hourly['FunSys_item'])), 
                         selected = '40',
                         multiple = FALSE, 
                         selectize = FALSE)
                     ),
                     # splitLayout(
                     #   cellArgs = list(style = "padding-left:0px;padding-bottom:0px;"),
                     #   selectInput(
                     #     inputId = 'hMsureGr', 
                     #     label = 'MsureGr:', 
                     #     choices = unique(as.vector(hourly['MsureGr'])), 
                     #     selected = 'P AIR DESP',
                     #     multiple = FALSE, 
                     #     selectize = FALSE)
                     #   ),
                     width = 3,
                     title = "Selection of Sensor Groups",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                   )
            ),
            
            column(12,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:45px;padding-bottom:15px;"),
                       sliderInput("hsliderDate", 'Range of dates:',
                                   mindate, 
                                   maxdate, 
                                   c(initial, maxdate),
                                   step = 300),
                       sliderInput("hsliderScale", 'Horizon Color Scale:',
                                   min = 0, 
                                   max = 5, 
                                   value = 3, 
                                   step = 0.1)
                     ),
                     splitLayout(
                       cellArgs = list(style = "padding:0px;padding-bottom:0px;"),
                       selectInput(
                         inputId="hinputDate_start", 
                         label = HTML("Exclude date from:"), 
                         choices = c('select...'='NULL',unique(as.vector(hourly['date']))),
                         selected = NULL,
                         multiple = FALSE, 
                         selectize = FALSE,
                         width=200),
                       
                       selectInput(
                         inputId="hinputDate_end", 
                         label = HTML("to:"), 
                         choices = c('select...'='NULL',unique(as.vector(hourly['date']))),
                         selected = NULL,
                         multiple = FALSE, 
                         selectize = FALSE,
                         width=200),
                       
                       selectInput(
                         inputId = 'hrange', 
                         label = 'Day Window:', 
                         choices = c('3 day'=3, 
                                     '5 days'=5,
                                     '7 days'=7,
                                     '14 days'=14),
                         selected = 5,
                         multiple = FALSE, 
                         selectize = FALSE),
                       
                       selectInput(
                         inputId = 'hinterval',
                         label = 'Time aggregation:', 
                         choices = '*',
                         multiple = FALSE, 
                         selectize = FALSE)
                     ),
                     width = 12,
                     title = "Other Settings",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                   )
            ),
            column(width = 12,
                   h5("Percentage of Changes per Sensor ID"),
                   plotOutput("horizonplot", height=650)
            ),
            column(width = 4, 
                   h5("Sensor ID with Highest Percentage Change"),
                   DT::dataTableOutput("htable_high"),
                   h5("Sensor ID with Persistent Trend"),
                   DT::dataTableOutput("htable_count")
            )
          )
        ),
        ##########################################
        ########### controlchart-start ###########
        ##########################################
        tabItem(
          tabName = "controlchart",
          #h1("Control Chart - Moving Range"),
          fluidRow(
            column(2,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:5px;"),
                       selectInput(
                         inputId = 'cinterval', 
                         label = 'Interval:', 
                         choices = '*',
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px'
                         )
                       ),
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:5px;"),
                       radioButtons(
                         inputId = "qcVioNo", 
                         label = "Sigma:",
                         inline = TRUE,
                         selected = "3",
                         c("1" = "2", "2" = "3", "3" = "4") #"0"="1", 
                         )
                       ),
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:5px;"),
                       radioButtons(
                         inputId = "alphaNum", 
                         label = " Anomaly:",
                         inline = TRUE,
                         selected = "0.1",
                         c("1"="0.05","2" = "0.1", "3" = "0.2") #, "4" = "0.2"
                       )
                     ),
                     width = 16,
                     title = "Select Levels",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                     )
                   ),
            column(7,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:3px;"),
                       selectInput(
                         inputId = 'cID', 
                         label = 'Sensor :', 
                         choices = unique(as.vector(hourly['ID'])), 
                         selected = 'MBH40CT021_XQ60',
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px'
                       )
                     ),
                     splitLayout(
                       cellArgs = list(style = "padding-left:45px;padding-bottom:5px;"),
                       sliderInput("csliderDate", 'Period Range :',
                                   mindate, 
                                   maxdate, 
                                   c(mindate, maxdate),
                                   step = 300)
                       ),
                     splitLayout(
                       cellArgs = list(style = "padding:0px;padding-bottom:2px;"),
                       selectInput(
                         inputId="cinputDate_start", 
                         label = HTML("Exclude period from:"), 
                         choices = c('select...'='NULL',unique(as.vector(hourly['date']))),
                         selected = "2007-07-07 01:00:00",
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px'),
                       
                       selectInput(
                         inputId="cinputDate_end", 
                         label = HTML("to:"), 
                         choices = c('select...'='NULL',unique(as.vector(hourly['date']))),
                         selected = "2007-07-10 23:00:00",
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px')
                       ),
                     width = 16,
                     title = "Select ID & Period (1)",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                     )
                   ),
            column(7,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:3px;"),
                       selectInput(
                         inputId = 'cID2', 
                         label = 'Sensor :', 
                         choices = unique(as.vector(hourly['ID'])), 
                         selected = 'MBH40CT021_XQ60',
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px'
                       )
                     ),
                     splitLayout(
                       cellArgs = list(style = "padding-left:45px;padding-bottom:5px;"),
                       sliderInput("csliderDate2", 'Period Range :',
                                   mindate, 
                                   maxdate, 
                                   c(mindate, maxdate),
                                   step = 300)
                       ),
                     splitLayout(
                       cellArgs = list(style = "padding:0px;padding-bottom:2px;"),
                       selectInput(
                         inputId="cinputDate_start2", 
                         label = HTML("Exclude period from:"), 
                         choices = c('select...'='NULL',unique(as.vector(hourly['date']))),
                         selected = "2007-07-23 10:00:00",
                         multiple = FALSE, 
                         selectize = FALSE,
                         width=200),
                       
                       selectInput(
                         inputId="cinputDate_end2", 
                         label = HTML("to:"), 
                         choices = c('select...'='NULL',unique(as.vector(hourly['date']))),
                         selected = "2007-07-24 15:00:00",
                         multiple = FALSE, 
                         selectize = FALSE,
                         width=200)
                       ),
                     width = 16,
                     title = "Select ID & Period (2)",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                     )
                   ),
            column(width = 8,
                   plotlyOutput("mR", height = 200),
                   plotlyOutput("mmR", height = 100),
                   plotlyOutput("QCvio",height = 200),
                   plotlyOutput("Anom",height = 200)
                   ),
            column(width = 8,
                   plotlyOutput("mR2", height = 200),
                   plotlyOutput("mmR2", height = 100),
                   plotlyOutput("QCvio2",height = 200),
                   plotlyOutput("Anom2",height = 200)
                   )
            )
          )
        ##########################################
        )
      )
    )        
        

server <- shinyServer(function(input, output, session) {
  #################################################################
  # [Parallel Plot]                                               #
  #################################################################
  
  # Select time interval
  observe({
    #files <- list.files(path='data/')
    updateSelectizeInput(session = session, 
                         inputId = 'hinterval_pc', 
                         choices = c('Every 5 mins'='per_5min.csv',
                                     'Every hour'='per_hour.csv',
                                     'Every day'='per_day.csv'
                         ),
                         select = 'per_hour.csv')
  })
  
  output$parallelplot <- renderPlotly({
    # Dynamic load data
    data  <- read_csv(sprintf("data/%s",input$hinterval_pc), locale=locale(tz="Singapore"))
    
    # Aggregate time
    incl_start <- input$hsliderDate_pc[1]
    incl_end <- input$hsliderDate_pc[2]
    excl_start <- input$hinputDate_start_pc
    excl_end <- input$hinputDate_end_pc
    ## check input for exclude dates
    if(input$hinputDate_start_pc == 'NULL' | input$hinputDate_end_pc == 'NULL') {
      data <- data[data$date > incl_start & data$date <= incl_end,]
    }
    else{
      data <- data[data$date > incl_start & data$date <= incl_end,]
      data <- data[!(data$date > excl_start & data$date <= excl_end),]
    }
    
    # Filter time range
    data <- data[data$FunSys_item==input$hFunSys_item_pc,]
    
    # Convert date into unix time
    unixtime <- as.numeric(as.POSIXct(data$date, format="%Y-%m-%d %H:%M:%S"))
    unix1 <- as.numeric(as.POSIXct(input$hsliderDate2_pc, format="%Y-%m-%d %H:%M:%S"))
    unix2 <- as.numeric(as.POSIXct(input$hsliderDate2_pc+(86400*as.numeric(input$hrange_pc)), format="%Y-%m-%d %H:%M:%S"))
    
    # Plot Parallel Coordinates
    if(input$hFunSys_item_pc == '40') {
      trace1 <- list(
        line = list(color = unixtime,
                    colorscale='Jet',
                    showscale=TRUE,
                    reversescale=TRUE),
        type = 'parcoords',
        frame = "Null", 
        dimensions = list(
          list(range = c(min(unixtime), max(unixtime)),
               label="UNIX_Time",
               values  = unixtime,
               constraintrange = c(unix1,unix2)),
          list(range = c(min(data$AP1),max(data$AP1)),
               label = 'AP1', 
               values = data$AP1),
          list(range = c(min(data$DISK11),max(data$DISK11)),
               label = 'DISK11', 
               values = data$DISK11),
          list(range = c(min(data$DISK12),max(data$DISK12)),
               label = 'DISK12', 
               values = data$DISK12),
          list(range = c(min(data$End_Face),max(data$End_Face)),
               label = 'End Face', 
               values = data$End_Face),
          list(range = c(min(data$L1),max(data$L1)),
               label = 'L1', 
               values = data$L1),
          list(range = c(min(data$L2),max(data$L2)),
               label = 'L2', 
               values = data$L2),
          list(range = c(min(data$L2oo3),max(data$L2oo3)),
               label = 'L2oo3', 
               values = data$L2oo3),
          list(range = c(min(data$MBH40GD010),max(data$MBH40GD010)),
               label = 'MBH40GD010', 
               values = data$MBH40GD010),
          list(range = c(min(data$HP1),max(data$HP1)),
               label = 'HP1', 
               values = data$HP1),
          list(range = c(min(data$HPCLR_1),max(data$HPCLR_1)),
               label = 'HPCLR_1', 
               values = data$HPCLR_1),
          list(range = c(min(data$HPCLR_2),max(data$HPCLR_2)),
               label = 'HPCLR_2', 
               values = data$HPCLR_2),
          list(range = c(min(data$HPCLR_3),max(data$HPCLR_3)),
               label = 'HPCLR_3', 
               values = data$HPCLR_3)
        )
      )
      layout <- list(
        margin = list(
          b = 40,
          l = 60,
          r = 10
        ),
        hovermode = "closest",
        showlegend = FALSE
      )}
    
    if(input$hFunSys_item_pc == '21') {
      trace1 <- list(
        line = list(color = unixtime,
                    colorscale='Jet',
                    showscale=TRUE,
                    reversescale=TRUE),
        type = 'parcoords',
        frame = "Null", 
        dimensions = list(
          list(range = c(min(unixtime), max(unixtime)),
               label="UNIX_Time",
               values  = unixtime,
               constraintrange = c(unix1,unix2)),
          list(range = c(min(data$L),max(data$L)),
               label = 'L',
               values = data$L),
          list(range = c(min(data$R),max(data$R)),
               label = 'R',
               values = data$R)
        )
      )
      layout <- list(
        margin = list(
          b = 40,
          l = 60,
          r = 10
        ),
        hovermode = "closest",
        showlegend = FALSE
      )}
   
    if(input$hFunSys_item_pc == '22') {
      trace1 <- list(
        line = list(color = unixtime,
                    colorscale='Jet',
                    showscale=TRUE,
                    reversescale=TRUE),
        type = 'parcoords',
        frame = "Null", 
        dimensions = list(
          list(range = c(min(unixtime), max(unixtime)),
               label="UNIX_Time",
               values  = unixtime,
               constraintrange = c(unix1,unix2)),
          list(range = c(min(data$H11),max(data$H11)),
               label = '11H',
               values = data$H11),
          list(range = c(min(data$H2),max(data$H2)),
               label = '2H',
               values = data$H2),
          list(range = c(min(data$H5),max(data$H5)),
               label = '5H',
               values = data$H5),
          list(range = c(min(data$H8),max(data$H8)),
               label = '8H',
               values = data$H8)
        )
      )
      layout <- list(
        margin = list(
          b = 40,
          l = 60,
          r = 10
        ),
        hovermode = "closest",
        showlegend = FALSE
      )} 
    
    if(input$hFunSys_item_pc == '50') {
      trace1 <- list(
        line = list(color = unixtime,
                    colorscale='Jet',
                    showscale=TRUE,
                    reversescale=TRUE),
        type = 'parcoords',
        frame = "Null", 
        dimensions = list(
          list(range = c(min(unixtime), max(unixtime)),
               label="UNIX_Time",
               values  = unixtime,
               constraintrange = c(unix1,unix2)),
          list(range = c(min(data$T1),max(data$T1)),
               label = 'T1',
               values = data$T1),
          list(range = c(min(data$T3),max(data$T3)),
               label = 'T3',
               values = data$T3)
        )
      )
      layout <- list(
        margin = list(
          b = 40,
          l = 60,
          r = 10
        ),
        hovermode = "closest",
        showlegend = FALSE
      )}
    
    p <- plot_ly()
    p <- add_trace(p, line=trace1$line, type=trace1$type, frame=trace1$frame, dimensions=trace1$dimensions)
    p <- layout(p, margin=layout$margin, hovermode=layout$hovermode, showlegend=layout$showlegend)
  })
  
  
  #################################################################
  # [Horizon graph]                                               #
  #################################################################
  observe({
    # Select time aggregation
    updateSelectizeInput(session = session, 
                         inputId = 'hinterval', 
                         choices = c('Every 5 mins'='five_min.csv',
                                     'Every hour'='hourly.csv',
                                     'Every day'='daily.csv'),
                         select = 'hourly.csv')
    
    # Select day window
    val <- as.numeric(input$hrange)
    updateSliderInput(session, "hsliderDate", value = c(initial, initial+(val*86400)),
                      min = mindate, max = maxdate, step = 300)
  })
  
  hdata <- reactive ({
    hdata_temp <- read_csv(sprintf("data/%s",input$hinterval), locale=locale(tz="Singapore"))
    hdata_temp <- as.data.frame(hdata_temp)
    
    # Select by FunSys_item and MsureGr
    #hdata <- hdata[hdata$MsureGr==input$hMsureGr,]
    hdata_temp <- hdata_temp[hdata_temp$FunSys_item==input$hFunSys_item,]
    
    # Select required columns
    hdata_temp <- select(hdata_temp, date, ID, Value)
    
    # Replace null under Value column with zero
    #hdata$Value[is.na(hdata$Value)] <- 0
    
    # Calculate percentage of changes
    hdata_temp <- change(hdata_temp, Var = 'Value', GroupVar = 'ID',
                         type = 'percent',
                         NewVar = 'PercentChange',
                         slideBy = -1)
    
    # Spreads into multiple columns
    ## assign row_number (remove Value if percentage of changes exist)
    hdata_temp <- hdata_temp %>% 
      group_by(ID) %>% 
      mutate(grouped_id = row_number()) %>%
      select(-Value)
    ## spread and remove grouped_id
    hdata_temp <- hdata_temp %>% 
      spread(ID, PercentChange) %>% 
      select(-grouped_id)
    
    
    # Aggregate time
    incl_start <- input$hsliderDate[1]
    incl_end <- input$hsliderDate[2]
    excl_start <- input$hinputDate_start
    excl_end <- input$hinputDate_end
    ## check input for exclude dates
    if(input$hinputDate_start == 'NULL' | input$hinputDate_end == 'NULL') {
      hdata_temp <- hdata_temp[hdata_temp$date > incl_start & hdata_temp$date <= incl_end,]
    }
    else{
      hdata_temp <- hdata_temp[hdata_temp$date > incl_start & hdata_temp$date <= incl_end,]
      hdata_temp <- hdata_temp[!(hdata_temp$date > excl_start & hdata_temp$date <= excl_end),]
    }
    
    # Add empty top rows
    #for (i in 1:216){
    #  hdata_temp <- hdata_temp[c(1,seq(nrow(hdata_temp))),]
    #  hdata_temp[1,1] <- hdata_temp[1,1]-(hdata_temp[3,1]-hdata_temp[2,1])
    #  hdata_temp[1,-1] <- NA
    #}
    
    # Moving average
    #n=3
    #hdata <- hdata %>% 
    #  mutate_all(funs(rollapplyr(., FUN = mean, width = n, fill = NA, partial = TRUE)))
    
    # Convert dataframe to xts
    hdata_temp <- as.xts(hdata_temp[,-1], order.by=as.POSIXct(hdata_temp$date,format='%Y-%m-%d %H:%M:%S'))
    
    return(hdata_temp)
  })
  
  hdata2 <- reactive ({
    hdata_temp <- read_csv(sprintf("data/%s",input$hinterval), locale=locale(tz="Singapore"))
    hdata_temp <- as.data.frame(hdata_temp)
    
    # Select by FunSys_item and MsureGr
    #hdata <- hdata[hdata$MsureGr==input$hMsureGr,]
    hdata_temp <- hdata_temp[hdata_temp$FunSys_item==input$hFunSys_item,]
    
    # Select required columns
    hdata_temp <- select(hdata_temp, date, ID, Value)
    
    # Replace null under Value column with zero
    #hdata$Value[is.na(hdata$Value)] <- 0
    
    # Calculate percentage of changes
    hdata_temp <- change(hdata_temp, Var = 'Value', GroupVar = 'ID',
                         type = 'percent',
                         NewVar = 'PercentChange',
                         slideBy = -1)
    
    # Aggregate time
    incl_start <- input$hsliderDate[1]
    incl_end <- input$hsliderDate[2]
    excl_start <- input$hinputDate_start
    excl_end <- input$hinputDate_end
    ## check input for exclude dates
    if(input$hinputDate_start == 'NULL' | input$hinputDate_end == 'NULL') {
      hdata_temp <- hdata_temp[hdata_temp$date > incl_start & hdata_temp$date <= incl_end,]
    }
    else{
      hdata_temp <- hdata_temp[hdata_temp$date > incl_start & hdata_temp$date <= incl_end,]
      hdata_temp <- hdata_temp[!(hdata_temp$date > excl_start & hdata_temp$date <= excl_end),]
    }
    
    hdata_temp <- select(hdata_temp, date, ID, PercentChange)
    
    return(hdata_temp)
  })
  
  # Plotting
  output$horizonplot <- renderPlot({
    
    # Plot horizon chart
    horizonplot(hdata(),
                #specifying scale of each color segment
                horizonscale = input$hsliderScale,
                #specifying baseline y value for the first (positive) segment (i.e. the value at which red changes to blue)
                origin = 0,
                #setting color scale bar
                colorkey = TRUE,
                #standard horizon and setting horizontal white grid lines
                panel = function(x, ...) {
                  panel.horizonplot(x, ...)
                  #h = 3 (3 horizontal grid lines)
                  #v = 0 (no vertical grid lines)
                  panel.grid(h=0, v=0,col = "gray70", lwd=1,lty = 3)
                },
                #setting panel border
                par.settings=theEconomist.theme(box = "gray70"),
                #setting the strip to left
                strip.left = FALSE,
                #setting number of columns and rows
                layout = c(1,ncol(hdata())),
                #setting ticks and axis
                scales = list(tck = c(1,0), y = list(tck = c(0,1), draw = FALSE, relation = "same", alternating = 2)),
                #setting labels
                xlab = NULL,
                ylab = list(rev(colnames(hdata())), rot = 0, cex = 0.8, pos = 3),
                col.regions=brewer.pal(n=8, 'RdBu'),
                main = NULL)
  })
  
  output$htable_high = DT::renderDataTable({
    hdata_high <- hdata2()
    hdata_high <- hdata_high %>%
      mutate(Sign = ifelse(PercentChange > 0, "Positive", ifelse(PercentChange < 0, "Negative", "Zero"))) %>%  # get the sign of the value
      ungroup
    hdata_high["PercentChange"] <-round(hdata_high["PercentChange"],3)
    hdata_high["date"] <- format(hdata_high$date, "%Y-%m-%d\n%H:%M:%S")
    hdata_high <- select(hdata_high, "ID", "date", "Sign", "PercentChange")
    
    hdata_pos <- hdata_high[order(as.numeric(as.character(hdata_high$PercentChange)),decreasing = TRUE),]
    hdata_pos <- hdata_pos[ !duplicated(hdata_pos$ID), ]
    
    hdata_neg <- hdata_high[order(as.numeric(as.character(hdata_high$PercentChange)),decreasing = FALSE),]
    hdata_neg <- hdata_neg[ !duplicated(hdata_neg$ID), ]
    
    hdata_table <- rbind(head(hdata_pos, 3), head(hdata_neg, 3))
    
    DT::datatable(hdata_table,
                  colnames = c('Sensor ID', 'Timestamp (end)', 'Trend', 'Percent Change(%)'),
                  options = list(initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                 pageLength = 10, 
                                 dom = 't'),
                  rownames= FALSE) %>%
      formatStyle('Sign', 
                  color = 'white',
                  backgroundColor = styleEqual(c('Positive', 'Negative'), c('#2166ac', '#b2182b'))) %>%
      formatStyle(columns = c(1, 2, 3), 
                  fontSize = '85%') %>%
      formatStyle(0, target= 'row', lineHeight='70%')
  })
  
  output$htable_count = DT::renderDataTable({
    hdata_count <- hdata2()
    hdata_count <- hdata_count %>%
      mutate(Sign = ifelse(PercentChange > 0, "Positive", ifelse(PercentChange < 0, "Negative", "Zero"))) %>%  # get the sign of the value
      ungroup
    
    hdata_count <- hdata_count[order(hdata_count["ID"]), ]
    hdata_count <- transform(hdata_count, Count = ave(Sign, rleid(ID, Sign), FUN = seq_along))
    hdata_count["date"] <- format(hdata_count$date, "%Y-%m-%d\n%H:%M:%S")
    hdata_count <- hdata_count[order(as.numeric(as.character(hdata_count$Count)),decreasing = TRUE),]
    hdata_count <- select(hdata_count, "ID", "date", "Sign", "Count")
    
    hdata_pos <- hdata_count[hdata_count["Sign"] == "Positive",]
    hdata_pos <- hdata_pos[ !duplicated(hdata_pos$ID), ]
    
    hdata_neg <- hdata_count[hdata_count["Sign"] == "Negative",]
    hdata_neg <- hdata_neg[ !duplicated(hdata_neg$ID), ]
    
    hdata_table <- rbind(head(hdata_pos, 3), head(hdata_neg, 3))
    
    DT::datatable(hdata_table,
                  colnames = c('Sensor ID', 'Timestamp (end)', 'Trend', 'Consecutive Count'),
                  options = list(initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '12px'});}"),
                                 pageLength = 10, 
                                 dom = 't'),
                  rownames= FALSE) %>%
      formatStyle('Sign', 
                  color = 'white',
                  backgroundColor = styleEqual(c('Positive', 'Negative'), c('#2166ac', '#b2182b'))) %>%
      formatStyle(columns = c(1, 2, 3), 
                  fontSize = '85%') %>%
      formatStyle(0, target= 'row', lineHeight='70%')
  })
  
  
  #################################################################
  # [Control Chart]                                               #
  #################################################################

  observe({
    #files <- list.files(path='data/')
    updateSelectizeInput(session = session, 
                         inputId = 'cinterval', 
                         choices = c('5 min'='five_min.csv',
                                     'Hourly'='hourly.csv',
                                     'Daily'='daily.csv'
                         ),
                         select = 'hourly.csv')
  })
  
  ## Period 1
  output$mR <- renderPlotly({
    
    cdata <- read_csv(sprintf("data/%s",input$cinterval), locale=locale(tz="Singapore"))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID,]
    
    # create grouping columns
    cdata$day <- as.Date(cdata$date) #cdata$Hr  = hour(cdata$date)  
    
    # Select required columns
    cdata <- select(cdata,date,day,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
    # Aggregate time
    cincl_start <- input$csliderDate[1]
    cincl_end <- input$csliderDate[2]
    cexcl_start <- input$cinputDate_start
    cexcl_end <- input$cinputDate_end
    
    # check input for exclude dates
    if(input$cinputDate_start == 'NULL' | input$cinputDate_end == 'NULL') {
      cdata <- cdata[cdata$date > cincl_start & cdata$date <= cincl_end,]
    }
    else{
      cdata <- cdata[cdata$date > cincl_start & cdata$date <= cincl_end,]
      cdata <- cdata[!(cdata$date > cexcl_start & cdata$date <= cexcl_end),]
    }
    
    # Compute Control Chart Parameter
    the_Mean <- mean(cdata$Value)
    the_mR <- mean(abs(diff(cdata$Value)))
    the_Sigma <- the_mR/1.128
    the_LCL <- the_Mean - 3 * the_Sigma
    the_UCL <- the_Mean + 3 * the_Sigma
    sd_LCL <- the_Mean - 3* sd(cdata$Value)
    sd_UCL <- the_Mean + 3* sd(cdata$Value)
    Rng <- (sd_UCL-sd_LCL)/6
    
    # Plot control chart
    xBar <- ggplot(cdata, aes(x=date, y = Value, group=day))+
      geom_point(color="black",alpha=0.9,size=0.8) + geom_line(color="black",alpha=0.7)+
      geom_hline(yintercept = the_LCL, color ="red", alpha=0.75,size=0.5)+
      geom_hline(yintercept = the_Mean, color ="black", alpha=0.90,size=0.3,linetype="longdash")+
      geom_hline(yintercept = the_UCL, color ="red", alpha=0.75,size=0.5)+
      geom_hline(yintercept = sd_LCL, color ="blue", alpha=0.75,size=0.5)+
      geom_hline(yintercept = sd_UCL, color ="blue", alpha=0.75,size=0.5)+
      theme_minimal()+
      ggtitle("ID & Period (1)")+
      #ylim(sd_LCL-0.5,sd_UCL+0.5)+
      theme(plot.title = element_text(size = 10),
            axis.title.x=element_blank(),axis.text.x=element_blank(),
            axis.title.y=element_text(color="black",size=9),axis.text.y=element_text(color="black",size=8,angle=0))

    ggplotly(xBar)

  })
  
  
  output$mmR <- renderPlotly({
    
    cdata <- read_csv(sprintf("data/%s",input$cinterval), locale=locale(tz="Singapore"))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID,]
    
    # create grouping columns
    cdata$day <- as.Date(cdata$date)  #cdata$Hr  = hour(cdata$date)
    
    # Select required columns
    cdata <- select(cdata,date,day,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
    # Aggregate time
    cincl_start <- input$csliderDate[1]
    cincl_end <- input$csliderDate[2]
    cexcl_start <- input$cinputDate_start
    cexcl_end <- input$cinputDate_end
    ## check input for exclude dates
    if(input$cinputDate_start == 'NULL' | input$cinputDate_end == 'NULL') {
      cdata <- cdata[cdata$date > cincl_start & cdata$date <= cincl_end,]
    }
    else{
      cdata <- cdata[cdata$date > cincl_start & cdata$date <= cincl_end,]
      cdata <- cdata[!(cdata$date > cexcl_start & cdata$date <= cexcl_end),]
    }
    
    # Compute Control Chart Parameter
    the_mRm <- mean(abs(diff(cdata$Value)))
    mR_LCLm <- 0
    mR_UCLm <- 3.267*the_mRm
    
    # Calculate mR changes
    cdata <- change(cdata, Var = 'Value', GroupVar = 'ID', TimeVar = 'date',
                     type = 'absolute',
                     NewVar = 'MovR',
                     slideBy = -1)
    cdata$MovR <- abs(cdata$MovR) #change diff to absolute
    
    # Plot control chart
    xBarm <- ggplot(cdata, aes(x=date, y = MovR, group=day))+
      geom_point(color="black",alpha=0.9,size=0.6) + geom_line(color="black",size=0.6) +
      geom_hline(yintercept = mR_LCLm,color="red", alpha=0.5, size=0.5)+
      geom_hline(yintercept = mR_UCLm, color = "red", alpha=0.5, size=0.5)+
      #ylim(-0.25,mR_UCLm+0.25)+
      theme_minimal()+
      theme(axis.title.x=element_text(color="black",size=8),axis.text.x=element_text(color="black",size=8,angle=0),
            axis.title.y=element_text(color="black",size=9),axis.text.y=element_text(color="black",size=7,angle=0))
    
    ggplotly(xBarm)
  })
  
  
  output$QCvio <- renderPlotly({
    cdata <- read_csv(sprintf("data/%s",input$cinterval), locale=locale(tz="Singapore"))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID,]
    
    # create grouping columns
    cdata$day <- as.Date(cdata$date)  #cdata$Hr  = hour(cdata$date)
    
    ## Select required columns
    cdata <- select(cdata,date,day,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
    ## Aggregate time
    cincl_start <- input$csliderDate[1]
    cincl_end <- input$csliderDate[2]
    cexcl_start <- input$cinputDate_start
    cexcl_end <- input$cinputDate_end
    
    ## check input for exclude dates
    if(input$cinputDate_start == 'NULL' | input$cinputDate_end == 'NULL') {
      cdata <- cdata[cdata$date > cincl_start & cdata$date <= cincl_end,]
    }
    else{
      cdata <- cdata[cdata$date > cincl_start & cdata$date <= cincl_end,]
      cdata <- cdata[!(cdata$date > cexcl_start & cdata$date <= cexcl_end),]
    }
    
    p <- input$qcVioNo
    QC_Violations <- ggplot(cdata, aes(x = date, y = Value))+ 
      stat_qc_violations(method = "XmR", point.size = 0.7, show.facets = c(p:p))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
            axis.text.y=element_text(color="black",size=6,angle=0),axis.text.x=element_blank())
    
    ggplotly(QC_Violations)
    
  })
  
  
  output$Anom <- renderPlotly({
    cdata <- read_csv(sprintf("data/%s",input$cinterval), locale=locale(tz="Singapore"))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID,]

    # Select required columns for renaming
    cdata <- select(cdata,date,ID,Value)%>% rename( package =ID, count =Value)
    
    # Aggregate time
    cincl_start <- input$csliderDate[1]
    cincl_end <- input$csliderDate[2]
    cexcl_start <- input$cinputDate_start
    cexcl_end <- input$cinputDate_end
    ## check input for exclude dates
    if(input$cinputDate_start == 'NULL' | input$cinputDate_end == 'NULL') {
      cdata <- cdata[cdata$date > cincl_start & cdata$date <= cincl_end,]
    }
    else{
      cdata <- cdata[cdata$date > cincl_start & cdata$date <= cincl_end,]
      cdata <- cdata[!(cdata$date > cexcl_start & cdata$date <= cexcl_end),]
    }
    
    cdata <- tbl_df(cdata) #need to change to tbl_df for time_decompose
    
    alphaNum <- input$alphaNum #set alpha lvl
    
    anomalized <- cdata %>%
      time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
      anomalize(remainder, method = "iqr", alpha = as.numeric(alphaNum), max_anoms = 0.2) %>%
      time_recompose() %>%
      plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5, alpha_ribbon = 0.5,size_dots = 1.0,size_circles = 1.0)
    #+ggtitle("Anomaly - IQR")
    a<-anomalized+theme(legend.position="none")+
      #theme_minimal()+theme(legend.title = element_blank())
      theme(axis.title.x=element_text(color="black",size=8),axis.title.y=element_text(color="black",size=10),
            axis.text.x=element_text(color="black",size=8),axis.text.y=element_text(color="black",size=9))
    
    ggplotly(a)
  })  
  
  
  
  ##### Period 2
  
  output$mR2 <- renderPlotly({
    
    cdata <- read_csv(sprintf("data/%s",input$cinterval), locale=locale(tz="Singapore"))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID2,] #change to cID2
    
    # create grouping columns
    cdata$day <- as.Date(cdata$date)  
    
    # Select required columns
    cdata2 <- select(cdata,date,day,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
    # Aggregate time
    cincl_start2 <- input$csliderDate2[1]
    cincl_end2 <- input$csliderDate2[2]
    cexcl_start2 <- input$cinputDate_start2
    cexcl_end2 <- input$cinputDate_end2
    
    # check input for exclude dates
    if(input$cinputDate_start2 == 'NULL' | input$cinputDate_end2 == 'NULL') {
      cdata2 <- cdata2[cdata2$date > cincl_start2 & cdata2$date <= cincl_end2,]
    }
    else{
      cdata2 <- cdata2[cdata2$date > cincl_start2 & cdata2$date <= cincl_end2,]
      cdata2 <- cdata2[!(cdata2$date > cexcl_start2 & cdata2$date <= cexcl_end2),]
    }
    
    # Compute Control Chart Parameter
    the_Mean2 <- mean(cdata2$Value)
    the_mR2 <- mean(abs(diff(cdata2$Value)))
    the_Sigma2 <- the_mR2/1.128
    the_LCL2 <- the_Mean2 - 3 * the_Sigma2
    the_UCL2 <- the_Mean2 + 3 * the_Sigma2
    sd_LCL2 <- the_Mean2 - 3* sd(cdata2$Value)
    sd_UCL2 <- the_Mean2 + 3* sd(cdata2$Value)
    Rng2 <- (sd_UCL2-sd_LCL2)/6
    
    # Plot control chart
    xBar2 <- ggplot(cdata2, aes(x=date, y=Value, group=day)) +
      geom_point(color="black",alpha=0.9,size=0.8) + geom_line(color="black",alpha=0.7) +
      #stat_QC(method="mR",auto.label = T)+
      #stat_mR(auto.label = T)+
      ggtitle("ID & Period (2)")+
      geom_hline(yintercept = the_LCL2, color ="red", alpha=0.75,size=0.5)+
      geom_hline(yintercept = the_Mean2, color ="black", alpha=0.90,size=0.3,linetype="longdash")+
      geom_hline(yintercept = the_UCL2, color="red" , alpha=0.75,size=0.5)+
      geom_hline(yintercept =  sd_LCL2, color="blue", alpha=0.75,size=0.5)+
      geom_hline(yintercept = sd_UCL2, color= "blue", alpha=0.75,size=0.5)+
      theme_minimal()+
      #ylim(sd_LCL2-0.5,sd_UCL2+0.5)+
      theme(plot.title = element_text(size = 10),
            axis.title.x=element_blank(),axis.text.x=element_blank(),
            axis.title.y=element_text(color="black",size=9),axis.text.y=element_text(color="black",size=8,angle=0))
    
    ggplotly(xBar2)
  })
  
  
  output$mmR2 <- renderPlotly({
    
    cdata <- read_csv(sprintf("data/%s",input$cinterval), locale=locale(tz="Singapore"))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID2,] #change to cID2
    
    # create grouping columns
    cdata$day <- as.Date(cdata$date)  
    
    # Select required columns
    cdata2 <- select(cdata,date,day,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
    # Aggregate time
    cincl_start2 <- input$csliderDate2[1]
    cincl_end2 <- input$csliderDate2[2]
    cexcl_start2 <- input$cinputDate_start2
    cexcl_end2 <- input$cinputDate_end2
    ## check input for exclude dates
    if(input$cinputDate_start2 == 'NULL' | input$cinputDate_end2 == 'NULL') {
      cdata2 <- cdata2[cdata2$date > cincl_start2 & cdata2$date <= cincl_end2,]
    }
    else{
      cdata2 <- cdata2[cdata2$date > cincl_start2 & cdata2$date <= cincl_end2,]
      cdata2 <- cdata2[!(cdata2$date > cexcl_start2 & cdata2$date <= cexcl_end2),]
    }
    
    # Compute Control Chart Parameter
    the_mRm2 <- mean(abs(diff(cdata2$Value)))
    mR_LCLm2 <- 0
    mR_UCLm2 <- 3.267*the_mRm2
    
    # Calculate mR changes
    cdata2 <- change(cdata2, Var = 'Value', GroupVar = 'ID', TimeVar = 'date',
                     type = 'absolute',
                     NewVar = 'MovR',
                     slideBy = -1)
    cdata2$MovR <- abs(cdata2$MovR) #change diff to absolute
    
    # Plot control chart
    xBarm2 <- ggplot(cdata2, aes(x=date, y = MovR, group=day)) +
      geom_point(color="black",alpha=0.9,size=0.6) + geom_line(color="black",size=0.6) +
      geom_hline(yintercept = mR_LCLm2,color="red", alpha=0.5, size=0.5)+
      geom_hline(yintercept = mR_UCLm2,color="red", alpha=0.5, size=0.5)+
      #ylim(-0.25,mR_UCLm2+0.25)+
      theme_minimal()+
      theme(axis.title.x=element_text(color="black",size=8),axis.text.x=element_text(color="black",size=8,angle=0),
            axis.title.y=element_text(color="black",size=9),axis.text.y=element_text(color="black",size=7,angle=0))
    
    ggplotly(xBarm2)
  })
  
  
  output$QCvio2 <- renderPlotly({
    cdata <- read_csv(sprintf("data/%s",input$cinterval), locale=locale(tz="Singapore"))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID2,] #change to cID2
    
    # create grouping columns
    cdata$day <- as.Date(cdata$date)  #cdata$Hr = hour(cdata$date)
    
    # Select required columns
    cdata2 <- select(cdata,date,day,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
    # Aggregate time
    cincl_start2 <- input$csliderDate2[1]
    cincl_end2 <- input$csliderDate2[2]
    cexcl_start2 <- input$cinputDate_start2
    cexcl_end2 <- input$cinputDate_end2
    ## check input for exclude dates
    if(input$cinputDate_start2 == 'NULL' | input$cinputDate_end2 == 'NULL') {
      cdata2 <- cdata2[cdata2$date > cincl_start2 & cdata2$date <= cincl_end2,]
    }
    else{
      cdata2 <- cdata2[cdata2$date > cincl_start2 & cdata2$date <= cincl_end2,]
      cdata2 <- cdata2[!(cdata2$date > cexcl_start2 & cdata2$date <= cexcl_end2),]
    }
    
    p <- input$qcVioNo #from global input
    
    QC_Violations2 <- ggplot(cdata2, aes(x = date, y = Value)) + 
      stat_qc_violations(method = "XmR", point.size = 0.7, show.facets = c(p:p))+
      theme_minimal()+
      theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
            axis.text.y=element_text(color="black",size=6,angle=0),axis.text.x=element_blank())
    
    ggplotly(QC_Violations2)
  })
  

  output$Anom2 <- renderPlotly({
    cdata <- read_csv(sprintf("data/%s",input$cinterval), locale=locale(tz="Singapore"))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID2,] #change to cID2
    
    # Select required columns for renaming
    cdata2 <- select(cdata,date,ID,Value)%>% rename( package =ID, count =Value)
    
    # Aggregate time
    cincl_start2 <- input$csliderDate2[1]
    cincl_end2 <- input$csliderDate2[2]
    cexcl_start2 <- input$cinputDate_start2
    cexcl_end2 <- input$cinputDate_end2
    ## check input for exclude dates
    if(input$cinputDate_start2 == 'NULL' | input$cinputDate_end2 == 'NULL') {
      cdata2 <- cdata2[cdata2$date > cincl_start2 & cdata2$date <= cincl_end2,]
    }
    else{
      cdata2 <- cdata2[cdata2$date > cincl_start2 & cdata2$date <= cincl_end2,]
      cdata2 <- cdata2[!(cdata2$date > cexcl_start2 & cdata2$date <= cexcl_end2),]
    }
    
    cdata2 <- tbl_df(cdata2) #need to change to tbl_df for time_decompose
    
    alphaNum <- input$alphaNum #set alpha lvl
    
    anomalized2 <- cdata2 %>%
      time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
      anomalize(remainder, method = "iqr", alpha = as.numeric(alphaNum), max_anoms = 0.2) %>%
      time_recompose() %>%
      plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5, alpha_ribbon = 0.5,size_dots = 1.0,size_circles = 1.0)
      #+ggtitle("Anomaly - IQR")
    a2<-anomalized2+theme(legend.position="none")+
      #theme_minimal()+theme(legend.title = element_blank())+
      theme(axis.title.x=element_text(color="black",size=8),axis.title.y=element_text(color="black",size=10),
            axis.text.x=element_text(color="black",size=8),axis.text.y=element_text(color="black",size=9))
    
    ggplotly(a2)
  })  
  
  
  
}) # end for shinyServer(

shinyApp(ui, server)
