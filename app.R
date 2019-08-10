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
              'anomalize')

for (p in packages) {
  if(!require(p, character.only = T)) {
    install.packages(p)
  }
  library(p, character.only = T)
}

options(shiny.maxRequestSize = 30*1024^2)

# Load processed data (for choices under selectInput)
hourly <- read_csv("./data/hourly.csv")
per_hour <- read_csv("./data/per_hour.csv")


# Extract time range (for min and max under sliderInput)
#mindate = as.Date("2007-07-01 00:00:00","%Y-%m-%d %H:%M:%S")
#maxdate = as.Date("2007-08-01 00:00:00","%Y-%m-%d %H:%M:%S")
mindate = min(hourly$date, na.rm = TRUE)
maxdate = max(hourly$date, na.rm = TRUE)

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
                       cellArgs = list(style = "padding-left:0px;padding-bottom:67px;"),
                       selectInput(
                         inputId = 'hFunSys_item_pc',
                         label = 'Function System:',
                         choices = unique(as.vector(per_hour['FunSys_item'])),
                         selected = '01',
                         multiple = FALSE,
                         selectize = FALSE)
                       ),
                       # selectInput(
                       #   inputId = 'hUnit_pc',
                       #   label = 'Unit:',
                       #   choices = unique(as.vector(hourly['Unit'])),
                       #   selected = 'degC',
                       #   multiple = FALSE,
                       #   selectize = TRUE),
                     width = 3,
                     title = "Selection of Sensor Groups",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                      )
                  ),

            column(12,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:45px;padding-bottom:15px;"),
                       sliderInput("hsliderDate_pc", 'Range of dates:',
                                   mindate,
                                   maxdate,
                                   c(mindate, maxdate),
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
                         inputId = 'hinterval_pc',
                         label = 'Time Interval:',
                         choices = '*',
                         multiple = FALSE,
                         selectize = FALSE,
                         width=100)
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
            column(3,
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
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:0px;"),
                       selectInput(
                         inputId = 'hMsureGr', 
                         label = 'MsureGr:', 
                         choices = unique(as.vector(hourly['MsureGr'])), 
                         selected = 'T AFT HPCLR',
                         multiple = FALSE, 
                         selectize = FALSE)
                       ),
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
                                   c(mindate, maxdate),
                                   step = 300),
                       sliderInput("hsliderScale", 'Horizon Color Scale:',
                                   min = 0, 
                                   max = 5, 
                                   value = 0.1, 
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
                         inputId = 'hinterval', 
                         label = 'Time Interval:', 
                         choices = '*',
                         multiple = FALSE, 
                         selectize = FALSE,
                         width=100)
                     ),
                     width = 12,
                     title = "Other Settings",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                   )
            ),
            #column(width = 10,
            #       horizonOutput("horizonplot", height=350)
            
            #),
            column(width = 15,
                   plotOutput("horizonplot2", height=650)
                   
            #),
            #column(width = 16,
            #       plotOutput("horizonplot3", height=650)
                   
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
            column(3,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:3px;"),
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
                       cellArgs = list(style = "padding-left:0px;padding-bottom:3px;"),
                       radioButtons(
                         inputId = "qcVioNo", 
                         label = " Violation Sigma:",
                         inline = TRUE,
                         selected = "1",
                         c("within"="1", "1" = "2", "2" = "3", "3" = "4") #
                         )
                       ),
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:3px;"),
                       radioButtons(
                         inputId = "alphaNum", 
                         label = " Anomaly lvl:",
                         inline = TRUE,
                         selected = "0.025",
                         c("1"="0.025","2" = "0.05", "3" = "0.1", "4" = "0.2")
                       )
                     ),
                     width = 16,
                     title = "Select Frequency/ Sigma",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                     )
                   ),
            column(6,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:2px;"),
                       selectInput(
                         inputId = 'cID', 
                         label = 'Sensor ID 1:', 
                         choices = unique(as.vector(hourly['ID'])), 
                         selected = 'MBD21CY001_XQ60', #'10CT001_XQ60',
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px'
                       )
                     ),
                     splitLayout(
                       cellArgs = list(style = "padding-left:45px;padding-bottom:10px;"),
                       sliderInput("csliderDate", 'Range of Period (ID1):',
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
                         selected = NULL,
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px'),
                       
                       selectInput(
                         inputId="cinputDate_end", 
                         label = HTML("to:"), 
                         choices = c('select...'='NULL',unique(as.vector(hourly['date']))),
                         selected = NULL,
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px')
                       ),
                     width = 16,
                     title = "ID 1",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                     )
                   ),
            column(6,
                   box(
                     splitLayout(
                       cellArgs = list(style = "padding-left:0px;padding-bottom:2px;"),
                       selectInput(
                         inputId = 'cID2', 
                         label = 'Sensor ID 2:', 
                         choices = unique(as.vector(hourly['ID'])), 
                         selected = 'MBD21CY001_XQ60', #'10CT001_XQ60',
                         multiple = FALSE, 
                         selectize = FALSE,
                         width = '180px'
                       )
                     ),
                     splitLayout(
                       cellArgs = list(style = "padding-left:45px;padding-bottom:10px;"),
                       sliderInput("csliderDate2", 'Range of Period (ID2):',
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
                         selected = NULL,
                         multiple = FALSE, 
                         selectize = FALSE,
                         width=200),
                       
                       selectInput(
                         inputId="cinputDate_end2", 
                         label = HTML("to:"), 
                         choices = c('select...'='NULL',unique(as.vector(hourly['date']))),
                         selected = NULL,
                         multiple = FALSE, 
                         selectize = FALSE,
                         width=200)
                       ),
                     width = 16,
                     title = "ID 2",
                     color = "teal", ribbon = TRUE, title_side = "top left"
                     )
                   ),
            column(width = 8,
                   plotlyOutput("mR", height=350),
                   plotlyOutput("mmR", height=200),
                   plotlyOutput("QCvio",height = 300),
                   plotlyOutput("Anom",height = 300)
                   ),
            column(width = 8,
                   plotlyOutput("mR2", height=350),
                   plotlyOutput("mmR2", height=200),
                   plotlyOutput("QCvio2",height = 300),
                   plotlyOutput("Anom2",height = 300)
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
    data  <- read_csv(sprintf("data/%s",input$hinterval_pc))
    
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
    
    # Plot Parallel Coordinates
    trace1 <- list(
      line = list(color = data$X1,
                  colorscale='Jet',
                  showscale=TRUE,
                  reversescale=TRUE),
      type = 'parcoords',
      frame = "Null",
      dimensions = list(
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
             values = data$H8),
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
             values = data$HPCLR_3),
        list(range = c(min(data$L),max(data$L)),
             label = 'L', 
             values = data$L),
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
        list(range = c(min(data$R),max(data$R)),
             label = 'R', 
             values = data$R),
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
    )
    
    ## TODO: Make axis readable 
    p <- plot_ly()
    p <- add_trace(p, line=trace1$line, type=trace1$type, frame=trace1$frame, dimensions=trace1$dimensions)
    p <- layout(p, margin=layout$margin, hovermode=layout$hovermode, showlegend=layout$showlegend)
  })
  
  
  #################################################################
  # [Horizon graph]                                               #
  #################################################################
  # Select time interval
  observe({
    #files <- list.files(path='data/')
    updateSelectizeInput(session = session, 
                         inputId = 'hinterval', 
                         choices = c('Every 5 mins'='five_min.csv',
                                     'Every hour'='hourly.csv',
                                     'Every day'='daily.csv'
                                     ),
                         select = 'hourly.csv')
    })
  
  # Plotting
  
  renderHorizon <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    htmlwidgets::shinyRenderWidget(expr, horizonOutput, env, quoted = TRUE)
  }
  
  output$horizonplot <- renderHorizon({
    
    hdata <- read_csv(sprintf("data/%s",input$hinterval))
    hdata <- as.data.frame(hdata)
    
    # Select by FunSys_item and MsureGr
    hdata <- hdata[hdata$MsureGr==input$hMsureGr,]
    hdata <- hdata[hdata$FunSys_item==input$hFunSys_item,]
    
    # Select required columns
    hdata <- select(hdata, date, ID, Value)
    
    # Replace null under Value column with zero
    #hdata$Value[is.na(hdata$Value)] <- 0
    
    # Calculate percentage of changes
    hdata <- change(hdata, Var = 'Value', GroupVar = 'ID',
                    type = 'percent',
                    NewVar = 'PercentChange',
                    slideBy = -1)
    
    # Spreads into multiple columns
    ## assign row_number (remove Value if percentage of changes exist)
    hdata <- hdata %>% 
      group_by(ID) %>% 
      mutate(grouped_id = row_number()) %>%
      select(-Value)
    ## spread and remove grouped_id
    hdata <- hdata %>% 
      spread(ID, PercentChange) %>% 
      select(-grouped_id)
    
    # Aggregate time
    incl_start <- input$hsliderDate[1]
    incl_end <- input$hsliderDate[2]
    excl_start <- input$hinputDate_start
    excl_end <- input$hinputDate_end
    ## check input for exclude dates
    if(input$hinputDate_start == 'NULL' | input$hinputDate_end == 'NULL') {
      hdata <- hdata[hdata$date > incl_start & hdata$date <= incl_end,]
    }
    else{
      hdata <- hdata[hdata$date > incl_start & hdata$date <= incl_end,]
      hdata <- hdata[!(hdata$date > excl_start & hdata$date <= excl_end),]
    }
    
    # Moving average
    #n=3
    #df07_2s <- df07_2s %>% 
    #  mutate_all(funs(rollapplyr(., FUN = mean, width = n, fill = NA, partial = TRUE)))
    
    # Convert dataframe to xts
    #hdata <- as.xts(hdata[,-1], order.by=as.POSIXct(hdata$date,format='%Y-%m-%d %H:%M:%S'))
    
    # Plot horizon chart
    horizon <- function(dates, df, date_format = "%Y-%m-%d",
                        digits = NULL, width = NULL, height = NULL,
                        axis_height = 30, axis_ticks=4, padding=15,
                        colors=NULL, tick_format=NULL, focus_format=NULL)
    {
      lab <- colnames(df)
      if(is.null(lab))
        lab <- paste0("col", 1:ncol(df))
      
      if(!is.data.frame(df))
        df <- as.data.frame(df)
      
      stopifnot(length(dates) == nrow(df))
      
      df <- as.list(df)
      names(df) <- NULL
      
      if(is.null(colors))
        colors <- c("#08519c", "#3182bd", "#6baed6", "#bdd7e7",
                    "#bae4b3", "#74c476", "#31a354", "#006d2c")
      if(length(colors) %% 2 != 0)
        stop("length(colors) must be even")
      
      if(is.null(digits)) {
        digits <- ceiling(log10(diff(range(unlist(df), na.rm=TRUE))))
        digits <- 4 - digits
        digits <- ifelse(digits < 0, 0, digits)
      }
      
      x = list(dates=dates, labels=lab, data=df, date_format=date_format,
               chartOpts=list(height=height, axis_height=axis_height,
                              axis_ticks=axis_ticks, colors=colors,
                              digits=digits, padding=padding,
                              tick_format=tick_format,
                              focus_format=focus_format))
      
      # create widget
      htmlwidgets::createWidget(
        name = 'horizon',
        x,
        width = width,
        height = height,
        sizingPolicy=htmlwidgets::sizingPolicy(
          padding=padding,
          browser.defaultWidth=800,
          browser.defaultHeight=600,
          browser.padding = padding,
          knitr.defaultWidth=800,
          knitr.defaultHeight=600,
          viewer.padding = padding),
        package = 'horizon'
      )
    }
    
    
    t<- parse_date_time(hdata$date, "%Y-%m-%d %H:%M:%S" )
    t<- factor(t, labels=format(hdata$date,"%Y-%m-%d %H:%M:%S"), ordered=TRUE)
    
    horizon(t, hdata[,-1], date_format = "%Y-%m-%d %H:%M:%S", digits = 3,
            width = NULL, height = NULL, axis_height = 30, axis_ticks = 15,
            padding = 0, 
            #colors = c("#420000", "#7a0000", "#9c0808", "#bd3131", "#d66b6b", "#e7bdbd",
            #           "#bdd7e7", "#6baed6", "#3182bd", "#08519c", "#003c7a", "#002142"), 
            colors = c("#7a0000", "#9c0808", "#bd3131", "#d66b6b", "#e7bdbd",
                       "#bdd7e7", "#6baed6", "#3182bd", "#08519c", "#003c7a"), 
            tick_format = "%m-%d", focus_format = "%Y-%m-%d %H:%M:%S")
  })
  
  output$horizonplot2 <- renderPlot({
    
    hdata <- read_csv(sprintf("data/%s",input$hinterval))
    hdata <- as.data.frame(hdata)
    
    # Select by FunSys_item and MsureGr
    hdata <- hdata[hdata$MsureGr==input$hMsureGr,]
    hdata <- hdata[hdata$FunSys_item==input$hFunSys_item,]
    
    # Select required columns
    hdata <- select(hdata, date, ID, Value)
    
    # Replace null under Value column with zero
    #hdata$Value[is.na(hdata$Value)] <- 0
    
    # Calculate percentage of changes
    hdata <- change(hdata, Var = 'Value', GroupVar = 'ID',
                    type = 'percent',
                    NewVar = 'PercentChange',
                    slideBy = -1)
    
    # Spreads into multiple columns
     ## assign row_number (remove Value if percentage of changes exist)
    hdata <- hdata %>% 
      group_by(ID) %>% 
      mutate(grouped_id = row_number()) %>%
      select(-Value)
     ## spread and remove grouped_id
    hdata <- hdata %>% 
      spread(ID, PercentChange) %>% 
      select(-grouped_id)
    
    # Aggregate time
    incl_start <- input$hsliderDate[1]
    incl_end <- input$hsliderDate[2]
    excl_start <- input$hinputDate_start
    excl_end <- input$hinputDate_end
    ## check input for exclude dates
    if(input$hinputDate_start == 'NULL' | input$hinputDate_end == 'NULL') {
      hdata <- hdata[hdata$date > incl_start & hdata$date <= incl_end,]
      }
    else{
      hdata <- hdata[hdata$date > incl_start & hdata$date <= incl_end,]
      hdata <- hdata[!(hdata$date > excl_start & hdata$date <= excl_end),]
      }
      
    # Moving average
    #n=3
    #df07_2s <- df07_2s %>% 
    #  mutate_all(funs(rollapplyr(., FUN = mean, width = n, fill = NA, partial = TRUE)))
    
    # Convert dataframe to xts
    hdata <- as.xts(hdata[,-1], order.by=as.POSIXct(hdata$date,format='%Y-%m-%d %H:%M:%S'))
    
    # Plot horizon chart
    horizonplot(hdata,
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
                  panel.grid(h=3, v=0,col = "gray70", lwd=1,lty = 3)
                },
                #setting panel border
                par.settings=theEconomist.theme(box = "gray70"),
                #setting the strip to left
                strip.left = FALSE,
                #setting number of columns and rows
                layout = c(1,ncol(hdata)),
                #setting ticks and axis
                scales = list(tck = c(1,0), y = list(tck = c(0,1), draw = FALSE, relation = "same", alternating = 2)),
                #setting labels
                xlab = NULL,
                ylab = list(rev(colnames(hdata)), rot = 0, cex = 0.8, pos = 3),
                col.regions=brewer.pal(n=8, 'RdBu'),
                main = NULL)
  })

  output$horizonplot3 <- renderPlot({
    
    hdata <- read_csv(sprintf("data/%s",input$hinterval))
    hdata <- as.data.frame(hdata)
    
    # Select by FunSys_item and MsureGr
    hdata <- hdata[hdata$MsureGr==input$hMsureGr,]
    hdata <- hdata[hdata$FunSys_item==input$hFunSys_item,]
    
    # Select required columns
    hdata <- select(hdata, date, ID, Value)
    
    # Replace null under Value column with zero
    #hdata$Value[is.na(hdata$Value)] <- 0
    
    # Calculate percentage of changes
    hdata <- change(hdata, Var = 'Value', GroupVar = 'ID',
                    type = 'percent',
                    NewVar = 'PercentChange',
                    slideBy = -1)
    
    # Spreads into multiple columns
    ## assign row_number (remove Value if percentage of changes exist)
    hdata <- hdata %>% 
      group_by(ID) %>% 
      mutate(grouped_id = row_number()) %>%
      select(-Value)
    ## spread and remove grouped_id
    hdata <- hdata %>% 
      spread(ID, PercentChange) %>% 
      select(-grouped_id)
    
    # Aggregate time
    incl_start <- input$hsliderDate[1]
    incl_end <- input$hsliderDate[2]
    excl_start <- input$hinputDate_start
    excl_end <- input$hinputDate_end
    ## check input for exclude dates
    if(input$hinputDate_start == 'NULL' | input$hinputDate_end == 'NULL') {
      hdata <- hdata[hdata$date > incl_start & hdata$date <= incl_end,]
    }
    else{
      hdata <- hdata[hdata$date > incl_start & hdata$date <= incl_end,]
      hdata <- hdata[!(hdata$date > excl_start & hdata$date <= excl_end),]
    }
    
    # Moving average
    #n=3
    #df07_2s <- df07_2s %>% 
    #  mutate_all(funs(rollapplyr(., FUN = mean, width = n, fill = NA, partial = TRUE)))
    
    # Convert dataframe to xts
    hdata <- as.xts(hdata[,-1], order.by=as.POSIXct(hdata$date,format='%Y-%m-%d %H:%M:%S'))
    
    require(RColorBrewer)
    require(quantmod)
    require(PerformanceAnalytics)
    
    x <- na.omit(hdata)
    
    #get some decent colors from RColorBrewer
    #we will use colors on the edges so 2:4 for red and 7:9 for blue
    col.brew <- (brewer.pal(name="RdBu",n=11))
    
    #get this to ease using it later
    n<-nrow(x)
    
    #set scale to be 10%
    horizonscale=input$hsliderScale
    
    
    dd = dim(x)
    
    #now let's do it with a loop and flip the negative up
    nbands = ceiling(max(abs(coredata(x)))/horizonscale)
    
    
    layout(matrix(c(1:(dd[2]+2)), ncol = 1, byrow = TRUE))
    
    for(j in 1:dd[2]){
      par(mar=c(0,8,0,8),cex=0.8, col="grey")
      plot(index(x[,j]),
           coredata(x[,j]),
           type="n",
           #bty="n",
           las=1,
           xaxt="n",
           yaxt="n",
           xlab=NA,
           ylab=NA,
           ylim=c(-horizonscale,horizonscale))
      par(usr=c(index(x[,j])[1],index(x[,j])[n],0,horizonscale))
      
      for (i in 1:nbands) {
        #draw positive
        polygon(
          c(index(x[,j])[1], index(x[,j]), index(x[,j])[n]),
          c(0, coredata(x[,j]) - (i-1) * horizonscale,0),
          col=col.brew[length(col.brew)-nbands+i-1],
          border=NA
        )
        #draw negative
        polygon(
          c(index(x[,j])[1], index(x[,j]), index(x[,j])[n]),
          c(0, -coredata(x[,j]) - (i-1) * horizonscale, 0),
          col=col.brew[nbands-i+1],
          border=NA
        )
        par(las=1,col="grey") 
        mtext(colnames(x[,j])[i], side = 2, line = 0.2, srt = -90, cex = 0.6, col="black")
      }
    }
    ix <- seq(min(time(x)), max(time(x)), by="1 day")
    par(mar=c(0,8,0,8),cex=0.8, col="grey")
    axis(side = 1, at = ix, labels = format(ix, "%b %d"), col="grey")
    par(mar=c(0,8,3.5,8),cex=0.6, col="grey")
    
    image(x = 0:10, y = 1, z = matrix(data = 0:10,), 
          col=brewer.pal(name="RdBu",n=11),
          axes=F, 
          xlab='', 
          ylab='')
    axis(1, at=c(0,10), labels = c('Negative %', 'Positive %'))
    
    
    # thanks https://www.r-bloggers.com/horizon-plots-in-base-graphics/
  })
  
  #################################################################
  # [Control Chart]                                               #
  #################################################################

  observe({
    #files <- list.files(path='data/')
    updateSelectizeInput(session = session, 
                         inputId = 'cinterval', 
                         choices = c('Every 5 mins'='five_min.csv',
                                     'Every hour'='hourly.csv',
                                     'Every day'='daily.csv'
                         ),
                         select = 'hourly.csv')
  })
  
  ## Period 1
  output$mR <- renderPlotly({
    
    cdata <- read_csv(sprintf("data/%s",input$cinterval))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID,]
    
    # create grouping columns
    cdata$Mth  = month(cdata$date)
    cdata$Day  = day(cdata$date)
    cdata$Hr  = hour(cdata$date)
    cdata$dy <- as.Date(cdata$date)  
    
    # Select required columns
    cdata <- select(cdata,date,dy,Mth,Day,Hr,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
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
    
    #mR_LCL <- 0
    #mR_UCL <- 3.267*the_mR
    
    Rng <- (sd_UCL-sd_LCL)/6
    
    # Plot control chart
    xBar <- ggplot(cdata, aes(x=date, y = Value)) +
      geom_point(color="black",alpha=0.9,size=0.8) + geom_line(color="black",alpha=0.7) +
      #stat_QC(method="mR",auto.label = T)+
      #stat_mR(auto.label = T)+
      ggtitle("Period 1")+
      geom_hline(yintercept = the_LCL, color ="red", alpha=0.75,size=0.5)+
      geom_hline(yintercept = the_Mean, color ="black", alpha=0.90,size=0.3,linetype="longdash")+
      geom_hline(yintercept = the_UCL, color ="red", alpha=0.75,size=0.5)+
      geom_hline(yintercept = sd_LCL, color ="blue", alpha=0.75,size=0.5)+
      geom_hline(yintercept = sd_UCL, color ="blue", alpha=0.75,size=0.5)
      #geom_hline(yintercept = mR_LCL,color="red", alpha=0.5, size=0.4)+
      #geom_hline(yintercept = mR_UCL, color = "red", alpha=0.5, size=0.4)
      #ylim(the_Mean-4*sd(cdata$Value),the_Mean+4*sd(cdata$Value))
    
    ggplotly(xBar)

  })
  
  
  output$mmR <- renderPlotly({
    
    cdata <- read_csv(sprintf("data/%s",input$cinterval))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID,]
    
    # create grouping columns
    cdata$Mth  = month(cdata$date)
    cdata$Day  = day(cdata$date)
    cdata$Hr  = hour(cdata$date)
    cdata$dy <- as.Date(cdata$date)  
    
    # Select required columns
    cdata <- select(cdata,date,dy,Mth,Day,Hr,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
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
    xBarm <- ggplot(cdata, aes(x=date, y = MovR)) +
      geom_point(color="black",alpha=0.9,size=1) + geom_line(color="black") +
      geom_hline(yintercept = mR_LCLm,color="red", alpha=0.5, size=0.5)+
      geom_hline(yintercept = mR_UCLm, color = "red", alpha=0.5, size=0.5)+
      ylim(-0.25,mR_UCLm+1)
    
    ggplotly(xBarm)
  })
  
  
  output$QCvio <- renderPlotly({
    cdata <- read_csv(sprintf("data/%s",input$cinterval))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID,]
    # create grouping columns
    cdata$Mth  = month(cdata$date)
    cdata$Day  = day(cdata$date)
    cdata$Hr  = hour(cdata$date)
    cdata$dy <- as.Date(cdata$date)  
    ## Select required columns
    cdata <- select(cdata,date,dy,Mth,Day,Hr,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
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
    QC_Violations <- ggplot(cdata, aes(x = date, y = Value)) + 
      stat_qc_violations(method = "XmR", point.size = 1.0, show.facets = c(p:p))
    
    ggplotly(QC_Violations)
    
  })
  
  
  output$Anom <- renderPlotly({
    cdata <- read_csv(sprintf("data/%s",input$cinterval))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID,] #change to cID2
    # create grouping columns
    #cdata$Mth  = month(cdata$date)
    #cdata$Day  = day(cdata$date)
    #cdata$Hr  = hour(cdata$date)
    #cdata$dy <- as.Date(cdata$date)  
    
    # Select required columns for renaming
    cdata <- select(cdata,date,ID,Value)%>% #dy,Mth,Day,Hr,FunSys_item, MsureGr, MsureGr_lvl
      rename( package =ID, count =Value)
    
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
      plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5, alpha_ribbon = 0.5,size_dots = 1.5,size_circles = 1.5)
    #+ggtitle("Anomaly - IQR")
    a<-anomalized+theme(legend.position="none")#+theme(legend.title = element_blank())
    
    ggplotly(a)
  })  
  
  
  
  ##### Period 2
  
  output$mR2 <- renderPlotly({
    
    cdata <- read_csv(sprintf("data/%s",input$cinterval))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID2,] #change to cID2
    
    # create grouping columns
    cdata$Mth  = month(cdata$date)
    cdata$Day  = day(cdata$date)
    cdata$Hr  = hour(cdata$date)
    cdata$dy <- as.Date(cdata$date)  
    
    # Select required columns
    cdata2 <- select(cdata,date,dy,Mth,Day,Hr,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
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
    
    #mR_LCL2 <- 0
    #mR_UCL2 <- 3.267*the_mR2
    
    Rng2 <- (sd_UCL2-sd_LCL2)/6
    
    # Plot control chart
    xBar2 <- ggplot(cdata2, aes(x=date, y = Value)) +
      geom_point(color="black",alpha=0.95,size=0.8) + geom_line(color="black",alpha=0.7) +
      #stat_QC(method="mR",auto.label = T)+
      #stat_mR(auto.label = T)+
      ggtitle("Period 2")+
      geom_hline(yintercept = the_LCL2,color="red", alpha=0.75,size=0.5)+
      geom_hline(yintercept = the_Mean2, color ="black", alpha=0.90,size=0.3,linetype="longdash")+
      geom_hline(yintercept = the_UCL2, color = "red", alpha=0.75,size=0.5)+
      geom_hline(yintercept =  sd_LCL2 ,color="blue", alpha=0.75,size=0.5)+
      geom_hline(yintercept = sd_UCL2, color = "blue", alpha=0.75,size=0.5)
      #geom_hline(yintercept = mR_LCL2,color="red", alpha=0.5, size=0.4)+
      #geom_hline(yintercept = mR_UCL2, color = "red", alpha=0.5, size=0.4)
      #ylim(the_Mean2-4*sd(cdata2$Value),the_Mean2+4*sd(cdata2$Value))
    
    ggplotly(xBar2)
  })
  
  
  output$mmR2 <- renderPlotly({
    
    cdata <- read_csv(sprintf("data/%s",input$cinterval))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID2,] #change to cID2
    
    # create grouping columns
    cdata$Mth  = month(cdata$date)
    cdata$Day  = day(cdata$date)
    cdata$Hr  = hour(cdata$date)
    cdata$dy <- as.Date(cdata$date)  
    
    # Select required columns
    cdata2 <- select(cdata,date,dy,Mth,Day,Hr,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
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
    xBarm2 <- ggplot(cdata2, aes(x=date, y = MovR)) +
      geom_point(color="black",alpha=0.9,size=1) + geom_line(color="black") +
      geom_hline(yintercept = mR_LCLm2,color="red", alpha=0.5, size=0.5)+
      geom_hline(yintercept = mR_UCLm2, color = "red", alpha=0.5, size=0.5)+
      ylim(-0.25,mR_UCLm2+1)
    
    ggplotly(xBarm2)
  })
  
  
  output$QCvio2 <- renderPlotly({
    cdata <- read_csv(sprintf("data/%s",input$cinterval))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID2,] #change to cID2
    
    # create grouping columns
    cdata$Mth  = month(cdata$date)
    cdata$Day  = day(cdata$date)
    cdata$Hr  = hour(cdata$date)
    cdata$dy <- as.Date(cdata$date)  
    
    # Select required columns
    cdata2 <- select(cdata,date,dy,Mth,Day,Hr,ID,Value,FunSys_item, MsureGr, MsureGr_lvl)
    
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
    
    p <- input$qcVioNo
    
    QC_Violations2 <- ggplot(cdata2, aes(x = date, y = Value)) + 
      stat_qc_violations(method = "XmR", point.size = 1.0, show.facets = c(p:p))    
    
    ggplotly(QC_Violations2)
  })
  

  output$Anom2 <- renderPlotly({
    cdata <- read_csv(sprintf("data/%s",input$cinterval))
    cdata <- as.data.frame(cdata)
    
    # Select by FunSys_item and Unit
    cdata <- cdata[cdata$ID==input$cID2,] #change to cID2
    
    # create grouping columns
    #cdata$Mth  = month(cdata$date)
    #cdata$Day  = day(cdata$date)
    #cdata$Hr  = hour(cdata$date)
    #cdata$dy <- as.Date(cdata$date)  
    
    # Select required columns for renaming
    cdata2 <- select(cdata,date,ID,Value)%>% #dy,Mth,Day,Hr,FunSys_item, MsureGr, MsureGr_lvl
          rename( package =ID, count =Value)
    
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
      plot_anomalies(time_recomposed = TRUE, alpha_dots = 0.5, alpha_ribbon = 0.5,size_dots = 1.5,size_circles = 1.5)
      #+ggtitle("Anomaly - IQR")
    a2<-anomalized2+theme(legend.position="none")#+theme(legend.title = element_blank())
    
    ggplotly(a2)
  })  
  
  
  
}) # end for shinyServer(

shinyApp(ui, server)

