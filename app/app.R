library(shiny)
library(bs4Dash)
library(vroom)
library(lubridate)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(tsibble)


source("functions/function_eda_preprocess.R")


###################################################################
################                 UI                ################
###################################################################

sidebar <- bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "Solar Forecast",
    brandColor = "primary",
    url = "https://akshajverma.com",
    src = "https://akshajverma.com/images/akshaj_profile.jpg",
    elevation = 3,
    opacity = 0.8,
    bs4SidebarUserPanel(
        img = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
        text = "About"
    ),
    bs4SidebarMenu(
        bs4SidebarHeader("Exploratory Analysis"),
        bs4SidebarMenuItem("Time Series Visualization", tabName = "ExploratoryAnalysis_SimpleViz", icon = "sliders"),
        bs4SidebarMenuItem("Seasonality", tabName = "ExploratoryAnalysis_Seasonality", icon = "sliders"),
        bs4SidebarHeader("Feature Engineering"),
        bs4SidebarHeader("Modelling"),
        bs4SidebarMenuItem("Univariate", tabName = "item1", icon = "sliders"),
        bs4SidebarMenuItem("Multivariate", tabName = "item2", icon = "sliders")
    )
)



navbar <- bs4DashNavbar()

# controlbar <- bs4DashControlbar()

footer <- bs4DashFooter(
    copyrights = a(
        href = "https://twitter.com/theairbend3r", 
        target = "_blank", "@theairbend3r"
    ),
    right_text = "2020"
)


body <- bs4DashBody(
    bs4TabItems(
        bs4TabItem(
            tabName = "ExploratoryAnalysis_SimpleViz",
            fluidRow(
                bs4InfoBox(title = "Number of rows", width = 4, value = 100),
                bs4InfoBox(title = "Numbebr of columns", width = 4, value = 12),
                bs4InfoBox(title = "Dataset Size", width = 4, value = "10Mb")
            ),
            fluidRow(
                bs4Card(
                    width = 4, title = "Configuration", closable = FALSE, solidHeader = TRUE, maximizable = FALSE,
                    selectizeInput("ExploratoryAnalysis_SimpleViz_Input_Column", label = "Select Features", multiple = FALSE, choices = c()),
                    selectizeInput("ExploratoryAnalysis_SimpleViz_Input_Granularity", label = "Select Granularity", multiple = FALSE, selected = "Monthly", choices = c("Hourly", "Daily", "Monthly", "Yearly")),
                    dateRangeInput("ExploratoryAnalysis_SimpleViz_Input_DateRange", label = "Select Date Range", autoclose = TRUE)
                ),
                bs4Card(
                    width = 8, title = "Plots", closable = FALSE, solidHeader = TRUE, maximizable = TRUE,
                    withSpinner(plotOutput("ExploratoryAnalysis_SimpleViz_Output_Plots"))
                )
            )
        ),
        bs4TabItem(
            tabName = "ExploratoryAnalysis_Seasonality"
        )
    )
)



ui <- bs4DashPage(
    old_school = FALSE,
    sidebar_min = TRUE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = FALSE,
    controlbar_overlay = TRUE,
    title = "Solar Forecast",
    navbar = navbar,
    sidebar = sidebar,
    # controlbar = controlbar,
    footer = footer,
    body = body
)


###################################################################
################             SERVER                ################
###################################################################
server <- function(input, output, session) {
    solar_df <- vroom(file = "../data/solar_data.csv", delim = ",", progress = TRUE)
    solar_df$date_time <- ymd_hms(solar_df$date_time)


    updateSelectizeInput(session, "ExploratoryAnalysis_SimpleViz_Input_Column", choices = names(solar_df))
    updateDateRangeInput(session, "ExploratoryAnalysis_SimpleViz_Input_DateRange", start = min(solar_df$date_time), end = max(solar_df$date_time))
    
    
    plotTimeSeriesCols <- function(df, col_name) {
        col_name <- enquo(col_name)
        
        df %>%
            ggplot(mapping = aes(x = date_time, y = !! col_name))
    }
    
    output$ExploratoryAnalysis_SimpleViz_Output_Plots <- renderPlot({
        df <- preprocessEDA(df = solar_df,
                            granularity = input$ExploratoryAnalysis_SimpleViz_Input_Granularity,
                            date_start = input$ExploratoryAnalysis_SimpleViz_Input_DateRange[1],
                            date_end = input$ExploratoryAnalysis_SimpleViz_Input_DateRange[2]
                            )
        df %>%
            ggplot(mapping = aes(x = date_time, y = .data[[input$ExploratoryAnalysis_SimpleViz_Input_Column]])) + geom_line()
        # plotTimeSeriesCols(solar_df, col_name = input$ExploratoryAnalysis_SimpleViz_Input_Column)
    })   
}




###################################################################
################              PAGE                 ################
###################################################################
shiny::shinyApp(
    ui = ui,
    server = server
)