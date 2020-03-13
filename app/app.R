library(shiny)
library(bs4Dash)
library(vroom)
library(lubridate)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(tsibble)
library(DT)
library(shinyWidgets)


source("functions/function_eda_preprocess.R")
source("functions/function_mmyyyy_dateinput.R")

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
        bs4SidebarHeader("Forecast"),
        bs4SidebarMenuItem("Univariate", tabName = "Forecast_Univariate", icon = "sliders"),
        bs4SidebarMenuItem("Multivariate", tabName = "Forecast_Multivariate", icon = "sliders")
    )
)



navbar <- bs4DashNavbar()

controlbar <- bs4DashControlbar(
    skin = "light",
    title = "My right sidebar",
    inputId =
    sliderInput(
        inputId = "obs",
        label = "Number of observations:",
        min = 0,
        max = 1000,
        value = 500
    )
)

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
                bs4Card(
                    width = 4, title = "Configuration", closable = FALSE, solidHeader = TRUE, maximizable = FALSE,
                    selectizeInput("ExploratoryAnalysis_SimpleViz_Input_Column", label = "Select Features", multiple = FALSE, choices = c(" ")),
                    selectizeInput("ExploratoryAnalysis_SimpleViz_Input_Granularity", label = "Select Granularity", multiple = FALSE, selected = "Monthly", choices = c("Hourly", "Daily", "Monthly", "Yearly")),
                    # dateRangeInput("ExploratoryAnalysis_SimpleViz_Input_DateRange", format = "mm/yyyy" , label = "Select Date Range", autoclose = TRUE),
                    dateRangeInput2("ExploratoryAnalysis_SimpleViz_Input_DateRange", "Select Date Range", startview = "year", minview = "months", maxview = "decades")
                ),
                bs4Card(
                    width = 8, title = "Plots", closable = FALSE, solidHeader = TRUE, maximizable = TRUE,
                    withSpinner(plotOutput("ExploratoryAnalysis_SimpleViz_Output_Plots"))
                )
            ),
            fluidRow(
                bs4Card(
                    width = 12, title = "Dataframe", closable = FALSE, solidHeader = TRUE, maximizable = FALSE,
                    DTOutput("ExploratoryAnalysis_SimpleViz_Output_DF")
                )
            )
        ),
        bs4TabItem(
            tabName = "Forecast_Univariate",
            fluidRow(
                bs4Card(
                    width = 4, title = "Train Configuration", closable = FALSE, solidHeader = TRUE, maximizable = FALSE,
                    selectizeInput("Forecast_Univariate_Input_TargetVariable", label = "Select Target", multiple = FALSE, choices = c(" ")),
                    selectizeInput("Forecast_Univariate_Input_Model", label = "Select Model", multiple = FALSE, selected = "Average", choices = c("Average", "Naive", "Seasonal Naive", "Drift")),
                    # dateRangeInput("Forecast_Univariate_Input_DateRange", label = "Select Date Range", autoclose = TRUE),
                    dateRangeInput2("Forecast_Univariate_Input_DateRange", "Select Date Range", startview = "year", minview = "months", maxview = "decades"),
                    actionButton("Forecast_Univariate_Button_Train", label = "Train")
                ),
                bs4Card(
                    width = 8, title = "Plots", closable = FALSE, solidHeader = TRUE, maximizable = TRUE,
                    withSpinner(plotOutput("Forecast_Univariate_Train_Output_Plots"))
                )
            )

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
    # navbar = navbar,
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

    
    #=============================================================
    #               EDA PAGE
    #=============================================================

    updateSelectizeInput(session, "ExploratoryAnalysis_SimpleViz_Input_Column", choices = names(solar_df)[-which(names(solar_df) %in% c("date_time", "id"))] )
    updateDateRangeInput(session, "ExploratoryAnalysis_SimpleViz_Input_DateRange", start = min(solar_df$date_time), end = max(solar_df$date_time))


    output$ExploratoryAnalysis_SimpleViz_Output_Plots <- renderPlot({
        req(solar_df)
        df <- preprocessEDA(df = solar_df,
                            granularity = input$ExploratoryAnalysis_SimpleViz_Input_Granularity,
                            date_start = input$ExploratoryAnalysis_SimpleViz_Input_DateRange[1],
                            date_end = input$ExploratoryAnalysis_SimpleViz_Input_DateRange[2]
                            )
        df %>%
            ggplot(mapping = aes(x = date_time, y = .data[[input$ExploratoryAnalysis_SimpleViz_Input_Column]])) +
            geom_line(aes(color = .data[[input$ExploratoryAnalysis_SimpleViz_Input_Column]])) +
            ggtitle(input$ExploratoryAnalysis_SimpleViz_Input_Column) +
            theme_light() +
            theme(legend.position = "none")
    })



    output$ExploratoryAnalysis_SimpleViz_Output_DF <- renderDataTable({
        req(solar_df)
    }, options = list(scrollX = TRUE, pageLength = 5))
    
    
    #=============================================================
    #               FORECAST UNIVARIATE
    #=============================================================
    
    
    solar_tsbl
    trained_model <- reactive({
        
    })
    
    
    
    
    
    
}




###################################################################
################              PAGE                 ################
###################################################################
shiny::shinyApp(
    ui = ui,
    server = server
)