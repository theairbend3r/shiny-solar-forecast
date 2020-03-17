library(shiny)
library(bs4Dash)

library(vroom)
library(lubridate)
library(dplyr)
library(ggplot2)
library(shinycssloaders)

library(DT)
library(shinyWidgets)

library(tsibble)
library(feasts)
library(fable)

# library(future)
# library(promises)
# 
# plan(multisession)
theme_set(theme_light())

source("functions/function_eda_preprocess.R")
source("functions/function_mmyyyy_dateinput.R")

###################################################################
################                 UI                ################
###################################################################

sidebar <- bs4DashSidebar(
    skin = "dark",
    status = "primary",
    title = "Solar Forecast",
    brandColor = "primary",
    url = "https://akshajverma.com",
    src = "https://akshajverma.com/images/akshaj_profile.jpg",
    elevation = 3,
    opacity = 0.8,
    # bs4SidebarUserPanel(
    #     img = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
    #     # url = "https://github.com/theairbend3r/shiny-solar-forecast",
    #     text = "About"
    # ),
    bs4SidebarMenu(
        bs4SidebarMenuItem("Home", tabName = "Home", icon = "home"),
        bs4SidebarMenuItem("Time Series Visualization", tabName = "ExploratoryAnalysis_SimpleViz", icon = "search"),
        bs4SidebarMenuItem("Forecast", tabName = "Forecast_Univariate", icon = "bolt")
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
            tabName = "Home",
            bs4Card(
                width = 12, title = "Home", closable = FALSE, solidHeader = FALSE, maximizable = FALSE, elevation = TRUE, collapsible = FALSE,
                h2("Solar Forecast"),
                p("Exploration and Univariate Prediction of Solar Irradiance using Statistical Time Series Methods."),
                p("The data was taken from a Solar Farm.")
            )
         ),
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
                    dataTableOutput("ExploratoryAnalysis_SimpleViz_Output_DF")
                )
            )
        ),
        bs4TabItem(
            tabName = "Forecast_Univariate",
            fluidRow(
                bs4Card(
                    width = 4, title = "Train Configuration", closable = FALSE, solidHeader = TRUE, maximizable = FALSE,
                    selectizeInput("Forecast_Univariate_Input_TargetVariable", label = "Select Target", multiple = FALSE, choices = c(" ")),
                    selectizeInput("Forecast_Univariate_Input_Model", label = "Select Model", multiple = FALSE, selected = "Average", choices = c("ARIMA", "NNETAR", "Seasonal Naive", "Average", "Naive")),
                    selectInput("Forecast_Univariate_Input_ForecastGranularity", "Granularity", choices = c("Day", "Month", "Year")),
                    numericInput("Forecast_Univariate_Input_ForecastHorizon", "Forecast Horizon", value = 12, min = 0),
                    dateRangeInput("Forecast_Univariate_Input_UniverseDateRange", label = "Select Universe", autoclose = TRUE),
                    dateInput("Forecast_Univariate_Input_TrainTestSplitDate", label = "Select Date for Train-Test Split", autoclose = TRUE),
                    
                    # dateRangeInput2("Forecast_Univariate_Input_TrainDateRange", "Select Train Subset", startview = "year", minview = "months", maxview = "decades"),
                    # dateRangeInput2("Forecast_Univariate_Input_TestDateRange", "Select Test Subset", startview = "year", minview = "months", maxview = "decades"),
                    actionButton("Forecast_Univariate_Button_Train", label = "Run")
                ),
                bs4TabCard(
                    id = "forecastOutput", width = 8, title = "Forecast Output", closable = FALSE, solidHeader = TRUE, maximizable  = TRUE,
                    bs4TabPanel(tabName = "Plot", active = TRUE, addSpinner(plotOutput("Forecast_Univariate_Train_Output_LinePlots"))),
                    bs4TabPanel(tabName = "Dataframe", active = FALSE, addSpinner(dataTableOutput("Forecast_Univariate_Train_Output_DF")))
                )
            ),
            fluidRow(
                bs4TabCard(
                    id = "fittedDiagnosis", width = 12, title = "Model Diagnosis", closable = FALSE, solidHeader = TRUE, maximizable  = TRUE,
                    bs4TabPanel(tabName = "Report", active = FALSE, addSpinner(dataTableOutput("Forecast_Univariate_Output_Report"))),
                    bs4TabPanel(tabName = "Accuracy", active = TRUE, addSpinner(dataTableOutput("Forecast_Univariate_Output_Accuracy"))),
                    bs4TabPanel(tabName = "Residuals", active = FALSE, addSpinner(plotOutput("Forecast_Univariate_Output_ResidualsLinePlot")), addSpinner(plotOutput("Forecast_Univariate_Output_ResidualsHistogramPlot"))),
                    bs4TabPanel(tabName = "ACF", active = FALSE, addSpinner(plotOutput("Forecast_Univariate_Output_ACFPlot")))
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
    
    solar_df <- readRDS("solar_df.rds")
    solar_tsbl <- readRDS("solar_tsbl.rds")
    
    #=============================================================
    #                 EXPLORATORY DATA ANALYSIS
    #=============================================================

    updateSelectizeInput(session, "ExploratoryAnalysis_SimpleViz_Input_Column", choices = names(solar_df)[-which(names(solar_df) %in% c("date_time", "id"))] )
    updateDateRangeInput(session, "ExploratoryAnalysis_SimpleViz_Input_DateRange", start = min(solar_df$date_time), end = max(solar_df$date_time))

    
    eda_proc_df <- reactive({
        req(solar_df)
        df <- preprocessEDA(df = solar_df,
                            granularity = input$ExploratoryAnalysis_SimpleViz_Input_Granularity,
                            date_start = input$ExploratoryAnalysis_SimpleViz_Input_DateRange[1],
                            date_end = input$ExploratoryAnalysis_SimpleViz_Input_DateRange[2]
        )
        
        
        return (df)
    })

    output$ExploratoryAnalysis_SimpleViz_Output_Plots <- renderPlot({
        req(eda_proc_df())

        eda_proc_df() %>%
            ggplot(mapping = aes(x = date_time, y = .data[[input$ExploratoryAnalysis_SimpleViz_Input_Column]])) +
            geom_line(aes(color = .data[[input$ExploratoryAnalysis_SimpleViz_Input_Column]])) +
            ggtitle(input$ExploratoryAnalysis_SimpleViz_Input_Column) +
            theme_light() +
            theme(legend.position = "none")
    })



    output$ExploratoryAnalysis_SimpleViz_Output_DF <- renderDataTable({
        req(eda_proc_df())
        eda_proc_df()
    }, options = list(scrollX = TRUE, pageLength = 5))
    
    
    #=============================================================
    #               FORECAST UNIVARIATE
    #=============================================================
    
    updateSelectizeInput(session, "Forecast_Univariate_Input_TargetVariable", choices = names(solar_df)[-which(names(solar_df) %in% c("date_time", "id"))] )
    updateDateRangeInput(session, "Forecast_Univariate_Input_UniverseDateRange", min = min(solar_df$date_time), start = min(solar_df$date_time), max = max(solar_df$date_time), end = max(solar_df$date_time))
    updateDateInput(session, "Forecast_Univariate_Input_TrainTestSplitDate", min = min(solar_df$date_time), max = max(solar_df$date_time), value = "2010-01-01")
    

    forecast_model <- eventReactive(input$Forecast_Univariate_Button_Train, {
        
        
        # Subset the universe by date
        subset_tsbl <- solar_tsbl %>%
            filter_index(as.character(input$Forecast_Univariate_Input_UniverseDateRange[1]) ~ as.character(input$Forecast_Univariate_Input_TrainTestSplitDate))
        
        # Group tsbl by granularity
        if (input$Forecast_Univariate_Input_ForecastGranularity == "Day") {
            grouped_subset_tsbl <- subset_tsbl %>% index_by(datetime_day = as.Date(date_time)) %>% summarise_all(mean)
        } else if (input$Forecast_Univariate_Input_ForecastGranularity == "Month") {
            grouped_subset_tsbl <- subset_tsbl %>% index_by(datetime_month = yearmonth(date_time)) %>% summarise_all(mean)
        } else if (input$Forecast_Univariate_Input_ForecastGranularity == "Year") {
            grouped_subset_tsbl <- subset_tsbl %>% index_by(datetime_year = year(date_time)) %>% summarise_all(mean)
        } else {
            stop("Invalid value of input$Forecast_Univariate_Input_ForecastGranularity.")
        }
        
        
        # Create train set to be less than the train-test-split date
        if (input$Forecast_Univariate_Input_ForecastGranularity == "Day") {
            train_grouped_subset_tsbl <- grouped_subset_tsbl %>%
                filter(datetime_day < input$Forecast_Univariate_Input_TrainTestSplitDate) 
        } else if (input$Forecast_Univariate_Input_ForecastGranularity == "Month") {
            train_grouped_subset_tsbl <- grouped_subset_tsbl %>%
                filter(datetime_month < input$Forecast_Univariate_Input_TrainTestSplitDate) 
        } else if (input$Forecast_Univariate_Input_ForecastGranularity == "Year") {
            train_grouped_subset_tsbl <- grouped_subset_tsbl %>%
                filter(datetime_year < input$Forecast_Univariate_Input_TrainTestSplitDate) 
        } else {
            stop("Invalid date time argument in univariate forecasting.")
        }
        
        
        target_col <- input$Forecast_Univariate_Input_TargetVariable
        forecast_horizon <- input$Forecast_Univariate_Input_ForecastHorizon

        # Train model
        if (input$Forecast_Univariate_Input_Model == "Average") model <- train_grouped_subset_tsbl %>% model(model_mean = MEAN(!! sym(target_col)))
        if (input$Forecast_Univariate_Input_Model == "Naive") model <- train_grouped_subset_tsbl %>% model(model_naive = NAIVE(!! sym(target_col)))
        if (input$Forecast_Univariate_Input_Model == "Seasonal Naive") model <- train_grouped_subset_tsbl %>% model(model_seasonalnaive = SNAIVE(!! sym(target_col)))
        if (input$Forecast_Univariate_Input_Model == "ARIMA") model <- train_grouped_subset_tsbl %>% model(model_seasonalnaive = ARIMA(!! sym(target_col)))
        if (input$Forecast_Univariate_Input_Model == "NNETAR") model <- train_grouped_subset_tsbl %>% model(model_seasonalnaive = NNETAR(!! sym(target_col)))
        
        # Forecast
        forecast_data <- model %>%
            forecast(h = forecast_horizon)
        
        
        out <- list(
            trained_model = model,
            forecast_data = forecast_data,
            grouped_subset_tsbl = grouped_subset_tsbl
        )
    })
    
    
    
    # Accuracy Table
    output$Forecast_Univariate_Output_Accuracy <- renderDataTable({
        req(forecast_model())
        train_acc <- accuracy(forecast_model()$trained_model)
        test_acc <- accuracy(forecast_model()$forecast_data, forecast_model()$grouped_subset_tsbl)
        
        bind_rows(train_acc, test_acc)
    }, options = list(scrollX = TRUE, pageLength = 5))

    
    # Report Table
    output$Forecast_Univariate_Output_Report <- renderDataTable({
        req(forecast_model())
        forecast_model()$trained_model %>% augment()
    }, options = list(scrollX = TRUE, pageLength = 5))
    
    
    # Residual Line Plot
    output$Forecast_Univariate_Output_ResidualsLinePlot <- renderPlot({
        req(forecast_model())
        forecast_model()$trained_model %>% 
            augment() %>%
            autoplot(.resid, aes(colour=.resid)) + ggtitle("Residuals")
    })
    
    
    # Residual Histogram Plot
    output$Forecast_Univariate_Output_ResidualsHistogramPlot <- renderPlot({
        req(forecast_model())
        forecast_model()$trained_model %>% 
            augment() %>%
            ggplot(mapping = aes(x = .resid)) + geom_histogram(fill="pink") + ggtitle("Histogram of Residuals")
    })
    
    
    # Residual ACF Plot
    output$Forecast_Univariate_Output_ACFPlot <- renderPlot({
        req(forecast_model())
        forecast_model()$trained_model %>% 
            augment() %>%
            ACF(.resid) %>%
            autoplot() + ggtitle("ACF of Residuals")
    })
    
    
    
    
    # Forecast Plot
    output$Forecast_Univariate_Train_Output_LinePlots <- renderPlot({
        req(forecast_model())
        forecast_model()$forecast_data %>%
            autoplot(forecast_model()$grouped_subset_tsbl, colour = "red")
    })
    
    
    # Forecast DF
    output$Forecast_Univariate_Train_Output_DF <- renderDataTable({
        req(forecast_model())
        forecast_model()$forecast_data
    }, options = list(scrollX = TRUE, pageLength = 5))
    
    
}




###################################################################
################              PAGE                 ################
###################################################################
shiny::shinyApp(
    ui = ui,
    server = server
)