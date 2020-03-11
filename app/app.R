library(shiny)
library(bs4Dash)
library(vroom)
library(lubridate)


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
                    selectizeInput("ExploratoryAnalysis_SimpleViz_Input_Column", label = "Select Features", multiple = TRUE, choices = c()),
                    selectizeInput("ExploratoryAnalysis_SimpleViz_Input_Granularity", label = "Select Granularity", multiple = FALSE, choices = c("Hourly", "Monthly", "Yearly")),
                    dateRangeInput("ExploratoryAnalysis_SimpleViz_Input_DateRange", label = "Select Date Range", autoclose = TRUE)
                ),
                bs4Card(
                    width = 8, title = "Plots", closable = FALSE, solidHeader = TRUE, maximizable = TRUE,
                    plotOutput("ExploratoryAnalysis_SimpleViz_Output_Plots")
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
    solar_data <- vroom(file = "../data/solar_data.csv")
    solar_data$date_time <- ymd_hms(solar_data$date_time)
    
    updateSelectizeInput(session, "ExploratoryAnalysis_SimpleViz_Input_Column", choices = names(solar_data))
    updateDateRangeInput(session, "ExploratoryAnalysis_SimpleViz_Input_DateRange", start = min(solar_data$date_time), end = max(solar_data$date_time))
    
}




###################################################################
################              PAGE                 ################
###################################################################
shiny::shinyApp(
    ui = ui,
    server = server
)