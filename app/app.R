library(shiny)
library(bs4Dash)


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
        bs4SidebarMenuItem("", tabName = "item2", icon = "sliders"),
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


body <- bs4DashBody()



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
server <- function(input, output) {}




###################################################################
################              PAGE                 ################
###################################################################
shiny::shinyApp(
    ui = ui,
    server = server
)