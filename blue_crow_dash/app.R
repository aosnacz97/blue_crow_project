################################################################################
# Libraries
################################################################################
suppressMessages(library(maps))
suppressMessages(library(lme4))
suppressMessages(library(shiny))
suppressMessages(library(shinyalert))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinydashboardPlus))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
suppressMessages(library(formattable))
suppressMessages(library(dashboardthemes))
suppressMessages(library(readxl))
suppressMessages(library(RcppRoll))
suppressMessages(library(plotly))
suppressMessages(library(shinyBS))
suppressMessages(library(shinyalert))
suppressMessages(library(shinycustomloader))
suppressMessages(library(geosphere))
suppressMessages(library(waiter))
suppressMessages(library(shinyjs))
suppressMessages(library(grid))
suppressMessages(library(jpeg))
suppressMessages(library(feather))
suppressMessages(library(sever))
suppressMessages(library(fabricerin))
suppressMessages(library(shinyalert))
suppressMessages(library(rdrop2))
suppressMessages(library(reshape2))
suppressMessages(library(RMySQL))
suppressMessages(library(rjson))
suppressMessages(library(dplyr))
suppressMessages(library(ggdark))
suppressMessages(library(ggthemes))
suppressMessages(library(scales))
suppressMessages(library(rdrop2))
suppressMessages(library(highcharter)) 
suppressMessages(library(corrplot))
suppressMessages(library(matrixStats))
suppressMessages(library(kableExtra))
suppressMessages(library(shinycustomloader))
suppressMessages(library(stringr))
suppressMessages(library(reactablefmtr))
suppressMessages(library(broom))
suppressMessages(library(htmltools))
suppressMessages(library(ppsr))
suppressMessages(library(httr))
suppressMessages(library(grDevices))
suppressMessages(library(echarts4r))
suppressMessages(library(patchwork))
suppressMessages(library(ggpubr))
suppressMessages(library(png))
suppressMessages(library(grid))
suppressMessages(library(cowplot))
suppressMessages(library(gridExtra))
suppressMessages(library(ggpubr))
suppressMessages(library(png))
suppressMessages(library(grid))
suppressMessages(library(cowplot))
suppressMessages(library(gridExtra))
suppressMessages(library(sysfonts))
suppressMessages(library(showtext))
suppressMessages(library(jsonlite))
suppressMessages(library(ggforce))
suppressMessages(library(showtext))
suppressMessages(library(fresh))
suppressMessages(library(memoise))
suppressMessages(library(fontawesome))
suppressMessages(library(gbm))
suppressMessages(library(odbc))
suppressMessages(library(RMySQL))
suppressMessages(library(config))

################################################################################
# Settings
################################################################################
options(shiny.maxRequestSize = 10*1024^2)
shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))
shinyOptions(cache = cachem::cache_mem(max_size = Inf))

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 7200000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.close();  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 7200000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"

tags$script(inactivity)
#code for server reloading screen
disconnected <- tagList(
  h1("The Blue Crow Demo Dash"),
  p("Has disconnected due to inactivity. Please hit REFRESH."),
  tags$img(src="cdleganes.gif"),
  reload_button("REFRESH", class = "warning")
)

#links social media buttons. URL address for the social media links in the app
url2 <- "https://anthonyosnacz.shinyapps.io/blue_crow_demo/" #this will be changed to the app's url
url <- "https://twitter.com/intent/tweet?text=Check out this app to explore Blue Crow's Data. @aosnacz&url=https://anthonyosnacz.shinyapps.io/blue_crow_demo/" #Change to your URL

################################################################################
# Themes
################################################################################
react_theme <- reactableTheme(color = "hsl(0, 0%, 90%)", backgroundColor = "hsl(0, 0%, 10%)", 
                              borderColor = "hsl(0, 0%, 18%)", stripedColor = "hsl(0, 0%, 13%)", 
                              headerStyle = list(`&:hover[aria-sort]` = list(backgroundColor = "hsl(0, 0%, 14%)")), 
                              tableBodyStyle = list(color = "hsl(0, 0%, 75%)"), rowHighlightStyle = list(color = "hsl(0, 0%, 90%)", 
                                                                                                         backgroundColor = "hsl(0, 0%, 14%)"), selectStyle = list(backgroundColor = "hsl(0, 0%, 20%)"), 
                              inputStyle = list(backgroundColor = "hsl(0, 0%, 10%)", borderColor = "hsl(0, 0%, 21%)", 
                                                `&:hover, &:focus` = list(borderColor = "hsl(0, 0%, 30%)")), 
                              pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"), 
                              pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)"))

options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 1)))
options(knitr.table.format = "latex")
thm <- hc_theme_merge(
  hc_theme_darkunica(),
  hc_theme(
    chart = list(
      backgroundColor = "black"
      #divBackgroundImage = "http://www.wired.com/images_blogs/underwire/2013/02/xwing-bg.gif"
    )
  )
)

thm3 <- hc_theme_merge(
  hc_theme_darkunica(),
  hc_theme(
    colors = c(
      "#00bfff", # blue
      #"#00FFFF", # Cyan
      #"#FF00FF" # magenta
      "#FFFF00", # yellow
      #"#ff8000", # orange
      "#ff0000" # Red
    ),
    chart = list(
      backgroundColor = "black"
      #divBackgroundImage = "https://www.drivelinebaseball.com/wp-content/uploads/2021/01/Deadlift-Back_-scaled.jpg"
    )
  )
)


################################################################################
# css
################################################################################
css = HTML("
  .blue .small-box {
    background-color: #87CEFA !important;
  }
  .red .small-box {
    background-color: #D20000 !important;
  }
")

################################################################################
# read in data (ideally from api/sql db, but in this case our csv)
################################################################################
individual_athlete_data_final <- read.csv("individual_athlete_data_final.csv", stringsAsFactors = FALSE)

norms <- read.csv("norms.csv", stringsAsFactors = FALSE)

norms_hand <- read.csv("norms_hand.csv", stringsAsFactors = FALSE)

norms_age <- read.csv("norms_age.csv", stringsAsFactors = FALSE)

norms_pos <- read.csv("norms_pos.csv", stringsAsFactors = FALSE)

################################################################################
# Create the UI
################################################################################
ui <- dashboardPage(
  # skin options: 
  skin = "blue",
  useShinyalert(),
  
  header = dashboardHeader(
    
    leftUi = tagList(
      
      dropdownBlock(
        id = "translate",
        title = "Translate App",
        icon = icon("language"),
        badgeStatus = NULL,
        HTML("<!DOCTYPE html><html lang='en-US'><body><h1>Blue Crow Demo Dash </h1><p>Translate this Demo Dash:</p><div id='google_translate_element'></div><script type='text/javascript'>function googleTranslateElementInit() {new google.translate.TranslateElement({pageLanguage: 'en'}, 'google_translate_element');}</script><script type='text/javascript' src='//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit'></script></body></html>")
      )
    ),
    
    #popup modal (window) link. This code opens a window with basic explanation of the app
    tags$li(class = "dropdown", actionLink("welcome", label = 'About', icon = icon("info-circle"), style = 'color:white'),
            bsTooltip("welcome", HTML("Click to know more about this app."), placement = "bottom", trigger = "hover", options = NULL)),
    
    #this code shows an icon (logo) on the top left of the screen the the left side bar is collapsed
    title = tagList(
      shiny::span(class = "logo-lg", img(src = "words.png", width = "100px", height = "50px")), 
      img(src = "blue_crow_logo_removebg.png", width = "35px", height = "33px")),
    titleWidth = 330),
  
  sidebar = dashboardSidebar(
    
    width = 330, #width of the left side bar
    
    sidebarMenu(
      
      fluidRow(width = "100%", align = "center", #style = "padding-right:45px",
               
               tags$br(),
               
               column(width = 5),
               
               column(width = 2,  style = "padding-right:45px",
                      
                      # Create a twitter button for users to share the app on twitter
                      tags$a(href = url, "Tweet", class="twitter-share-button", `data-show-count` = "true"),
                      includeScript("http://platform.twitter.com/widgets.js")),
               
               column(width = 1),
               
               # Create 'linkedin-share-button' for users to share the app on linkedin
               column(width = 2,  style = "padding-right:45px",
                      tags$script(src = "https://platform.linkedin.com/in.js", type = "text/javascript", "lang: en_US"),
                      tags$script(type = "IN/Share",`data-url` = url2)),
               
               column(width = 2)
               
               
      ),
      menuItem("Athlete Assessment Summary", tabName = "assessmentsummary", icon = icon("project-diagram"), startExpanded = F)
    )),
  
  body = dashboardBody(
    useShinyjs(),
    tags$head(tags$style(css)),
    
    tags$head(tags$style(HTML('/* body */.content-wrapper, .right-side {background-color: #000000;}'))),
    
    use_waiter(), #this function is needed to add the loader page displayed while the app loads 
    
    #bring in sever dependencies
    useSever(),
    
    #this code supresses potential error messages that may confuse the user
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    #this sets the width of dropdownBlock is embedded on the header part
    tags$head(tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu {width:400px;}'))),
    tags$head(tags$style(HTML("input[type=\"number\"] {
                                                      width: 100px;
                                                    }"))),
    
    
    #this sets the customised look for slider inputs on the right side bar
    chooseSliderSkin("Square"),
    
    #this function is needed to initialize shiny alert for modal and popups windows
    useShinyalert(),
    
    #this function is needed for shinyJS
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "assessmentsummary",
        tabBox(title = "", id = "tab1", height = "100%", width = "100%", 
               ################################################################################
               # Tab 1 will give percentile scores for an athlete's most recent assessment
               # "Quick Stats"
               ################################################################################
               tabPanel("Quick Stats", icon = icon("project-diagram"),
                        fluidRow(
                          column(2,
                                 pickerInput("athlete_filter", "Select Athlete:",
                                             choices = sort(unique(individual_athlete_data_final$athletes)), 
                                             multiple = FALSE, 
                                             options = list(`actions-box` = TRUE, `live-search`=TRUE))),
                          fluidRow(column(2, pickerInput("norms_input_qs", "Select Norms:",
                                                         choices = list("All Baseline Data" = 1, "Age Group" = 2,
                                                                        "Position" = 3, "Laterality" = 4),selected = 2)))
                        ),
                        fluidRow(column(width = 12, align = "right",
                                        actionButton("quickstats_info", label = '', icon = icon("info-circle"), style = 'color:#0B0B45; background-color:#FFFFFF; border-color:#FFFFFF'),
                                        bsTooltip("quickstats_info", HTML("About This Tab"), placement = "left", trigger = "hover", options = NULL))),
                        fluidRow(box(title = HTML("Percentile Graphs"), status = "navy",  closable = FALSE, collapsible = TRUE, withLoader(plotOutput("quickstats_graphs", height = 1000), type = "image", loader = "logo_leganes.gif"), width = 12, background = "black", solidHeader = TRUE)),
                        fluidRow(box(title = HTML("Where an Athlete Ranks Graphs"), status = "navy",  closable = FALSE, collapsible = TRUE, withLoader(plotOutput("ridge_plots_qs_output", height = 1000), type = "image", loader = "logo_leganes.gif"), width = 12, background = "black", solidHeader = TRUE))
                        
               ),
               ################################################################################
               # Tab 2 will provide a table that displays normative values
               # "Norms"
               ################################################################################
               
               tabPanel("Norms", icon = icon("dumbbell"),
                        fluidRow(column(2, pickerInput("norms_input", "Select Norms:",
                                                       choices = list("All Baseline Data" = 1, "Age Group" = 2,
                                                                      "Position" = 3, "Laterality" = 4),selected = 2)),
                                 column(2, pickerInput("norms_input_metric", "Select Metric:",
                                                                                              choices = sort(unique(norms$metric)), 
                                                                                              selected = sort(unique(norms$metric)), 
                                                                                              multiple = T, 
                                                                                              options = list(`actions-box` = TRUE, `live-search`=TRUE)))
                                 
                        ),
                        
                        fluidRow(box(fluidRow(column(width = 12, align = "right",
                                                     actionButton("normsinfo", label = '', icon = icon("info-circle"), style = 'color:#0B0B45; background-color:#FFFFFF; border-color:#FFFFFF'),bsTooltip("normsinfo", HTML("About This Table"), placement = "left", trigger = "hover", options = NULL))),
                                     title = "Norms", status = "navy", closable = FALSE, collapsible = TRUE,  DT::dataTableOutput("norms_table"), width = 12, solidHeader = TRUE))
                        
               ),
               
               ################################################################################
               # Tab 3 will provide an athlete's historical trends for selected metric
               # "Athlete History"
               ################################################################################
               
               tabPanel("Athlete History", icon = icon("chart-line"),
                        fluidRow(
                          column(2,
                                 pickerInput("athlete_filter_ah", "Select Athlete:",
                                             choices = sort(unique(individual_athlete_data_final$athletes)), 
                                             multiple = FALSE, 
                                             options = list(`actions-box` = TRUE, `live-search`=TRUE))),
                          column(2,
                                 pickerInput("date_filter_ah", "Select Date:",
                                             choices = sort(unique(individual_athlete_data_final$date)), 
                                             selected = sort(unique(individual_athlete_data_final$date)),
                                             multiple = T, 
                                             options = list(`actions-box` = TRUE, `live-search`=TRUE))),
                          column(2, pickerInput("norms_input_ah", "Select Norms:",
                                                choices = list("All Baseline Data" = 1, "Age Group" = 2,
                                                               "Position" = 3, "Laterality" = 4),selected = 2))
                        ),
                        fluidRow(column(width = 12, align = "right",
                                        actionButton("athletehistory_info", label = '', icon = icon("info-circle"), style = 'color:#0B0B45; background-color:#FFFFFF; border-color:#FFFFFF'),
                                        bsTooltip("athletehistory_info", HTML("About This Tab"), placement = "left", trigger = "hover", options = NULL))),
                        fluidRow(box(title = HTML("Athlete Metric History"), status = "navy",  closable = FALSE, collapsible = TRUE, withLoader(plotOutput("athletehistory_graphs", height = 1000), type = "image", loader = "logo_leganes.gif"), width = 12, background = "black", solidHeader = TRUE))
               )
               
        )#tabbox
      )),#tabitem #tabitems
    
    
    
    
    ################################################################################
    #code for page loader. this is the landing page displayed while the app loads
    ################################################################################
    
    waiter_show_on_load(
      color = "black",
      div(style = "color:black;",
          tags$h2("Loading...", style = "color:navy", align = "center"),
          tags$img(src="blue_crow_logo_removebg.png", width="1000px", height="500px")
      )
    )
    
    
  ),#dashboardbody ends here
  
  controlbar = dashboardControlbar(controlbarMenu(id = "control_menu", controlbarItem("Switch", tabName = "darkmode", icon = icon("globe-americas"), switchInput(inputId = "switchpanel", onLabel = "On",
                                                                                                                                                                 offLabel = "Off",size = "mini", onStatus = "navy", value = TRUE)))),
  title = "DashboardPage",
  ################################################################################
  #Footer. The footer bar displayed a the bottom of the dashboard###########
  #this codes adds a footer to the dashboard
  ################################################################################
  
  footer = dashboardFooter(
    
    #footer details on the right side of the footer
    right = HTML(paste(img(src = "words.png", width = "100px", height = "33px"), 
                       tags$span("Dash", style = "font-family: Arial; color: grey; font-size: 16px"))),
    
    #footer details on the left side of the footer
    left = HTML(paste(icon = icon("copyright"), tags$span("2022. Created by Anthony Osnacz", style = "font-family: Arial; color: grey; font-size: 16px"),
                      tags$span(tags$a(href= "https://twitter.com/aosnacz", icon("twitter"))),
                      sep = " "))
  ))

################################################################################
# Server Side
################################################################################

server <- function(input, output, session) {
  
  #Intro pop up window. This creates a window with basic explanation about what the app does
  observeEvent(input$welcome, {
    
    showModal(
      
      modalDialog(
        HTML(paste(img(src = "words.png", width = "447px", height = "139px"), tags$br(), tags$span("", style = "font-family: Arial; color: Black; font-size:40px"))),
        tags$hr(),
        tags$br(),
        tags$h4("Introducing the Blue Crow Demo Dash", style = "padding-left:2.3em; font-family: Arial; color: black"),
        tags$img(src = "dashboard.png", width = "40px", height = "40px", align = "left"),
        p("The Demo Dash displays metrics collected by Blue Crow High Performance Assessments", style = "padding-left:3em; font-family: Arial; color: black"),
        tags$br(),
        tags$h4("Athlete Assessment Summary", style = "padding-left:2.3em; font-family: Arial; color: black"),
        tags$img(src = "report.jpg", width = "40px", height = "40px", align = "left"),
        p("This page is home to individual player assessment scores. The Quick Stats tab shows percentile scores from an athlete's latest assessment. You can use the dropdown filters to change what populations you compare the athlete to! The Norms tab shows different normative values for different populations! You can use the dropdowns to filter for different groups and metrics. The Athlete History tab shows athlete's progression in KPIs over time. ", style = "padding-left:3em; font-family: Arial; color: black"),
        tags$br(),
        tags$hr(),
        HTML(paste(tags$span("Any other questions? Reach out on ", style = "font-family: Arial; color: black"), tags$span(tags$a(href= "https://twitter.com/aosnacz?lang=en", "Twitter!")))),
        
        size = "l",
        easyClose = TRUE)
      
    )#showmodal
    
    
  })#observeEvent 
  
  ################################################################################
  # Tab 1 will give percentile scores for an athlete's most recent assessment
  # "Quick Stats"
  ################################################################################
  #Info pop up window. Users can click on the info button to get an explanation of this report
  observeEvent(input$quickstats_info, {
    
    
    showModal(
      
      modalDialog(
        
        tags$h3("Quick Stats Tab Info", style = "font-family: Arial; color: black"),
        tags$hr(),
        tags$h5("After selecting a player, the date filter will automatically filter for an athlete's latest assessment. The value of the metric, the percent change from last assessment in said metric, and magnitude of change are displayed on the graph. The red dotted bars represent the middle 68% of a normal distribution, so +/- 1 standard deviation from mean. The white line represents the mean. Below is a picture describing how to interpret the data. A bar that goes past the red line is an above average score, a bar that falls below that line is below average.", style = "font-family: Arial; color: black"),
        HTML(paste(img(src = "example.png", width = "500px", height = "250px"), tags$br(), tags$span("", style = "font-family: Arial; color: Black; font-size:40px"))),
        tags$h5("Below the bar charts are graphs that shows where an athlete metric value lies within their population. The vertical dotted line represents the athlete's latest metric value.", style = "font-family: Arial; color: black"),
        tags$h5("This Demo Dash provides the following metrics for each athlete:", style = "font-family: Arial; color: black"),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Final Speed (km/h): "), "The speed of the final stage completed before being removed from the test."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "VO2max: "), "An estimation of the VO2max value for the athlete based on the final speed of the test, age, body weight and gender."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "0-10Y Split: "), "Time to complete the first 10Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "10-30Y Split: "), "Time to complete the last 20Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "0-30Y Split: "), "Time to complete the total distance from 0 to 30Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "7 Site Fat %: "), "Overall body % percentage taken using skinfold methods from seven sites"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Weight (lb): "), "Body weight in pounds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Height (in): "), "Player's height in inches"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "RSI Modified: "), "RSI Mod = CMJ Height / Time 2 Takeoff. This metric describes how 'explosive' an athlete is, that is how high and how fast."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Impulse: "), "Concentric impulse is the amount of force applied during the time spent in the concentric phase of the jump. We filter out any outliers by looking for tests greater than 3 std's away from mean, and then take the average of the remaining jumps."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Impulse - 100ms"), "This metric is the amount of force applied during the first 100ms of the concentric phase in the CMU. We filter out the outlier jumps using the criteria from above and then take the average of the metric between remaining jumps."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Mean Power / Body Weight: "), "(Force Produced during the concentric phase * velocity) / body weight. This metric is how much power was produced during the concentric phase of a jump relative to an athlete's body weight. This metric describes how powerful concentrically an athlete is relative to their size."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Eccentric Mean Power / Body Weight: "), "(Force Produced during the eccentric phase * velocity) / body weight. This metric is how much power was produced during the concentric phase of a jump relative to an athlete's body weight. This metric describes how powerful an athlete is eccentrically relative to their size."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Vertical Velocity @ Take off"), "Vertical Velocity at takeoff = impulse / body weight in kg. This metric describes how rapidly an athlete displaces their center of mass vertically."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Relative Max Strength: "), "The value of the estimated 1RM divided by the body weight of the athlete"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Estimated 1RM: "), "The estimated 1RM based on weight and number of repetitions using Brzycki's method"))),
        size = "l",
        
        easyClose = TRUE)
      
    )#showmodal
  })
  
  # quick stats
  individual_athlete_data_final2 <- reactive({
    
    individual_athlete_data_final %>%
      dplyr::filter(athletes %in% c(input$athlete_filter)) %>%
      dplyr::filter(date == max(date))
    
  }) 
  
  
  output$quickstats_graphs <- renderPlot({
    # "All Baseline Data" = 1, "Age Group" = 2, "Position" = 3, "Laterality" = 4
    # norms_input_groups, norms_input_test, norms_input_metric, norms_input_metric
    if (input$norms_input_qs == 1) {
      
      ggplot(individual_athlete_data_final2()) +
        geom_bar(aes(x=date, y=percentile_all), stat="identity", alpha = 1, width = .5, fill = "#000FFF") +
        geom_hline(yintercept= 15.9, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 84.1, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 50, color = "white", alpha=1, size= .5) +
        geom_text(aes(x=date, y=percentile_all, label=percentile_all), position=position_dodge(width=0.9), vjust=-0.25, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_all, label= paste0(individual_athlete_data_final2()$pct_change, "%")), position=position_dodge(width=0.9), vjust=1.5, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_all, label= individual_athlete_data_final2()$effect_size_interpretation), position=position_dodge(width=0.9), vjust=3, size = 4.5, fontface = "bold", color = "white") +
        dark_mode(theme_fivethirtyeight())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
        xlab("Date") + 
        ylab(paste0(individual_athlete_data_final2()$metric, "Percentile Relative to all data")) +
        facet_wrap(assessment~metric)
    }
    else if (input$norms_input_qs  == 2) {
      ggplot(individual_athlete_data_final2()) +
        geom_bar(aes(x=date, y=percentile_age), stat="identity", alpha = 1, width = .5, fill = "#000FFF") +
        geom_hline(yintercept= 15.9, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 84.1, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 50, color = "white", alpha=1, size= .5) +
        geom_text(aes(x=date, y=percentile_age, label=percentile_age), position=position_dodge(width=0.9), vjust=-0.25, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_age, label= paste0(individual_athlete_data_final2()$pct_change, "%")), position=position_dodge(width=0.9), vjust=1.5, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_age, label= individual_athlete_data_final2()$effect_size_interpretation), position=position_dodge(width=0.9), vjust=3, size = 4.5, fontface = "bold", color = "white") +
        dark_mode(theme_fivethirtyeight())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
        xlab("Date") + 
        ylab(paste0(individual_athlete_data_final2()$metric, "Percentile Relative to ", individual_athlete_data_final2()$age_group)) +
        facet_wrap(assessment~metric~individual_athlete_data_final2()$age_group)
    }
    
    else if (input$norms_input_qs  == 3) {
      ggplot(individual_athlete_data_final2()) +
        geom_bar(aes(x=date, y=percentile_pos), stat="identity", alpha = 1, width = .5, fill = "#000FFF") +
        geom_hline(yintercept= 15.9, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 84.1, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 50, color = "white", alpha=1, size= .5) +
        geom_text(aes(x=date, y=percentile_pos, label=percentile_pos), position=position_dodge(width=0.9), vjust=-0.25, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_pos, label= paste0(individual_athlete_data_final2()$pct_change, "%")), position=position_dodge(width=0.9), vjust=1.5, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_pos, label= individual_athlete_data_final2()$effect_size_interpretation), position=position_dodge(width=0.9), vjust=3, size = 4.5, fontface = "bold", color = "white") +
        dark_mode(theme_fivethirtyeight())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
        xlab("Date") + 
        ylab(paste0(individual_athlete_data_final2()$metric, "Percentile Relative to ", individual_athlete_data_final2()$position)) +
        facet_wrap(assessment~metric~individual_athlete_data_final2()$position)
    }
    
    else {
      ggplot(individual_athlete_data_final2()) +
        geom_bar(aes(x=date, y=percentile_hand), stat="identity", alpha = 1, width = .5, fill = "#000FFF") +
        geom_hline(yintercept= 15.9, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 84.1, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 50, color = "white", alpha=1, size= .5) +
        geom_text(aes(x=date, y=percentile_hand, label=percentile_hand), position=position_dodge(width=0.9), vjust=-0.25, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_hand, label= paste0(individual_athlete_data_final2()$pct_change, "%")), position=position_dodge(width=0.9), vjust=1.5, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_hand, label= individual_athlete_data_final2()$effect_size_interpretation), position=position_dodge(width=0.9), vjust=3, size = 4.5, fontface = "bold", color = "white") +
        dark_mode(theme_fivethirtyeight())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
        xlab("Date") + 
        ylab(paste0(individual_athlete_data_final2()$metric, "Percentile Relative to ", individual_athlete_data_final2()$laterality)) +
        facet_wrap(assessment~metric~individual_athlete_data_final2()$laterality)
      
    }
    
  })
  
  weight_room_assessments_velo_investigation2 <- reactive({
    
    # "All Baseline Data" = 1, "Age Group" = 2, "Position" = 3, "Laterality" = 4
    # norms_input_groups, norms_input_test, norms_input_metric, norms_input_metric
    
    if( input$norms_input_qs == 2)
    {dplyr::group_by(dplyr::filter(dplyr::group_by(individual_athlete_data_final, athletes,age_group, assessment, metric), date==min(date))) %>%
        dplyr::filter(age_group %in% c(individual_athlete_data_final2()$age_group)) %>%
        dplyr::filter(metric %in% c(individual_athlete_data_final2()$metric)) %>%
        ungroup()
      
    }
    else if( input$norms_input_qs == 3)
    {dplyr::group_by(dplyr::filter(dplyr::group_by(individual_athlete_data_final, athletes,position, assessment, metric), date==min(date))) %>%
        dplyr::filter(position %in% c(individual_athlete_data_final2()$position)) %>%
        dplyr::filter(metric %in% c(individual_athlete_data_final2()$metric)) %>%
        ungroup()
      
    }
    else if( input$norms_input_qs == 4)
    {dplyr::group_by(dplyr::filter(dplyr::group_by(individual_athlete_data_final, athletes,laterality, assessment, metric), date==min(date))) %>%
        dplyr::filter(laterality %in% c(individual_athlete_data_final2()$laterality)) %>%
        dplyr::filter(metric %in% c(individual_athlete_data_final2()$metric)) %>%
        ungroup()
      
    }
    else 
    {dplyr::group_by(dplyr::filter(dplyr::group_by(individual_athlete_data_final, athletes, assessment, metric), date==min(date))) %>%
        dplyr::filter(metric %in% c(individual_athlete_data_final2()$metric)) %>%
        ungroup()}
    
  })
  
  
  
  ################################################################################
  # Rainclouds by Input
  ################################################################################
  # "All Baseline Data" = 1, "Age Group" = 2, "Position" = 3, "Laterality" = 4
  # norms_input_groups, norms_input_test, norms_input_metric, norms_input_metric
  
  ridge_plots_qs <- reactive({
    
    if (input$norms_input_qs == 2) # "Age Group" = 2
      
    {individual_athlete_data_final2_ridge <- weight_room_assessments_velo_investigation2()
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      dplyr::filter(!is.na(individual_athlete_data_final2_ridge$percentile_age)) %>% 
      dplyr::filter(!is.na(individual_athlete_data_final2_ridge$age_group))
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      mutate(x = percentile_age)
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      dplyr::group_by(age_group, metric) %>% 
      mutate(
        n = n(),
        median = median(x),
        max = max(x),
        Velo_Group2 = age_group
      ) %>% 
      ungroup()
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>%
      dplyr::group_by(age_group, metric) %>% 
      #filter(n >= 10) %>% 
      mutate(species_num = as.numeric(fct_rev(age_group))) %>% 
      ungroup() 
    
    grouping_vline <- individual_athlete_data_final2() %>%
      group_by(metric) %>%
      summarise(percentile_age = mean(percentile_age))
    
    ## create a second chart with raincloud plotsNew_RSI
    ggplot(individual_athlete_data_final2_ridge, aes(unlist(individual_athlete_data_final2_ridge$percentile_age), individual_athlete_data_final2_ridge$species_num, color = age_group)) +
      stat_summary(
        geom = "linerange",
        fun.min = function(x) -Inf,
        fun.max = function(x) median(x, na.rm = TRUE),
        linetype = "dotted",
        orientation = "y",
        size = .7
      ) +
      geom_point(
        aes(y = individual_athlete_data_final2_ridge$species_num - .15), 
        shape = "|",
        size = 5,
        alpha = .5
      ) +
      ggdist::stat_halfeye(
        aes(
          y = individual_athlete_data_final2_ridge$species_num,
          color = age_group,
          fill = after_scale(colorspace::lighten(color, .2))
        ),
        shape = 18,
        point_size = 3,
        interval_size = 1.8,
        adjust = .5,
        .width = c(0, 1)
      ) +
    geom_text(
      aes(x = median, label = format(round(median, 2), nsmall = 2)),
      stat = "unique",
      color = "white",
      family = "gotham",
      fontface = "bold",
      size = 3.4,
      nudge_y = .15
    ) +
    geom_text(
      aes(x = max, label = glue::glue("n = {n}")),
      stat = "unique",
      family = "gotham",
      fontface = "bold",
      size = 2.5,
      hjust = 0,
      nudge_x = .01,
      nudge_y = .02
    ) +
    scale_y_continuous(
      limits = c(.55, NA),
      breaks = 1:1,
      labels = paste0(unique(individual_athlete_data_final2()$age_group))
    ) +
    scale_color_manual(values = c("#000FFF"), guide = "none") +
    scale_fill_manual(values = c("#000FFF"), guide = "none") +
    labs(
      x = NULL,
      y = NULL,
      title = paste0(unique(individual_athlete_data_final2()$athletes), " by Age Group"),
      subtitle = paste0("Where an athlete stacks up vs peers"),
      caption = "Plot by @aosnacz"
    ) +
      geom_vline(data = grouping_vline, aes(xintercept = percentile_age), colour="black", linetype = "dashed", size = 1, alpha = .5) +
      theme(panel.background = element_blank()) +
      facet_wrap(assessment~metric)
    }
    else if(input$norms_input_qs == 3) # "Position" = 3
    {individual_athlete_data_final2_ridge <- weight_room_assessments_velo_investigation2()
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      dplyr::filter(!is.na(individual_athlete_data_final2_ridge$percentile_pos)) %>% 
      dplyr::filter(!is.na(individual_athlete_data_final2_ridge$position))
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      mutate(x = percentile_pos)
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      dplyr::group_by(position, metric) %>% 
      mutate(
        n = n(),
        median = median(x),
        max = max(x),
        Velo_Group2 = position
      ) %>% 
      ungroup()
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>%
      dplyr::group_by(position, metric) %>% 
      #filter(n >= 10) %>% 
      mutate(species_num = as.numeric(fct_rev(position))) %>% 
      ungroup() 
    
    grouping_vline <- individual_athlete_data_final2() %>%
      group_by(metric) %>%
      summarise(percentile_pos = mean(percentile_pos))
    
    ## create a second chart with raincloud plotsNew_RSI
    ggplot(individual_athlete_data_final2_ridge, aes(unlist(individual_athlete_data_final2_ridge$percentile_pos), individual_athlete_data_final2_ridge$species_num, color = position)) +
      stat_summary(
        geom = "linerange",
        fun.min = function(x) -Inf,
        fun.max = function(x) median(x, na.rm = TRUE),
        linetype = "dotted",
        orientation = "y",
        size = .7
      ) +
      geom_point(
        aes(y = individual_athlete_data_final2_ridge$species_num - .15), 
        shape = "|",
        size = 5,
        alpha = .5
      ) +
      ggdist::stat_halfeye(
        aes(
          y = individual_athlete_data_final2_ridge$species_num,
          color = position,
          fill = after_scale(colorspace::lighten(color, .2))
        ),
        shape = 18,
        point_size = 3,
        interval_size = 1.8,
        adjust = .5,
        .width = c(0, 1)
      ) +
      geom_text(
        aes(x = median, label = format(round(median, 2), nsmall = 2)),
        stat = "unique",
        color = "white",
        family = "gotham",
        fontface = "bold",
        size = 3.4,
        nudge_y = .15
      ) +
      geom_text(
        aes(x = max, label = glue::glue("n = {n}")),
        stat = "unique",
        family = "gotham",
        fontface = "bold",
        size = 2.5,
        hjust = 0,
        nudge_x = .01,
        nudge_y = .02
      ) +
      scale_y_continuous(
        limits = c(.55, NA),
        breaks = 1:1,
        labels = paste0(unique(individual_athlete_data_final2()$position))
      ) +
      scale_color_manual(values = c("#000FFF"), guide = "none") +
      scale_fill_manual(values = c("#000FFF"), guide = "none") +
      labs(
        x = NULL,
        y = NULL,
        title = paste0(unique(individual_athlete_data_final2()$athletes), " by Position"),
        subtitle = paste0("Where an athlete stacks up vs peers"),
        caption = "Plot by @aosnacz"
      ) +
      geom_vline(data = grouping_vline, aes(xintercept = percentile_pos), colour="black", linetype = "dashed", size = 1, alpha = .5) +
      theme(panel.background = element_blank()) +
      facet_wrap(assessment~metric)
    }    
    else if(input$norms_input_qs == 4) # "Laterality" = 4
    {individual_athlete_data_final2_ridge <- weight_room_assessments_velo_investigation2()
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      dplyr::filter(!is.na(individual_athlete_data_final2_ridge$percentile_hand)) %>% 
      dplyr::filter(!is.na(individual_athlete_data_final2_ridge$laterality))
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      mutate(x = percentile_hand)
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      dplyr::group_by(laterality, metric) %>% 
      mutate(
        n = n(),
        median = median(x),
        max = max(x),
        Velo_Group2 = laterality
      ) %>% 
      ungroup()
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>%
      dplyr::group_by(laterality, metric) %>% 
      #filter(n >= 10) %>% 
      mutate(species_num = as.numeric(fct_rev(laterality))) %>% 
      ungroup() 
    
    grouping_vline <- individual_athlete_data_final2() %>%
      group_by(metric) %>%
      summarise(percentile_hand = mean(percentile_hand))
    
    ## create a second chart with raincloud plotsNew_RSI
    ggplot(individual_athlete_data_final2_ridge, aes(unlist(individual_athlete_data_final2_ridge$percentile_hand), individual_athlete_data_final2_ridge$species_num, color = laterality)) +
      stat_summary(
        geom = "linerange",
        fun.min = function(x) -Inf,
        fun.max = function(x) median(x, na.rm = TRUE),
        linetype = "dotted",
        orientation = "y",
        size = .7
      ) +
      geom_point(
        aes(y = individual_athlete_data_final2_ridge$species_num - .15), 
        shape = "|",
        size = 5,
        alpha = .5
      ) +
      ggdist::stat_halfeye(
        aes(
          y = individual_athlete_data_final2_ridge$species_num,
          color = laterality,
          fill = after_scale(colorspace::lighten(color, .2))
        ),
        shape = 18,
        point_size = 3,
        interval_size = 1.8,
        adjust = .5,
        .width = c(0, 1)
      ) +
      geom_text(
        aes(x = median, label = format(round(median, 2), nsmall = 2)),
        stat = "unique",
        color = "white",
        family = "gotham",
        fontface = "bold",
        size = 3.4,
        nudge_y = .15
      ) +
      geom_text(
        aes(x = max, label = glue::glue("n = {n}")),
        stat = "unique",
        family = "gotham",
        fontface = "bold",
        size = 2.5,
        hjust = 0,
        nudge_x = .01,
        nudge_y = .02
      ) +
      scale_y_continuous(
        limits = c(.55, NA),
        breaks = 1:1,
        labels = paste0(unique(individual_athlete_data_final2()$laterality))
      ) +
      scale_color_manual(values = c("#000FFF"), guide = "none") +
      scale_fill_manual(values = c("#000FFF"), guide = "none") +
      labs(
        x = NULL,
        y = NULL,
        title = paste0(unique(individual_athlete_data_final2()$athletes), " by Laterality"),
        subtitle = paste0("Where an athlete stacks up vs peers"),
        caption = "Plot by @aosnacz"
      ) +
      geom_vline(data = grouping_vline, aes(xintercept = percentile_hand), colour="black", linetype = "dashed", size = 1, alpha = .5) +
      theme(panel.background = element_blank()) +
      facet_wrap(assessment~metric)
    }
    else 
    {individual_athlete_data_final2_ridge <- weight_room_assessments_velo_investigation2()
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      dplyr::filter(!is.na(individual_athlete_data_final2_ridge$percentile_all))
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      mutate(x = percentile_all)
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>% 
      dplyr::group_by(metric) %>% 
      mutate(
        n = n(),
        data = paste0("All Data"),
        median = median(x),
        max = max(x),
      ) %>% 
      ungroup()
    
    individual_athlete_data_final2_ridge <- individual_athlete_data_final2_ridge %>%
      dplyr::group_by(metric) %>% 
      #filter(n >= 10) %>% 
      mutate(species_num = as.numeric(fct_rev(data))) %>% 
      ungroup() 
    
    grouping_vline <- individual_athlete_data_final2() %>%
      group_by(metric) %>%
      summarise(percentile_all = mean(percentile_all))
    
    ## create a second chart with raincloud plotsNew_RSI
    ggplot(individual_athlete_data_final2_ridge, aes(unlist(individual_athlete_data_final2_ridge$percentile_all), individual_athlete_data_final2_ridge$species_num, color = data)) +
      stat_summary(
        geom = "linerange",
        fun.min = function(x) -Inf,
        fun.max = function(x) median(x, na.rm = TRUE),
        linetype = "dotted",
        orientation = "y",
        size = .7
      ) +
      geom_point(
        aes(y = individual_athlete_data_final2_ridge$species_num - .15), 
        shape = "|",
        size = 5,
        alpha = .5
      ) +
      ggdist::stat_halfeye(
        aes(
          y = individual_athlete_data_final2_ridge$species_num,
          color = data,
          fill = after_scale(colorspace::lighten(color, .2))
        ),
        shape = 18,
        point_size = 3,
        interval_size = 1.8,
        adjust = .5,
        .width = c(0, 1)
      ) +
      geom_text(
        aes(x = median, label = format(round(median, 2), nsmall = 2)),
        stat = "unique",
        color = "white",
        family = "gotham",
        fontface = "bold",
        size = 3.4,
        nudge_y = .15
      ) +
      geom_text(
        aes(x = max, label = glue::glue("n = {n}")),
        stat = "unique",
        family = "gotham",
        fontface = "bold",
        size = 2.5,
        hjust = 0,
        nudge_x = .01,
        nudge_y = .02
      ) +
      scale_y_continuous(
        limits = c(.55, NA),
        breaks = 1:1,
        labels = paste0("All Baseline Data")
      ) +
      scale_color_manual(values = c("#000FFF"), guide = "none") +
      scale_fill_manual(values = c("#000FFF"), guide = "none") +
      labs(
        x = NULL,
        y = NULL,
        title = paste0(unique(individual_athlete_data_final2()$athletes), " against All Baseline Data"),
        subtitle = paste0("Where an athlete stacks up vs peers"),
        caption = "Plot by @aosnacz"
      ) +
      geom_vline(data = grouping_vline, aes(xintercept = percentile_all), colour="black", linetype = "dashed", size = 1, alpha = .5) +
      theme(panel.background = element_blank()) +
      facet_wrap(assessment~metric)
    }
  })
  
  output$ridge_plots_qs_output <- renderPlot({ridge_plots_qs()})
  
  
  
  ################################################################################
  # Tab 2 will provide a table that displays normative values
  # "Norms"
  ################################################################################
  #Info pop up window. Users can click on the info button to get an explanation of this report
  observeEvent(input$normsinfo, {
    
    
    showModal(
      
      modalDialog(
        
        tags$h3("Norms Tab Info", style = "font-family: Arial; color: black"),
        tags$hr(),
        tags$h5("This tab shows the normative values for all assessment metrics relative to different populations. The dropdown menus can be used to filter for specific metrics/populations/etc. The mean data is the average score for the selected population, the sd score is the standard deviation value away from the mean.", style = "font-family: Arial; color: black"),
        tags$h5("This Demo Dash provides the following metrics for each athlete:", style = "font-family: Arial; color: black"),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Final Speed (km/h): "), "The speed of the final stage completed before being removed from the test."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "VO2max: "), "An estimation of the VO2max value for the athlete based on the final speed of the test, age, body weight and gender."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "0-10Y Split: "), "Time to complete the first 10Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "10-30Y Split: "), "Time to complete the last 20Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "0-30Y Split: "), "Time to complete the total distance from 0 to 30Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "7 Site Fat %: "), "Overall body % percentage taken using skinfold methods from seven sites"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Weight (lb): "), "Body weight in pounds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Height (in): "), "Player's height in inches"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "RSI Modified: "), "RSI Mod = CMJ Height / Time 2 Takeoff. This metric describes how 'explosive' an athlete is, that is how high and how fast."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Impulse: "), "Concentric impulse is the amount of force applied during the time spent in the concentric phase of the jump. We filter out any outliers by looking for tests greater than 3 std's away from mean, and then take the average of the remaining jumps."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Impulse - 100ms"), "This metric is the amount of force applied during the first 100ms of the concentric phase in the CMU. We filter out the outlier jumps using the criteria from above and then take the average of the metric between remaining jumps."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Mean Power / Body Weight: "), "(Force Produced during the concentric phase * velocity) / body weight. This metric is how much power was produced during the concentric phase of a jump relative to an athlete's body weight. This metric describes how powerful concentrically an athlete is relative to their size."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Eccentric Mean Power / Body Weight: "), "(Force Produced during the eccentric phase * velocity) / body weight. This metric is how much power was produced during the concentric phase of a jump relative to an athlete's body weight. This metric describes how powerful an athlete is eccentrically relative to their size."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Vertical Velocity @ Take off"), "Vertical Velocity at takeoff = impulse / body weight in kg. This metric describes how rapidly an athlete displaces their center of mass vertically."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Relative Max Strength: "), "The value of the estimated 1RM divided by the body weight of the athlete"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Estimated 1RM: "), "The estimated 1RM based on weight and number of repetitions using Brzycki's method"))),
        size = "l",
        
        easyClose = TRUE)
      
    )#showmodal
  })
  
  norms2 <- reactive({norms %>%
      dplyr::filter(metric %in% c(input$norms_input_metric))
  }) 
  
  norms_age2 <- reactive({norms_age %>% 
      #dplyr::filter(age_group %in% c(input$norms_input_groups)) %>%
      dplyr::filter(metric %in% c(input$norms_input_metric))
  }) 
  
  norms_pos2 <- reactive({norms_pos %>%
      #dplyr::filter(position %in% c(input$norms_input_groups)) %>%
      dplyr::filter(metric %in% c(input$norms_input_metric))
  }) 
  
  norms_hand2 <- reactive({norms_hand %>% 
      #dplyr::filter(laterality %in% c(input$norms_input_groups)) %>%
      dplyr::filter(metric %in% c(input$norms_input_metric))
  }) 
  
  output$norms_table <- DT::renderDataTable({
    # "All Baseline Data" = 1, "Age Group" = 2, "Position" = 3, "Laterality" = 4
    # norms_input_groups, norms_input_test, norms_input_metric, norms_input_metric
    if (input$norms_input == 1) {
      
      datatable(distinct(norms2()), extensions = 'Buttons', options = list(scrollX = TRUE,
                                                                           dom = 'Bfrtip',
                                                                           buttons = c('csv'),
                                                                           initComplete = JS(
                                                                             "function(settings, json) {",
                                                                             "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#0B0B45'});",
                                                                             "}")
      ))
    }
    else if (input$norms_input == 2) {
      datatable(distinct(norms_age2()), extensions = 'Buttons', options = list(scrollX = TRUE,
                                                                               dom = 'Bfrtip',
                                                                               buttons = c('csv'),
                                                                               initComplete = JS(
                                                                                 "function(settings, json) {",
                                                                                 "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#0B0B45'});",
                                                                                 "}")
      ))
    }
    else if (input$norms_input == 3) {
      datatable(distinct(norms_pos2()), extensions = 'Buttons', options = list(scrollX = TRUE,
                                                                               dom = 'Bfrtip',
                                                                               buttons = c('csv'),
                                                                               initComplete = JS(
                                                                                 "function(settings, json) {",
                                                                                 "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#0B0B45'});",
                                                                                 "}")
      ))
    }
    else if (input$norms_input == 4)  {
      datatable(distinct(norms_hand2()), extensions = 'Buttons', options = list(scrollX = TRUE,
                                                                                dom = 'Bfrtip',
                                                                                buttons = c('csv'),
                                                                                initComplete = JS(
                                                                                  "function(settings, json) {",
                                                                                  "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#0B0B45'});",
                                                                                  "}")
      ))
    }
    else{}
  })
  
  
  ################################################################################
  # Tab 3 will provide an athlete's historical trends for selected metric
  # "Athlete History"
  ################################################################################
  
  #Info pop up window. Users can click on the info button to get an explanation of this report
  observeEvent(input$athletehistory_info, {
    
    showModal(
      
      modalDialog(
        
        tags$h3("Athlete History Tab Info", style = "font-family: Arial; color: black"),
        tags$hr(),
        tags$h5("After selecting a player, the date filter will automatically filter for an athlete's 5 most recent assessments. The value of the metric, the percent change from last assessment in said metric, and magnitude of change are displayed on the graph. The red dotted bars represent the middle 68% of a normal distribution, so +/- 1 standard deviation from mean. The white line represents the mean. Below is a picture describing how to interpret the data. A bar that goes past the red line is an above average score, a bar that falls below that line is below average.", style = "font-family: Arial; color: black"),
        HTML(paste(img(src = "example.png", width = "500px", height = "250px"), tags$br(), tags$span("", style = "font-family: Arial; color: Black; font-size:40px"))),
        tags$h5("This Demo Dash provides the following metrics for each athlete:", style = "font-family: Arial; color: black"),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Final Speed (km/h): "), "The speed of the final stage completed before being removed from the test."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "VO2max: "), "An estimation of the VO2max value for the athlete based on the final speed of the test, age, body weight and gender."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "0-10Y Split: "), "Time to complete the first 10Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "10-30Y Split: "), "Time to complete the last 20Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "0-30Y Split: "), "Time to complete the total distance from 0 to 30Y in seconds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "7 Site Fat %: "), "Overall body % percentage taken using skinfold methods from seven sites"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Weight (lb): "), "Body weight in pounds"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Height (in): "), "Player's height in inches"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "RSI Modified: "), "RSI Mod = CMJ Height / Time 2 Takeoff. This metric describes how 'explosive' an athlete is, that is how high and how fast."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Impulse: "), "Concentric impulse is the amount of force applied during the time spent in the concentric phase of the jump. We filter out any outliers by looking for tests greater than 3 std's away from mean, and then take the average of the remaining jumps."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Impulse - 100ms"), "This metric is the amount of force applied during the first 100ms of the concentric phase in the CMU. We filter out the outlier jumps using the criteria from above and then take the average of the metric between remaining jumps."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Concentric Mean Power / Body Weight: "), "(Force Produced during the concentric phase * velocity) / body weight. This metric is how much power was produced during the concentric phase of a jump relative to an athlete's body weight. This metric describes how powerful concentrically an athlete is relative to their size."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Eccentric Mean Power / Body Weight: "), "(Force Produced during the eccentric phase * velocity) / body weight. This metric is how much power was produced during the concentric phase of a jump relative to an athlete's body weight. This metric describes how powerful an athlete is eccentrically relative to their size."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Vertical Velocity @ Take off"), "Vertical Velocity at takeoff = impulse / body weight in kg. This metric describes how rapidly an athlete displaces their center of mass vertically."))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Relative Max Strength: "), "The value of the estimated 1RM divided by the body weight of the athlete"))),
        tags$h5(HTML(paste(tags$span(style="color: #000FFF", "Estimated 1RM: "), "The estimated 1RM based on weight and number of repetitions using Brzycki's method"))),
        size = "l",
        
        easyClose = TRUE)
      
    )#showmodal
    
    
  })
  #Player Test Reactivity
  # athlete history
  observeEvent(
    input$athlete_filter_ah,
    updatePickerInput(session, "date_filter_ah", "Select Test Date:",
                      choices = sort(unique(individual_athlete_data_final$date[individual_athlete_data_final$athletes==input$athlete_filter_ah])),
                      selected = tail(sort(unique(individual_athlete_data_final$date[individual_athlete_data_final$athletes==input$athlete_filter_ah])), 5)
    )
  ) 
  
  
  #Player Test Reactivity
  # quick stats
  individual_athlete_data_final3 <- reactive({individual_athlete_data_final %>%
      dplyr::filter(athletes %in% c(input$athlete_filter_ah)) %>%
      dplyr::filter(date %in% c(input$date_filter_ah))
    
  }) 
  
  output$athletehistory_graphs <- renderPlot({
    # "All Baseline Data" = 1, "Age Group" = 2, "Position" = 3, "Laterality" = 4
    # norms_input_groups, norms_input_test, norms_input_metric, norms_input_metric
    if (input$norms_input_ah == 1) {
      
      ggplot(individual_athlete_data_final3()) +
        geom_bar(aes(x=date, y=percentile_all), stat="identity", alpha = 1, width = .5, fill = "#000FFF") +
        geom_hline(yintercept= 15.9, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 84.1, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 50, color = "white", alpha=1, size= .5) +
        geom_text(aes(x=date, y=percentile_all, label=percentile_all), position=position_dodge(width=0.9), vjust=-0.25, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_all, label= paste0(individual_athlete_data_final3()$pct_change, "%")), position=position_dodge(width=0.9), vjust=1.5, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_all, label= individual_athlete_data_final3()$effect_size_interpretation), position=position_dodge(width=0.9), vjust=3, size = 4.5, fontface = "bold", color = "white") +
        dark_mode(theme_fivethirtyeight())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
        xlab("Date") + 
        ylab(paste0(individual_athlete_data_final3()$metric, "Percentile Relative to all data")) +
        facet_wrap(assessment~metric)
    }
    else if (input$norms_input_ah  == 2) {
      ggplot(individual_athlete_data_final3()) +
        geom_bar(aes(x=date, y=percentile_age), stat="identity", alpha = 1, width = .5, fill = "#000FFF") +
        geom_hline(yintercept= 15.9, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 84.1, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 50, color = "white", alpha=1, size= .5) +
        geom_text(aes(x=date, y=percentile_age, label=percentile_age), position=position_dodge(width=0.9), vjust=-0.25, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_age, label= paste0(individual_athlete_data_final3()$pct_change, "%")), position=position_dodge(width=0.9), vjust=1.5, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_age, label= individual_athlete_data_final3()$effect_size_interpretation), position=position_dodge(width=0.9), vjust=3, size = 4.5, fontface = "bold", color = "white") +
        dark_mode(theme_fivethirtyeight())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
        xlab("Date") + 
        ylab(paste0(individual_athlete_data_final3()$metric, "Percentile Relative to ", individual_athlete_data_final3()$age_group)) +
        facet_wrap(assessment~metric~individual_athlete_data_final3()$age_group)
    }
    
    else if (input$norms_input_ah  == 3) {
      ggplot(individual_athlete_data_final3()) +
        geom_bar(aes(x=date, y=percentile_pos), stat="identity", alpha = 1, width = .5, fill = "#000FFF") +
        geom_hline(yintercept= 15.9, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 84.1, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 50, color = "white", alpha=1, size= .5) +
        geom_text(aes(x=date, y=percentile_pos, label=percentile_pos), position=position_dodge(width=0.9), vjust=-0.25, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_pos, label= paste0(individual_athlete_data_final3()$pct_change, "%")), position=position_dodge(width=0.9), vjust=1.5, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_pos, label= individual_athlete_data_final3()$effect_size_interpretation), position=position_dodge(width=0.9), vjust=3, size = 4.5, fontface = "bold", color = "white") +
        dark_mode(theme_fivethirtyeight())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
        xlab("Date") + 
        ylab(paste0(individual_athlete_data_final3()$metric, "Percentile Relative to ", individual_athlete_data_final3()$position)) +
        facet_wrap(assessment~metric~individual_athlete_data_final3()$position)
    }
    
    else {
      ggplot(individual_athlete_data_final3()) +
        geom_bar(aes(x=date, y=percentile_hand), stat="identity", alpha = 1, width = .5, fill = "#000FFF") +
        geom_hline(yintercept= 15.9, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 84.1, color = "red",linetype = "dashed",  alpha=1, size= .5) +
        geom_hline(yintercept= 50, color = "white", alpha=1, size= .5) +
        geom_text(aes(x=date, y=percentile_hand, label=percentile_hand), position=position_dodge(width=0.9), vjust=-0.25, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_hand, label= paste0(individual_athlete_data_final3()$pct_change, "%")), position=position_dodge(width=0.9), vjust=1.5, size = 4.5, fontface = "bold", color = "white") +
        geom_text(aes(x=date, y=percentile_hand, label= individual_athlete_data_final3()$effect_size_interpretation), position=position_dodge(width=0.9), vjust=3, size = 4.5, fontface = "bold", color = "white") +
        dark_mode(theme_fivethirtyeight())+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
        xlab("Date") + 
        ylab(paste0(individual_athlete_data_final3()$metric, "Percentile Relative to ", individual_athlete_data_final3()$laterality)) +
        facet_wrap(assessment~metric~individual_athlete_data_final3()$laterality)
      
    }
    
  })
  
  ################################################################################
  # End of app
  ################################################################################
  
  #Piece of code needed to hide the landing page after 3 seconds####
  Sys.sleep(3)
  waiter_hide()
  
  #piece for the sever loading screen
  sever(html = disconnected, bg_color = "#000")
  
}

#function to create render the app.
shinyApp(ui, server)
