
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(shinyBS)

boastPalette <- c("#AC8DCE")

dashboardPage(skin="purple",
              
              #Title
              dashboardHeader(title="Testing and CI Relationship", 
                              titleWidth = 250,
                              tags$li(class = "dropdown", actionLink("info", icon("info"))),
                              tags$li(class = "dropdown", 
                                      tags$a(href='https://github.com/EducationShinyAppTeam/BOAST', icon("github"))),
                              tags$li(class = "dropdown",
                                      tags$a(href='https://shinyapps.science.psu.edu/', icon("home")))),
              
              #Sidebar
              dashboardSidebar(
                width = 250,
                sidebarMenu(id = "tabs",
                  menuItem("Overview", tabName = "over", icon = icon("tachometer-alt")),
                  menuItem("Choose a Data Set", tabName = "explore1", icon = icon("table")),
                  menuItem("Testing and CI Relationship", tabName = "explore2", icon = icon("wpexplorer")),
                  menuItem("References", tabName = "refs", icon = icon("leanpub"))),
                tags$div(class = "sidebar-logo",boastUtils::psu_eberly_logo("reversed")
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
                ),
                tags$style(
                  type = "text/css",
                  ".content-wrapper,.right-side {
                    background-color: white;
                  }"
                ),
                tabItems(
                  tabItem(tabName = "over",
                          fluidPage(
                          h1("Testing and Confidence Relationship"),
                          p("In this app you will explore how a significance test relates to a confidence interval."),
                          br(),
                          h2("Instructions"),
                          p(tags$li("Choose one of the preloaded datasets or input your own.")),
                          p(tags$li("Go to the App and select the variable you would like to use. ")),
                          p(tags$li("Set the Sample Size, Null Hypothesis, Alternative Hyptohesis, and Confidence Level. ")),
                          p(tags$li("Follow the advice to explore the topic.")),
                          div(style = "text-align: center",bsButton("explore", "Explore", icon("bolt"), size = "large")),
                          br(),
                          h2("Acknowledgements"),
                          p("This app was developed and coded by David Robinson and Yingjie (Chelsea) Wang."),
                          p("This application was modified by Yubaihe Zhou in 2019 and Gonghao Liu in 2020.",
                            br(),
                            br(),
                            br(),
                            div(class = "updated", "Last Update: 6/28/2020 by GL."))
                  )
                  ),
                  tabItem(tabName = "explore1",
                          fluidPage(
                            titlePanel("Uploading Files"),
                            sidebarLayout(
                              sidebarPanel(
                                #Let them choose a preloaded dataset or input their own
                                #If it is preloaded output some information about the dataset
                                selectInput(inputId = "chooseM", "Select which data set you would like to use", choices = c("NHL Data" = "NHL","Bike Sharing" = "BikeSharing",InputYourOwn = "input")),
                                conditionalPanel("input.chooseM == 'NHL'",
                                                 "This data set was collected by statisticians working for the NHL over the course of the 2016-2017 NHL season. ",br(),br(),
                                                 "It is filtered to only include players who played at least half of the season (41 games). ",br(),br(),
                                                 "There are 526 players in this set"
                                ),
                                conditionalPanel("input.chooseM == 'BikeSharing'",
                                                 "This Dataset is about Capital Bikeshare program in Washington, D.C, during 2011 to 2012.",br(),br(),
                                                 "Each row represent the data in an hour.",br(),br(),
                                                 "There are 10,886 recorded in the dataset."
                                ),
                                conditionalPanel("input.chooseM == 'input'",
                                                 uiOutput("fileIncludeM"),
                                                 fileInput('file1', 'Choose Data File:',
                                                           accept=c('.csv', '.txt', '.xls',
                                                                    '.xlsx', '.sas7bdat')),
                                                 checkboxInput('header', 'Header', TRUE)
                                ) 
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Data Display",
                                                     dataTableOutput('displayM')
                                            ),
                                            tabPanel("Data Summary",
                                                     verbatimTextOutput('summaryM'),
                                                     uiOutput("MissingNoticeM")
                                            )
                                  )
                                )
                              )
                            )
                          ),
                  tabItem(tabName = "explore2",
                          fluidRow(
                            withMathJax(),
                            column(4,
                                   h4("Conduct a test about the population mean"),
                                   uiOutput("var.selM"),
                                   uiOutput("size.selM"),
                                   uiOutput("null.selM"),
                                   #use mu not equal to the null mean
                                   radioButtons(inputId= "altM","Set the alternative hypothesis", choices = c("\\(\\mu\\neq\\text{null mean}\\)", "\\(\\mu<\\text{null mean}\\)","\\(\\mu>\\text{null mean}\\)")),

                                   tags$style(type = "text/css",
                                              "
                                              .irs-slider {width: 8px; height: 20px; top: 22px;}
                                              "),
                                   sliderInput(inputId = "conflev1M","Select the confidence level (%):",min = 85,max = 99,value = 95),
                                   tableOutput("pvalueM")
                                   ),
                            column(8,
                                   plotOutput("plot.histM"),
                                   h4("How does the interval relate to the p-value when you change the sample size, null mean, and confidence level?"),
                                   plotOutput("plot.CIM")
                                   
                            )
                            )
                          
                  ),
                  tabItem(tabName = "refs",
                          withMathJax(),
                          h2("References"),
                          p(
                            class = "hangingindent",
                            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
                             (v0.61). [R package]. Available from
                            https://CRAN.R-project.org/package=shinyBS"
                          ),
                          p(
                            class = "hangingindent",
                            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
                             [R Package]. Available from
                              https://github.com/EducationShinyAppTeam/boastUtils"
                          ),
                          p(
                            class = "hangingindent",
                            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
                             dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
                             https://CRAN.R-project.org/package=shinydashboard"
                          ),
                          p(
                            class = "hangingindent",
                            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
                             (2019). shiny: Web application framework for R. (v1.4.0)
                             [R Package]. Available from https://CRAN.R-project.org/package=shiny"
                          ),
                          p(
                            class = "hangingindent",
                            "Fanaee-T, Hadi, and Gama, Joao (2013). Progress in Artificial Intelligence. Available from
                             https://www.kaggle.com/c/bike-sharing-demand/overview"
                          ),
                          p(
                            class = "hangingindent",
                            "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
                            [R Package]. Springer-Verlag New York. Available from
                              https://ggplot2.tidyverse.org"
                          )
                )
              )
)
)



