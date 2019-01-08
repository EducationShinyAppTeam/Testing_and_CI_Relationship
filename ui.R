
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(shinyBS)

#This was for trying to get a vetical slider
# js<-"$(function() {
#       var $elie = $(document.getElementsByClassName('form-group shiny-input-container'));
# rotate(270);
# function rotate(degree) {
# $elie.css({ WebkitTransform: 'rotate(' + degree + 'deg)'});
# $elie.css({ '-moz-transform': 'rotate(' + degree + 'deg)'});
# }
# });"

dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Testing and CI relationship", titleWidth = 250),
              
              #Sidebar
              dashboardSidebar(
                width = 250,
                sidebarMenu(id = "tabs",
                  
                  menuItem("Overview", tabName = "over", icon = icon("dashboard")),
                  menuItem("Choose a DataSet", tabName = "second", icon = icon("table")),
                  menuItem("App", tabName = "third", icon = icon("wpexplorer"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
                ),
                tags$style(
                  type = "text/css",
                  ".content-wrapper,.right-side {
                    background-color: white;
                  }"
                ),
                tabItems(
                  
                  tabItem(tabName = "over",
                          tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                          br(),br(),br(),
                          
                          h3(strong("About:")),
                          h4("In this app you will explore how a significance test relates to a confidence interval."),
                          br(),
                          h3(strong("Instructions:")),
                          h4(tags$li("Choose one of the preloaded datasets or input your own.")),
                          h4(tags$li("Go to the App and select the variable you would like to use. ")),
                          h4(tags$li("Set the Sample Size, Null Hypothesis, Alternative Hyptohesis, and Confidence Level. ")),
                          h4(tags$li("Follow the advice to explore the topic.")),
                          div(style = "text-align: center",bsButton("explore", "Explore", icon("bolt"), size = "large")),
                          br(),
                          h3(strong("Acknowledgements:")),
                          h4("This app was developed and coded by David Robinson and Yingjie (Chelsea) Wang, and updated by Yubaihe Zhou.")
                          
                  ),
                  tabItem(tabName = "second",
                          fluidPage(
                            div(style="display: inline-block;vertical-align:top;",
                                tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                            ),
                            titlePanel("Uploading Files"),
                            sidebarLayout(
                              sidebarPanel(
                                #Let them choose a preloaded dataset or input their own
                                #If it is preloaded output some information about the dataset
                                selectInput(inputId = "chooseM", "Select which data set you would like to use", choices = c("NHL Data" = "NHL","CEO Salaries" = "ceosal",InputYourOwn = "input")),
                                conditionalPanel("input.chooseM == 'NHL'",
                                                 "This data set was collected by statisticians working for the NHL over the course of the 2016-2017 NHL season. ",br(),br(),
                                                 "It is filtered to only include players who played at least half of the season (41 games). ",br(),br(),
                                                 "There are 526 players in this set", br(),br(),
                                                 "Player = Player's Name", br(),
                                                 "GP = Games Played",br(),
                                                 "G = Goals", br(),
                                                 "A = Assists",br(),
                                                 "PTS = Points",br(),
                                                 "PIM = Penalties in Minutes", br(),
                                                 "S = Shots on Goal", br(),
                                                 "TOI = Time on Ice", br(),
                                                 "BLK = Blocks at Even Strength",br(),
                                                 "HIT = Hits at Even Strength", br(),
                                                 "FOW = Faceoff Wins at Even Strength", br(),
                                                 "FOL = Faceoff Losses at Even Strenth"
                                ),
                                conditionalPanel("input.chooseM == 'ceosal'",
                                                 "In 1994 Forbes collected CEO data on 'America's Best Small Companies'.",br(),br(),
                                                 "Small companies were defined as those with annual sales greater than five and less than $350 million. Companies were ranked according to 5-year average return on investment. This data covers the first 60 ranked firms."
                                ),
                                conditionalPanel("input.chooseM == 'input'",
                                                 uiOutput("fileIncludeM"),
                                                 fileInput('file1', 'Choose Data File:',
                                                           accept=c('.csv', '.txt', '.xls',
                                                                    '.xlsx', '.sas7bdat')),
                                                 checkboxInput('header', 'Header', TRUE)
                                ) 
                                # Only show this panel if the plot type is a histogram
                                #uiOutput("varSelect")
                                
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
                  tabItem(tabName = "third",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                          ),
                          fluidRow(
                            withMathJax(),
                            column(4,
                                   h4("Conduct a test about the population mean"),
                                   uiOutput("var.selM"),
                                   uiOutput("size.selM"),
                                   uiOutput("null.selM"),
                                   #numericInput(inputId = "nullM", "Set the null hypothesis (The population mean is equal to ___)", value = 0, min = 0, max = 10000),
                                   #use mu not equal to the null mean
                                   radioButtons(inputId= "altM","Set the alternative hypothesis", choices = c("$$\\mu\\neq null mean$$ " = "choice1", "$$\\mu < null mean$$" = "choice2","$$\\mu > null mean$$" = "choice3")),
                                   #selectInput(inputId = "alt", "Set the alternative hypothesis",choices = c("$$\\mu\\neq null mean$$ ", "$$\\mu < H_0$$",NotEqualH0 = "notEqual")),
                                   tags$style(type = "text/css",
                                              "
                                              .irs-slider {width: 8px; height: 20px; top: 22px;}
                                              "),
                                   sliderInput(inputId = "conflev1M","Select the Confidence level:",min = 0,max = 99,value = 95),
                                   tableOutput("pvalueM")
                                   #tags$head(tags$style("#pvalueM{color: blue;font-size: 30px;font-style: bold;}")),
                                   ),
                            column(8,
                                   plotOutput("plot.histM"),
                                   h4("How does the interval relate to the p-value when you change the sample size, null mean, and confidence level."),
                                   plotOutput("plot.CIM")
                                   
                            )
                            )
                          
                  )
                  
                )
              )
)



