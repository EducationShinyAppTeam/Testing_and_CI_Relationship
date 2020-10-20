library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(shinyBS)
library(boastUtils)

dashboardPage(
  skin="purple",
  # Header ----
  dashboardHeader(
    title="Testing and CI Relationship",
    titleWidth = 250,
    tags$li(class = "dropdown", actionLink("info", icon("info"))),
    tags$li(class = "dropdown",
            tags$a(href='https://shinyapps.science.psu.edu/', icon("home"))),
    tags$li(
      class = "dropdown",
      tags$a(target = "_blank", icon("comments"),
             href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=[Testing and CI Relationship]"
      )
    )
  ),
  # Sidebar ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Overview", tabName = "over", icon = icon("tachometer-alt")),
      menuItem("Testing and CI Relationship", tabName = "explore2",
               icon = icon("wpexplorer")),
      menuItem("References", tabName = "refs", icon = icon("leanpub"))),
    tags$div(class = "sidebar-logo", boastUtils::psu_eberly_logo("reversed"))
  ),
  # Body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    ),
    ## Overview ----
    tabItems(
      tabItem(
        tabName = "over",
        h1("Testing and Confidence Relationship"),
        p("In this app you will explore how a significance test relates to a
          confidence interval."),
        br(),
        h2("Instructions"),
        tags$ol(
          tags$li("Pick a data set and attribute to test."),
          tags$li("Set up the test and see the result.")
        ),
        div(style = "text-align: center",
            bsButton(
              inputId = "explore",
              label = "Explore",
              icon = icon("bolt"),
              size = "large")),
        br(),
        h2("Acknowledgements"),
        p("This app was developed and coded by David Robinson and Yingjie
          (Chelsea) Wang. This application was modified by Yubaihe Zhou in 2019
          and Gonghao Liu in 2020.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 6/28/2020 by GL."))
      ),
     
      # Exploration Page ----
      tabItem(
        tabName = "explore2",
        withMathJax(),
        h2("Null Hypothesis Tests and Confidence Intervals"),
        # This is my preference: Drop the Choose a Data Set Tab entirely
        h3("Step 1: Pick a Data Set and Attribute to Test"),
        fluidRow(
          column(
            width = 4,
            offset = 1,
            selectInput(
              inputId = "pickData",
              label = "Pick a data set",
              choices = c("NHL Data" = "NHL",
                          "Bike Sharing" = "bikeSharing") # this is testing an else condition
            )
          ),
          column(
            width = 5,
            offset = 1,
            uiOutput("pickVar")
          )
        ),
        conditionalPanel(
          condition = "input.pickData == 'NHL'",
          "This data set was collected by statisticians working for the NHL over
          the course of the 2016-2017 NHL season. It is filtered to only include
          players who played at least half of the season (41 games). There are
          526 players in this set."
        ),
        conditionalPanel(
          condition = "input.pickData == 'bikeSharing'",
          "This Dataset is about Capital Bikeshare program in Washington, D.C.,
          during 2011 to 2012. There are 10,886 rows in the dataset, each row 
          represents an hour."
        ),
        column(
          h3("Summary"),
          width = 10,
          offset = 1,
          verbatimTextOutput('summaryM')
        ),
        br(),
        fluidRow(
          column(
            width = 4,
            wellPanel(
              h3("Step 2: Set up the Test"),
              sliderInput(
                inputId = "sampleSize",
                label = "Select a sample size",
                min = 30,
                max = 200,
                step = 1,
                value = 100
              ),
              numericInput(
                inputId = "nullValue",
                label = "Set the null hypothesis value, \\(\\mu_0\\)",
                value = 0
              ),
              radioButtons(
                inputId = "altHyp",
                label = "Set the alternative hypothesis",
                choices = list("\\(\\mu\\neq\\mu_0\\)" = "two.sided",
                            "\\(\\mu<\\mu_0\\)" = "less",
                            "\\(\\mu>\\mu_0\\)" = "greater")
              ),
              sliderInput(
                inputId = "confLevel",
                label = "Select the confidence level",
                min = 85,
                max = 99,
                step = 1,
                value = 95,
                post = "%"
              )
            )
          ),
          column(
            width = 8,
            h3("Results"),
            plotOutput("sampleHist"),
            p("See the shape of the histpgram above and ask yourself a question: 
              How does the interval relate to the p-value when you change the
              sample size, null mean, and confidence level?"),
            h4("Hypothesis Test"),
            tableOutput("nhsTest"),
            h4("Confidence Interval"),
            plotOutput("confInt"),
            p("Is there any arrow shows in the graph above? if so, what is the direction?")
          )
        )
      ),
      ## References ----
      tabItem(
        tabName = "refs",
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
          "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package].
          Available from https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
          dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
          https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019).
          shiny: Web application framework for R. (v1.4.0) [R Package].
          Available from https://CRAN.R-project.org/package=shiny"
        ),
        p(
          class = "hangingindent",
          "Fanaee-T, Hadi, and Gama, Joao (2013). Progress in Artificial
          Intelligence. Available from
          https://www.kaggle.com/c/bike-sharing-demand/overview"
        ),
        p(
          class = "hangingindent",
          "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
          [R Package]. Springer-Verlag New York. Available from
          https://ggplot2.tidyverse.org"
        ),
        p(
          class = "hangingindent",
          "Yihui Xie, Joe Cheng and Xianying Tan (2020). DT: A Wrapper of 
          the JavaScript Library 'DataTables'. [R Package]. Available from
          https://CRAN.R-project.org/package=DT"
        ),
        p(
          class = "hangingindent",
          "Andri Signorell et mult. al. (2020). DescTools: Tools for 
          descriptive statistics. [R Package]. Available from
          https://cran.r-project.org/package=DescTools"
        ),
        p(
          class = "hangingindent",
          "John Fox, Bill Venables, Anthony Damico and Anne Pier Salverda
          (2020). english: Translate Integers into English. [R Package]. 
          Available from https://CRAN.R-project.org/package=english"
        )
      )
    )
  )
)