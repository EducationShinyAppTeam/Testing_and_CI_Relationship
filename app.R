# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(boastUtils)
library(ggplot2)
library(DT)
library(english)
library(DescTools)

# Define global constants, functions, and load data ----
bikeSharing <- read.csv(
  file = "BikeSharing.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)
NHLdata <- read.csv(
  file = "NHLplayerdata1617.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

cleanVariableNames <- function(varName){
  if (grepl(pattern = "\\.\\.C\\.", x = varName)) {
    return(
      gsub(
        pattern = "\\.\\.C\\.",
        replacement = " \\(C\\)",
        x = varName
      )
    )
  } else if (grepl(pattern = "\\.km\\.h\\.", x = varName)) {
    return(
      gsub(
        pattern = "\\.km\\.h\\.",
        replacement = " \\(km\\/h\\)",
        x = varName
      )
    )
  } else {
    return(
      gsub(
        pattern = "\\.",
        replacement = " ",
        x = varName
      )
    )
  }
}

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "purple",
    ## Header ----
    dashboardHeader(
      title = "NHST and CIs",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Testing_and_CI_Relationship"
        )
      ),
      tags$li(class = "dropdown",
              tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home")))
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "over", icon = icon("tachometer-alt")),
        menuItem("Explore Relationship", tabName = "explore2", icon = icon("wpexplorer")),
        menuItem("References", tabName = "refs", icon = icon("leanpub"))),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview ----
        tabItem(
          tabName = "over",
          h1("The Relationship Between Null Hypothesis Significance Testing and
             Confidence Intervals"),
          p("In this app you will explore how a significance test relates to a
          confidence interval."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Pick a data set and attribute to test."),
            tags$li("Set up the test and see the result.")
          ),
          div(style = "text-align: center;",
              bsButton(
                inputId = "explore",
                label = "Explore",
                icon = icon("bolt"),
                size = "large")),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by David Robinson and Yingjie
          (Chelsea) Wang. This application was modified by Yubaihe Zhou in 2019
          as well as Gonghao Liu and Neil Hatfield in 2020.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/10/2020 by NJH."))
        ),
        ### Exploration Page ----
        tabItem(
          tabName = "explore2",
          withMathJax(),
          h2("Null Hypothesis Tests and Confidence Intervals"),
          p(
            "There is a relationship between null hypothesis tests and confidence
            intervals for the chosen parameter. To explore this, you will first
            need to select a real world data set to use and then an attribute to
            test."
          ),
          h3("Step 1: Pick a Data Set and Attribute to Test"),
          fluidRow(
            column(
              width = 5,
              offset = 1,
              selectInput(
                inputId = "pickData",
                label = "Pick a data set",
                choices = c(
                  "NHL Data" = "NHL",
                  "Bike Sharing" = "bikeSharing"
                )
              )
            ),
            column(
              width = 6,
              offset = 0,
              uiOutput("pickVar")
            )
          ),
          uiOutput("dataInfo"),
          br(),
          box(
            title = "Summary Statistics",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            p(
              "The following table displays Tukey's Five Number Summaries plus the
            values of each attribute in the data set."),
            DT::dataTableOutput("summaryData")
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
                  choices = list(
                    "\\(\\mu\\neq\\mu_0\\)" = "two.sided",
                    "\\(\\mu<\\mu_0\\)" = "less",
                    "\\(\\mu>\\mu_0\\)" = "greater"
                  )
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
              htmlOutput("histAria"),
              p(
                "Look at the shape of the histogram above and ask yourself the
                following question: How does the interval shown below relate to
                the p-value when you change the sample size, null mean, and
                confidence level?"
              ),
              h4("Hypothesis Test"),
              tableOutput("nhsTest"),
              h4("Confidence Interval"),
              plotOutput(outputId = "confInt", height = "200px"),
              htmlOutput("ciAria"),
              p("Is there an arrow showing in the graph above? If so, what in
                which direction is the arrow pointing? How does this relate to
                the hypotheses in the test?")
            )
          )
        ),
        ### References ----
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
            "Fanaee-Tork, H., and Gama, J. (2013). Progress in Artificial
          Intelligence. Available from
          https://www.kaggle.com/c/bike-sharing-demand/overview"
          ),
          p(
            class = "hangingindent",
            "Fox, J., Venables, B., Damico, A., and Pier Salverda, A.
          (2020). english: Translate Integers into English. [R Package].
          Available from https://CRAN.R-project.org/package=english"
          ),
          p(
            class = "hangingindent",
            "Signorell, A., et al. (2020). DescTools: Tools for descriptive
            statistics. [R package]. Available from
            https://cran.r-project.org/package=DescTools"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
          [R Package]. Springer-Verlag New York. Available from
          https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2020). DT: A Wrapper of
          the JavaScript Library 'DataTables'. [R Package]. Available from
          https://CRAN.R-project.org/package=DT"
          )
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output,session) {
  ## Explore Button ----
  observeEvent(input$explore, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "explore2"
    )
  })

  ## The information button ----
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "Choose a data set to explore and see what will affect the testing
             result and confidence interval.",
      type = "info"
    )
  })

  ## Display data context ----
  observeEvent(
    eventExpr = input$pickData,
    handlerExpr = {
      output$dataInfo <- renderUI({
        switch(
          EXPR = input$pickData,
          "NHL" = "This data set was collected by statisticians working for the
                   NHL over the course of the 2016-2017 NHL season. It is filtered
                   to only include players who played at least half of the season
                   (41 games). There are 526 players in this set.",
          "bikeSharing" = "This data set is about the Capital Bikeshare program
                           in Washington, D.C., during 2011 to 2012. There are
                           10,886 rows in the dataset, and each row represents
                           an hour.",
          "Please select a data set."
        )
      })
    }
  )

  ## Select Data Set ----
  selectedData <- eventReactive(
    eventExpr = input$pickData,
    valueExpr = {
      temp <- switch(
        EXPR = input$pickData,
        "NHL" = NHLdata,
        "bikeSharing" = bikeSharing,
        "error"
      )
      # The follow lines remove columns with character values and factors
      temp <- temp[, !sapply(temp, is.character)]
      temp <- temp[, !sapply(temp, is.factor)]
      return(temp)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Make Attribute List ----
  observeEvent(
    eventExpr = selectedData(),
    handlerExpr = {
      tempNameList <- lapply(X = names(selectedData()), cleanVariableNames)
      nameList <- list()
      for (i in 1:(length(tempNameList))) {
        nameList[[tempNameList[[i]]]] = names(selectedData())[i]
      }
      output$pickVar <- renderUI({
        selectInput(
          inputId = "selectedVar",
          label = "Select an attribute",
          choices = nameList
        )
      })
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Update upper bound of sample size slider ----
  ### Note: we aren't revising the lower bound so there must be at least 30 obs.
  observeEvent(
    eventExpr = selectedData(),
    handlerExpr = {
      updateSliderInput(
        session = session,
        inputId = "sampleSize",
        max = min(nrow(selectedData()), 200)
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Set initial null hypothesis value ----
  observeEvent(
    eventExpr = input$selectedVar,
    handlerExpr = {
      updateNumericInput(
        session = session,
        inputId = "nullValue",
        value = round(mean(selectedData()[, input$selectedVar]), digits = 2)
    )
  })

  test <- eventReactive(
    eventExpr = input$pickData,
    valueExpr = input$selectedVar
  )

  ## Create the sampled data frame
  sampledData <- eventReactive(
    eventExpr = c(input$selectedVar, isolate(selectedData())),
    valueExpr = {
      if (length(selectedData()) < 1 || is.null(input$selectedVar)) {
        return(data.frame(x = rnorm(30, mean = 0, sd = 1)))
      } else {
        return(
          data.frame(
            x = sample(
              x = selectedData()[, input$selectedVar],
              size = ifelse(
                test = is.null(input$sampleSize),
                yes = 30,
                no = input$sampleSize
              ),
              replace = FALSE
            )
          )
        )
      }
    }
  )

  ## Make the sample histogram ----
  output$sampleHist <- renderPlot({
    if (is.null(input$selectedVar) ||
       !(input$selectedVar %in% names(selectedData()))) {
      # This is a catch to suppress warning messages
    } else {
      ggplot(
        data = sampledData(),
        mapping = aes(x = x)
      ) +
        geom_histogram(
          # Using the Freedman-Diaconis Rule for bin widths
          binwidth = function(x){
            ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))
          },
          color = "black",
          fill = psuPalette[6],
          closed = "left",
          boundary = 0
        ) +
        theme_bw() +
        xlab(cleanVariableNames(input$selectedVar)) +
        ylab("Frequency") +
        ggtitle(paste("Histogram of", input$sampleSize, "Observations")) +
        theme(
          plot.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1), add = 0))
    }
  })

  ## Run the Z Test ----
  zTest <- eventReactive(
    eventExpr = c(input$selectedVar, sampledData(), input$nullValue),
    valueExpr = {
      nhst <- DescTools::ZTest(
        x = sampledData()$x,
        alternative = input$altHyp,
        mu = input$nullValue,
        # Using full data set as population proxy
        # use a catch to prevent an error
        sd_pop = ifelse(
          test = is.null(input$selectedVar),
          yes = 1,
          no = sd(selectedData()[,input$selectedVar])
        ),
        conf.level = input$confLevel / 100
      )
      results <- data.frame(
        "z" = round(nhst$statistic, 3),
        "p-value" = round(nhst$p.value, 4)
      )
      names(results) <- c("Z statistic", "p-value")
      return(results)
    }
  )

  ## Make the Hypothesis Test Table ----
  output$nhsTest <- renderTable(
    expr = zTest()
  )

  ## Make the Confidence Interval Plot ----
  output$confInt <- renderPlot({
    if (is.null(input$selectedVar) ||
       !(input$selectedVar %in% names(selectedData()))) {
      # This is a catch to suppress warning messages
    } else {
      se <- sd(selectedData()[, input$selectedVar]) / sqrt(input$sampleSize)
      ### Construct confidence interval bounds
      if (input$altHyp == "less") {
        #### Set lower bound to always be less than the null value
        lowerBound = min(sampledData()$x, input$nullValue) - 2
        upperBound = mean(sampledData()$x) + qnorm(p = input$confLevel / 100,
                                                   mean = 0,
                                                   sd = 1) * se
      } else if (input$altHyp == "greater") {
        lowerBound = mean(sampledData()$x) - qnorm(p = input$confLevel / 100,
                                                   mean = 0,
                                                   sd = 1) * se
        #### Set upper bound to always be greater than the null value
        upperBound = max(sampledData()$x, input$nullValue) + 2
      } else {
        lowerBound = mean(sampledData()$x) - qnorm(
          p = 1 - (1 - input$confLevel / 100) / 2,
          mean = 0, sd = 1) * se
        upperBound = mean(sampledData()$x) + qnorm(
          p = 1 - (1 - input$confLevel / 100) / 2,
          mean = 0, sd = 1) * se
      }

      ### Base plot
      ciPlot <- ggplot(
        ata = sampledData(),
        mapping = aes(x = x)
      ) +
        theme_bw() +
        xlab(cleanVariableNames(input$selectedVar)) +
        ggtitle(paste(english::Words(input$confLevel),
                      "Percent Confidence Interval")) +
        theme(
          plot.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 14)
        ) +
        scale_x_continuous(expand = expansion(mult = 0.1, add = 0)) +
        scale_y_continuous(limits = c(0.95, 1.05)) +
        scale_color_manual(
          name = "",
          values = c(
            "Sample Arithmetic Mean" = psuPalette[6],
            "Null Value" = psuPalette[4]
          )
        )

      ### Add the interval
      if (input$altHyp == "less") {
        ciPlot <- ciPlot + geom_segment(
          mapping = aes(
            x = lowerBound,
            y = 1,
            xend = upperBound,
            yend = 1
          ),
          arrow = arrow(ends = "first", type = "closed"),
          lineend = "square",
          linejoin = "mitre",
          size = 1.5,
          na.rm = TRUE
        ) +
          geom_segment(
            mapping = aes(
              x = upperBound,
              y = 0.975,
              xend = upperBound,
              yend = 1.025
            ),
            lineend = "square",
            linejoin = "mitre",
            size = 1.5,
            na.rm = TRUE
          )
      } else if (input$altHyp == "greater") {
        ciPlot <- ciPlot + geom_segment(
          mapping = aes(
            x = lowerBound,
            y = 1,
            xend = upperBound,
            yend = 1
          ),
          arrow = arrow(ends = "last", type = "closed"),
          lineend = "square",
          linejoin = "mitre",
          size = 1.5,
          na.rm = TRUE
        ) +
          geom_segment(
            mapping = aes(
              x = lowerBound,
              y = 0.975,
              xend = lowerBound,
              yend = 1.025
            ),
            lineend = "square",
            linejoin = "mitre",
            size = 1.5,
            na.rm = TRUE
          )
      } else {
        ciPlot <- ciPlot + geom_segment(
          mapping = aes(
            x = lowerBound,
            y = 1,
            xend = upperBound,
            yend = 1
          ),
          lineend = "square",
          linejoin = "mitre",
          size = 1.5,
          na.rm = TRUE
        ) +
          geom_segment(
            mapping = aes(
              x = upperBound,
              y = 0.975,
              xend = upperBound,
              yend = 1.025
            ),
            lineend = "square",
            linejoin = "mitre",
            size = 1.5,
            na.rm = TRUE
          ) +
          geom_segment(
            mapping = aes(
              x = lowerBound,
              y = 0.975,
              xend = lowerBound,
              yend = 1.025
            ),
            lineend = "square",
            linejoin = "mitre",
            size = 1.5,
            na.rm = TRUE
          )
      }

      ### Add Sample and Null points
      ciPlot + geom_point(
        mapping = aes(
          x = mean(sampledData()$x),
          y = ifelse(
            test = abs(mean(sampledData()$x) - input$nullValue) <= 0.2,
            yes = 1.01,
            no = 1
          ),
          color = "Sample Arithmetic Mean"
        ),
        size = 7,
        na.rm = TRUE
      ) +
        geom_point(
          mapping = aes(
            x = input$nullValue,
            y = ifelse(
              test = abs(mean(sampledData()$x) - input$nullValue) <= 0.2,
              yes = 0.99,
              no = 1
            ),
            color = "Null Value"
          ),
          size = 7,
          na.rm = TRUE
        )
    }
  },
  ### Restrict the height of the plot.
  height = 200
  )

  ## Summary Tables ----
  output$summaryData <- DT::renderDataTable(
    expr = {
      temp1 <- as.data.frame(
        sapply(
          X = selectedData(),
          FUN = summary,
          digits = 4
        )
      )
      names(temp1) <- lapply(X = names(temp1), cleanVariableNames)
      return(temp1)
    },
    caption = paste(
      "Descriptive Statistics for",
      ifelse(
        test = input$pickData == "NHL",
        yes = "NHL Data",
        no = "Bike Sharing Data"
      )
    ),
    style = "bootstrap4",
    rownames = TRUE,
    autoHideNavigation = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = "dt-center", targets = 1:length(names(selectedData())))
      )
    )
  )

  ## ARIA Labels ----
  observeEvent(
    eventExpr = c(input$selectedVar, input$sampleSize, input$altHyp, input$confLevel),
    handlerExpr = {
      output$histAria <- renderUI({
        tags$script(HTML(
          paste0(
            "$(document).ready(function() {
            document.getElementById('sampleHist').setAttribute('aria-label',
            `This histogram shows a random sample of ", input$sampleSize,
            " observations of the ", cleanVariableNames(input$selectedVar),
            " attribute.`)})"
          )
        ))
      })

      output$ciAria <- renderUI({
        tags$script(HTML(
          paste0(
            "$(document).ready(function() {
            document.getElementById('confInt').setAttribute('aria-label',
            `This plot shows the confidence interval associated with the null
            hypothesis test you set up using a random sample of ",
            input$sampleSize, " observations of the ",
            cleanVariableNames(input$selectedVar)," attribute. You set the
            confidence level at ", english::words(input$confLevel), "percent
            level and chose an alternative hypothesis which is ",
            cleanVariableNames(input$altHyp), ".`)})"
          )
        ))
      })
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

}

# Boast app call ----
boastUtils::boastApp(ui = ui, server = server)