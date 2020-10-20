library(boastUtils)
library(shiny)
library(ggplot2)
library(DT)
library(DescTools)
library(english)
library(shinyWidgets)

# Read in the data sets ----
bikeSharing <- read.csv("BikeSharing.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
NHLdata <- read.csv("NHLplayerdata1617.csv", header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

# Define color palettes ----
boastPalette <- c("#0072B2","#D55E00","#009E73","#CE77A8",
                  "#000000","#E69F00","#999999","#56B4E9","#CC79A7")
psuPalette <- c("#1E407C","#BC204B","#3EA39E","#E98300",
                "#999999","#AC8DCE","#F2665E","#99CC00")


# Create the shiny server ----
shinyServer(function(input, output,session) {
  ## Explore Button ----
  observeEvent(input$explore, {
    updateTabItems(session, "pages", "explore2")
  })

  ## The information button ----
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Choose a data set to explore and see what will affect the testing result and confidence interval.",
      type = "info"
    )
  })

  ## Select Data Set ----
  selectedData <- reactive({
    if(input$pickData == "NHL") {
      temp <- NHLdata
    } else if (input$pickData == "bikeSharing") {
      temp <- bikeSharing
    } else {
      temp <- iris
    }
    # The follow lines remove columns with character values and factors
    # This can be extended to also include dates.
    temp <- temp[, !sapply(temp, is.character)]
    temp <- temp[, !sapply(temp, is.factor)]
    return(temp)
  })

  ## Make Attribute List ----
  ### This will remove periods, replacing with spaces, and will automatically
  ### work with ANY data set that gets loaded and selected
  output$pickVar <- renderUI({
    nameList <- list()
    for (i in seq_len(length(names(selectedData())))){
      tempName <- gsub("\\.", " ", names(selectedData())[i])
      nameList[[tempName]] <- names(selectedData())[i]
    }
    selectInput(
      inputId = "selectedVar",
      label = "Select an attribute",
      choices = nameList
    )
  })

  ## Update upper bound of sample size slider ----
  ### Note: we aren't revising the lower bound so there must be at least 30 obs.
  observeEvent(selectedData(),{
    updateSliderInput(
      session = session,
      inputId = "sampleSize",
      max = min(nrow(selectedData()), 200)
    )
  })

  ## Set initial null hypothesis value ----
  observeEvent(input$selectedVar, {
    updateNumericInput(
      session = session,
      inputId = "nullValue",
      value = round(mean(selectedData()[, input$selectedVar]) ,2)
    )
  })

  ## Create the sampled data frame
  sampledData <- reactive({
    ### Construct a temporary data set while elements are still loading
    if(length(selectedData()) < 1 || is.null(input$selectedVar)){
      return(data.frame(x = rnorm(30, mean = 0, sd = 1)))
    } else {
      return(data.frame(x = sample(selectedData()[, input$selectedVar],
                            size = ifelse(is.null(input$sampleSize),
                                          30,
                                          input$sampleSize))))
    }
  })

  ## Make the sample histogram ----
  output$sampleHist <- renderPlot({
    if(is.null(input$selectedVar) ||
       !(input$selectedVar %in% names(selectedData()))){
      # This is a catch to suppress warning messages
    } else {
    ggplot2::ggplot(data = sampledData(), mapping = aes(x = x)) +
      ggplot2::geom_histogram(
        # Using the Freedman-Diaconis Rule for bin widths
        binwidth = function(x){ifelse(IQR(x) == 0, 0.1,
                                      2 * IQR(x) / (length(x)^(1/3)))},
        color = "black",
        fill = psuPalette[6],
        closed = "left",
        boundary = 0) +
      ggplot2::theme_bw() +
      xlab(gsub("\\."," ", input$selectedVar)) +
      ggtitle(paste("Histogram of", input$sampleSize, "Observations")) +
      ggplot2::theme(
        plot.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
      ) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(0, 0.1), add = 0))
    }
  })

  ## Run the Z Test ----
  zTest <- reactive({
    nhst <- DescTools::ZTest(x = sampledData()$x,
                             alternative = input$altHyp,
                             mu = input$nullValue,
                             # Using full data set as population proxy
                             # use a catch to prevent an error
                             sd_pop = ifelse(is.null(input$selectedVar),
                                             1,
                                             sd(selectedData()[,input$selectedVar])),
                             conf.level = input$confLevel / 100)
    results <- data.frame(
      "z" = round(nhst$statistic, 3),
      "p-value" = round(nhst$p.value, 4)
    )
    names(results) <- c("Z statistic", "p-value")
    return(results)
  })

  ## Make the Hypothesis Test Table ----
  output$nhsTest <- renderTable(
    expr = zTest()
  )

  ## Make the Confidence Interval Plot ----
  output$confInt <- renderPlot({
    if(is.null(input$selectedVar) ||
       !(input$selectedVar %in% names(selectedData()))){
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
      ciPlot <- ggplot2::ggplot(data = sampledData(), mapping = aes(x = x)) +
        ggplot2::theme_bw() +
        xlab(gsub("\\."," ", input$selectedVar)) +
        ggtitle(paste(english::Words(input$confLevel),
                      "Percent Confidence Interval")) +
        ggplot2::theme(
          plot.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 14)
        ) +
        ggplot2::scale_x_continuous(expand = expansion(mult = 0.1, add = 0)) +
        ggplot2::scale_y_continuous(limit = c(0.95, 1.05)) +
        ggplot2::scale_color_manual(name = "",
                                  values = c("Sample Arithmetic Mean" = psuPalette[6],
                                             "Null Value" = psuPalette[4]))

      ### Add the interval
      if(input$altHyp == "less") {
        ciPlot <- ciPlot + ggplot2::geom_segment(
          aes(x = lowerBound, y = 1, xend = upperBound, yend = 1),
          arrow = arrow(ends = "first", type = "closed"),
          lineend = "square", linejoin = "mitre", size = 1.5, na.rm = TRUE) +
          ggplot2::geom_segment(
            aes(x = upperBound, y = 0.975, xend = upperBound, yend = 1.025),
            lineend = "square", linejoin = "mitre", size = 1.5, na.rm = TRUE
          )
      } else if (input$altHyp == "greater") {
        ciPlot <- ciPlot + ggplot2::geom_segment(
          aes(x = lowerBound, y = 1, xend = upperBound, yend = 1),
          arrow = arrow(ends = "last", type = "closed"),
          lineend = "square", linejoin = "mitre", size = 1.5, na.rm = TRUE) +
          ggplot2::geom_segment(
            aes(x = lowerBound, y = 0.975, xend = lowerBound, yend = 1.025),
            lineend = "square", linejoin = "mitre", size = 1.5, na.rm = TRUE
          )
      } else {
        ciPlot <- ciPlot + ggplot2::geom_segment(
          aes(x = lowerBound, y = 1, xend = upperBound, yend = 1),
          lineend = "square", linejoin = "mitre", size = 1.5, na.rm = TRUE) +
          ggplot2::geom_segment(
            aes(x = upperBound, y = 0.975, xend = upperBound, yend = 1.025),
            lineend = "square", linejoin = "mitre", size = 1.5, na.rm = TRUE
          ) +
          ggplot2::geom_segment(
            aes(x = lowerBound, y = 0.975, xend = lowerBound, yend = 1.025),
            lineend = "square", linejoin = "mitre", size = 1.5, na.rm = TRUE
          )
      }

      ### Add Sample and Null points
      ciPlot + ggplot2::geom_point(aes(x = mean(sampledData()$x),
                                       y = ifelse(abs(mean(sampledData()$x) -
                                                        input$nullValue) <= 0.2,
                                                  1.01, 1),
                                       color = "Sample Arithmetic Mean"),
                                   size = 7, na.rm = TRUE) +
        ggplot2::geom_point(aes(x = input$nullValue,
                                y = ifelse(abs(mean(sampledData()$x) -
                                                 input$nullValue) <= 0.2,
                                           0.99, 1),
                                color = "Null Value"),
                            size = 7, na.rm = TRUE)
    }
  },
  ### Restrict the height of the plot.
  height = 200
  )

  # Older Code ----

  #summary of the data the user chose
  output$summaryM <- renderPrint({
    datafile <- selectedData()
    summary(datafile)
  })
  
  #Alt text for histogram
  output$histText <- renderText({
    paste("See the shape of the histpgram above.")
  })
})