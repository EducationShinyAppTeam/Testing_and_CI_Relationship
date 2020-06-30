
library(shiny)
library(ggplot2)
library(DT)
library(stringr) 
library(openxlsx)
library(viridis)


#Read in the built in datasets
BikeSharing = read.csv("BikeSharing.csv",header=TRUE)  
NHLdata = read.csv("NHLplayerdata1617.csv",header = TRUE)
NHLdata = NHLdata[,-1]

boastPalette <- c("#0072B2","#D55E00","#009E73","#CE77A8",
                  "#000000","#E69F00","#999999","#56B4E9","#CC79A7")
psuPalette <- c("#1E407C","#BC204B","#3EA39E","#E98300",
                "#999999","#AC8DCE","#F2665E","#99CC00")

shinyServer(function(input, output,session) {
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "explore2")
  })
  #Means Test
  #reactive for how the user chooses the dataset they want to use
  dsM <- reactive({
    return(input$chooseM)
  })
  
  #reactive for what variable the user wants to use to make a test on
  vs <- reactive({
    return(input$var.selM)
  })
  
  #output information
  output$fileIncludeM <- renderUI({
    h3("CSV(.csv), TXT(.txt), Excel(.xlsx) dataset.")
  })
  
  #read in the dataset that they want to use
  # if statements for which they chose and then if statements for type of file if they are inputing their own
  readDataM <- reactive({
    ds1 = dsM()
    if (ds1 == "BikeSharing") {
      datafile <- BikeSharing
    }
    else if(ds1 =="NHL"){
      datafile <-NHLdata
    }
    else if (ds1 == "input"){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      
      if(length(grep(".txt", inFile,ignore.case = TRUE)) > 0){
        datafile <- read.table(inFile$datapath, header = input$header)
      }else if(length(grep(".csv", inFile, ignore.case = TRUE)) > 0){
        datafile <- read.csv(inFile$datapath,header = input$header, sep=",")
      }else if(length(grep(".xlsx", inFile, ignore.case = TRUE)) > 0){
        datafile <- read.xlsx(inFile$datapath, 1)
      }
    }
    
  })
  
  #reactive for the confidence level the user chooses
  clevM<-reactive({
    return(input$conflevM)
    
  })
  
  #reactive for the confidence level the user chooses
  clev1M<-reactive({
    return(input$conflev1M)
    
  })
  
  #reactive for the value of the null hypothesis that the user wants to test
  hM <-reactive({
    return(input$nullM)
  })
  
  #reactive for alternative hypothesis test type
  alt.testM <- reactive({
    return(input$altM)
  })
  
  
  
  # vals and observeEvent makes it so that the sample only changes when the user changes the sample size
  vals1M <- reactiveValues(sample1 = 0)
  
  observeEvent(input$size.sel1M, {
    ss = input$size.sel1M
    datafile <-readDataM()
    vals1M$sample1 = datafile[sample(nrow(datafile),ss),] #random select samples
  })
  
  #data table of the data file the user chose
  output$displayM <- renderDataTable({
    datafile <- readDataM()
    datatable(datafile)
  })
  
  #summary of the data the user chose
  output$summaryM <- renderPrint({
    datafile <- readDataM()
    summary(datafile)
    
  })
  
  #Output notice if the file the user uploaded is values
  output$MissingNoticeM <-renderUI({
    p("Notice: Missing data are marked as 'NA' and these rows of data will be removed while ploting and analysing")
  })
  
  #Render UI to have a select input based on something that happens on the server side 
  
  # renderUI to have a select input based on the columns the user chooses... this is the one I use for calculations and plots
  output$var.selM<- renderUI({
    datafile <- readDataM()
    items = names(datafile)
    if (input$chooseM == "BikeSharing"){
      selectInput("var.sel1M", "Select Variable", choices = list("Tempeture" = "Tempeture",
                                                                 "Humidity" = "Humidity",
                                                                 "Windspeed" = "Windspeed",
                                                                 "Total Rental" = "Total.Rental"))
      }
    else if (input$chooseM == "NHL"){
      selectInput("var.sel1M", "Select Variable", choices = list("Games Played" = "Games.Played",
                                                                 "Goals" = "Goals",
                                                                 "Assists" = "Assists",
                                                                 "Points" = "Points",
                                                                 "Penalties in Minutes" = "Penalties.in.Minutes",
                                                                 "Shots on Goal" = "Shots.on.Goal",
                                                                 "Time on Ice" = "Time.on.Ice",
                                                                 "Blocks at Even Strength" = "Blocks.at.Even.Strength",
                                                                 "Hits at Even Strength" = "Hits.at.Even.Strength",
                                                                 "Faceoff Wins at Even Strength" = "Faceoff.Wins.at.Even.Strength",
                                                                 "Faceoff Losses at Even Strenth" = "Faceoff.Losses.at.Even.Strenth")
      )}
    else {
      selectInput("var.sel1M", "Select Variable", items)
    }
  })
  
  # renderUI to have numeric input for the sample size where the max is the number of rows in the selected datafile
  output$size.selM <- renderUI({
    datafile <- readDataM()
    max1 = nrow(datafile)
    max2 = 200
    sliderInput("size.sel1M","Select a sample size", min = 30 , max = max2, val = 100)
  })
  
  output$null.selM <- renderUI({
    datafile <- readDataM()
    vs1 = input$var.sel1M
    m1 = mean(datafile[,vs1])
    m1 = round(m1,2)
    numericInput(inputId = "nullM", withMathJax("Set the null hypothesis (\\(\\mu\\) = )"), value = m1)
    
  })
  
  observeEvent(input$chooseM, {
    newsize = sample(10:58,1)
    updateNumericInput(session, inputId = "size.sel1M" , value = newsize)
  })
  
  #plot the histogram of the variable/dataset that is selected
  output$plot.histM <- renderPlot({
    
    ss = input$size.sel1M
    datafile <- vals1M$sample1 #random selected samples
    ds1 = dsM() #dataset that the user selected
    vs1 = c(input$var.sel1M)
    par(bg = "lightsteelblue")
    vs2 = toString(vs1)
    vs3 = str_replace_all(vs2, "\\.", " ") 
    qplot(datafile[,vs1],
          geom="histogram",
          xlab = paste(vs3),
          main = paste(vs3),
          col = I("black"),
          fill = I("#AC8DCE")
        ) + 
      theme(panel.background = element_rect(fill = 'white'))
  })
  
  #display the confidence interval of the variable/dataset that is selected
  output$plot.CIM <- renderPlot({
    alt1 = alt.testM()
    ss = input$size.sel1M
    datafile <- vals1M$sample1
    #ds1 = dsM()
    ds1 = readDataM()
    vs1 = input$var.sel1M
    h1 = hM()
    
    #make the confidence level a decimal so it can be used in calculations
    clevel <- clev1M()
    clevel = clevel/100
    
    #find the mean and standard deviation of the variable the user wants
    m1 = mean(datafile[,vs1])
    sd1 = sd(ds1[,vs1])
    n1 = ss
    se1 = sd1/sqrt(ss)
    
    #These are the upper and lower bounds of the confidence interval
    if(alt1 == 'choice1'){
      x1 = m1 - qnorm((clevel+1)/2)*se1
      x2 = m1 + qnorm((clevel+1)/2)*se1
    }
    else{
      x1 = m1 - qnorm(clevel)*se1
      x2 = m1 + qnorm(clevel)*se1
    }
    
    #This will be the window that I use for displaying the confidence interval
    windx1 = m1 - qnorm(0.99)*se1 - 2*se1
    windx2 = m1 + qnorm(0.99)*se1 + 2*se1
    
    #Need the matrix for plotting the line in the interval
    A = matrix(c(x1,m1,x2,20,20,20), nrow = 3, ncol = 2)
    par(bg = "lightsteelblue")
    #Put the interval at arbritary y values and only display the x
    par(bg = "white")
    vs1 = c(input$var.sel1M)
    vs2 = toString(vs1)
    vs3 = str_replace_all(vs2, "\\.", " ") 
    plot(NULL,
         xlim = c(min(datafile[,vs1]),
                  max(datafile[,vs1])),
         ylim = c(10,30),
         xlab = paste(vs3),
         type = 'l',
         ylab="",
         yaxt="n", 
         main = "Interval")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
           "white")
    
    #axis(side=2, at=seq(10, 30, by=10))
    lines(A,lwd = 2.5)
    #This is the point for the mean of the data
    points(m1,20, lwd = 10, pch = 19,col = "tomato")
    points(x1,20, lwd = 4, pch = "l")
    points(x2,20,lwd = 4, pch = "l")
    #this is the point for the H0
    points(h1,20,lwd = 10,pch = 19, col = "violet")
    #text(x,y,labels=names)
    text(m1,20,labels = "sample mean", pos = 3)
    text(h1,20,labels = "null mean", pos = 1)
  },height = 200)
  
  #Calculate the p value of the test.
  output$pvalueM <- renderTable({
    alt1 = alt.testM()
    ss = input$size.sel1M
   # ds1 = dsM()
    datafile <- vals1M$sample1
    ds1 = readDataM()
    vs1 = input$var.sel1M
    h1 = hM()
    
    m1 = mean(datafile[,vs1])
    
    #should be standard deviation of the whole population
    sd1 = sd(ds1[,vs1])
    n1 = ss
    se1 = sd1/sqrt(n1)
    
    z = (m1 - h1) / (se1)
    if(alt1 == "choice1"){
      #if samp mean less than hyp value then want it true
      if(m1<h1){
        p1 = 2 * pnorm(z)
      }
      else{
        p1 = 2* pnorm(z,lower.tail = FALSE)
      }
    }
    else if(alt1 == "choice2"){
      p1 = pnorm(z)
    }
    else{
      p1 = pnorm(z,lower.tail = FALSE)
    }
    
    #paste("P-value is  ", round(p1,4))
    ctable = matrix(c(z,p1),nrow=1)
    colnames(ctable) = c("z-statistic","p-value")
    ctable
    
  })
})