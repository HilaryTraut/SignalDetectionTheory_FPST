# NOTE: Some code cannibalized from "https://r.tquant.eu/GrazApps/Group7_SignalDetection/" 

#pacman::p_load(shiny, shinydashboard)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)

#################################################################################################################
#FUNCTIONS
################################################################################################################
sdtTable <- function(hit, miss, FA, cr){
    #calculate the rates (probability) from the entered absolute values
    hitrate1B <- hit/(hit+miss)
    missrate1B <- miss/(hit+miss)
    farate1B <- FA/(FA+cr)
    crrate1B <- cr/(FA+cr)
    
    #add them to seperate lists and create dataframe
    Rainy1B <- c(hitrate1B, missrate1B)
    Sunny1B <- c(farate1B, crrate1B)
    
    Weather1B <-c("Shoot","No Shoot")
    rates1B <- data.frame(Weather1B, Rainy1B, Sunny1B)
    
    colnames(rates1B) <- c("", "Armed", "Unarmed")
    rates1B
}

zscoreTable <- function(hit, miss, FA, cr){
    #calculate the rates (probability) from the entered absolute values
    hitrate1B<- hit/(hit+miss)
    farate1B <- FA/(FA+cr)
    
    if (hitrate1B == 1) {
        hitrate1B <- 1 - 1/(2*(hit+miss))
    } else if (hitrate1B == 0) {
        hitrate1B <- 0 + 1/(2*(hit+miss))
    } else {
        hitrate1B <- hit/(hit+miss)
    }
    if (farate1B == 0) {
        farate1B <- 0 + 1/(2*(FA+cr))
    } else if (farate1B == 1) {
        farate1B <- 1 - 1/(2*(FA+cr))
    } else {
        farate1B <- FA/(FA+cr)
    }
    
    zHit1B <- qnorm(hitrate1B)
    zFA1B <- qnorm(farate1B)
    dPrime1B <- zHit1B - zFA1B
    Crit1B = -0.5*(zHit1B + zFA1B)
    
    #add them to seperate lists and create dataframe
    zrate1B <- data.frame(zHit1B, zFA1B, dPrime1B, Crit1B)
    colnames(zrate1B) <- c("z Hit", "z False Alarm", "dPrime", "Criterion")
    zrate1B    
}

denPlot <- function(hit, miss, FA, cr){
    #Pre-calculations
    hitrate1B<- hit/(hit+miss)
    farate1B <- FA/(FA+cr)
    
    if (hitrate1B == 1) {
        hitrate1B <- 1 - 1/(2*(hit+miss))
    } else if (hitrate1B == 0) {
        hitrate1B <- 0 + 1/(2*(hit+miss))
    } else {
        hitrate1B <- hit/(hit+miss)
    }
    if (farate1B == 0) {
        farate1B <- 0 + 1/(2*(FA+cr))
    } else {
        farate1B <- FA/(FA+cr)
    }
    
    zHit1B <- qnorm(hitrate1B)
    zFA1B <- qnorm(farate1B)
    dPrime1B <- zHit1B - zFA1B
    Crit1B = -0.5*(zHit1B + zFA1B)
    
    FAR1B <- seq(-5,10,length=1000)
    HR1B <- seq(-5,10,length=1000)
    
    # get normal probability density functions
    dFAR1B <- dnorm(FAR1B,mean=0,sd=1)
    dHR1B <- dnorm(HR1B,mean=dPrime1B,sd=1) # sd=1 for equal variance SD
    
    # draw the density function + line for criterion
    bg = "Grey"
    plot(FAR1B, dFAR1B, type="l", col='blue', xlab="", ylab="", ylim=c(0,.5), lwd=2) # FAR distribution
    par(new=T)
    plot(HR1B, dHR1B, type="l", col='red', axes=F, xlab="Signal Strength", ylab="Normal probability density function", ylim=c(0,.5), lwd=2) # HR distribution
    abline(v=Crit1B,lty=3, lwd=2) # dotted line for criterion
    polygon(c(FAR1B[FAR1B>=Crit1B], Crit1B), c(dFAR1B[FAR1B>=Crit1B], 0), col="gray") #shading for False alarm
    legend("topright",legend=c("False Alarm Rate (noise only)","Hit Rate (signal + noise)"),col=c("blue","red"),lty=1)
    par(new=F)
}
rocPlot <- function(hit, miss, FA, cr){
    #Pre-calculations
    hitrate1B<- hit/(hit+miss)
    farate1B <- FA/(FA+cr)
    hitrate1B<- hit/(hit+miss)
    farate1B <- FA/(FA+cr)
    
    if (hitrate1B == 1) {
        hitrate1B <- 1 - 1/(2*(hit+miss))
    } else if (hitrate1B == 0) {
        hitrate1B <- 0 + 1/(2*(hit+miss))
    } else {
        hitrate1B <- hit/(hit+miss)
    }
    if (farate1B == 0) {
        farate1B <- 0 + 1/(2*(FA+cr))
    } else {
        farate1B <- FA/(FA+cr)
    }
    
    zHit1B <- qnorm(hitrate1B)
    zFA1B <- qnorm(farate1B)
    dPrime1B <- zHit1B - zFA1B
    Crit1B = -0.5*(zHit1B + zFA1B)
    
    FAR1B <- seq(-5,10,length=1000)
    HR1B <- seq(-5,10,length=1000)
    
    # get response probabilities for each distribution
    pFAR <- 1-pnorm(FAR1B,mean=0,sd=1)
    pHR <- 1-pnorm(HR1B,mean=dPrime1B,sd=1)
    
    # get response probabilities at criterion
    pFAR.crit <- 1-pnorm(Crit1B,mean=0,sd=1)
    pHR.crit <- 1-pnorm(Crit1B,mean=dPrime1B,sd=1)
    
    # draw the ROC + dot for criterion + chance line
    plot(pFAR,pHR, type="l", col='black', xlim=c(0,1), ylim=c(0,1), xlab="", ylab="")
    par(new=T)
    plot(pFAR.crit, pHR.crit, col='black', pch=19, xlim=c(0,1), ylim=c(0,1), axes=F, 
         xlab="FA Rate", ylab="Hit Rate")
    abline(a=0,b=1,lty=3)
    par(new=F)
}

#Generates data table based on SDT parameter inputs
dataPara <- function(criterion, dprime){
    zFA <-criterion*(-1)- 0.5*dprime
    zH <- dprime + zFA
    Hits <-pnorm(zH)
    FalseAlarm <-pnorm(zFA)
    
    Rainy <-c(Hits,1-Hits)
    Sunny <-c(FalseAlarm, 1-FalseAlarm)
    Weather <- c("Shoot","No Shoot")
    
    prob <- data.frame(Weather, Rainy, Sunny)
    colnames(prob) <- c("", "Armed", "Unarmed")
    prob
}

#Generates zscores of data based on SDT paramter inputs
zdataPara <- function(criterion, dprime){
    zFA <-criterion*(-1)- 0.5*dprime
    zH <- dprime + zFA
    
    zrate <- data.frame(zH, zFA)
    colnames(zrate) <- c("z Hit", "z False Alarm")
    zrate
}

#Generates distribution plot based on SDT parameter inputs
distPara <- function(criterion, dprime, SD){
    FAR <- seq(-5,10,length=1000)
    HR <- seq(-5,10,length=1000)
    
    # get normal probability density functions
    dFAR <- dnorm(FAR,mean=0,sd=1)
    dHR <- dnorm(HR,mean=dprime,sd=SD) # sd=1 for equal variance SD
    
    
    # draw the density function + line for criterion
    plot(FAR, dFAR, type="l", col='blue', xlab="", ylab="", ylim=c(0,.5), lwd=2) # FAR distribution
    par(new=T)
    plot(HR, dHR, type="l", col='red', axes=F, xlab="Signal Strength", ylab="Normal probability density function", ylim=c(0,.5), lwd=2) # target distribution
    abline(v=criterion,lty=3, lwd=2) # dotted line for criterion
    polygon(c(FAR[FAR>=criterion], criterion), c(dFAR[FAR>=criterion], 0), col="gray") #shading for False alarm
    legend("topright",legend=c("False Alarm Rate (noise only)","Hit Rate (signal + noise)"),col=c("blue","red"),lty=1)
    par(new=F)
}

histPara <- function(criterion, dprime){
    zFA <-criterion*(-1)- 0.5*dprime
    zH <- dprime + zFA
    Hits <-pnorm(zH)
    FalseAlarm <-pnorm(zFA)
    cr <- 1-FalseAlarm
    miss <- 1- Hits
    
    n <- c(Hits, miss, cr, FalseAlarm)
    Condition <- c("Armed", "Armed", "Unarmed", "Unarmed")
    Type <- c("Hit", "Miss","Correct Rejection", "False Alarm")
    temp <- data.frame(n,Condition,Type)
    ggplot(temp, aes(fill = Type, y = n, x = Condition)) +
        geom_bar(postiion = "fill", stat = "identity") +
        ggtitle("Accuracy Rates")
    
}
#################################################################################################################
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Introduction", tabName = "Introduction"),
        menuItem("Parameters", tabName = "Parameters"),
        menuItem("Accuracy", tabName = "Accuracy")
    )
)

ui <- dashboardPage(
    dashboardHeader(title = "SDT Models"),
    #dashboardSidebar(disable = TRUE),
    sidebar,
    dashboardBody(
        tabItems(
            tabItem(tabName = "Introduction",
                    box(title = "Instructions:", status = "warning", collapsible = TRUE,
                        "This applet is a tool for visualizing how different adjustments to participants' response accuracy differentially impact\
                Signal Detection Theory parameters (sensitivity, or d-prime, and criterion. You can make your own adjustments by simply editing\
                the absolute values at the top of each example.",
                        br(), br(),
                        "The 'Accuracy' tab will allow you to observe how changes in participants' raw Hit, Miss, Correct Rejection, and False Alarm rates\
                        subsuquently influence their SDT parameters.",
                        br(), br(),
                        "The 'Parameter' tab will allow you to explore how to changes in participants' SDT parameters themselves subsuquently influence\
                        other SDT parameters as well as their raw performance."),
                    box(title = "Equations:", status = "danger", collapsible = TRUE,
                        "Below are the standard sensitivity (d') and criterion (c) equations for your reference.",
                        br(), br(),
                        "d' = z(Hit) - z(False Alarm)",
                        br(),
                        "c = -0.5*[z(Hit) + z(False Alarm)]"
                        
                    )),
            tabItem(tabName = "Parameters",
                    column(width = 6,
                           box(title = "Example 1:", width = NULL, collapsible = TRUE, align = "center",
                               solidHeader = TRUE, status = "primary",
                               fluidRow(
                                   column(12,
                                          sliderInput("dprime_ex1",
                                                      "Discriminability (d'):",
                                                      min = 0,
                                                      max = 4,
                                                      value = 2.92,
                                                      step = .10),
                                          sliderInput("criterion_ex1",
                                                      "Decision criterion (C):",
                                                      min = -5,
                                                      max = 5,
                                                      value = -0.18,
                                                      step = .10),
                                          sliderInput("SD_ex1",
                                                      "Standard Deviation of the Signal:",
                                                      min = 0,
                                                      max = 6,
                                                      value = 1,
                                                      step = .25)),
                               ),
                               hr(),
                               fluidRow(
                                   column(6,
                                          h4("Performance Data"),
                                          br(),
                                          tableOutput("data_ex1")),
                                   column(6,
                                          h4("z-Scored Data"),
                                          br(),
                                          tableOutput("zdata_ex1")
                                   ),
                               ),
                               hr(),
                               fluidRow(
                                   column(6,
                                          h4("Density Curves"),
                                          plotOutput("distPlot_ex1")
                                   ),
                                   column(6,
                                          plotOutput("hist_ex1")
                                   ))
                           ) #close box
                    ), #close column
                    column(width = 6,
                           box(title = "Example 2:", width = NULL, collapsible = TRUE, align = "center",
                               solidHeader = TRUE, status = "primary",
                               fluidRow(
                                   column(12,
                                          sliderInput("dprime_ex2",
                                                      "Discriminability (d'):",
                                                      min = 0,
                                                      max = 4,
                                                      value = 2.92,
                                                      step = .10),
                                          sliderInput("criterion_ex2",
                                                      "Decision criterion (C):",
                                                      min = -5,
                                                      max = 5,
                                                      value = -0.18,
                                                      step = .10),
                                          sliderInput("SD_ex2",
                                                      "Standard Deviation of the Signal:",
                                                      min = 0,
                                                      max = 6,
                                                      value = 1,
                                                      step = .25)),
                               ),
                               hr(),
                               fluidRow(
                                   column(6,
                                          h4("Performance Data"),
                                          br(),
                                          tableOutput("data_ex2")),
                                   column(6,
                                          h4("z-Scored Data"),
                                          br(),
                                          tableOutput("zdata_ex2")
                                   ),
                               ),
                               hr(),
                               fluidRow(
                                   column(6,
                                          h4("Density Curves"),
                                          plotOutput("distPlot_ex2")
                                   ),
                                   column(6,
                                          plotOutput("hist_ex2")
                                   ))
                           ) #close box
                    ), #close column
            ),  #close tabitem
            tabItem(tabName = "Accuracy",
                    fluidRow(
                        column(width = 6,
                               box(title = "Example 1:", width = NULL, collapsible = TRUE, align = "center",
                                   solidHeader = TRUE, status = "primary",
                                   fluidRow(
                                       verticalLayout( 
                                           h4("Raw Performance Data"),
                                           br(),
                                           fluidRow(
                                               column(width = 4, offset = 1,
                                                      numericInput("hit_ex1", label = "Hits", value=95)),
                                               column(width = 4, #offset = 2,
                                                      numericInput("cr_ex1", "Correct Rejection",value=90))
                                           ),
                                           fluidRow(
                                               column(4, offset = 1,
                                                      h5("Miss"),
                                                      textOutput("miss_ex1")
                                               ),
                                               column(4, 
                                                      h5("False Alarm"),
                                                      textOutput("FA_ex1")
                                               ),
                                           )
                                       ), #verticalLayout close
                                       hr(),
                                   ),
                                   fluidRow(
                                       column(5,
                                              h4("Performance Data"),
                                              br(),
                                              tableOutput("rate2_ex1")
                                       ),
                                       column(6,
                                              h4("z scores"),
                                              br(),
                                              tableOutput("zrate_ex1")
                                       )
                                   ),
                                   fluidRow(
                                       column(10, offset = 2,
                                              h4('Density Plot'),
                                              plotOutput("Dist_ex1"),
                                              bsPopover("Dist_ex1", title = 'Density Plot', 
                                                        content = 'This is a density plot',
                                                        placement = 'top', trigger = 'hover'))
                                       
                                   ),
                               ) #close box
                        ), #close column
                        column(width = 6,
                               box(title = "Example 2:", width = NULL, collapsible = TRUE, align = "center",
                                   solidHeader = TRUE, status = "primary",
                                   fluidRow(
                                       verticalLayout( 
                                           h4("Raw Performance Data"),
                                           br(),
                                           fluidRow(
                                               column(width = 4, offset = 1,
                                                      numericInput("hit_ex2", label = "Hits", value=95)),
                                               column(width = 4, #offset = 2,
                                                      numericInput("cr_ex2", "Correct Rejection",value=90))
                                           ),
                                           fluidRow(
                                               column(4, offset = 1,
                                                      h5("Miss"),
                                                      textOutput("miss_ex2")
                                               ),
                                               column(4, 
                                                      h5("False Alarm"),
                                                      textOutput("FA_ex2")
                                               ),
                                           )
                                       ), #verticalLayout close
                                       hr(),
                                   ),
                                   fluidRow(
                                       column(5,
                                              h4("Performance Data"),
                                              br(),
                                              tableOutput("rate2_ex2")
                                       ),
                                       column(6,
                                              h4("z scores"),
                                              br(),
                                              tableOutput("zrate_ex2")
                                       )
                                   ),
                                   fluidRow(
                                       column(10, offset = 2,
                                              h4('Density Plot'),
                                              plotOutput("Dist_ex2"),
                                              bsPopover("Dist_ex2", title = 'Density Plot', 
                                                        content = 'This is a density plot',
                                                        placement = 'top', trigger = 'hover'))
                                       
                                   ),
                               ) #close box
                        ), #close column
                    ), #close fluidROw
            ) #close tabItem
            
        ) #close tabitems
    ) #close dashboarbody
) #close dashboardpage

#################################################################################################################
server <- function(input, output) {

#ABSTRACT TAB
    #EXAMPLE 1########################################
    
    #Reactive false alarm/miss rate output
    FA_ex1 <- reactive({100 - input$cr_ex1})
    output$FA_ex1 <- renderText({print(FA_ex1())})
    
    miss_ex1 <- reactive({100 - input$hit_ex1})
    output$miss_ex1 <- renderText({print(miss_ex1())})
    
    #ABSOLUTE VALUE Table
    output$rate2_ex1 <- renderTable({sdtTable(input$hit_ex1, miss_ex1(), FA_ex1(), input$cr_ex1)}, 
                                    options= list(searching = F, paging = F), include.rownames=FALSE) 
    
    #Z SCORE Table
    output$zrate_ex1 <- renderTable({zscoreTable(input$hit_ex1, miss_ex1(), FA_ex1(), input$cr_ex1)},
                                    include.rownames=FALSE)
    
    #DENSITY PLOT
    output$Dist_ex1 <- renderPlot({denPlot(input$hit_ex1, miss_ex1(), FA_ex1(), input$cr_ex1)},
                                  height=400,width=400)
    
    #ROC CURVE PLOT
    output$ROC <- renderPlot({rocPlot(input$hit_ex1, miss_ex1(), FA_ex1(), input$cr_ex1)},
                             height=400,width=400)
    
    #EXAMPLE2########################################
    
    #Reactive false alarm/miss rate output
    FA_ex2 <- reactive({100 - input$cr_ex2})
    output$FA_ex2 <- renderText({print(FA_ex2())})
    
    miss_ex2 <- reactive({100 - input$hit_ex2})
    output$miss_ex2 <- renderText({print(miss_ex2())})
    
    #ABSOLUTE VALUE Table
    output$rate2_ex2 <- renderTable({sdtTable(input$hit_ex2, miss_ex2(), FA_ex2(), input$cr_ex2)}, 
                                    options= list(searching = F, paging = F), include.rownames=FALSE) 
    
    #Z SCORE Table
    output$zrate_ex2 <- renderTable({zscoreTable(input$hit_ex2, miss_ex2(), FA_ex2(), input$cr_ex2)},
                                    include.rownames=FALSE)
    
    #DENSITY PLOT
    output$Dist_ex2 <- renderPlot({denPlot(input$hit_ex2, miss_ex2(), FA_ex2(), input$cr_ex2)},
                                  height=400,width=400)
    
    #ROC CURVE PLOT
    output$ROC <- renderPlot({rocPlot(input$hit_ex2, miss_ex2(), FA_ex2(), input$cr_ex2)},
                             height=400,width=400)
#PARAMTER TAB
    #EXAMPLE 1########################################
    output$data_ex1 <- renderTable({dataPara(input$criterion_ex1, input$dprime_ex1)},
                               options = list(searching = F, paging = F), include.rownames = FALSE)
    output$zdata_ex1 <- renderTable({zdataPara(input$criterion_ex1, input$dprime_ex1)},
                                include.rownames = FALSE)
    output$distPlot_ex1 <- renderPlot({distPara(input$criterion_ex1, input$dprime_ex1, input$SD_ex1)},
                                  height = 400, width = 400)
    output$hist_ex1 <- renderPlot({histPara(input$criterion_ex1, input$dprime_ex1)},
                                  height = 400, width = 400)
    
    #EXAMPLE 2########################################
    output$data_ex2 <- renderTable({dataPara(input$criterion_ex2, input$dprime_ex2)},
                               options = list(searching = F, paging = F), include.rownames = FALSE)
    output$zdata_ex2 <- renderTable({zdataPara(input$criterion_ex2, input$dprime_ex2)},
                                include.rownames = FALSE)
    output$distPlot_ex2 <- renderPlot({distPara(input$criterion_ex2, input$dprime_ex2, input$SD_ex2)},
                                  height = 400, width = 400)
    output$hist_ex2 <- renderPlot({histPara(input$criterion_ex2, input$dprime_ex2)},
                                  height = 300, width = 300)
} # server close

#################################################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
