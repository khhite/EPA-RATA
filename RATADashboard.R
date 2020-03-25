# Load packages ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DescTools)
library(ggpubr)
library(data.table)
library(DT)

source("helper.R")

# User interface ----
ui <- dashboardPage(
  dashboardHeader(title = "Relative Accuracy Test Audits (RATA)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("SO2 & NOx Concentration", tabName = "SO2NOx", icon = icon("dashboard")),
      menuItem("NOx Rate", tabName = "NOxR", icon = icon("dashboard")),
      menuItem("CO2 & O2", tabName = "CO2O2", icon = icon("dashboard")),
      menuItem("H2O", tabName = "H2O", icon = icon("dashboard")),
      
      
      sliderInput("ra_4qtr", "Annual Relative Accuracy",
                  min = 0, max = 10, value = 7.5, step = 0.5
                  ),
      sliderInput("ra_2qtr", "Semi-Annual Relative Accuracy",
                  min = 0, max = 15, value = 10, step = 0.5
                  ),
      sliderInput("annual_aps", "% of proposed Semi-Annual APS used to determine Annual APS",
                  min = 0, max = 1, value = .75, step = 0.05
                  )
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "SO2NOx",
            fluidRow(
              column(width = 6,
                     box(
                       title = "Current Standards", width = NULL,
                       "Initial values are set for the current standard but can be adjusted using the sliders below.", br(),
                       column(width = 6,
                              valueBoxOutput("so2nox_a_aps_cur", width = 12)
                       ),
                       column(width = 6,
                              valueBoxOutput("so2nox_sa_aps_cur", width = 12)
                       ),
                       column(width = 6,
                              sliderInput("so2nox_4q_diff", "SO2 & NOx ppm APS Annual value", min = 0, max = 15, value = 12, step = 0.5
                              )
                       ),
                       column(width = 6,
                              sliderInput("so2nox_2q_diff", "SO2 & NOx ppm APS Semi-Annual value", min = 0, max = 20, value = 15, step = 0.5
                              )
                       )
                     ),
                     
                     box(
                       title = "Results of Current Standards", width = NULL,
                       div(style = 'overflow-x:scroll', tableOutput("currentresults_tbl")),
                       "All observations that failed their tests will be excluded from the statistical analysis used to 
                       determine new proposed standards.", br(), br()
                     )
                ),
              
              column(width = 6,
                     box(
                       title = "Proposed Standards", width = NULL,
                       "Proposed Standards are obtained from the value of 2 Standard Deviations of the historical values.
                       Use sliders to adjust values.", br(),
                       column(width = 6,
                              valueBoxOutput("so2nox_a_aps_val", width = 12)
                       ),
                       column(width = 6,
                              valueBoxOutput("so2nox_sa_aps_val", width = 12)
                       ),
                       column(width = 6,
                              uiOutput("so2nox_a_aps_in")
                       ),
                       column(width = 6,
                              uiOutput("so2nox_sa_aps_in")
                       )
                     ),
                     box(
                       title = "Results of Proposed Standards", width = NULL, 
                       div(style = 'overflow-x: scroll', tableOutput('propresults_tbl'))
                     )
                     
              )
  
           ),
           
           fluidRow(
             box(
               title = "SO2 & NOx Historical Data", width = 12,
               column(width = 4,
                      title = "SO2 & NOx Historical Data",  
                      tableOutput("so2nox_cstats")
               ),
               column(width = 8,
               plotOutput("so2nox_cplt")
               )
             )
             
          )
           
      ),
      
      # Second tab content
      tabItem(tabName = "NOxR",
              fluidRow(
                column(width = 6,
                       box(
                         title = "Current Standards", width = NULL,
                         "Initial values are set for the current standard but can be adjusted using the sliders below.", br(),
                         column(width = 6,
                                valueBoxOutput("noxr_a_aps_cur", width = 12)
                         ),
                         column(width = 6,
                                valueBoxOutput("noxr_sa_aps_cur", width = 12)
                         ),
                         column(width = 6,
                                sliderInput("noxr_4q_diff", "NOx rate APS Annual value", min = 0, max = 0.100, value = 0.015, step = 0.005
                                )
                         ),
                         column(width = 6,
                                sliderInput("noxr_2q_diff", "NOx rate APS Semi-Annual value", min = 0, max = .500, value = 0.200, step = 0.05
                                )
                         )
                       ),
                       
                       box(
                         title = "Results of Current Standards", width = NULL,
                         div(style = 'overflow-x:scroll', tableOutput("noxr_currentresults_tbl")),
                         "All observations that failed their tests will be excluded from the statistical analysis used to 
                       determine new proposed standards.", br(), br()
                       )
                ),
                
                column(width = 6,
                       box(
                         title = "Proposed Standards", width = NULL,
                         "Proposed Standards are obtained from the value of 2 Standard Deviations of the historical values.
                       Use sliders to adjust values.", br(),
                         column(width = 6,
                                valueBoxOutput("noxr_a_aps_val", width = 12)
                         ),
                         column(width = 6,
                                valueBoxOutput("noxr_sa_aps_val", width = 12)
                         ),
                         column(width = 6,
                                uiOutput("noxr_a_aps_in")
                         ),
                         column(width = 6,
                                uiOutput("noxr_sa_aps_in")
                         )
                       ),
                       box(
                         title = "Results of Proposed Standards", width = NULL, 
                         div(style = 'overflow-x: scroll', tableOutput('noxr_propresults_tbl'))
                       )
                       
                )
                
              ),
              
              fluidRow(
                box(
                  title = "NOx Rate Historical Data", width = 12,
                  column(width = 4,
                         title = "NOx Rate Historical Data",  
                         tableOutput("noxr_cstats")
                  ),
                  column(width = 8,
                         plotOutput("noxr_cplt")
                  )
                )
                
              )
      ),
      
      tabItem(tabName = "CO2O2",
              fluidRow(
                column(width = 6,
                       box(
                         title = "Current Standards", width = NULL,
                         "Initial values are set for the current standard but can be adjusted using the sliders below.", br(),
                         column(width = 6,
                                valueBoxOutput("co2o2_a_aps_cur", width = 12)
                         ),
                         column(width = 6,
                                valueBoxOutput("co2o2_sa_aps_cur", width = 12)
                         ),
                         column(width = 6,
                                sliderInput("co2o2_4q_diff", "CO2 and O2 APS Annual value", min = 0, max = 1, value = 0.7, step = 0.05
                                )
                         ),
                         column(width = 6,
                                sliderInput("co2o2_2q_diff", "CO2 and O2 APS Semi-Annual value", min = 0, max = 2, value = 1.0, step = 0.5
                                )
                         )
                       ),
                       
                       box(
                         title = "Results of Current Standards", width = NULL,
                         div(style = 'overflow-x:scroll', tableOutput("co2o2_currentresults_tbl")),
                         "All observations that failed their tests will be excluded from the statistical analysis used to 
                       determine new proposed standards.", br(), br()
                       )
                ),
                
                column(width = 6,
                       box(
                         title = "Proposed Standards", width = NULL,
                         "Proposed Standards are obtained from the value of 2 Standard Deviations of the historical values.
                       Use sliders to adjust values.", br(),
                         column(width = 6,
                                valueBoxOutput("co2o2_a_aps_val", width = 12)
                         ),
                         column(width = 6,
                                valueBoxOutput("co2o2_sa_aps_val", width = 12)
                         ),
                         column(width = 6,
                                uiOutput("co2o2_a_aps_in")
                         ),
                         column(width = 6,
                                uiOutput("co2o2_sa_aps_in")
                         )
                       ),
                       box(
                         title = "Results of Proposed Standards", width = NULL, 
                         div(style = 'overflow-x: scroll', tableOutput('co2o2_propresults_tbl'))
                       )
                       
                )
                
              ),
              
              fluidRow(
                box(
                  title = "CO2 and O2 Historical Data", width = 12,
                  column(width = 4,
                         title = "CO2 and O2 Historical Data",  
                         tableOutput("co2o2_cstats")
                  ),
                  column(width = 8,
                         plotOutput("co2o2_cplt")
                  )
                )
                
              )
      ),
      
      tabItem(tabName = "H2O",
              fluidRow(
                column(width = 6,
                       box(
                         title = "Current Standards", width = NULL,
                         "Initial values are set for the current standard but can be adjusted using the sliders below.", br(),
                         column(width = 6,
                                valueBoxOutput("h2o_a_aps_cur", width = 12)
                         ),
                         column(width = 6,
                                valueBoxOutput("h2o_sa_aps_cur", width = 12)
                         ),
                         column(width = 6,
                                sliderInput("h2o_4q_diff", "Moisture (%H2O) APS Annual value", min = 0, max = 2, value = 1.0, step = 0.5
                                )
                         ),
                         column(width = 6,
                                sliderInput("h2o_2q_diff", "Moisture (%H2O) APS Semi-Annual value", min = 0, max = 2, value = 1.5, step = 0.5
                                )
                         )
                       ),
                       
                       box(
                         title = "Results of Current Standards", width = NULL,
                         div(style = 'overflow-x:scroll', tableOutput("h2o_currentresults_tbl")),
                         "All observations that failed their tests will be excluded from the statistical analysis used to 
                       determine new proposed standards.", br(), br()
                       )
                ),
                
                column(width = 6,
                       box(
                         title = "Proposed Standards", width = NULL,
                         "Proposed Standards are obtained from the value of 2 Standard Deviations of the historical values.
                       Use sliders to adjust values.", br(),
                         column(width = 6,
                                valueBoxOutput("h2o_a_aps_val", width = 12)
                         ),
                         column(width = 6,
                                valueBoxOutput("h2o_sa_aps_val", width = 12)
                         ),
                         column(width = 6,
                                uiOutput("h2o_a_aps_in")
                         ),
                         column(width = 6,
                                uiOutput("h2o_sa_aps_in")
                         )
                       ),
                       box(
                         title = "Results of Proposed Standards", width = NULL, 
                         div(style = 'overflow-x: scroll', tableOutput('h2o_propresults_tbl'))
                       )
                       
                )
                
              ),
              
              fluidRow(
                box(
                  title = "Moisture (H2O%) Historical Data", width = 12,
                  column(width = 4,
                         title = "Moisture (H2O%) Historical Data",  
                         tableOutput("h2o_cstats")
                  ),
                  column(width = 8,
                         plotOutput("h2o_cplt")
                  )
                )
                
              )
      )
      
    )
  )
)

# Server logic ----
server <- function(input, output) {
  #### SO2 & NOx ####
  so2nox_results <- reactive({
    resultsdat <- testresults(so2nox_filtered, input$ra_4qtr, input$ra_2qtr, input$so2nox_4q_diff, input$so2nox_2q_diff)
    return(resultsdat)
  })
  
  so2nox_passed <- reactive({
    passeddat <- so2nox_results() %>% filter(testresult !="FAILED")
    passeddat <- addsd(passeddat)
    return(passeddat)
  })
  
  so2nox_stats <- reactive({
    statsdat <- getstats(so2nox_passed()$Mean.Diff)
    return(statsdat)
  })
  
  so2nox_sa_aps <- reactive({
    sd2_low <-  round2(mean(so2nox_passed()$Mean.Diff) - (2*sd(so2nox_passed()$Mean.Diff)),2)
    sd2_high <-  round2(mean(so2nox_passed()$Mean.Diff) + (2*sd(so2nox_passed()$Mean.Diff)),2)
    so2noxsaaps <- round2(pmax(abs(sd2_low),abs(sd2_high)), 0)
    return(so2noxsaaps)
  })
  
  so2nox_a_aps <- reactive({
    a_aps <- round2(input$annual_aps*so2nox_sa_aps(), 0) 
    return(a_aps)
  })

  
  output$so2nox_a_aps_cur <- renderValueBox({
    valueBox(
      input$so2nox_4q_diff, "SO2 & NOx Current Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$so2nox_sa_aps_cur <- renderValueBox({
    valueBox(
      input$so2nox_2q_diff, "SO2 & NOx Current Semi-Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$so2nox_cstats <- renderTable({
    so2nox_stats()
    
  })
  
  output$so2nox_cplt <- renderPlot({
    plotdist(so2nox_passed(), Mean.Diff, 25, "SO2 & NOx ppm RATAs Mean Difference") 
  })
  
  
  output$currentresults_tbl <- renderTable({
    display_cur_results(so2nox_results()) 
    }, bordered = TRUE, align = 'c'
    )
  
  output$so2nox_a_aps_val <- renderValueBox({
    valueBox(
      so2nox_a_aps(),"SO2 & NOx Proposed Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$so2nox_sa_aps_val <- renderValueBox({
    valueBox(
      so2nox_sa_aps(),"SO2 & NOx Proposed Semi-Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  
  output$so2nox_sa_aps_in <- renderUI({
    sliderInput("so2nox_sa_aps_in", "Proposed APS Semi-Annual value", min = 0, max = 15, value = so2nox_sa_aps(), step = 1
                )
  })
  
  output$so2nox_a_aps_in <- renderUI({
    sliderInput("so2nox_a_aps_in", "Proposed APS Annual value", min = 0, max = 15, value = so2nox_a_aps(), step = 1
    )
  })
  
  so2nox_propresults <- reactive({
    resultsdat <- testresults(so2nox_passed(), input$ra_4qtr, input$ra_2qtr, input$so2nox_a_aps_in, input$so2nox_sa_aps_in)
    comb_results <- add_column(so2nox_passed(), propresults=resultsdat$testresult)
    return(comb_results)
  })
  
  output$propresults_tbl <- renderTable({
    display_prop_results(so2nox_propresults())
    }, bordered = TRUE, align = 'c'
    )

  
  #### NOx Rate ####
  noxr_results <- reactive({
    resultsdat <- testresults(noxrrata_filtered, input$ra_4qtr, input$ra_2qtr, input$noxr_4q_diff, input$noxr_2q_diff)
    return(resultsdat)
  })
  
  noxr_passed <- reactive({
    passeddat <- noxr_results() %>% filter(testresult !="FAILED")
    passeddat <- addsd(passeddat)
    return(passeddat)
  })
  
  noxr_stats <- reactive({
    statsdat <- getstats(noxr_passed()$Mean.Diff)
    return(statsdat)
  })
  
  noxr_sa_aps <- reactive({
    sd2_low <-  round2(mean(noxr_passed()$Mean.Diff) - (2*sd(noxr_passed()$Mean.Diff)),4)
    sd2_high <-  round2(mean(noxr_passed()$Mean.Diff) + (2*sd(noxr_passed()$Mean.Diff)),4)
    noxrsaaps <- round2(pmax(abs(sd2_low),abs(sd2_high)), 3)
    return(noxrsaaps)
  })
  
  noxr_a_aps <- reactive({
    a_aps <- round2(input$annual_aps*noxr_sa_aps(), 3) 
    return(a_aps)
  })
  
  
  output$noxr_a_aps_cur <- renderValueBox({
    valueBox(
      input$noxr_4q_diff, "NOx Rate Current Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$noxr_sa_aps_cur <- renderValueBox({
    valueBox(
      input$noxr_2q_diff, "NOx Rate Current Semi-Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$noxr_cstats <- renderTable({
    noxr_stats()
    
  })
  
  output$noxr_cplt <- renderPlot({
    plotdist(noxr_passed(), Mean.Diff, 25, "NOx Rate RATAs Mean Difference") 
  })
  
  
  output$noxr_currentresults_tbl <- renderTable({
    display_cur_results(noxr_results()) 
  }, bordered = TRUE, align = 'c'
  )
  
  output$noxr_a_aps_val <- renderValueBox({
    valueBox(
      noxr_a_aps(),"NOx Rate Proposed Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$noxr_sa_aps_val <- renderValueBox({
    valueBox(
      noxr_sa_aps(),"NOx Rate Proposed Semi-Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$noxr_sa_aps_in <- renderUI({
    sliderInput("noxr_sa_aps_in", "Proposed APS Semi-Annual value", min = 0, max = 0.100, value = noxr_sa_aps(), step = 0.005
    )
  })
  
  output$noxr_a_aps_in <- renderUI({
    sliderInput("noxr_a_aps_in", "Proposed APS Annual value", min = 0, max = .500, value = noxr_a_aps(), step = 0.05
    )
  })
  
  noxr_propresults <- reactive({
    resultsdat <- testresults(noxr_passed(), input$ra_4qtr, input$ra_2qtr, input$noxr_a_aps_in, input$noxr_sa_aps_in)
    comb_results <- add_column(noxr_passed(), propresults=resultsdat$testresult)
    return(comb_results)
  })
  
  output$noxr_propresults_tbl <- renderTable({
    display_prop_results(noxr_propresults())
  }, bordered = TRUE, align = 'c'
  )
  
  #### CO2 & O2 ####
  co2o2_results <- reactive({
    resultsdat <- testresults(co2o2, input$ra_4qtr, input$ra_2qtr, input$co2o2_4q_diff, input$co2o2_2q_diff)
    return(resultsdat)
  })
  
  co2o2_passed <- reactive({
    passeddat <- co2o2_results() %>% filter(testresult !="FAILED")
    passeddat <- addsd(passeddat)
    return(passeddat)
  })
  
  co2o2_stats <- reactive({
    statsdat <- getstats(co2o2_passed()$Mean.Diff)
    return(statsdat)
  })
  
  co2o2_sa_aps <- reactive({
    sd2_low <-  round2(mean(co2o2_passed()$Mean.Diff) - (2*sd(co2o2_passed()$Mean.Diff)),4)
    sd2_high <-  round2(mean(co2o2_passed()$Mean.Diff) + (2*sd(co2o2_passed()$Mean.Diff)),4)
    co2o2saaps <- round2(pmax(abs(sd2_low),abs(sd2_high)), 1)
    return(co2o2saaps)
  })
  
  co2o2_a_aps <- reactive({
    a_aps <- round2(input$annual_aps*co2o2_sa_aps(), 1) 
    return(a_aps)
  })
  
  
  output$co2o2_a_aps_cur <- renderValueBox({
    valueBox(
      input$co2o2_4q_diff, "CO2 & O2 % Current Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$co2o2_sa_aps_cur <- renderValueBox({
    valueBox(
      input$co2o2_2q_diff, "CO2 & O2 % Current Semi-Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$co2o2_cstats <- renderTable({
    co2o2_stats()
    
  })
  
  output$co2o2_cplt <- renderPlot({
    plotdist(co2o2_passed(), Mean.Diff, 25, "CO2 & O2 % RATAs Mean Difference") 
  })
  
  
  output$co2o2_currentresults_tbl <- renderTable({
    display_cur_results(co2o2_results()) 
  }, bordered = TRUE, align = 'c'
  )
  
  output$co2o2_a_aps_val <- renderValueBox({
    valueBox(
      co2o2_a_aps(),"CO2 & O2 % Proposed Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$co2o2_sa_aps_val <- renderValueBox({
    valueBox(
      co2o2_sa_aps(),"CO2 & O2 % Proposed Semi-Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$co2o2_sa_aps_in <- renderUI({
    sliderInput("co2o2_sa_aps_in", "Proposed APS Semi-Annual value", min = 0, max = 1, value = co2o2_sa_aps(), step = 0.05
    )
  })
  
  output$co2o2_a_aps_in <- renderUI({
    sliderInput("co2o2_a_aps_in", "Proposed APS Annual value", min = 0, max = 2, value = co2o2_a_aps(), step = 0.5
    )
  })
  
  co2o2_propresults <- reactive({
    resultsdat <- testresults(co2o2_passed(), input$ra_4qtr, input$ra_2qtr, input$co2o2_a_aps_in, input$co2o2_sa_aps_in)
    comb_results <- add_column(co2o2_passed(), propresults=resultsdat$testresult)
    return(comb_results)
  })
  
  output$co2o2_propresults_tbl <- renderTable({
    display_prop_results(co2o2_propresults())
  }, bordered = TRUE, align = 'c'
  )
  
  #### Moisture (H2o%) ####
  h2o_results <- reactive({
    resultsdat <- testresults(h2omh2o, input$ra_4qtr, input$ra_2qtr, input$h2o_4q_diff, input$h2o_2q_diff)
    return(resultsdat)
  })
  
  h2o_passed <- reactive({
    passeddat <- h2o_results() %>% filter(testresult !="FAILED")
    passeddat <- addsd(passeddat)
    return(passeddat)
  })
  
  h2o_stats <- reactive({
    statsdat <- getstats(h2o_passed()$Mean.Diff)
    return(statsdat)
  })
  
  h2o_sa_aps <- reactive({
    sd2_low <-  round2(mean(h2o_passed()$Mean.Diff) - (2*sd(h2o_passed()$Mean.Diff)),4)
    sd2_high <-  round2(mean(h2o_passed()$Mean.Diff) + (2*sd(h2o_passed()$Mean.Diff)),4)
    h2osaaps <- round2(pmax(abs(sd2_low),abs(sd2_high)), 1)
    return(h2osaaps)
  })
  
  h2o_a_aps <- reactive({
    a_aps <- round2(input$annual_aps*h2o_sa_aps(), 1) 
    return(a_aps)
  })
  
  
  output$h2o_a_aps_cur <- renderValueBox({
    valueBox(
      input$h2o_4q_diff, "Moisture (H2O%) Current Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$h2o_sa_aps_cur <- renderValueBox({
    valueBox(
      input$h2o_2q_diff, "Moisture (H2O%) Current Semi-Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$h2o_cstats <- renderTable({
    h2o_stats()
    
  })
  
  output$h2o_cplt <- renderPlot({
    plotdist(h2o_passed(), Mean.Diff, 25, "Moisture (H2O%) RATAs Mean Difference") 
  })
  
  
  output$h2o_currentresults_tbl <- renderTable({
    display_cur_results(h2o_results()) 
  }, bordered = TRUE, align = 'c'
  )
  
  output$h2o_a_aps_val <- renderValueBox({
    valueBox(
      h2o_a_aps(),"Moisture (H2O%) Proposed Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$h2o_sa_aps_val <- renderValueBox({
    valueBox(
      h2o_sa_aps(),"Moisture (H2O%) Proposed Semi-Annual APS",
      icon = NULL, color = "green"
    )
  })
  
  output$h2o_sa_aps_in <- renderUI({
    sliderInput("h2o_sa_aps_in", "Proposed APS Semi-Annual value", min = 0, max = 2, value = h2o_sa_aps(), step = 0.5
    )
  })
  
  output$h2o_a_aps_in <- renderUI({
    sliderInput("h2o_a_aps_in", "Proposed APS Annual value", min = 0, max = 2, value = h2o_a_aps(), step = 0.5
    )
  })
  
  h2o_propresults <- reactive({
    resultsdat <- testresults(h2o_passed(), input$ra_4qtr, input$ra_2qtr, input$h2o_a_aps_in, input$h2o_sa_aps_in)
    comb_results <- add_column(h2o_passed(), propresults=resultsdat$testresult)
    return(comb_results)
  })
  
  output$h2o_propresults_tbl <- renderTable({
    display_prop_results(h2o_propresults())
  }, bordered = TRUE, align = 'c'
  )
  
}

shinyApp(ui, server)
