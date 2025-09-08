

# Reshma Kassanjee. Last updated 20 August 2025
# Infectious disease modelling
# SIRS with demography
# Learning tool for University of Cape Town Master of Public Health program


library(shiny)
library(bslib)
library(deSolve)
library(tidyverse)

source('fns.R')


### Define UI ---------------


ui <- 
  page_sidebar( 
    
    title = h1(strong('SIRS with demography'))
    
    #
    #
    
    #### SIDEBAR - start
    
    , sidebar = sidebar(
      
      width  = 350
      , style = 'padding-top:1px;'
      , h3(strong('Inputs'))
      , tags$p('Use slider. Or type value & click ⟳ to update.'
               , style = 'font-size:80%')
      
      ## I0
      
      , sliderInput('in_I0',
                    label = strong('Number initially infected (/1000)')
                    , min = inmin_I0, max = inmax_I0, step = instep_I0, value = indefault_I0)
      , splitLayout(
        numericInput('inplusnum_I0', NULL, value = indefault_I0, min = inmin_I0, max = inmax_I0, width = '100%')
        , actionButton('update_I0', '⟳',
                       style = 'padding:2px 6px; font-size:80%; margin-top:3px;')
        , span(textOutput('error_I0'), style='color:red; font-size:70%;')
        , cellWidths = c('35%', '25%', '40%')
      )
      
      ## beta
      
      , sliderInput('in_beta',
                    label = strong('Transmission coefficient (per day)')
                    , min = inmin_beta, max = inmax_beta, step = instep_beta, value = indefault_beta)
      , splitLayout(
        numericInput('inplusnum_beta', NULL, value = indefault_beta, min = inmin_beta, max = inmax_beta, width = '100%')
        , actionButton('update_beta', '⟳',
                       style = 'padding:2px 6px; font-size:80%; margin-top:3px;')
        , span(textOutput('error_beta'), style='color:red; font-size:70%;')
        , cellWidths = c('35%', '25%', '40%')
      )
      
      ## duration infection    
      
      , sliderInput('in_durinf',
                    label = strong('Mean duration of infection (days)')
                    , min = inmin_durinf, max = inmax_durinf, step = instep_durinf, value = indefault_durinf)
      , splitLayout(
        numericInput('inplusnum_durinf', NULL, value = indefault_durinf, min = inmin_durinf, max = inmax_durinf, width = '100%')
        , actionButton('update_durinf', '⟳',
                       style = 'padding:2px 6px; font-size:80%; margin-top:3px;')
        , span(textOutput('error_durinf'), style='color:red; font-size:70%;')
        , cellWidths = c('35%', '25%', '40%')
      )
      
      ## waning indicator
      
      , checkboxInput('in_indwaning'
                      , 'Include loss of immunity'
                      , value = FALSE)
      
      ## duration immunity
      
      , sliderInput('in_durimm',
                    label = strong('Mean duration of immunity (years)')
                    , min = inmin_durimm, max = inmax_durimm, step = instep_durimm, value = indefault_durimm)
      , splitLayout(
        numericInput('inplusnum_durimm', NULL, value = indefault_durimm, min = inmin_durimm, max = inmax_durimm, width = '100%')
        , actionButton('update_durimm', '⟳',
                       style = 'padding:2px 6px; font-size:80%; margin-top:3px;')
        , span(textOutput('error_durimm'), style='color:red; font-size:70%;')
        , cellWidths = c('35%', '25%', '40%')
      )
      
      ## birth indicator
      
      , checkboxInput('in_indbirth'
                      , 'Include birth & death (constant N)'
                      , value = FALSE)
      
      ## duration life
      
      , sliderInput('in_durlife',
                    label = strong('Mean lifespan (years)')
                    , min = inmin_durlife, max = inmax_durlife, step = instep_durlife, value = indefault_durlife)
      , splitLayout(
        numericInput('inplusnum_durlife', NULL, value = indefault_durlife, min = inmin_durlife, max = inmax_durlife, width = '100%')
        , actionButton('update_durlife', '⟳',
                       style = 'padding:2px 6px; font-size:80%; margin-top:3px;')
        , span(textOutput('error_durlife'), style='color:red; font-size:70%;')
        , cellWidths = c('35%', '25%', '40%')
      )
      
      # time scale for plotting
      
      , sliderInput('in_maxtime'
                    , label = 'Maximum time for y-axis (years)'
                    , min = 0.5, max = 20, step = 0.5, value = 5)
      
    ) # SIDERBAR end
    
    #
    #
    
    #### NAVIGATION PANELS
    
    , navset_card_underline(
      title = h3(strong('Outputs'))
      
      ## PANEL 1 (scrollable content): Full collection of outputs
      
      , nav_panel('Full collection'
                  , div(style = 'overflow-y: auto; height: 80vh; padding: 1em;'
                        , layout_columns(
                          col_widths = c(6,6,6,6,2,10)
                          
                          , card(card_header('Infected')
                                 , plotOutput('plot_i'))
                          
                          , card(card_header('Infected & effective reproductive number')
                                 , plotOutput('plot_ireff_a'))
                          
                          , card(card_header('Population counts')
                                 , plotOutput('plot_allstates_a'))
                          
                          , card(card_header('Prevalence and incidence')
                                 , plotOutput('plot_incprev_a'))
                          
                          , card(card_header('Summary measures')
                                 , textOutput('calcr0')
                                 , tableOutput('tabinfections'))
                          
                          , card(card_header('Population counts for 20 years')
                                 , plotOutput('plot_allstates_20y'))
                          
                        ) # layout_columns(
                  ) # div(style =
      ) # PANEL 1 end
      
      ## PANEL 2 (fluid): Population counts
      
      , nav_panel('Population counts'
                  , card(
                    full_screen = TRUE
                    , plotOutput('plot_allstates_b', fill = TRUE)
                  )
      ) # PANEL 2 end
      
      ## PANEL 3 (fluid): Infected and effective R
      
      , nav_panel('Infected & effective reproductive number'
                  , card(
                    full_screen = TRUE
                    , plotOutput('plot_ireff_b', fill = TRUE)
                  )
      ) # PANEL 3
      
      ## PANEL 4 (fluid): Prevalence and incidence
      
      , nav_panel('Prevalence and incidence'
                  , card(
                    full_screen = TRUE
                    , plotOutput('plot_incprev_b', fill = TRUE)
                  )
      ) # PANEL 4
      
    ) # END NAVIGATION PANELS 
    
    , tags$p('Reshma Kassanjee. Last updated Aug 2025. Some rights reserved. CC BY-NC 4.0'
             , style = 'font-size:80%')
    
    #
    #
    
  ) # END UI



### Define server logic --------


server <- function(input, output, session) {
  
  #
  #
  
  ## SYNC ALL INPUTS
  
  ## SYNC INPUT: I0
  
  # Reactive value to hold current
  I0_val <- reactiveVal(indefault_I0)
  # Slider moves → update reactive and numeric box
  observeEvent(input$in_I0, {
    I0_val(input$in_I0)
    updateNumericInput(session, 'inplusnum_I0', value = input$in_I0)
    output$error_I0 <- renderText('')   # clear error
  })
  # Numeric input + Update button → update slider if valid
  observeEvent(input$update_I0, {
    val <- input$inplusnum_I0
    if (!is.null(val) && !is.na(val) && val >= inmin_I0 && val <= inmax_I0) {
      I0_val(val)
      updateSliderInput(session, 'in_I0', value = val)
      output$error_I0 <- renderText('')   # clear error
    } else {
      output$error_I0 <- renderText('0–1000\n(steps:1)')  # small inline error
    }
  })
  
  ## SYNC INPUT: beta
  
  # Reactive value to hold current
  beta_val <- reactiveVal(indefault_beta)
  # Slider moves → update reactive and numeric box
  observeEvent(input$in_beta, {
    beta_val(input$in_beta)
    updateNumericInput(session, 'inplusnum_beta', value = input$in_beta)
    output$error_beta <- renderText('')   # clear error
  })
  # Numeric input + Update button → update slider if valid
  observeEvent(input$update_beta, {
    val <- input$inplusnum_beta
    if (!is.null(val) && !is.na(val) && val >= inmin_beta && val <= inmax_beta) {
      beta_val(val)
      updateSliderInput(session, 'in_beta', value = val)
      output$error_beta <- renderText('')   # clear error
    } else {
      output$error_beta <- renderText('0–10\n(steps:0.05)')  # small inline error
    }
  })
  
  ## SYNC INPUT: duration infection
  
  # Reactive value to hold current
  durinf_val <- reactiveVal(indefault_durinf)
  # Slider moves → update reactive and numeric box
  observeEvent(input$in_durinf, {
    durinf_val(input$in_durinf)
    updateNumericInput(session, 'inplusnum_durinf', value = input$in_durinf)
    output$error_durinf <- renderText('')   # clear error
  })
  # Numeric input + Update button → update slider if valid
  observeEvent(input$update_durinf, {
    val <- input$inplusnum_durinf
    if (!is.null(val) && !is.na(val) && val >= inmin_durinf && val <= inmax_durinf) {
      durinf_val(val)
      updateSliderInput(session, 'in_durinf', value = val)
      output$error_durinf <- renderText('')   # clear error
    } else {
      output$error_durinf <- renderText('1–100\n(steps:1)')  # small inline error
    }
  })
  
  ## SYNC INPUT: duration immunity
  
  # Reactive value to hold current
  durimm_val <- reactiveVal(indefault_durimm)
  # Slider moves → update reactive and numeric box
  observeEvent(input$in_durimm, {
    durimm_val(input$in_durimm)
    updateNumericInput(session, 'inplusnum_durimm', value = input$in_durimm)
    output$error_durimm <- renderText('')   # clear error
  })
  # Numeric input + Update button → update slider if valid
  observeEvent(input$update_durimm, {
    val <- input$inplusnum_durimm
    if (!is.null(val) && !is.na(val) && val >= inmin_durimm && val <= inmax_durimm) {
      durimm_val(val)
      updateSliderInput(session, 'in_durimm', value = val)
      output$error_durimm <- renderText('')   # clear error
    } else {
      output$error_durimm <- renderText('0.5-50\n(steps:0.5)')  # small inline error
    }
  })
  
  ## SYNC INPUT: duration life
  
  # Reactive value to hold current
  durlife_val <- reactiveVal(indefault_durlife)
  # Slider moves → update reactive and numeric box
  observeEvent(input$in_durlife, {
    durlife_val(input$in_durlife)
    updateNumericInput(session, 'inplusnum_durlife', value = input$in_durlife)
    output$error_durlife <- renderText('')   # clear error
  })
  # Numeric input + Update button → update slider if valid
  observeEvent(input$update_durlife, {
    val <- input$inplusnum_durlife
    if (!is.null(val) && !is.na(val) && val >= inmin_durlife && val <= inmax_durlife) {
      durlife_val(val)
      updateSliderInput(session, 'in_durlife', value = val)
      output$error_durlife <- renderText('')   # clear error
    } else {
      output$error_durlife <- renderText('0.1-50\n(steps:0.1)')  # small inline error
    }
  })
  
  #
  #
  
  ## GENERATE OUTPUTS
  
  ## SIMULATE EPIDEMIC
  
  out_dd <- reactive({
    (simulate_epidemic(I0_val()
                       , beta_val()
                       , durinf_val()
                       , input$in_indwaning
                       , durimm_val()
                       , input$in_indbirth
                       , durlife_val())
     |> mutate(time.years = time/365.25)
    )
  })
  
  ## RO
  
  out_ro <- reactive({
    
    beta_val()*(1/durinf_val()+input$in_indbirth/(durlife_val()*365.25))^(-1)
    
  })
  
  ## PLOT INFECTED OVER TIME
  
  plot_i <- reactive({
    
    dd_long <- (out_dd() 
                |> filter(time < input$in_maxtime*365.25)
    )
    
    maxI <- dd_long |> pull(I) |> max()
    
    (ggplot(data = dd_long
            , mapping = aes(x= time.years, y = I))
      + labs(x = 'Time (years)', y = 'Number infected (I)')
      + geom_line(lwd = 2.5, col = '#CD2626', alpha = 0.8)
      + scale_y_continuous(limits = c(0,maxI*1.08)) 
      + theme_minimal(base_size = in_basetext)
    )
    
  })
  
  ## PLOT INFECTED and R EFF OVER TIME
  
  plot_ireff <- reactive({
    
    if (input$in_indbirth==TRUE) {
      
      dd_long <- (out_dd() 
                  |> filter(time < input$in_maxtime*365.25)
                  |> mutate(reff = S/in_N*beta_val()*(1/durinf_val()+1/(durlife_val()*365.25))^(-1))
      )
      
    } else { 
      
      dd_long <- (out_dd() 
                  |> filter(time < input$in_maxtime*365.25)
                  |> mutate(reff = S/in_N*beta_val()*durinf_val())
      )
      
    }
    
    maxt <- dd_long |> pull(time.years) |> max()
    maxI <- dd_long |> pull(I) |> max()
    maxReff <- dd_long |> pull(reff) |> max()
    if (maxReff < 1.1){
      maxReff <- 1.1
    }
    
    dd_long <- (dd_long
                |> mutate(reffScaled = reff/maxReff*maxI*1.05)
                |> select(time.years, I, reffScaled)
                |> pivot_longer(cols = c(I, reffScaled)
                                , values_to = 'value'
                                , names_to = 'metric')
    )
    
    (ggplot(data = dd_long
            , mapping = aes(x= time.years, y = value, colour = metric))
      + labs(x = 'Time (years)')
      + geom_line(lwd = 2.5, alpha = 0.8)
      + geom_hline(yintercept = 1/maxReff*maxI*1.05, col = 'grey60', lty = 2, lwd = 2)
      + scale_y_continuous(name = 'Number infected (I)'
                           , limits = c(0,maxI*1.08)
                           , sec.axis = sec_axis(~.*maxReff/maxI/1.05, name='Effective reproductive number'))
      + scale_colour_manual(name = NULL
                            , labels = c('Infected\n(left axis)', 'Effective reproductive number\n(right axis)')
                            , values = c(reffScaled = 'black', I = '#CD2626'))
      + annotate('text', x = maxt*0.90, y = 1/maxReff*maxI*1.05, label = "R_eff = 1",
                  color = "grey50", vjust = -0.5, size = 5)
      + theme_minimal(base_size = in_basetext)
      + theme(legend.position = 'top'
              , legend.text = element_text(size = in_basetext-1))
    )
    
  })
  
  ## PLOT ALL COMPARTMENTS / STATES OVER TIME
  
  plot_allstates <- reactive({
    
    dd_long <- (out_dd()
                |> select(-infections)
                |> filter(time < input$in_maxtime*365.25)
                |> pivot_longer(cols = c(S,I,R)
                                , values_to = 'count'
                                , names_to = 'compartment')
                |> mutate(compartment = factor(compartment, levels = c('S','I','R'))) 
    )
    
    (ggplot(data = dd_long
            , mapping = aes(x= time.years, y = count, colour = compartment))
      + labs(x = 'Time (years)', y = 'Population count')
      + geom_line(lwd = 2.5, alpha = 0.8)
      + scale_colour_manual(name = NULL
                            , values = c(S = '#4682B4', I = '#CD2626', R = '#FF8C00')
      )
      + scale_y_continuous(limits = c(0,in_N*1.01)) 
      + theme_minimal(base_size = in_basetext)
      + theme(legend.position = 'top'
              , legend.text = element_text(size = in_basetext-1))
    )
    
  })
  
  ## PLOT INCIDENCE AND PREVALENCE
  
  plot_incprev <- reactive({
    
    dd_long <- (out_dd()
                |> filter(time < input$in_maxtime*365.25)
                |> mutate(prev = I/in_N*100
                          , inc = beta_val()*S*I/in_N)
    )
    
    maxInc <- dd_long |> pull(inc) |> max()
    if (maxInc < 0.5){
      maxInc <- 0.5
    }
    maxPrev <- dd_long |> pull(prev) |> max()
    
    dd_long <- (dd_long
                |> mutate(incScaled = inc/maxInc*maxPrev*0.8)
                |> select(time.years, incScaled, prev)
                |> pivot_longer(cols = c(incScaled,prev)
                                , values_to = 'value'
                                , names_to = 'measure')
                |> mutate(measure = factor(measure, levels = c('prev','incScaled'))) 
    )
    
    (ggplot(data = dd_long
            , mapping = aes(x= time.years, y = value, colour = measure))
      + labs(x = 'Time (years)')
      + geom_line(lwd = 2.5, alpha = 0.6)
      + scale_colour_manual(name = NULL
                            , values = c(prev = 'magenta', incScaled = 'blue3')
                            , labels = c('Prevalence\n(left axis)', 'Incidence\n(right axis)'))
      + scale_y_continuous(name = 'Prevalence (I/N), as a %'
                           , limits = c(0,maxPrev*1.05)
                           , sec.axis = sec_axis(~.*maxInc/maxPrev/0.8, name='Incidence (new cases per day)'))
      + theme_minimal(base_size = in_basetext)
      + theme(legend.position = 'top'
              , legend.text = element_text(size = in_basetext-1))
    )
    
  })
  
  ## PLOT ALL COMPARTMENTS / STATES OVER TIME - LONGER TIME HORIZON
  
  plot_allstates_20y <- reactive({
    
    dd_long <- (out_dd()
                |> select(-infections)
                |> pivot_longer(cols = c(S,I,R)
                                , values_to = 'count'
                                , names_to = 'compartment')
                |> mutate(compartment = factor(compartment, levels = c('S','I','R'))) 
    )
    
    (ggplot(data = dd_long
            , mapping = aes(x= time.years, y = count, colour = compartment))
      + labs(x = 'Time (years)', y = 'Population count')
      + geom_line(lwd = 2.5, alpha = 0.8)      
      + scale_colour_manual(name = NULL
                            , values = c(S = '#4682B4', I = '#CD2626', R = '#FF8C00')
      )
      + scale_y_continuous(limits = c(0,in_N*1.01))
      + theme_minimal(base_size = in_basetext)
      + theme(legend.position = 'top'
              , legend.text = element_text(size = in_basetext-1))
    )
    
  })
  
  ## SOME MEASURES
  
  output$calcr0 <- renderText({
    sprintf('R0: %1.2f', out_ro())
  })
  
  output$tabinfections <- renderTable({
    (out_dd() 
    |> mutate(
      time.5yr  = cumsum(time > 365.25*5) == 1
      , time.10yr = cumsum(time > 365.25*10) == 1
      , time.20yr = time == 365.25*20
    )
    |> filter(time.5yr | time.10yr | time.20yr)
    |> transmute(
      'Time (years)'       = format(round(time.years, 0), nsmall = 0)
      , 'Total infections' = format(round(infections, 0), nsmall = 0)
    ))
  })
  
  #
  #
  
  ## GATHER OUTPUTS
  
  output$plot_allstates_a <- renderPlot({ plot_allstates() })
  output$plot_allstates_b <- renderPlot({ plot_allstates() })
  output$plot_allstates_20y <- renderPlot({ plot_allstates_20y() })
  output$plot_i <- renderPlot({ plot_i() })
  output$plot_ireff_a <- renderPlot({ plot_ireff() })
  output$plot_ireff_b <- renderPlot({ plot_ireff() })
  output$plot_incprev_a <- renderPlot({ plot_incprev() })
  output$plot_incprev_b <- renderPlot({ plot_incprev() })
  
}


### Run the app --------


shinyApp(ui = ui, server = server)

