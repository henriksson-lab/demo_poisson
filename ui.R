library(plotly)
library(shiny)
library(ggplot2)

################################################################################
########### Samplemeta #########################################################
################################################################################


tab_about <- fluidPage(
  p("This demo was originally developed by ", a("Johan Henriksson", href="http://www.henlab.org")),
  p("Licensed under 2-clause BSD license, https://opensource.org/license/bsd-2-clause/")
)

################################################################################
########### Shrinkage tab ######################################################
################################################################################

tab_shrinkage <- fluidPage(
  
  
  fluidRow(
    
    checkboxInput(
      inputId = "shrinkage_enable", 
      label = "Enable shrinkage",
      value = TRUE),
    p("Red lines show the shrinkage applied. The new point is a biased estimate."),
    
    
    column(6,
           
           div(class = "label-left",
               
               sliderInput(
                 inputId = "shrinkage_random_seed1",
                 label = "Random seed:",
                 min=0,
                 max=10,
                 step = 1,
                 value=1
               ),
               
               sliderInput(
                 inputId = "shrinkage_num_samples1",
                 label = "Number of samples:",
                 min=0,
                 max=1000,
                 step = 1,
                 value=100
               ),
               
               sliderInput(
                 inputId = "shrinkage_lambda1",
                 label = "Poisson lambda parameter:",
                 min=0,
                 max=1000,
                 step = 1,
                 value=10
               ),
               p("X/Y, with X and Y Poisson distributed:"),
               plotOutput(outputId = "plotShrinkage1", height = "400px")
           ),
    ),
    column(6,
           
           div(class = "label-left",
               sliderInput(
                 inputId = "shrinkage_random_seed2",
                 label = "Random seed:",
                 min=0,
                 max=10,
                 step = 1,
                 value=2
               ),
               
               sliderInput(
                 inputId = "shrinkage_num_samples2",
                 label = "Number of samples:",
                 min=0,
                 max=1000,
                 step = 1,
                 value=100
               ),
               
               sliderInput(
                 inputId = "shrinkage_lambda2",
                 label = "Poisson lambda parameter:",
                 min=0,
                 max=1000,
                 step = 1,
                 value=10
               ),
               p("X/Y, with X and Y Poisson distributed:"),
               plotOutput(outputId = "plotShrinkage2", height = "400px")
           )
           
    ),
    
  )
)

################################################################################
########### Poisson tab ########################################################
################################################################################

tab_poisson <- fluidPage(
  
  
  fluidRow(
    
    column(6,

           div(class = "label-left",
               
               sliderInput(
                 inputId = "poisson_random_seed",
                 label = "Random seed:",
                 min=0,
                 max=10,
                 step = 1,
                 value=1
               ),
               
               sliderInput(
                 inputId = "poisson_num_samples",
                 label = "Number of samples:",
                 min=0,
                 max=1000,
                 step = 1,
                 value=1
               ),
               
               sliderInput(
                 inputId = "poisson_lambda",
                 label = "Poisson lambda parameter:",
                 min=0,
                 max=1000,
                 step = 1,
                 value=1
               ),
               
               uiOutput(outputId = "plotPoissonStats"),
               
               a("Wikipedia on Poisson", href="https://en.wikipedia.org/wiki/Poisson_distribution")
               
           )
    ),
    column(6,
           plotOutput(outputId = "plotPoissonHist", height = "400px"),
    )
  )
)

################################################################################
########### Sampling tab #######################################################
################################################################################


tab_sampling <- fluidPage(
  fluidRow(
    column(6,
           div(class = "label-left",
               
               sliderInput(
                 inputId = "sampling_random_seed",
                 label = "Random seed:",
                 min=0,
                 max=10,
                 step = 1,
                 value=1
               ),
               
               sliderInput(
                 inputId = "sampling_num_points",
                 label = "Number of points:",
                 min=1,
                 max=100,
                 step = 1,
                 value=1
               ),
               
               sliderInput(
                 inputId = "sampling_num_samples",
                 label = "Number of samples:",
                 min=0,
                 max=1000,
                 step = 1,
                 value=1
               ),
           ),
           
           p("Statistics frequently assumes that samples are independent. Once any point is sampled more than once, the samples are no longer independent"),
           
           p("Number of repeated samples:"),
           tableOutput(outputId = "plotSamplingTable")
    ),
    column(6,
           plotOutput(outputId = "plotSamplingScatter", height = "400px"),
    ),
  )
)



################################################################################
########### Total page #########################################################
################################################################################

#https://stackoverflow.com/questions/72040479/how-to-position-label-beside-slider-in-r-shiny

ui <- fluidPage(
  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
  )),
  
  titlePanel("Demo of Poisson distribution and shrinkage"),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Poisson distribution", tab_poisson),
                tabPanel("Shrinkage of X/Y", tab_shrinkage),
                tabPanel("Repeated sampling", tab_sampling),
                tabPanel("About", tab_about)
    )
  )
  
)



