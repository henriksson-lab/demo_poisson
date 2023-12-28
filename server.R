library(plotly)
library(Cairo)
options(shiny.usecairo=T)


if(FALSE){
  install.packages("matlib")
}



if(FALSE){
  #To run this app
  library(shiny)
  runApp(".")
}


server <- function(input, output, session) {


  ##############################################################################
  ########### General functions ################################################
  ##############################################################################
  
  
  ##############################################################################
  ########### Callbacks - dataset ##############################################
  ##############################################################################
  
#  observeEvent(c(input$input_ds),{
    ######### Side bar
    #updateSliderInput(session, "num_training_point", min=0, max=nrow(thedat), value = nrow(thedat), step = 1)
 # })

  
  ##############################################################################
  ########### Sampling tab #####################################################
  ##############################################################################

  sampling_points <- reactive({
    
    set.seed(input$sampling_random_seed)
    
    points <- data.frame(
      x=runif(input$sampling_num_points),
      y=runif(input$sampling_num_points),
      sampled=0
    )
    
    samples <- sample(1:nrow(points),input$sampling_num_samples, replace = TRUE)
    tab_samples <- table(samples)
    
    points$sampled[as.integer(names(tab_samples))] <- tab_samples

    histo <- table(tab_samples)
    
    num_rep_samples <- rbind(
      data.frame(
        numsamples=0,
        cnt=sum(points$sampled==0)
      ),
      data.frame(
        numsamples=names(histo),
        cnt=as.integer(histo)
      )
    )

    list(
      points=points,
      samples=samples,
      num_rep_samples=num_rep_samples
    )
  })
  
  
  output$plotSamplingScatter <- renderPlot({
    points <- sampling_points()$points
    p1 <- ggplot(points, aes(x,y,color=sampled)) + geom_point()+
      theme_minimal()
    p2 <- ggplot(sampling_points()$num_rep_samples, aes(numsamples,cnt)) + geom_bar(stat = "identity") +
      theme_minimal()
    egg::ggarrange(p1,p2, ncol = 1)
    
  })

  output$plotSamplingTable <- renderTable({
    sampling_points()$num_rep_samples
  })
  
  
  ##############################################################################
  ########### Poisson tab ######################################################
  ##############################################################################
  

  poisson_points <- reactive({
    
    set.seed(input$poisson_random_seed)
    
    points <- data.frame(
      x=rpois(input$poisson_num_samples, lambda = input$poisson_lambda)
    )
    
    sampled_dist <- sqldf::sqldf("select count(x) as cnt, x from points group by x")

    real_dist <- data.frame(
      x=0:(input$poisson_lambda*3+5)
    )
    real_dist$p <- dpois(real_dist$x, lambda = input$poisson_lambda)

    list(
      points=points,
      sampled_dist=sampled_dist,
      real_dist=real_dist
    )
  })
  
  
  output$plotPoissonHist <- renderPlot({
    sol <- poisson_points()

    max_x <- max(sol$sampled_dist$x, sol$real_dist$x)
    
    p1 <- ggplot(sol$sampled_dist, aes(x,cnt)) + geom_bar(stat = "identity") +
      theme_minimal() +
      xlab("x")+ylab("Count") +
      xlim(c(0,max_x))
    p2 <- ggplot(sol$real_dist, aes(x,p)) + geom_bar(stat = "identity") +
      theme_minimal() +
      xlab("x")+ylab("Probability")+
      xlim(c(0,max_x))
    
    egg::ggarrange(p1,p2, ncol = 1)
    
  })
  
  
  output$plotPoissonStats <- renderUI({
    sol <- poisson_points()
    
    p(
      p("Average:",mean(sol$points$x)),
      p("Variance:",var(sol$points$x))
    )
  })
  
  
  
  ##############################################################################
  ########### Shrinkage tab ####################################################
  ##############################################################################
  
  shrink_one_set <- function(points, min_sd){
    mul_factor <- min_sd / sd(points$x, na.rm = TRUE)
    points$shrunk_x <- points$x * mul_factor
    points
  }
  
  shrink_points <- reactive({

    set.seed(input$shrinkage_random_seed1)
    points1 <- data.frame(
      x=log2(rpois(input$shrinkage_num_samples1, lambda = input$shrinkage_lambda1) / rpois(input$shrinkage_num_samples1, lambda = input$shrinkage_lambda1)),
      i=1:input$shrinkage_num_samples1
    )
    points1$shrunk_x <- points1$x

    set.seed(input$shrinkage_random_seed2)
    points2 <- data.frame(
      x=log2(rpois(input$shrinkage_num_samples2, lambda = input$shrinkage_lambda2) / rpois(input$shrinkage_num_samples2, lambda = input$shrinkage_lambda2)),
      i=1:input$shrinkage_num_samples2
    )
    points2$shrunk_x <- points2$x
    
    min_sd <- min(sd(points1$x, na.rm = TRUE), sd(points2$x, na.rm = TRUE))
    
    if(input$shrinkage_enable){
      if(sd(points1$x) > min_sd){
        points1 <- shrink_one_set(points1, min_sd)
      }
      if(sd(points2$x) > min_sd){
        points2 <- shrink_one_set(points2, min_sd)
      }
    }

    list(
      points1=points1,
      points2=points2
    )
  })
  
  
  getShrinkageRange <- function(){
    sol <- shrink_points()
    max(
      -min(c(sol$points1$x,sol$points1$x)),
      +max(c(sol$points2$x,sol$points2$x))
    ) 
  }
  
  plotShrinkageRange <- function(points){
    max_x <- getShrinkageRange()
    
    ### Prepare line from x to shrunk x
    all_lines <- rbind(
      data.frame(
        i=points$i,
        x=points$shrunk_x
      ),
      points[c("i","x")]
    )

    ggplot(all_lines, aes(i,x,group=i)) + 
      geom_line(color="red") +
      geom_point(data = points, mapping = aes(i,shrunk_x)) +
      theme_minimal() +
      xlab("sample")+
      ylab("log2 X/Y") +
      ylim(c(-max_x,max_x))
  }
  
  output$plotShrinkage1 <- renderPlot({
    sol <- shrink_points()
    plotShrinkageRange(sol$points1)
  })
  output$plotShrinkage2 <- renderPlot({
    sol <- shrink_points()
    plotShrinkageRange(sol$points2)
  })
  
  
  
  
}



