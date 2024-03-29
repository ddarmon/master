library(shiny)
library(knitr)
library(kableExtra)

ui <- fluidPage(
   
   # Application title
   titlePanel("Sampling Distribution of the Sample Mean"),
   withMathJax(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'popDist', label = 'Population Distribution', choices = c('Fixed Data', 'Normal', 'Uniform', 'Log-Normal')),
        # sliderInput("n", "Size of random sample \\(n\\):", min = 2, max = 1000, value = 2),
        numericInput("n", "Sample Size:", min = 1, max = 1000, value = 2),
        # sliderInput("num.samples", "Number of Samples to Draw", min = 1, max = 1000, value = 1),
        numericInput("num.samples", "Collect \\(S\\) additional random samples of size \\(n\\):", min = 1, max = 1000, value = 1),
        actionButton("go", label = "Draw samples"),
        actionButton("clear", label = "Clear samples"),
        checkboxInput(inputId = 'showGaussian', label = 'Show Gaussian overlay?', value = FALSE),
        htmlOutput("stats_html"),
        tableOutput("data_kable")
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
  draw.sample = reactiveValues()
  rng = reactiveValues()
  
  draw.sample$x = NULL
  draw.sample$sample.statistics = NULL
  
  # For Uniform:
  a = -3
  b =  3
  
  my.runif = function(n){
    return(runif(n, min = a, max = b))
  }
  
  # For Log-normal
  
  m = 0
  s = 1
  mu = exp(m + s^2/2)
  sigma = sqrt((exp(s^2) - 1)*exp(2*m + s^2))
  
  my.rlnorm = function(n){
    return(rlnorm(n, meanlog = m, sdlog = s))
  }
  
  # hard.data = 1:10
  # hard.data = runif(14)
  
  # N = 14
  # hard.data = sample(52:68, size = N, replace = FALSE)
  # hard.data = sample(52:68, size = N, replace = TRUE)
  # hard.data = sample(c(52, 60, 68), size = N, replace = TRUE)
  
  # Heights
  # hard.data = c(64, 63.6, 72, 63, 66, 69, 62, 63, 63, 62)
  
  # Handedness
  hard.data = c(3, 10, 8, 10, 10, 10, 10, 8, 10, 10)
  
  tab = table(hard.data)
  
  dHD = as.numeric(tab/sum(tab))
  
  xHD = as.numeric(names(tab))
  
  
  my.sample = function(n){
    return(sample(hard.data, size = n, replace = TRUE))
  }
  
  observe({
    input$popDist
    if (input$popDist == 'Fixed Data'){
      rng$plot.at = xHD
    }
  })
  
  observe({
    input$go
    
    rng$r = switch(input$popDist,
                   "Normal" = rnorm,
                   "Uniform" = my.runif,
                   "Log-Normal" = my.rlnorm,
                   "Fixed Data" = my.sample)
    
    rng$d = switch(input$popDist,
                   "Normal" = dnorm,
                   "Uniform" = function(x) dunif(x, min = a, max = b),
                   "Log-Normal" = function(x) dlnorm(x, m, s),
                   "Fixed Data" = dHD)
    
    rng$mean = switch(input$popDist,
                   "Normal" = 0,
                   "Uniform" = (a + b)/2,
                   "Log-Normal" = mu,
                   "Fixed Data" = mean(hard.data))
    
    rng$sd  = switch(input$popDist,
                     "Normal" = 1,
                     "Uniform" = sqrt((b - a)^2/12),
                     "Log-Normal" = s,
                     "Fixed Data" = sqrt(mean((hard.data - mean(hard.data))^2)))
    
    rng$median  = switch(input$popDist,
                     "Normal" = 0,
                     "Uniform" = (a + b)/2,
                     "Log-Normal" = exp(m),
                     "Fixed Data" = median(hard.data))
    
    rng$range = switch(input$popDist,
                       "Normal" = c(-6, 6),
                       "Uniform" = c(-6, 6),
                       "Log-Normal" = c(-6, 6),
                       "Fixed Data" = range(hard.data) + c(-1, 1)*IQR(hard.data))
    
    if (input$num.samples == 1){
      draw.sample$x = rng$r(input$n)
      
      sample.statistics = mean(draw.sample$x)
      
      draw.sample$sample.statistics =
        c(isolate(draw.sample$sample.statistics), sample.statistics)
    }else{
      draw.sample$x = matrix(rng$r(input$n*input$num.samples), nrow = input$num.samples, ncol = input$n)

      sample.statistics = rowMeans(isolate(draw.sample$x))

      draw.sample$x = isolate(draw.sample$x[input$num.samples, ])

      draw.sample$sample.statistics =
        c(isolate(draw.sample$sample.statistics), sample.statistics)
    }
    })
  
  observe({
    input$n
    input$popDist
    
    # draw.sample$x = rng$r(input$n)
    # draw.sample$sample.statistics = mean(isolate(draw.sample$x))
    
    draw.sample$x = NULL
    draw.sample$sample.statistics = NULL
  })
  
  observe({
    input$clear

    if (input$num.samples == 1){
      draw.sample$x = NULL
      draw.sample$sample.statistics = NULL
    }else{
      draw.sample$x = rng$r(isolate(input$n))
      draw.sample$sample.statistics = c(mean(isolate(draw.sample$x)))
    }
  })
  
   output$distPlot <- renderPlot({
     jitter.amount = diff(rng$range)*0.01
     
     if (input$popDist == 'Fixed Data'){
       plot.at = rng$plot.at
     }
     
      # generate bins based on input$bins from ui.R
      xs = seq(-6, 6, by = 0.001)
      
      ticksize.use = 0.1
      lwd.use = 2
      
      if (input$popDist == 'Log-Normal'){
        ylim.use = 1.0
      }else{
        ylim.use = 1.0
      }
      
      
      par(mfrow = c(3, 1), mar=c(5,8,2,1), cex.lab = 2, cex.axis = 2)
      if (input$popDist == 'Fixed Data'){
        plot(0, 0, cex = 0, xlim = rng$range, ylim = c(0, ylim.use), xlab = 'Measurement Values', ylab = expression(atop('Population Distribution')), main = '')
        
        segments(x0 = plot.at, y0 = 0, y1 = rng$d, lwd = 2)
        points(plot.at, rng$d, pch = 16, cex = 2)
      }else{
        plot(0, 0, cex = 0, xlim = rng$range, ylim = c(0, ylim.use), xlab = 'Measurement Values', ylab = expression(atop('Population Distribution')), main = '')
        lines(xs, rng$d(xs), col = 'blue', lwd = 2)
      }
      abline(v = rng$mean, col = 'blue', lwd = 2, lty = 2)
      
      ylim.rug = -(ylim.use*0.15)
      
      if (input$popDist == 'Fixed Data'){
        plot(0, 0, cex = 0, xlim = rng$range, ylim = c(ylim.rug, ylim.use), xlab = 'Sample Data', ylab = expression(atop('Density Histogram', 'of Sample')), main = '', yaxt = 'n')
      }else{
        plot(0, 0, cex = 0, xlim = rng$range, ylim = c(ylim.rug, ylim.use), xlab = 'Sample Data', ylab = expression(atop('Density Histogram', 'of Sample')), main = '', yaxt = 'n')
      }
      
      if (is.null(draw.sample$x) == FALSE){
        hist(draw.sample$x, freq = FALSE, add = T, breaks = 'Scott')
        segments(x0 = jitter(draw.sample$x, factor = jitter.amount), y0 = ylim.rug, y1 = -0.005, lwd = lwd.use)
        abline(v = tail(draw.sample$sample.statistics, 1), col = 'red', lwd = 2)
      }
      abline(v = rng$mean, col = 'blue', lwd = 2, lty = 2)
      axis(side = 2, at = seq(0, ylim.use, 0.25))
      
      N = length(draw.sample$sample.statistics)
      
      ticksize.use = 0.05
      lwd.use = 3
      
      mean.xbar  = rng$mean
      sigma.xbar = rng$sd/sqrt(input$n)
      
      f0 = dnorm(x = 0, mean = 0, sd = sigma.xbar)
      
      xs = seq(rng$range[1], rng$range[2], by = 0.001)
      
      if (input$popDist == 'Fixed Data'){
        ylim.max = 2
      }else{
        ylim.max = 2.5*f0
      }
      
      ylim.rug = -(max(ylim.max)*0.15)
      
      
      if (N > 1){
        hist(draw.sample$sample.statistics, breaks = 'scott', freq = FALSE, xlim = rng$range, ylim = c(ylim.rug, ylim.max), xlab = 'Sample Means', ylab = expression(atop('Density Histogram', 'of' ~ bar(italic(X)))), main = '', yaxt = 'n')
        
        segments(x0 = draw.sample$sample.statistics[1:(N-1)], y0 = ylim.rug, y1 = 0.2*ylim.rug, col = 'darkorange', lwd = lwd.use)
        segments(x0 = draw.sample$sample.statistics[N], y0 = ylim.rug, y1 = 0.2*ylim.rug, col = 'red', lwd = lwd.use)
      }else{
        hist(draw.sample$sample.statistics, freq = FALSE, xlim = rng$range, ylim = c(ylim.rug, ylim.max), xlab = 'Sample Means', ylab = expression(atop('Density Histogram', 'of' ~ bar(italic(X)))), main = '', yaxt = 'n')
        segments(x0 = draw.sample$sample.statistics, y0 = ylim.rug, y1 = 0.2*ylim.rug, col = 'red', lwd = lwd.use)
      }
      if(input$showGaussian){
        lines(xs, dnorm(xs, mean = mean.xbar, sd = sigma.xbar), col = 'blue', lwd = 2)
      }
      axis(side = 2, at = seq(0, ylim.max, 0.25))
   }, height = 650)
   
   output$data_kable <- function() {
     my.df = data.frame(x = draw.sample$x)
     colnames(my.df) = 'Sampled Values'
     
     kable(my.df, format = 'html') %>% kable_styling(font_size = 20)
   }
   
   output$stats_html <- renderUI({
     x = draw.sample$x
     my.df = data.frame(xbar = mean(x), s = sd(x), median = median(x))
     
     withMathJax(HTML(sprintf("<p><p><font size='4'><strong>Sample Statistics</strong>
                  <font size='5'><table>
                              <tr>
                              <th>Parameter</th>
                              <th></th> 
                              <th>Statistic</th>
                              <th></th>
                              </tr>
                              <tr>
                              <td align = 'right'>\\(\\mu = \\) 
                              <td align = 'right'>%.3f
                              <td align = 'right'>\\(\\bar{x} = \\) 
                              <td align = 'right'>%.3f
                              </tr>
                              <tr>
                              <td align = 'right'>\\(\\sigma = \\) 
                              <td align = 'right'>%.3f
                              <td align = 'right'>\\(s = \\) 
                              <td align = 'right'>%.3f
                              </tr>
                              <tr>
                              <td align = 'right'>\\(\\tilde{\\mu} = \\) 
                              <td align = 'right'>%.3f
                              <td align = 'right'>\\(\\tilde{x} = \\) 
                              <td align = 'right'>%.3f
                              </tr>
                              </table>
                  
                  <p><p>", rng$mean, mean(x), rng$sd, sd(x), rng$median, median(x))))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

