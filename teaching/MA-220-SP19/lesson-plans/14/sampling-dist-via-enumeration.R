library(shiny)

from.xs.to.ps = function(xs, p){
  return(p[ceiling(xs + 0.5)])
}

ui <- fluidPage(
  
  withMathJax(),
   # Application title
   titlePanel("Enumeration of Samples from a Finite Population"),

   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'popDist', label = 'Population Distribution', choices = c('Uniform Box', 'Ramp Box', 'Random Box')),
         sliderInput("n",
                     HTML("Sample size \\(n\\):"),
                     min = 1,
                     max = 50,
                     value = 1, step = 1),
        checkboxInput(inputId = 'showGaussian', label = 'Show Gaussian overlay?', value = FALSE),
         plotOutput("plotDist")
      ),

      mainPanel(
        tableOutput("enum_kable")
      )
   )
)

server <- function(input, output) {
  library(dplyr)
  library(kableExtra)
  
  rng = reactiveValues()
  
  # rng$p.start = rep(1/10, 10)
  
  box.model = 0:9
  
  rng$box.model = box.model
  
  observe({
    # input$popDist

    if (input$popDist == 'Uniform Box'){
      rng$p.start = rep(1/10, 10)
    } else if (input$popDist == 'Ramp Box'){
      # set.seed(2)
      # rng$p.start = runif(10)
      # rng$p.start = rng$p.start/sum(rng$p.start)
      
      rng$p.start = (1:10)/sum(1:10)
    } else if (input$popDist == 'Random Box'){
      u = runif(10)
      rng$p.start = u/sum(u)
    }
  })
  
  output$plotDist = renderPlot({
    ns = seq(0, length(rng$p.start) - 1)
    n = 1
    
    xs = seq(0.01, n*9 + 2, by = 0.01) - 0.5
    
    ps = from.xs.to.ps(xs, rng$p.start)
    
    p = rng$p.start
    
    par(mfrow = c(2, 1), mar=c(5,7,2,1), cex.lab = 2, cex.axis = 2)
    plot(0, 0, cex = 0, xlim = range(rng$box.model), ylim = c(0, 1), xaxt = 'n', xlab = expression(italic(x)), ylab = expression(P(italic(X == x))))
    points(rng$box.model, rng$p.start, pch = 16, cex = 2)
    segments(x0 = rng$box.model, y0 = 0, y1 = rng$p.start, lwd = 2)
    axis(1, at = rng$box.model)
    abline(v = sum(p*ns/n), col = 'blue', lwd = 2)

    xlab.use = expression(bar(italic(x))[italic(n)])
    ylab.use = expression(P(bar(italic(X))[italic(n)] == bar(italic(x))[italic(n)]))
    
    mu.xbar = sum(p*ns/n)
    sigma.xbar = sqrt(sum(p*(ns/n)^2) - mu.xbar^2)
    
    sigma.box = sqrt(sum(p*(ns/n)^2) - mu.xbar^2)
    
    from.xs.to.ps = function(xs, p){
      return(p[ceiling(xs + 0.5)])
    }
    
    ps = from.xs.to.ps(xs, p)
    
    for (i in 1:input$n){
      n = i + 1
      
      p = convolve(rng$p.start, rev(p), type = 'o')
      
      xs = seq(0.01, n*9 + 1, by = 0.01) - 0.5
      
      ps = from.xs.to.ps(xs, p)
      
      ns = seq(0, length(p) - 1)
      
      mu.xbar = sum(p*ns/n)
      sigma.xbar = sqrt(sum(p*(ns/n)^2) - mu.xbar^2)
      
      if (n == isolate(input$n)){
        plot(ns/n, p, ylim = c(0, 0.2), pch = 16, cex = 0, xlab = xlab.use, ylab = ylab.use)
        
        lines(xs/n, ps, lwd = 2)
        if (n <= 10){
          segments(ns/n-0.5/n, 0, ns/n-0.5/n, p, lwd = 2)
          segments(ns/n+0.5/n, 0, ns/n+0.5/n, p, lwd = 2)
          abline(h = 0, lwd = 1)
        }
        abline(v = mu.xbar, col = 'blue', lwd = 2)
        abline(v = c(mu.xbar - sigma.xbar, mu.xbar + sigma.xbar), lty = 2)
        if(input$showGaussian){
          x.for.norm = xs/n
          lines(x.for.norm, (1/n)*dnorm(x.for.norm, mean = mu.xbar, sd = sigma.xbar), col = 'red', lwd = 4)
        }
      }
    }
  }, height = 800)
  
  output$enum_kable <- function() {
    if (input$n <= 7){
    p.start = rng$p.start
    box.model = rng$box.model

    m = length(box.model)
    
    n = input$n
    
    # ss.command = sprintf('as.matrix(expand.grid(', , '))', sep = "")
    s = paste0(rep('box.model', n), sep = ',', collapse = "")
    s = substring(s, 1, nchar(s) - 1)
    
    e = parse(text = paste0('as.matrix(expand.grid(', s, '))', collapse = ""))
    sample.space = eval(e)
    
    MAT = matrix(NA, nrow = min(dim(sample.space)[1], 1000), ncol = 5)
    
    for (omega in 1:min(dim(sample.space)[1], 1000)){
      MAT[omega, 1] = toString(sample.space[omega, ])
      
      cur.row = sample.space[omega, ]
      
      pomega = 1.
      
      for (i in 1:dim(sample.space)[2]){
        pomega = pomega*p.start[cur.row[i] + 1]
      }
      
      MAT[omega, 2] = pomega
      
      MAT[omega, 3] = round(mean(cur.row), digits = 5)
      
      sd.use = sqrt(var(cur.row)*((n-1)/n))
      
      MAT[omega, 4] = round(sd.use, digits = 3)
      
      MAT[omega, 5] = median(cur.row)
    }
    
    my.dat = data.frame(MAT)
    
    colnames(my.dat) = c('Sampled Values', 'Prob. of Sample', 'Mean', 'SD', 'Median')
    
    kable(my.dat, format = 'html')
    my.dat %>%
      knitr::kable(format = "html", digits = 2) %>%
      kable_styling("striped", font_size = 32, full_width = F)
    }
  }
}

# Run the application 
shinyApp(ui = ui, server = server)