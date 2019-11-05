library(shiny)

ui <- fluidPage(
   
   # Application title
   titlePanel("Density Histograms to Density Functions"),
   
   withMathJax(),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("n",
                     HTML("# of Bins:"),
                     min = 2,
                     max = 200,
                     value = 2, step = 1),
         sliderInput("s",
                     HTML("Shape Parameter for Distribution:"),
                     min = 1,
                     max = 10,
                     value = 2, step = 1),
         checkboxInput(inputId = 'showPopDist', label = HTML('<h4>Show population density function?</h4>'), value = FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("densHist")
      )
   )
)

server <- function(input, output) {
	
	phist = function(xs, ps,ymax){
	  dx = xs[2] - xs[1]
	  my.hist = hist(rnorm(10), plot = FALSE)

	  my.hist$breaks = seq(min(xs), max(xs) + dx, dx)
	  my.hist$density = ps
  
	  plot(my.hist, freq = FALSE, xlab = 'Range Values', ylab = 'Density Histogram', main = '', ylim = c(-0.5, ymax), xaxt = 'n')
	}
  
  observe({
	  input$n
  })
  
   output$densHist <- renderPlot({
	   par(mar=c(5,5,6,1), cex.lab = 2, cex.axis = 2, lwd = 3)

	   a = 0
	   b = 1
	   
	   n = input$n
	   s = input$s
	   
	     dx = (b-a)/n

	   xs = seq(a, b - dx, by = dx)

	   ps = rep(0, length(xs))

	   for (x.ind in 1:length(xs)){
	     ps[x.ind] = (pbeta(xs[x.ind] + dx, s, 5) - pbeta(xs[x.ind], s, 5))/dx
	   }

	   ymax = 5
	   phist(xs, ps, ymax)
	   axis(side = 1, at = seq(0, 1, by = 0.1))
	   title(main = sprintf('# Bins = %g\nLength(Bin) = %f\nTotal Area Under Bars = %f', n, dx, sum(ps)*dx), cex.main = 2)
	   
	   if(input$showPopDist) {
	     xs = seq(0, 1, by = 0.01)
	     ds = dbeta(xs, s, 5)
	     
	     lines(xs, ds, col = 'blue', lwd = 3)
	     lines(c(-1, 0), c(0, 0), col = 'blue', lwd = 3)
	   }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)