library(shiny)

server = function(input, output){
	output$auc = renderPlot({
		xs = seq(-10, 10, 0.01)*input$sd + input$mean
		ys = dnorm(xs, sd = input$sd, mean = input$mean)
		
		Fval = pnorm(input$z)
		
		lwd.use = 3
		
		par(mar=c(5,5,2,1), cex.lab = 2, cex.axis = 2, mfrow = c(2, 1))
		plot(xs, ys, main = sprintf("Area / Probability = %.10f", Fval), xlim = c(-5, 5)*input$sd + input$mean, type = 'l', lwd = lwd.use, xlab = expression(italic(x)), ylab = expression(italic(f(x))), xaxt = 'n')
		polygon(c(xs[xs < input$z*input$sd + input$mean], input$z*input$sd + input$mean), c(ys[xs<input$z*input$sd + input$mean], 0), col = 'red')
		axis(side = 1, at = seq(-5, 5, 1)*input$sd + input$mean)
		abline(v = input$z*input$sd + input$mean, lwd = 1.5)
		
		zs = pnorm(xs, sd = input$sd, mean = input$mean)
		plot(xs, zs, xlab = expression(x), ylab = expression(italic(F(x))), type = 'l', lwd = lwd.use, xlim = c(-5, 5)*input$sd + input$mean, ylim = c(0, 1), yaxt = 'n')
		axis(side = 2, at = c(0, 0.5, 1))
		
		points(input$z, pnorm(input$z, sd = input$sd, mean = input$mean), pch = 16, cex = 2, col = 'red')
	})
}

ui = fluidPage(
  withMathJax(),
  tags$p("The cumulative distribution function \\(P(X \\leq x)\\) for a normal random variable \\(X\\)."),
	# The inputId and label arguments 
	# Use ?sliderInput to find the arguments for sliderInput
	sliderInput(inputId = "z", label = "Choose a value for x:", value = 0, min = -5, max = 5, step = 0.1, width = '100%'),
	numericInput(inputId = "mean", label = HTML("&mu;:"), value = 0),
	numericInput(inputId = "sd", label = HTML("&sigma;:"), value = 1, min = 0.01),
	plotOutput("auc"))

shinyApp(ui = ui, server = server)