from.xs.to.ps = function(xs, p){
  return(p[ceiling(xs + 0.5)])
}

# data.type = 'uniform'
data.type = 'random'

if (data.type == 'uniform'){
  p.start = rep(1/10, 10)
} else if (data.type == 'random'){
  set.seed(2)
  p.start = runif(10)
  # p.start = c(rep(.25, 5), rep(0.5, 5))
  p.start = p.start/sum(p.start)
}

ns = seq(0, length(p.start) - 1)

xlab.use = expression(bar(italic(x))[italic(n)])
ylab.use = expression(P(bar(italic(X))[italic(n)] == bar(italic(x))[italic(n)]))

n = 1

xs = seq(0.01, n*9 + 2, by = 0.01) - 0.5

ps = from.xs.to.ps(xs, p.start)

p = p.start

mu.xbar = sum(p*ns/n)
sigma.xbar = sqrt(sum(p*(ns/n)^2) - mu.xbar^2)

# pdf(sprintf('figures/mean-%s-n%g.pdf', data.type, n))
jpeg.name = sprintf('figures/mean-%s-n%g.jpeg', data.type, n)
jpeg(jpeg.name, width = 480*2, height = 480)
mar.use = 6
par(mar=c(5,5,2,1), cex.lab = 2, cex.axis = 2, mar = c(mar.use, mar.use, mar.use, mar.use))
plot(ns/n, p.start, ylim = c(0, 0.2), pch = 16, cex = 0, main = substitute(italic(n) == N, list(N = n)), xlab = xlab.use, ylab = ylab.use)

lines(xs/n, ps, lwd = 2)
segments((ns-0.5)/n, 0, (ns-0.5)/n, p.start, lwd = 2)
segments((ns+0.5)/n, 0, (ns+0.5)/n, p.start, lwd = 2)
abline(h = 0, lwd = 1)
abline(v = mu.xbar, col = 'blue', lwd = 2)
abline(v = c(mu.xbar - sigma.xbar, mu.xbar + sigma.xbar), lty = 2)

p = p.start

from.xs.to.ps = function(xs, p){
  return(p[ceiling(xs + 0.5)])
}

ps = from.xs.to.ps(xs, p)
lines(xs, ps, lwd = 2)
dev.off()

cat(sprintf('<img src = "sampling-dist-demo/%s">\n', jpeg.name))

cat(sprintf("E[Sn] = %f\n", sum(p*ns)))

for (i in 1:100){
  n = i + 1
  
  p = convolve(p.start, rev(p), type = 'o')
  # p = convolve(p.start, p, type = 'open')
  # p = convolve(c(p.start, rep(0, (n-1)*length(ns))), c(p, rep(0, length(ns))), type = 'open')
  
  xs = seq(0.01, n*9 + 1, by = 0.01) - 0.5
  
  ps = from.xs.to.ps(xs, p)
  
  ns = seq(0, length(p) - 1)
  
  mu.xbar = sum(p*ns/n)
  sigma.xbar = sqrt(sum(p*(ns/n)^2) - mu.xbar^2)
  
  if ((n <= 10) || (n == 30) || (n == 100)){
    # pdf(sprintf('figures/mean-%s-n%g.pdf', data.type, n))
    jpeg.name = sprintf('figures/mean-%s-n%g.jpeg', data.type, n)
    jpeg(jpeg.name, width = 480*2, height = 480)
    par(mar=c(5,5,2,1), cex.lab = 2, cex.axis = 2, mar = c(mar.use, mar.use, mar.use, mar.use))
    plot(ns/n, p, ylim = c(0, 0.2), pch = 16, cex = 0, main = substitute(italic(n) == N, list(N = n)), xlab = xlab.use, ylab = ylab.use)
    
    lines(xs/n, ps, lwd = 2)
    if (n < 30){
      segments(ns/n-0.5/n, 0, ns/n-0.5/n, p, lwd = 2)
      segments(ns/n+0.5/n, 0, ns/n+0.5/n, p, lwd = 2)
      abline(h = 0, lwd = 1)
    }
    abline(h = 0, lwd = 1)
    abline(v = mu.xbar, col = 'blue', lwd = 2)
    abline(v = c(mu.xbar - sigma.xbar, mu.xbar + sigma.xbar), lty = 2)
    dev.off()
    
    # pdf(sprintf('figures/mean-w-normal-%s-n%g.pdf', data.type, n))
    jpeg.name = sprintf('figures/mean-w-normal-%s-n%g.jpeg', data.type, n)
    jpeg(jpeg.name, width = 480*2, height = 480)
    par(mar=c(5,5,2,1), cex.lab = 2, cex.axis = 2, mar = c(mar.use, mar.use, mar.use, mar.use))
    plot(ns/n, p, ylim = c(0, 0.2), pch = 16, cex = 0, main = substitute(italic(n) == N, list(N = n)), xlab = xlab.use, ylab = ylab.use)
    
    lines(xs/n, ps, lwd = 2)
    if (n < 30){
      segments(ns/n-0.5/n, 0, ns/n-0.5/n, p, lwd = 2)
      segments(ns/n+0.5/n, 0, ns/n+0.5/n, p, lwd = 2)
      abline(h = 0, lwd = 1)
    }
    abline(h = 0, lwd = 1)
    abline(v = mu.xbar, col = 'blue', lwd = 2)
    abline(v = c(mu.xbar - sigma.xbar, mu.xbar + sigma.xbar), lty = 2)
    x.for.norm = xs/n
    lines(x.for.norm, (1/n)*dnorm(x.for.norm, mean = mu.xbar, sd = sigma.xbar), col = 'red', lwd = 4)
    dev.off()
    
    # cat(sprintf("E[Sn] = %f\n", mu.xbar))
    # cat(sprintf('<img src = "sampling-dist-demo/%s">\n', jpeg.name))
  }
}
