data.type = 'uniform'
# data.type = 'random'

if (data.type == 'uniform'){
  p.start = rep(1/10, 10)
} else if (data.type == 'random'){
  set.seed(2)
  p.start = runif(10)
  p.start = p.start/sum(p.start)
}

ns = seq(0, length(p.start) - 1)

xlab.use = expression(italic(s)[italic(n)])
ylab.use = expression(P(italic(S)[italic(n)] == italic(s)[italic(n)]))

n = 1

# pdf(sprintf('figures/sum-%s-n%g.pdf', data.type, n))
jpeg(sprintf('figures/sum-%s-n%g.jpeg', data.type, n), width = 480*2, height = 480)
mar.use = 6
par(mar=c(5,5,2,1), cex.lab = 2, cex.axis = 2, mar = c(mar.use, mar.use, mar.use, mar.use))
plot(ns, p.start, ylim = c(0, 0.2), pch = 16, cex = 0, main = substitute(italic(n) == N, list(N = n)), xlab = xlab.use, ylab = ylab.use)

lines(xs, ps, lwd = 2)
segments(ns-0.5, 0, ns-0.5, p.start, lwd = 2)
segments(ns+0.5, 0, ns+0.5, p.start, lwd = 2)
abline(h = 0, lwd = 1)

p = p.start

xs = seq(0.01, n*9 + 2, by = 0.01) - 0.5

from.xs.to.ps = function(xs, p){
  return(p[ceiling(xs + 0.5)])
}

ps = from.xs.to.ps(xs, p)
lines(xs, ps, lwd = 2)
dev.off()

cat(sprintf("E[Sn] = %f\n", sum(p*ns)))

for (i in 1:100){
  n = i + 1
  
  p = convolve(p.start, rev(p), type = 'open')
  
  xs = seq(0.01, n*9 + 1, by = 0.01) - 0.5
  
  from.xs.to.ps = function(xs, p){
    return(p[ceiling(xs + 0.5)])
  }
  
  ps = from.xs.to.ps(xs, p)
  
  ns = seq(0, length(p) - 1)
  
  if ((n <= 10) || (n == 30) || (n == 100)){
    # pdf(sprintf('figures/sum-%s-n%g.pdf', data.type, n))
    jpeg(sprintf('figures/sum-%s-n%g.jpeg', data.type, n), width = 480*2, height = 480)
    par(mar=c(5,5,2,1), cex.lab = 2, cex.axis = 2, mar = c(mar.use, mar.use, mar.use, mar.use))
    plot(ns, p, ylim = c(0, 0.2), pch = 16, cex = 0, main = substitute(italic(n) == N, list(N = n)), xlab = xlab.use, ylab = ylab.use)
    
    lines(xs, ps, lwd = 2)
    if (n < 30){
      segments(ns-0.5, 0, ns-0.5, p, lwd = 2)
      segments(ns+0.5, 0, ns+0.5, p, lwd = 2)
    }
    abline(h = 0, lwd = 1)
    dev.off()
  }
  
  cat(sprintf("E[Sn] = %f\n", sum(p*ns)))
}
