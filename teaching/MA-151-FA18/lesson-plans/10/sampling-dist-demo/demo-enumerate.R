getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data.type = 'uniform'
# data.type = 'random'

if (data.type == 'uniform'){
  p.start = rep(1/10, 10)
} else if (data.type == 'random'){
  set.seed(2)
  p.start = runif(10)
  p.start = p.start/sum(p.start)
}

box.model = 0:9

mean.box = mean(box.model)
sd.box   = sqrt(var(box.model)*((n-1)/n))

# sample.space = as.matrix(expand.grid(box.model, box.model))
sample.space = as.matrix(expand.grid(box.model, box.model, box.model))

n = dim(sample.space)[2]

cat(sprintf("\\begin{tabular}{c || c || c | c | c}\n"))

cat(sprintf("Sample & Prob. Sample & Mean & SD & Mode\\\\\n\\hline\n"))

for (omega in 1:dim(sample.space)[1]){
  cat(sprintf('%s', toString(sample.space[omega, ], sep = " & ")))
  
  cur.row = sample.space[omega, ]
  
  pomega = 1.
  
  for (i in 1:dim(sample.space)[2]){
    pomega = pomega*p.start[cur.row[i] + 1]
  }
  
  cat(sprintf(' & %.3f', pomega))
  
  cat(sprintf(' & %.2f', mean(cur.row)))
  
  sd.use = sqrt(var(cur.row)*((n-1)/n))
  
  cat(sprintf(' & %.2f', sd.use))
  
  cat(sprintf(' & %g', getmode(cur.row)))
  
  cat(sprintf(' \\\\\n'))
}

cat(sprintf("\\end{tabular}\n"))