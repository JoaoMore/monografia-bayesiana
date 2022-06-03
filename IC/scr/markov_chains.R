library(markovchain)
library(dplyr)

# Criar função que analisa a convergencia de acordo com o numero de iterações ----

convergencia.markov <- function(n, markov.chain, cell, initial.state) {
  
  runs <- c(initial.state, rmarkovchain(n = n, object = markov.chain, t0 = initial.state))
  lag.runs <- lead(runs)
  
  size <- dim(transicao)[1]
  estados <- states(markov.chain)
  m1 <- matrix(0, size, size, dimnames = list(estados, estados))
  
  it <- numeric()
  phat <- numeric()
  
  for (k in 1:length(runs)) {
    m2 <- prop.table(table(runs[1:k], lag.runs[1:k]), margin = 1)
    
    i <- match(rownames(m2), rownames(m1))
    j <- match(colnames(m2), colnames(m1))
    
    m1[i,j] <- m2
    
    phat[k] <- m1[cell[1], cell[2]]
  }
  
  estimate <- tibble(it = 1:k, phat)
  iterations <- k-1
  transitions <- table(runs, lag.runs, dnn = c("", ""))
  
  value <- list(runs = runs, estimate = estimate, 
                iterations = iterations, transitions = transitions)
  
  invisible(value)
  
}

# Criação de um objeto da classe markovchain ----

estados <- letters[1:4] #os estados devem ser caracteres

transicao <- matrix(c(0.7, 0, 0.3, 0,
                      0.5, 0, 0.5, 0,
                      0,0.4,0,0.6, 
                      0,0.2,0,0.8), nrow = 4, byrow = T)

teste <- new('markovchain', 
             states = estados, 
             transitionMatrix = transicao,
             byrow = T)

# Teste ----

N <- 1000

markov <- convergencia.markov(n = N, 
                    markov.chain = teste, 
                    cell = c(1,1), #célula de interesse a se estimar da matriz de transição
                    initial.state = 'a')

plot(phat ~ it, data = markov$estimate, type = 'l', main = 'Cadeia de Markov')
abline(h = 0.7, col = 'red', lty = 2)

sample.teste <- sample(estados, N, prob = transicao[1,], replace = T)

sample.probs <- cumsum(sample.teste == 'a')/1:N

plot((1:N), sample.probs, type = 'l', main = 'Sample')
abline(h = 0.7, col = 'red', lty = 2)

