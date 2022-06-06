library(markovchain)
library(dplyr)

# Criar função que analisa a convergencia de acordo com o numero de iterações ----

convergencia.markov <- function(n, # número de interações
                                markov.chain, # objeto da classe markov chain
                                cell, # célula de interesse a se estimar (todas as probabilidades de transição são estimadas, no entanto, somente a especificada nesse argumento terá o seu valor por iteração retornado)
                                initial.state) {
  
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
  probability_matrix <- m1
  
  value <- list(runs = runs, estimate = estimate, 
                iterations = iterations, transitions = transitions,
                probability_matrix = probability_matrix)
  
  invisible(value)
  
}

#Função que amostra valores de uma posteriori com priori beta ou kumaraswamy
stan_markov <- function(priori, #priori escolhida (beta ou kumaraswamy)
                        markov.chain, #objeto markov.chain com as transições do processo
                        a, b, #hiperparâmetros da posteriori 
                        chains = 1, #número de cadeias a serem utilizadas
                        seed = Sys.time() #semente aleatória
                        ) {
  
  data.stan <- list(N = sum(markov.chain$transitions[1,]),
                    y = markov.chain$transitions[1,1],
                    a = a,
                    b = b)
  
  N = data.stan$N
  
  switch(priori,
         beta = stan(file = 'stan files/beta.stan',data = data.stan, 
                     chains = chains, seed = seed, iter = N*2),
         kumaraswamy = stan(file = 'stan files/kuma.stan', data = data.stan, 
                            chains = chains, seed = seed, iter = N*2)
  )
  
}