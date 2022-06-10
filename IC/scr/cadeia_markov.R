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
                probability_matrix = probability_matrix,
                chain = markov.chain)
  
  invisible(value)
  
}

# Função que amostra valores de uma posteriori com priori beta ou kumaraswamy
stan_markov <- function(priori, #priori escolhida (beta ou kumaraswamy)
                        markov.chain, #objeto markov.chain com as transições do processo
                        a, b, #hiperparâmetros da posteriori 
                        chains = 1, #número de cadeias a serem utilizadas
                        seed = sample.int(.Machine$integer.max, 1) #semente aleatória
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

# Função que compara a convergência dos modelos clássico e bayesiano
model.diag <- function(stan.model, #modelo stan
                       markov.model #modelo clássico 
) {
  
  prob <- markov.model$chain@transitionMatrix[1,1]
  i <- sum(markov.model$transitions[1,])
  p <- stan.model@sim[["samples"]][[1]][["p"]][(i+1):(i*2)]
  
  df <- tibble(media = (cumsum(p)/(1:i)), i = (1:i))
  
  par(mfrow = c(1,2))
  
  {plot(phat ~ it, data = markov.model$estimate, type = 'l',  ylim = c(prob-.1,prob+.1),
        main = 'Método Clássico', 
        xlab = 'Iterações', ylab = 'Estimativa')
    abline(h = prob, col = 'red', lty = 2)}
  
  {plot(media ~ i, data = df, type = 'l', ylim = c(prob-.1, prob+.1), 
        main = 'Método Bayesiano', 
        xlab = 'Iterações', ylab = 'Estimativa')
    abline(h = prob, col = 'red', lty = 2)}
  
}