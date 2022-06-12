# Bibliotecas e funções ---------------------------------------------------

source('scr/cadeia_markov.R')
library(rstan)
options(mc.cores = parallel::detectCores())

# Matriz 3x3 --------------------------------------------------------------

## Caso 1 ----

estados <- letters[1:3] #os estados devem ser caracteres


transicao <- matrix(c(0.3, 0.2, 0.5,
                      0.35, 0.45, 0.2,
                      0.35, 0.25, 0.4), nrow = 3, byrow = T)

{caso1.3x3 <- new('markovchain', 
                  states = estados, 
                  transitionMatrix = transicao,
                  byrow = T)
  
  N <- 1000
  
  set.seed(1)
  
  caso1.3x3 <- convergencia.markov(n = N, 
                                   markov.chain = caso1.3x3, 
                                   cell = c(1,1),
                                   initial.state = 'a')}

a <- c(1,3,8)
b <- c(2,6,9)
prior <- c('beta', 'kumaraswamy')

df <- expand.grid(a = a, b = b, prior = prior)

caso1.3x3.df <- tibble(p = numeric(), 
                   priori = numeric(), 
                   alfa = numeric(), 
                   beta = numeric(),
                   media.priori = numeric(),
                   diff = numeric(),
                   media.post = numeric(),
                   se = numeric(), 
                   sd = numeric(),
                   q2.5 = numeric(),
                   q50 = numeric(),
                   q97.5 = numeric(),
                   rhat = numeric()) 


for (i in 1:18) {
  model <- stan_markov(priori = df$prior[i], 
                       markov.chain = caso1.3x3, 
                       a = df$a[i], b = df$b[i], chains = 1)
  
  t <- summary(model)
  
  name <- 
    paste('caso1.3x3', '.a', df$a[i], '.b', df$b[i], '.', df$prior[i], sep = '')
  
  
  temp <- tibble(p = caso1.3x3$chain@transitionMatrix[1,1], 
                 priori = df$prior[i], 
                 alfa = df$a[i], 
                 beta = df$b[i],
                 media.priori = (df$a[i]/(df$a[i] + df$b[i])),
                 diff = abs(media.priori-p),
                 media.post = t$summary[1,1],
                 se = t$summary[1,2], 
                 sd = t$summary[1,3],
                 q2.5 = t$summary[1,4],
                 q50 = t$summary[1,6],
                 q97.5 = t$summary[1,8],
                 rhat = t$summary[1,10]) 
  
  caso1.3x3.df <- rbind(caso1.3x3.df, temp)
  
  assign(name, model)
  
  print(i)
  
}

## Caso 2 ----
estados <- letters[1:3] #os estados devem ser caracteres

transicao <- matrix(c(0.1, 0.4, 0.5,
                      0.13, 0.49, 0.38,
                      0.10, 0.4, 0.5), nrow = 3, byrow = T)

{caso2.3x3 <- new('markovchain', 
                  states = estados, 
                  transitionMatrix = transicao,
                  byrow = T)
  
  N <- 1000
  
  set.seed(1)
  
  caso2.3x3 <- convergencia.markov(n = N, 
                                   markov.chain = caso2.3x3, 
                                   cell = c(1,1),
                                   initial.state = 'a')}

a <- c(1,3,8)
b <- c(2,6,9)
prior <- c('beta', 'kumaraswamy')

df <- expand.grid(a = a, b = b, prior = prior)

caso2.3x3.df <- tibble(p = numeric(), 
                       priori = numeric(), 
                       alfa = numeric(), 
                       beta = numeric(),
                       media.priori = numeric(),
                       diff = numeric(),
                       media.post = numeric(),
                       se = numeric(), 
                       sd = numeric(),
                       q2.5 = numeric(),
                       q50 = numeric(),
                       q97.5 = numeric(),
                       rhat = numeric()) 


for (i in 1:18) {
  model <- stan_markov(priori = df$prior[i], 
                       markov.chain = caso1.3x3, 
                       a = df$a[i], b = df$b[i], chains = 1)
  
  t <- summary(model)
  
  name <- 
    paste('caso1.3x3', '.a', df$a[i], '.b', df$b[i], '.', df$prior[i], sep = '')
  
  
  temp <- tibble(p = caso1.3x3$chain@transitionMatrix[1,1], 
                 priori = df$prior[i], 
                 alfa = df$a[i], 
                 beta = df$b[i],
                 media.priori = (df$a[i]/(df$a[i] + df$b[i])),
                 diff = abs(media.priori-p),
                 media.post = t$summary[1,1],
                 se = t$summary[1,2], 
                 sd = t$summary[1,3],
                 q2.5 = t$summary[1,4],
                 q50 = t$summary[1,6],
                 q97.5 = t$summary[1,8],
                 rhat = t$summary[1,10]) 
  
  caso2.3x3.df <- rbind(caso2.3x3.df, temp)
  
  assign(name, model)
  
  print(i)
  
}
## Caso 3 ----
estados <- letters[1:3] #os estados devem ser caracteres

transicao <- matrix(c(0.1, 0.4, 0.5,
                      0.13, 0.55, 0.32,
                      0.10, 0.35, 0.55), nrow = 3, byrow = T)

{caso3.3x3 <- new('markovchain', 
                  states = estados, 
                  transitionMatrix = transicao,
                  byrow = T)
  
  N <- 1000
  
  set.seed(1)
  
  caso3.3x3 <- convergencia.markov(n = N, 
                                   markov.chain = caso3.3x3, 
                                   cell = c(1,1),
                                   initial.state = 'a')}

a <- c(1,3,8)
b <- c(2,6,9)
prior <- c('beta', 'kumaraswamy')

df <- expand.grid(a = a, b = b, prior = prior)

caso3.3x3.df <- tibble(p = numeric(), 
                       priori = numeric(), 
                       alfa = numeric(), 
                       beta = numeric(),
                       media.priori = numeric(),
                       diff = numeric(),
                       media.post = numeric(),
                       se = numeric(), 
                       sd = numeric(),
                       q2.5 = numeric(),
                       q50 = numeric(),
                       q97.5 = numeric(),
                       rhat = numeric()) 


for (i in 1:18) {
  model <- stan_markov(priori = df$prior[i], 
                       markov.chain = caso1.3x3, 
                       a = df$a[i], b = df$b[i], chains = 1)
  
  t <- summary(model)
  
  name <- 
    paste('caso1.3x3', '.a', df$a[i], '.b', df$b[i], '.', df$prior[i], sep = '')
  
  
  temp <- tibble(p = caso1.3x3$chain@transitionMatrix[1,1], 
                 priori = df$prior[i], 
                 alfa = df$a[i], 
                 beta = df$b[i],
                 media.priori = (df$a[i]/(df$a[i] + df$b[i])),
                 diff = abs(media.priori-p),
                 media.post = t$summary[1,1],
                 se = t$summary[1,2], 
                 sd = t$summary[1,3],
                 q2.5 = t$summary[1,4],
                 q50 = t$summary[1,6],
                 q97.5 = t$summary[1,8],
                 rhat = t$summary[1,10]) 
  
  caso3.3x3.df <- rbind(caso3.3x3.df, temp)
  
  assign(name, model)
  
  print(i)
  
}
# Salvando os modelos ----

rm(a,b,estados,i,N,name,prior, df, model, transicao, temp, t)

save.image(file = 'out/caso3x3.RData')