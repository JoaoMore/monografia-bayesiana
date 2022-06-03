source('scr/cadeia_markov.R')
library(rstan)
options(mc.cores = parallel::detectCores())

# Caso 1 - Probabilidades próximas de 0.5 ---------------------------------

## Inferência clássica -----------------------------------------------------

### Criação de um objeto da classe markovchain ----

estados <- letters[1:2] #os estados devem ser caracteres

p <- 0.45
transicao <- matrix(c(0.45, 0.55,
                      0.57, 0.43), nrow = 2, byrow = T)

caso1 <- new('markovchain', 
             states = estados, 
             transitionMatrix = transicao,
             byrow = T)

### Run ----

N <- 1000

set.seed(1)
markov <- convergencia.markov(n = N, 
                              markov.chain = caso1, 
                              cell = c(1,1), #célula de interesse a se estimar da matriz de transição
                              initial.state = 'a')

plot(phat ~ it, data = markov$estimate, type = 'l', main = 'Inferência clássica da probabilidade de transição', ylim = c(0,1))
abline(h = p, col = 'red', lty = 2) #valor real da probabilidade 


## Inferência Bayesiana ----
### Binomial-beta ----
set.seed(1)
(data.stan <- list(N = 50,
                   y = rbinom(50, 50, p),
                   a = 2, 
                   b = 3))

caso1.2x2.stan.beta <- stan(file = 'stan files/beta.stan', 
                   data = data.stan, chains = 4, seed = 1, iter = N*4)


df1 <- tibble(caso1.2x2.stan.beta %>% as.data.frame()) %>% 
  mutate(pm = cumsum(p)/1:N)

plot(1:N, df1$pm, type = 'l', ylim = c(p-.2,p+.2),
     main = 'Inferência Bayesiana da probabilidade de transição')
abline(h = p, col = 'red', lty = 2)

### Kumaraswamy ----

set.seed(1)
(data.stan <- list(N = 50,
                   y = rbinom(50,50, p),
                   a = 2, 
                   b = 3))

caso1.2x2.stan.kuma <- stan(file = 'stan files/kuma.stan', 
                   data = data.stan, chains = 1, seed = 1, iter = N*2)


df1 <- tibble(caso1.2x2.stan.kuma %>% as.data.frame()) %>% 
  mutate(pm = cumsum(p)/1:N)

plot(1:N, df1$pm, type = 'l', ylim = c(0.4,.5),
     main = 'Inferência Bayesiana da probabilidade de transição')
abline(h = .45, col = 'red', lty = 2)


# Caso 2 - Probabilidades extremas na primeira linha ---------------------------------

## Inferência clássica -----------------------------------------------------

### Criação de um objeto da classe markovchain ----

estados <- letters[1:2] #os estados devem ser caracteres

transicao <- matrix(c(0.1, 0.9,
                      0.57, 0.43), nrow = 2, byrow = T)

caso2 <- new('markovchain', 
             states = estados, 
             transitionMatrix = transicao,
             byrow = T)

### Run ----

N <- 1000

set.seed(1)
markov <- convergencia.markov(n = N, 
                              markov.chain = caso2, 
                              cell = c(1,1), #célula de interesse a se estimar da matriz de transição
                              initial.state = 'a')

plot(phat ~ it, data = markov$estimate, type = 'l', main = 'Inferência clássica da probabilidade de transição', ylim = c(0,1))
abline(h = 0.1, col = 'red', lty = 2) #valor real da probabilidade 


# Inferência Bayesiana ----
### Binomial-beta ----
set.seed(1)
(data.stan <- list(N = 50,
                   y = rbinom(50, 50, p),
                   a = 2, 
                   b = 3))

caso2.2x2.stan.beta <- stan(file = 'stan files/beta.stan', 
                            data = data.stan, chains = 4, seed = 1, iter = N*4)


df1 <- tibble(caso2.2x2.stan.beta %>% as.data.frame()) %>% 
  mutate(pm = cumsum(p)/1:N)

plot(1:N, df1$pm, type = 'l', ylim = c(p-.2,p+.2),
     main = 'Inferência Bayesiana da probabilidade de transição')
abline(h = p, col = 'red', lty = 2)

### Kumaraswamy ----

set.seed(1)
(data.stan <- list(N = 50,
                   y = rbinom(50,50, p),
                   a = 2, 
                   b = 3))

caso2.2x2.stan.kuma <- stan(file = 'stan files/kuma.stan', 
                            data = data.stan, chains = 1, seed = 1, iter = N*2)


df1 <- tibble(caso2.2x2.stan.kuma %>% as.data.frame()) %>% 
  mutate(pm = cumsum(p)/1:N)

plot(1:N, df1$pm, type = 'l', ylim = c(0.4,.5),
     main = 'Inferência Bayesiana da probabilidade de transição')
abline(h = .45, col = 'red', lty = 2)
# Caso 3 - Probabilidades extremas nas duas linhas ---------------------------------

## Inferência clássica -----------------------------------------------------

### Criação de um objeto da classe markovchain ----

estados <- letters[1:2] #os estados devem ser caracteres

transicao <- matrix(c(0.15, 0.85,
                      0.10, 0.90), nrow = 2, byrow = T)

caso3 <- new('markovchain', 
             states = estados, 
             transitionMatrix = transicao,
             byrow = T)

### Run ----

N <- 1000

set.seed(1)
markov <- convergencia.markov(n = N, 
                              markov.chain = caso3, 
                              cell = c(1,1), #célula de interesse a se estimar da matriz de transição
                              initial.state = 'a')

plot(phat ~ it, data = markov$estimate, type = 'l', main = 'Inferência clássica da probabilidade de transição', ylim = c(0,1))
abline(h = .1, col = 'red', lty = 2) #valor real da probabilidade 


# Inferência Bayesiana ----

### Binomial-beta ----
set.seed(1)
(data.stan <- list(N = 50,
                   y = rbinom(50, 50, p),
                   a = 2, 
                   b = 3))

caso3.2x2.stan.beta <- stan(file = 'stan files/beta.stan', 
                            data = data.stan, chains = 4, seed = 1, iter = N*4)


df1 <- tibble(caso3.2x2.stan.beta %>% as.data.frame()) %>% 
  mutate(pm = cumsum(p)/1:N)

plot(1:N, df1$pm, type = 'l', ylim = c(p-.2,p+.2),
     main = 'Inferência Bayesiana da probabilidade de transição')
abline(h = p, col = 'red', lty = 2)

### Kumaraswamy ----

set.seed(1)
(data.stan <- list(N = 50,
                   y = rbinom(50,50, p),
                   a = 2, 
                   b = 3))

caso1.2x2.stan.kuma <- stan(file = 'stan files/kuma.stan', 
                            data = data.stan, chains = 1, seed = 1, iter = N*2)


df1 <- tibble(caso1.2x2.stan.kuma %>% as.data.frame()) %>% 
  mutate(pm = cumsum(p)/1:N)

plot(1:N, df1$pm, type = 'l', ylim = c(0.4,.5),
     main = 'Inferência Bayesiana da probabilidade de transição')
abline(h = .45, col = 'red', lty = 2)
