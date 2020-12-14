library(microbenchmark)
library(purrr)

Factorial Loop

Factorial_loop1 <- function (x) {
  result <- 1
  if (x == 0) return(1)
  for (i in 1:x) {
    result <- result * i
  }
  return(result)
}

Factorial_loop2 <- function(x) {
  if (x == 0 || x == 1)
    return(1)
  for (i in (x - 1):1) {
    x <- x * i
  }
  x
}


Factorial Reduce
Factorial_reduce1 <- function (x){
        if (x == 0) return(1)
        result <- reduce(as.numeric(1:x), `*`)
        return(result)
}

Factorial_reduce2 <- function(x) {
  if (x == 0)
    return(1)
  reduce(as.numeric(1:x), `*`)
}

Factorial Func
Factorial_func1 <- function (x) {
        if (x == 0) return(1)
        result <- x * Factorial_func1(x-1)
        return(result)
}

Factorial_func2 <- function(x) {
  if (x == 0)
    return(1)
  x * Factorial_func2(x - 1)
}

Factorial Memoization
Factorial_mem1 <- function(x) {
        if (x == 0) {
                return(1)
        } 
        fact_tbl[x] <<- x * Factorial_mem1(x-1)
        return(fact_tbl[x])
}
Factorial_mem2 <- function(x) {
  if (x == 0)
    return(1)
  if (!is.na(fact_tbl)[x])
    return(fact_tbl[x])
  fact_tbl[x] <<- x * Factorial_mem2(x - 1)
  fact_tbl[x]
}

fact_tbl <- c(rep(NA, 100))

Comparison
smp <- 100
factorial(100)

smp %>% 
lapply(function(x) {
    cat('Factorial_loop1   is', x %>% Factorial_loop1, '\n')
    cat('Factorial_reduce1 is', x %>% Factorial_reduce1, '\n')
    cat('Factorial_func1   is', x %>% Factorial_func1, '\n')
    cat('Factorial_mem1    is', x %>% Factorial_mem1, '\n')
    cat('Factorial_loop2   is', x %>% Factorial_loop2, '\n')
    cat('Factorial_reduce2 is', x %>% Factorial_reduce2, '\n')
    cat('Factorial_func2   is', x %>% Factorial_func2, '\n')
    cat('Factorial_mem2    is', x %>% Factorial_mem2, '\n')
    })
    
    
  all.equal(
  smp %>% Factorial_loop1,
  smp %>% Factorial_reduce1, 
  smp %>% Factorial_func1, 
  smp %>% Factorial_mem1, 
  smp %>% Factorial_loop2, 
  smp %>% Factorial_reduce2, 
  smp %>% Factorial_func2, 
  smp %>% Factorial_mem2)
  
  
  microbenchmark(
  smp %>% Factorial_loop1,
  smp %>% Factorial_reduce1, 
  smp %>% Factorial_func1, 
  smp %>% Factorial_mem1, 
  smp %>% Factorial_loop2, 
  smp %>% Factorial_reduce2, 
  smp %>% Factorial_func2, 
  smp %>% Factorial_mem2)
  
  
  ## fatorial 1000
fact_tbl <- c(rep(NA, 1000))
microbenchmark(
  1000 %>% Factorial_loop1,
  1000 %>% Factorial_reduce1, 
  1000 %>% Factorial_func1, 
  1000 %>% Factorial_mem1, 
  1000 %>% Factorial_loop2, 
  1000 %>% Factorial_reduce2, 
  1000 %>% Factorial_func2, 
  1000 %>% Factorial_mem2)
  
  
  