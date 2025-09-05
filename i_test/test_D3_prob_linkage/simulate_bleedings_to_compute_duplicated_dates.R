# provo a generare le date di bleeding, immaginando 1300 casi su 2500 giorni
library(data.table)

#### Poisson

set.seed(123)

n_days <- 2500        
lambda <- 0.5 # so che si osservano circa 1300 eventi su un periodo di 7 anni (circa 2500 giorni)

# Simulo il numero di eventi per ciascun giorno
events_per_day <- rpois(n_days, lambda)

# Costruisco le date degli eventi
dates <- as.data.table(rep(1:n_days, times = events_per_day))

dates[, uniqueN(V1)]

# probabilità teorica
ppois(1, 0.5, lower.tail = F)
