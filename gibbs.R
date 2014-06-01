# Generate a national election cycle using the model presented in strausswalks

# Each poll reports Democratic national two-party vote as 
# y_i ~ N(\mu_i, s^2_i)
# where s^2_i is the variance based on q_i random respondents
# s^2_i = y_i*(1 - y_i) / q_i
# And house polling bias is taken into account by
# \mu_i = \alpha_{t_i} + \delta_{j_i}
# where \alpha_t is the true national preference at t days until the election, and \delta_j is the house bias.
# House biases must add up to 0.
# National preference is modeled as a random walk backwards through time, \alpha_t ~ N(\alpha_{t-1}, \omega^2)

pollVar <- function(y) {
  return(y * (1-y) / 2000)
}

GenData <- function() {
  T = 100
  # The national preference at \alpha_0 is 0.48
  # Shock is 0.01
  # So, starting at \alpha_0, generate preferences at each day according to shock.
  pref <- c()
  pref[1] = 0.48
  shock = 0.007
  
  set.seed(217) # Looks realistic!
  for (i in 2:T) {
    pref[i] <- rnorm(1, pref[i-1], shock)
  }

  # Two polling firms, Giddyup Poll and Rasputin Reporting.
  # Giddyup's bias is 0.03, Rasputin's is -0.03
  # Giddyup has a poll on 2, 4, 6, 8, 10, Rasputin has a poll on 3, 5, 7, 9
  giddyup_bias = 0.03
  giddyup <- rep(NA, T)
  for (i in seq(2, T, 5)) {
    biased_pref <- pref[i] + giddyup_bias
    giddyup[i] <- rnorm(1, biased_pref, pollVar(biased_pref))
  }
  rasputin_bias = -0.03
  rasputin <- rep(NA, T)
  for (i in seq(3, T, 5)) {
    biased_pref <- pref[i] + rasputin_bias
    rasputin[i] <- rnorm(1, biased_pref, pollVar(biased_pref))
  }
  
  
  
  
  for (t in 1:T) {
    if (!is.na(giddyup[t]) && !is.na(rasputin[t])) {
      stop("Time period ", t, " has more than one poll!")
    }
  }
  
  return(list(pref=pref, giddyup=giddyup, rasputin=rasputin))
}

data <- GenData()

# Great! Let's do a Gibbs sampler
runs <- 100
# Prior on \alpha_0 ~ N(0.522, 0.0253^2)
pref <- matrix(ncol=length(data$pref), nrow=runs)
pref[1,] = rep(0.522, length(data$pref))
biases <- list(giddyup=c(0), rasputin=c(0))
# biases <- list(giddyup=rep(0.03, runs), rasputin=rep(-0.03, runs))
shock <- c(0.00005) # omega^2
# shock <- rep(0.007, runs)

# So we start with 10 alphas, 2 houses biases, and the shock.
# For each iteration,
for (i in 2:runs) {
  # First alpha. There is no \alpha_{-1}!
  mean <- pref[i-1, 2]
  variance <- shock[i-1]
  pref[i, 1] <- rnorm(1, mean, sqrt(variance))
  
  # First for national preferences
  for (t in 2:(runs-1)) {
    poll <- NA
    if (!is.na(data$giddyup[t])) {
      poll <- data$giddyup[t]
      bias <- biases$giddyup[i-1]
    } else if (!is.na(data$rasputin[t])) {
      poll <- data$rasputin[t]
      bias <- biases$rasputin[i-1]
    }
    
    if (is.na(poll)) {
      message("no poll")
      mean <- ((pref[i, t-1] + pref[i-1, t+1]) / shock[i-1]) * (2 / shock[i-1])^(-1)
      variance <- (2 / shock[i-1])^(-1)
    } else {
      mean <- ((poll - bias) / pollVar(poll) + (pref[i, t-1] + pref[i-1, t+1]) / shock[i-1]) * (1 / pollVar(poll) + 2 / shock[i-1])^(-1)
      variance <- 1 / (1 / pollVar(poll) + 2 / shock[i-1])
    }
    pref[i, t] <- rnorm(1, mean, sqrt(variance))
  }
  
  poll <- NA
  if (!is.na(data$giddyup[runs])) {
    poll <- data$giddyup[runs]
    bias <- biases$giddyup[i-1]
  } else if (!is.na(data$rasputin[runs])) {
    poll <- data$rasputin[runs]
    bias <- biases$rasputin[i-1]
  }
  
  if (is.na(poll)) {
    mean <- pref[i, runs-1]
    variance <- shock[i-1]
  } else {
    mean <- ((poll - bias) / pollVar(poll) + pref[i, runs-1] / shock[i-1]) / (1 / pollVar(poll) + 1 / shock[i-1])
    variance <- 1 / (1 / pollVar(poll) + 1 / shock[i-1])
  }
  pref[i, runs] <- rnorm(1, mean, sqrt(variance))
  
  # Now for the biases
  mean <- sum((data$giddyup - pref[i,]) / pollVar(data$giddyup), na.rm = TRUE) / (sum(1 / pollVar(data$giddyup), na.rm=TRUE) + 100)
  variance <- 1 / (sum(1 / pollVar(data$giddyup), na.rm=TRUE) + 100)
  biases$giddyup[i] <- rnorm(1, mean, variance)
  biases$rasputin[i] <- -biases$giddyup[i]
  
  # And the shock
  shape <- (runs - 2) / 2
  beta <- sum((c(pref[i,], NA) - c(NA, pref[i,]))^2, na.rm = TRUE) / 2
  print(shape)
  print(beta)
  shock[i] <- 1 / rgamma(1, shape, beta)
  print(c("iter", i))
  while (1 / shock[i] <= 10000) {
    shock[i] <- 1 / rgamma(1, shape, beta)
  }
}

results <- list(true=data$pref, giddyup=data$giddyup, rasputin=data$rasputin, gibbs=pref)
dput(results, "strausswalk.data")

d <- dget('strausswalk.data')
plot(d$true, type = "l", xlim = c(T, 1), ylim = c(0.4, 0.6), lwd = 2)
points(d$giddyup, pch = 3)
points(d$rasputin, pch = 1)
pref <- d$gibbs
lines(seq(1, 100), colMeans(pref))
lines(seq(1, 100), apply(pref, 2, mean) + (apply(pref, 2, sd) * qnorm(0.975)))
lines(seq(1, 100), apply(pref, 2, mean) - (apply(pref, 2, sd) * qnorm(0.975)))