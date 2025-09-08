
# Reshma Kassanjee. Last updated 20 August 2025
# Infectious disease modelling
# SIRS with demography
# Learning tool for UCT MPH


## INPUTS + constructions

in_maxyears <- 20
in_N <- 1000

in_basetext <- 16

inmin_I0 <- 0
inmax_I0 <- 1000
indefault_I0 <- 1
instep_I0 <- 1

inmin_beta <- 0
inmax_beta <- 10
indefault_beta <- 0.2
instep_beta <- 0.05

inmin_durinf <- 1
inmax_durinf <- 100
indefault_durinf <- 10
instep_durinf <- 1

inmin_durimm <- 0.5
inmax_durimm <- 50
indefault_durimm <- 2
instep_durimm <- 0.5

inmin_durlife <- 0.1
inmax_durlife <- 50
indefault_durlife <- 10
instep_durlife <- 0.1

timevec <- seq(0, in_maxyears*365.25, by = 1) 


## ODEs

sirs_demography <- function(t,y,parms){
  with(c(as.list(y),parms),{
    
    N <- S + I + R
    
    infections  <- beta*S*I/N
    recoveries  <- gamma*I
    waning      <- alpha*R
    births      <- mu*N
    deaths      <- mu*y
    
    dSdt <- births - infections              + waning - deaths[['S']]
    dIdt <-        + infections - recoveries          - deaths[['I']]
    dRdt <-                     + recoveries - waning - deaths[['R']]
    
    return(list(c(dSdt,dIdt,dRdt,infections)))
    
  })
}


## FUNCTION TO SIMULATE TRAJECTORY

simulate_epidemic <- function(in_I0
                              , in_beta
                              , in_durinf
                              , in_indwaning
                              , in_durimm
                              , in_indbirth
                              , in_durlife
                              , in_maxyears){
  
  # in days
  
  ## CONSTRUCTED INPUTS
  
  alpha_constructed <- (in_indwaning)*(1/(in_durimm*365.25))
  mu_constructed    <- (in_indbirth)*(1/(in_durlife*365.25))
  
  parms0 <- list(beta = in_beta
                , gamma = 1/in_durinf
                , alpha = alpha_constructed
                , mu = mu_constructed
  )
  
  pop0 <- c(S = in_N - in_I0
            , I = in_I0
            , R = 0
            , infections = 0) 
  
  ## ODEs solving 
  
  dd <- data.frame(lsoda(
    y = pop0,
    times = timevec,
    func = sirs_demography,
    parms = parms0
  ))
  
  ## RETURN
  
  return(dd)
  
}




