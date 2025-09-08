
# Reshma Kassanjee. Last updated 8 September 2025
# Infectious disease modelling
# SIRS with demography and vaccination at birth
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

inmin_vaxstart <- 0
inmax_vaxstart <- 20
indefault_vaxstart <- 0
instep_vaxstart <- 0.5

inmin_vaxp <- 0
inmax_vaxp <- 100
indefault_vaxp <- 0
instep_vaxp <- 1


timevec <- seq(0, in_maxyears*365.25, by = 1) 


## ODEs

sirsvax_demography <- function(t,y,parms){
  with(c(as.list(y),parms),{
    
    N <- S + I + R + V
    
    infections    <- beta*S*I/N
    recoveries    <- gamma*I
    waning        <- alpha*R
    births        <- mu*N*c(S = 1-pv*(t>=tau), V = pv*(t>=tau))
    deaths        <- mu*y
    vaccinations  <- births[['V']]
    
    dSdt <- births[['S']] - infections                + waning  - deaths[['S']]
    dIdt <-               + infections  - recoveries            - deaths[['I']]
    dRdt <-                             + recoveries  - waning  - deaths[['R']]
    dVdt <- births[['V']]                                       - deaths[['V']]
    
    return(list(c(dSdt,dIdt,dRdt,dVdt,vaccinations,infections)))
    
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
                              , in_indvax
                              , in_vaxstart
                              , in_vaxp
                              , in_maxyears){
  
  # in days
  
  # browser()
  
  ## CONSTRUCTED INPUTS
  
  alpha_constructed     <- (in_indwaning)*(1/(in_durimm*365.25))
  mu_constructed        <- (in_indbirth)*(1/(in_durlife*365.25))
  vaxstart_contructed   <- in_indvax*in_vaxstart*365.25
  vaxp_constructed      <- in_indvax*in_vaxp/100
  
  parms0 <- list( beta      = in_beta
                , gamma     = 1/in_durinf
                , alpha     = alpha_constructed
                , mu        = mu_constructed
                , tau       = vaxstart_contructed
                , pv        = vaxp_constructed
  )
  
  pop0 <- c(  S   = in_N - in_I0
            , I   = in_I0
            , R   = 0
            , V   = 0
            , cV  = 0
            , cI  = 0) 
  
  ## ODEs solving 
  
  dd <- data.frame(lsoda(
    y = pop0,
    times = timevec,
    func = sirsvax_demography,
    parms = parms0
  ))
  
  ## RETURN
  
  return(dd)
  
}




