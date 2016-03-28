
nu <- function(df){
  
  a <- min(df$EXP)
  b <- max(df$EXP)
  p <- seq(a, b, length.out = 50)
  
  k <- length(p)
  r <- rep(0,k)
  for(i in 1:k){
    r[i] = sum((1 - pbeta(p[i]/df$EXP, df$a,df$b ))*df$Frecuencia)
  }
  
  expo <- p/b
  
  prob_exc <- 1 - exp(-r)
  
  per_ret <- 1/r
  
  prob_acum <- 1 - prob_exc
  
  dat_fr <- data.frame(p, expo, r, prob_exc, per_ret, prob_acum)
  
  names(dat_fr) <- c("Pérdida", "%Exposición", "OutPut", "Prob. Excedencia", "Periodo de Retorno", "Prob. Acum.")
  
  return(dat_fr)
}

sismo <- nu(val_dataframe[val_dataframe$Temporalidad == 1,])
hidro <- nu(val_dataframe[val_dataframe$Temporalidad != 1,])
total <- nu(val_dataframe)
