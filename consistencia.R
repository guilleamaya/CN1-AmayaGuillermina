mu <- 5  # Media
sigma <- 1  # Desviación estándar


tamaños_muestras <- c(500, 1000, 1500, 2000)
cantidad_muestras <- 500
entropia_poblacion <- (1/2)* (log((sigma^2)* (2* pi*exp(1))))

estimador_entropia_muestras <- matrix(0, nrow = cantidad_muestras, ncol = length(tamaños_muestras))
esperanzas <- numeric(length(tamaños_muestras))


for (j in 1:length(tamaños_muestras)){
  n <- tamaños_muestras[j]
  
  for (i in 1:cantidad_muestras){
    muestra <- rnorm(n, mean = mu, sd = sigma)
    
      histograma <- hist(muestra, plot = FALSE)
      densidad <- histograma$density 
      densidad[densidad == 0] <- 1e-10
      estimador_entropia_muestra <- -sum(densidad * log(densidad)) 
      estimador_entropia_muestras[i, j] <- estimador_entropia_muestra
  } 
  esperanzas[j] <- mean(estimador_entropia_muestras[, j])
}

print(esperanzas)

plot(tamaños_muestras, esperanzas, type = "b", col = "blue", 
     xlab = "Tamaño de muestra", ylab = "Esperanza de la entropía",
     main = "Esperanza de la entropía en función del tamaño de muestra",
     pch = 19)
