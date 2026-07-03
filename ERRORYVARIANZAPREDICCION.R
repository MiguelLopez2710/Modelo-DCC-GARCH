
phi <- 0.7       
theta <- 0.5     
sigma2 <- 1      
max_k <- 15      

# Inicializamos los vectores vacíos
var_ar1 <- numeric(max_k)
var_ma1 <- numeric(max_k)
var_arma11 <- numeric(max_k)

err_ar1 <- numeric(max_k)
err_ma1 <- numeric(max_k)
err_arma11 <- numeric(max_k)

# Generamos las perturbaciones aleatorias
set.seed(123) 
eps <- rnorm(max_k, mean = 0, sd = sqrt(sigma2))


for (i in 1:max_k) {
  
  # --- A) CÁLCULO DE VARIANZAS ---
  var_ar1[i] <- sigma2 * (1 - phi^(2*i)) / (1 - phi^2)
  
  if (i == 1) {
    var_ma1[i] <- sigma2
    var_arma11[i] <- sigma2
  } else {
    var_ma1[i] <- sigma2 * (1 + theta^2)
    var_arma11[i] <- sigma2 * (1 + ((phi - theta)^2) * (1 - phi^(2*(i - 1))) / (1 - phi^2))
  }
  

  # Error AR(1)
  suma_ar <- 0
  for(j in 0:(i-1)) {
    suma_ar <- suma_ar + (phi^j) * eps[i-j]
  }
  err_ar1[i] <- suma_ar
  
  # Error MA(1)
  if (i == 1) {
    err_ma1[i] <- eps[1]
  } else {
    err_ma1[i] <- eps[i] - theta * eps[i-1]
  }
  
  # Error ARMA(1,1)
  if (i == 1) {
    err_arma11[i] <- eps[1]
  } else {
    suma_arma <- eps[i]
    for(j in 1:(i-1)) {
      suma_arma <- suma_arma + (phi^(j-1) * (phi - theta)) * eps[i-j]
    }
    err_arma11[i] <- suma_arma
  }
}


cat("\n--- TABLA COMPARATIVA DE VARIANZAS TEÓRICAS ---\n")
tabla_varianzas <- data.frame(
  Horizonte_k = 1:max_k,
  AR1 = round(var_ar1, 4),
  MA1 = round(var_ma1, 4),
  ARMA11 = round(var_arma11, 4)
)
print(tabla_varianzas)


k <- 1:max_k

# Gráfica 1 Varianza Teórica
max_y_var <- max(c(var_ar1, var_ma1, var_arma11), na.rm = TRUE) + 0.5
plot(k, var_ar1, type = "o", pch = 16, col = "blue", lwd = 2,
     ylim = c(0, max_y_var), xlab = "Horizonte (k)", ylab = "Varianza",
     xaxt="n")
axis(1, at = 1:max_k)
lines(k, var_ma1, type = "o", pch = 17, col = "red", lwd = 2, lty = 2)
lines(k, var_arma11, type = "o", pch = 15, col = "forestgreen", lwd = 2, lty = 3)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
legend("bottomright", legend = c("AR(1)", "MA(1)", "ARMA(1,1)"),
       col = c("blue", "red", "forestgreen"), pch = c(16, 17, 15), 
       lty = c(1, 2, 3), lwd = 2, bty = "n", cex = 0.8)

#Gráfica 2 con Errores Simulados
min_y_err <- min(c(err_ar1, err_ma1, err_arma11), na.rm = TRUE) - 1
max_y_err <- max(c(err_ar1, err_ma1, err_arma11), na.rm = TRUE) + 1
plot(k, err_ar1, type = "o", pch = 16, col = "blue", lwd = 2,
     ylim = c(min_y_err, max_y_err), xlab = "Horizonte (k)", ylab = "Valor del Error",
     xaxt="n")
axis(1, at = 1:max_k)
abline(h = 0, col = "black", lwd = 2) 
lines(k, err_ma1, type = "o", pch = 17, col = "red", lwd = 2, lty = 2)
lines(k, err_arma11, type = "o", pch = 15, col = "forestgreen", lwd = 2, lty = 3)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
legend("bottomright", legend = c("AR(1)", "MA(1)", "ARMA(1,1)"),
       col = c("blue", "red", "forestgreen"), pch = c(16, 17, 15), 
       lty = c(1, 2, 3), lwd = 2, bty = "n", cex = 0.8)