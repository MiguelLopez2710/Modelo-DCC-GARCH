set.seed(123)
y <- arima.sim(model = list(ar = 0.7, ma = 0.4), n = 250)

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
# Reducimos el tercer número (margen superior) de 3 a 1 al no tener títulos
par(mar = c(4, 4, 1, 1))

# --- GRÁFICO 1: TRAYECTORIA ---
plot(y, type = "l", col = "steelblue", lwd = 1.5,
     main = "", ylab = "Valor", xlab = "Tiempo")
abline(h = 0, col = "red", lty = 2)

# --- GRÁFICO 2: CORRELOGRAMA FAS ---
acf(y, main = "", col = "darkblue", lwd = 2)

# --- GRÁFICO 3: CORRELOGRAMA FAP ---
pacf(y, main = "", col = "darkblue", lwd = 2)

par(mfrow = c(1, 1))
