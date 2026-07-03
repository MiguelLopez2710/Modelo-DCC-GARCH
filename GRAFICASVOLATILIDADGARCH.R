# ==============================================================================
# FASE 5.3: EXTRACCIÓN Y GRÁFICOS DE VOLATILIDAD CONDICIONAL
# ==============================================================================

# Extraemos la volatilidad (sigma) de los modelos GARCH(1,1) y la anualizamos
vol_oro <- sigma(fit_oro_garch) * sqrt(252)
vol_sp500 <- sigma(fit_sp_garch) * sqrt(252)
vol_nasdaq <- sigma(fit_nas_garch) * sqrt(252)

# 1. PDF Volatilidad Oro
pdf("volatilidad_condicional_oro.pdf", width = 10, height = 5)
plot(vol_oro, main = "Dinámica de la Volatilidad Condicional: ORO", 
     col = "darkgoldenrod", ylab = "Volatilidad Anualizada", xlab = "Fecha", lwd = 1.5)
dev.off()

# 2. PDF Volatilidad S&P 500
pdf("volatilidad_condicional_sp500.pdf", width = 10, height = 5)
plot(vol_sp500, main = "Dinámica de la Volatilidad Condicional: S&P 500", 
     col = "darkblue", ylab = "Volatilidad Anualizada", xlab = "Fecha", lwd = 1.5)
dev.off()

# 3. PDF Volatilidad NASDAQ
pdf("volatilidad_condicional_nasdaq.pdf", width = 10, height = 5)
plot(vol_nasdaq, main = "Dinámica de la Volatilidad Condicional: NASDAQ", 
     col = "darkred", ylab = "Volatilidad Anualizada", xlab = "Fecha", lwd = 1.5)
dev.off()

print("¡Gráficas de volatilidad generadas con éxito en tu carpeta!")