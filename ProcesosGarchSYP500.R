# 1. Definir las especificaciones de los modelos
spec_sp_garch  <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")
spec_sp_egarch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")
spec_sp_igarch <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")

# 2. Estimar los modelos con los datos del S&P 500 
fit_sp_garch  <- ugarchfit(spec = spec_sp_garch, data = rentabilidades[, "SP500"], solver = "hybrid")
fit_sp_egarch <- ugarchfit(spec = spec_sp_egarch, data = rentabilidades[, "SP500"], solver = "hybrid")
fit_sp_igarch <- ugarchfit(spec = spec_sp_igarch, data = rentabilidades[, "SP500"], solver = "hybrid")

# 3. Extraer y mostrar la tabla de criterios AIC
aic_sp <- data.frame(
  Modelo = c("GARCH(1,1) Estándar", "EGARCH(1,1) Asimétrico", "IGARCH(1,1) Integrado"),
  AIC = c(infocriteria(fit_sp_garch)[1], infocriteria(fit_sp_egarch)[1], infocriteria(fit_sp_igarch)[1])
)

print(aic_sp)