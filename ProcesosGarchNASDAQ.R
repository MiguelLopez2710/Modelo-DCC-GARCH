# 1. Definir las especificaciones de los modelos 
spec_nas_garch  <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")
spec_nas_egarch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")
spec_nas_igarch <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")

# 2. Estimar los modelos con los datos del NASDAQ 
fit_nas_garch  <- ugarchfit(spec = spec_nas_garch, data = rentabilidades[, "NASDAQ"], solver = "hybrid")
fit_nas_egarch <- ugarchfit(spec = spec_nas_egarch, data = rentabilidades[, "NASDAQ"], solver = "hybrid")
fit_nas_igarch <- ugarchfit(spec = spec_nas_igarch, data = rentabilidades[, "NASDAQ"], solver = "hybrid")

# 3. Extraer y mostrar la tabla de criterios AIC
aic_nas <- data.frame(
  Modelo = c("GARCH(1,1) Estándar", "EGARCH(1,1) Asimétrico", "IGARCH(1,1) Integrado"),
  AIC = c(infocriteria(fit_nas_garch)[1], infocriteria(fit_nas_egarch)[1], infocriteria(fit_nas_igarch)[1])
)

print(aic_nas)