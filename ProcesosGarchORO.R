library(rugarch)

# 1. Definir las especificaciones de los modelos
spec_oro_garch  <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")
spec_oro_egarch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")
spec_oro_igarch <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")

# 2. Estimar los modelos con los datos del Oro 
fit_oro_garch  <- ugarchfit(spec = spec_oro_garch, data = rentabilidades[, "ORO"], solver = "hybrid")
fit_oro_egarch <- ugarchfit(spec = spec_oro_egarch, data = rentabilidades[, "ORO"], solver = "hybrid")
fit_oro_igarch <- ugarchfit(spec = spec_oro_igarch, data = rentabilidades[, "ORO"], solver = "hybrid")

# 3. Extraer y mostrar la tabla de criterios AIC
aic_oro <- data.frame(
  Modelo = c("GARCH(1,1) Estándar", "EGARCH(1,1) Asimétrico", "IGARCH(1,1) Integrado"),
  AIC = c(infocriteria(fit_oro_garch)[1], infocriteria(fit_oro_egarch)[1], infocriteria(fit_oro_igarch)[1])
)

print(aic_oro)