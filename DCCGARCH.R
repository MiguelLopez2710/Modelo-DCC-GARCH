library(rmgarch)

# 1. Definimos la especificación univariante (sGARCH 1,1)
spec_clasico <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)

# Creamos la unión de los 3 activos
especificaciones_multiples <- multispec(replicate(3, spec_clasico))

# 2. Configuración del DCC Clásico estándar 
especificacion_dcc <- dccspec(
  uspec = especificaciones_multiples, 
  dccOrder = c(1,1), 
  distribution = "mvnorm"
)

# 3. Estimar el modelo DCC definitivo
ajuste_dcc <- dccfit(especificacion_dcc, data = rentabilidades)

# 4. Mostrar los resultados
show(ajuste_dcc)