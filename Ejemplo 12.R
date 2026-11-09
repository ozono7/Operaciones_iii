# ==========================================================================
# Extracción Líquido-Líquido en la Industria Alimentaria
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Extracción de antioxidantes a partir de cáscaras de cítricos
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
if (!require(ggtern)) install.packages("ggtern", dependencies = TRUE)  # Para diagramas ternarios  
library(ggplot2)  
library(ggtern)  

# Datos del problema  
composicion_inicial <- c(Agua = 0.60, Solidos = 0.30, Antioxidantes = 0.10)  # Fracciones másicas  
K <- 3.5  # Coeficiente de distribución K = y / x (fracción másica de antioxidantes en etanol / fracción másica en agua), adimensional  
etapas <- 2  # Número de etapas de extracción  
# Masa de etanol fresco por etapa: el archivo no indicaba la masa de
# disolvente y el equilibrio riguroso la necesita. Se supone 1 kg de etanol
# por kg de alimentación en cada etapa (disolvente fresco en cada etapa,
# corriente cruzada, según Treybal, Mass-Transfer Operations, 3.ª ed.,
# cap. 10). Los resultados dependen de esta relación disolvente/alimentación.
masa_etanol_por_etapa <- 1  # kg de etanol por kg de alimentación y por etapa

# Equilibrio de extracción en una etapa con disolventes inmiscibles
# (Treybal, Mass-Transfer Operations, 3.ª ed., cap. 10): el soluto se
# reparte entre las dos fases líquidas según K = y / x, donde x e y son
# las fracciones másicas de soluto en el refinado líquido (fase acuosa) y
# en el extracto líquido (fase de etanol). Con una base de 1 kg de
# alimentación, si a es la masa de soluto que queda en el refinado:
#   x = a / (I_R + a),  y = (A - a) / (I_E + A - a),
# donde A es la masa total de soluto, I_R la masa de agua del refinado e
# I_E la masa de etanol del extracto. La condición y = K * x da una
# ecuación cuadrática para a; se toma la raíz menor que A (la otra es mayor
# que la masa total de soluto y no tiene sentido físico).
reparto_equilibrio <- function(A, I_R, I_E, K) {
  coef2 <- K - 1
  coef1 <- A - I_R - K * I_E - K * A
  coef0 <- A * I_R
  discriminante <- coef1^2 - 4 * coef2 * coef0
  a <- (-coef1 - sqrt(discriminante)) / (2 * coef2)  # soluto en el refinado (kg)
  x <- a / (I_R + a)  # fracción másica de soluto en el refinado líquido
  y <- (A - a) / (I_E + A - a)  # fracción másica de soluto en el extracto líquido
  return(list(a = a, x = x, y = y))
}

# Función para calcular la composición de las fases líquidas después de cada etapa
# Los sólidos (0,30 kg por kg de alimentación) son inertes: quedan en la
# corriente de refinado y no entran en el equilibrio entre las dos fases
# líquidas, así que el diagrama ternario muestra los tres componentes
# líquidos (Agua, Etanol, Antioxidantes).
calcular_composiciones <- function(composicion_inicial, K, etapas, masa_etanol_por_etapa) {
  # Inicializar matrices para almacenar los resultados
  composicion_agua <- matrix(nrow = etapas, ncol = 3, dimnames = list(NULL, c("Agua", "Etanol", "Antioxidantes")))
  composicion_etanol <- matrix(nrow = etapas, ncol = 3, dimnames = list(NULL, c("Agua", "Etanol", "Antioxidantes")))
  eficiencia <- numeric(etapas)
  fraccion_x <- numeric(etapas)
  fraccion_y <- numeric(etapas)

  # Masas por cada kilogramo de alimentación (kg)
  masa_agua <- as.numeric(composicion_inicial["Agua"])
  masa_antioxidantes <- as.numeric(composicion_inicial["Antioxidantes"])

  for (i in 1:etapas) {
    # Reparto de equilibrio de los antioxidantes que entran a la etapa
    reparto <- reparto_equilibrio(masa_antioxidantes, masa_agua, masa_etanol_por_etapa, K)
    fraccion_x[i] <- reparto$x
    fraccion_y[i] <- reparto$y

    # Composición de las fases líquidas (fracciones másicas, cada una suma 1)
    composicion_agua[i, ] <- c(Agua = masa_agua / (masa_agua + reparto$a),
                               Etanol = 0,
                               Antioxidantes = reparto$x)

    composicion_etanol[i, ] <- c(Agua = 0,
                                 Etanol = masa_etanol_por_etapa / (masa_etanol_por_etapa + masa_antioxidantes - reparto$a),
                                 Antioxidantes = reparto$y)

    # Eficiencia de extracción en la etapa actual: antioxidantes extraídos
    # entre antioxidantes que entraron a la etapa
    eficiencia[i] <- ((masa_antioxidantes - reparto$a) / masa_antioxidantes) * 100

    # Actualizar la masa de antioxidantes para la siguiente etapa: lo que
    # quedó en el refinado líquido
    masa_antioxidantes <- reparto$a
  }

  return(list(Agua = composicion_agua, Etanol = composicion_etanol, Eficiencia = eficiencia,
              x = fraccion_x, y = fraccion_y, Restante = masa_antioxidantes))
}

# Aplicar la función  
resultados <- calcular_composiciones(composicion_inicial, K, etapas, masa_etanol_por_etapa)  
composicion_agua <- resultados$Agua  
composicion_etanol <- resultados$Etanol  
eficiencia <- resultados$Eficiencia  
eficiencia_global <- (as.numeric(composicion_inicial["Antioxidantes"]) - resultados$Restante) /
  as.numeric(composicion_inicial["Antioxidantes"]) * 100  # % extraído en total

# Crear un data frame para graficar  
datos <- data.frame(  
  Fase = rep(c("Fase Rica en Agua", "Fase Rica en Etanol"), each = etapas),  
  Etapa = rep(1:etapas, times = 2),  
  Agua = c(composicion_agua[, "Agua"], composicion_etanol[, "Agua"]),  
  Etanol = c(composicion_agua[, "Etanol"], composicion_etanol[, "Etanol"]),  
  Antioxidantes = c(composicion_agua[, "Antioxidantes"], composicion_etanol[, "Antioxidantes"])  
)  

# Diagrama ternario  
p <- ggtern(data = datos, aes(x = Agua, y = Etanol, z = Antioxidantes)) +  
  geom_point(aes(color = Fase, shape = as.factor(Etapa)), size = 4) +  
  labs(title = "Diagrama Ternario: Extracción de Antioxidantes",  
       x = "Agua",  
       y = "Etanol",  
       z = "Antioxidantes",  
       color = "Fase",  
       shape = "Etapa") +  
  theme_rgbw() +  
  theme(legend.position = "right")

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_12_ternario_antioxidantes.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Diagrama Ternario: Extracción de Antioxidantes
# --------------------------------------------------------------------------
# - Ejes: en un diagrama ternario no hay un eje X y un eje Y como en un gráfico
#   común: cada esquina del triángulo es un componente puro (Agua, Etanol y
#   Antioxidantes) y cada punto indica una composición. Las tres fracciones
#   másicas son adimensionales (kg de componente por kg de mezcla) y siempre
#   suman 1.
# - Eje X (una esquina): Agua, en fracción másica (adimensional, de 0 a 1).
# - Eje Y (otra esquina): Etanol, en fracción másica (adimensional, de 0 a 1).
# - Eje Z (tercera esquina): Antioxidantes, en fracción másica (adimensional,
#   de 0 a 1).
# - Color y forma: el color distingue la fase (rica en agua o rica en etanol)
#   y la forma del punto distingue la etapa de extracción (etapa 1 o etapa 2).
# - Lo que se aprecia: cuatro puntos, dos por etapa. En cada etapa el punto de
#   la fase rica en etanol está más cerca de la esquina de los antioxidantes
#   que el punto de la fase rica en agua: el etanol se lleva los antioxidantes.
#   En la etapa 1 el refinado líquido tiene 0,9774 de agua y 0,0226 de
#   antioxidantes, y el extracto 0,9207 de etanol y 0,0793 de antioxidantes;
#   en la etapa 2 el refinado baja a 0,0034 de antioxidantes y el extracto a
#   0,0117. Los sólidos inertes no aparecen en el triángulo: quedan en la
#   corriente de refinado y no entran en el equilibrio entre las fases líquidas.
# - Qué significa: cada etapa extrae una parte de los antioxidantes que quedaban
#   en la fase acuosa y los pasa al etanol. La eficiencia de cada etapa (en %)
#   se imprime en la consola: la primera extrae el 86,10 % de lo que entra y
#   la segunda el 85,47 % de lo que dejó la primera; en total se extrae el
#   97,98 % de los antioxidantes iniciales. En cada etapa las fracciones de
#   equilibrio cumplen K = y / x = 3,5.

# Mostrar resultados en la consola  
cat("Composición de la fase líquida rica en agua después de cada etapa:\n")  
print(composicion_agua)  
cat("\nComposición de la fase líquida rica en etanol después de cada etapa:\n")  
print(composicion_etanol)  
cat("\nEficiencia de extracción en cada etapa:\n")  
print(eficiencia)  
cat("\nEficiencia global de extracción después de", etapas, "etapas:", round(eficiencia_global, 2), "%\n")
cat("Comprobación de equilibrio en cada etapa (y / x = K =", K, "):\n")
print(round(resultados$y / resultados$x, 2))

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Un diagrama ternario representa mezclas de tres componentes: cada punto es
#   una composición y las tres fracciones másicas (adimensionales) suman 1.
# - En una extracción en varias etapas, cada etapa recupera una parte de lo que
#   quedó de la anterior: la primera etapa es la que más extrae en masa,
#   aunque la eficiencia de cada etapa (en % de lo que entra a esa etapa)
#   sea parecida.
# - La relación de distribución K (adimensional, aquí K = 3,5) dice hacia qué
#   fase migra el soluto: en el equilibrio, K = y / x con x e y fracciones
#   másicas de soluto en el refinado líquido y en el extracto líquido
#   (Treybal). La consola imprime la comprobación: y / x = 3,5 en las dos
#   etapas.
# - Los sólidos de la cáscara son inertes: quedan en la corriente de refinado
#   y no entran en el equilibrio entre las dos fases líquidas, por eso el
#   triángulo muestra los tres componentes líquidos (Agua, Etanol,
#   Antioxidantes).
# - El equilibrio riguroso necesita la masa de disolvente por etapa: el
#   archivo supone 1 kg de etanol fresco por kg de alimentación en cada
#   etapa (corriente cruzada); con otra relación disolvente/alimentación
#   los números cambian.

