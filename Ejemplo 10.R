# ==========================================================================
# Extracción Líquido-Líquido con Diagrama Ternario
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Extracción de ácido benzoico entre agua y benceno
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
if (!require(ggtern)) install.packages("ggtern", dependencies = TRUE)  # Para diagramas ternarios  
library(ggplot2)  
library(ggtern)  

# Datos del problema  
composicion_inicial <- c(Agua = 0.80, Benceno = 0.10, Acido_benzoico = 0.10)  # Fracciones másicas  
K <- 4.5  # Coeficiente de distribución K = y / x (fracción másica de ácido en benceno / fracción másica en agua), adimensional  

# Equilibrio de extracción en una etapa con disolventes inmiscibles
# (Treybal, Mass-Transfer Operations, 3.ª ed., cap. 10): el soluto se
# reparte entre las dos fases según K = y / x, donde x e y son las
# fracciones másicas de soluto en el refinado (fase acuosa) y en el
# extracto (fase orgánica). Con una base de 1 kg de mezcla, si a es la masa
# de soluto que queda en el refinado:
#   x = a / (I_R + a),  y = (A - a) / (I_E + A - a),
# donde A es la masa total de soluto, I_R la masa de disolvente del
# refinado e I_E la del extracto. La condición y = K * x da una ecuación
# cuadrática para a; se toma la raíz menor que A (la otra es mayor que la
# masa total de soluto y no tiene sentido físico).
reparto_equilibrio <- function(A, I_R, I_E, K) {
  coef2 <- K - 1
  coef1 <- A - I_R - K * I_E - K * A
  coef0 <- A * I_R
  discriminante <- coef1^2 - 4 * coef2 * coef0
  a <- (-coef1 - sqrt(discriminante)) / (2 * coef2)  # soluto en el refinado (kg)
  x <- a / (I_R + a)  # fracción másica de soluto en el refinado
  y <- (A - a) / (I_E + A - a)  # fracción másica de soluto en el extracto
  return(list(a = a, x = x, y = y))
}

# Base de cálculo: 1 kg de mezcla inicial. El agua (0,80 kg) y el benceno
# (0,10 kg) son los dos disolventes, prácticamente inmiscibles: el agua
# queda en el refinado y el benceno en el extracto. El ácido benzoico
# (0,10 kg) se reparte entre ambas fases según K.
masa_agua <- as.numeric(composicion_inicial["Agua"])  # kg por kg de mezcla
masa_benceno <- as.numeric(composicion_inicial["Benceno"])  # kg por kg de mezcla
masa_acido_total <- as.numeric(composicion_inicial["Acido_benzoico"])  # kg por kg de mezcla

reparto <- reparto_equilibrio(masa_acido_total, masa_agua, masa_benceno, K)
masa_acido_agua <- reparto$a  # kg de ácido en la fase acuosa
masa_acido_benceno <- masa_acido_total - masa_acido_agua  # kg de ácido en la fase orgánica

# Composición de cada fase en fracciones másicas (cada una suma 1). La masa
# de cada fase es su disolvente más el soluto que contiene, así que las
# fracciones de equilibrio cumplen K = y / x por construcción.
masa_fase_agua <- masa_agua + masa_acido_agua
masa_fase_benceno <- masa_benceno + masa_acido_benceno
composicion_agua <- c(Agua = masa_agua / masa_fase_agua,
                      Benceno = 0,
                      Acido_benzoico = reparto$x)
composicion_benceno <- c(Agua = 0,
                         Benceno = masa_benceno / masa_fase_benceno,
                         Acido_benzoico = reparto$y)

# Fracción de ácido extraído a la fase orgánica (eficiencia de la etapa)
eficiencia <- masa_acido_benceno / masa_acido_total * 100  # %

# Crear un data frame para graficar  
datos <- data.frame(  
  Fase = c("Fase Acuosa", "Fase Orgánica"),  
  Agua = c(composicion_agua[1], composicion_benceno[1]),  # Extraer el primer valor (Agua)  
  Benceno = c(composicion_agua[2], composicion_benceno[2]),  # Extraer el segundo valor (Benceno)  
  Acido_benzoico = c(composicion_agua[3], composicion_benceno[3])  # Extraer el tercer valor (Ácido benzoico)  
)  

# Diagrama ternario  
p <- ggtern(data = datos, aes(x = Agua, y = Benceno, z = Acido_benzoico)) +  
  geom_point(aes(color = Fase), size = 4) +  
  labs(title = "Diagrama Ternario: Extracción de Ácido Benzoico",  
       x = "Agua",  
       y = "Benceno",  
       z = "Ácido benzoico",  
       color = "Fase") +  
  theme_rgbw() +  
  theme(legend.position = "right")

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_10_ternario_benzoico.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Diagrama Ternario: Extracción de Ácido Benzoico
# --------------------------------------------------------------------------
# - Ejes: en un diagrama ternario no hay un eje X y un eje Y como en un gráfico
#   común: cada esquina del triángulo es un componente puro (Agua, Benceno y
#   Ácido benzoico) y cada punto indica una composición. Las tres fracciones
#   másicas son adimensionales (kg de componente por kg de mezcla) y siempre
#   suman 1.
# - Eje X (una esquina): Agua, en fracción másica (adimensional, de 0 a 1).
# - Eje Y (otra esquina): Benceno, en fracción másica (adimensional, de 0 a 1).
# - Eje Z (tercera esquina): Ácido benzoico, en fracción másica (adimensional,
#   de 0 a 1).
# - Lo que se aprecia: dos puntos, uno cerca de la esquina del agua (fase
#   acuosa: 0,9333 de agua y 0,0667 de ácido benzoico) y otro cerca de la
#   esquina del benceno (fase orgánica: 0,70 de benceno y 0,30 de ácido
#   benzoico).
# - Qué significa: el ácido benzoico prefiere el benceno al agua: la fracción
#   másica en la fase orgánica (0,30) es 4,5 veces la de la fase acuosa
#   (0,0667), que es justo K = 4,5. Por eso la extracción lo saca del agua
#   y lo lleva al benceno. Aun así, solo el 42,86 % del ácido pasa al
#   benceno: K no lo decide todo, también cuenta la cantidad de disolvente
#   (hay 0,80 kg de agua por cada 0,10 kg de benceno).

# Mostrar resultados en la consola  
cat("Composición en la fase acuosa después de la extracción:\n")  
print(composicion_agua)  
cat("\nComposición en la fase orgánica (benceno) después de la extracción:\n")  
print(composicion_benceno)  
cat("\nComprobación de equilibrio: y / x =", round(reparto$y / reparto$x, 2), "(K =", K, ")\n")
cat("Ácido benzoico extraído a la fase orgánica:", round(eficiencia, 2), "%\n")

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Un diagrama ternario representa mezclas de tres componentes: cada punto es
#   una composición y las tres fracciones másicas (adimensionales) suman 1.
# - Para leerlo se busca qué tan cerca está cada punto de cada esquina: cerca
#   de una esquina significa que ese componente domina la mezcla.
# - La relación de distribución K (adimensional) dice hacia qué fase migra el
#   soluto: con K = 4,5 el ácido benzoico se concentra en el benceno.
# - En el equilibrio, K = y / x con x e y fracciones másicas de soluto en el
#   refinado y en el extracto (Treybal): las fracciones que calcula el
#   archivo cumplen 0,30 / 0,0667 = 4,5.
# - K no lo decide todo: la fracción extraída también depende de la cantidad
#   de disolvente. Con 0,80 kg de agua por 0,10 kg de benceno, solo el
#   42,86 % del ácido pasa a la fase orgánica aunque K = 4,5.
# - Las fracciones de cada fase suman 1: en el diagrama ternario cada punto
#   es la composición dentro de su fase.

