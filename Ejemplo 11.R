# ==========================================================================
# Extracción Líquido-Líquido en Bioquímica Industrial
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Extracción de proteínas usando un sistema acuoso bifásico (ABS)
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
if (!require(ggtern)) install.packages("ggtern", dependencies = TRUE)  # Para diagramas ternarios  
library(ggplot2)  
library(ggtern)  

# Datos del problema  
composicion_inicial <- c(Agua = 0.80, PEG = 0.10, Proteinas = 0.10)  # Fracciones másicas  
K <- 3.2  # Coeficiente de distribución K = y / x (fracción másica de proteína en PEG / fracción másica en fosfato), adimensional  

# Equilibrio de extracción en una etapa con disolventes inmiscibles
# (Treybal, Mass-Transfer Operations, 3.ª ed., cap. 10): el soluto se
# reparte entre las dos fases según K = y / x, donde x e y son las
# fracciones másicas de soluto en el refinado (fase rica en fosfato) y en
# el extracto (fase rica en PEG). Con una base de 1 kg de mezcla, si a es
# la masa de soluto que queda en el refinado:
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

# Base de cálculo: 1 kg de mezcla inicial. El agua (0,80 kg) y el PEG
# (0,10 kg) son los dos disolventes: el agua queda en la fase rica en
# fosfato y el PEG en su fase. En un ABS real las dos fases son acuosas y
# el agua se reparte entre ellas; el archivo usa la idealización de manual
# de disolventes inmiscibles. Las proteínas (0,10 kg) se reparten entre
# ambas fases según K.
masa_agua <- as.numeric(composicion_inicial["Agua"])  # kg por kg de mezcla
masa_PEG <- as.numeric(composicion_inicial["PEG"])  # kg por kg de mezcla
masa_proteinas_total <- as.numeric(composicion_inicial["Proteinas"])  # kg por kg de mezcla

reparto <- reparto_equilibrio(masa_proteinas_total, masa_agua, masa_PEG, K)
masa_proteinas_fosfato <- reparto$a  # kg de proteínas en la fase rica en fosfato
masa_proteinas_PEG <- masa_proteinas_total - masa_proteinas_fosfato  # kg de proteínas en la fase rica en PEG

# Composición de cada fase en fracciones másicas (cada una suma 1). La masa
# de cada fase es su disolvente más el soluto que contiene, así que las
# fracciones de equilibrio cumplen K = y / x por construcción.
masa_fase_fosfato <- masa_agua + masa_proteinas_fosfato
masa_fase_PEG <- masa_PEG + masa_proteinas_PEG
composicion_fosfato <- c(Agua = masa_agua / masa_fase_fosfato,
                         PEG = 0,
                         Proteinas = reparto$x)
composicion_PEG <- c(Agua = 0,
                     PEG = masa_PEG / masa_fase_PEG,
                     Proteinas = reparto$y)

# Fracción de proteínas extraída a la fase rica en PEG (eficiencia de la etapa)
eficiencia <- masa_proteinas_PEG / masa_proteinas_total * 100  # %

# Crear un data frame para graficar  
datos <- data.frame(  
  Fase = c("Fase Rica en Fosfato", "Fase Rica en PEG"),  
  Agua = c(composicion_fosfato[1], composicion_PEG[1]),  # Extraer el primer valor (Agua)  
  PEG = c(composicion_fosfato[2], composicion_PEG[2]),  # Extraer el segundo valor (PEG)  
  Proteinas = c(composicion_fosfato[3], composicion_PEG[3])  # Extraer el tercer valor (Proteínas)  
)  

# Diagrama ternario  
p <- ggtern(data = datos, aes(x = Agua, y = PEG, z = Proteinas)) +  
  geom_point(aes(color = Fase), size = 4) +  
  labs(title = "Diagrama Ternario: Extracción de Proteínas usando ABS",  
       x = "Agua",  
       y = "PEG",  
       z = "Proteínas",  
       color = "Fase") +  
  theme_rgbw() +  
  theme(legend.position = "right")

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_11_ternario_abs.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Diagrama Ternario: Extracción de Proteínas usando ABS
# --------------------------------------------------------------------------
# - Ejes: en un diagrama ternario no hay un eje X y un eje Y como en un gráfico
#   común: cada esquina del triángulo es un componente puro (Agua, PEG y
#   Proteínas) y cada punto indica una composición. Las tres fracciones
#   másicas son adimensionales (kg de componente por kg de mezcla) y siempre
#   suman 1.
# - Eje X (una esquina): Agua, en fracción másica (adimensional, de 0 a 1).
# - Eje Y (otra esquina): PEG (polietilenglicol), en fracción másica
#   (adimensional, de 0 a 1).
# - Eje Z (tercera esquina): Proteínas, en fracción másica (adimensional,
#   de 0 a 1).
# - Lo que se aprecia: dos puntos, uno cerca de la esquina del agua (fase rica
#   en fosfato: 0,9226 de agua y 0,0774 de proteínas) y otro cerca de la
#   esquina del PEG (fase rica en PEG: 0,7524 de PEG y 0,2476 de proteínas).
# - Qué significa: en este sistema acuoso bifásico (ABS) las proteínas prefieren
#   la fase rica en PEG: la fracción másica en la fase de PEG (0,2476) es
#   3,2 veces la de la fase de fosfato (0,0774), que es justo K = 3,2. Por
#   eso el ABS sirve para separar proteínas sin usar disolventes orgánicos.
#   Aun así, solo el 32,91 % de las proteínas pasa a la fase de PEG: K no
#   lo decide todo, también cuenta la cantidad de disolvente (hay 0,80 kg
#   de agua por cada 0,10 kg de PEG).

# Mostrar resultados en la consola  
cat("Composición en la fase rica en fosfato después de la extracción:\n")  
print(composicion_fosfato)  
cat("\nComposición en la fase rica en PEG después de la extracción:\n")  
print(composicion_PEG)  
cat("\nComprobación de equilibrio: y / x =", round(reparto$y / reparto$x, 2), "(K =", K, ")\n")
cat("Proteínas extraídas a la fase rica en PEG:", round(eficiencia, 2), "%\n")

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Un sistema acuoso bifásico (ABS) forma dos fases acuosas (una rica en PEG
#   y otra rica en fosfato) que permiten separar proteínas sin disolventes
#   orgánicos.
# - Un diagrama ternario representa mezclas de tres componentes: cada punto es
#   una composición y las tres fracciones másicas (adimensionales) suman 1.
# - La relación de distribución K (adimensional) dice hacia qué fase migra el
#   soluto: con K = 3,2 las proteínas se concentran en la fase rica en PEG.
# - En el equilibrio, K = y / x con x e y fracciones másicas de soluto en el
#   refinado y en el extracto (Treybal): las fracciones que calcula el
#   archivo cumplen 0,2476 / 0,0774 = 3,2.
# - K no lo decide todo: la fracción extraída también depende de la cantidad
#   de disolvente. Con 0,80 kg de agua por 0,10 kg de PEG, solo el 32,91 %
#   de las proteínas pasa a la fase rica en PEG aunque K = 3,2.
# - Las fracciones de cada fase suman 1: en el diagrama ternario cada punto
#   es la composición dentro de su fase.

