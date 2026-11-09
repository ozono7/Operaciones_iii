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
K <- 4.5  # Relación de distribución (C_benceno / C_agua)  

# Función para calcular la composición de las fases después de la extracción  
calcular_composiciones <- function(composicion_inicial, K) {  
  # Masa total de ácido benzoico en la mezcla inicial  
  masa_total_acido <- composicion_inicial["Acido_benzoico"]  
  
  # Concentración en la fase acuosa después de la extracción  
  C_agua <- masa_total_acido / (1 + K)  
  
  # Concentración en la fase orgánica (benceno) después de la extracción  
  C_benceno <- K * C_agua  
  
  # Composición final de las fases  
  composicion_agua <- c(Agua = composicion_inicial["Agua"],  
                        Benceno = 0,  
                        Acido_benzoico = C_agua)  
  
  composicion_benceno <- c(Agua = 0,  
                           Benceno = composicion_inicial["Benceno"],  
                           Acido_benzoico = C_benceno)  
  
  # Cada composición se normaliza dentro de su fase para que las tres
  # fracciones másicas sumen 1, como exige un diagrama ternario. La relación
  # K del archivo reparte la masa de ácido entre las fases (modelo
  # simplificado): primero se reparte la masa y después se normaliza.
  composicion_agua <- composicion_agua / sum(composicion_agua)
  composicion_benceno <- composicion_benceno / sum(composicion_benceno)

  return(list(Agua = composicion_agua, Benceno = composicion_benceno))  
}  

# Aplicar la función  
resultados <- calcular_composiciones(composicion_inicial, K)  
composicion_agua <- resultados$Agua  
composicion_benceno <- resultados$Benceno  

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
#   acuosa, con poco ácido benzoico) y otro cerca de la esquina del benceno
#   (fase orgánica, con mucho más ácido benzoico).
# - Qué significa: el ácido benzoico prefiere el benceno al agua: la relación
#   de distribución K = 4,5 indica que su concentración en benceno es 4,5 veces
#   la del agua. Por eso la extracción lo saca del agua y lo lleva al benceno.

# Mostrar resultados en la consola  
cat("Composición en la fase acuosa después de la extracción:\n")  
print(composicion_agua)  
cat("\nComposición en la fase orgánica (benceno) después de la extracción:\n")  
print(composicion_benceno)  

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Un diagrama ternario representa mezclas de tres componentes: cada punto es
#   una composición y las tres fracciones másicas (adimensionales) suman 1.
# - Para leerlo se busca qué tan cerca está cada punto de cada esquina: cerca
#   de una esquina significa que ese componente domina la mezcla.
# - La relación de distribución K (adimensional) dice hacia qué fase migra el
#   soluto: con K = 4,5 el ácido benzoico se concentra en el benceno.
# - Las fracciones de cada fase se normalizan para que sumen 1: en el diagrama
#   ternario cada punto es la composición dentro de su fase.

