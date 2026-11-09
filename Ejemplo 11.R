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
K <- 3.2  # Relación de distribución (C_PEG / C_Fosfato)  

# Función para calcular la composición de las fases después de la extracción  
calcular_composiciones <- function(composicion_inicial, K) {  
  # Masa total de proteínas en el caldo de fermentación inicial  
  masa_total_proteinas <- composicion_inicial["Proteinas"]  
  
  # Concentración en la fase rica en fosfato después de la extracción  
  C_fosfato <- masa_total_proteinas / (1 + K)  
  
  # Concentración en la fase rica en PEG después de la extracción  
  C_PEG <- K * C_fosfato  
  
  # Composición final de las fases  
  composicion_fosfato <- c(Agua = composicion_inicial["Agua"],  
                           PEG = 0,  
                           Proteinas = C_fosfato)  
  
  composicion_PEG <- c(Agua = 0,  
                       PEG = composicion_inicial["PEG"],  
                       Proteinas = C_PEG)  
  
  # Cada composición se normaliza dentro de su fase para que las tres
  # fracciones másicas sumen 1, como exige un diagrama ternario. La relación
  # K del archivo reparte la masa de proteína entre las fases (modelo
  # simplificado): primero se reparte la masa y después se normaliza.
  composicion_fosfato <- composicion_fosfato / sum(composicion_fosfato)
  composicion_PEG <- composicion_PEG / sum(composicion_PEG)

  return(list(Fosfato = composicion_fosfato, PEG = composicion_PEG))  
}  

# Aplicar la función  
resultados <- calcular_composiciones(composicion_inicial, K)  
composicion_fosfato <- resultados$Fosfato  
composicion_PEG <- resultados$PEG  

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
#   en fosfato, con poca proteína) y otro cerca de la esquina del PEG (fase
#   rica en PEG, con mucha más proteína).
# - Qué significa: en este sistema acuoso bifásico (ABS) las proteínas prefieren
#   la fase rica en PEG: la relación de distribución K = 3,2 indica que su
#   concentración en la fase de PEG es 3,2 veces la de la fase de fosfato. Por
#   eso el ABS sirve para separar proteínas sin usar disolventes orgánicos.

# Mostrar resultados en la consola  
cat("Composición en la fase rica en fosfato después de la extracción:\n")  
print(composicion_fosfato)  
cat("\nComposición en la fase rica en PEG después de la extracción:\n")  
print(composicion_PEG)  

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
# - Las fracciones de cada fase se normalizan para que sumen 1: en el diagrama
#   ternario cada punto es la composición dentro de su fase.

