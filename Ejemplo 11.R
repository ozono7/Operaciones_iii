# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Extracción Líquido-Líquido en Bioquímica Industrial  
# Problema: Extracción de proteínas usando un sistema acuoso bifásico (ABS)  

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
ggtern(data = datos, aes(x = Agua, y = PEG, z = Proteinas)) +  
  geom_point(aes(color = Fase), size = 4) +  
  labs(title = "Diagrama Ternario: Extracción de Proteínas usando ABS",  
       x = "Agua",  
       y = "PEG",  
       z = "Proteínas",  
       color = "Fase") +  
  theme_rgbw() +  
  theme(legend.position = "right")  

# Mostrar resultados en la consola  
cat("Composición en la fase rica en fosfato después de la extracción:\n")  
print(composicion_fosfato)  
cat("\nComposición en la fase rica en PEG después de la extracción:\n")  
print(composicion_PEG)  

