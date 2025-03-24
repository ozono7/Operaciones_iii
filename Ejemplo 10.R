# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Extracción Líquido-Líquido con Diagrama Ternario  
# Problema: Extracción de ácido benzoico entre agua y benceno  

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
ggtern(data = datos, aes(x = Agua, y = Benceno, z = Acido_benzoico)) +  
  geom_point(aes(color = Fase), size = 4) +  
  labs(title = "Diagrama Ternario: Extracción de Ácido Benzoico",  
       x = "Agua",  
       y = "Benceno",  
       z = "Ácido benzoico",  
       color = "Fase") +  
  theme_rgbw() +  
  theme(legend.position = "right")  

# Mostrar resultados en la consola  
cat("Composición en la fase acuosa después de la extracción:\n")  
print(composicion_agua)  
cat("\nComposición en la fase orgánica (benceno) después de la extracción:\n")  
print(composicion_benceno)  

