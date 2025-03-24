# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Extracción Líquido-Líquido en la Industria Alimentaria  
# Problema: Extracción de antioxidantes a partir de cáscaras de cítricos  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
if (!require(ggtern)) install.packages("ggtern", dependencies = TRUE)  # Para diagramas ternarios  
library(ggplot2)  
library(ggtern)  

# Datos del problema  
composicion_inicial <- c(Agua = 0.60, Solidos = 0.30, Antioxidantes = 0.10)  # Fracciones másicas  
K <- 3.5  # Relación de distribución (C_Etanol / C_Agua)  
etapas <- 2  # Número de etapas de extracción  

# Función para calcular la composición de las fases después de cada etapa  
calcular_composiciones <- function(composicion_inicial, K, etapas) {  
  # Inicializar vectores para almacenar los resultados  
  composicion_agua <- matrix(nrow = etapas, ncol = 3, dimnames = list(NULL, c("Agua", "Solidos", "Antioxidantes")))  
  composicion_etanol <- matrix(nrow = etapas, ncol = 3, dimnames = list(NULL, c("Agua", "Solidos", "Antioxidantes")))  
  eficiencia <- numeric(etapas)  
  
  # Composición inicial  
  masa_total_antioxidantes <- composicion_inicial["Antioxidantes"]  
  
  for (i in 1:etapas) {  
    # Concentración en la fase rica en agua después de la extracción  
    C_agua <- masa_total_antioxidantes / (1 + K)  
    
    # Concentración en la fase rica en etanol después de la extracción  
    C_etanol <- K * C_agua  
    
    # Composición final de las fases  
    composicion_agua[i, ] <- c(Agua = composicion_inicial["Agua"],  
                               Solidos = composicion_inicial["Solidos"],  
                               Antioxidantes = C_agua)  
    
    composicion_etanol[i, ] <- c(Agua = 0,  
                                 Solidos = 0,  
                                 Antioxidantes = C_etanol)  
    
    # Eficiencia de extracción en la etapa actual  
    eficiencia[i] <- (C_etanol * 100) / masa_total_antioxidantes  
    
    # Actualizar la masa total de antioxidantes para la siguiente etapa  
    masa_total_antioxidantes <- C_agua  
  }  
  
  return(list(Agua = composicion_agua, Etanol = composicion_etanol, Eficiencia = eficiencia))  
}  

# Aplicar la función  
resultados <- calcular_composiciones(composicion_inicial, K, etapas)  
composicion_agua <- resultados$Agua  
composicion_etanol <- resultados$Etanol  
eficiencia <- resultados$Eficiencia  

# Crear un data frame para graficar  
datos <- data.frame(  
  Fase = rep(c("Fase Rica en Agua", "Fase Rica en Etanol"), each = etapas),  
  Etapa = rep(1:etapas, times = 2),  
  Agua = c(composicion_agua[, "Agua"], composicion_etanol[, "Agua"]),  
  Solidos = c(composicion_agua[, "Solidos"], composicion_etanol[, "Solidos"]),  
  Antioxidantes = c(composicion_agua[, "Antioxidantes"], composicion_etanol[, "Antioxidantes"])  
)  

# Diagrama ternario  
ggtern(data = datos, aes(x = Agua, y = Solidos, z = Antioxidantes)) +  
  geom_point(aes(color = Fase, shape = as.factor(Etapa)), size = 4) +  
  labs(title = "Diagrama Ternario: Extracción de Antioxidantes",  
       x = "Agua",  
       y = "Sólidos",  
       z = "Antioxidantes",  
       color = "Fase",  
       shape = "Etapa") +  
  theme_rgbw() +  
  theme(legend.position = "right")  

# Mostrar resultados en la consola  
cat("Composición en la fase rica en agua después de cada etapa:\n")  
print(composicion_agua)  
cat("\nComposición en la fase rica en etanol después de cada etapa:\n")  
print(composicion_etanol)  
cat("\nEficiencia de extracción en cada etapa:\n")  
print(eficiencia)  

