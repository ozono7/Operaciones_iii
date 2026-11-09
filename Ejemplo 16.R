# ==========================================================================
# Evaluación de las Isotermas de Sorción en la Extracción de Aceite
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Extracción de aceite de una harina de pescado usando benceno
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
masa_harina <- 100  # Masa de harina de pescado (kg)  
concentracion_aceite <- 0.20  # Fracción de aceite en la harina  
concentracion_inertes <- 0.80  # Fracción de sólidos inertes en la harina  
relacion_solvente_solido <- 2  # Relación solvente/sólido (kg benceno/kg harina)  
q_max <- 0.5  # Capacidad máxima de sorción (kg/kg)  
K <- 2  # Constante de equilibrio ((kg/kg)^-1)  
etapas <- 3  # Número de etapas de extracción  

# Función de la isoterma de Langmuir  
isoterma_langmuir <- function(C, q_max, K) {  
  q <- (q_max * K * C) / (1 + K * C)  
  return(q)  
}  

# Función para calcular la extracción en cada etapa
# La isoterma da q, el aceite RETENIDO por kg de inertes en equilibrio con
# la concentración C del líquido (Langmuir, 1916: q_e es la cantidad
# adsorbida por unidad de masa de adsorbente). El balance de cada etapa es:
#   aceite que entra = aceite en el extracto + aceite retenido
#   O_in = C * S + q(C) * M,
# con S la masa de benceno y M la masa de inertes. De ahí sale C (el balance
# es una ecuación cuadrática en C), el aceite extraído es C * S y el
# retenido es q * M; el sólido, con el aceite retenido, pasa a la etapa
# siguiente.
calcular_extraccion <- function(masa_harina, concentracion_aceite, relacion_solvente_solido, q_max, K, etapas) {  
  masa_aceite_inicial <- masa_harina * concentracion_aceite  # Masa inicial de aceite (kg)  
  masa_inertes <- masa_harina * concentracion_inertes  # Masa de sólidos inertes (kg)  
  masa_benceno <- masa_harina * relacion_solvente_solido  # Masa de benceno por etapa (kg)  
  
  # Inicializar vectores para almacenar los resultados  
  aceite_extraido <- numeric(etapas)  
  aceite_retenido <- numeric(etapas)  
  aceite_restante <- numeric(etapas + 1)  # +1 para almacenar el valor inicial  
  concentracion <- numeric(etapas)  
  q_equilibrio <- numeric(etapas)  
  eficiencia <- numeric(etapas)  
  
  # Condiciones iniciales  
  aceite_restante[1] <- masa_aceite_inicial  
  
  for (i in 1:etapas) {  
    # Aceite que entra a la etapa: el que quedó retenido en el sólido
    aceite_entrada <- aceite_restante[i]

    # Resolver el balance O_in = C * S + q(C) * M para C. Con la isoterma
    # de Langmuir el balance es una ecuación cuadrática en C:
    #   (K*S) * C^2 + (S + M*q_max*K - O_in*K) * C - O_in = 0
    coef2 <- K * masa_benceno
    coef1 <- masa_benceno + masa_inertes * q_max * K - aceite_entrada * K
    coef0 <- -aceite_entrada
    C <- (-coef1 + sqrt(coef1^2 - 4 * coef2 * coef0)) / (2 * coef2)  # Concentración de aceite en la fase líquida (kg/kg)

    # Cantidad de aceite retenido por kg de inertes en equilibrio con C (kg/kg)
    q <- isoterma_langmuir(C, q_max, K)

    # Aceite en el extracto (disuelto en el benceno) y aceite retenido en
    # el sólido (kg)
    aceite_extraido[i] <- C * masa_benceno
    aceite_retenido[i] <- q * masa_inertes

    concentracion[i] <- C
    q_equilibrio[i] <- q

    # El sólido, con el aceite retenido, pasa a la etapa siguiente
    aceite_restante[i + 1] <- aceite_retenido[i]

    # Eficiencia de extracción en la etapa actual (% del aceite inicial)
    eficiencia[i] <- (aceite_extraido[i] / masa_aceite_inicial) * 100  
  }  
  
  return(list(aceite_extraido = aceite_extraido, aceite_retenido = aceite_retenido,
              aceite_restante = aceite_restante, concentracion = concentracion,
              q_equilibrio = q_equilibrio, eficiencia = eficiencia))  
}  

# Aplicar la función  
resultados <- calcular_extraccion(masa_harina, concentracion_aceite, relacion_solvente_solido, q_max, K, etapas)  
aceite_extraido <- resultados$aceite_extraido  
aceite_retenido <- resultados$aceite_retenido  
aceite_restante <- resultados$aceite_restante  
concentracion <- resultados$concentracion  
q_equilibrio <- resultados$q_equilibrio  
eficiencia <- resultados$eficiencia  

# Crear un data frame para graficar la isoterma de sorción  
C <- seq(0, 1, by = 0.01)  # Concentración de aceite en la fase líquida (kg/kg)  
q <- isoterma_langmuir(C, q_max, K)  # Cantidad de aceite adsorbido (kg/kg)  

datos_isoterma <- data.frame(  
  Concentracion = C,  
  Adsorcion = q  
)  

# Gráfico de la isoterma de sorción  
p_isoterma <- ggplot(datos_isoterma, aes(x = Concentracion, y = Adsorcion)) +  
  geom_line(color = "blue", linewidth = 1) +  
  labs(title = "Isoterma de Sorción de Langmuir",  
       x = "Concentración de Aceite en la Fase Líquida (kg/kg)",  
       y = "Cantidad de Aceite Adsorbido (kg/kg)") +
  theme_minimal()

# Mostrar gráfico
print(p_isoterma)

# Guardar el gráfico
ggsave("ejemplo_16_isoterma.png", plot = p_isoterma, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Isoterma de Sorción de Langmuir
# --------------------------------------------------------------------------
# - Eje X: Concentración de aceite en la fase líquida, en kilogramos de aceite
#   por kilogramo de benceno (kg/kg). Va de 0 a 1 kg/kg: es la masa de aceite
#   por cada kilogramo de benceno, no por litro, porque el archivo calcula
#   C como aceite disuelto entre masa de benceno.
# - Eje Y: Cantidad de aceite adsorbido, en kilogramos de aceite por kilogramo
#   de sólido (kg/kg). Indica cuánto aceite retiene cada kilogramo de inertes.
# - Lo que se aprecia: la curva sube con pendiente fuerte al principio y luego
#   se aplana, acercándose a un valor máximo sin superarlo.
# - Qué significa: al principio hay muchos sitios libres y el sólido adsorbe
#   mucho aceite por cada aumento de concentración; cuando los sitios se van
#   ocupando, adsorber más cuesta más, hasta llegar a la capacidad máxima
#   qmax = 0,5 kg/kg del archivo. Esa forma curva es la firma de la isoterma
#   de Langmuir.

# Crear un data frame para graficar la extracción en función del número de etapas  
datos_extraccion <- data.frame(  
  Etapa = 1:etapas,  
  Aceite_Extraido = aceite_extraido,  
  Eficiencia = eficiencia  
)  

# Gráfico de la extracción en función del número de etapas  
p_extraccion <- ggplot(datos_extraccion, aes(x = Etapa, y = Aceite_Extraido)) +  
  geom_line(color = "red", linewidth = 1) +  
  geom_point(color = "red", size = 3) +  
  labs(title = "Extracción de Aceite en Múltiples Etapas",  
       x = "Número de Etapas",  
       y = "Aceite Extraído (kg)") +
  theme_minimal()

# Mostrar gráfico
print(p_extraccion)

# Guardar el gráfico
ggsave("ejemplo_16_extraccion_etapas.png", plot = p_extraccion, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Extracción de Aceite en Múltiples Etapas
# --------------------------------------------------------------------------
# - Eje X: Número de etapas. Es un conteo (1, 2, 3), adimensional: no tiene
#   unidades físicas, solo indica la primera, segunda o tercera etapa.
# - Eje Y: Aceite extraído en esa etapa, en kilogramos (kg).
# - Lo que se aprecia: los puntos unidos por la línea muestran cuánto aceite
#   se extrae en cada etapa: 14,83 kg en la primera, 3,73 kg en la segunda
#   y 1,03 kg en la tercera. La cantidad extraída por etapa va disminuyendo.
# - Qué significa: las primeras etapas son las más productivas y las siguientes
#   aportan cada vez menos; por eso hay que valorar si una etapa más compensa
#   el benceno y el tiempo que consume. En total se extraen 19,59 kg de los
#   20 kg iniciales (eficiencia global 98 %); en el sólido quedan retenidos
#   0,41 kg. La eficiencia de cada etapa (en %) se imprime en la consola.

# Mostrar resultados en la consola  
cat("Cantidad de aceite extraído en cada etapa (kg):\n")  
print(aceite_extraido)  
cat("\nAceite retenido en el sólido en cada etapa (kg):\n")  
print(aceite_retenido)  
cat("\nConcentración de equilibrio en el líquido en cada etapa (kg/kg):\n")  
print(concentracion)  
cat("\nAceite retenido por kg de inertes en cada etapa (kg/kg):\n")  
print(q_equilibrio)  
cat("\nEficiencia de extracción en cada etapa (%):\n")  
print(eficiencia)  
cat("\nEficiencia global de extracción después de", etapas, "etapas:", round(sum(eficiencia)), "%\n")  

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La isoterma de Langmuir tiene forma de curva que se aplana: la cantidad
#   adsorbida crece con la concentración hasta llegar a una capacidad máxima
#   (qmax, en kg/kg).
# - En la isoterma, q es el aceite RETENIDO por kg de sólido en equilibrio
#   con la concentración C del líquido (Langmuir, 1916), no el aceite
#   extraído: el extraído es el que queda disuelto en el benceno, C por la
#   masa de benceno.
# - El balance de cada etapa es: aceite que entra = aceite en el extracto
#   (C por la masa de benceno) + aceite retenido (q por la masa de inertes).
#   El sólido, con el aceite retenido, pasa a la etapa siguiente.
# - En una extracción por etapas, cada etapa extrae menos que la anterior porque
#   queda menos soluto por extraer; la eficiencia global es la suma de las
#   eficiencias de cada etapa (en %).
# - El número de etapas es un conteo adimensional, mientras que el aceite
#   extraído se mide en kilogramos (kg).

