# Diseño de una torre de absorción y el cálculo del número de etapas usando el método de McCabe-Thiele

# Cargar librería
library(ggplot2)  # Para crear gráficos

# ---------------------- Datos del problema ----------------------
flujo_gas <- 100  # Flujo de gas en kg/h
flujo_liquido <- 1.5 * flujo_gas  # Flujo de líquido en kg/h (1.5 veces el flujo de gas)
y_entrada <- 0.02  # Fracción molar de A en el gas de entrada
y_salida <- 0.002  # Fracción molar de A en el gas de salida
pendiente_equilibrio <- 1.5  # Pendiente de la línea de equilibrio (relación y/x en equilibrio)

# ---------------------- Balance de masa ----------------------
x_entrada <- 0  # Se asume que el líquido entra puro (sin soluto A)
x_salida <- (flujo_gas / flujo_liquido) * (y_entrada - y_salida)  # Fracción molar de A en el líquido de salida

# ---------------------- Definir las líneas de equilibrio y operación ----------------------

# Línea de equilibrio: y = m * x
# Representa la relación entre las fracciones molares de A en el gas y el líquido en equilibrio.
linea_equilibrio <- function(x) pendiente_equilibrio * x

# Línea de operación: y = y_entrada - (G/L) * (x - x_entrada)
# Representa la relación entre las fracciones molares de A en el gas y el líquido durante la operación.
linea_operacion <- function(x) y_entrada - (flujo_gas / flujo_liquido) * (x - x_entrada)

# ---------------------- Generar puntos para graficar ----------------------
# Se generan valores de x (fracción molar de A en el líquido) desde 0 hasta x_salida.
x_valores <- seq(0, x_salida, length.out = 100)

# Se calculan los valores de y correspondientes a la línea de equilibrio y la línea de operación.
y_equilibrio <- linea_equilibrio(x_valores)
y_operacion <- linea_operacion(x_valores)

# ---------------------- Cálculo del número de etapas ----------------------
# Inicialización de variables para el cálculo de etapas.
x_etapa <- x_salida  # Comenzamos desde la fracción molar de A en el líquido de salida.
y_etapa <- linea_operacion(x_etapa)  # Fracción molar de A en el gas correspondiente.
etapas <- data.frame(x = numeric(), y = numeric())  # Dataframe para almacenar las etapas.

lista_etapas <- list()  # Lista temporal para almacenar las coordenadas de las etapas.
iteracion <- 1  # Contador de iteraciones.
max_iteraciones <- 50  # Número máximo de iteraciones para evitar bucles infinitos.

# Bucle para calcular las etapas:
# 1. Se mueve verticalmente desde la línea de operación hasta la línea de equilibrio.
# 2. Se mueve horizontalmente desde la línea de equilibrio hasta la línea de operación.
# Esto se repite hasta que la fracción molar de A en el gas (y_etapa) sea menor o igual a y_salida.
while (y_etapa > y_salida && iteracion <= max_iteraciones) {
  # Guardar la etapa actual (punto en la línea de operación).
  lista_etapas[[iteracion]] <- c(x_etapa, y_etapa)
  iteracion <- iteracion + 1
  
  # Calcular la siguiente etapa en la línea de equilibrio (movimiento vertical).
  x_etapa <- y_etapa / pendiente_equilibrio
  lista_etapas[[iteracion]] <- c(x_etapa, y_etapa)
  iteracion <- iteracion + 1
  
  # Calcular la siguiente etapa en la línea de operación (movimiento horizontal).
  y_etapa <- linea_operacion(x_etapa)
}

# Convertir la lista de etapas a un data.frame para facilitar su uso en ggplot.
etapas <- as.data.frame(do.call(rbind, lista_etapas))
colnames(etapas) <- c("x", "y")

# ---------------------- Graficar el diagrama McCabe-Thiele ----------------------
ggplot() +
  # Línea de equilibrio (azul, punteada).
  geom_line(aes(x = x_valores, y = y_equilibrio), color = "blue", size = 1, linetype = "dashed") +
  
  # Línea de operación (roja, continua).
  geom_line(aes(x = x_valores, y = y_operacion), color = "red", size = 1) +
  
  # Puntos de las etapas (negros).
  geom_point(data = etapas, aes(x = x, y = y), color = "black", size = 3) +
  
  # Línea que conecta las etapas (negra, punteada).
  geom_path(data = etapas, aes(x = x, y = y), color = "black", linetype = "dotted") +
  
  # Estilos del gráfico.
  labs(title = "Diagrama McCabe-Thiele para la Torre de Absorción",
       x = "Fracción molar de A en líquido",
       y = "Fracción molar de A en gas") +
  theme_minimal()

# ---------------------- Mostrar el número de etapas ----------------------
# El número de etapas es la mitad del número de puntos en la lista de etapas.
num_etapas <- nrow(etapas) / 2
cat("Número de etapas requeridas:", num_etapas, "\n")
