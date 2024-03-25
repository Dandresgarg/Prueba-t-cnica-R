library(sf)
library(spdep)
library(openxlsx)
library(ggplot2)

###############################################################################################
################################MANEJO DE CENSO DE EDIFICACIONES###############################
###############################################################################################

################Descargue del archivo e importación al paquete estadístico#####################

Ejercicio <- read_sf("C:\\Users\\dandr\\Desktop\\Trabajo\\Investigación\\Ejercicio.csv")

#############################Obtener el número de filas y columnas#############################
#Esto lo hice para verificar queel archivo se hubiera pasado de forma correcta
dimensiones <- dim(Ejercicio)
print(dimensiones)

############################Contar los valores faltantes por columna###########################
#De esta manera no había ninguno, se asumió que los valores faltantes eran valores en blnco
valores_faltantes_por_columna <- colSums(is.na(Ejercicio))

########Contar los valores faltantes por columna (si se representan como cadenas vacías)#######
valores_faltantes_por_columna <- colSums(Ejercicio == "")

############################Ordenar los resultados de mayor a menor############################
#Esto lo hice porque sentía que me ahorraba un paso al ordenarlos de esta manera, pero no contaba
#con que fueran tres variables las que representaran de gran manera el número de vacíos en el archivo
#En caso dado de no haber procedido así, sino haber guardado el objeto con las 4 variables con más
#valores faltantes, simplemente hubiese obtenido: CONCRETO, MEZ_OBRA, USO_DOS Y TRIMESTRE, pero 
#dificilmente me hubiera percatado que el mismo valor de trimestre correspondía para unas 42 variables más
valores_faltantes_ordenados <- valores_faltantes_por_columna[order(-valores_faltantes_por_columna)]

#############Mostrar los valores faltantes por columna ordenados de mayor a menor##############
print(valores_faltantes_ordenados)

####################Crear un objetocon los valores faltantes ordenados####################
df_valores_faltantes <- data.frame(Columna = names(valores_faltantes_ordenados),
                                   Valores_faltantes = valores_faltantes_ordenados)

# Seleccionar las 4 primeras filas con más valores faltantes
df_valores_faltantes_top4 <- head(df_valores_faltantes, 4)

####################Abrir ventana para mejor visualización####################
x11()

# Crear el gráfico de barras para las 4 columnas con más valores faltantes
#En este gráfico se evidencia la gran diferencia entre los datos faltantes de la tercer variable
#con los que faltan en la cuarta
ggplot(df_valores_faltantes_top4, aes(x = Columna, y = Valores_faltantes)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Columna", y = "Número de Valores Faltantes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#############Crear el gráfico de barras pero ya para  todas las variables#####################
#Acá se evidencia de manera global que las variables con más valores nulos de manera muy notoria son solo 3
ggplot(df_valores_faltantes, aes(x = Columna, y = Valores_faltantes)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Columna", y = "Número de Valores Faltantes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#################Excluir las variables CONCRETO, MEZ_OBRA y USO_DOS###########################
#Esto para ver como es el comportamiento de los valores faltantes de las demás variables
#Aunque por el procedimiento anterior ya se sabía que todos los demás tenían de a 20 faltantes excepto la columna ANO
df_valores_faltantes_sin_CONC_MZO_UD <- df_valores_faltantes[!df_valores_faltantes$Columna %in% c("CONCRETO", "MEZ_OBRA", "USO_DOS"), ]

####################Mostarndo el gráfico de barras sin las variables excluidas####################
ggplot(df_valores_faltantes_sin_CONC_MZO_UD, aes(x = Columna, y = Valores_faltantes)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Columna", y = "Número de Valores Faltantes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#############################Conteo de edificaciones por ano######################################

# Conteo de edificaciones por año
conteo_por_ano <- aggregate(Ejercicio$NRO_EDIFIC, by = list(Ejercicio$ANO_CENSO), FUN = length)

# Renombrar las columnas
colnames(conteo_por_ano) <- c("ANO_CENSO", "Conteo")

# Crear el gráfico de barras
ggplot(conteo_por_ano, aes(x = ANO_CENSO, y = Conteo)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Año de Censo", y = "Conteo de Edificaciones en Construcción", 
       title = "Conteo de Edificaciones en Construcción por Año") +
  theme_minimal()

######## Contar los casos donde AREAUNITGA es igual a cero cuando UNIDADESGA es igual a cero#########
casos_area_cero_unidades_cero <- sum(Ejercicio$AREAUNITGA[Ejercicio$UNIDADESGA == 0] == 0)

######## Contar los casos donde UNIDADESGA es igual a cero cuando AREAUNITGA es igual a cero########
casos_unidades_cero_area_cero <- sum(Ejercicio$UNIDADESGA[Ejercicio$AREAUNITGA == 0] == 0)

#Mostrar en pantalla:
#Como era de esperarse son el mismo número

print(paste("Casos donde AREAUNITGA es igual a cero cuando UNIDADESGA es igual a cero:", casos_area_cero_unidades_cero))
print(paste("Casos donde UNIDADESGA es igual a cero cuando AREAUNITGA es igual a cero:", casos_unidades_cero_area_cero))

#######################Numero promedio de unidades de garajes cubiertos################################

#Esta conversión se realizó porque en mi caso personal las variables no eran tipo numeric, sino character
Ejercicio$UNIDADESGA <- as.numeric(Ejercicio$UNIDADESGA)
Ejercicio$PRECIOUNIG <- as.numeric(Ejercicio$PRECIOUNIG)

class(Ejercicio$UNIDADESGA)
class(Ejercicio$PRECIOUNIG)


promedio_unidadesga <- mean(Ejercicio$UNIDADESGA, na.rm = TRUE)
promedio_preciounig <- mean(Ejercicio$PRECIOUNIG, na.rm = TRUE)

# Imprimir el resultado
#Estas operaciones fueron hechas para verificar que ya fueran datos numericos operables
print(promedio_unidadesga)
print(promedio_preciounig)

# Calcular el promedio de UNIDADESGA y PRECIOUNIG por municipio
resultados_mpio <- aggregate(cbind(UNIDADESGA, PRECIOUNIG) ~ DPTO_MPIO, Ejercicio, mean, na.rm = TRUE)

# Imprimir los resultados
print(resultados_mpio)

# Calcular el promedio de UNIDADESGA y PRECIOUNIG por año
resultados_ano <- aggregate(cbind(UNIDADESGA, PRECIOUNIG) ~ ANO_CENSO, Ejercicio, mean, na.rm = TRUE)

# Imprimir los resultados
print(resultados_ano)


# Calcular el promedio de UNIDADESGA y PRECIOUNIG por municipio y año
resultados_final <- aggregate(cbind(UNIDADESGA, PRECIOUNIG) ~ DPTO_MPIO + ANO_CENSO, Ejercicio, mean)

# Imprimir los resultados
print(resultados_final)
