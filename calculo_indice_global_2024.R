# Script para leer y traer toda la información de los colegios
# calcular todos sus indicadores y generar el indice global 
# total

# lectura de las funciones
source('indice_global_icfes.R')
library(openxlsx)

# definicion de variables

# es la variable que identifica a los colegios
# puede ser el nombre o el codigo
var_colegio = 'COLE_NOMBRE_ESTABLECIMIENTO'
colegio_esp = 'INSTITUCION EDUCATIVA DISTRITAL JOSE RAIMUNDO SOJO'
columnas_puntajes = c("PUNT_INGLES", "PUNT_MATEMATICAS", 
                      "PUNT_SOCIALES_CIUDADANAS",     
                      "PUNT_C_NATURALES", "PUNT_LECTURA_CRITICA" )
annos = 'PERIODO'

# lectura de la base de datos
resultados = read.xlsx('Resultados__nicos_Saber_11_Atlantico.xlsx',sheet='Resultados__nicos_Saber_11_2024')

length(table(resultados[var_colegio][[1]]))

# calcular los indicadores de un solo colegio

col_res =  resultados[resultados[var_colegio][[1]]==colegio_esp,]
print(sum(resultados[var_colegio][[1]]==colegio_esp))
colnames(col_res)

unique_years = sort(unique(col_res[annos][[1]]))

# crear una tabla vacia
matrix_empty = matrix(ncol = length(columnas_puntajes), nrow = length(unique_years),0)

# tabla de puntajes
puntajes_t = data.frame( matrix_empty)
colnames(puntajes_t) = columnas_puntajes
rownames(puntajes_t) = unique_years

for( year in unique_years){
  for(materia in columnas_puntajes){
    puntajes_t[year,materia] = calcular_indi
  }
  
}
