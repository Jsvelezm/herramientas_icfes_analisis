

#indice_v = c(NA,40,43,43,44,45,45,45,48,49,49)

#' Calcula el indice de una de las area evaluadas por el icfes
#' (instituto colombiano para la evalaucion de la educacion sup-
#' erior)
#' @param indice_v Un vector numerico con puntajes entre 0 y 100
#' para los estudiantes que hayan presentado el icfes durante el
#' periodo de analisis, debe incluir los estudiantes sin dato,
#' como un NA.
#' 
#' @return indice_f Un valor numerico, El indice global bajo la formula
#' establecida por el icfes
#' 
#' @examples 
#' # Calcular el indicador
#' indice_v = c(NA,40,43,43,44,45,45,45,48,49,49)
#' calculo_indicador(indice_v)
#' 
#' @export

calculo_indicador =  function(indice_v){
  sample_size = length(indice_v)
  # el numero de estudiantes que entran en el calculo
  # del indice es el 80 por ciento.
  n_est_ind = sample_size*0.8
  n_est_ind_f = ifelse(n_est_ind<9,9,n_est_ind)
  
  # ordenamos el indice de menor a mayor
  indice_sort = indice_v[order(indice_v,decreasing = T)][0:n_est_ind_f]
  
  # generamos un array con los numeros del 0 al 100
  indices_key = c(100:0)
  
  indices_key = as.data.frame(indices_key)
  indice = as.data.frame(table(indice_sort))
  
  
  
  #creamos una lista donde cada uno de los datos se repite la cantidad de veces
  # que esta en la muestra
  #print(indices_key)
  #print(indice)
  indices_s = merge(indices_key, indice, by.x='indices_key', by.y='indice_sort', all.x=T,sort=F)
  indices_s = indices_s[order(indices_s['indices_key'][[1]],decreasing = T),]
  indices_s[is.na(indices_s)] = 0
  
  total =  sum(indices_s['Freq'])
  
  indices_s['Frecuencia_acum'] = cumsum(indices_s['Freq'])/total
  
  
  # mf es la media de las freq acumuladas descendentes
  # sf es la varianza de las acum
  mf = mean(indices_s['Frecuencia_acum'][[1]])
  sf = var(indices_s['Frecuencia_acum'][[1]])
  
  # formula establecida por el icfes
  indice_f = mf/(1-sf)
  return(indice_f)
}

#calculo_indicador(indice_v )

