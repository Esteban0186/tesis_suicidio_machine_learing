inercia_intraclase <- function(datos, grupos) {
  # Aseguramos que los datos sean matriz numérica para las operaciones matemáticas
  datos <- as.matrix(datos)
  
  # Combina los datos y los grupos
  tabla <- as.data.frame(cbind(datos, grupos = grupos))
  
  # IMPORTANTE: Ordenamos los IDs para asegurar que el índice de la lista 
  # coincida con el número del grupo (1, 2, 3...)
  ids_grupos <- sort(unique(grupos))
  
  # Calcula los centroides
  centroides <- lapply(ids_grupos, function(id) {
    # Filtramos usando la columna 'grupos'
    subconjunto <- tabla[tabla$grupos == id, 1:(ncol(tabla)-1), drop=FALSE]
    colMeans(subconjunto) # colMeans es más rápido que apply(..., mean)
  })
  
  # Calcula la suma de las distancias al cuadrado
  suma_cuadrados <- sum(sapply(ids_grupos, function(id_grupo) {
    # Extraemos datos del grupo
    subconjunto <- tabla[tabla$grupos == id_grupo, 1:(ncol(tabla)-1), drop=FALSE]
    subconjunto <- as.matrix(subconjunto) # sweep necesita matriz
    
    # Como ordenamos ids_grupos, podemos usar id_grupo como índice seguro
    centroide_actual <- centroides[[id_grupo]]
    
    # Resta el centroide a cada punto (sweep) y suma cuadrados
    # MARGIN=2 significa operar por columnas
    sum(sweep(subconjunto, 2, centroide_actual, "-")^2)
  }))
  
  return(suma_cuadrados)
}