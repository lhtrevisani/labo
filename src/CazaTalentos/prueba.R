## borro el espacio de trabajo
rm(list=ls())

#defino una semilla
set.seed(102191)

#calcula cuantos encestes logra un jugador con indice de enceste prob
#que hace qyt tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <- 0.7
peloton    <- ( 501:599 ) / 1000
jugadores  <- c( peloton, mejor ) #intencionalmente el mejor esta al final

names(jugadores) <- c(paste0('jugador ',1:99), 'mejor')


## podría ir in crescendo el percentil a eliminar.

mejor_ganador = 0
mejor_eliminado = 0
tiros_final = c()
mejor_rank = c()

for( i in 1:100 ){

  tiros = data.frame(prob = jugadores, eliminado = rep(0, 100))
  cant_tiros_ronda = 25
  rondas = 5
  percentil_a_eliminar = 0.95
  tiros_acumulados = 0

  for( i in 1:rondas ){
    
    aciertos = mapply(ftirar, tiros[tiros$eliminado == 0, 'prob'], cant_tiros_ronda)      # aciertos de cada jugador
    pos_i = rank(-aciertos, ties.method= "first")                                         # "posición" en esa ronda
  
    tiros[tiros$eliminado == 0, paste0("aciertos_tiro_", i)] = aciertos
    tiros[tiros$eliminado == 0, paste0("pos_tiro_", i)] = pos_i
  
    if (i == 1){
      tiros$aciertos_acumulados = aciertos
      tiros$mediana_acumuada = pos_i
    } else {
      tiros$aciertos_acumulados = apply(tiros[,grep("aciertos", colnames(tiros))], 1, sum)
      tiros$mediana_acumuada = apply(tiros[,grep("pos", colnames(tiros))], 1, mean)
    }
  
    tiros_acumulados = tiros_acumulados + cant_tiros_ronda * (100 - sum(tiros$eliminado))
    tiros$eliminado <- ifelse(rank(tiros$mediana_acumuada, ties.method = "first") > (100 - sum(tiros$eliminado)) * percentil_a_eliminar, 1, tiros$eliminado)
    
    percentil_a_eliminar = percentil_a_eliminar - 0.05
    
  }
  
  if(row.names(tiros[which.min(tiros[,6]),]) == "mejor")  mejor_ganador  <- mejor_ganador + 1
  tiros_final = c(tiros_final, tiros_acumulados)
  if(tiros['mejor', 'eliminado'] == 1) mejor_eliminado <- mejor_eliminado + 1
  mejor_rank = c(mejor_rank, rank(tiros$mediana_acumuada, ties.method = "first")[100])
  

}

mejor_ganador/1000
mejor_ganador_b/1000
mejor_eliminado/1000
sum(tiros$eliminado)
tiros_acumulados
mejor_rank
mejor_rank_t

######################################################################################

## prob de eliminar al mejor si tiran n veces (y elimino al x% peor)
mejor_eliminado = 0
tiros = data.frame(jugador = names(jugadores))

for( i in 1:10000 ){
  aciertos = mapply(ftirar, jugadores, 50)
  pos_i = rank(-aciertos, ties.method= "first")
  if(pos_i[100] > 90)  mejor_eliminado  <- mejor_eliminado + 1
  
}
mejor_eliminado/10000

######################################################################################

mejor_ganador = 0
mejor_ganador_b = 0
mejor_eliminado = 0
mejor_rank = c()
mejor_rank_t = c()

for( i in 1:1000){
  
  tiros = data.frame(prob = jugadores, eliminado = rep(0, 100))
  cant_tiros_ronda = c(25, 25, 25, 25, 25, 25, 25, 25)
  tiros_acumulados = 0
  jugadores_a_eliminar = c(0, 0, 0, 50, 25, 10, 5, 0)#, 15, 20, 20, 10, 20)
  rondas = length(jugadores_a_eliminar)
  
  for( i in 1:rondas ){
    
    aciertos = mapply(ftirar, tiros[tiros$eliminado == 0, 'prob'], cant_tiros_ronda[i])      # aciertos de cada jugador
    pos_i = rank(-aciertos, ties.method= "first")                                         # "posición" en esa ronda
    percent_rank = (pos_i - 1) / (length(aciertos) - 1)
    
    tiros[tiros$eliminado == 0, paste0("aciertos_tiro_", i)] = aciertos
    tiros[tiros$eliminado == 0, paste0("pos_tiro_", i)] = percent_rank
    
    if (i == 1){
      tiros$aciertos_acumulados = aciertos
      tiros$mediana_acumuada = percent_rank
    } else {
      tiros$aciertos_acumulados = apply(tiros[,grep("aciertos", colnames(tiros))], 1, sum)
      tiros$mediana_acumuada = apply(tiros[,grep("pos", colnames(tiros))], 1, mean)
    }
    
    tiros_acumulados = tiros_acumulados + cant_tiros_ronda[i] * length(aciertos)
    tiros$eliminado <- ifelse(rank(tiros$mediana_acumuada, ties.method = "first") > (100 - sum(tiros$eliminado)) - jugadores_a_eliminar[i], 1, tiros$eliminado)
    
  }
  
  if(row.names(tiros[which.min(tiros[,6]),]) == "mejor")  mejor_ganador  <- mejor_ganador + 1
  if(row.names(tiros[which.max(tiros[,5]),]) == "mejor")  mejor_ganador_b  <- mejor_ganador_b + 1
  if(tiros['mejor', 'eliminado'] == 1) mejor_eliminado <- mejor_eliminado + 1
  mejor_rank = c(mejor_rank, rank(tiros$mediana_acumuada, ties.method = "first")[100])
  mejor_rank_t = c(mejor_rank, rank(tiros$aciertos_acumulados, ties.method = "first")[100])
  
  
}


mejor_ganador/1000
mejor_ganador_b/1000
mejor_eliminado/1000
sum(tiros$eliminado)
tiros_acumulados
prop.table(table(mejor_rank))



############################################################################################################################################


mejor_ganador = 0
mejor_ganador_b = 0
mejor_eliminado = 0
mejor_rank = c()
mejor_rank_t = c()

for( j in 1:10000){
  
  tiros = data.frame(prob = jugadores, eliminado = rep(0, 100))
  cant_tiros_ronda = c(25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 50, 100, 100)
  tiros_acumulados = 0
  jugadores_a_eliminar = c(5, 10, 15, 20, 5, 10, 10, 5, 5, 5, 5, 1, 1, 1, 0)
  rondas = length(jugadores_a_eliminar)
  
  for( i in 1:rondas ){
    
    aciertos = mapply(ftirar, tiros[tiros$eliminado == 0, 'prob'], cant_tiros_ronda[i])      # aciertos de cada jugador
    pos_i = rank(-aciertos, ties.method= "first")                                         # "posición" en esa ronda
    percent_rank = (pos_i - 1) / (length(aciertos) - 1)
    
    tiros[tiros$eliminado == 0, paste0("aciertos_tiro_", i)] = aciertos
    tiros[tiros$eliminado == 0, paste0("pos_tiro_", i)] = percent_rank
    
    if (i == 1){
      tiros$aciertos_acumulados = aciertos
      tiros$mediana_acumuada = percent_rank
    } else {
      tiros$aciertos_acumulados = apply(tiros[,grep("aciertos_tiro", colnames(tiros))], 1, sum)
      tiros$mediana_acumuada = apply(tiros[,grep("pos", colnames(tiros))], 1, mean)
    }
    
    tiros_acumulados = tiros_acumulados + cant_tiros_ronda[i] * length(aciertos)
    tiros$eliminado <- ifelse(rank(-tiros$aciertos_acumulados, ties.method = "first") > (100 - sum(tiros$eliminado)) - jugadores_a_eliminar[i], 1, tiros$eliminado)
    
  }
  
  if(row.names(tiros[which.min(tiros[,6]),]) == "mejor")  mejor_ganador  <- mejor_ganador + 1
  if(row.names(tiros[which.max(tiros[,5]),]) == "mejor")  mejor_ganador_b  <- mejor_ganador_b + 1
  if(tiros['mejor', 'eliminado'] == 1) mejor_eliminado <- mejor_eliminado + 1
  mejor_rank = c(mejor_rank, rank(tiros$mediana_acumuada, ties.method = "first")[100])
  mejor_rank_t = c(mejor_rank_t, rank(-tiros$aciertos_acumulados, ties.method = "first")[100])
  
  
}

mejor_ganador_b/10000
mejor_eliminado/10000
sum(tiros$eliminado)
tiros_acumulados
prop.table(table(mejor_rank_t))
cumsum(prop.table(table(mejor_rank_t)))



#podría hacer algunas pocas rondas como hice antes (eliminando por posición. y entre las pocas muestras que quedan, elegir al que más veces haya metido más)