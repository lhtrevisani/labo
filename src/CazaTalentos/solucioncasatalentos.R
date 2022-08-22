## borro el espacio de trabajo
rm(list=ls())

#defino una semilla
set.seed(412414)

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

######## ESTRATEGIA  ############

# la idea es ir eliminando en cada ronda a los jugadores con menor cantidad de tiros encestados acumulados.
# se comienza con rondas de pocos tiros, y cuando ya quedan menos, se los hace tirar más veces.

mejor_ganador = 0

for( j in 1:10000){                                                                         # corro 10000 veces el problema
  
  tiros = data.frame(prob = jugadores, eliminado = rep(0, 100))                             # creo un dataframe para ir guardando el resultado de las corridas
  cant_tiros_ronda = c(25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 50, 300)        # cantidad de tiros a realizar por cada participante
  tiros_acumulados = 0                                                                      # cuento la cantidad de tiros acumulados
  jugadores_a_eliminar = c(5, 10, 15, 20, 5, 10, 10, 5, 5, 5, 5, 1, 1, 0)                # jugadores a eliminar al final de cada ronda
  rondas = length(jugadores_a_eliminar)                                                     # cantidad de rondas
  
  for( i in 1:rondas ){
    
    aciertos = mapply(ftirar, tiros[tiros$eliminado == 0, 'prob'], cant_tiros_ronda[i])      # aciertos de cada jugador
    tiros[tiros$eliminado == 0, paste0("aciertos_tiro_", i)] = aciertos
    
    if (i == 1){
      tiros$aciertos_acumulados = aciertos
    } else {
      tiros$aciertos_acumulados = apply(tiros[,grep("aciertos_tiro", colnames(tiros))], 1, sum)
    }
    
    tiros_acumulados = tiros_acumulados + cant_tiros_ronda[i] * length(aciertos)
    tiros$eliminado <- ifelse(rank(-tiros$aciertos_acumulados, ties.method = "first") > (100 - sum(tiros$eliminado)) - jugadores_a_eliminar[i], 1, tiros$eliminado)
    
  }
  
  if(row.names(tiros[which.max(tiros[,4]),]) == "mejor")  mejor_ganador  <- mejor_ganador + 1
  #if(tiros['mejor', 'eliminado'] == 1) mejor_eliminado <- mejor_eliminado + 1
  
}

mejor_ganador/10000
tiros_acumulados

