#' Obtenir la taille des populations à l'année suivante dans une communauté
#'
#' @param community
#' @param K = capacité biotique du milieu
#'
#' @return la communauté avec une colonne en plus pour la taille de chaque population à l'année a+1
#' @export
#'
#' @examples
model <- function(community, K){
  interaction<-attributes(community)$interaction
  popt0<-as.integer(current_size(community))
  popevol=c()
  for (i in 1:ncol(community)){
    somme=0
    for (j in 1:ncol(community)){
      somme=somme+interaction[i,j]*popt0[j]
    }
    R=attributes(community[[i]])$rate
    popevol=c(popevol, popt0[i]*R*(1-(somme)/K))
  }
  community<-append_size(community, as.integer(popevol))
  community
}

#' Obtenir la taille des populations dans une communauté après x années
#'
#' @param community
#' @param K = capacité biotique du milieu
#' @param x = nombre d'année qu'on veut prédire
#'
#' @return la communauté avec x colonnes en plus pour la taille de chaque population à l'année a+x
#' @export
#'
#' @examples
model.multi.years <- function(community, K, x){
  for (i in 1:x){
    community<-model(community,K)
  }
  community
}

