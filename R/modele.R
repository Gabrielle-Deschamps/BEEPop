#' Obtenir la taille des populations à l'année suivante dans une communauté
#'
#' @param community
#' @param K = capacité biotique du milieu
#'
#' @return la communauté avec une colonne en plus pour la taille de chaque population à l'année a+1
#' @export
#'
#' @examples
<<<<<<< HEAD
model <- function(community, K){
=======
modele <- function(community){

>>>>>>> 8a045e39d25788f775b97b963641b880399f4491
  interaction<-attributes(community)$interaction
  popt0<-current_size(community)

  R <- growth_rate(community)
  K <- maximum_capacity(community)
  somme <- interaction %*% popt0

  popevol <- (1 - somme/K) * R * popt0

  community<-append_size(community, popt0 + popevol)
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

