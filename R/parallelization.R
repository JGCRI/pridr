
create_cores <- function(num_cores=NULL){
  if(is.null(num_cores)){

  n.cores <- parallel::detectCores() - 1}else{

    n.cores = num_cores

  if(num_cores > parallel::detectCores()){

    stop("Number of cores you have specified are higher than those actually available")
  }}

  my.cluster <- parallel::makeCluster(
    n.cores,
    type = "PSOCK")

  clusterExport( my.cluster, c('compute_lognormal_country', 'compute_lognormal_dist','erfinv','%>%','rename','mutate','distinct','select','ungroup'
                       ,'group_by','arrange','if_else'))
  return(my.cluster)
}
