

#' parallelization function to generate multiple cores from the CPU.
#'
#' Note that we think this works only for Windows.To make it work for MAC, just change the `makeCluster()` type argument
#' in the function to FORK.
#' @export
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

  clusterExport(my.cluster,c('compute_lognormal_country', 'compute_lognormal_dist','erfinv','%>%','rename','mutate','distinct','select','ungroup'
                       ,'group_by','arrange','if_else','adjust_negative_predicted_features','get_deciles_from_components',
                       'gather','spread','pc_center_sd','pc_loading_matrix','left_join','compute_PC_model_components','compute_palma_ratio','as.name','PC_model','filter'),envir=globalenv())

  clusterEvalQ(cl = my.cluster,expr = library(pridr))

  return(my.cluster)
}
