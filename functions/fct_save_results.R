#' fct_save_results
#'
#' @description A wrapper to save an object to rds
#'
#' @param subDir The folder
#' @param slar_taskid The taskid from slurm
#' @param object The object to save
#'
#' @return Save the object and return NULL
fct_save_results <- function(subDir, slar_taskid, object){
  if (!file.exists(subDir)) dir.create(subDir)
  filename_r <- paste0(subDir, "/result_job_", slar_taskid, "_.rds")
  saveRDS(object, file = filename_r)
  return()
}