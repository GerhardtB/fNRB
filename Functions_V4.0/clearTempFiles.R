#' Delete unused raster temp files
#'
#' The raster library often saves temp files to the C: drive, and standard
#' cleanup methods can fail for lengthy parallel processing jobs, causing the
#' disk to fill up. Use this function to prevent that problem.
#'
#' @param tempdir Character: full file path of the folder where temp files are
#'   stored. Will be searched recursively.
#' @param loop Logical: should the function keep monitoring and deleting temp
#'   files indefinitely until the script is manually terminated?

clearTempFiles<-function(tempdir=tempdir(),loop=FALSE){
  run=T
  while(run){
    files<-list.files(tempdir,recursive=TRUE,full.names=TRUE)
    files<-files[substr(files,nchar(files)-2,nchar(files)) %in% c("grd","gri")]
    for(f in files) try(file.remove(f))
    run<-loop
    if(run) Sys.sleep(5*60)
  }
}
