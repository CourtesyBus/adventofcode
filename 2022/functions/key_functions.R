#' 
#' Key functions
#' 



# GET DATA ----------------------------------------------------------------

f_getdata <- function(day, path = "2022/data/", pattern = NULL, full.names = TRUE) {
  
  # check numeric value given
  stopifnot(is.numeric(day))
  
  # replace NULL pattern if not otherwise specified
  if (is.null(pattern)) {pattern <- paste0("d", formatC(x = day, width = 2, format = "d", flag = "0"), ".*\\.txt")}
  
  # get files in directory
  dir(
    path = path,
    pattern = pattern,
    full.names = full.names
  ) |> 
    
    # set names
    (\(.) setNames(object = ., nm = tools::file_path_sans_ext(basename(.))))() |>
    
    # read data
    lapply(FUN = data.table::fread, header = FALSE) |> 
    
    # assign to variable
    force() -> lst
  
  
  # return list
  return(lst)
  
}


