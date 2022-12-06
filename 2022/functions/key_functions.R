#' 
#' Key functions
#' 



# GET DATA ----------------------------------------------------------------

f_getdata <- function(day, path = "2022/data/", pattern = NULL, full.names = TRUE, return_paths = FALSE, read_lines = FALSE) {
  
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
    
    # set names to filepath
    (\(.) setNames(object = ., nm = tools::file_path_sans_ext(basename(.))))() |> 
    
    # assign variable
    force() -> files
  
  # return result
  if (return_paths == TRUE) {
    
    files
    
  } else if (read_lines == TRUE) {
    
    # read data as lines
    lapply(
      X = files,
      FUN = readr::read_lines
    )
    
  } else {
    
    # read data as data.table
    lapply(
      X = files,
      FUN = data.table::fread, header = FALSE
    )
    
  } |> 
    
    # return result
    (\(.) return(.))()
  
}

