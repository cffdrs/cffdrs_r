#' Get Values Block Stackfix
#' 
#' This function is originally from spatial.tools package which
#' has been archived. We do not accept responsibility for writing
#' this function and all IP belongs to the previous maintainers
#' of that package. If that pacakage is revived we will revert
#' back to it.
#' 
#' @seealso \code{\link{spatial.tools}}
#' 
#' @noRd
#' 

.getValuesBlock_stackfix <-
function (x, row = 1, nrows = 1, col = 1, ncols = (ncol(x) - 
                                                     col + 1), lyrs = (1:nlayers(x))) 
{
  single_filename <- NULL
  if (class(x) == "RasterStack") {
    all_filenames <- sapply(x@layers, function(X) {
      filename(X)
    })
    inMemory_layers <- (1:nlayers(x))[sapply(x@layers, function(X) {
      inMemory(X)
    })]
    unique_filenames <- unique(all_filenames)
    unique_getValuesBlock <- foreach(single_filename = unique_filenames, 
                                     .packages = c("raster")) %dopar% {
                                       if (single_filename != "") 
                                         getValuesBlock(brick(single_filename), row, nrows, 
                                                        col, ncols)
                                       else getValuesBlock(stack(x, bands = inMemory_layers), 
                                                           row, nrows, col, ncols)
                                     }
    band_layers <- sapply(x@layers, function(x) x@data@band)
    nlyrs_out <- length(lyrs)
    out_matrix <- matrix(nrow = (nrows * ncols), ncol = nlyrs_out)
    for (i in 1:nlyrs_out) {
      current_layer <- lyrs[i]
      file_index <- which(all_filenames[i] == unique_filenames)
      out_matrix[, i] <- unique_getValuesBlock[[file_index]][, 
                                                             band_layers[i]]
    }
    return(out_matrix)
  }
  else {
    if (class(x) == "RasterLayer" || nlayers(x) == 
        1) 
      return(getValuesBlock(x, row = row, nrows = nrows, 
                            col = col, ncols = ncols))
    else return(getValuesBlock(x, row = row, nrows = nrows, 
                               col = col, ncols = ncols, lyrs = lyrs))
  }
}
