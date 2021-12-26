#' @title extract_shift_time
#' @description extract_shift_time
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object a lagged_scatter_result class object.
#' @param numeric shift time as numeric or character
#' @export
#' @return A vector.
#' @examples
#' data("object", package = "laggedcor")
#' extract_shift_time(object = object, numeric = TRUE)
#' extract_shift_time(object = object, numeric = FALSE)

extract_shift_time =
  function(object,
           numeric = TRUE) {
    shift_time =
      object@shift_time
    
    if (numeric) {
      shift_time =
        shift_time %>%
        lapply(function(x) {
          x %>%
            stringr::str_replace("\\(", "") %>%
            stringr::str_replace("\\]", "") %>%
            stringr::str_split(",") %>%
            `[[`(1) %>%
            as.numeric() %>%
            mean()
        }) %>%
        unlist()
      return(shift_time)
    } else{
      return(shift_time)
    }
  }







#' @title extract_all_cor
#' @description extract_all_cor
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object a lagged_scatter_result class object.
#' @export
#' @return A vector.
#' @examples
#' data("object", package = "laggedcor")
#' extract_all_cor(object = object)

extract_all_cor =
  function(object) {
    shift_time =
      object@shift_time
    
    all_cor = object@all_cor
    names(all_cor) = shift_time
    return(all_cor)
  }






#' @title extract_all_cor_p
#' @description extract_all_cor_p
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object a lagged_scatter_result class object.
#' @export
#' @return A vector.
#' @examples
#' data("object", package = "laggedcor")
#' extract_all_cor_p(object = object)

extract_all_cor_p =
  function(object) {
    shift_time =
      object@shift_time
    
    all_cor_p = object@all_cor_p
    names(all_cor_p) = shift_time
    return(all_cor_p)
  }


#' @title extract_max_cor
#' @description extract_max_cor
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object a lagged_scatter_result class object.
#' @export
#' @return A vector.
#' @examples
#' data("object", package = "laggedcor")
#' extract_max_cor(object = object)

extract_max_cor =
  function(object) {
    shift_time =
      object@shift_time
    
    all_cor = object@all_cor
    names(all_cor) = shift_time
    all_cor[which.max(all_cor)]
  }


#' @title extract_global_cor
#' @description extract_global_cor
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object a lagged_scatter_result class object.
#' @export
#' @return A vector.
#' @examples
#' data("object", package = "laggedcor")
#' extract_global_cor(object = object)

extract_global_cor =
  function(object) {
    shift_time =
      object@shift_time
    
    all_cor = object@all_cor
    names(all_cor) = shift_time
    all_cor[object@which_global_idx]
  }
