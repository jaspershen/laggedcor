#' Extract Shift Time from a Lagged Correlation Object
#'
#' Retrieves the shift time(s) from a lagged correlation object, with an option to process and
#' return them as numeric values.
#'
#' @param object An object that includes a `shift_time` component, expected to be in a specific 
#'        format (string or numeric).
#' @param numeric A logical value; if `TRUE`, the function will convert `shift_time` values into
#'        numeric means, otherwise, it will return raw shift times. Defaults to `TRUE`.
#'
#' @return If `numeric` is TRUE, returns a numeric vector of the mean shift times extracted from 
#'         the object's `shift_time` attribute. If `numeric` is FALSE, returns the raw shift time
#'         values as they are stored in the object.
#'
#' @details
#' The function first retrieves the `shift_time` component of the input object. If the `numeric`
#' parameter is set to `TRUE`, the function proceeds to convert the shift time(s) to numeric
#' format. It removes parentheses, splits the string at commas, takes the first element, converts
#' it to numeric and calculates the mean if there are multiple values. This is all done using
#' a chain of `lapply` and `stringr` functions. If `numeric` is FALSE, the raw shift time data is
#' returned as is.
#'
#' @note
#' The `object` is expected to have an accessible `shift_time` attribute. The function relies on
#' the `stringr` package for string manipulation.
#' @export
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







#' Extract All Correlations from an Object
#'
#' Retrieves all correlation coefficients from a given object with predefined structure,
#' associating them with their respective shift times.
#'
#' @param object An object that includes `all_cor` and `shift_time` attributes.
#'
#' @return A named vector where each name is the corresponding shift time and each value is the
#'         correlation coefficient associated with that shift time.
#'
#' @details
#' The function accesses the `all_cor` component from the input object, which is expected to contain
#' a series of correlation coefficients. It then retrieves the `shift_time` component from the same
#' object and uses it to name the elements in the `all_cor` vector, effectively binding each
#' correlation coefficient with its respective shift time. The resulting named vector is then
#' returned.
#'
#' @note
#' The `object` is assumed to have an accessible `all_cor` and `shift_time` attributes. This
#' function is useful for subsequent analysis where the relationship between correlation
#' coefficients and their corresponding shift times is needed.
#'
#' @export
#'
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
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





#' Extract All Correlation P-Values from an Object
#'
#' Retrieves all correlation p-values from a given object with a predefined structure,
#' associating them with their respective shift times.
#'
#' @param object An object that includes `all_cor_p` and `shift_time` attributes.
#'
#' @return A named vector where each name is the corresponding shift time and each value is the
#'         p-value for the correlation coefficient associated with that shift time.
#'
#' @details
#' The function accesses the `all_cor_p` component from the input object, which is expected to contain
#' a series of p-values for correlation tests. It then retrieves the `shift_time` component from the same
#' object and uses it to name the elements in the `all_cor_p` vector, effectively binding each
#' p-value with its corresponding shift time. The resulting named vector is then
#' returned.
#'
#' @note
#' The `object` is presumed to have an accessible `all_cor_p` and `shift_time` attributes. This
#' function is crucial for analyses where the significance of the correlation coefficients
#' needs to be assessed in relation to their shift times.
#'
#' @export
#'
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



#' Extract Maximum Correlation Value
#'
#' Retrieves the maximum correlation value from the `all_cor` attribute of the given object
#' and provides the associated shift time as the name of the value.
#'
#' @param object An object that includes `all_cor` and `shift_time` attributes.
#'
#' @return A single named element. The name is the shift time associated with the maximum correlation 
#'         value in the `all_cor` attribute of the object.
#'
#' @details
#' The function selects the maximum value from the `all_cor` attribute, which is a vector of correlation
#' coefficients. It then matches this maximum value with its corresponding shift time from the
#' `shift_time` attribute. This shift time becomes the name of the returned value, allowing for easy
#' identification of when the maximum correlation occurs.
#'
#' @note
#' The `object` is expected to have an `all_cor` attribute that contains correlation values and
#' a `shift_time` attribute that contains the corresponding shift times. This function simplifies
#' the process of identifying the point in time with the highest correlation coefficient.
#'
#' @export
#'
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


#' Extract Global Correlation Value
#'
#' Retrieves a specific correlation value indexed by `which_global_idx` from the `all_cor` attribute 
#' of the given object, with the associated shift time as its name.
#'
#' @param object An object that includes `all_cor`, `shift_time`, and `which_global_idx` attributes.
#'
#' @return A single named element from the `all_cor` attribute. The name is the shift time associated 
#'         with this correlation value, indexed by `which_global_idx`.
#'
#' @details
#' This function is specifically designed to extract a correlation value that is designated as
#' "global" within the context of the object's dataset. The `which_global_idx` is expected to be
#' an attribute within the object that specifies the index of this global correlation value in
#' the `all_cor` vector. The corresponding shift time is extracted from the `shift_time` attribute
#' and assigned as the name to the returned value.
#'
#' @note
#' The function assumes that the object has attributes `all_cor` and `shift_time` with the same length,
#' and `which_global_idx` that points to a valid index within them. The purpose of `which_global_idx`
#' should be documented elsewhere, detailing why this particular index is significant.
#'
#' @export
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @examples
#' data("object", package = "laggedcor")
#' extract_global_cor(object = object)

extract_global_cor <-
  function(object) {
    shift_time =
      object@shift_time
    
    all_cor = object@all_cor
    names(all_cor) = shift_time
    all_cor[object@which_global_idx]
  }
