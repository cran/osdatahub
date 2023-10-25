
#' Set credentials for OS Data Hub
#'
#' In order to use the Ordnance Survey Data Hub a valid API key is required.
#' @param apikey (character) Required project API key.
#'
#' @details Stores the user provided character string in an environment variable
#'   named \code{OS_API_KEY}. No validation of the key is applied when storing.
#'   To obtain a key go to \url{https://osdatahub.os.uk/}.
#'
#' @returns (Invisibly) A logical value from \code{Sys.setenv} whether an
#'   environment variable was set.
#'
#' @examples
#' set_os_key('my-api-key')
#'
#' @rdname set_os_key
#' @export
set_os_key <- function(apikey){
  if(missing(apikey)){
    stop('Please provide an OS Data Hub API key.', call. = FALSE)
  }

  if(is.null(apikey)){
    stop('Please provide an OS Data Hub API key.', call. = FALSE)
  }

  if(apikey == ''){
    stop('Please provide an OS Data Hub API key.', call. = FALSE)
  }

  if(length(apikey) > 1L){
    stop('Please provide only one OS Data Hub API key.', call = FALSE)
  }

  ret <- Sys.setenv(OS_API_KEY = apikey)

  invisible(ret)
}


#' Return the OS API key stored in the environment
#'
#' @details Be careful not to reveal secrets including API keys. This function
#'   may print the API key to the console. It is used internally by the
#'   \code{osdatahub} query functions.
#'
#' @returns If an environment variable named \code{OS_API_KEY} is present, the
#'   character string for the variable is returned.
#'
#' @examplesIf has_os_key()
#' my_api_key <- get_os_key()
#'
#' @rdname set_os_key
#' @export
get_os_key <- function(){
  oskey <- Sys.getenv('OS_API_KEY')

  if (identical(oskey, "")) {
    stop('Please set the environment variable to your OS Data Hub API key.
         Use `set_os_key` or `Sys.setenv(OS_API_KEY=)`',
         call. = FALSE)
  }

  return(oskey)
}


#' Check if an OS API has been stored in the environment
#'
#' @details Primarily this is used internally to control when examples are
#'   executed.
#'
#' @returns If an environment variable named \code{OS_API_KEY} is present, then
#'   \code{TRUE}, else this function returns \code{FALSE}.
#'
#' @examples
#' has_os_key()
#'
#' @rdname set_os_key
#' @export
has_os_key <- function(){
  oskey <- Sys.getenv('OS_API_KEY')

  present <- FALSE

  if(!is.null(oskey) & !identical(oskey, "")){
    present <- TRUE
  }

  return(present)
}
