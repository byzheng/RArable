# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:38 PM Saturday, 25 August 2018
# * Copyright: AS IS



# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
ARABLE_OPTIONS <- settings::options_manager(base_url = 'https://api-user.arable.cloud/api/v1',
                                  email = Sys.getenv('ARABLE_EMAIL'),
                                  password = Sys.getenv('ARABLE_PASSWORD'),
                                  tenant = Sys.getenv('ARALE_TENANT'),
                                  apikey = Sys.getenv('ARABLE_APIKEY'))


#' Set or get options for my package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{base_url}}{ The base url for Arable API}
#'  \item{\code{email}}{ The email for Arable}
#'  \item{\code{password}}{ The password for Arable}
#'  \item{\code{tenant}}{ The tenant for Arable}
#'  \item{\code{apikey}}{ The apikey for Arable}
#' }
#'
#' @export
arable_options <- function(...) {
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    ARABLE_OPTIONS(...)
}

#' Reset global options for pkg
#'
#' @export
arable_reset <- function() {
    settings::reset(arable_OPTIONS)
}
