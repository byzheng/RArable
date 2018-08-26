# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:38 PM Saturday, 25 August 2018
# * Copyright: AS IS

# A package wide variable to store the access token
.state <- new.env(parent = emptyenv())


#' Check whether Arable is connected
#'
#' @return TRUE is connected.
.check_connection <- function() {
    if (is.null(.state$authorization) || nchar(.state$authorization) == 0) {
        stop("Authentication exception: not connected.")
    }
    return(TRUE)
}

#' Format data and time for Arable
#'
#' @param dt Date and time format
#' @param tz timezone
#'
#' @return A string of formatted data and tme
.format_datetime <- function(dt, tz = 'GMT') {
    if ('Date' %in% class(dt)) {
        dt <- as.POSIXct(format(dt), tz = tz)
    }
    r <- format(dt, format = "%Y-%m-%dT%H:%M:%SZ", tz = 'GMT')
    r
}


#' Connect to the Arable API
#'
#' @param email The email address
#' @param password The password
#' @param tenant The company name
#' @param apikey The API key (optional)
#'
#' @return TRUE if connected
#' @export
#'
#' @examples
connect <- function(email = Sys.getenv('ARABLE_EMAIL'),
                    password = Sys.getenv('ARABLE_PASSWORD'),
                    tenant = Sys.getenv('ARALE_TENANT'),
                    apikey = Sys.getenv('ARABLE_APIKEY')) {

    if (!is.null(apikey) && nchar(apikey) > 0) {
        ARABLE_OPTIONS(apikey = apikey)
        .state$authorization <- apikey
        return(TRUE)
    } else if (!is.null(email) && nchar(email) > 0 &&
               !is.null(password) && nchar(password) > 0 &&
               !is.null(tenant) && nchar(tenant) > 0) {
        # Test the connection
        url <- sprintf("%s/auth/user/%s", ARABLE_OPTIONS('base_url'), tenant)
        # utf-8 encode/decode for python3 support

        token <- RCurl::base64Encode(paste(email, password, sep = ':'))
        request <- httr::POST(url, httr::add_headers(Authorization = paste0('Basic ', token)))

        status <- httr::status_code(request)

        if (status != 200) {
            stop(httr::http_status(request)$message)
        }

        response <- httr::content(request)
        header_authorization <- paste0('Bearer ', response$access_token)
        .state$authorization <- header_authorization
        ARABLE_OPTIONS(email = email)
        ARABLE_OPTIONS(password = password)
        ARABLE_OPTIONS(tenant = tenant)
        return(TRUE)
    } else {
        stop('Missing parameter; connect requires email, password, and tenant.')
    }
    return(TRUE)
}

#' List information of all devices in Arable
#'
#' @param device_id The ID of device
#' @param name The name of device
#'
#' @return A list of device information
#' @export
get_devices <- function(device_id = NULL, name = NULL) {
    .check_connection()

    url <- paste0(ARABLE_OPTIONS('base_url'), "/devices")
    if (!is.null(device_id)) {
        url <- paste0(url, "/", device_id)
    } else if (!is.null(name)) {
        url <- paste0(url, "?name=", name)
    }
    request <- httr::GET(url, httr::add_headers(Authorization = .state$authorization))

    status <- httr::status_code(request)

    if (status != 200) {
        stop(httr::http_status(request)$message)
    }

    response <- httr::content(request)
    response
}

#' Get data from Arable for devices
#'
#' @param devices A character vector of device names
#' @param start The start timestamp
#' @param end The end timestamp
#' @param location optional; id of a location to retrieve data for; devices ignored if this is present
#' @param format optional; use format=csv to get csv-formatted data; otherwise data is returned as json
#' @param order  optional; "time" (time ascending) or "-time" (time descending)
#' @param measure optional; "calibrated", "hourly", or "daily"
#' @param select optional; "all", "spectrometer", "microclimate", or "radiometer"
#' @param limit  optional; maximum number of data points to return; defaults to 1000
#'
#' @return A data frame of data for devices
#' @export
get_data <- function(devices, start, end,
                  location = NULL,
                  format = 'csv',
                  order = "time",
                  measure = 'hourly',
                  select = 'all',
                  limit = 1000) {
    url <- paste0(ARABLE_OPTIONS('base_url'), "/data")

    query <- list()
    query$devices <- paste0("[", paste(paste0("'", devices, "'"), collapse = ','), "]")
    query$start <- .format_datetime(start)
    query$end <- .format_datetime(end)
    if (!is.null(limit)) {
        query$limit <- limit
    }
    if (!is.null(select)) {
        query$select <- select
    }
    if (!is.null(measure)) {
        query$measure <- measure
    }
    if (!is.null(order)) {
        query$order <- order
    }
    if (!is.null(format)) {
        query$format <- format
    }
    if (!is.null(location)) {
        query$location <- location
    }

    result <- list()
    k <- 0
    repeat {
        request <- httr::GET(url, httr::add_headers(Authorization = .state$authorization),
                             query = query)
        status <- httr::status_code(request)
        if (status != 200) {
            stop(httr::http_status(request)$message)
        }
        response <- httr::content(request)
        if (nrow(response) == 0) {
            break
        }

        k <- k + 1
        result[[k]] <- response
        if (nrow(response) < query$limit) {
            break
        } else {
            query$start <- .format_datetime(max(response$time) + 1)
        }

    }
    r <- dplyr::bind_rows(result)
    r
}
