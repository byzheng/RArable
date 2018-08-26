# RArable
A R package for Arable API

## Usage

```r
library(RArable)
email <- 'your-email@company.com'
password <- 'your-password'
tenant <- 'your-organisation'
connect(email = email, password = password, tenant = tenant)
devices <- get_devices()

if (length(devices) > 0) {
    devices <- devices[[1]]$name
    start <- as.POSIXct("2018-04-25 00:00:00")
    end <- Sys.time()
    observations <- get_data(devices = devices, start = start, end = end)
}
```
