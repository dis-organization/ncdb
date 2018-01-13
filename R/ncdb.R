#' Driver for ncdb virtual database.
#'
#' @keywords internal
#' @export
#' @import DBI
#' @import methods
setClass("ncdbDriver", contains = "DBIDriver")


#' @export
#' @rdname ncdb-class
setMethod("dbUnloadDriver", "ncdbDriver", function(drv, ...) {
  TRUE
})


setMethod("show", "ncdbDriver", function(object) {
  cat("<ncdbDriver>\n")
})

#' @export
ncdb <- function() {
  new("ncdbDriver")
}

#' ncdb connection class.
#'
#' @export
#' @keywords internal
setClass("ncdbConnection",
         contains = "DBIConnection",
         slots = list(
           host = "character",
           username = "character",
           tidync = "tidync"
         )
)


#' @param drv An object created by \code{ncdb()}
#' @rdname ncdb
#' @export
#' @examples
#' \dontrun{
#' l3 <- "S20092742009304.L3m_MO_CHL_chlor_a_9km.nc"
#' l3path <- system.file("extdata", "oceandata", l3, package = "tidync")
#'
#' db <- dbConnect(Rncdb::ncdb(), host = l3path)
#' #dbWriteTable(db, "mtcars", mtcars)
#' #dbGetQuery(db, "SELECT * FROM mtcars WHERE cyl == 4")
#' }
setMethod("dbConnect", "ncdbDriver", function(drv, host = "",  ...) {
  # ...
  if (nchar(host) < 1L) stop("host cannot be empty")
  tnc <- try(tidync::tidync(host), silent = TRUE)
  if (inherits(tnc, "try-error")) {
    message(sprintf("cannot connect to host, is this a NetCDF source? %s", host))
    print(tnc)
    stop()
  }
  new("ncdbConnection", host = host, tidync = tnc,  ...)
})
#' @param object An object created by \code{dbConnect(ncdb(), host = "[ncsource]")}
#' @rdname ncdb
#' @export
setMethod("show", "ncdbConnection", function(object) {
  if (is.null(object@tidync$source)) {
    print("NULL (no NetCDF source)")
    return(invisible(NULL))
  }
  message("available grids: ")
  print(object@tidync$grid$variable)
})
#' @param conn An object created by \code{dbConnect(ncdb(), host = "[ncsource]")}
#' @rdname ncdb
#' @export
setMethod("dbDisconnect", "ncdbConnection", function(conn, ...) {
  message("removing links to NetCDF source")
  ## TODO: put in name of source from conn@tidync$source...
  ## and, perhaps modify the object record by reference ... somehow?
  conn@tidync <- structure(list(), class = "tidync")
})


#' ncdb results class.
#'
#' @keywords internal
#' @export
setClass("ncdb",
         contains = "DBIResult",
         slots = list(ptr = "externalptr")
)

#' Send a query to ncdb.
#'
#' @export
#' @examples
#' # This is another good place to put examples
setMethod("dbSendQuery", "ncdbConnection", function(conn, statement, ...) {
  # some code
  new("ncdbResult", ...)
})
