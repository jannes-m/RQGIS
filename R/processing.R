#' @title Find and list available QGIS algorithms
#' @description \code{find_algorithms} lists or queries all algorithms which
#'   can be used via the command line and the QGIS API.
#' @param osgeo4w_root Path to the OSGEO or QGIS folder on your system.
#' @param search_term A character to query QGIS functions, i.e. to list only
#'   functions which contain the indicated string.
#' @details Function \code{find_algorithms} simply calls
#'   \code{processing.alglist} using Python.
#' @return Python console output will be captured as an R character vector.
#' @author Jannes Muenchow, QGIS developer team
#' @examples
#' # list all available QGIS algorithms
#' find_algorithms()
#' # find a function which adds coordinates
#' find_algorithms(search_term = "add")
#' @export
find_algorithms <- function(search_term = "",
                            osgeo4w_root = find_root()) {

  if (is.null(osgeo4w_root)) {
    stop("Please specify the path to your OSGeo4W-installation!")
  }

  execute_cmds(processing_name = "processing.alglist",
               params = shQuote(search_term),
               intern = TRUE)
}

#' @title Get usage of a specific GIS function
#' @description \code{get_usage} lists all function parameters of a specific GIS
#'   function.
#' @param algorithm_name Name of the function whose parameters are being
#'   searched for.
#' @param osgeo4w_root Path to OSGeo4W on your system.
#' @param intern Logical which indicates whether to capture the output of the
#'   command as an \code{R} character vector (see also
#'   \code{\link[base]{system}}.
#' @author Jannes Muenchow, QGIS developer team
#' @export
#' @examples
#' # find a function which adds coordinates
#' find_algorithms(search_term = "add")
#' # find function arguments of saga:addcoordinatestopoints
#' get_usage(algorithm_name = "saga:addcoordinatestopoints")
get_usage <- function(algorithm_name = "",
                      osgeo4w_root =
                        ifelse(Sys.info()["sysname"] == "Windows",
                               find_root(), NULL),
                      intern = FALSE) {

  if (is.null(osgeo4w_root)) {
    stop("Please specify the path to your OSGeo4W-installation!")
  }

  execute_cmds(processing_name = "processing.alghelp",
               params = shQuote(algorithm_name),
               intern = intern)
}

#' @title Get options of parameters for a specific GIS option
#' @description \code{get_options} lists all available parameter options for
#'   the required GIS function.
#' @param algorithm_name Name of the GIS function for which options should be
#'   returned.
#' @param osgeo4w_root Path to OSGeo4W on your system.
#' @author Jannes Muenchow, QGIS devleoper team
#' @examples
#' get_options(algorithm_name = "saga:slopeaspectcurvature")
get_options <- function(algorithm_name = "",
                        osgeo4w_root =
                          ifelse(Sys.info()["sysname"] == "Windows",
                                 find_root(), NULL)) {

  if (is.null(osgeo4w_root)) {
    stop("Please specify the path to your OSGeo4W-installation!")
  }
  execute_cmds(processing_name = "processing.algoptions",
               params = shQuote(algorithm_name))
}



#' @title Interface to QGIS commands
#' @description \code{run_qgis} is the workhorse of the R-QGIS interface: It
#'   calls the QGIS API from within R to run QGIS algorithms while passing the
#'   corresponding function arguments.
#' @param algorithm Name of the GIS function to be used (see
#'   \code{\link{find_algorithms}}).
#' @param params A list of function arguments that should be used in conjunction
#'   with the selected GIS function (see \code{\link{get_usage}} and
#'   \code{\link{get_options}}).
#' @param osgeo4w_root Path to OSGeo4W on your system.
#' @details This workhorse function calls QGIS via Python (QGIS API) using the
#'   command line.
#' @author Jannes Muenchow, QGIS developer team
#' @export
#' @examples
#' \dontrun{
#' # find out how a function is called
#' find_algorithms(search_term = "add")
#' # find out how it works
#' get_usage(algorithm_name = "saga:addcoordinatestopoints")
#' # specify the parameters in the exact same order as listed by get_usage
#' params <- list(INPUT = "C:/Users/pi37pat/Desktop/test/random_squares.shp",
#'      OUTPUT = "C:/Users/pi37pat/Desktop/test/qgis_testi2.shp")
#' run_qgis(algorithm = "saga:addcoordinatestopoints",
#'          params = params)
#' }
run_qgis <- function(algorithm = NULL, params = list(),
                     osgeo4w_root =
                       ifelse(Sys.info()["sysname"] == "Windows",
                              find_root(), NULL)) {

  if (is.null(osgeo4w_root)) {
    stop("Please specify the path to your OSGeo4W-installation!")
  }

  nm = names(params)
  val = as.character(unlist(params))
  # build command
  # start <- paste0("processing.runalg('algOrName' = ", shQuote(algorithm))
  start <- shQuote(algorithm)
  # mmh, processing.runalg does not accept arguments... that's unfortunate
  # args <- paste(shQuote(nm), shQuote(val),  sep = " = ", collapse = ", ")
  args <- paste(shQuote(val), collapse = ", ")
  args <- paste0(paste(start, args, sep = ", "))
  # run QGIS command
  execute_cmds(processing_name = "processing.runalg",
               params = args)
}
