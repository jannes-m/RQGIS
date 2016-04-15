#' @title Looking for OSGEO on your system
#' @description \code{find_root} looks for OSGeo on your system under C:,
#'   C:/Program Files and C:/Program Files (x86). So far, this function is only
#'   available for Windows.
#' @param root_name Name of the folder where QGIS, SAGA, GRASS, etc. is
#'   installed. Under Windows this is usually \code{C:/OSGeo4W64}.
#' @author Jannes Muenchow
#' @export
#' @examples
#' find_root(root_name = "OSGeo4w")
find_root <- function(root_name = "OSGeo4W") {
  osgeo4w_root <- NULL

  if (Sys.info()["sysname"] == "Windows") {
    if (any(grepl(root_name, dir("C:/")))) {
      osgeo4w_root <- paste0("C:\\",
                             grep(root_name, dir("C:/"), value = TRUE)[1])
    } else if (any(grepl(root_name, dir("C:/Program Files")))) {
      osgeo4w_root <-
        paste0("C:\\Program Files\\",
               grep(root_name, dir("C:/Program Files"), value = TRUE)[1])
    } else if (any(grepl(root_name, dir("C:/Program Files (x86)")))) {
      osgeo4w_root <-
        paste0("C:\\Program Files (x86)\\",
               grep(root_name, dir("C:/Program Files (x86)"), value = TRUE)[1])
    }
  }
  if (is.null(osgeo4w_root)) {
    stop("Sorry, I could not find ", root_name, " on your system!
         Please provide the path to OSGeo4W yourself!")
  }
  osgeo4w_root
}

#' @title Read command skeletons
#' @description This function simply reads prefabricated Python and batch
#'   commands.
#' @param osgeo4w_root Path to the OSGeo folder or QGIS folder
#' @author Jannes Muenchow
read_cmds <- function(osgeo4w_root = ifelse(Sys.info()["sysname"] == "Windows",
                                            find_root(), NULL)) {
  if (is.null(osgeo4w_root)) {
    stop("Please specify the path to your OSGeo4W-installation!")
  }

  # load raw Python file
  py_cmd <- system.file("python", "raw_py.py", package = "RQGIS")
  py_cmd <- readLines(py_cmd)
  # change paths if necessary
  if (osgeo4w_root != "C:\\OSGeo4W64") {
    py_cmd[11] <- paste0("QgsApplication.setPrefixPath('",
                         osgeo4w_root, "\\apps\\qgis'", "True)")
    py_cmd[15] <- paste0("sys.path.append(r'", osgeo4w_root,
                         "\\apps\\qgis\\python\\plugins')")
  }

  # load windows batch command
  cmd <- system.file("win", "init.cmd", package = "RQGIS")
  cmd <- readLines(cmd)
  # check osgewo4w_root

  # check if GRASS path is correct and which version is available on the system
  vers <- dir(paste0(osgeo4w_root, "\\apps\\grass"))
  if (length(vers) < 1) {
    stop("Please install at least one GRASS version under '../OSGeo4W/apps/'!")
  }
  # check if grass-7.0.3 is available
  if (!any(grepl("grass-7.0.3", vers))) {
    # if not, simply use the older version
    cmd <- gsub("grass.*\\d", vers[1], cmd)
  }

  # return your result
  list("cmd" = cmd,
       "py_cmd" = py_cmd)
}

#' @title Building and executing cmd and Python scripts
#' @description This helper function constructs the batch and Python scripts
#'   which are necessary to run QGIS from the command line.
#' @param processing_name Name of the function from the processing library that
#'   should be used.
#' @param params Parameter to be used with the processing function.
#' @param osgeo4w_root Path to the OSGeo4W installation on your system.
#' @param intern Logical which indicates whether to capture the output of the
#'   command as an \code{R} character vector (see also \code{\link[base]{system}}.
#' @author Jannes Muenchow
execute_cmds <- function(processing_name = "",
                         params = "",
                         osgeo4w_root =
                           ifelse(Sys.info()["sysname"] == "Windows",
                                  find_root(), NULL),
                         intern = FALSE) {

  if (is.null(osgeo4w_root)) {
    stop("Please specify the path to your OSGeo4W-installation!")
  }

  cwd <- getwd()
  on.exit(setwd(cwd))
  tmp_dir <- tempdir()
  setwd(tmp_dir)
  # load raw Python file (has to be called from the command line)
  cmds <- read_cmds(osgeo4w_root = osgeo4w_root)
  py_cmd <- c(cmds$py_cmd,
              paste0(processing_name, "(", params, ")",
                     "\n"))
  py_cmd <- paste(py_cmd, collapse = "\n")
  cat(py_cmd, file = "py_cmd.py")

  # write batch command
  cmd <- c(cmds$cmd, "python py_cmd.py")
  cmd <- paste(cmd, collapse = "\n")
  cat(cmd, file = "batch_cmd.cmd")
  system("batch_cmd.cmd", intern = intern)
}

#' @title Checking paths to QGIS applications on Windows
#' @details \code{check_apps} checks if all the necessary applications (QGIS,
#'   Python27, Qt4, GDAL, GRASS, msys, SAGA) are installed in the correct
#'   locations.
#' @param osgeo4w_root Path to the root directory of the OSGeo4W-installation,
#'   usually C:/OSGeo4W64 or C:/OSGeo4w32.
#' @return The function returns a list with the paths to all the necessary
#'   QGIS-applications.
#'  @examples 
#' \dontrun{
#' check_apps("C:/OSGeo4W64)
#' }
#' @author Jannes Muenchow, Patrick Schratz
check_apps <- function(osgeo4w_root) {
    
    path_apps <- paste0(osgeo4w_root, "/apps")
    
    # define apps to check
    apps <- c("qgis", "Python27", "Qt4", "gdal", "msys", "grass", "saga")
    out <- lapply(apps, function(app) {
        if (any(grepl(app, dir(path_apps)))) {
            path <- paste(path_apps, app, sep = "/")
        }
        else {
            path <- NULL
            txt <- paste0("There is no ", app, "folder in ",
                          path_apps, ".")
            ifelse(app %in% c("qgis", "Python27", "Qt4"),
                   stop(txt, " Please install ", app, 
                        " using the 'OSGEO4W' advanced installation", 
                        " routine."),
                   message(txt, " You might want to install ", app,
                           " using the 'OSGEO4W' advanced installation", 
                           " routine."))
        }
        gsub("//|\\\\", "/", path)
    })
    names(out) <- apps
    # return your result
    out
}

### not functional
check_apps_mac <- function(gdal = "gdal", grass = "GRASS", msys = "msys", 
                           Python27 = "python2.7", qgis = "QGIS", qt4 = "qt", 
                           saga = "saga") {
    
    if (Sys.info()["sysname"] == "Darwin") {
        
        # check gdal
        if (any(grepl(gdal, dir("/usr/local/Cellar")))) {
            gdal_root <- paste0("/usr/local/Cellar/gdal/",
                                grep('[0-9]', dir("/usr/local/Cellar/gdal"),
                                     value = TRUE)[1], "/bin")
            gdal_root = paste0("GDAL path: ", gdal_root)
            print(gdal_root)
        }
        else {
            stop("It seems you do not have 'GDAL' installed. Please install
                 'it on your system!
                 To do so, execute the following lines in a terminal and follow
                 the instructions:
                 1. usr/bin/ruby -e '$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)'
                 2. brew install gdal")
        }
        
        # check grass
        if (any(grepl(grass, dir("/Applications")))) {
            grass_root <- paste0("/Applications/", grep(grass, dir("/Applications/"),
                                                        value = TRUE)[1])
            grass_root = paste0("GRASS path: ", grass_root)
            print(grass_root)
        }
        else {
            stop("It seems you do not have 'GRASS' installed. Please install
                 it on your system!
                 To do so, follow the instructions on this site: 
                 'https://grass.osgeo.org/download/software/mac-osx/'")
        }
        
        # check python
        if (any(grepl(Python27, dir("/usr/bin")))) {
            python_root <- paste0("/usr/bin/", grep(Python27, dir("/usr/bin"),
                                                    value = TRUE)[1])
            python_root = paste0("Python path: ", python_root)
            print(python_root)
        }
        else {
            stop("It seems you do not have 'Python 2.7' installed. Please install
                 it on your system!
                 To do so, install the latest Python 2 release from this site:
                 'https://www.python.org/downloads/mac-osx/'")
        }
        
        # check QGIS
        if (any(grepl(qgis, dir("/Applications")))) {
            qgis_root <- paste0("/Applications/", grep(qgis, dir("/Applications/"),
                                                       value = TRUE)[1])
            qgis_root = paste0("QGIS path: ", qgis_root)
            print(qgis_root)
        }
        else {
            stop("It seems you do not have 'QGIS' installed. Please install
                 it on your system!
                 To do so, follow the instructions on this site:
                 'https://www.qgis.org/de/site/forusers/download.html'")
        }
        
        # check qt4
        if (any(grepl(qt4, dir("/usr/local/Cellar")))) {
            qt4_root <- paste0("/usr/local/Cellar/qt/",
                               grep('[0-9]', dir("/usr/local/Cellar/qt"),
                                    value = TRUE)[1], "/bin")
            qt4_root = paste0("Qt4 path: ", qt4_root)
            print(qt4_root)
        }
        else {
            stop("It seems you do not have 'Qt4' installed. Please install
                 it on your system!
                 To do so, execute the following lines in a terminal and follow
                 the instructions:
                 1. usr/bin/ruby -e '$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)'
                 2. brew install qt4")
        }
        
        }
    else {
        stop("It seems you are not running a MAC but either Windows or Linux. 
             Please use functions according to your system")
        }
    }


set_env <- function(path = NULL) {
    
    path <- "C:/OSGeo4W64/"
    
    if (!is.null(path)) {
        out <- list(root = path)
        out <- c(out, check_apps(osgeo4w_root = path))
        
        
    } else {
        # raw command
        # change to C: drive and (&) list all subfolders of C:
        # /b bare format (no heading, file sizes or summary)
        # /s include all subfolders
        # findstr allows you to use regular expressions
        raw <- "C: & dir /s /b | findstr"
        
        # search QGIS
        cmd <- paste(raw, shQuote("bin\\\\qgis.bat$"))
        tmp <- shell(cmd, intern = TRUE)        
        # search GRASS
        cmd <- paste(raw, shQuote("grass-[0-9].*\\bin$"))
        tmp <- shell(cmd, intern = TRUE)
        
        # search msys
        
        # look for Python27
        cmd <- paste(raw, shQuote("Python27$"))
        shell(cmd, intern = TRUE)
        
        # search Qt4
        
        # search SAGA
    }
    # output should be a list containing paths to
    # SAGA
    # QGIS
    # GRASS
    # Python27
    # msys
    # GDAL
    # Qt4
    
}

