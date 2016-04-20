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
  
  if (Sys.info()["sysname"] == "Darwin") {
      osgeo4w_root = "/applications/QGIS.app/Contents"
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
read_cmds <- function(osgeo4w_root =find_root()) {  
    
  if (is.null(osgeo4w_root)) {
    stop("Please specify the path to your OSGeo4W-installation!")
  }
  
    if (Sys.info()["sysname"] == "Windows") {
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
    
    if (Sys.info()["sysname"] == "Darwin") {
        # load raw Python file
        py_cmd <- system.file("python", "raw_py_mac.py", package = "RQGIS")
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
                         osgeo4w_root = find_root(),
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
check_apps <- function(osgeo4w_root = find_root()) {
    
    
    if (Sys.info()["sysname"] == "Windows") {
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
    
    if (Sys.info()["sysname"] == "Darwin") {
        
        osgeo4w_root = find_root()
        path_apps <- osgeo4w_root
        
        all_paths = list()
        
        
        # check gdal
        gdal = sub('/GdalAlgorithm.py','', system2("find",args='/Applications/QGIS.app/Contents/Resources/python/plugins/processing/algs/gdal -name "GdalAlgorithm.py" -type f -print',stdout=TRUE))
        
        if (length(gdal) == 0) {
            stop("It seems you do not have 'GDAL' installed. Please install it on your system! To do so, execute the following lines in a terminal and follow the instructions: 1. usr/bin/ruby -e '$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)' 2. brew install gdal")
        }
        
        # check python
        py = sub('/ProcessingPlugin.py','', system2("find",args='/Applications/QGIS.app/Contents/Resources/python -name "ProcessingPlugin.py" -type f -print',stdout=TRUE))
        
        if (length(py) == 0){
            stop("It seems you do not have 'Python 2.7' installed. Please install
                 it on your system!
                 To do so, install the latest Python 2 release from this site:
                 'https://www.python.org/downloads/mac-osx/'")
        }
        
        # check qgis
        qgis = sub('/BarPlot.py','', system2("find",args='/Applications/QGIS.app/Contents/Resources/python/plugins/processing/algs/qgis -name "BarPlot.py" -type f -print',stdout=TRUE))
        
        if (length(qgis) == 0) {
            stop("It seems you do not have 'QGIS' installed. Please install
                 it on your system!
                 To do so, install the latest Python 2 release from this site:
                 'https://www.python.org/downloads/mac-osx/'")
        }
        
        
        # check grass
        grass = sub('/grass.txt','', system2("find",args='/Applications/QGIS.app/Contents/Resources/python/plugins/processing/algs/grass -name "grass.txt" -type f -print',stdout=TRUE))
        
        if (length(grass) == 0) {
            stop("It seems you do not have 'GRASS' installed. Please install
                 it on your system!
                 To do so, install the latest Python 2 release from this site:
                 'https://www.python.org/downloads/mac-osx/'")
        }
        
        # check SAGA
        saga = sub('/SagaUtils.py','', system2("find",args='/Applications/QGIS.app/Contents/Resources/python/plugins/processing/algs/SAGA -name "SagaUtils.py" -type f -print',stdout=TRUE))
        
        if (length(saga) == 0) {
            stop("It seems you do not have 'SAGA' installed. Please install
                 it on your system!
                 To do so, install the latest Python 2 release from this site:
                 'https://www.python.org/downloads/mac-osx/'")
        }
        
        # check Qt4
        Qt4 = sub('/Qt.so','', system2("find",args='/Applications/QGIS.app/Contents/Resources/python/PyQt4 -name "Qt.so" -type f -print',stdout=TRUE))
        
        if (length(Qt4) == 0) {
            stop("It seems you do not have 'Qt4' installed. Please install
                 it on your system!
                 To do so, install the latest Python 2 release from this site:
                 'https://www.python.org/downloads/mac-osx/'")
        }
        
        
        all_paths = c(osgeo4w_root, qgis, py, Qt4, gdal, grass, saga)
        all_paths
    }
}
check_apps()


set_env <- function(path = NULL) {
    
    if (Sys.info()["sysname"] == "Windows") {
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
    
    if (Sys.info()["sysname"] == "Darwin") {
        
        path <- "/applications/QGIS.app/Contents"
        
        if (!is.null(path)) {
            tmp = check_apps()
            out <- list(root = path, Python27 = tmp[[2]], Qt4 = tmp[[3]], 
                        gdal = tmp[[4]], grass = tmp[[5]], saga = tmp[[6]])
        }
        
        out
    }
    
}
