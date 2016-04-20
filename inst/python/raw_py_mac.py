# import all the libraries you need
import os
from qgis.core import *
from osgeo import ogr
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.gui import *
import sys
import os
# initialize QGIS application
QgsApplication.setPrefixPath('C:\OSGeo4W64\apps\qgis', True)
app = QgsApplication([], True)
QgsApplication.initQgis()
# add the path to processing framework
sys.path.append(r'C:\OSGeo4W64\apps\qgis\python\plugins')
# import and initialize the processing framework
from processing.core.Processing import Processing
Processing.initialize()
import processing

