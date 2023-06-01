install.packages("devtools") 
install.packages("roxygen2")

library(devtools)
library(pkgbuild)
library(roxygen2)
  
# lets check if Rtools are installed
pkgbuild::find_rtools()

# set the path for the packages 
# If you create more packages - it's good to have one separate folder
# to store all of them. Here we will use new folder in which we will 
# create folders with new packages (as a part of the larger path)
# (remember to put slash `/` at the end)

path_to_packages <- "D:/Data Science/summer semester/Advanced R/Project/RAutoGrader-Automatic-R-Code-Testing-Tool/My Packages/"

create_package(path = paste0(path_to_packages, 
                             "MyFirstPackage"))

devtools::check(paste0(path_to_packages, 
                       "MyFirstPackage"))

roxygenise(package.dir = paste0(path_to_packages, 
                                "MyFirstPackage"))

devtools::check(paste0(path_to_packages, 
                       "MyFirstPackage"))

devtools::install(pkg = paste0(path_to_packages, 
                               "MyFirstPackage"), 
                  reload = TRUE)

library(MyFirstPackage)

MyFirstPackage::insert()

?insert
