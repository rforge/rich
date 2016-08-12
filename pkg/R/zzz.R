.onAttach <- function(libname, pkgname){
  pkg.version <- packageDescription("rich", fields = "Version")
  startup.txt <- paste("\nrich version ", pkg.version, " is loaded\n", "You can access the package vignette by typing vignette('rich_introduction') in the R console", sep="")
  packageStartupMessage(startup.txt)
}

