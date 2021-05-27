# debug: setwd("C:/Users/eduar/OneDrive/Git/mylibrary")
repos_name <- getOption("repos")[1]

setrepos <- function(repos=repos) {
  repos_name <- repos 
}

loadlibrary <- function(packagename) 
{
  if (!require(packagename,character.only = TRUE))
  {
    install.packages(packagename, repos=repos_name, dep=TRUE, verbose = FALSE)
    require(packagename,character.only = TRUE)
  }
}

