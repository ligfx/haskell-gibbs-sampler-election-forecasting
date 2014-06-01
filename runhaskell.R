library(stringr)
runhaskell <- function(name) {
	module <- unlist(str_split(name, "\\.", n=2))[1]
	return(system(paste("export LD_LIBRARY_PATH=/usr/local/lib && echo 'import qualified ", module, "; main = ", name, "' | runhaskell"), intern=TRUE))
}
library(jsonlite)
library(functional)
runhaskellJ <- Compose(runhaskell, fromJSON)
