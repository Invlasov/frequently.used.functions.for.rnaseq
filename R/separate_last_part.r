# this function is often neede when working with GEO series matrix - it separates the last part of string after specified pattern
#' Function that gets the last part of string, splat by pattern.
#' For example "why are you here" splat by this function with default delim would return "here"
#' @export
#' @param x Input string to be splat.
#' @param pattern Patter to use in strsplit. Single space by default
#' @return Splat part.

separate_last_part<-function(x,pattern=" ") {
splat<-strsplit(x=x,split=pattern)
last_part<-vector(mode="character")
	for (j in 1:length(splat)) {
	last_part<-c(last_part,splat[[j]][length(splat[[j]])])
	}
	return(last_part)
}