#this function is often needed when working with GEO series matrix - it separates the first part of string after specified pattern

separate_first_part<-function(x,pattern=" ") {
splat<-strsplit(x=x,split=pattern)
last_part<-vector(mode="character")
	for (j in 1:length(splat)) {
	last_part<-c(last_part,splat[[j]][1])
	}
	return(last_part)
}
