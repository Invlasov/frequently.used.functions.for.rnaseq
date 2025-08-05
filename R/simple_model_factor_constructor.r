#' This function creates simple factors from more complex ones, like when you need to manually create several individual factors from a single multi value factor.
#' @param x Vector of values to create factors of.
#' @param def Character or number - what will be first level factor?
#' @param pattern Character or number - what will be second level factor?
#' @return Factor, in whicn first level is NOT identical to pattern, and second is what is identical to pattern.
#' @export

simple_model_factor_constructor<-function(x,def="0",pattern="1") {
	out_vector<-rep(def,length(x))
	out_vector[x==pattern]<-pattern
	out_factor<-factor(out_vector,levels=c(def,pattern))
	return(out_factor)
}
