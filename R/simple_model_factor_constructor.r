#creates simple factors from more complex ones, like when you need to manually create several individual factors from a single multi value factor
simple_model_factor_constructor<-function(x,def="0",pattern="1") {
	out_vector<-rep(def,length(x))
	out_vector[x==pattern]<-pattern
	out_factor<-factor(out_vector,levels=c(def,pattern))
	return(out_factor)
}
