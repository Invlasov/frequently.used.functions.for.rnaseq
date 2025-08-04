element_number_n_from_list<-function(x,n=1) {
		output<-vector(length(x),mode=typeof(x[[i]][n]))
		for (i in 1:length(x)) {
		output[i]<-x[[i]][n]
		}
	return(output)
}
