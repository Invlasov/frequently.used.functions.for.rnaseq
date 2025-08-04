# function (mostly for geo datasets) to replace iteratively all of the cetrain pattern with a different string 
replace_by_list<-function(to_replace,replacement,input_vec,...) {
	input_vec_r<-input_vec
	for (i in 1:length(to_replace)) {
	input_vec_r<-gsub(to_replace[i],replacement,input_vec_r,...)
	}
	return(input_vec_r)

}