#small function to do the pattern match for the list which would output the filter for list elements regardless of the length of vectors in a list. 
match_to_object_list_on_listrows<-function(input_list,object_list) {
	output_filter<-rep(FALSE,length=length(input_list))
	for (i in 1:length(input_list)) {
		for (j in 1:length(input_list[[i]])) {
			if (input_list[[i]][j] %in% object_list) {
				output_filter[i]<-TRUE
			}
		} 
	}
	return(output_filter)
}
