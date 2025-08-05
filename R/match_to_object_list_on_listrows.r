#small function to do the pattern match for the list which would output the filter for list elements regardless of the length of vectors in a list. 
#' This function checks for presence on an object in the list for every element of a 2 d list, and returns a 1 d list matching in any object of the list is present in a 2d part of the list.
#' @param input_list List to be cheked.
#' @param object_list vector of element to check list elements against.
#' @return a boolean vector of presence of any elements from a object list in a vector in each slot of a list.
#' @export

match_to_object_list_on_listrows<-function(input_list,object_list) {
	output_filter<-rep(FALSE,length=length(input_list))
	for (i in 1:length(input_list)) {
		for (j in 1:length(input_list[[i]])) {
			if (any(input_list[[i]][j] %in% object_list)) {
				output_filter[i]<-TRUE
			}
		} 
	}
	return(output_filter)
}
