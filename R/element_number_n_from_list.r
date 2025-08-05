#' This function returns element number n from each entry in a 2d list.
#' @param x 2d list.
#' @param n Number of element from list to get.
#' @return A vector of elements number n.
#' @export


element_number_n_from_list<-function(x,n=1) {
		output<-vector(length(x),mode=typeof(x[[i]][n]))
		for (i in 1:length(x)) {
		output[i]<-x[[i]][n]
		}
	return(output)
}
