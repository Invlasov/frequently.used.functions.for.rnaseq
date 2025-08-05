#' This function (mostly useful in processing GEO experiment matrices) to replace iteratively all of the cetrain pattern with a different string.
#' @param to_replace Vector of stuff to replace.
#' @param replacement Variable to replace it with.
#' @param input_vec Vector to replace stuff in.
#' @param ... Other parameters to pass to gsub.
#' @return Vector with replaced contents.
#' @export

replace_by_list<-function(to_replace,replacement,input_vec,...) {
	input_vec_r<-input_vec
	for (i in 1:length(to_replace)) {
	input_vec_r<-gsub(to_replace[i],replacement,input_vec_r,...)
	}
	return(input_vec_r)

}