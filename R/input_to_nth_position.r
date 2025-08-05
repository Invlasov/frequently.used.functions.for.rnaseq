#' This function inputs a new column to the designated position in a df.
#' @param data_frame_input Data frame to input column in it.
#' @param vector_input Column to add to the data frame.
#' @param position Where to put a new column. If NA - put it at the end.
#' @param name_of_new_column Name for a new column
#' @export
#' @return A data frame with a column added to a position.

input_to_nth_position<-function(data_frame_input,vector_input,position=NA,name_of_new_column="new_column") {
	data_frame_output<-cbind(data_frame_input,vector_input)
	if (is.na(position)) {
		colnames(data_frame_output)[ncol(data_frame_output)] <- name_of_new_column
	} else {
		data_frame_output<-data_frame_output[,c(1:(position-1),ncol(data_frame_output),position:(ncol(data_frame_output)-1))]
		colnames(data_frame_output)[position] <- name_of_new_column
	} 
	return(data_frame_output)
}
