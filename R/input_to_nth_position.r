input_to_nth_position<-function(data_frame_input,vector_input,position=NULL,name_of_new_column="new_column") {
	data_frame_output<-cbind(data_frame_input,vector_input)
	if (is.null(position)) {
		colnames(data_frame_output)[ncol(data_frame_output)] <- name_of_new_column
	} else {
		data_frame_output<-data_frame_output[,c(1:(position-1),ncol(data_frame_output),position:(ncol(data_frame_output)-1))]
		colnames(data_frame_output)[position] <- name_of_new_column
	} 
	return(data_frame_output)
}
