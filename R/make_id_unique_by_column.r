#' This function makes ID's in data frame unique, selecting columns by the rule, applied to leading column. Usually used for cases where a gene has multiple probeset on microarray, and you have to choose one to get fc and p.val.
#' For example, if there is a leading column with values 10, 100, and -199 for a given unique value in id col, min rule will end up taking the row with -199, as will extreme.
#' @param input_frame Data frame to process.
#' @param id_column Number of a column to make unique.
#' @param leading_column This column is used to make decision which of rows to take.
#' @param method The method of choosing a row (applied to a leading column).
#' @export
#' @return A data frame with a designated column having only unique values.


make_id_unique_by_column<-function(input_frame, id_column, leading_column, method="extreme") {
	if (!is.data.frame(input_frame)) {stop("Make sure input is data frame")}
	if (!is.numeric(input_frame[,leading_column])) {stop("Make sure leading column is numeric")}

	unique_ids<-unique(input_frame[,id_column])
	if (length(unique_ids)==nrow(input_frame)) {
		return(input_frame)
	} else {
		output_matrix<-matrix(,nrow=length(unique_ids),ncol=ncol(input_frame))
			for (i in 1:length(unique_ids)) {
				matchvec<-input_frame[,id_column]==unique_ids[i]
				where<-(1:length(matchvec))[matchvec]
				if (length(where)>1) {
					subset<-input_frame[where,]
						if (method=="extreme") {
						subset<-subset[max(abs(subset[,leading_column]))==abs(subset[,leading_column]),,drop=FALSE]
						} else if (method=="max") {
						subset<-subset[max(subset[,leading_column])==subset[,leading_column],,drop=FALSE]
						} else if (method=="min") {
						subset<-subset[min(subset[,leading_column])==subset[,leading_column],,drop=FALSE]
						} else {
						stop("Unknown method")
						}
					output_matrix[i,]<-unlist(subset[1,],use.names=FALSE)
				} else if (length(where)==1) {
					output_matrix[i,]<-unlist(input_frame[where,],use.names=FALSE)
				} else {
					print(paste("No matching gene name at line ",i,sep="", collapse=NULL))
				}
			}
		output_matrix<-as.data.frame(output_matrix,stringsAsFactors=FALSE)
		output_matrix<-clone_data_frame_column_type(donor=input_frame,acceptor=output_matrix)
		colnames(output_matrix)<-colnames(input_frame)
		return(output_matrix)		
	}			
}
