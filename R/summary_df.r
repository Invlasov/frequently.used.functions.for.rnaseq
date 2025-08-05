#' This function gives a short summary of columns and their unique contents.
#' @param input_i_df Df to summarize
#' @param max_length Max length of unique entries in a col. 
#' @param exluded Vector of contents to exclude from summary.
#' @return No return. Pastes the output to STDOUT.
#' @export


summary_df<-function(input_i_df,max_length=30,exluded="") {
for (i in 1:ncol(input_i_df)) {
	uvals<-unique(input_i_df[input_i_df[,i]!=exluded,i])

	if (length(uvals) < max_length) {
		uvals_paste<-paste(uvals,sep="",collapse =", ")
		print(paste0("Column:",colnames(input_i_df)[i]," has unique values ",uvals_paste))
	}
}
}
