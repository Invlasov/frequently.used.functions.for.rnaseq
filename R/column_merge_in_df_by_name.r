#' This function replaces first instance of a column (in data frame) with a certain name by what is produced by "column merger" function by combining it with a second column.
#' param input_df Data frame from which column types will be copied.
#' param colname_1 First column to be merged. Will be replaced by column merger.
#' param colname_2 Second column to be merged. Will be dropped.
#' param new_name A name for column, created by merging. If NA (default) it gets replaced by a name of colname 1.
#' param ... Additionall parameters, passed to "column merger" function.
#' return Data frame with a merged column in a place of colunm with colname_1.

column_merge_in_df_by_name<-function(input_df,colname_1,colname_2,new_name=NA,...) {
	if (is.na(new_name)) {
	new_name<-colname_1
	}
	col_1_pos<-grep(paste0("^",colname_1,"$"),colnames(input_df))[1]
	col_2_pos<-grep(paste0("^",colname_2,"$"),colnames(input_df))[1]
	col_1<-input_df[,col_1_pos]
	col_2<-input_df[,col_2_pos]
	# print()
	pos_to_put_in<-min(col_1_pos,col_2_pos)
	pos_to_drop<-max(col_1_pos,col_2_pos)
	# write.table(file=filepath_2,x=output_df,quote = FALSE,sep = "\t",row.names = FALSE,col.names=TRUE)
	out_column<-column_merger(col_1,col_2,...)
	input_df<-input_df[,pos_to_drop*-1]
	input_df[,pos_to_put_in]<-out_column
	colnames(input_df)[pos_to_put_in]<-new_name
	return(input_df)
}
