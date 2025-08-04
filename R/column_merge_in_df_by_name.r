#replaces first instance of a column in df by what is produced by column merger
column_merge_in_df_by_name<-function(input_df,colname_1,colname_2,new_name="column_added_by_column_merger",...) {
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
