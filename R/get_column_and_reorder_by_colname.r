# simple function to get cols in order by inputing df and colnames
get_column_and_reorder_by_colname<-function(input_df,col_names_in_order) {
	match_vec<-col_names_in_order%in%colnames(input_df)
	if (sum(match_vec) < length(col_names_in_order)) {
	msg<-paste0("Error - colnames ", match_vec, " are missing from df colnames", collapse=", ")
	print(msg)
	} else {
	cols_nums<-rep(-9999,length(col_names_in_order))
		for (i in 1:length(col_names_in_order)) {
		cols_nums_i<-grep(col_names_in_order[i],colnames(input_df),fixed=TRUE)
		cols_nums[i]<-cols_nums_i
		}
		
	}
	return(input_df[,cols_nums])
}
