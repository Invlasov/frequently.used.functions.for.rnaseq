# gives a short summary of colunbs and their unique contents
summary_df<-function(input_i_df,max_length=30,exluded="") {
for (i in 1:ncol(input_i_df)) {
	uvals<-unique(input_i_df[input_i_df[,i]!=exluded,i])

	if (length(uvals) < max_length) {
		uvals_paste<-paste(uvals,sep="",collapse =", ")
		print(paste0("Column:",colnames(input_i_df)[i]," has unique values ",uvals_paste))
	}
}
}
