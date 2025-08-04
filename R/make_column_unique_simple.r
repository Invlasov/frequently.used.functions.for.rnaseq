# like previous "make column unique" functions, but just combines unique values with ", " delimiter
make_column_unique_simple<-function(x,colnum=1,delim=", ") {
row_unique<-unique(x[,colnum])
input_matrix<-matrix(nrow=length(row_unique),ncol=ncol(x))

for (i in 1:length(row_unique)) {
		subset_df<-x[(x[,colnum]==row_unique[i]),,drop=FALSE]
	if (nrow(subset_df)>1) {
		output_vector<-vector("character",length=ncol(subset_df))
		for (j in 1:length(output_vector)) {
			output_vector[j]<-paste0(unique(subset_df[,j]),collapse=delim)
		}
		input_matrix[i,]<-output_vector
	} else {
		input_matrix[i,]<-unlist(subset_df)
	}

}

input_df<-data.frame(input_matrix)
colnames(input_df)<-colnames(x)
return(input_df)
}