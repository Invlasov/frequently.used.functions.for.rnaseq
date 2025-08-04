# similiarly to cbind_with_fill, rowbinds while filling empty columns with fill
rbind_with_fill<-function(x,y,fill=0) {
	if (!identical(colnames(x),colnames(y))) {
		# rownames(temp_x)
		temp_x<-add_missing_cols_from_x_to_y(x,y,fill=fill)
		temp_y<-add_missing_cols_from_x_to_y(y,x,fill=fill)
		temp_y<-temp_y[,order(colnames(temp_y)),drop=FALSE]
		temp_x<-temp_x[,order(colnames(temp_x)),drop=FALSE]
		output<-rbind(temp_x,temp_y)
		colnames(output)<-colnames(temp_x)
		rownames(output)<-c(rownames(temp_x),rownames(temp_y))
	} else {
	output<-rbind(x,y)
	}
return(output)
}
