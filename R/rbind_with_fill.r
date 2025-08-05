# similiarly to cbind_with_fill, rowbinds while filling empty columns with fill
#' This function uses add_missing_cols_from_x_to_y to perform rbind that fills empry rows and columns with fill instead of stopping
#' param x Data frame 1 
#' param y Data frame 2
#' param fill Variable to fill empty fields with.
#' return Data frame. Combined from data frames 1 and 2, with empty cols filled.
#' @export

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
