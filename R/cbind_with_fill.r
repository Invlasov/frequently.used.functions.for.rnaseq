#' This function uses add_missing_rows_from_x_to_y to perform cbind that fills empry rows and columns with fill instead of stopping
#' @param x Data frame 1 
#' @param y Data frame 2
#' @param fill Variable to fill empty fields with.
#' @return Data frame. Combined from data frames 1 and 2, with empty rows filled.
#' @export


cbind_with_fill<-function(x,y,fill=0) {
	if (!identical(rownames(x),rownames(y))) {
		temp_x<-add_missing_rows_from_x_to_y(x,y,fill=fill)
		temp_y<-add_missing_rows_from_x_to_y(y,x,fill=fill)
		temp_y<-temp_y[order(rownames(temp_y)),,drop=FALSE]
		temp_x<-temp_x[order(rownames(temp_x)),,drop=FALSE]
		output<-cbind(temp_x,temp_y)
		rownames(output)<-rownames(temp_x)
	} else {
	output<-cbind(x,y)
	}

return(output)
}
