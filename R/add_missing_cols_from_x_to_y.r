#adds missing (by colnames) cols from frame x to frame y

add_missing_cols_from_x_to_y<-function(y, x,fill=0) {
	x_to_y<-colnames(x) %in% colnames(y)
	add_to_y<-matrix(rep(fill,(nrow(y)*sum(!x_to_y))),nrow=nrow(y),ncol=sum(!x_to_y))
	colnames(add_to_y)<-colnames(x)[!x_to_y]
	rownames(add_to_y)<-rownames(y)
	temp_y<-cbind(y,add_to_y)
	colnames(temp_y)<-c(colnames(y),colnames(add_to_y))
	return(temp_y)
}
