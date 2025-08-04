#adds missing (by rownames) rows from frame x to frame y
add_missing_rows_from_x_to_y<-function(y, x,fill=0) {
	x_to_y<-rownames(x) %in% rownames(y)
	add_to_y<-matrix(rep(fill,(ncol(y)*sum(!x_to_y))),ncol=ncol(y),nrow=sum(!x_to_y))
	rownames(add_to_y)<-rownames(x)[!x_to_y]
	colnames(add_to_y)<-colnames(y)
	temp_y<-rbind(y,add_to_y)
	rownames(temp_y)<-c(rownames(y),rownames(add_to_y))
	return(temp_y)
}
