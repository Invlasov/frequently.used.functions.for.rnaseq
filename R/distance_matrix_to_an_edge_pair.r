#' This function converts symmetrical distance matrixes (such as matrixes, obtained by as.matrix from an output of dist function) into data frame of edges (with distance).
#' @param x Symmetrical distance matrix.
#' @return Data frame of edge pairs (with distance).
#' @export

distance_matrix_to_an_edge_pair<-function(x) {
	done_already<-c()
	cname<-colnames(x)
	rname<-rownames(x)
	from<-c()
	to<-c()
	distance<-c()
	for (i in 1:nrow(x)) {
		for (j in 1:ncol(x)) {
			this_identificator<-paste0(cname[j],"_to_",rname[i])
			reverse_identificator<-paste0(rname[i],"_to_",cname[j])
			if (!(this_identificator %in% done_already) & !(reverse_identificator %in% done_already) &!(identical(cname[j],rname[i]))) {
				from<-c(from,rname[i])
				to<-c(to,cname[j])
				distance<-c(distance,x[i,j])
			} 
		}	
	}
	return(data.frame("From"=from,"To"=to,"Distance"=distance,stringsAsFactors=FALSE))
}
