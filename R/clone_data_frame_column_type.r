#this function clones the types of columns from one data frame to another
clone_data_frame_column_type<-function(donor, acceptor) {
	if (is.data.frame(donor) & is.data.frame(acceptor)) {
		if (ncol(donor)==ncol(acceptor)) {
			for (i in 1:ncol(donor)) {
			mode(acceptor[,i])<-mode(donor[,i])
			}
			return(acceptor)
		} else {
			stop("Make sure both inputs have same amount of columns")
		}
	} else {
		stop("Make sure both inputs are data frames")
	}
	
}