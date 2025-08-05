#' This function creates an empty matrix with designated number of rows and columns, filled by "fill"
#' @param rnum Number of rows.
#' @param colnum Number of columns
#' @param fill Variable to fill empty fields with.
#' @return Matrix, filled with fill variable.
#' @export


create_empty_matrix_and_fill_it<-function(rnum,colnum,fill=0) {
replength<-rnum*colnum
input_vec<-rep(fill,replength)
out_matr<-matrix(input_vec,ncol=colnum,nrow=rnum)
return(out_matr)
}

