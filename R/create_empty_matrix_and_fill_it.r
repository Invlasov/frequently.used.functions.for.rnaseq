create_empty_matrix_and_fill_it<-function(rnum,colnum,fill=0) {
replength<-rnum*colnum
input_vec<-rep(fill,replength)
out_matr<-matrix(input_vec,ncol=colnum,nrow=rnum)
return(out_matr)
}

