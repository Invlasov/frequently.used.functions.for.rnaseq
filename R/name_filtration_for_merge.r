# function that firlters names in both df to match eachother
name_filtration_for_merge<-function(df_1,df_2,row_1,row_2,byrow=TRUE) {
common_names<-intersect(row_1,row_2)
new_order_1<-order(row_1)
new_order_2<-order(row_2)
row_1<-row_1[new_order_1]
row_2<-row_2[new_order_2]

	if (byrow) {
		df_1<-df_1[new_order_1,]
		df_2<-df_2[new_order_2,]
	
		df_1<-df_1[row_1 %in% common_names,]
		df_2<-df_2[row_2 %in% common_names,]


		
	} else {
		df_1<-df_1[,new_order_1]
		df_2<-df_2[,new_order_2]
	
		df_1<-df_1[,row_1 %in% common_names]
		df_2<-df_2[,row_2 %in% common_names]
	}
	return(list(df_1,df_2,common_names))
}
