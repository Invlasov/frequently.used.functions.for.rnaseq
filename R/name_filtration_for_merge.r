#' This function firlters contents of a two rows (or cols) in two dfs, and preserve only rows (or cols) which contain common elements between the two.
#' Warning could accept not vectors from dfs, but will result in unpredictable outputs.
#' @param df_1 First data frame
#' @param df_2 Second data frame
#' @param row_1 First row to be used for filtering.
#' @param row_2 Second row to be used for filtering.
#' @param byrow Use rows or calls? Putting false there will make script work by columns instead.
#' @return A list, with 1 being a first filtered df,2 being a second filtered df and 3 having a list of common elements by which filtration was performed.
#' @export


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
