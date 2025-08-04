#select the column to make unique, selecting the correct one by numerical criterion
select_the_column<-function(data_frame,id=1,column_number,criteria=c("max","min","extreme")) {
output_vector<-rep(FALSE,nrow(data_frame))
position_numbers<-1:nrow(data_frame)
unique_ids<-unique(data_frame[,id])
	for (i in 1:length(unique_ids)) {
		matches<-data_frame[,id]==unique_ids[i]
		number_of_matches<-sum(matches)
		if (number_of_matches==1) {
			output_vector[position_numbers[matches]]<-TRUE
		} else {
			subset_matches<-position_numbers[matches]
			subset<-data_frame[matches,]
			if (criteria=="max") {
			#out_subset<-subset[max(subset[,column_number])==subset[,column_number],]
			out_number<-subset_matches[max(subset[,column_number])==subset[,column_number]]
			} else if (criteria=="min") {
			#out_subset<-subset[min(subset[,column_number])==subset[,column_number],]
			out_number<-subset_matches[min(subset[,column_number])==subset[,column_number]]
			} else if (criteria=="extreme") {
			#out_subset<-subset[max(abs(subset[,column_number]))==abs(subset[,column_number]),]
			out_number<-subset_matches[max(abs(subset[,column_number]))==abs(subset[,column_number])]
			} else {
			stop("Unknown criteria")
			}
			output_vector[out_number[1]]<-TRUE
		}
		#print(i)
	}
return(data_frame[output_vector,])
}
