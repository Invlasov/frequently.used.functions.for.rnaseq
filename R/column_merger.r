# merging columns, keeping stuff from both if it is not empty field
column_merger<-function(column_1,column_2,empty_field="",sep_paste="\t") {
	out_col<-rep(empty_field,length(column_1))
	namap_1<-is.na(column_1)
	namap_2<-is.na(column_2)
	empty_map_1<-(column_1 %in% empty_field) | namap_1
	empty_map_2<-(column_2 %in% empty_field) | namap_2
# we need separate logic for three difffernt cases
# case 1 - empty in both? easy, just skip
# case 2 - full in both? paste with a predetermined sep
# case 3 - empty in one? easy, just fill with non empty (or just)
	empty_in_both<-(empty_map_1 & empty_map_2)
	full_in_both<-(!empty_map_1 & !empty_map_2)
	empty_in_one<-xor(empty_map_1,empty_map_2)
	# out_col[empty_in_both]<-empty_field
	pasted_replacement<-rep(empty_field,sum(full_in_both))
	for (i in 1:sum(full_in_both)) {
		pasted_replacement[i]<-paste0((column_1[full_in_both])[i],sep_paste,(column_2[full_in_both])[i])
	}
	out_col[full_in_both]<-pasted_replacement
	
	out_col[(!empty_map_1 & empty_map_2)]<-column_1[(!empty_map_1 & empty_map_2)]
	out_col[(empty_map_1 & !empty_map_2)]<-column_2[(empty_map_1 & !empty_map_2)]
	
# out_col[empty_in_one]<-empty_field
return(out_col)
}
