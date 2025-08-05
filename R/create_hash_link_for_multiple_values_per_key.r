#creates hash link for several different values per single unique key. Basically appends value with 
create_hash_link_for_multiple_values_per_key<-function(keys,values,input_hash=NA,make_unique=TRUE,do_sort=TRUE) {
	if (is.na(input_hash)) {
	input_hash<-hash::hash()
	}
	for (i in 1:length(keys)) {
		if (hash::has.key(hash=input_hash, keys[i])) {
			inside<-input_hash[[keys[i]]]
			# print(inside)
			appended<-c(inside,values[i])
			if (make_unique) {
				appended<-unique(appended)
			} 
			if (do_sort) {
				appended<-sort(appended)
			}
			hash::.set(hash=input_hash,keys=keys[i],values=appended)	
		} else {
			hash::.set(hash=input_hash,keys=keys[i],values=values[i])
		}
	}
	return(input_hash)
}
