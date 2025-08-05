#creates hash link for several different values per single unique key. Basically appends value with 
#' This function creates hash link for several different values per single unique key. Basically appends value with second instance it encounters etc.
#' @param keys Vector of keys, same as you would pass to .set for hash.
#' @param values Vector of values, same as you would pass to .set for hash.
#' @param input_hash Hash to add values to. If none is provided it is created within script (default)
#' @param make_unique Do you only want unique values in hash returnes?
#' @param do_sort Do you want unique values in hash returnes sorted?
#' @return Hash with appended values, wich returns vectors of values as input. 
#' @export
#' @importFrom hash hash
#' @importFrom hash has.key



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
