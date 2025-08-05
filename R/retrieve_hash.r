#' This function input a number of hash keys one by one in specified order, and returns a vector with list with 
#' @param x Vector of keys.
#' @param hash_name Hash from hash package.
#' @param list_mode Should the return be a list? Could be usefull if retrieveing from hashes, created with create_hash_link_for_multiple_values_per_key.r
#' @return Vector/2d list of values obtained from hash.
#' @export
#' @importFrom hash hash
#' @importFrom hash has.key



retrieve_hash<-function(x,hash_name,list_mode=FALSE) {
	if (list_mode) {
		retrieved<-vector(length=length(x),mode="list")
	} else {
		retrieved<-vector(length=length(x))
	}
	for (i in 1:length(x)) {
		if (hash::has.key(x[i],hash_name)) {
			if (list_mode) {
				retrieved[[i]]<-hash_name[[x[i]]]
			} else {
				retrieved[i]<-hash_name[[x[i]]]
			}
		} else {
		print(paste0("No value found for key:",x[i]))
		}
	}
	return(retrieved)
}