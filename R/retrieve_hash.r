#this function gets a number of hash keys one by one in specified order
retrieve_hash<-function(x,hash_name) {
	require(hash)
	retrieved<-vector(length=length(x))
	for (i in 1:length(x)) {
		if (hash::has.key(x[i],hash_name)) {
		retrieved[i]<-hash_name[[x[i]]]
		} else {
		print(paste0("No value found for key:",x[i]))
		}
	}
	return(retrieved)
}