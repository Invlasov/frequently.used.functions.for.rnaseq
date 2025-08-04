paste_the_insides_of_a_list_with_separator<-function(input.list,input.sep=", ") { 
		output<-vector(mode="character",length=length(input.list))
		for (i in 1:length(input.list)) {
			output[i]<-paste(input.list[[i]],sep=input.sep,collapse=input.sep)
		}
	return(output)
	}

