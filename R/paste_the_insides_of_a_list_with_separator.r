#' This function converts 2d list into 1d vector pasted insides. Yes, probably you can also do this with lapply.
#' @param input.list List to go through and paste.
#' @param input.sep Separator to paste with.
#' @export
#' @return Vector of characters with pasted insides.


paste_the_insides_of_a_list_with_separator<-function(input.list,input.sep=", ") { 
		output<-vector(mode="character",length=length(input.list))
		for (i in 1:length(input.list)) {
			output[i]<-paste(input.list[[i]],sep=input.sep,collapse=input.sep)
		}
	return(output)
	}

