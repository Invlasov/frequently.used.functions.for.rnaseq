#this is rowttest function for use with "apply" on data frames.
#it takes vector of values and position of experiments and controls in vector. and returns either p.val of t test
rowttest<-function(input,experiments,controls,p.val=TRUE,paired=FALSE) {
	output<-tryCatch(
		{
			intermediate<-t.test(x=input[experiments],y=input[controls],paired=paired)
			return(intermediate$p.value)
		},
		error=function(cond) {
			message("Here's the original error message:")
            message(cond)
			message("\n")
			if (p.val) {return(1)}
			else (return(NA))
			},
		warning=function(cond) {
			message(paste("T-Test caused a warning at"))
            message("Here's the original warning message:")
            message(cond)
			
		},
			finally={}
	)
	return(output)
}
