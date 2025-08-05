#' This is rowttest function for use with "apply" on data frames.
#' It takes vector of values and position of experiments and controls in vector and returns a single p.val
#' @param input Numeric vector. Values to run t.test on
#' @param experiments Numeric vector. Position of experiment values in an input vector.
#' @param controls Numeric vector. Position of control values in an input vector.
#' @param paired Boolean. Should t-test be paired or unpaired?
#' @importFrom stats t.test
#' @return Vector of p.values per row.
#' @export

rowttest<-function(input,experiments,controls,paired=FALSE) {
	output<-tryCatch(
		{
			intermediate<-t.test(x=input[experiments],y=input[controls],paired=paired)
			return(intermediate$p.value)
		},
		error=function(cond) {
			message("Here's the original error message:")
            message(cond)
			message("\n")
			if (intermediate$p.value) {return(1)}
			else (return(NA))
			},
		warning=function(cond) {
			message(paste("T-Test caused a warning at"))
            message("Here's the original warning message:")
            message(cond)
			
		},
			finally={}
	)
}
