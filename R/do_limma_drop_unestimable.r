#' A wrapper around limma that automatically reruns it while dropping unestimable variables from design matrix.
#' @param design_matrix Numeric matrix. A design matrix, with smaples in row matching columns of count and and variables in columns. Should be generated through model.matrix from stats. Passes it into
#' @param output_file_result A path to a file. FC and p.val for chosen contrast for each expressed gene are going to be written in the file with that path
#' @param output_file_degs A path to a file. FC and p.val for only degs (if any) for each expressed gene are going to be written in the file with that path
#' @param output_plot_path A path to a file. If not NA - a plot of mena-variance trend will be written there.
#' @param ... Other arguments you need to pass to do_limma
#' @return Writes outputs to the designated paths
#' @export


do_limma_drop_unestimable<-function(design_matrix,output_file_result,output_file_degs,output_plot_path,...) {	

	test<-capture.output(do_limma(design_matrix=design_matrix_dropped,output_file_result=output_file_result,output_file_degs=output_file_degs,output_plot_path=output_plot_path,...))
	if (!(identical(test,character(0)))) {
		non_estimable_string<-separate_last_part(test[1],": ")
		non_estimable<-unlist(strsplit(non_estimable_string," "))
		msg<-paste0("Not estimable factors:",non_estimable_string,", droping them and reruning limma.",collapse=", ")
		print(msg)
		design_matrix_dropped<-design_matrix[,!(colnames(design_matrix)%in% non_estimable)]
		unlink(c(output_file_result,output_file_degs))
		if (!is.na(output_plot_path)) {
			unlink(c(output_plot_path))
		}
		do_limma(design_matrix=design_matrix_dropped,output_file_result=output_file_result,output_file_degs=output_file_degs,output_plot_path=output_plot_path,...)
	}
}
