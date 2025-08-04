do_limma_drop_unestimable<-function(count_matrix,design_matrix,output_file_result,output_file_degs,local_ensembl_annotation,contrast_num=2,threshold_fc=1.5,threshold_pval=0.05,method_string="hommel",output_plot_path,pre_filtration=TRUE,do_fc_filtration=TRUE) {	
     require(limma)
	 require(edgeR)

	test<-capture.output(do_limma(count_matrix=count_matrix,design_matrix=design_matrix,output_file_result=output_file_result,
	output_file_degs=output_file_degs,local_ensembl_annotation=local_ensembl_annotation,contrast_num=contrast_num,threshold_fc=threshold_fc,threshold_pval=threshold_pval,method_string=method_string,
	output_plot_path=output_plot_path,pre_filtration=pre_filtration,do_fc_filtration=do_fc_filtration))
	if (!(identical(test,character(0)))) {
		non_estimable_string<-separate_last_part(test[1],": ")
		non_estimable<-unlist(strsplit(non_estimable_string," "))
		msg<-paste0("Not estimable factors:",non_estimable_string,", droping them and reruning limma.",collapse=", ")
		print(msg)
		design_matrix<-design_matrix[,!(colnames(design_matrix)%in% non_estimable)]
		unlink(c(output_path,output_path_degs,output_plot_path))
		do_limma(count_matrix=count_matrix,design_matrix=design_matrix,output_file_result=output_file_result, 
		output_file_degs=output_file_degs,local_ensembl_annotation=local_ensembl_annotation,contrast_num=contrast_num,
		threshold_fc=threshold_fc,threshold_pval=threshold_pval, method_string=method_string,output_plot_path=output_plot_path,
		pre_filtration=pre_filtration,do_fc_filtration=do_fc_filtration)
	}
}
