make_cross_folds=function(table, nfolds, times=1, na.action="mutate"){
	dims=dim(table);
	nrows=dims[1];
	df=fix_df(df, na.action);
	rows=1:nrows;
	rows=sample(rows, nrows);

	fold_size=floor(nrows / nfolds);
	last_fold_row=nfolds * fold_size;
	extra_start=last_fold_row + 1;
	extra_end=nrows;

	cross_folds=list();
	for (i in 1:nfolds){
		for(q in 1:times){

			fold_end=fold_size * i;
			fold_start=1 + fold_end - fold_size;

			fold_plus_extra=c(fold_start:fold_end, extra_start:extra_end);
			test_rows=rows[fold_plus_extra];
			train_rows=setdiff(rows, test_rows);
			cross_folds[[i]]=list(train_rows, test_rows);
		}
	}
	cross_folds;
}


predict_knn=function(df, ..., color,k=10, nfold=10, na.action="mutate"){
	vars=enquos(...);
	color=enquo(color);
	df=fix_df(df, na.action);
	cross_fold=make_cross_folds(df, nfold, na.action="");
	train_rows=cross_fold[[1]][[1]];
	test_rows=cross_fold[[1]][[2]];
	space_table=df%>%
		select(!!!vars);
	fold_train=space_table%>%
		slice(train_rows);
	class_table=df%>%
		select(!!color);
	fold_classes=class_table%>%
		slice(train_rows);
	fold_classes=fold_classes[[1]];
	fold_classes=factor(fold_classes);
	fold_test=space_table%>%
		slice(test_rows);
	knn(fold_train, fold_test, fold_classes, k);
}

predict_knn_error=function(df, folds, ..., color,k=10, na.action="mutate"){
	vars=enquos(...);
	color=enquo(color);
	df=fix_df(df, na.action);
	space_table=df%>%
		select(!!!vars);
	class_table=df%>%
		select(!!color);
	err=0;
	for(cross_fold in folds){
		train_rows=cross_fold[[1]];
		test_rows=cross_fold[[2]];
		fold_train=space_table%>%
			slice(train_rows);
		fold_classes=class_table%>%
			slice(train_rows);
		fold_classes=fold_classes[[1]];
		fold_classes=factor(fold_classes);
		fold_test=space_table%>%
			slice(test_rows);
		predicted=knn(fold_train, fold_test, fold_classes, k);
		# Test
		actual=class_table%>%
			slice(test_rows);
		err=err+reduce2(
			predicted,
			actual[[1]],
			.f=function(acc,p,a)acc+(p==a),
			.init=0
		)/length(predicted);
	}
	err/length(folds);
}

plot_knn_err=function(df, ..., color,k=1:50, nfold=10, times=1, na.action="mutate"){
	vars=enquos(...);
	color=enquo(color);
	df=fix_df(df, na.action);
	folds=make_cross_folds(df, nfold, times=times, na.action="");
	err=c();
	min_k=min(k)-1;
	for(i in k){

		err[i-min_k]=predict_knn_error(
			df, folds, !!!vars, color=!!color, k=i
		)
	}
	df=tibble(k=k, err=err);
	print(
		ggplot(df, aes(k, err))+
			geom_point()+geom_line()
	);
	df;
}


predict_knn_errors=function(df, folds, ..., color,k=10, na.action="mutate"){
	vars=enquos(...);
	color=enquo(color);
	df=fix_df(df, na.action);
	space_table=df%>%
		select(!!!vars);
	class_table=df%>%
		select(!!color);
	err=c();
	i=1L;
	for(cross_fold in folds){
		train_rows=cross_fold[[1]];
		test_rows=cross_fold[[2]];
		fold_train=space_table%>%
			slice(train_rows);
		fold_classes=class_table%>%
			slice(train_rows);
		fold_classes=fold_classes[[1]];
		fold_classes=factor(fold_classes);
		fold_test=space_table%>%
			slice(test_rows);
		predicted=knn(fold_train, fold_test, fold_classes, k);
		# Test
		actual=class_table%>%
			slice(test_rows);
		err[i]=reduce2(
			predicted,
			actual[[1]],
			.f=function(acc,p,a)acc+(p==a),
			.init=0
		)/length(predicted);
		i=i+1;
	}
	err;
}


plot_knn_errbar=function(df, ..., color,k=1:200, nfold=10, times=10, na.action="mutate"){
	# Bad setings warning
	if(max(k)-min(k)<=30){
		warning("Best k value may be inacurate due to limited checks");
	}
	if(times<=5){
		warning("Result likely differs between runs, increase times to increase accuracy");
	}
	if(nfold<=5){
		warning("nfold is low, consider increasing nfold to 10 (Few train values)");
	}
	else if(nfold>=20){
		warning("nfold is high, consider decreasing nfold to 10 (Few test values)");
	}
	vars=enquos(...);
	color=enquo(color);
	df=fix_df(df, na.action);
	folds=make_cross_folds(df, nfold, times=times, na.action="");
	err=tibble(k=integer(), err=double());
	for(i in k){
		err=err%>%
			add_row(
				err=predict_knn_errors(
					df, folds, !!!vars, color=!!color, k=i, na.action=""
				),
				k=i
			);
	}
	# Get stats
	sum=err%>%
		group_by(k)%>%
		summarise(avg=mean(err), max=max(err), min=min(err));
	# Get min
	min_avg=(
		sum%>%
			summarise(min=min(avg))
	)$min[1];
	min_avg=sum%>%
		filter(avg==min_avg)%>%
		select(k, avg);

	# ggplot
	plot=ggplot()+
		geom_point(data=err, aes(k, err))+
		geom_point(data=sum, aes(k, avg), color="green")+
		geom_line(data=sum, aes(k, avg), color="green")+
		geom_point(data=sum, aes(k, max), color="red")+
		geom_line(data=sum, aes(k, max), color="red")+
		geom_point(data=sum, aes(k, min), color="blue")+
		geom_line(data=sum, aes(k, min), color="blue")+
		labs(
			title=paste("knn best k-value:", min_avg$k, collapse=", "),
			subtitle=paste0("error: ", min_avg$avg[1]*100,"%")
		)
	# Quick warning checks
	if(max(min_avg$k)>max(k)*.9){
		warning("k value close to max, recomend expanding max range");
	}
	if(min(min_avg$k)<min(k)*1.1){
		warning("k value close to min, recomend expanding min range");
	}
	list(err=err, sum=sum, plot=plot, min_avg);
}

fix_df=function(df, action){
	action=tolower(action);
	if(action=="mutate"){
		return(
			df%>%
				mutate(
					!!color:=factor(
						if_else(is.na(!!color),"", as.character(!!color))
					)
				)
		);
	}
	if(action=="remove"){
		return(
			df%>%
				filter(!is.na(!!color))
		);
	}
	return(df);
}
