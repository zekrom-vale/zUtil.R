cf=function(table, nfolds, times=1){
	dims=dim(table);
	nrows=dims[1];
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

predict_knn_=function(df, folds, ..., color,k=10, na.action="mutate", l=0, prob=FALSE, use.all=TRUE){
	vars=enquos(...);
	color=enquo(color);
	df=fix_df(df, vars, color, na.action);
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
		predicted=knn(fold_train, fold_test, fold_classes, k, l=l, prob=prob, use.all=use.all);
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

knn_cv_=function(df, ..., color,k=1:50, nfold=10, times=1, na.action="mutate", l=0, prob=FALSE, use.all=TRUE){
	vars=enquos(...);
	color=enquo(color);
	df=fix_df(df, vars, color, na.action);
	folds=cf(df, nfold, times=times);
	err=c();
	min_k=min(k)-1;
	for(i in k){

		err[i-min_k]=predict_knn_(
			df, folds, !!!vars, color=!!color, k=i, l=l, prob=prob, use.all=use.all
		)
	}
	df=tibble(k=k, err=err);
	print(
		ggplot(df, aes(k, err))+
			geom_point()+geom_line()
	);
	df;
}


predict_knn=function(df, folds, ..., color,k=10, na.action="mutate", l=0, prob=FALSE, use.all=TRUE){
	vars=enquos(...);
	color=enquo(color);
	df=fix_df(df, vars, color, na.action);
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
		predicted=knn(fold_train, fold_test, fold_classes, k, l=l, prob=prob, use.all=use.all);
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


knn_cv=function(df, ..., color,k=1:200, nfold=10, times=10, na.action="mutate", l=0, prob=FALSE, use.all=TRUE){
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
	df=fix_df(df, vars, color, na.action);
	err=tibble(k=integer(), err=double(), facet=character());
	levels=lapply(
			select(df, !!!groups(df)), function(x){
				l=levels(x);
				if(is.null(l))warning(paste0("Ignoring group: ", x));
				l;
			}
		);
	levels=cross(Filter(function(x)!is.null(x),levels));
	process=function(df, facet){
		folds=cf(df, nfold, times=times);
		for(i in min(k):min(length(df[[1]]),max(k))){
			err=err%>%
				add_row(
					err=predict_knn(
						df, folds, !!!vars, color=!!color, k=i,
						na.action="", l=l, prob=prob, use.all=use.all
					),
					k=i,
					facet=facet
				);
		}
		err;
	}
	if(length(levels)==0)err=process(df,"")
	else{
		for(i in levels){
			df2=df;
			for(name in names(i)){
				q=enquo(sym(name));
				df2%>%
					filter(!!q==i[[name]]);
			}
			err=process(df, paste(i, collapse="|"));
		}
	}
	# Get stats
	sum=err%>%
		group_by(k)%>%
		summarise(avg=mean(err,na.rm=TRUE), max=max(err,na.rm=TRUE), min=min(err,na.rm=TRUE));
	# Get min
	min_avg=(
		sum%>%
			summarise(min=min(avg, na.rm=TRUE))
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
		facet_wrap(~facet)+
		labs(
			title=paste0(
				"knn best k-value: ",
				paste(
					min_avg$k,
					collapse=", "
				)
			),
			subtitle=paste0("error: ", min_avg$avg[1]*100,"%")
		)
	# Quick warning checks
	if(max(min_avg$k)>max(k)*.9){
		warning("k value close to max, recomend expanding max range");
	}
	if(min(min_avg$k)<min(k)*1.1){
		warning("k value close to min, recomend expanding min range");
	}
	list(err=err, sum=sum, plot=plot, min=min_avg);
}

fix_df=function(df, vars, color, action){
	action=tolower(action);

	if(action=="mutate"){
		return(
			df%>%
				mutate_at(
					vars(!!color, !!!vars),
					function(x){
						if(is.numeric(x))return(
							if_else(is.na(x), 0, as.numeric(x))
						)
						if(is.factor(x))return(
							factor(if_else(is.na(x), "", as.character(x)))
						)
						if(is.character(x))return(
							if_else(is.na(x), "", x)
						)
						x;
					}
				)
		);
	}
	if(action=="mutate|remove"){
		return(
			df%>%
				mutate(
					color:=factor(
						if_else(is.na(!!color),"", as.character(!!color))
					)
				)%>%
				filter_at(
					vars(!!!vars),
					all_vars(!is.na(.))
				)
		);
	}
	if(action=="remove"){
		return(
			df%>%
				filter_at(
					vars(!!color, !!!vars),
					all_vars(!is.na(.))
				)
			);
	}
	return(df);
}


lm_cv=function(df, formula, nfolds = 10, times=2, cutoff=0.5){
	cross_folds=cf(df, nfolds, times);
	err=c();
	for(i in 1:nfolds){
		err[i]=lm_regression_error(df, formula, cross_folds[[i]], cutoff)
	}
	err;
}

lm_regression_error=function(df, formula, cross_fold, cutoff=0.5){
	factor=sym(lhs.vars(formula));
	actual_values=(
		df%>%
			slice(cross_fold[[2]])%>%
			select(!!factor)
	)[[1]];
	predictions=predict(
		lm(
			formula,
			df%>%
				slice(cross_fold[[1]])
		),
		df%>%
			slice(cross_fold[[2]])
	);
	predictions=if_else(predictions > cutoff, 1, 0);
	errors=abs(actual_values - predictions);
	mean(errors[!is.na(errors)]);
}
