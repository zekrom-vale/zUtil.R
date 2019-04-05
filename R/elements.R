################################################################################
#																	Elements
################################################################################
# See how at https://dplyr.tidyverse.org/articles/programming.html
# This function returns thae values that are not distinct
# List the columns you want to get the rows that are not distinct
not_distinct=function(df, ...){
	nd_vars=enquos(...);
	df%>%
		group_by(
			!!!nd_vars
		)%>%
		filter(
			reduce(!!!nd_vars, function(a,x)a&(n()>1), .init=TRUE)
		);
}
# Legacy alternative function using distinct and %in%
not_distinct_=function(df, col, ...){
	nd_vars=enquos(col,...);
	nd_col=enquo(col);

	dups=(
		df%>%
			setdiff(
				df%>%
					distinct(!!!nd_vars, .keep_all=TRUE)
			)
	)%>%select(!!nd_col);

	df%>%
		filter(
			!!nd_col%in%dups[[1]]
		);
}

# Filter where NA
# ... is for the columns
filter_na=function(df, ...){
	names=map(unlist(enquos(...)), .f=function(x)quo(is.na(!!x)));
	filter(
		df, !!!names
	);
}

# Filter where not NA
# ... is for the columns
filter_not_na=function(df, ...){
	names=map(unlist(enquos(...)), .f=function(x)quo(!is.na(!!x)));
	filter(
		df, !!!names
	);
}

# Filter when it detects the string
# Use <Column to Test>=<Value to Detect>
# .op defults to `&`
where=function(df, ..., .negate=FALSE, .op=NULL){
	names=quos(..., .named=TRUE);
	if(.negate){
		names=map2(
			names,
			names%>%names()%>%syms(),
			.f=function(val, name)quo(!str_detect(!!name,!!val))
		);
	}
	else{
		names=map2(
			names,
			names%>%names()%>%syms(),
			.f=function(val, name)quo(str_detect(!!name,!!val))
		);
	}
	if(is.null(.op)){
		names=unlist(names, use.names=FALSE);
		filter(df, !!!names);
	}
	else{
		filter(
			df,
			reduce(
				list(!!!names),
				.f=.op
			)
		)
	}
}

# function(){
# 	lv=.init;
# 	print(names);
# 	for(i in names){
# 		.op(!!i, lv);
# 	}
# 	return(lv);
# }

# reduce(
# 	names,
# 	.f=function(acc, cur){
# 		print(cur);
# 		.op(acc, !!cur); # Why does this not work!?
# 	},
# 	.init=.init
# )
