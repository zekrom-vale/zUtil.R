################################################################################
#																		Sets
################################################################################

# Count if matching a string
count_when=function(vector, value){
	reduce(
		vector,
		.f=function(accumulaton, current){
			accumulaton+if_else(
				is.na(current),
				0L,
				as.integer(str_detect(current,value))
			)
		},
		.init=0L
	)
}

# Count only rows that match the condition
count_if=function(vector, condition){
	reduce(
		vector,
		.f=function(accumulaton, current){
			accumulaton+if_else(
				is.na(current),
				0L,
				as.integer(condition(current))
			)
		},
		.init=0L
	)
}
# Allows the use of multiple values in a dataframe
# ... is for conditions
# .group is passed to count as ...
# .wt and .sort is passed to count / tally as wt and sort (Resp)
count_ifs=function(df, ..., .group=NULL, .wt=NULL, .sort=FALSE){
	vars=quos(...);
	if(is.null(.wt)){
		if(is.null(.group)){
			(
				tally(
					filter(df, !!!vars),
					sort=.sort
				)
			)$n;
		}
		else{
			.group=enquos(.group);
			(
				count(
					filter(df, !!!vars),
					!!!.group,
					sort=.sort
				)
			)$n;
		}
	}
	else{
		if(is.null(.group)){
			(
				tally(
					filter(df, !!!vars),
					wt=.wt,
					sort=.sort
				)
			)$n;
		}
		else{
			.group=enquos(.group);
			(
				count(
					filter(df, !!!vars),
					!!!.group,
					wt=.wt,
					sort=.sort
				)
			)$n;
		}
	}
}


# length=length(df[[1]]);
# row=1;
# while(row<=length){
#
# 	row=row+1;
# }

# auto union, slices the tables down to match to union
auto_union=function(x,y){
	names=syms(
		intersect(
			names(x),
			names(y)
		)
	);
	names=map(names, function(x)enquo(x));
	union(
		select(x, !!!names),
		select(y, !!!names)
	)
}
# Automatic set difference
auto_setdiff=function(x,y){
	names=syms(
		intersect(
			names(x),
			names(y)
		)
	);
	names=map(names, function(x)enquo(x));
	setdiff(
		select(x, !!!names),
		select(y, !!!names)
	)
}

symdiff=function(A, B, ...){
	setdiff(
		union(
			A, B, ...
		),
		intersect(
			A, B, ...
		)
	);
}
