################################################################################
# 													Visulization functions
################################################################################
# See the nth chunk of 20 rows
chunk=function(df,index=1, start=1, end=20){
	slice(df,(start:end)+end*index)
}

# See the nth chunk of 10 rows
see=function(df,index=1, start=1, end=10){
	slice(df,(start:end)+end*index)
}


# See the n1st to n2ed chunk of 20 rows
chunks=function(df,first=1, last=3, start=1, end=20){
	i=first;
	while(i<=last){
		i=i+1;
		print(chunk(df, i, start=start, end=end));
	}
}

# Pan over, freese columns with frozen (Keep them to the left)
panH=function(df, index=2, start=1, end=8, frozen=NULL){
	frozen=enquos(frozen);
	arr=Filter(function(x)x<length(df),(start:end)+end*(index-1));
	select(df, !!!frozen, arr);
}

# Pan over, freese columns with frozen (Keep them to the left)
pan=function(df, index=2, start=1, end=12, frozen=NULL){
	frozen=enquos(frozen);
	arr=Filter(function(x)x<length(df),(start:end)+end*(index-1));
	select(df,!!!frozen, arr);
}

# Quick function to view the contents
v=function(.){
	View(.)
}

