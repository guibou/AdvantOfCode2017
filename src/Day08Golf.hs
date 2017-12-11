main=interact$show.maximum.(\r->map(l r)$fst<$>r).foldl f[].map words.lines
z=read
l r o=sum[v|(k,v)<-r,k==o]
f m[a,b,c,_,e,f,g]|(case f of"<="->(>=);"<"->(>);">="->(<=);">"->(<);"=="->(==);"!="->(/=))(z g)$l m e=(a,if b=="inc"then z$c else-(z c)):m|1>0=m
