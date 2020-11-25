

Class_Intervals=function(x, breaks){

  
#Get the residuals....
  
Res_PCM=read.csv(file=paste(path_output, "Residuals_PCM.csv", sep=""), header=TRUE, sep=",")[,-1]
 Col_Res_PCM=read.csv(file=paste(path_output, "Residuals_PCM.csv", sep=""), header=TRUE, sep=",")[,1]
 #colnames(Res_PCM)=paste(colnames(Res_PCM), "_res", sep="")

if(substr(Col_Res_PCM,1,1)[1]=="P"){
  IN=as.numeric(substr(Col_Res_PCM,2,nchar(as.character(Col_Res_PCM))))
}else{
  IN=Col_Res_PCM 
}


T=rowSums(x[which(rownames(x)%in%IN==TRUE),],na.rm=TRUE)


S = quantile(T, probs=seq(0,1, 1/breaks), na.rm=TRUE)
N = breaks-1
vec = c("<","<=")
lst = lapply(numeric(N), function(x) vec)
Gri=as.matrix(expand.grid(lst))
Grid=cbind(Gri, rep("<=",nrow(Gri)))
Mat=matrix(rep(S[2:(breaks+1)], nrow(Grid)), nrow=nrow(Grid), byrow=TRUE)

P=function(x,y){return(paste(x,y,sep=""))}

Intervals=matrix(mapply(P, Grid, Mat), ncol=breaks, byrow=FALSE)

Dmat=matrix(rep(T,nrow(Grid)), nrow=nrow(Grid), byrow=TRUE)

###here save the group intervals

Detect=function(x,y){
 #x=Dmat[,6]
#y=Intervals[6,]
 

  Dbound=outer(x,y,paste)
  Eval_Pars=function(Z){eval(parse(text=Z))}
  Where=apply(Dbound, c(1,2), Eval_Pars)
  Position=function(x){return(min(which(x%in%1), na.rm=TRUE))}
  return(apply(Where,1, Position))
  
}

what=list()
for(i in 1:nrow(Dmat)){
  what[[i]]=Detect(Dmat[i,],Intervals[i,])
}

sol=do.call(rbind,what)
k=apply(sol,1,table)


if(class(k)=="matrix"){
Grouping=min(which(apply(k,2,var)==min(apply(k,2,var, na.rm=TRUE))))
}else{
  take_in=which(lapply(k, length)==breaks)
  K=k[take_in]
  Grouping=take_in[min(which(lapply(K,var)==min(unlist(lapply(K,var, na.rm=TRUE)))  ))  ]
}
CI=sol[Grouping,]  

return(CI)


}