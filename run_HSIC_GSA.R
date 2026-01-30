library(sensitivity)

rm(list=ls())

############################################################
## Load DATA
############################################################
NOM = "GujanMestras"
load(paste0(NOM,"-","DomingosT2.82m.RData"))

ireseau = 3 ## lead time number ireseau (higher means small lead time)
print(ireseau)
load(file=paste0("./",NOM,"-",tempete,"-ireseau",ireseau,"_MC100.RData"))

## formating of inputs
OU = which(RR[,2] == 2)
RR = RR[OU,]
RR = RR[,-2]

## formating of the output (takes some time)
hat = NULL
for (imc in 1:nrow(RR)){
	hat0 = abs(HE.HAT[[OU[imc]]][1,]-truth) ## absolute difference with the reference solution
	hat = rbind(hat,hat0)
}

############################################################
## HSIC-GSA
############################################################
## parametrisation of the HSIC kernel for the output
kernelY <- list(method="PCA", 
                data.centering=TRUE, data.scaling=FALSE,
                fam="rbf", expl.var=0.90, combi="sum", position="extern")

## parametrisation of the HSIC kernel for the intputs
sensi <- sensiHSIC(model=NULL, X = RR,
			 kernelX=c(
					rep("categ_anova",ncol(RR))),
					anova = list(obj = "FO", is.uniform = FALSE),
					paramX=NA,
					kernelY=kernelY,
					nboot = 0, conf = 0.90 ## control here the bootstrap
					)
## compute HSIC First order indices
ss = tell(sensi, y=data.frame(hat))

## formating for plotting
rownames(ss$FO) = rownames(ss$S) = c("Correction","Training","AE model","Ensemble","Gp_SWL","GP_Z")
plot(ss,which="FO")

