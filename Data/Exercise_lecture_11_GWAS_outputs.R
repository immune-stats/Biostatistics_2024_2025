library(gap)

library(MASS)

output.gwas<-read.csv('~/Desktop/MiNI_PW/2024_2025_Biostatistics_Winter/Data/Data_Lecture_11_GWAS.csv',header=T)

num.SNPs<-dim(output.gwas)[1]

colors<-c()

color.palette<-rainbow(23)

tabela<-table(output.gwas$CHR)

colors<-rep(NA,num.SNPs)

chr.annot<-c()

for(i in 1:23){

	new.end<-sum(tabela[1:i])
	
	if(i!=1)new.begin<-sum(tabela[1:(i-1)]) else new.begin<-1
	
	colors[(new.begin+1):new.end]<-rep(color.palette[i])	
	
	chr.annot<-c(chr.annot,(new.begin+new.end)/2)
	
}

chromosomes<-c(1:23)

table(colors)

summary(-log10(output.gwas[,'P']))

par(bg='white')

plot(-log10(output.gwas[,'P']),cex=0.5,pch=19,col=as.character(colors),las=1,ylim=c(0,15),ylab=expression(-log[10]('p-value')),xlab='chromosome',axes=F,main='')

box(lwd=1.5)

axis(1, chr.annot, chromosomes)

axis(2,seq(0,15,0.1),rep('',151),tcl=-0.25)

axis(2,0:15,lwd=1.5,las=1)

abline(h=-log10(0.05/num.SNPs),lty=2)

text(1,-log10(0.05/num.SNPs)+0.5,'Bonferroni',adj=0)

qqunif(output.gwas[,'P'],main='Q-Q plot of Uniform distribution',ylim=c(0,8.1),xlim=c(0,6),las=1)

abline(a=0,b=1)

min(output.gwas[,'P'])

adj.pvalues.bh<-p.adjust(output.gwas[,'P'],method='BH')

plot(-log10(adj.pvalues.bh),cex=0.5,pch=19,col=as.character(colors),las=1,ylim=c(0,5),ylab=expression(-log[10]('p-value')),xlab='chromosome',axes=F,main='')

box(lwd=1.5)

axis(1, chr.annot, chromosomes)

axis(2,seq(0,15,0.1),rep('',151),tcl=-0.25)

axis(2,0:15,lwd=1.5,las=1)

abline(h=-log10(0.05),lty=2)

text(1,-log10(0.05)+0.5,'FDR',adj=0)