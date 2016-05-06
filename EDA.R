### Read Data
    require(ggplot2)
    require(dplyr)
    require(reshape2)
    require(longitudinal)
    setwd("~/Documents/GitHub/")
    load(file="Method_Project_Data/Clean_Data_Narrow.Robj") 
    load(file="Method_Project_Data/Clean_Data.Robj") 
    load(file="Method_Project_Data/Clean_Data_Wide.Robj") 

### Summary Statistics
    # 58997 Loci
    length(unique(df$locus))
    # all 23 chromosomes
    length(unique(df$chr))
    # signal summary stats
    summary(df$signal)
    table(df$cell)
    table(df$signal.type)
    table(df$sample)
    
### Check to see if samples are iid
    df2$diffs <- with(df2,  sample_1 - sample_2)
    tapply(df2$diffs, FUN=mean, INDEX = df2[c("signal.type","cell")])
    ggplot(df2) +
        geom_density(aes(x=diffs, fill=signal.type), alpha=0.25) + 
        facet_grid(signal.type~cell) + xlim(c(-20,20)) 
    
### Graphics
    # we will use log(signal for EDA)
    df$lsignal  <- log(df$signal)
    ggplot(df) + geom_density(aes(x=lsignal, fill=signal.type), alpha=0.5) +
        facet_wrap("signal.type")
    ggplot(df) + geom_density(aes(x=lsignal, fill=signal.type), alpha=0.5) +
        facet_grid(signal.type~cell)
    ggplot(df) +
        geom_density(aes(x=lsignal, fill=cell), alpha=0.4)+
        facet_wrap("chr")

### Lengths, Start/Stop
ggplot(df)+
    geom_density(aes(x=start, fill=chr))+
    facet_wrap("chr")

### Signals By Location
chrom <- paste("chr", c(1:22, "X"), sep="")
for(i in chrom){
    x <- filter(df, chr==i)
    pdf(file = paste("signal_by_location_",i,".pdf", sep=""),
        width=12, height=12, onefile=T)
    print(
        ggplot(x)+
        geom_point(aes(x=start, y=signal, color=signal.type), alpha=0.25) + 
        facet_grid(signal.type~cell, scales="free_y", space="fixed")+
        ggtitle(i)
    )
    dev.off()
    cat("yay", i)
}

### Look at just one small location on the genome
x <- filter(df, chr=="chr1", locus<100)
ggplot(x)+
    geom_point(aes(x=start, y=signal, color=signal.type), alpha=0.25) + 
    facet_grid(signal.type~cell, scales="free_y", space="fixed")+
    geom_smooth(aes(x=start, y=signal, color=signal.type))

### Look at autocorrelation along genome
par(mfrow=c(2,2))
x <- filter(data, chr=="chr1")
series.data <- as.matrix(as.vector(t(as.matrix(select(x, K562_H3K4me1_BR1, K562_H3K4me1_BR2)))))
one.series <- as.longitudinal(x=series.data, repeats=2, time=x$start)
acf(one.series, lag.max = 200, type = c("correlation"), main="Autocorr K562 H3K4me1 chr1")

x <- filter(data, chr=="chr1")
series.data <- as.matrix(as.vector(t(as.matrix(select(x, Huvec_H3K4me1_BR1, Huvec_H3K4me1_BR2)))))
one.series <- as.longitudinal(x=series.data, repeats=2, time=x$start)
acf(one.series, lag.max = 200, type = c("correlation"), main="Autocorr Huvec H3K4me1 chr1")

x <- filter(data, chr=="chr1")
series.data <- as.matrix(as.vector(t(as.matrix(select(x, K562_Control_BR1, K562_Control_BR2)))))
one.series <- as.longitudinal(x=series.data, repeats=2, time=x$start)
acf(one.series, lag.max = 200, type = c("correlation"), main="Autocorr K562 Control chr1")

x <- filter(data, chr=="chr1")
series.data <- as.matrix(as.vector(t(as.matrix(select(x, Huvec_Control_BR1, Huvec_Control_BR2)))))
one.series <- as.longitudinal(x=series.data, repeats=2, time=x$start)
acf(one.series, lag.max = 200, type = c("correlation"), main="Autocorr Huvec Control chr1")


acf(x.ts,100)
head(data)

### Correlation between signal types
ggplot(data)+
    geom_point(aes(x=K562_Control_BR, y=K562_Control_BR2))
ggplot(data)+
    geom_point(aes(x=K562_Control_BR1, y=Huvec_Control_BR1), alpha=0.10)+
    ylim(c(0,10)) + xlim(c(0,20))
ggplot(df) +
    geom_point(aes)

### Correlation between cell measurements
ggplot(df3) +
    geom_point(aes(x=Huvec_1, y=Huvec_2, col=signal.type), alpha=0.25)+
    facet_wrap("signal.type", scales="free")
ggplot(df3) +
    geom_point(aes(x=K562_1, y=K562_2, col=signal.type), alpha=0.25)+
    facet_wrap("signal.type", scales="free")
