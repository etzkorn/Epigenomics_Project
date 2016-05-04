### Read Data
    require(ggplot2)
    require(dplyr)
    setwd("~/Documents/GitHub/Methods_Project/")
    df <- read.csv(file="Clean_Data.csv") # use long data in ggplot
    df2 <- read.csv(file="Clean_Data_Wide.csv") # use long data in ggplot
    
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

### Correlation between signal types
ggplot(data)+
    geom_point(aes(x=K562_Control_BR, y=K562_Control_BR2))
ggplot(data)+
    geom_point(aes(x=K562_Control_BR1, y=Huvec_Control_BR1), alpha=0.10)+
    ylim(c(0,10)) + xlim(c(0,20))
ggplot(df) +
    geom_point(aes)

### Correlation between cell measurements
ggplot(df2) +
    geom_point(aes(x=Huvec_1, y=Huvec_2, col=signal.type), alpha=0.25)+
    facet_wrap("signal.type", scales="free")
ggplot(df2) +
    geom_point(aes(x=K562_1, y=K562_2, col=signal.type), alpha=0.25)+
    facet_wrap("signal.type", scales="free")
