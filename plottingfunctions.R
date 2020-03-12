require(ggplot2)

preprocessing <- function(path){
    thisdata <- read.csv(path, header = TRUE)
    # remove undefined data
    thisdata <- thisdata[thisdata$denominazione_provincia != "In fase di definizione/aggiornamento",]
    # get provinces
    provinces <- unique(thisdata$denominazione_provincia)
    # modify date to remove hour
    thisdata$Date <- as.Date(thisdata$data)
    # add increase in reported cases instead of accumulate
    thisdata$new_cases <- NA
    for(thisprovince in provinces){
        thisdata$new_cases[thisdata$denominazione_provincia == thisprovince] <- c(thisdata$totale_casi[thisdata$denominazione_provincia == thisprovince][1], diff(thisdata$totale_casi[thisdata$denominazione_provincia == thisprovince]))
    }
    return(thisdata)
}

regionalplot <- function(data, region, accumulative = TRUE){
    nplots <- length(unique(data$denominazione_provincia[data$denominazione_regione==region]))
    if(accumulative){
        myplot <- ggplot(data=data[data$denominazione_regione == region,], aes(x=Date, y=totale_casi, group = 1))
    }
    else{
        myplot <- ggplot(data=data[data$denominazione_regione == region,], aes(x=Date, y=new_cases, group = 1))
    }
    
    myplot <- myplot+
        geom_line() +
        ggtitle("Increase in Cases by Province") +
        ylab("")+
        xlab("")+
        facet_wrap(~denominazione_provincia, ncol = round(sqrt(nplots)))+
        theme(axis.title.y = element_text(face = "bold", size = 12), axis.title.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face="bold", size=10), axis.text.x = element_text(face="bold", size=10))
    return(myplot)
}

if(interactive()){
    library(plotly)
    # read province data
    datapath <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
    
    thisdata <- preprocessing(datapath)
    # get regions
    regions <- unique(thisdata$denominazione_regione)
    thisregion <- regions[6]
    # plot and save as pdf
    for(thisregion in regions){
        pdf(paste("plots",paste(thisregion,"_region.pdf", sep=""), sep = .Platform$file.sep), width = 16, height = 9)
        myplot <- regionalplot(thisdata, thisregion)
        print(myplot)
        dev.off()
    }
    
}