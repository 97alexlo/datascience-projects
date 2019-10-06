#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
################################################################################################################
################## shiny url: https://shiny.rcg.sfu.ca/u/ala148/shinyapp/ #########################################
################################################################################################################

shinyServer(function(input, output) {

  output$tempPlot <- renderPlot({

    load("CanadianMeanTemp.Rdata")
    bctemp=MeanTemp[which(MeanTemp$`InfoTemp[3]`=='BC'),]
    bctemp1=bctemp[which(bctemp$Annual>-1000),]
    bctemp1=bctemp1[which(bctemp1$Year>=1979),]
    b=NULL
    c=NULL
    i=NULL
    for(i in 1979:2017){
      a<-bctemp1$Annual[which(bctemp1$Year==i)]
      b=sum(a)/length(a)
      c=c(c,b)
    }
    x=data.frame(year=seq(1979,2017),temperature=c)
    
    load("C02NorthernHemisphere.Rdata")
    z<-Co2North[,c(1,12)]
    
    load("CanadianAvgSnow.Rdata")
    bcsnow=AllSnow[which(AllSnow$`InfoTemp[3]`=='BC'),]
    bcsnow1=bcsnow[which(bcsnow$Annual>-1000),]
    e=NULL
    f=NULL
    g=NULL
    k=NULL
    for(k in 1979:2017){
      e<-bcsnow1$Annual[which(bcsnow1$Year==k)]
      f=sum(e)/length(e)
      g=c(g,f)
    }
    y=data.frame(year=seq(1979,2017),snow=g)
    
    if(input$dataSource=="temp"){
    #plot(x[,1],x[,2],main="Average Annual Temperatures in BC", xlab="Year",ylab="Temperature (Celcius)",xlim=c(1979,2017),type="l",
     #    col="orange")
   # legend("bottomright",c("Linear Regression","Loess Regression","Average annual temperature","Long-term average"),lty=c(1,1,1,1),
       #    lwd=c(2,2,2,2),col=c("darkred","blue","orange","darkgrey"))
      p=ggplot(x,aes(x=x[,1],y=x[,2]))+geom_line()+ggtitle("Average Annual Temperatures in BC")+ylab("temp")+xlab("year")+scale_colour_manual(values=c("black"))
      print(p)
    if(input$templin){
      #abline(lm(x[,2]~x[,1]),col="darkred",lwd=2)
      p=p+stat_smooth(colour="darkred",method='lm',se=F)
      print(p)
    }
    if(input$temploess){
      #lines(x[,1],predict(loess(x[,2]~x[,1])),col="blue",lwd=2)
      p=p+geom_smooth(method='loess',se=F,color="blue")
      print(p)
    }
    if(input$tempavg){
      #abline(h=mean(c),col="darkgrey",lwd=2)
      p=p+geom_hline(yintercept=mean(c),colour="darkgrey",size=1.5)
      print(p)
    }

    }
    
    else{
      if(input$dataSource=="co2"){
        plot(z[,1],z[,2], col = 'darkblue',
             main="C02 levels at the 49th parallel North",
             xlab="Year",ylab="C02 ppm",type="l",lwd=2)
        legend("bottomright",c("C02 levels","Linear regression"),lty=c(1,1,1,1),
               lwd=c(2,2),col=c("darkblue","darkred"))
        if(input$co2lin){
          abline(lm(z[,2]~z[,1]),col="darkred",lwd=2)
        }
      }

      if(input$dataSource=="snow"){
        plot(y[,1],y[,2],type="l",main="Average Annual Snowfall in BC",col="darkgreen",xlab="Year",ylab="Snow (cm)",lwd=2)
        legend("topright",c("Linear Regression","Loess Regression","Average annual snowfall"),lty=c(1,1,1),
               lwd=c(2,2,2),col=c("darkred","blue","darkgreen"))
        if(input$snowloess){
          lines(y[,1],predict(loess(y[,2]~y[,1])),col="blue",lwd=2)
        }
        if(input$snowlin){
          abline(lm(y[,2]~y[,1]),col="darkred",lwd=2)
        }
      }
    }
})
  
  output$results<-renderPlot({
    load("CanadianMeanTemp.Rdata")
    bctemp=MeanTemp[which(MeanTemp$`InfoTemp[3]`=='BC'),]
    bctemp1=bctemp[which(bctemp$Annual>-1000),]
    bctemp1=bctemp1[which(bctemp1$Year>=1979),]
    b=NULL
    c=NULL
    i=NULL
    for(i in 1979:2017){
      a<-bctemp1$Annual[which(bctemp1$Year==i)]
      b=sum(a)/length(a)
      c=c(c,b)
    }
    x=data.frame(year=seq(1979,2017),temperature=c)
    
    load("C02NorthernHemisphere.Rdata")
    Co2North$YearDecimal<-as.integer(Co2North$YearDecimal)
    co2data=Co2North[,c(1,12)]
    s=NULL
    i=NULL
    for(i in 1979:2017){
      q<-co2data[,2][which(co2data$Year==i)]
      w=sum(q)/length(q)
      s=c(s,w)
    }
    t=data.frame(year=seq(1979,2017),temperature=s)
    par(mar=c(5, 4, 4, 6) + 0.1)
    plot(x[,1],x[,2],main="Average Annual Temperature and CO2 levels in BC", xlab="Year",ylab="Temperature (Celcius)",xlim=c(1979,2017),type="l",
         col="orange")
    if(input$resulttemp){
    abline(lm(x[,2]~x[,1]),col="darkred",lwd=2)
    }
    if(input$resultloess){
      lines(x[,1],predict(loess(x[,2]~x[,1])),col="darkgreen",lwd=2)
    }
    par(new=T)
    plot(t[,1],t[,2],type="l",lwd=2,col="darkblue",axes=F,xlab="Year",ylab=" ")
    mtext("CO2 ppm",side=4,col="darkblue") 
    axis(4, col="darkblue",col.axis="darkblue",las=1)
    if(input$resultco2){
    abline(lm(t[,2]~t[,1]),lwd=2,col="purple")
    }
    legend("bottomright",cex=.9,
           c("Linear regression of temperature","Average annual temperature","Average annual CO2 levels","Linear regression of CO2 levels","Loess regression of temperature"),
           lty=c(1,1,1,1,1),
           lwd=c(2,2,2,2,2),col=c("darkred","orange","darkblue","purple","darkgreen"))
    
  })
output$text<-renderUI({ 
  HTML("<p>Slope of temperature regression line: 0.023876.</p>
        <p>Equation for temperature: 
       <p>Slope of CO2 levels regression line: 1.777 </p>
      <p>
        <p>This means the average annual temperature is increasing by an estimated rate of
       0.023876 degrees celcius per year and CO2 levels are increasing at a rate of 1.777 ppm per year since 1979 in BC. The temperature loess 
regression line shows that the temperature began increasing dramatically in ~2005. The graphs shows that both CO2 levels and temperature are
       increasing as time increases.</p>" )
                      })
  
output$about<-renderUI({
  HTML("<h3>Introduction</h3>
<p >Hello, my name is Alexander and I am a 4th year student studying Statistics at SFU. 
Im interested in cleaning and analyzing data to predict outcomes and solve problems.The goal of this app is to investigate climate change in BC from 1979 to 2017.</p>")
})

output$references<-renderUI({
  HTML("<h3>References</h3>
       <p>Dlugokencky, E.J., K.W. Thoning, P.M. Lang, and P.P. Tans (2017), NOAA Greenhouse Gas Reference from Atmospheric Carbon Dioxide Dry Air Mole Fractions from the NOAA ESRL Carbon Cycle Cooperative Global Air Sampling Network.</p>
       <p>Vincent, L. A., X. L. Wang, E. J. Milewska, H. Wan, F. Yang, and V. Swail, 2012. A second generation of homogenized Canadian monthly surface air temperature for climate trend analysis, J. Geophys. Res., 117, D18110, doi:10.10292012JD017859.</p>
       <p>Mekis, É. and L.A. Vincent, 2011: An overview of the second generation adjusted daily precipitation dataset for trend analysis in Canada. Atmosphere-Ocean, 49(2), 163-177.</p>"
  )
})

})
