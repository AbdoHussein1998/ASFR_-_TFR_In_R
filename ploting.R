#### ploting ####

graph_asfr=t(ASFR[,seq(4,10)])
graph_asfr<-data.frame(graph_asfr)
colnames(graph_asfr) <- ASFR$country_name
graph_asfr[,"age"]<-factor(row.names(graph_asfr))
graph_asfr[,"age"]<- as.numeric(graph_asfr$age)


plot(x = graph_asfr$age,
     y = graph_asfr$Croatia,
     xlab = "age",
     ylab = "ASFR",
     xaxt= "n",
     col="blue",
     type="b",
     main="ASFR of 2024",
     lwd=3,
     ylim = c(0, 150))

axis(side = 1,at = graph_asfr$age,labels = row.names(graph_asfr))
lines(x = graph_asfr$age,y= graph_asfr$Cyprus,col="red",type="b",lwd=3)
lines(x = graph_asfr$age,y= graph_asfr$Botswana,col="#858503",type="b",lwd=3)
lines(x = graph_asfr$age,y= graph_asfr$Colombia,col="#630313",type="b",lwd=3)
lines(x = graph_asfr$age,y= graph_asfr$Andorra,col="#6b09a8",type="b",lwd=3)

legend("topright",
       legend=c("Croatia","Cyprus","Botswana","Colombia","Andorra"),
       col=c("blue","red","#858503","#630313","#6b09a8"),
       lty = 1,lwd=5)
       



selected_country_c=c(12,14,5,9,2)
country_value=TFR_df[selected_country_c,]
country_name=labels(TFR_df)[[1]][selected_country_c]
bar_graph=data.frame(row.names = country_name, country_value)

barplot(bar_graph$country_value,xlab = "Country",
        ylab = "TFR",
        names.arg = row.names(bar_graph),
        col = c("blue","red","#858503","#630313","#6b09a8"),
        main = "TFR by country")

barplot(bar_graph$country_value,ylab = "Country",
        xlab = "TFR",
        names.arg = row.names(bar_graph),
        col = c("blue","red","#858503","#630313","#6b09a8"),
        main = "TFR by country",horiz = T)


