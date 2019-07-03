---
title: "Akien-Dashboard by Marvin Aziz"
output: 
  flexdashboard::flex_dashboard:
    navbar:
      - { title: "Marvin on LinkedIn", href: "https://www.linkedin.com/in/marvin-aziz/", align: right }
    orientation: columns
    vertical_layout: fill
    theme: default
    
---





```r
# Lade JSON Datei aus js-export

json <- fromJSON(file = "C:\\Users\\admin\\Documents\\Alles mit JS\\DEGIRO_API\\ergebnis.json")


#Lade alle Symbole aus txt
txt_symbols <- read.csv("AKTIEN_SYMBOLE.txt", sep = ",", header = F)
```

```
## Warning in read.table(file = file, header = header, sep = sep,
## quote = quote, : unvollstädige letzte Zeile von readTableHeader in
## 'AKTIEN_SYMBOLE.txt' gefunden
```

```r
txt_symbols <- as.character(t(txt_symbols))

# Funktion um HDAX Werte zu laden

werte_hdax_fct <- function(x){

    wert_temp <- try(na.omit(getSymbols(x, src = "yahoo", from = today() -days(260), to = today(), auto.assign = F, warnings = F)))

  return(wert_temp)
}
# Lade und packe alle werte in die Liste

# hdax_list <- list()
# for (x in seq_along(txt_symbols)){
#   hdax_list[[x]] <-  werte_hdax_fct(txt_symbols[x]);
# }

  hdax_list <- try(sapply(txt_symbols, werte_hdax_fct)); #### WARUM NUR MACHST DU JETZT PROBLEME?!
```

```
## 'getSymbols' currently uses auto.assign=TRUE by default, but will
## use auto.assign=FALSE in 0.5-0. You will still be able to use
## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
## and getOption("getSymbols.auto.assign") will still be checked for
## alternate defaults.
## 
## This message is shown once per session and may be disabled by setting 
## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.
```

```
## Warning: LIN.DE contains missing values. Some functions will not work if
## objects contain missing values in the middle of the series. Consider using
## na.omit(), na.approx(), na.fill(), etc to remove or replace them.
```

```r
help_df <- list(hdax_list$DLG.DE,hdax_list$ZAL.DE,hdax_list$MUV2.DE, hdax_list$NEM.DE, hdax_list$EVT.DE, hdax_list$LIN.DE, hdax_list$NDX1.DE )
```

Übersicht
================================


Row {.tabset .tabset-fade data-width=650}
------------------------------------------------



### Zalando


```r
### Interactive Chart Line Plot

stock_plot <- function(stock){
  p_evt <- try(ggplot(hdax_list[[stock]], aes(x = index(hdax_list[[stock]]), y = hdax_list[[stock]][,4]))) +
  geom_line(color = "darkblue") +
  ggtitle(names(hdax_list[[stock]])[4]) +
  xlab("Datum") + ylab("Preis") +
  theme_bw() +
  
 
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month")
  mytext_evt=paste("Day = ", index(hdax_list[[stock]]), "\n" , "Close = ", hdax_list[[stock]][,4], "\n",   sep="")
  pp_evt=plotly_build(p_evt)
  style( pp_evt, text=mytext_evt, hoverinfo = "text", traces = c(1, 2) )
 
  
}


stock_plot("ZAL.DE")
```

```
## Don't know how to automatically pick scale for object of type xts/zoo. Defaulting to continuous.
```

```
## Warning: You've referenced non-existent traces
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fcc5b191253.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc39d85f14\webshot3fcc5b191253.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```


### Linde


```r
stock_plot("LIN.DE")
```

```
## Don't know how to automatically pick scale for object of type xts/zoo. Defaulting to continuous.
```

```
## Warning: You've referenced non-existent traces
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fcc58433f07.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc731b75c9\webshot3fcc58433f07.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```

### Dialouge Semiconductor


```
## Don't know how to automatically pick scale for object of type xts/zoo. Defaulting to continuous.
```

```
## Warning: You've referenced non-existent traces
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fcc3d871e63.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc361a1e84\webshot3fcc3d871e63.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```


### Nementschek


```r
stock_plot("NEM.DE")
```

```
## Don't know how to automatically pick scale for object of type xts/zoo. Defaulting to continuous.
```

```
## Warning: You've referenced non-existent traces
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fcc337f7d17.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc6c3f3199\webshot3fcc337f7d17.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```


### Evotec


```r
stock_plot("EVT.DE")
```

```
## Don't know how to automatically pick scale for object of type xts/zoo. Defaulting to continuous.
```

```
## Warning: You've referenced non-existent traces
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fcc2b5c62e6.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc1a346b8d\webshot3fcc2b5c62e6.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```


### MUE RUE


```r
stock_plot("MUV2.DE")
```

```
## Don't know how to automatically pick scale for object of type xts/zoo. Defaulting to continuous.
```

```
## Warning: You've referenced non-existent traces
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fcc5d9643ed.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc40e74d96\webshot3fcc5d9643ed.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```


### Nordex


```r
stock_plot("NDX1.DE")
```

```
## Don't know how to automatically pick scale for object of type xts/zoo. Defaulting to continuous.
```

```
## Warning: You've referenced non-existent traces
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fccb9423b.png": Das System kann die angegebene Datei nicht
## finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc12db734f\webshot3fccb9423b.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```




Column
------------------------------------------------

### Depot Positionen



```r
wert_heute <- function(x){
  wert_temp <- c(1:101)
  for(i in seq_along(x)){

      wert_temp[i] <- tail(x[[i]][,6], n=1)  
      
  }
  
  return (wert_temp)
}


# bsp für EINEN wochenschlusskurs : mean(tail(dlg[,6], n = 130)[120:125])

# Versuche ausschließlich die Namen in eine Liste zu packen
tabelle_namen <- function(x){
  
  namen_tabelle <- tibble(.rows = 101)
for(i in 1:101){
  for (y in 1:101) {
    
  }
  namen_tabelle[i] <- tibble(names(hdax_list[[i]][,6]))

}
namen_tabelle <- namen_tabelle[1,]
return(namen_tabelle[x])
}

namen_tabelle <- tabelle_namen(1:101)


# Berechnung des RSL einer Aktie ( auf zwei Nachkommestellen genau )

wert_rsl <- function(x){
    
    wochendurchschn_bechtle <- tibble(.rows =27)
    letzter_kurs <- tibble(.rows = 1)
    rsl_kurse <- tibble(.rows = 1)
    
    
    for(g in 1:length(x)){
    beg <- 125
    end <- 130
    letzter_kurs[1,g] <- try(wert_heute(x)[[g]])
    
  for(i in 1:26){
    
    if( NA %in% x[[g]][,6]){
      wochendurchschn_bechtle[i,g] <- "FALSCHER WERT"
      next()
      
    }else{
      wochendurchschn_bechtle[i,g] <- try(mean(as.double(unlist(tail(x[[g]][,6],130)[beg:end]))), silent = TRUE)  
      beg <- beg - 5
      end <- end -5

    }
  
  }
    wochendurchschn_bechtle[27,g] <- try( mean(as.double(unlist(wochendurchschn_bechtle[1:26,g]))) )
    
    rsl_kurse[1,g] <- try((as.double(unlist(letzter_kurs[1,g])))/((as.double(unlist(wochendurchschn_bechtle[27,g])))))
    
    }
    return(rsl_kurse)
    
    
}


# Alle Namen bekommen
get_names <- function(x){
   names_temp <- seq_along(x)
  for(i in seq_along(x)){
   
    
    names_temp[i] <- names(x[[i]][,6])

    }
     return(names_temp)
}

# Alle Werte bekommen

get_wert <- function(x){
  wert_temp <- seq_along(x)
  
  for(i in seq_along(x)){
    
    wert_temp[i] <- as.double(tail(x[[i]][,6])[6])
    
  }
  return(wert_temp)
}


# Obige Version kürzen um Tabelle zu erstellen
werte_tabelle <- data.frame(
  
  Aktie = get_names(hdax_list),
  Wert = get_wert(hdax_list),
  RSL = try(t(wert_rsl(hdax_list)))
)

depot_tabelle <- data.frame(
  
  Aktie = get_names(help_df),
  Wert = get_wert(help_df),
  RSL = try(t(wert_rsl(help_df)))
)



# Schönes Format für alle Werte der Tabelle

werte_tabelle$Wert <- format(as.numeric(werte_tabelle$Wert),trim = T)
werte_tabelle$RSL <-  format(as.numeric(werte_tabelle$RSL),trim = T)


depot_tabelle$Wert <- format(as.numeric(depot_tabelle$Wert),trim = T)
depot_tabelle$RSL <-  format(as.numeric(depot_tabelle$RSL),trim = T)

# #Wandle Performace (%) in Prozent um
# werte_tabelle$Perf_1W <- sprintf("%.1f %%", 100*werte_tabelle$Perf_1W)
# # Spucke alle RSL Werte numerisch aus 
# werte_tabelle$RSL <- as.numeric(werte_tabelle$RSL)
# mache eine schöne Tabelle
for(x in seq_along(depot_tabelle$RSL)){
  
  depot_tabelle$RSL[x] <- ifelse("NA" %in% depot_tabelle$RSL[x],"https://www.tradesignalonline.com/scanner/default.aspx", depot_tabelle$RSL[x])
  
}

depot_tabelle <- depot_tabelle[order(depot_tabelle$RSL, decreasing = T),]
depot_tabelle[order(depot_tabelle$RSL, decreasing = T),] %>%

knitr::kable() %>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Aktie </th>
   <th style="text-align:left;"> Wert </th>
   <th style="text-align:left;"> RSL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> V1 </td>
   <td style="text-align:left;"> DLG.DE.Adjusted </td>
   <td style="text-align:left;"> 35.46 </td>
   <td style="text-align:left;"> 1.257425 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V4 </td>
   <td style="text-align:left;"> NEM.DE.Adjusted </td>
   <td style="text-align:left;"> 52.95 </td>
   <td style="text-align:left;"> 1.227024 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V2 </td>
   <td style="text-align:left;"> ZAL.DE.Adjusted </td>
   <td style="text-align:left;"> 39.02 </td>
   <td style="text-align:left;"> 1.178925 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V6 </td>
   <td style="text-align:left;"> LIN.DE.Adjusted </td>
   <td style="text-align:left;"> 176.65 </td>
   <td style="text-align:left;"> 1.147310 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V5 </td>
   <td style="text-align:left;"> EVT.DE.Adjusted </td>
   <td style="text-align:left;"> 24.58 </td>
   <td style="text-align:left;"> 1.139300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V3 </td>
   <td style="text-align:left;"> MUV2.DE.Adjusted </td>
   <td style="text-align:left;"> 220.70 </td>
   <td style="text-align:left;"> 1.088746 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V7 </td>
   <td style="text-align:left;"> NDX1.DE.Adjusted </td>
   <td style="text-align:left;"> 12.12 </td>
   <td style="text-align:left;"> 1.024513 </td>
  </tr>
</tbody>
</table>

```r
# Erstelle ein CandleStick Chart 
#  p <- tail(as.data.frame(hdax[[1]]), 60) %>%
#   plot_ly(x = as.Date.POSIXct(hdax[[1]][,0]), type="candlestick",
#           open = ~DLG.DE.Open, close = ~DLG.DE.Close,
#           high = ~DLG.DE.High, low = ~DLG.DE.Low) %>%
#   layout(title = "DLG Candlestick Chart [60D]",
#           xaxis = list(rangeslider = list(visible = F))
#          )
# p
```
### Bearbeiten


```r
print("OUTPUT")
```

```
## [1] "OUTPUT"
```


Column {data-width=300}
-----------------------------------------------------------------------
### Gesamtwert Depot


```r
  invested <-sum(json$Gesamt);
  verfuegbar <- json$Verfuegbar;


gesamt <- invested + verfuegbar


gauge((invested), min = 0, max = gesamt, symbol = '€', gaugeSectors(
      success = c((gesamt*0.7), (gesamt)), warning = c((gesamt*0.55), (gesamt*0.69)), danger = c(0, (gesamt*0.54))
))
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fccefd5b73.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc6be155a2\webshot3fccefd5b73.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```


### Investiertes Geld


```r
anteil_investiert <- round(invested/gesamt *100, digits = 1)

gauge(anteil_investiert, min = 0, max = 100, symbol = '%', gaugeSectors(
      success = c(80, 95), warning = c(40, 79), danger = c(0, 39)
))
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fcc28cbab1.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc6eadd0f\webshot3fcc28cbab1.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```

### Cash


```r
gauge(verfuegbar, min = 0, max = gesamt, symbol = '€', gaugeSectors(
      success = c(0, 400), warning = c(401, 2000), danger = c(2001, 4000)
))
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork):
## path[1]="webshot3fcc4af670b9.png": Das System kann die angegebene Datei
## nicht finden
```

```
## Warning in file(con, "rb"): kann Datei 'C:
## \Users\admin\AppData\Local\Temp\RtmpshYWcT\file3fcc21443895\webshot3fcc4af670b9.png'
## nicht öffnen: No such file or directory
```

```
## Error in file(con, "rb"): kann Verbindung nicht öffnen
```

### Hinweise
Folgende Aktien solltest du ende des Monats verkaufen:


```
## [1] "NDX1.DE.Adjusted  -> ACHTUNG, bald solltest du evtl. verkaufen."
```



HDAX Werte
================================

Column {data-width=650}
-----------------------------------------------------------------------

### HDAX Top 70


```r
  werte_tabelle <- werte_tabelle[order(werte_tabelle$RSL, decreasing = T),]
  werte_tabelle <- as.data.frame(werte_tabelle, row.names = 1:101) 

  for(x in seq_along(werte_tabelle$RSL)){
  
  werte_tabelle$RSL[x] <- ifelse("NA" %in% werte_tabelle$RSL[x],"https://www.tradesignalonline.com/scanner/default.aspx", werte_tabelle$RSL[x])
  
}
  
werte_tabelle %>%

knitr::kable() %>%
kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Aktie </th>
   <th style="text-align:left;"> Wert </th>
   <th style="text-align:left;"> RSL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> COP.DE.Adjusted </td>
   <td style="text-align:left;"> 71.000 </td>
   <td style="text-align:left;"> 1.3302593 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIB.DE.Adjusted </td>
   <td style="text-align:left;"> 18.040 </td>
   <td style="text-align:left;"> 1.2985103 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DLG.DE.Adjusted </td>
   <td style="text-align:left;"> 35.460 </td>
   <td style="text-align:left;"> 1.2574253 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPR.DE.Adjusted </td>
   <td style="text-align:left;"> 61.950 </td>
   <td style="text-align:left;"> 1.2395905 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> O1BC.DE.Adjusted </td>
   <td style="text-align:left;"> 375.500 </td>
   <td style="text-align:left;"> 1.2341092 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NEM.DE.Adjusted </td>
   <td style="text-align:left;"> 52.950 </td>
   <td style="text-align:left;"> 1.2270240 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ADS.DE.Adjusted </td>
   <td style="text-align:left;"> 271.500 </td>
   <td style="text-align:left;"> 1.2252948 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> COK.DE.Adjusted </td>
   <td style="text-align:left;"> 46.720 </td>
   <td style="text-align:left;"> 1.2224494 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ISR.DE.Adjusted </td>
   <td style="text-align:left;"> 39.180 </td>
   <td style="text-align:left;"> 1.2169868 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BC8.DE.Adjusted </td>
   <td style="text-align:left;"> 101.000 </td>
   <td style="text-align:left;"> 1.2161824 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SRT3.DE.Adjusted </td>
   <td style="text-align:left;"> 180.300 </td>
   <td style="text-align:left;"> 1.2129824 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PUM.DE.Adjusted </td>
   <td style="text-align:left;"> 58.650 </td>
   <td style="text-align:left;"> 1.2042757 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SAP.DE.Adjusted </td>
   <td style="text-align:left;"> 120.760 </td>
   <td style="text-align:left;"> 1.2040600 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ZAL.DE.Adjusted </td>
   <td style="text-align:left;"> 39.020 </td>
   <td style="text-align:left;"> 1.1789246 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RHM.DE.Adjusted </td>
   <td style="text-align:left;"> 107.650 </td>
   <td style="text-align:left;"> 1.1487957 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LIN.DE.Adjusted </td>
   <td style="text-align:left;"> 176.650 </td>
   <td style="text-align:left;"> 1.1473101 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PBB.DE.Adjusted </td>
   <td style="text-align:left;"> 10.580 </td>
   <td style="text-align:left;"> 1.1463995 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EVT.DE.Adjusted </td>
   <td style="text-align:left;"> 24.580 </td>
   <td style="text-align:left;"> 1.1393002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AIR.DE.Adjusted </td>
   <td style="text-align:left;"> 124.780 </td>
   <td style="text-align:left;"> 1.1373292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WDI.DE.Adjusted </td>
   <td style="text-align:left;"> 148.050 </td>
   <td style="text-align:left;"> 1.1327546 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DRW3.DE.Adjusted </td>
   <td style="text-align:left;"> 55.400 </td>
   <td style="text-align:left;"> 1.1324347 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BEI.DE.Adjusted </td>
   <td style="text-align:left;"> 105.550 </td>
   <td style="text-align:left;"> 1.1267873 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RKET.DE.Adjusted </td>
   <td style="text-align:left;"> 25.360 </td>
   <td style="text-align:left;"> 1.1228118 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> B4B.DE.Adjusted </td>
   <td style="text-align:left;"> 16.075 </td>
   <td style="text-align:left;"> 1.1211612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HEI.DE.Adjusted </td>
   <td style="text-align:left;"> 71.160 </td>
   <td style="text-align:left;"> 1.1209484 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HNR1.DE.Adjusted </td>
   <td style="text-align:left;"> 142.200 </td>
   <td style="text-align:left;"> 1.1142200 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ALV.DE.Adjusted </td>
   <td style="text-align:left;"> 212.000 </td>
   <td style="text-align:left;"> 1.1122344 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DHER.DE.Adjusted </td>
   <td style="text-align:left;"> 39.890 </td>
   <td style="text-align:left;"> 1.1096338 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LXS.DE.Adjusted </td>
   <td style="text-align:left;"> 52.260 </td>
   <td style="text-align:left;"> 1.1055122 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KBX.DE.Adjusted </td>
   <td style="text-align:left;"> 98.000 </td>
   <td style="text-align:left;"> 1.1035746 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FRA.DE.Adjusted </td>
   <td style="text-align:left;"> 75.600 </td>
   <td style="text-align:left;"> 1.1017340 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KGX.DE.Adjusted </td>
   <td style="text-align:left;"> 55.440 </td>
   <td style="text-align:left;"> 1.0934301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MTX.DE.Adjusted </td>
   <td style="text-align:left;"> 209.500 </td>
   <td style="text-align:left;"> 1.0922903 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DB1.DE.Adjusted </td>
   <td style="text-align:left;"> 124.400 </td>
   <td style="text-align:left;"> 1.0889606 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MUV2.DE.Adjusted </td>
   <td style="text-align:left;"> 220.700 </td>
   <td style="text-align:left;"> 1.0887457 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AFX.DE.Adjusted </td>
   <td style="text-align:left;"> 86.750 </td>
   <td style="text-align:left;"> 1.0854274 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G1A.DE.Adjusted </td>
   <td style="text-align:left;"> 25.000 </td>
   <td style="text-align:left;"> 1.0839052 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DPW.DE.Adjusted </td>
   <td style="text-align:left;"> 28.900 </td>
   <td style="text-align:left;"> 1.0831634 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AOX.DE.Adjusted </td>
   <td style="text-align:left;"> 14.240 </td>
   <td style="text-align:left;"> 1.0786797 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EVK.DE.Adjusted </td>
   <td style="text-align:left;"> 25.610 </td>
   <td style="text-align:left;"> 1.0778322 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SY1.DE.Adjusted </td>
   <td style="text-align:left;"> 84.640 </td>
   <td style="text-align:left;"> 1.0756574 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> G24.DE.Adjusted </td>
   <td style="text-align:left;"> 46.720 </td>
   <td style="text-align:left;"> 1.0718865 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UN01.DE.Adjusted </td>
   <td style="text-align:left;"> 26.630 </td>
   <td style="text-align:left;"> 1.0647789 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FIE.DE.Adjusted </td>
   <td style="text-align:left;"> 63.800 </td>
   <td style="text-align:left;"> 1.0634435 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QIA.DE.Adjusted </td>
   <td style="text-align:left;"> 35.690 </td>
   <td style="text-align:left;"> 1.0549852 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HLE.DE.Adjusted </td>
   <td style="text-align:left;"> 43.480 </td>
   <td style="text-align:left;"> 1.0509608 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SIE.DE.Adjusted </td>
   <td style="text-align:left;"> 104.600 </td>
   <td style="text-align:left;"> 1.0507313 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FME.DE.Adjusted </td>
   <td style="text-align:left;"> 69.040 </td>
   <td style="text-align:left;"> 1.0489593 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IGY.DE.Adjusted </td>
   <td style="text-align:left;"> 41.700 </td>
   <td style="text-align:left;"> 1.0449446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DTE.DE.Adjusted </td>
   <td style="text-align:left;"> 15.212 </td>
   <td style="text-align:left;"> 1.0446590 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VOW3.DE.Adjusted </td>
   <td style="text-align:left;"> 148.220 </td>
   <td style="text-align:left;"> 1.0360902 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EOAN.DE.Adjusted </td>
   <td style="text-align:left;"> 9.551 </td>
   <td style="text-align:left;"> 1.0351290 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NDX1.DE.Adjusted </td>
   <td style="text-align:left;"> 12.120 </td>
   <td style="text-align:left;"> 1.0245129 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SHL.DE.Adjusted </td>
   <td style="text-align:left;"> 37.110 </td>
   <td style="text-align:left;"> 1.0240486 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GXI.DE.Adjusted </td>
   <td style="text-align:left;"> 64.750 </td>
   <td style="text-align:left;"> 1.0232175 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FRE.DE.Adjusted </td>
   <td style="text-align:left;"> 47.680 </td>
   <td style="text-align:left;"> 1.0204122 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BOSS.DE.Adjusted </td>
   <td style="text-align:left;"> 58.500 </td>
   <td style="text-align:left;"> 1.0191450 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BAYN.DE.Adjusted </td>
   <td style="text-align:left;"> 60.940 </td>
   <td style="text-align:left;"> 1.0185762 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BNR.DE.Adjusted </td>
   <td style="text-align:left;"> 43.310 </td>
   <td style="text-align:left;"> 1.0158438 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BAS.DE.Adjusted </td>
   <td style="text-align:left;"> 63.920 </td>
   <td style="text-align:left;"> 1.0141147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FNTN.DE.Adjusted </td>
   <td style="text-align:left;"> 17.595 </td>
   <td style="text-align:left;"> 1.0088294 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DAI.DE.Adjusted </td>
   <td style="text-align:left;"> 48.925 </td>
   <td style="text-align:left;"> 1.0052282 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RWE.DE.Adjusted </td>
   <td style="text-align:left;"> 21.670 </td>
   <td style="text-align:left;"> 1.0047345 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AT1.DE.Adjusted </td>
   <td style="text-align:left;"> 7.246 </td>
   <td style="text-align:left;"> 1.0014993 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PFV.DE.Adjusted </td>
   <td style="text-align:left;"> 129.000 </td>
   <td style="text-align:left;"> 1.0011226 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SANT.DE.Adjusted </td>
   <td style="text-align:left;"> 20.480 </td>
   <td style="text-align:left;"> 1.0008923 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PSM.DE.Adjusted </td>
   <td style="text-align:left;"> 13.815 </td>
   <td style="text-align:left;"> 0.9958331 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SDF.DE.Adjusted </td>
   <td style="text-align:left;"> 16.380 </td>
   <td style="text-align:left;"> 0.9952101 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SOW.DE.Adjusted </td>
   <td style="text-align:left;"> 30.200 </td>
   <td style="text-align:left;"> 0.9949433 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TEG.DE.Adjusted </td>
   <td style="text-align:left;"> 20.320 </td>
   <td style="text-align:left;"> 0.9938320 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LEG.DE.Adjusted </td>
   <td style="text-align:left;"> 99.200 </td>
   <td style="text-align:left;"> 0.9893379 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RRTL.DE.Adjusted </td>
   <td style="text-align:left;"> 45.040 </td>
   <td style="text-align:left;"> 0.9870819 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MRK.DE.Adjusted </td>
   <td style="text-align:left;"> 91.960 </td>
   <td style="text-align:left;"> 0.9866557 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HEN3.DE.Adjusted </td>
   <td style="text-align:left;"> 86.020 </td>
   <td style="text-align:left;"> 0.9854055 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1COV.DE.Adjusted </td>
   <td style="text-align:left;"> 44.710 </td>
   <td style="text-align:left;"> 0.9826141 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GYC.DE.Adjusted </td>
   <td style="text-align:left;"> 20.100 </td>
   <td style="text-align:left;"> 0.9791660 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DEQ.DE.Adjusted </td>
   <td style="text-align:left;"> 24.300 </td>
   <td style="text-align:left;"> 0.9687913 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CON.DE.Adjusted </td>
   <td style="text-align:left;"> 128.220 </td>
   <td style="text-align:left;"> 0.9662890 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VNA.DE.Adjusted </td>
   <td style="text-align:left;"> 42.000 </td>
   <td style="text-align:left;"> 0.9655347 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NDA.DE.Adjusted </td>
   <td style="text-align:left;"> 42.830 </td>
   <td style="text-align:left;"> 0.9624799 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BMW.DE.Adjusted </td>
   <td style="text-align:left;"> 65.090 </td>
   <td style="text-align:left;"> 0.9614716 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TKA.DE.Adjusted </td>
   <td style="text-align:left;"> 12.825 </td>
   <td style="text-align:left;"> 0.9600107 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> JEN.DE.Adjusted </td>
   <td style="text-align:left;"> 28.450 </td>
   <td style="text-align:left;"> 0.9576637 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AIXA.DE.Adjusted </td>
   <td style="text-align:left;"> 8.394 </td>
   <td style="text-align:left;"> 0.9474851 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DBK.DE.Adjusted </td>
   <td style="text-align:left;"> 6.780 </td>
   <td style="text-align:left;"> 0.9450167 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FPE3.DE.Adjusted </td>
   <td style="text-align:left;"> 34.580 </td>
   <td style="text-align:left;"> 0.9323187 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MOR.DE.Adjusted </td>
   <td style="text-align:left;"> 84.450 </td>
   <td style="text-align:left;"> 0.9279814 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CBK.DE.Adjusted </td>
   <td style="text-align:left;"> 6.319 </td>
   <td style="text-align:left;"> 0.9257330 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> O2D.DE.Adjusted </td>
   <td style="text-align:left;"> 2.457 </td>
   <td style="text-align:left;"> 0.9216943 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WAF.DE.Adjusted </td>
   <td style="text-align:left;"> 64.240 </td>
   <td style="text-align:left;"> 0.9029981 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARL.DE.Adjusted </td>
   <td style="text-align:left;"> 23.170 </td>
   <td style="text-align:left;"> 0.8883328 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WCH.DE.Adjusted </td>
   <td style="text-align:left;"> 69.580 </td>
   <td style="text-align:left;"> 0.8870859 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HOT.DE.Adjusted </td>
   <td style="text-align:left;"> 107.100 </td>
   <td style="text-align:left;"> 0.8842288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DUE.DE.Adjusted </td>
   <td style="text-align:left;"> 29.970 </td>
   <td style="text-align:left;"> 0.8838027 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NOEJ.DE.Adjusted </td>
   <td style="text-align:left;"> 36.440 </td>
   <td style="text-align:left;"> 0.8744241 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OSR.DE.Adjusted </td>
   <td style="text-align:left;"> 28.960 </td>
   <td style="text-align:left;"> 0.8739200 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UTDI.DE.Adjusted </td>
   <td style="text-align:left;"> 28.960 </td>
   <td style="text-align:left;"> 0.8575741 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DRI.DE.Adjusted </td>
   <td style="text-align:left;"> 29.320 </td>
   <td style="text-align:left;"> 0.8559737 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IFX.DE.Adjusted </td>
   <td style="text-align:left;"> 15.550 </td>
   <td style="text-align:left;"> 0.8506400 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DWNI.DE.Adjusted </td>
   <td style="text-align:left;"> 32.270 </td>
   <td style="text-align:left;"> 0.8027141 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LHA.DE.Adjusted </td>
   <td style="text-align:left;"> 15.070 </td>
   <td style="text-align:left;"> 0.7728288 </td>
  </tr>
</tbody>
</table>
