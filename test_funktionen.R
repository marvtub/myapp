#test code

tibble_test <- tibble(.rows =101)
tibble_rsl <- tibble(.rows = 101)
for(i in 1:101){
  
  beg1 <- 125
  end1 <- 130
  tibble_test[1,i] <- as.character(tabelle_namen(i))
  for(y in 2:26){
    
    tibble_test[y,i] <- mean(as.double(tail(hdax_list[[i]][,6], n = 130)[beg1:end1]))
    
     # tibble_test[] <- lapply(tibble_test, function(x) as.numeric(as.character(x))) <- ist QUATSCH
    
    beg <- beg1 - 5
    end1 <- end1 -5
  }
 
    tibble_test[28,i] <-  mean(as.double(unlist(tibble_test[2:27,i])))
    
}

for(g in 1:101){
  
  tibble_rsl[g,1]<- as.numeric(temp_letzte_kurse[[g,2]])/as.numeric(tibble_test[[28,g]])
  
}




temp_letzte_kurse$RSL <- tibble_rsl[,1]

# tibble_rsl[[4,1]] <- liefert RSL der 4. Aktie



#Test 130 Tage Kurse
tage130_bechtle <- tail(hdax_list[[1]][,6], 130)



# Ablauf für RSL EINER Aktie --> Funktioniert!
#Schritt 1
#Test 26 Mal Wochendurschnitt
beg <- 125
end <- 130
wochendurchschn_bechtle <- c(1:26)
for(i in 1:26){
  wochendurchschn_bechtle[i] <- mean(as.double(unlist(tail(hdax_list[[1]][,6], 130)[beg:end])))  
beg <- beg - 5
end <- end -5
}

#Schritt 2
#Test Durchschnitt 26 Wochen
durchschn_26_bechtle_woche <- mean(wochendurchschn_bechtle[1:26])

#Schritt 3
# Test letzter kurs
letzter_kurs_bechtle <- wert_heute(hdax_list)[[1]]

#Schritt 4
#Test RSL BEchtle
rsl_bechtle <- (letzter_kurs_bechtle)/(durchschn_26_bechtle_woche)




# FOR LOOP OHNE FOR LOOP ---> PURRR (klaptt noch nicht)
possibly_wert_rsl <- possibly(wert_rsl, otherwise= NA)
safely_wert_rsl <- safely(wert_heute)
test1 <- map(hdax_list, wert_heute)





