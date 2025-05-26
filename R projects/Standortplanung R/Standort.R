# Daten einlesen----------------------------------------------------------------------------------------------------------------

Standorte <- data.frame(read.csv("../../Standortplanung R/Standorte.csv", sep = ";", header = TRUE, stringsAsFactors=FALSE))
#Standorte <- read_csv("../../Standortplanung R/Standorte.csv")
namen <- Standorte$Standort
Standorte <- Standorte[, ! names(Standorte) %in% "Standort", drop = F]
Standorte <- as.data.frame(sapply(Standorte, as.numeric))
row.names(Standorte) <- namen

getwd()

Abnehmer <- data.frame(read.csv("../../Standortplanung R/Abnehmer.csv", sep = ";", header = TRUE, stringsAsFactors=FALSE))
Abnehmer$Bedarf <- as.numeric(Abnehmer$Bedarf)

Transportkosten <- as.data.frame(read.csv("../../Standortplanung R/Transportkosten.csv", sep = ";", header = TRUE, stringsAsFactors=FALSE))
namen <- Transportkosten$Standort
Transportkosten <- Transportkosten[, ! names(Transportkosten) %in% "Standort", drop = F]
Transportkosten <- as.data.frame(sapply(Transportkosten, as.numeric))
row.names(Transportkosten) <- namen

Abnehmer %>% 
  glimpse()

Standorte %>% 
  glimpse()

Transportkosten %>% 
  glimpse()

namen %>% 
  view


# Funktion zur L?sung des Transportproblems (Greedy)----------------------------------------------------------------------------

Transportproblem <- function(df){
  kosten <- as.data.frame(matrix(numeric(), ncol=ncol(Transportkosten),nrow=0, dimnames=list(NULL, colnames(Transportkosten))))
  Kapazit?t <- c()
  bed <- as.data.frame(t(as.numeric(Abnehmer$Bedarf)))
  colnames(bed) <- colnames(kosten)
  rownames(bed)[nrow(bed)] <- "Bedarf"
  C <- 0
  
  for(i in 1:nrow(df)){
    kosten <- rbind(kosten, Transportkosten[which(row.names(Transportkosten) == df[i]), ])
    Kapazit?t <- rbind(Kapazit?t, Standorte[which(row.names(Standorte) == df[i]), "Kapazit?t"])
    C <- C + Standorte[which(row.names(Standorte) == df[i]), "Fixkosten"]
  }
  Kapazit?t <- rbind(Kapazit?t, NA)
  kosten <- rbind(kosten,bed)
  kosten <- cbind(kosten,  Kapazit?t)
  
  Rest_Bedarf <- rowSums(kosten[nrow(kosten),], na.rm = TRUE)
  Rest_Kapazit?t <- sum(kosten$Kapazit?t, na.rm = TRUE)
  
  while(Rest_Bedarf > 0 && Rest_Kapazit?t > 0){
    index <- which(kosten == min(subset(kosten, row.names(kosten) != "Bedarf"), na.rm = TRUE), arr.ind = TRUE)
    Menge <- min(kosten[index[1,1], ncol(kosten)], kosten[nrow(kosten) , index[1,2]])
    C <- C + kosten[index[1,1], index[1,2]] * Menge
    kosten[index[1,1], ncol(kosten)] <- kosten[index[1,1], ncol(kosten)] - Menge
    kosten[nrow(kosten) , index[1,2]] <- kosten[nrow(kosten) , index[1,2]] - Menge
    if(!is.na(kosten[index[1,1], ncol(kosten)]) && kosten[index[1,1], ncol(kosten)] == 0)
      kosten[index[1,1], ] <- NA
    if(!is.na(kosten[nrow(kosten) , index[1,2]]) && kosten[nrow(kosten) , index[1,2]] == 0)
      kosten[ , index[1,2]] <- NA
    Rest_Bedarf <- Rest_Bedarf - Menge
    Rest_Kapazit?t <- Rest_Kapazit?t - Menge
  }
  C <- C + Rest_Bedarf * 100000
  return(data.frame(C = C, Restbedarf = Rest_Bedarf))
}

# Funktion zur Anzeige der Transportmengen--------------------------------------------------------------------------------------

Transportmengen <- function(df){
  kosten <- as.data.frame(matrix(numeric(), ncol=ncol(Transportkosten),nrow=0, dimnames=list(NULL, colnames(Transportkosten))))
  mengen <- as.data.frame(matrix(numeric(), ncol=ncol(Transportkosten),nrow=nrow(df), dimnames=list(NULL, colnames(Transportkosten))))
  row.names(mengen) <- df[,1]
  Kapazit?t <- c()
  bed <- as.data.frame(t(as.numeric(Abnehmer$Bedarf)))
  colnames(bed) <- colnames(kosten)
  rownames(bed)[nrow(bed)] <- "Bedarf"
  
  for(i in 1:nrow(df)){
    kosten <- rbind(kosten, Transportkosten[which(row.names(Transportkosten) == df[i]), ])
    Kapazit?t <- rbind(Kapazit?t, Standorte[which(row.names(Standorte) == df[i]), "Kapazit?t"])
  }
  Kapazit?t <- rbind(Kapazit?t, NA)
  kosten <- rbind(kosten,bed)
  kosten <- cbind(kosten,  Kapazit?t)
  
  Rest_Bedarf <- rowSums(kosten[nrow(kosten),], na.rm = TRUE)
  Rest_Kapazit?t <- sum(kosten$Kapazit?t, na.rm = TRUE)
  
  while(Rest_Bedarf > 0 && Rest_Kapazit?t > 0){
    index <- which(kosten == min(subset(kosten, row.names(kosten) != "Bedarf"), na.rm = TRUE), arr.ind = TRUE)
    Menge <- min(kosten[index[1,1], ncol(kosten)], kosten[nrow(kosten) , index[1,2]])
    mengen[index[1,1], index[1,2]] <- Menge
    kosten[index[1,1], ncol(kosten)] <- kosten[index[1,1], ncol(kosten)] - Menge
    kosten[nrow(kosten) , index[1,2]] <- kosten[nrow(kosten) , index[1,2]] - Menge
    if(!is.na(kosten[index[1,1], ncol(kosten)]) && kosten[index[1,1], ncol(kosten)] == 0)
      kosten[index[1,1], ] <- NA
    if(!is.na(kosten[nrow(kosten) , index[1,2]]) && kosten[nrow(kosten) , index[1,2]] == 0)
      kosten[ , index[1,2]] <- NA
    Rest_Bedarf <- Rest_Bedarf - Menge
    Rest_Kapazit?t <- Rest_Kapazit?t - Menge
  }
  return(data.frame(mengen))
}

# ADD-Heuristik-----------------------------------------------------------------------------------------------------------------

# Gesamtbedarf <- sum(Abnehmer$Bedarf)
Standorte_offen <- c()
zeile <- as.numeric(which.min(Standorte$Fixkosten / Standorte$Kapazit?t))
Standorte_offen <- rbind(Standorte_offen, row.names(Standorte[zeile, ]))
colnames(Standorte_offen) <- "Standort"
Gesamtkosten <- Inf
improvement = TRUE

while( improvement == TRUE && nrow(Standorte_offen) < nrow(Standorte) ){
  Cmin <- Inf
  for( i in 1:nrow(Standorte) ){
    if(!row.names(Standorte[i, ]) %in% Standorte_offen){
      fiktiv <- rbind(Standorte_offen, row.names(Standorte[i, ]))
      Ergebnis <- Transportproblem(fiktiv)
      if(Ergebnis$C < Cmin){
        Cmin <- Ergebnis$C
        zeile <- i
      }
    }
  }
  if(Cmin < Gesamtkosten){
    Standorte_offen <- rbind(Standorte_offen, row.names(Standorte[zeile, ]))
    Gesamtkosten <- Cmin
    improvement <- TRUE
  }
  else
    improvement <- FALSE
}

Transportmengen <- Transportmengen(Standorte_offen)
