## Wunderdogin koodauspähkinä, "Hassuimmat sanat"
## 
## Tehtävänä laskea Alastalon salissa - kirjan sanojen peräkkäiset vokaalit. 
## Jokaisesta vokaaliketjusta sana saa n*2^n pistettä.
## Tehtävän tarkempi kuvaus löytyy osoitteesta: 
## http://wunderdog.fi/koodaus-hassuimmat-sanat/ (20.1.2015)
##

# Ratkaissut: Juhani Takkunen 20.1.2015
# Rinnakkaistettu 8.-9.2. -JT
# Rinnakkaistus parannettu 13.2. -JT


#### FUNKTIOT ####

lataaKirja <- function(kirja.source, isURL=FALSE){
  # lataa tekstin annetulta nettisivulta
  # - poistaa siitä rivivaihdot \n ja \r
  # - UTF-8
  # 
  # Args:
  #   kirjanURL: nettisivun url, jolta teksti halutaan hakea
  # 
  # Returns:
  #   sanat: string-taulukko, jossa jokainen sana on jaettu omaan soluun
  #
  # Testattu toimivaksi sivulla: 
  # http://wunderdog.fi/koodaus-hassuimmat-sanat/alastalon_salissa.txt
  
  library(XML)
  kirja.html = htmlTreeParse(kirjanURL, encoding = "UTF-8", useInternal = TRUE)
  kirja.text = xpathApply(kirja.html, '//p', xmlValue)
  kirja.text = gsub("([\n\r\t])", ' ', kirja.text) # poistaa turhat rivivaihdot
  kirja.text = gsub("([\'-])", '', kirja.text)     # poistaa tarpeettomat yhdysmerkit
  kirja.text = unlist(strsplit(kirja.text, " "))
  return(kirja.text)
}

onkoVokaali <- function(kirjain){
  # Testataan, onko annettu kirjain vokaali
  # 
  # Args:
  #   kirjain: char, jonka vokaaliuutta testataan
  # 
  # Returns:
  #   boolean: TRUE, jos kyseessä on sopiva vokaali
  #
  # Huomioita:  - vokaalitaulukko on itse luotu, eikä perustu R-kirjastoihin.
  #             - testaa isot ja pienet kirjaimet
  
  # Määritellään vokaalit
  vokaalit <- c("a", "e", "i", "o", "u", "y", "ä", "ö", "å", 
                "A", "E", "I", "O", "U", "Y", "Ä", "Ö", "Å")
  vokaali <- match(kirjain, vokaalit, nomatch = 0)
  if(vokaali>0) return(TRUE)
  else          return(FALSE)
}

laskePisteet <- function(n){
  # Lasketaan, montako vokaalipistettä annettu vokaalimäärä saa.
  #
  # Args: 
  #   n: yhtäjaksoisten vokaalien määrä (integer)
  # Returns
  #   pisteet: vokaaleista saatava pistemäärä (integer)
  
  # Laskentakaavan tehtävänanto:
  # Jokainen vokaaliketju saa n×2^n pistettä, jossa n on 
  # vokaalien määrä ketjussa. Sanan vokaaliketjujen saamat 
  # pisteet lasketaan yhteen, jolloin saadaan sanan hassuuspisteet.
  # Esimerkiksi sana "koira" saa 10 pistettä, koska "koira" 
  # sisältää vokaaliketjut "oi" (2×22 = 8 pistettä) ja "a" 
  # (1×21 = 2 pistettä), ja 8 + 2 = 10.
  # 
  # Sana "hääyöaie" saa 896 pistettä, koska vokaaliketju "ääyöaie" 
  # saa (7×27 = 896 pistettä).
  # lähde: http://wunderdog.fi/koodaus-hassuimmat-sanat/
  pisteet = n*2^n
}

laskeSananPisteet <- function(sana){
  # Lasketaan, montako "vokaalipistettä" kyseinen sana saa.
  #
  # Args: 
  #   sana: tutkittava sana (string)
  # Returns: pisteet
  #   pisteet: vokaalipisteiden määrä
  #
  # Huomioita:
  # - hyväksytyt vokaalit, on määritetty funktiossa onkoVokaali
  
  pisteet = 0 # kokonaispisteet
  iEkaVokaali = -1 # vokaaliketjun ensimmäisen vokaalin indeksi
  
  for(i in 1:nchar(sana)){
    kirjain = substring(sana, i, i)
    if(onkoVokaali(kirjain) == TRUE && iEkaVokaali == -1){ # vokaalisarja alkaa
      iEkaVokaali = i
    } else if (onkoVokaali(kirjain) == FALSE && iEkaVokaali != -1){ # epävokaali päätti vokaalisarjan
      pisteet = pisteet + laskePisteet(i-iEkaVokaali)
      iEkaVokaali = -1 # nollataan vokaalilaskuri
    }
  } 
  if(iEkaVokaali!=-1){ # sana päättyy vokaalisarjaan
    pisteet = pisteet + laskePisteet(i-iEkaVokaali+1)
  }
  return(pisteet)
} 


#### PÄÄOHJELMA ####
library(foreach)
library(doParallel)

# Alustetaan käyttäjän määriteltävät muuttujat
# kirjanURL = "http://wunderdog.fi/koodaus-hassuimmat-sanat/alastalon_salissa.txt" #linkki ei toiminut 8.2.
kirjanURL = "http://www.cs.helsinki.fi/u/jtakkune/ohjelmat/wunderdog/alastalon_salissa.txt"
n_ytimet = 1 # lasketaan rinnakkain n-ytimellä

# Ladataan sanat muistiin 
sanat = lataaKirja(kirjanURL)
cat("Kirja ladattu muistiin - kirjassa on", length(sanat), "sanaa. \n")

if(n_ytimet == 1){ # EI RINNAKKAISLASKENTAA
  cat("Lasketaan sanojen pisteet ilman rinnakkaislaskentaa yhdellä ytimellä. \n")
  
  aikaleima <- proc.time()
  tulos <- sapply(sanat, laskeSananPisteet, USE.NAMES = TRUE)
  aikaleima = proc.time() - aikaleima
  
  cat("Laskenta ohi ajassa:", aikaleima[3], "s ilman rinnakkaislaskentaa. \n")
  
}else if(n_ytimet >= 2){ # RINNAKKAISLASKENTA:
  cat("Lasketaan sanojen pisteet rinnakkaislaskennalla käyttäen", n_ytimet, "ydintä. \n")
  
  # Alustetaan rinnakkaislaskenta
  aikaleima <- proc.time()
  cl<-makeCluster(n_ytimet)
  registerDoParallel(cl)
  
  # Jaetaan taulukko n-osaan ja käydään jokainen osa läpi omalla ytimellä.
  jaa_taulukko <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
  tulos <- foreach(osa = jaa_taulukko(sanat, n_ytimet), .combine='c', .inorder=FALSE) %dopar% {
    sapply(osa, laskeSananPisteet, USE.NAMES = TRUE)
  }
  stopCluster(cl)
  
  aikaleima = proc.time() - aikaleima
  cat("Rinnakkaislaskenta ohi ajassa:", aikaleima[3], "s", n_ytimet, "ytimellä. \n")
}


##### TULOSTA VASTAUKSET ####
cat("----------- \n")
cat("Maksimipisteet sai sana \n")
maxSana = names(which(tulos == max(tulos)))
maxPisteet = max(tulos)
cat(sprintf("%s %d pisteellä!", maxSana, maxPisteet))

