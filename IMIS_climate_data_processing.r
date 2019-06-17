#################################################################
######       Nettoyage + reconstruction des donnees +      ######
######              calcul des dates de fonte              ######
######             des series climatiques IMIS             ######
######                                                     ######
######          KLEIN Geoffrey  - Septembre 2016           ######
#################################################################

# 2mAT: T°C de l'air a 2m ; GST: T°C du sol a 0m ; SST: T°C de surface de la neige

#######################################################
  ## Nettoyage de ce qui a ete predecemment fait sous R
#######################################################

    rm(list=ls())

####################################################################################
  ## Choix du repertoire de travail pour la lecture et l'enregistrement des fichiers
####################################################################################

    setwd(choose.dir())

########################################
  ## Lancement des libraires necessaires
########################################

    require(zoo)
    require(reshape2)

###########################
  ## Lancement des fichiers
###########################

# Lancement de la fonction #

    clean.sta <- function(file=NULL){

# Lancement de la liste pour regrouper tous les parametres par station #

    # Neige
    overfinal <- list()
    # Climatiques
    overfinal2 <- list()

# Boucle afin de traiter plusieurs fichiers en meme temps #

    if (is.null(file))
    file <- choose.files()

    for (i in 1:length(file)) {

# Lecture du fichier pour une station #

    myts <- read.table(file[i], sep=";", head=T, stringsAsFactors=F)

# Isoler le nom de la station #

    nom_sta <- myts[nrow(myts),1]

#######################################################
  ## Preparation des donnees et traitement preliminaire
#######################################################

# Convertir en numerique les colonnes necessaires #

    myts[,2:6] <- sapply(myts[,2:6], as.numeric)

# Remplacer par NA les lignes identiques pour toutes les temperatures (2mAT, GST & SST) #

    identique <- function(x){
	  u<-unique(x)
  	if(length(u)==1)
	  { res<-rep(NA,length(x)) }
  	else { res <- x }
  	return(res)
    }

    # Application de la fonction precedente
    myts[(2:(nrow(myts)-1)),2:4]<-t(apply(myts[(2:(nrow(myts)-1)),2:4],1,identique))
    myts[,2] <- as.numeric(myts[,2])
    myts[,3] <- as.numeric(myts[,3])
    myts[,4] <- as.numeric(myts[,4])

# Supprimer les donnees fausses & non nettoyables manuellement detectees pour certaines stations #

    if(nom_sta=="ANV3"){
        myts[,5] <- ifelse(myts[,5] %in% c(56,57,91,92,142:148,162,248,264,342,472,585:595),NA,myts[,5]) }
    if(nom_sta=="ARO2"){
        myts[,5] <- ifelse(myts[,5] %in% c(83,95,155,239,240,271,473,528,618,623,627,628,629),NA,myts[,5]) }
    if(nom_sta=="ATT2"){
        myts[146400:149157,3] <- NA
        myts[12950:13500,5] <- NA
        myts[,5] <- ifelse(myts[,5] %in% c(105:115,196:198),NA,myts[,5]) }
    if(nom_sta=="BED2"){
        myts[244050:244973,2] <- NA }
    if(nom_sta=="BER3"){
        myts[,5] <- ifelse(myts[,5] %in% c(219,137,148,151:160,256,358,409,429,560:569),NA,myts[,5])
        myts[238000:241520,5] <- 0
        myts[241520:244840,5] <- NA }
    if(nom_sta=="BOG2"){
        myts[,5] <- ifelse(myts[,5] %in% c(405,472,519,610:626),NA,myts[,5])
        myts[26400:31550,3] <- NA }
    if(nom_sta=="BOV2"){
        myts[,5] <- ifelse(myts[,5] %in% c(92,207,247,259,307,350,363,456,590:610),NA,myts[,5]) }
    if(nom_sta=="DAV3"){
        myts[,5] <- ifelse(myts[,5] %in% c(368,369,388,400,428,488,529:533,541,552,559),NA,myts[,5])
        myts[c(251400:251900,268300:271850),3] <- NA }
    if(nom_sta=="DAV5"){
        myts[,5] <- ifelse(myts[,5] %in% c(0,102:106,110:117,139:161),NA,myts[,5]) }
    if(nom_sta=="DIA2"){
        myts[c(25148),3] <- NA
        myts[,3] <- ifelse(myts[,3] %in% c(191,192),NA,myts[,3]) }
    if(nom_sta=="DTR2"){
        myts[,5] <- ifelse(myts[,5] %in% c(0,323,335,548,549),NA,myts[,5])
        myts[c(332850:nrow(myts)),5] <- 0 }
    if(nom_sta=="EGH2"){
        myts[203593:204645,3:4] <- NA }
    if(nom_sta=="ELA2"){
        myts[,5] <- ifelse(myts[,5] %in% c(468:474),NA,myts[,5]) }
    if(nom_sta=="ELM2"){
        myts[,5] <- ifelse(myts[,5] %in% c(195,200,220,252,280,285,289,302,309,328,343,347,419,438,456,457,
        471,474,484,488,491,500,524,547,556,569,562,619:625),NA,myts[,5]) }
    if(nom_sta=="FIS2"){
        myts[c(332965:332995),5] <- 37 }
    if(nom_sta=="FNH2"){
        myts[,5] <- ifelse(myts[,5] %in% c(0,613,616,618),NA,myts[,5]) }
    if(nom_sta=="GAD2"){
        myts[155450:156565,2] <- NA }
    if(nom_sta=="GAN2"){
        myts[,5] <- ifelse(myts[,5] %in% c(0,18,27,77:80,104,108,111,112,115,121,125:129,131,136,139:147,
        150,151,238,243,324,383,392,466,488,529,556,578:600),NA,myts[,5])
        myts[79577:79590,5] <- NA }
    if(nom_sta=="GOR2"){
        myts[44288:44440,3] <- NA }
    if(nom_sta=="ILI2"){
        myts[,5] <- ifelse(myts[,5] %in% c(78,166,175,226,441,536,609),NA,myts[,5])}
    if(nom_sta=="KES2"){
        myts[,5] <- ifelse(myts[,5] %in% c(8,9),NA,myts[,5]) }
    if(nom_sta=="KLO3"){
        myts[,5] <- ifelse(myts[,5] %in% c(0,2,3,4,8,10,20,99,274,285,288,323,324,341,344,356,359,379:383,388,391,
        395,400,402,415,419,421,431,453:465,471,472,490:500,527:535),NA,myts[,5]) }
    if(nom_sta=="LAG2"){
        myts[c(100000:100170,100282:100350),2] <- NA
        myts[c(203130:206890,207000:207720,208800:217800),3] <- NA
        myts[c(51420:51450,53990:54110),4] <- NA
        myts[,5] <- ifelse(myts[,5] %in% c(70,98,146,149,155,177,223,268,316),NA,myts[,5]) }
    if(nom_sta=="LAG3"){
        myts[91277:91620,2:5] <- NA
        myts[,5] <- ifelse(myts[,5] %in% c(94,97,98,100,135,136,149,154,158,178,
        193,196,253,260:312),NA,myts[,5]) }
    if(nom_sta=="LAU2"){
        myts[235350:236065,2] <- NA }
    if(nom_sta=="LHO2"){
        myts[,5] <- ifelse(myts[,5] %in% c(70,134,135,140,141,157:168,194,242,557,558),NA,myts[,5])
        myts[c(31075:31370,31455:31560,31860:31895,32685:32755,32930:33050,33145:33200,33560:33825,33995:34015,
        34180:34490),5] <- NA }
    if(nom_sta=="MAE2"){
        myts[c(222390:225000,225960:226420,226780:234370),3] <- NA }
    if(nom_sta=="MUT2"){
        myts[,5] <- ifelse(myts[,5] %in% c(391,398,399,411),NA,myts[,5]) }
    if(nom_sta=="NAR2"){
        myts[,5] <- ifelse(myts[,5] %in% c(50,64,150,163,566,680:690),NA,myts[,5]) }
    if(nom_sta=="NAS2"){
        myts[,5] <- ifelse(myts[,5] %in% c(130,157:165),NA,myts[,5]) }
    if(nom_sta=="NEN2"){
        myts[129360:130085,5] <- NA }
    if(nom_sta=="OBW3"){
        myts[,5] <- ifelse(myts[,5] %in% c(96,628,629),NA,myts[,5]) }
    if(nom_sta=="PAR2"){
        myts[,5] <- ifelse(myts[,5] %in% c(540:560),NA,myts[,5])
        myts[80279:86285,2] <- NA }
    if(nom_sta=="PUZ2"){
        myts[,5] <- ifelse(myts[,5] %in% c(581:592),NA,myts[,5]) }
    if(nom_sta=="ROA2"){
        myts[,5] <- ifelse(myts[,5] %in% c(277,608,609),NA,myts[,5]) }
    if(nom_sta=="ROT2"){
        myts[133700:134175,2] <- NA
        myts[132090:135287,5] <- NA
        myts[,5] <- ifelse(myts[,5] %in% c(59,61,124,198,215,216,223,258,297,597,373,384,448,453,478,591:599),NA,myts[,5]) }
    if(nom_sta=="SAA2"){
        myts[,5] <- ifelse(myts[,5] %in% c(136,171,173,184,194,187,197,205,244,249,264,270,286,308,311,332,338,346,
        384,432,442,446,458,473,524,527,528:532,540,592:598),NA,myts[,5]) }
    if(nom_sta=="SAA3"){
        myts[c(193360:193530),5] <- NA }
    if(nom_sta=="SCA3"){
        myts[,5] <- ifelse(myts[,5] %in% c(555:559),NA,myts[,5])
        myts[97360:97387,5] <- NA }
    if(nom_sta=="SCB2"){
        myts[,5] <- ifelse(myts[,5] %in% c(31,92,531:539),NA,myts[,5]) }
    if(nom_sta=="SHE2"){
        myts[231000:231316,2] <- NA }
    if(nom_sta=="SIM2"){
        myts[,5] <- ifelse(myts[,5] %in% c(139,150,213:229,234,390,399,453,499,513,568:579),NA,myts[,5])
        myts[60050:60220,5] <- NA }
    if(nom_sta=="SLF2"){
        myts[c(173600:173845,174035:174380,174597:174720,175900:175980,176196:176650,178470:180143,182380:185000,
        190540:191200,243795:243830,244365:244380,244715:245350,245795:245810,246220:246370,248770:249793),3] <- NA
        myts[,5] <- ifelse(myts[,5] %in% c(14,34,67:71,79,80,88,97,103,108,128:142,156,159,160,163,169,194,
        213,215,237:240,244:248,282:284,355,367,405,450:455,529,561,562,568,569),NA,myts[,5])
        myts[c(174390:175610,175990:176010,176035:176055,234730:236260),5] <- NA }
    if(nom_sta=="SPN2"){
        myts[,5] <- ifelse(myts[,5] %in% c(563,638,639),NA,myts[,5]) }
    if(nom_sta=="SPN3"){
        myts[c(149480:149497,307650:307682),5] <- NA }
    if(nom_sta=="STH2"){
        myts[235285:236150,2] <- NA }
    if(nom_sta=="STN2"){
        myts[,5] <- ifelse(myts[,5] %in% c(191,231,295,417,443,522,594,595),NA,myts[,5]) }
    if(nom_sta=="TIT2"){
        myts[220730:222435,3:4] <- NA }
    if(nom_sta=="TUJ3"){
        myts[,5] <- ifelse(myts[,5] %in% c(16,23,24,31,32,588),NA,myts[,5]) }
    if(nom_sta=="URS2"){
        myts[,5] <- ifelse(myts[,5] %in% c(86,87,159,232:236,306:309,380,379,385,451:454,525:533,598:609),NA,myts[,5]) }
    if(nom_sta=="VIN2"){
        myts[187000:192000,5][myts[187000:192000,5] >= 100] <- NA
        myts[,5] <- ifelse(myts[,5] %in% c(25,53,54,60,65:67,73,90),NA,myts[,5]) }
    if(nom_sta=="WFJ2"){
        myts[,5] <- ifelse(myts[,5] %in% c(0,351,674,998),NA,myts[,5]) }
    if(nom_sta=="ZER4"){
        myts[c(32370:32437,34855:34880),5] <- NA
        myts[,5] <- ifelse(myts[,5] %in% c(74,77,78),NA,myts[,5]) }

# Reboucher les trous 30mn pour les stations ayant produit des donnees toutes les heures #

    reb30mn <- function(x){
        for (i in 2:(length(x)-2)) {
            if(!is.na(x[i-1]) & is.na(x[i]) & !is.na(x[i+1]) & is.na(x[i+2])){
            x[i] <- mean(c(x[i-1],x[i+1])) }
            }
            return(x)
            }

    myts[,c(2:6)] <- apply(as.data.frame(myts[,c(2:6)]),2,reb30mn)
    myts[,5] <- round(myts[,5],0)

####################################################################
  ## Nettoyage general des donnees climatiques (valeurs ponctuelles)
####################################################################

#####################
#### Temperature ####
#####################

# Suppression des valeurs impossibles pour la 2mAT et SST (>+40°C & <-40°C) #

    impossible1 <- function (x){
    st <- ifelse (x<(-40) | x>40, NA,x)
    return(st)
    }

    myts[,c(2,4)] <- apply(as.data.frame(myts[,c(2,4)]),2,impossible1)
    myts[,2] <- as.numeric(myts[,2])
    myts[,4] <- as.numeric(myts[,4])

# Suppression des valeurs impossibles pour la GST (>+50°C & <-25°C) #

    impossible2 <- function (x){
    st2 <- ifelse (x<(-25) | x>50, NA,x)
    return(st2)
    }

    myts[,3] <- apply(as.data.frame(myts[,3]),2,impossible2)
    myts[,3] <- as.numeric(myts[,3])

###############
#### Neige ####
###############

# Suppression des valeurs impossibles (<= 0 & >=600cm) #

    impossible3 <- function (x){
    st3 <- ifelse (x<=0 | x>=600,NA,x)
    return(st3)
    }

    myts[,5] <- apply(as.data.frame(myts[,5]),2,impossible3)
    myts[,5] <- as.numeric(myts[,5])

#######################################################################################
  ## Nettoyage specifique des donnees climatiques (sequence valeurs d'une meme colonne)
#######################################################################################

#####################
#### Temperature ####
#####################

# Suppression des sauts brutaux de T°C de sequences de donnees <=50 donnees pour tous les capteurs #

    out2NA <- function(x,seuil){
    st1 = NULL
    # Memorisation de la derniere valeur correcte
    temp <- st1[1] <- x[1]
    # Creation d'une sequence equivalente a la colonne traitee
    st1 <- x
    # Creation de la reconnaissance temporelle de la boucle (pour les ecarts)
    tps <- time(x)
    ind_temp <- 1
    # Boucle de traitement
    for (i in 2:length(x)){
        if((!is.na(x[i])) & (!is.na(x[i-1])) & (abs((x[i])-(temp)) >= seuil) & (tps[i]-tps[ind_temp] <= 50)){
    st1[i] <- NA }
    else {
    temp <- x[i]
    ind_temp <- i } }
    return(st1)
    }

    myts[,2] <- apply(as.data.frame(myts[,2]),2,function(x) out2NA(x,8)) # Modifier le 8°C si besoin (ecart max) #
    myts[,2] <- as.numeric(myts[,2])
    myts[,3] <- apply(as.data.frame(myts[,3]),2,function(x) out2NA(x,15)) # Modifier le 15°C si besoin (ecart max) #
    myts[,3] <- as.numeric(myts[,3])
    myts[,4] <- apply(as.data.frame(myts[,4]),2,function(x) out2NA(x,15)) # Modifier le 15°C si besoin (ecart max) #
    myts[,4] <- as.numeric(myts[,4])

# Si la var de la 2mAT ou de la SST est nulle sur les 10 valeurs suivantes, alors NA #

    # Creation colonne pour la variance pour les 2 parametres
    myts$variance2mAT <- myts$temp_200cm
    myts$varianceSST <- myts$temp_surf_neige
    # Calcul de la variance
    vartemp <- function(x){
        for (i in 1:(length(x))) {
            if((!is.na(x[i]))) {
            x[i] <- var(x[i:(i+10)],na.rm=T)
            }
            else {x[i] <- NA}
            }
            return(x)
            }

    myts[,8] <- apply(as.data.frame(myts[,8]),2,vartemp)
    myts[,8] <- as.numeric(myts[,8])
    myts[,9] <- apply(as.data.frame(myts[,9]),2,vartemp)
    myts[,9] <- as.numeric(myts[,9])

    # Suppression des T°C fausses
    supprtemp <- function(x,y){
        for (i in 1:length(x)) {
            if((!is.na(x[i])) & (!is.na(y[i])) & (y[i]==0)) {
            x[i] <- NA
            }
            }
            return(x)
            }

    myts[,2] <- mapply(supprtemp,myts[,2],myts[,8])
    myts[,4] <- mapply(supprtemp,myts[,4],myts[,9])
    # Suppression des colonnes creees
    myts$variance2mAT <- NULL
    myts$varianceSST <- NULL

# Si la GST >7°C & sa var <=0.1 sur les 20 valeurs suivantes, alors NA #

    # Creation colonne pour la variance
    myts$varianceGST <- myts$temp_0cm
    # Calcul de la variance
    vartemp2 <- function(x){
        for (i in 1:(length(x))) {
            if((!is.na(x[i]))) {
            x[i] <- var(x[i:(i+20)],na.rm=T)
            }
            else {x[i] <- NA}
            }
            return(x)
            }

    myts[,8] <- apply(as.data.frame(myts[,8]),2,vartemp2)
    myts[,8] <- as.numeric(myts[,8])

    # Suppression des T°C fausses
    supprtemp2 <- function(x,y){
        for (i in 1:length(x)) {
            if((!is.na(x[i])) & (!is.na(y[i])) & (x[i]>7) & (y[i]<=0.1)) {
            x[i] <- NA
            }
            }
            return(x)
            }

    myts[,3] <- mapply(supprtemp2,myts[,3],myts[,8])
    # Suppression de la colonne creee
    myts$varianceGST <- NULL

# Suppression des bugs hivernaux pour la GST (sequence courte de T°C < -10°C): Si la moy des 200 valeurs precedant & suivant
# le bug est <3°C ou =NA & >-3°C ou =NA, alors NA (max 200 donnees) #

    # cree 200 lignes fakes pour avoir des valeurs numeriques au depart et a la fin (en cas de NA a ces endroits)
    myts <- rbind(0,myts)
    rma <- myts[rep(1,199),]
    myts <- rbind(rma,myts)
    test <- myts[1:200,]
    myts <- rbind(myts,test)
    myts[,3] <- as.numeric(myts[,3])

    fun1 <- function(x,n){
     crit <- (x < -10)
     rna <- rle(crit)
     sna <- cumsum(rna$lengths)
     for(i in which(rna$values)){
       prev <- x[(sna[i - 1] - n + 1):sna[i - 1]]
       aftr <- x[(sna[i] + 1):(sna[i] + n)]
       if(((mean(prev,na.rm=T) < 3)|(is.na(mean(prev,na.rm=T)))) & ((mean(aftr,na.rm=T) < 3)|(is.na(mean(aftr,na.rm=T))))
       & ((mean(prev,na.rm=T) > -3)|(is.na(mean(prev,na.rm=T)))) & ((mean(aftr,na.rm=T) > -3)|(is.na(mean(aftr,na.rm=T))))
       & (((sna[i])-(sna[i - 1]))< 200))
       x[(sna[i - 1]):(sna[i])] <- NA
     }
     x
     }

    myts[,3] <- fun1(myts[,3], n = 200)

    # Supprimer les lignes fakes
    myts <- myts[-c(1:200),]
    myts <- myts[-c((nrow(myts)-199):nrow(myts)),]

###############
#### Neige ####
###############

# Si la hauteur de neige >0 et la var de la neige est nulle sur les 100 valeurs suivantes, alors NA #

    # Creation colonne pour la variance de la neige
    myts$varianceneige <- myts$hauteur_neige
    # Calcul de la variance
    varneige <- function(x){
        for (i in 1:(length(x))) {
            if((!is.na(x[i])) & ((x[i])>0)) {
            x[i] <- var(x[i:(i+100)],na.rm=T)
            }
            else {x[i] <- NA}
            }
            return(x)
            }

    myts[,8] <- apply(as.data.frame(myts[,8]),2,varneige)
    myts[,8] <- as.numeric(myts[,8])

    # Suppression de la neige fausse
    supprneige1 <- function(x,y){
        for (i in 1:length(x)) {
            if((!is.na(x[i])) & (!is.na(y[i])) & (y[i]==0)) {
            x[i] <- NA
            }
            }
            return(x)
            }

    myts[,5] <- mapply(supprneige1,myts[,5],myts[,8])
    # Suppression de la colonne creee
    myts$varianceneige <- NULL

# Suppression des augmentations ou chutes brutales de hauteur de neige en 30mn (>=20cm ou <= -10cm) sur max 100 valeurs #

    # Reconstruction des petits trous de NA par la derniere valeur avant le trou (10 valeurs max)
    imputation <- function(x){
    met <- na.locf(x, maxgap = 10,na.rm=FALSE)
    return(met)
    }

    myts[,5] <- apply(as.data.frame(myts[,5]),2,imputation)
    myts[,5] <- as.numeric(myts[,5])

    avalanche <- function(x){
    st1 = NULL
    # Memorisation de la derniere valeur correcte
    temp <- st1[1] <- x[1]
    # Creation d'une sequence equivalente a la colonne traitee
    st1 <- x
    # Creation de la reconnaissance temporelle de la boucle (pour les ecarts)
    tps <- time(x)
    ind_temp <- 1
    # Boucle de traitement
    for (i in 2:(length(x))){
        if((!is.na(x[i])) & (!is.na(x[i-1])) & (x[i]>0) & (((x[i])-(temp) >= 20)|((x[i])-(temp) <= -10)) &
         (tps[i]-tps[ind_temp] <= 100)){
        st1[i] <- NA }
    else {
    temp <- x[i]
    ind_temp <- i } }
    return(st1)
    }

    myts[,5] <- apply(as.data.frame(myts[,5]),2,avalanche)
    myts[,5] <- as.numeric(myts[,5])

#######################################################
  ## Nettoyage specifique de la neige (selon GST & SST)
#######################################################

# Suppression des hauteurs de neige inverifiables (sans GST + SST a cote) #

    inverifiable <- function(x,y,z){
        for (i in 1:length(x)) {
             if(((is.na(x[i])) | (is.na(y[i]))) & (!is.na(z[i]))) {
             z[i] <- NA
            }}
            return(z)
            }

    myts[,5] <- mapply(inverifiable,myts[,3],myts[,4],myts[,5])

# Si la GST varie fortement (>0.2) & que max SST >8°C, alors pas de neige (=0cm) #

    # Creation colonne pour la variance de la GST
    myts$varGST <- myts$temp_0cm
    # Calcul de la variance
    varGST <- function(x){
        for (i in 1:(length(x))) {
            if((!is.na(x[i]))) {
            x[i] <- var(x[i:(i+49)],na.rm=T)
            }
            else {x[i] <- NA}
            }
            return(x)
            }

    myts[,8] <- apply(as.data.frame(myts[,8]),2,varGST)
    myts[,8] <- as.numeric(myts[,8])

    # Creation colonne pour le max de la SST
    myts$maxSST <- myts$temp_surf_neige
    # Calcul du max
    maxSST <- function(x) {
        for (i in 1:(length(x))) {
            if(!is.na(x[i])) {
            x[i] <- (max(x[i:(i+49)],na.rm=T))
            } }
            return(x)
            }
    myts[,9] <- apply(as.data.frame(myts[,9]),2,maxSST)
    myts[,9] <- as.numeric(myts[,9])

    # Creation colonne pour le max de la GST
    myts$maxGST <- myts$temp_0cm
    # Calcul du max
    maxGST <- function(x){
        for (i in 1:(length(x))) {
            if((!is.na(x[i]))) {
            x[i] <- max(x[i:(i+49)],na.rm=T)
            }
            else {x[i] <- NA}
            }
            return(x)
            }

    myts[,10] <- apply(as.data.frame(myts[,10]),2,maxGST)
    myts[,10] <- as.numeric(myts[,10])

    # Suppression de la neige fausse
    supprneige0 <- function(w,x,y,z){
    for (i in 1:length(x)) {

    # Faible hauteur de neige <=30cm ====> Probablement pas de neige #

        if((!is.na(z[i])) & ((z[i])<=30)){

        #### Lorsque GST et SST OK pour valider presence de neige en meme temps

        # Check si tous les parametres sont presents
        if((!is.na(x[i])) & (!is.na(y[i])) & (!is.na(w[i])) &

        # max >= 8°C pour la SST ou max >= 3°C pour la GST ou var > 1 pour la GST
        (((x[i])>=8) | ((y[i])>=3) | ((w[i])>1)))

            { z[i] <- 0 }
        }
        }
        return(z)
        }

    myts[,5] <- mapply(supprneige0,myts[,8],myts[,9],myts[,10],myts[,5])

    # Suppression de la neige fausse
    supprneige2 <- function(w,x,y,z){
    for (i in 1:length(x)) {

    # Forte hauteur de neige >30cm ====> GST isolee #

        if((!is.na(z[i])) & ((z[i])>30)){

        #### Lorsque GST et SST OK pour valider presence de neige en meme temps

        # Check si tous les parametres sont presents
        if((!is.na(w[i])) & (!is.na(x[i])) & (!is.na(y[i])) &

        # max >= 8°C pour la SST
        ((x[i])>=8) &

        # max >= 3°C pour la GST  + var GST >=0.2
        ((y[i])>=3) & ((w[i])>=0.2))

            { z[i] <- 0 }
        }
        }
        return(z)
        }

    myts[,5] <- mapply(supprneige2,myts[,8],myts[,9],myts[,10],myts[,5])

    # Suppression des colonnes creees
    myts$varGST <- NULL
    myts$maxSST <- NULL
    myts$maxGST <- NULL

###############################
  ## Reconstruction des donnees
###############################

#####################
#### Temperature ####
#####################

# Reconstruction des trous hivernaux pour la GST: Si la moy des 500 valeurs precedant & suivant le trou est <2°C & >-2°C,
# & si le max est <6°C, alors on rebouche avec "0°C" (max 500 NA) #

    # cree 500 lignes fakes pour avoir des valeurs numeriques au depart et a la fin (en cas de NA a ces endroits)
    myts <- rbind(0,myts)
    rma <- myts[rep(1,499),]
    myts <- rbind(rma,myts)
    test <- myts[1:500,]
    myts <- rbind(myts,test)
    myts[,3] <- as.numeric(myts[,3])

    fun2 <- function(x,n){
     na <- is.na(x)
     rna <- rle(na)
     sna <- cumsum(rna$lengths)
     for(i in which(rna$values)){
         prev <- x[(sna[i - 1] - n + 1):sna[i - 1]]
         aftr <- x[(sna[i] + 1):(sna[i] + n)]
         if((mean(prev,na.rm=T) < 2) & (mean(aftr,na.rm=T) < 2)
          & (mean(prev,na.rm=T) > -2)  & (mean(aftr,na.rm=T) > -2)
          & (sum(is.na(prev))<(n/2))   & (sum(is.na(aftr))<(n/2))
          & (max(prev,na.rm=T)) < 6    & (max(aftr,na.rm=T) < 6)
          & (((sna[i])-(sna[i - 1]))< 500))
          x[(sna[i - 1]):(sna[i])] <- 0
     }
     x
     }

    myts[,3] <- fun2(myts[,3], n = 500)

    # Supprimer les lignes fakes
    myts <- myts[-c(1:500),]
    myts <- myts[-c((nrow(myts)-499):nrow(myts)),]

# Remplissage des "petits trous" de NA pour toutes les T°C (max 12 donnees: 6h) #

    imputation2 <- function(x){
    met <- na.approx(x, maxgap = 12,na.rm=FALSE)
    return(met)
    }

    myts[,c(2:4)] <- apply(as.data.frame(myts[,c(2:4)]),2,imputation2)

###############
#### Neige ####
###############

# Recreer les donnees en fin de code pour certaines stations afin d'eviter des bugs de R #

    if(nom_sta=="DTR2"){
        myts[c(332850:nrow(myts)),5] <- 0 }
    if(nom_sta=="SLF2"){
        myts[c(332570:nrow(myts)),5] <- 0 }

# Remplissage des "petits trous" de NA (max 48 donnees: 1 jour) #

    imputation2 <- function(x){
    met <- na.approx(x, maxgap = 48,na.rm=FALSE)
    return(met)
    }

    myts[,5] <- apply(as.data.frame(myts[,5]),2,imputation2)
    myts[,5] <- as.numeric(myts[,5])

# Enregistrement des donnees traitees et des dates de fonte #

    write.table(myts, file=paste(nom_sta,"_reconstruit.csv",sep=""), sep=";", quote=F, row.names = FALSE)
    }

    return(myts)
    }

# Choisir le ou les fichiers a traiter, et application de la fonction #

    clean.sta()

########################################################################
                       ## Fin du script ##
########################################################################