# charlotte1
projet1 logiciel statistique
# devoir individuelle_logiciel_statistique
#nom & prenom : Achoundong Charlotte, Matricule:11367441

#Partie 0 – Importation des données 

data_1<-read.csv("C:/Users/Admin/Desktop/projet2_logiciel-statistique/2002-2018-property-sales-data.csv",header=T,sep=",")
data_1
View(data_1)
#Partie 1 – Traitement des dates et analyse

#2-Renomme la colonne Taxkey pour Tax.Key

names(data_1)[names(data_1) == "Taxkey"] <- "Tax.Key"
View(data_1)

#3-Ajoutons 2 colonnes supplémentaires au fichier

data_1$Sale_year<-substr(data_1$Sale_date,1,4)
data_1$Sale_month<-substr(data_1$Sale_date,6,7)
View(data_1)

#4- Ajout et conversion 

data_1$Sale_date <- lapply(data_1$Sale_date, function(x) paste0(x, "-01"))
data_1$Sale_date <- as.Date(unlist(data_1$Sale_date), format = "%Y-%m-%d")
View(data_1)

#5- a)annee des volumes de vente eleve en nombre de tranactions

year_counts <- table(data_1$Sale_year)
max_year <- names(which.max(year_counts))
max_year
#en 2018 les volumes de ventes etaient plus elévé en terme de nombre de transactions

#5- b) mois des volumes de ventes eleve en dollars(prix de vente)

data_1$Sale_month_new <- format(data_1$Sale_date, "%Y-%m")
monthly_sales <- aggregate(Sale_price ~ Sale_month_new, data = data_1, sum)
max_month <- monthly_sales$Sale_month_new[which.max(monthly_sales$Sale_price)]
max_month
# en juin les ventes etaient elevés en dollars

# Partie 2 – Traitements sur les chaînes de caractères et des numériques 

#6- liste des valeurs différentes que prend la variable « Proptype »

unique(data_1$PropType)
# "Commercial"  "Residential" "Vacant Land" "Office"      "Warehouse"

#7- l’extraction des lignes pour lesquelles la valeur de Proptype est vide

Proptype_vide <- data_1[is.na(data_1$PropType) | data_1$PropType == "", ]
Proptype_vide

#8- Extraction de  toutes les lignes de la table de données pour lesquelles le style commence soit parOffice Building soit Warehouse Building 

data_1_style <- data_1[grepl("^Office Building|^Warehouse Building", data_1$Style), ]
data_1_style
#proptype vide

#remplacement

data_1$PropType[is.na(data_1$PropType) & grepl("^Office Building", data_1$Style)] <- "Office"
data_1$PropType[is.na(data_1$PropType) & grepl("^Warehouse Building", data_1$Style)] <- "Warehouse"

data_1$PropType[
  (data_1$PropType == "" | is.na(data_1$PropType)) & grepl("^Office Building", data_1$Style)
] <- "Office"

data_1$PropType[
  (data_1$PropType == "" | is.na(data_1$PropType)) & grepl("^Warehouse Building", data_1$Style)
] <- "Warehouse"

#9- regroupement des valeurs de « Residential », « Lg Apartment » et « Condominium » sous la valeur « Residential 

data_1$PropType[data_1$PropType %in% c("Residential", "Lg Apartment", "Condominium")] <- "Residential"
data_1$PropType[data_1$PropType %in% c("Residential", "Lg Apartment", "Condominium")]

#10- Création d'une colonne nommée index_prix_vente qui indiquera à quel point le prix est élevé : 1 faisant référence au prix le plus bas et le prix le plus haut prendra comme valeur le nombre d’observations de la table. 

data_1$index_prix_vente <- rank(data_1$Sale_price, ties.method = "first")
data_1$index_prix_vente

#Partie 3 – Représentation graphique des ventes 

#11-Conversion de  la variable « Sale_year » en une variable catégorielle (factor).

data_1$Sale_year <- as.factor(data_1$Sale_year)

# 12-Création de  2 tables, nb_transactions et avg_price, qui contiendront respectivement le nombre de transactions réalisées par année (Sale_year) et le prix moyen des ventes par années.  

nb_transactions <- aggregate(Sale_price ~ Sale_year, data = data_1, FUN = length)
names(nb_transactions)[2] <- "nb_transactions"
nb_transactions

avg_price <- aggregate(Sale_price ~ Sale_year, data = data_1, FUN = mean)
names(avg_price)[2] <- "avg_price"
avg_price

#13-representation graphique

par(mfrow = c(1, 2))
plot(nb_transactions$Sale_year, nb_transactions$nb_transactions,
     type = "b", col = "blue", pch = 19,
     xlab = "Année", ylab = "Nombre de transactions")

plot(avg_price$Sale_year, avg_price$avg_price,
     type = "b", col = "red", pch = 19,
     xlab = "Année", ylab = "Prix moyen")

mtext("Nb of transactions VS average sale price",
      outer = TRUE, cex = 1.5, line = -2)
dev.off()

#14- modification du code de sorte qu’en plus de partager le résultat par année, le résultat soit en plus partagé par type de propriété PropType.

nb_transactions2 <- aggregate(Sale_price ~ Sale_year + PropType,
                              data = data_1, FUN = length)
names(nb_transactions2)[3] <- "nb_transactions"
nb_transactions2

avg_price2 <- aggregate(Sale_price ~ Sale_year + PropType,
                        data = data_1, FUN = mean)
names(avg_price2)[3] <- "avg_price"
avg_price2

#15- Écriture de la fonction qui se nomme « plot_by_Proptype »  

plot_by_Proptype <- function(Proptype_list) {
  nb_proptype <- length(Proptype_list)
  par(mfrow = c(nb_proptype, 2), mar = c(5, 5, 2, 2), oma = c(2, 2, 4, 2))
  for(i in Proptype_list){
  
 # nombre transactions 
    
 plot(
      nb_transactions2$Sale_year[nb_transactions2$PropType == i], 
      nb_transactions2$nb_transactions[nb_transactions2$PropType == i],
      type = "b",
      xlab = paste0("Sale year - ", i),
      ylab = "Nb of transactions",
      las = 2,
      col = "blue",
      pch = 19
    )
# Prix moyen 
 plot(
      avg_price2$Sale_year[avg_price2$PropType == i], 
      avg_price2$avg_price[avg_price2$PropType == i],
      type = "b",
      xlab = paste0("Sale year - ", i),
      ylab = "Avg sale price",
      las = 2,
      col = "red",
      pch = 19
    ) }
  
# Titre 
  
  mtext("Nb of transaction VS average sale price", outer = TRUE, cex = 1.2, line = 1)
   }

#16-Appelons la fonction pour les valeurs « Commercial » et « Residential »

plot_by_Proptype(c("Commercial", "Residential"))

# Partie 4 – Croisement d’information

#17-importation des données 

data_2<-read.csv("C:/Users/Admin/Desktop/projet2_logiciel-statistique/unpaid_tax.csv",header=T,sep=",")
data_2
View(data_2)

merge_delinq <- merge(data_1, data_2, by = "Tax.Key", all = FALSE)
merge_delinq
head(merge_delinq)
dim(merge_delinq)
#dimmension : 1827   34

#18-existatnce des doublons ou pas

# verification des  doublons

tax_count <- table(merge_delinq$Tax.Key)
tax_count
#oui il ya des doublons dans cette nouvelle base de données

tax_count_sorted <- sort(tax_count, decreasing = TRUE)
tax_count_sorted

# cette jointure simple  ne fait pas de sens parceque dans fichier data_1 Tax.Key peut apparaitre plus dune fois
#si une propriete a ete vendu plus d'une fois,il en est de meme dans data_2  ou les taxes sont impayées sur plusieurs périodes
# ce qui entraine des doublons dans la jointure
# pour palier a cela , on peut garder uniquement les derniere vente
