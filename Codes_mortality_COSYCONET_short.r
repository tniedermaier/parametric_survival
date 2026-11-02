
rm(list=ls(all=TRUE))
library(haven)

#alt:
old <- read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\D_endvisite_all_a.sas7bdat")

#neu:
new <- read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\endvisite_all.sas7bdat")

class(old) <- class(new) <- "data.frame"

names(old)[names(old)=="ev_1_grund_ende"] <- "ev_1_grund_ende_old"
names(old)[names(old)=="ev_1a_grund_andere"] <- "ev_1a_grund_andere_old"
dat <- merge(old[,c("subject", "ev_1_grund_ende_old", "ev_1a_grund_andere_old")], new[,c("subject","ev_1_grund_ende", "ev_1a_grund_andere")])

(tmp <- merge(old[,c("subject","ev_2_sterb_dat")], new[,c("subject","ev_2_sterb_dat")], by="subject"))

tmp$date_died <- tmp$ev_2_sterb_dat.x

#Leute mit bekanntem Sterbedatum im neuen (umfangreicheren) Datensatz:
IDs_mortality <- dat[dat$subject %in% new[!is.na(new$ev_2_sterb_dat),]$subject,]$subject
length(IDs_mortality) #318 (vs. 297 im alten Datensatz).

#Relevante Datensatzteile einlesen:
Basis <- haven::read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\B_basis_a.sas7bdat") #für die Datumsvariablen
Dropout <- haven::read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\C_drop_out_a.sas7bdat") #für das Dropout-Datum
class(Basis) <- class(Dropout) <- "data.frame"

#Mergen zu einem Datensatz:
## Erst mal Spaltennamen "subject" und "SUBJECT" vereinheitlichen zu "subject":
colnames(Basis)[colnames(Basis)=="SUBJECT"] <- "subject"

#"study" löschen bei dazugemergten Datensätzen, weil sonst Fehler:
Dropout$study <- NULL
#Ebenso redundante Datumsvariablen im Drop-out-Datensatz:
Dropout[,c("DATUM_V1","DATUM_V2","DATUM_V3","DATUM_V4","DATUM_V5","DATUM_V6","DATUM_V7")] <- NULL

#merge Basis, Dropout
Basis
Basis$STUDY <- NULL
cosy <- merge(Basis, Dropout, by="subject", all.x=TRUE)
rm(Basis, Dropout)


#Dupletten löschen:
cosy <- cosy[-which(cosy$subject=="251-189-626" & cosy$lauf_2==71),]
cosy <- cosy[-which(cosy$subject=="738-056-394" & cosy$lauf_2==234),]

#Dropout-Kodierung:
ev_dat <- haven::read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\endvisite_all.sas7bdat")
class(ev_dat) <- "data.frame"

ev_dat$ev_1_grund_ende2 <- ev_dat$ev_1_grund_ende
ev_dat[ev_dat$ev_1_grund_ende==2,]$ev_1_grund_ende2 <- "died"
ev_dat[ev_dat$ev_1_grund_ende %in% c(1,3:11),]$ev_1_grund_ende2 <- "other"

table(ev_dat$ev_1_grund_ende2) #Sollten ... Leute mit "other" als Grund sein, die händisch zuzuordnen sind.

dropout_reasons <- readxl::read_xlsx("S:\\AG_Holle\\TN\\1_COSYCONET\\other_dropout_reasons\\all_people_with_other_dropout_reason_new_final.xlsx")
class(dropout_reasons) <- "data.frame"
dropout_reasons[,"ev_1a_grund_andere"] <- NULL #Lösche ev_1a_grund_andere. Letzteres, weil schon vorhanden in ev_dat.

dropout_reasons$X <- NULL
x <- intersect(ev_dat$subject, dropout_reasons$subject); length(x) #580
y <- merge(dropout_reasons, ev_dat, by="subject")
ev_dat2 <- merge(ev_dat, y[,c("subject","Kategorisierung_TN")], by="subject", all=TRUE)

cosy <- merge(cosy, ev_dat2[,c("subject","ev_1_grund_ende2","Kategorisierung_TN")], by="subject", all=TRUE)
cosy[!is.na(cosy$Kategorisierung_TN),]$ev_1_grund_ende2 <- cosy[!is.na(cosy$Kategorisierung_TN),]$Kategorisierung_TN

cosy$dropout_reason_name <- NA
cosy[!is.na(cosy$ev_1_grund_ende2) & cosy$ev_1_grund_ende2=="died",]$dropout_reason_name <- "died"
cosy[!is.na(cosy$ev_1_grund_ende2) & cosy$ev_1_grund_ende2 %in% c("too_ill","hospitalized","nursing_care","moved","no_time","not_interested","not_reached","other","too_much_effort","unknown","Zentrum_geschlossen"),]$dropout_reason_name <- "other"
cosy[!is.na(cosy$ev_1_grund_ende2) & cosy$ev_1_grund_ende2==12,]$dropout_reason_name <- "no_dropout"

#cosy[!is.na(cosy$dropout_reason) & cosy$ev_1_grund_ende2=="no_COPD",]$dropout_reason_name <- "no_COPD" #Keine Leute mehr, da ausgeschlossen.
table(cosy$dropout_reason_name, exclude=NULL)
cosy[is.na(cosy$dropout_reason_name),c("ev_1_grund_ende2")] #40 "no_COPD"

#Variablen definieren:
cosy <- merge(cosy, tmp, by="subject", all.x=TRUE)

cosy[,c("DATUM_V1","DATUM_V2","DATUM_V3","DATUM_V4","DATUM_V5","DATUM_V6","DATUM_V7")]

cosy[,c("date_died","ev_2_sterb_dat.x","ev_2_sterb_dat.y")]
nrow(cosy[!is.na(cosy$date_died) & is.na(cosy$ev_2_sterb_dat.x),]) #keine
nrow(cosy[!is.na(cosy$date_died) & is.na(cosy$ev_2_sterb_dat.y),]) #keine
nrow(cosy[!is.na(cosy$ev_2_sterb_dat.x) & is.na(cosy$ev_2_sterb_dat.y),]) #keine
#D.h. ev_2_sterb_dat.y hat die wenigsten Missings und die anderen beiden Variablen steuern nichts an Infos bei.
cosy[,c("date_died","ev_2_sterb_dat.x")] <- NULL
cosy$date_died <- cosy$ev_2_sterb_dat.y
cosy$ev_2_sterb_dat.y <- NULL
#Jetzt nur noch eine Variable "date_died" vorhanden mit allen (direkt) verfügbaren Sterbedaten.

nrow(cosy[cosy$dropout_reason_name=="died" & is.na(cosy$date_died),]) #(Nur) 27 Leute, bei denen man Sterbedatum imputieren kann.

cosy[cosy$dropout_reason_name=="died" & is.na(cosy$date_died),c("DATUM_V1","DATUM_V2","DATUM_V3","DATUM_V4","DATUM_V5","DATUM_V6","DATUM_V7")]

#1.) Wenn V1 vorhanden und V2 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V1 + 3 Monate.
cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V1) & is.na(cosy$DATUM_V2),]$date_died <- cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V1) & is.na(cosy$DATUM_V2),]$DATUM_V1 + 90

#2.) Wenn V2 vorhanden und V3 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V2 + 6 Monate.
cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V2) & is.na(cosy$DATUM_V3),]$date_died <- cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V2) & is.na(cosy$DATUM_V3),]$DATUM_V2 + 180

#3.) Wenn V3 vorhanden und V4 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V3 + 9 Monate.
cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V3) & is.na(cosy$DATUM_V4),]$date_died <- cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V3) & is.na(cosy$DATUM_V4),]$DATUM_V3 + 270

#4.) Wenn V4 vorhanden und V5 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V4 + 9 Monate.
cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5),]$date_died <- cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5),]$DATUM_V4 + 270

#5.) Wenn V5 vorhanden und V6 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V5 + 9 Monate.
cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6),]$date_died <- cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6),]$DATUM_V5 + 270

#6.) Wenn V6 vorhanden und V7 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V6 + 9 Monate.
cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$date_died <- cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$DATUM_V6 + 270

#7.) Wenn V7 vorhanden und Sterbedatum fehlt: angenommenes Sterbedatum = DATUM_V7 + 9 Monate.
cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V7),]$date_died <- cosy[!is.na(cosy$dropout_reason_name) & cosy$dropout_reason_name=="died" & is.na(cosy$date_died) & !is.na(cosy$DATUM_V7),]$DATUM_V7 + 270

sum(!is.na(cosy$date_died)) #345. Der ganze Aufwand hat die Zahl der verfügbaren Sterbedaten also von 318 auf 345 erhöht bzw. um 27 Werte.
IDs_died <- cosy[!is.na(cosy$date_died),]$subject

cosy$first_date <- cosy$DATUM_V1
cosy$status #if death was observed: status = 1; if survival time was censored: status = 0.
cosy$status <- ifelse(!is.na(cosy$date_died), 1, 0)

cosy$last_date <- NA #Letztes Datum soll sein: Entweder Sterbedatum, falls vorhanden, oder drout_datum, falls vorhanden, oder Mittelwert zw. letzter vorhandener Visite und nächster fehlender Visite.

#Sterbedatum, falls vorhanden:
cosy[!is.na(cosy$date_died),]$last_date <- cosy[!is.na(cosy$date_died),]$date_died

#drout_datum, falls vorhanden:
cosy[is.na(cosy$last_date) & !is.na(cosy$drout_datum),]$last_date <- cosy[is.na(cosy$last_date) & !is.na(cosy$drout_datum),]$drout_datum

#1.) Wenn V1 vorhanden und V2 ff. fehlt: angenommenes Zensierungsdatum = DATUM_V1
cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V1) & is.na(cosy$DATUM_V2) & is.na(cosy$DATUM_V3) & is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$last_date <- cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V1) & is.na(cosy$DATUM_V2) & is.na(cosy$DATUM_V3) & is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$DATUM_V1

#2.) Wenn V2 vorhanden und V3 ff. fehlt: angenommenes Zensierungsdatum = DATUM_V2
cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V2) & is.na(cosy$DATUM_V3) & is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$last_date <- cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V2) & is.na(cosy$DATUM_V3) & is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$DATUM_V2

#3.) Wenn V3 vorhanden und V4 ff. fehlt: angenommenes Zensierungsdatum = DATUM_V3
cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V3) & is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$last_date <- cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V3) & is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$DATUM_V3

#4.) Wenn V4 vorhanden und V5 fehlt: angenommenes Zensierungsdatum = DATUM_V4
cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$last_date <- cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V4) & is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$DATUM_V4

#5.) Wenn V5 vorhanden und V6 fehlt: angenommenes Zensierungsdatum = DATUM_V5
cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$last_date <- cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V5) & is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$DATUM_V5

#6.) Wenn V6 vorhanden und V7 fehlt: angenommenes Zensierungsdatum = DATUM_V6
cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$last_date <- cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V6) & is.na(cosy$DATUM_V7),]$DATUM_V6

#7.) Wenn V7 vorhanden: angenommenes Zensierungsdatum = DATUM_V7
cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V7),]$last_date <- cosy[is.na(cosy$last_date) & !is.na(cosy$DATUM_V7),]$DATUM_V7

cosy$last_date #Enthält jezt für alle Leute einen Wert.
cosy$time <- as.Date(cosy$last_date) - cosy$first_date

cosy[cosy$time==0,c("DATUM_V1","DATUM_V2","DATUM_V3","DATUM_V4","DATUM_V5","DATUM_V6","DATUM_V7")] #ok
cosy$last_date #Enthält jezt für alle Leute einen Wert.

#Beschränken auf relevante Variablen:
cosy <- cosy[,c("subject","time","status")]

#Datensatz mit nur den relevanten Variablen exportieren:
write.csv2(cosy, "cosy_mortality.csv")
#Und wieder einlesen (zum Beweis, dass es damit durchläuft):
cosy <- read.csv2("cosy_mortality.csv")
