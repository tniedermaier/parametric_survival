
rm(list=ls(all=TRUE))
library(haven)

#time0 = Originalzeit in Tagen +1
#time1 = time0/365.25 = Originalzeit in Jahren -> Für SA (no exclusions)
#time2 = time1, aber ohne 1. Jahr
#time3 = time2, aber ohne 1. + letztes Jahr -> Für Hauptanalysen.

#alt:
old <- read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\D_endvisite_all_a.sas7bdat")

#neu:
new <- read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\endvisite_all.sas7bdat")
#Beachten: Im neuen Datensatz sind auch Mortalitätsinfos!

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
Anthro <- haven::read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\A_anthropometrie_all.sas7bdat")
Basis <- haven::read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\B_basis_a.sas7bdat")
Dropout <- haven::read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\C_drop_out_a.sas7bdat")
FEV <- read.csv2("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\J_holle_add_14032023_.csv")
class(Anthro) <- class(Basis) <- class(Dropout) <- "data.frame"

#Mergen zu einem Datensatz:
## Erst mal Spaltennamen "subject" und "SUBJECT" vereinheitlichen zu "subject":
colnames(Basis)[colnames(Basis)=="SUBJECT"] <- "subject"
colnames(FEV)[colnames(FEV)=="SUBJECT"] <- "subject"

#"study" löschen bei dazugemergten Datensätzen, weil sonst Fehler:
Dropout$study <- NULL
#Ebenso redundante Datumsvariablen im Drop-out-Datensatz:
Dropout[,c("DATUM_V1","DATUM_V2","DATUM_V3","DATUM_V4","DATUM_V5","DATUM_V6","DATUM_V7")] <- NULL

#merge Anthro, Basis, Dropout, Pbt2, Pbt5, Sgrq, FEV
Anthro_wide <- reshape(Anthro, idvar="subject", timevar="visit", v.names=c("at_1_gew","at_2_gr"), direction="wide", sep="_")

BA <- merge(Basis, Anthro_wide, by="subject", all.x=TRUE)
BA$STUDY <- NULL
BA_FEV <- merge(BA, FEV, by="subject", all.x=TRUE)
BA_FEV_do <- merge(BA_FEV, Dropout, by="subject", all.x=TRUE)
cosy_compl <- BA_FEV_do
rm(Anthro, Anthro_wide, BA, Basis, FEV, BA_FEV, Dropout)

cosy_compl <- gdata::rename.vars(data=cosy_compl, from=c("FEV1_GLI_PERCENT_PRED_V1","FEV1_GLI_PERCENT_PRED_V2","FEV1_GLI_PERCENT_PRED_V3","FEV1_GLI_PERCENT_PRED_V4","FEV1_GLI_PERCENT_PRED_V5","FEV1_GLI_PERCENT_PRED_V6","FEV1_GLI_PERCENT_PRED_V7"), to=c("FEV1_GLI_V1","FEV1_GLI_V2","FEV1_GLI_V3","FEV1_GLI_V4","FEV1_GLI_V5","FEV1_GLI_V6","FEV1_GLI_V7"))
names(cosy_compl)

#Dupletten löschen:
cosy_compl <- cosy_compl[-which(cosy_compl$subject=="251-189-626" & cosy_compl$lauf_2==71),]
cosy_compl <- cosy_compl[-which(cosy_compl$subject=="738-056-394" & cosy_compl$lauf_2==234),]

#Dropout-Kodierung:
ev_dat <- haven::read_sas("S:\\AG_Holle\\PerMed-COPD\\3_Datenmanagement\\3_2_Projektanträge\\PA_Holle\\endvisite_all.sas7bdat")
class(ev_dat) <- "data.frame"

ev_dat$ev_1_grund_ende2 <- ev_dat$ev_1_grund_ende
ev_dat[ev_dat$ev_1_grund_ende==1,]$ev_1_grund_ende2 <- "not_reached"
ev_dat[ev_dat$ev_1_grund_ende==2,]$ev_1_grund_ende2 <- "died"
ev_dat[ev_dat$ev_1_grund_ende==3,]$ev_1_grund_ende2 <- "moved"
ev_dat[ev_dat$ev_1_grund_ende==4,]$ev_1_grund_ende2 <- "hospitalized"
ev_dat[ev_dat$ev_1_grund_ende==5,]$ev_1_grund_ende2 <- "nursing_care"
ev_dat[ev_dat$ev_1_grund_ende==6,]$ev_1_grund_ende2 <- "not_interested"
ev_dat[ev_dat$ev_1_grund_ende==7,]$ev_1_grund_ende2 <- "no_time"
ev_dat[ev_dat$ev_1_grund_ende==8,]$ev_1_grund_ende2 <- "too_much_effort"
ev_dat[ev_dat$ev_1_grund_ende==9,]$ev_1_grund_ende2 <- "too_ill"
ev_dat[ev_dat$ev_1_grund_ende==10,]$ev_1_grund_ende2 <- "unknown"
ev_dat[ev_dat$ev_1_grund_ende==11,]$ev_1_grund_ende2 <- "other"
table(ev_dat$ev_1_grund_ende2) #Sollten ... Leute mit "other" als Grund sein, die händisch zuzuordnen sind.

dropout_reasons <- readxl::read_xlsx("S:\\AG_Holle\\TN\\1_COSYCONET\\other_dropout_reasons\\all_people_with_other_dropout_reason_new_final.xlsx")
class(dropout_reasons) <- "data.frame"
dropout_reasons[,"ev_1a_grund_andere"] <- NULL #Lösche ev_1a_grund_andere. Letzteres, weil schon vorhanden in ev_dat.

dropout_reasons$X <- NULL
x <- intersect(ev_dat$subject, dropout_reasons$subject); length(x) #580
y <- merge(dropout_reasons, ev_dat, by="subject")
ev_dat2 <- merge(ev_dat, y[,c("subject","Kategorisierung_TN")], by="subject", all=TRUE)

cosy_compl <- merge(cosy_compl, ev_dat2[,c("subject","ev_1_grund_ende2","Kategorisierung_TN")], by="subject", all=TRUE)
cosy_compl[!is.na(cosy_compl$Kategorisierung_TN),]$ev_1_grund_ende2 <- cosy_compl[!is.na(cosy_compl$Kategorisierung_TN),]$Kategorisierung_TN

cosy_compl$dropout_reason_name <- NA
cosy_compl[!is.na(cosy_compl$ev_1_grund_ende2) & cosy_compl$ev_1_grund_ende2=="died",]$dropout_reason_name <- "died"
cosy_compl[!is.na(cosy_compl$ev_1_grund_ende2) & cosy_compl$ev_1_grund_ende2 %in% c("too_ill","hospitalized","nursing_care"),]$dropout_reason_name <- "too_ill"
cosy_compl[!is.na(cosy_compl$ev_1_grund_ende2) & cosy_compl$ev_1_grund_ende2 %in% c("moved","no_time","not_interested","not_reached","other","too_much_effort","unknown"),]$dropout_reason_name <- "not_reached_not_interested"
cosy_compl[!is.na(cosy_compl$ev_1_grund_ende2) & cosy_compl$ev_1_grund_ende2 %in% c("Zentrum_geschlossen"),]$dropout_reason_name <- "center_closed"
cosy_compl[!is.na(cosy_compl$ev_1_grund_ende2) & cosy_compl$ev_1_grund_ende2==12,]$dropout_reason_name <- "no_dropout"

#cosy_compl[!is.na(cosy_compl$dropout_reason) & cosy_compl$ev_1_grund_ende2=="no_COPD",]$dropout_reason_name <- "no_COPD" #Keine Leute mehr, da ausgeschlossen.
table(cosy_compl$dropout_reason_name, exclude=NULL)
cosy_compl[is.na(cosy_compl$dropout_reason_name),c("ev_1_grund_ende2")] #40 "no_COPD" -> ausschließen

#Variablen definieren, bevor ich cosy0 und cosy1 bilde:
cosy_compl$age_cat <- NA
cosy_compl[cosy_compl$AGE_V1<55,]$age_cat <- "<55"
cosy_compl[cosy_compl$AGE_V1>=55 & cosy_compl$AGE_V1<65,]$age_cat <- "55-64"
cosy_compl[cosy_compl$AGE_V1>=65 & cosy_compl$AGE_V1<75,]$age_cat <- "65-74"
cosy_compl[cosy_compl$AGE_V1>=75,]$age_cat <- "≥75"

cosy_compl$stage <- NA
cosy_compl[!is.na(cosy_compl$FEV1_GLI_V1) & cosy_compl$FEV1_GLI_V1>=80,]$stage <- 1
cosy_compl[!is.na(cosy_compl$FEV1_GLI_V1) & cosy_compl$FEV1_GLI_V1>=50 & cosy_compl$FEV1_GLI_V1<80,]$stage <- 2
cosy_compl[!is.na(cosy_compl$FEV1_GLI_V1) & cosy_compl$FEV1_GLI_V1>=30 & cosy_compl$FEV1_GLI_V1<50,]$stage <- 3
cosy_compl[!is.na(cosy_compl$FEV1_GLI_V1) & cosy_compl$FEV1_GLI_V1<30,]$stage <- 4

cosy_compl$smoking_v1 <- ifelse(cosy_compl$SMOKER_V1==2, "current", ifelse(cosy_compl$SMOKER_V1==1, "former", "never"))
cosy_compl$sex <- ifelse(cosy_compl$SEX==1, "male", "female")
cosy_compl$never_smo <- ifelse(cosy_compl$smoking_v1=="never", 1, 0)

#Dasselbe nochmal, aber mit eigener Gruppe für PRISm-Leute:
# cosy_compl$stage2 <- cosy_compl$stage
# cosy_compl[!is.na(cosy_compl$TIFF_V1) & cosy_compl$TIFF_V1>=0.7,]$stage2 <- "PRISm"

# cosy_compl$PRISm <- NA
# cosy_compl[!is.na(cosy_compl$TIFF_V1) & cosy_compl$TIFF_V1>=0.70,]$PRISm <- 1
# cosy_compl[!is.na(cosy_compl$TIFF_V1) & cosy_compl$TIFF_V1<0.70,]$PRISm <- 0

# #Und nun nochmal mit eigener Gruppe für Pre-COPD:
# cosy_compl$stage3 <- cosy_compl$stage #stage ist nur gemäß FEV1 zugeordnet.
# cosy_compl[!is.na(cosy_compl$TIFF_V1) & cosy_compl$TIFF_V1>=0.7 & cosy_compl$FEV1_GLI_V1<80,]$stage3 <- "PRISm"
# cosy_compl[!is.na(cosy_compl$TIFF_V1) & cosy_compl$TIFF_V1>=0.7 & cosy_compl$FEV1_GLI_V1>=80,]$stage3 <- "Pre-COPD"

cosy_compl <- cosy_compl[-which(is.na(cosy_compl$dropout_reason_name) & cosy_compl$ev_1_grund_ende2=="no_COPD"),]

cosy_compl <- merge(cosy_compl, tmp, by="subject", all.x=TRUE)

cosy_compl[,c("DATUM_V1","DATUM_V2","DATUM_V3","DATUM_V4","DATUM_V5","DATUM_V6","DATUM_V7")]
cosy_compl[!is.na(cosy_compl$DATUM_V1)]

cosy_compl[,c("date_died","ev_2_sterb_dat.x","ev_2_sterb_dat.y")]
nrow(cosy_compl[!is.na(cosy_compl$date_died) & is.na(cosy_compl$ev_2_sterb_dat.x),]) #keine
nrow(cosy_compl[!is.na(cosy_compl$date_died) & is.na(cosy_compl$ev_2_sterb_dat.y),]) #keine
nrow(cosy_compl[!is.na(cosy_compl$ev_2_sterb_dat.x) & is.na(cosy_compl$ev_2_sterb_dat.y),]) #keine
#D.h. ev_2_sterb_dat.y hat die wenigsten Missings und die anderen beiden Variablen steuern nichts an Infos bei.
cosy_compl[,c("date_died","ev_2_sterb_dat.x")] <- NULL
cosy_compl$date_died <- cosy_compl$ev_2_sterb_dat.y
cosy_compl$ev_2_sterb_dat.y <- NULL
#Jetzt nur noch eine Variable "date_died" vorhanden mit allen (direkt) verfügbaren Sterbedaten.

nrow(cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died),]) #(Nur) 27 Leute, bei denen man Sterbedatum imputieren kann.

cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died),c("DATUM_V1","DATUM_V2","DATUM_V3","DATUM_V4","DATUM_V5","DATUM_V6","DATUM_V7")]

#1.) Wenn V1 vorhanden und V2 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V1 + 3 Monate.
cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V1) & is.na(cosy_compl$DATUM_V2),]$date_died <- cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V1) & is.na(cosy_compl$DATUM_V2),]$DATUM_V1 + 90

#2.) Wenn V2 vorhanden und V3 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V2 + 6 Monate.
cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V2) & is.na(cosy_compl$DATUM_V3),]$date_died <- cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V2) & is.na(cosy_compl$DATUM_V3),]$DATUM_V2 + 180

#3.) Wenn V3 vorhanden und V4 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V3 + 9 Monate.
cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V3) & is.na(cosy_compl$DATUM_V4),]$date_died <- cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V3) & is.na(cosy_compl$DATUM_V4),]$DATUM_V3 + 270

#4.) Wenn V4 vorhanden und V5 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V4 + 9 Monate.
cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5),]$date_died <- cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5),]$DATUM_V4 + 270

#5.) Wenn V5 vorhanden und V6 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V5 + 9 Monate.
cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6),]$date_died <- cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6),]$DATUM_V5 + 270

#6.) Wenn V6 vorhanden und V7 fehlt und Sterbedatum auch: angenommenes Sterbedatum = DATUM_V6 + 9 Monate.
cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$date_died <- cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$DATUM_V6 + 270

#7.) Wenn V7 vorhanden und Sterbedatum fehlt: angenommenes Sterbedatum = DATUM_V7 + 9 Monate.
cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V7),]$date_died <- cosy_compl[cosy_compl$dropout_reason_name=="died" & is.na(cosy_compl$date_died) & !is.na(cosy_compl$DATUM_V7),]$DATUM_V7 + 270

sum(!is.na(cosy_compl$date_died)) #345. Der ganze Aufwand hat die Zahl der verfügbaren Sterbedaten also von 318 auf 345 erhöht bzw. um 27 Werte.

cosy_compl$first_date <- cosy_compl$DATUM_V1
cosy_compl$status #if death was observed: status = 1; if survival time was censored: status = 0.
cosy_compl$status <- ifelse(!is.na(cosy_compl$date_died), 1, 0)

cosy_compl$last_date <- NA #Letztes Datum soll sein: Entweder Sterbedatum, falls vorhanden, oder drout_datum, falls vorhanden, oder Mittelwert zw. letzter vorhandener Visite und nächster fehlender Visite.

#Sterbedatum, falls vorhanden:
cosy_compl[!is.na(cosy_compl$date_died),]$last_date <- cosy_compl[!is.na(cosy_compl$date_died),]$date_died

#drout_datum, falls vorhanden:
cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$drout_datum),]$last_date <- cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$drout_datum),]$drout_datum

#1.) Wenn V1 vorhanden und V2 ff. fehlt: angenommenes Zensierungsdatum = DATUM_V1
cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V1) & is.na(cosy_compl$DATUM_V2) & is.na(cosy_compl$DATUM_V3) & is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$last_date <- cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V1) & is.na(cosy_compl$DATUM_V2) & is.na(cosy_compl$DATUM_V3) & is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$DATUM_V1

#2.) Wenn V2 vorhanden und V3 ff. fehlt: angenommenes Zensierungsdatum = DATUM_V2
cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V2) & is.na(cosy_compl$DATUM_V3) & is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$last_date <- cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V2) & is.na(cosy_compl$DATUM_V3) & is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$DATUM_V2

#3.) Wenn V3 vorhanden und V4 ff. fehlt: angenommenes Zensierungsdatum = DATUM_V3
cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V3) & is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$last_date <- cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V3) & is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$DATUM_V3

#4.) Wenn V4 vorhanden und V5 fehlt: angenommenes Zensierungsdatum = DATUM_V4
cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$last_date <- cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V4) & is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$DATUM_V4

#5.) Wenn V5 vorhanden und V6 fehlt: angenommenes Zensierungsdatum = DATUM_V5
cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$last_date <- cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V5) & is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$DATUM_V5

#6.) Wenn V6 vorhanden und V7 fehlt: angenommenes Zensierungsdatum = DATUM_V6
cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$last_date <- cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V6) & is.na(cosy_compl$DATUM_V7),]$DATUM_V6

#7.) Wenn V7 vorhanden: angenommenes Zensierungsdatum = DATUM_V7
cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V7),]$last_date <- cosy_compl[is.na(cosy_compl$last_date) & !is.na(cosy_compl$DATUM_V7),]$DATUM_V7

cosy_compl$last_date #Enthält jezt für alle Leute einen Wert.
cosy_compl$time <- as.Date(cosy_compl$last_date) - cosy_compl$first_date

cosy_compl[cosy_compl$time==0,c("DATUM_V1","DATUM_V2","DATUM_V3","DATUM_V4","DATUM_V5","DATUM_V6","DATUM_V7")] #ok
cosy_compl$last_date #Enthält jezt für alle Leute einen Wert.
cosy_compl$sex <- ifelse(cosy_compl$SEX==1, "male", "female")

#Beschränken auf relevante Variablen:
cosy <- cosy_compl[,c("subject","time","status","AGE_V1","stage","sex","smoking_v1","TIFF_V1")]

cosy$status_backup <- cosy$status

#Datensatz mit nur den relevanten Variablen exportieren:
write.csv2(cosy, "cosy_mortality.csv")
#Und wieder einlesen (zum Beweis, dass es damit durchläuft):
cosy <- read.csv2("cosy_mortality.csv")

library(flexsurv)
library(survival)
library("survminer")

cosy$time0 <- cosy$time+1 #Das Modell mag keine Zeitpunkte von 0.
cosy$time1 <- cosy$time0 / 365.25 #time1 = "originale" Zeit-Variable +1, aber in Jahre umgerechnet.
cosy$time2 <- cosy$time-365 #time2 = 1. Jahr weglassen. Aber bis Ende von V7 analysieren (also 12-90 Monate).
cosy[cosy$time2 <0,]$time2 <- NA
cosy$time2 <- cosy$time2 / 365.25 #time2 in Jahre umgerechnet.
#x-Achse nur bis 7.5 Jahre gehen lassen (Bzw. hier 6.5 Jahre, weil ich ja schon 1 Jahr abgezogen habe.):
cosy$time3 <- cosy$time2
cosy[!is.na(cosy$time3) & cosy$time3>6.5,]$time3 <- 6.5 #time3 = von Jahr 1 bis 6.5.
cosy[!is.na(cosy$status) & cosy$status==1 & !is.na(cosy$time3) & cosy$time3==6.5,]$status <- 0 #Zensieren nach 7.5 Jahren.


cosy$age_group2 <- NA
cosy[cosy$AGE_V1<55,]$age_group2 <- "40-54"
cosy[cosy$AGE_V1>=55 & cosy$AGE_V1<60,]$age_group2 <- "55-59"
cosy[cosy$AGE_V1>=60 & cosy$AGE_V1<65,]$age_group2 <- "60-64"
cosy[cosy$AGE_V1>=65 & cosy$AGE_V1<70,]$age_group2 <- "65-69"
cosy[cosy$AGE_V1>=70 & cosy$AGE_V1<75,]$age_group2 <- "70-74"
cosy[cosy$AGE_V1>=75 & cosy$AGE_V1<80,]$age_group2 <- "75-79"
cosy[cosy$AGE_V1>=80,]$age_group2 <- "80+"
cosy$age_group2 <- relevel(factor(cosy$age_group2), ref="65-69")

#Jetzt erst mal K-M-Plots:
surv_obj <- survfit(Surv(time = time2, event = status) ~ stage, data=cosy)
(kmsurvplot <- ggsurvplot(surv_obj, palette="jco")) #sieht ok aus.
ggsurvplot(survfit(Surv(time = time2, event = status_backup) ~ stage, data=cosy), palette="jco") #Mit originaler Statusvariable.

# Table 1:
cosy$age_group3 <- NA
cosy[cosy$AGE_V1<55,]$age_group3 <- "40-54"
cosy[cosy$AGE_V1>=55 & cosy$AGE_V1<65,]$age_group3 <- "55-64"
cosy[cosy$AGE_V1>=65 & cosy$AGE_V1<75,]$age_group3 <- "65-74"
cosy[cosy$AGE_V1>=75,]$age_group3 <- "75+"
cosy$age_group3 <- relevel(factor(cosy$age_group3), ref="65-74")

do.call(cbind, tapply(cosy$age_group3, cosy$stage, table))
round(100*prop.table(do.call(cbind, tapply(cosy$age_group3, cosy$stage, table)),2),1)
do.call(cbind, tapply(cosy$sex, cosy$stage, table))
round(100*prop.table(do.call(cbind, tapply(cosy$sex, cosy$stage, table)),2),1)
do.call(cbind, tapply(cosy$smoking_v1, cosy$stage, table))
round(100*prop.table(do.call(cbind, tapply(cosy$smoking_v1, cosy$stage, table)),2),1)

#Table 2: Cox-Modell: (Hauptanalysen mit time3, d.h. ohne 1. + letztes Jahr)
summary(coxph(Surv(time3, status) ~ as.factor(stage) + age_group2 + sex, data=cosy))
round(summary(coxph(Surv(time3, status) ~ as.factor(stage) + age_group2 + sex, data=cosy))$coef, 3)

#SA: mit time1 (d.h. no exclusions)
summary(coxph(Surv(time1, status) ~ as.factor(stage) + age_group2 + sex, data=cosy))
round(summary(coxph(Surv(time1, status) ~ as.factor(stage) + age_group2 + sex, data=cosy))$coef, 3)


#Table 2: Exp.-Modell: (Hauptanalysen mit time3, d.h. ohne 1. + letztes Jahr)
set.seed(1)
(exp_model <- flexsurvreg(Surv(time3, status) ~ as.factor(stage) + sex + age_group2, data = cosy, dist = "exp"))
#-> Schätzer sind halbwegs ähnlich, was sie auch sein sollten.

#p-Werte:
estimates <- exp_model$res[,"est"]
se <- exp_model$res[,"se"]
z_values <- estimates / se
(p_values <- 2 * pnorm(-abs(z_values)))

#SA: mit time1 (d.h. no exclusions)
(exp_model_SA <- flexsurvreg(Surv(time1, status) ~ as.factor(stage) + sex + age_group2, data = cosy, dist = "exp"))
(exp_model_SA_no_sex <- flexsurvreg(Surv(time1, status) ~ as.factor(stage) + age_group2, data = cosy, dist = "exp"))

#SA: Excluding participants with Tiffeneau index >= 0.7:
summary(coxph(Surv(time3, status) ~ as.factor(stage) + age_group2 + sex, data=cosy[!is.na(cosy$TIFF_V1) & cosy$TIFF_V1<0.7,]))
round(summary(coxph(Surv(time3, status) ~ as.factor(stage) + age_group2 + sex, data=cosy[!is.na(cosy$TIFF_V1) & cosy$TIFF_V1<0.7,]))$coef, 3)

(exp_model_SA3 <- flexsurvreg(Surv(time3, status) ~ as.factor(stage) + sex + age_group2, data = cosy[!is.na(cosy$TIFF_V1) & cosy$TIFF_V1<0.7,], dist = "exp"))


#p-Werte:
estimates <- exp_model_SA$res[,"est"]
se <- exp_model_SA$res[,"se"]
z_values_SA <- estimates / se
(p_values_SA <- 2 * pnorm(-abs(z_values_SA)))


#Table 3: Schätzer (1-Jahres-Mort.) je nach Alter, Geschlecht und Stadium (erst mal ohne KIs):

age_groups <- c("40-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
stages <- c(1, 2, 3, 4)
sexes <- c("male", "female")
combinations <- expand.grid(age_group2 = age_groups, stage = stages, sex = sexes)
combinations$surv_prob <- predict(exp_model, newdata = combinations, type = "survival", time = 1)
combinations$mortality_rate <- 1 - combinations$surv_prob
combinations$mortality_rate2 <- combinations$mortality_rate[2]
res <- combinations[,c("age_group2","sex","stage","mortality_rate2")]
res[,4] <- round(res[,4], 5)
res_exp_male <- cbind(res[1:7,], res[8:14,], res[15:21,], res[22:28,])
res_exp_male[,c(5,6,9,10,13,14)] <- NULL
res_exp_male
res_exp_female <- cbind(res[29:35,], res[36:42,], res[43:49,], res[50:56,])
res_exp_female[,c(5,6,9,10,13,14)] <- NULL
res_exp_female

#Konfidenzintervalle dafür lt. Llama-3.3:
age_groups <- c("40-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
stages <- c(1, 2, 3, 4)
sexes <- c("male", "female")
combinations <- expand.grid(age_group2 = age_groups, stage = stages, sex = sexes)
surv_prob <- predict(exp_model, newdata = combinations, type = "survival", time = 1)
class(surv_prob) <- "data.frame"
combinations$surv_prob <- surv_prob$.pred_survival

combinations$mortality_rate <- 1 - combinations$surv_prob
res <- combinations[,c("age_group2","sex","stage","mortality_rate")]

surv_prob_se <- predict(exp_model, newdata = combinations, type = "survival", time = 1, se = TRUE)

combinations$se_surv_prob <- surv_prob_se$.std_error
combinations$lower_ci <- 1 - (combinations$surv_prob + 1.96 * combinations$se_surv_prob)
combinations$upper_ci <- 1 - (combinations$surv_prob - 1.96 * combinations$se_surv_prob)
res <- combinations[, c("age_group2", "sex", "stage", "mortality_rate", "lower_ci", "upper_ci")]
#Sieht plausibel aus.
res[,4:6] <- round(res[,4:6], 5)
paste0(res$mortality_rate, " (", res$lower_ci, "-", res$upper_ci, ")")
res2 <- cbind(res, paste0(res$mortality_rate, " (", res$lower_ci, "-", res$upper_ci, ")"))
colnames(res2)[7] <- "res"
res2

#Und noch 1x dasselbe, aber nicht nach Geschlecht stratifiziert:
(exp_model_test <- flexsurvreg(Surv(time2, status) ~ as.factor(stage) + age_group2, data = cosy, dist = "exp"))
combinations <- expand.grid(age_group2 = age_groups, stage = stages)
combinations$surv_prob <- predict(exp_model_test, newdata = combinations, type = "survival", time = 1)
combinations$mortality_rate <- 1 - combinations$surv_prob
combinations$mortality_rate2 <- combinations$mortality_rate[2]
res <- combinations[,c("age_group2","stage","mortality_rate2")]
res[,3] <- round(res[,3], 5)
res_exp <- cbind(res[1:7,], res[8:14,], res[15:21,], res[22:28,])
res_exp[,c(2,4,5,7,8,10,11)] <- NULL
colnames(res_exp) <- c("age_group", paste("stage", 1:4))
res_exp


#Konfidenzintervalle dafür lt. Llama-3.3:
age_groups <- c("40-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
stages <- c(1, 2, 3, 4)
combinations <- expand.grid(age_group2 = age_groups, stage = stages)
surv_prob <- predict(exp_model_test, newdata = combinations, type = "survival", time = 1)
class(surv_prob) <- "data.frame"
combinations$surv_prob <- surv_prob$.pred_survival

combinations$mortality_rate <- 1 - combinations$surv_prob
res <- combinations[,c("age_group2","stage","mortality_rate")]

surv_prob_se <- predict(exp_model_test, newdata = combinations, type = "survival", time = 1, se = TRUE)

combinations$se_surv_prob <- surv_prob_se$.std_error
combinations$lower_ci <- 1 - (combinations$surv_prob + 1.96 * combinations$se_surv_prob)
combinations$upper_ci <- 1 - (combinations$surv_prob - 1.96 * combinations$se_surv_prob)
res <- combinations[, c("age_group2", "stage", "mortality_rate", "lower_ci", "upper_ci")]
#Sieht plausibel aus.
res[,3:5] <- round(res[,3:5], 5)
res2 <- data.frame(paste0(res$mortality_rate, " (", res$lower_ci, "-", res$upper_ci, ")"))

#SA (Supplementary Table 1): mit time1 (d.h. no exclusions), nicht nach Geschlecht stratifiziert.
combinations$surv_prob <- predict(exp_model_SA_no_sex, newdata = combinations, type = "survival", time = 1)
combinations$mortality_rate <- 1 - combinations$surv_prob
combinations$mortality_rate2 <- combinations$mortality_rate[2]
res <- combinations[,c("age_group2","stage","mortality_rate2")]
res[,3] <- round(res[,3], 5)
res_exp_SA_no_sex <- cbind(res[1:7,], res[8:14,], res[15:21,], res[22:28,])
res_exp_SA_no_sex[,c(2,4,5,7,8,10,11)] <- NULL
colnames(res_exp_SA_no_sex) <- c("age_group", paste("stage", 1:4))


#Supplementary Table 2: 
tmp <- cosy[!is.na(cosy$TIFF_V1) & cosy$TIFF_V1 <=0.7,]
flexsurvreg(Surv(time3, status) ~ as.factor(stage) + age_group2, data=tmp, dist="exp")

(exp_model_tmp <- flexsurvreg(Surv(time3, status) ~ as.factor(stage) + age_group2, data=tmp, dist="exp"))
combinations <- expand.grid(age_group2 = age_groups, stage = stages)
combinations$surv_prob <- predict(exp_model_tmp, newdata = combinations, type = "survival", time = 1)
combinations$mortality_rate <- 1 - combinations$surv_prob
combinations$mortality_rate2 <- combinations$mortality_rate[2]
res <- combinations[,c("age_group2","stage","mortality_rate2")]
res[,3] <- round(res[,3], 5)
res_exp <- cbind(res[1:7,], res[8:14,], res[15:21,], res[22:28,])
res_exp[,c(2,4,5,7,8,10,11)] <- NULL
colnames(res_exp) <- c("age_group", paste("stage", 1:4))
res_exp


#Konfidenzintervalle dafür:
age_groups <- c("40-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
stages <- c(1, 2, 3, 4)
combinations <- expand.grid(age_group2 = age_groups, stage = stages)
surv_prob <- predict(exp_model_tmp, newdata = combinations, type = "survival", time = 1)
class(surv_prob) <- "data.frame"
combinations$surv_prob <- surv_prob$.pred_survival

combinations$mortality_rate <- 1 - combinations$surv_prob
res <- combinations[,c("age_group2","stage","mortality_rate")]

surv_prob_se <- predict(exp_model_tmp, newdata = combinations, type = "survival", time = 1, se = TRUE)

combinations$se_surv_prob <- surv_prob_se$.std_error
combinations$lower_ci <- 1 - (combinations$surv_prob + 1.96 * combinations$se_surv_prob)
combinations$upper_ci <- 1 - (combinations$surv_prob - 1.96 * combinations$se_surv_prob)
res <- combinations[, c("age_group2", "stage", "mortality_rate", "lower_ci", "upper_ci")]
#Man braucht also nicht mal die Simulationen, wie es ChatGPT vorgeschlagen hat. Sieht so auch ganz plausibel aus.
res[,3:5] <- round(res[,3:5], 5)
res2 <- data.frame(paste0(res$mortality_rate, " (", res$lower_ci, "-", res$upper_ci, ")"))



#Figure 2: Kaplan-Meier-Plot.
surv_obj <- survfit(Surv(time = time1, event = status) ~ stage, data=cosy)
(kmsurvplot1 <- ggsurvplot(surv_obj, palette="jco")) #sieht ok aus.
dev.off()

#1. Jahr weglassen und bei 7.5 (bzw. im Plot 6.5) Jahren abschneiden:
surv_obj <- survfit(Surv(time = time3, event = status) ~ stage, data=cosy)
(kmsurvplot2 <- ggsurvplot(surv_obj, palette="jco")) #sieht ok aus.
dev.off()

#Figure 3: Parametric survival curves, stratified by GOLD stages 1-4, excluding first year and censoring >7.5 years of follow-up.
tmp <- cosy
tmp[!is.na(tmp$status) & tmp$status==1 & !is.na(tmp$time3) & tmp$time3>=6.5,]$status <- 0
tmp[!is.na(tmp$time3) & tmp$time3>=6.5,]$time3 <- 6.5
plot(flexsurvreg(formula = Surv(time3, status) ~ as.factor(stage), data = tmp, dist="exp"))

#Supplementary Figure 1: Parametric survival curves, stratified by GOLD stages 1-4 (assigning the Pre-COPD and PRISm group to GOLD 1-4 according to FEV1), including first and last year of follow-up.
tmp <- cosy
plot(flexsurvreg(formula = Surv(time1, status) ~ as.factor(stage), data = tmp, dist="exp"))


####################################
# Weitere (Sensitivitäts-)Analysen (Supplementary Table 3):

# Mortalität je nach Stadium und Altersgruppe. Gesamten Zeitraum einschließen. Aber alle mit Tiff. >0.7 ausschließen. Außerdem nicht in Pre-COPD und PRISm aufdröseln + nicht groß diskutieren.

set.seed(1)
tmp <- cosy[!is.na(cosy$TIFF_V1) & cosy$TIFF_V1 <0.7,]
(exp_model_SA2 <- flexsurvreg(Surv(time3, status) ~ as.factor(stage) + sex + age_group2, data = tmp, dist = "exp"))

age_groups <- c("40-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
stages <- c(1, 2, 3, 4)
combinations <- expand.grid(age_group2 = age_groups, stage = stages, sex = sexes)
combinations$surv_prob <- predict(exp_model_SA2, newdata = combinations, type = "survival", time = 1)
combinations$mortality_rate <- 1 - combinations$surv_prob
combinations$mortality_rate2 <- combinations$mortality_rate[2]
res <- combinations[,c("age_group2","sex","stage","mortality_rate2")]
res[,4] <- round(res[,4], 6)
