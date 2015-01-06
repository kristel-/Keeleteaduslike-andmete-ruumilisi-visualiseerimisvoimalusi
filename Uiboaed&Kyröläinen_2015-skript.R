# Kui kasutate ja muudate selle töö tulemusel valminud skripti ja faile, palume neile viidata järgnevalt
# Uiboaed, Kristel, Aki-Juhani Kyröläinen 2015. "Keeleteaduslike andmete ruumilisi visualiseerimisvõimalusi". Eesti Rakenduslingvistika Ühingu aastaraamat XI, lk x-x.

# loe sisse kaardiandmestik
koordinaadid <- read.csv("murdealadeKoordinaadid.csv", header=T, sep=";")

# koordinaatandmestiku esitamine
# argument "type" määratleb esitatavad andmed joonena (l=line)
# argumendid "ylab", "xlab", "xaxt" ja "yaxt" jäävad ilma tekstita, st et telgede selgitavat teksti graafikul ei kuvata
# kaartide salvestamine tiff-formaadis, graafiku mõõdud on siin esitatud sentimeetrites
# argument "compression" vähendab faili suurust
# argument "res" (resolution) määrab graafiku resolutsiooni (300 või 600 on sobiv trükiformaadi esitamiseks)
# paketis gridExtra on funktioon grid.arrange(), mille abil on võimalik esitada mitut pilti samal graafikul
# salvestame alguses pildid ja seejärel esitame need grid.arrange-funktsiooni abil
# argument "ncol" määrab veerbude arvu, antud juhul on see 2, sest esitame kaks pilti kõrvuti ühes reas kahe veeruna
# funktsioon dev.off() suleb tiff-käsu

tiff(filename="koord.tiff", width = 10, height = 10, units = "cm", compression = "lzw", res = 600)
plot(koordinaadid$lon, koordinaadid$lat, type="l", ylab="", xlab="", xaxt="n", yaxt="n")
dev.off()

## võta R-i sessioonis kasutusele paketid ggmap, ggplot2 ja gridExtra
library(ggplot2)
library(ggmap)
library(gridExtra)

# loe sisse värviline kaart, mis vaikimisi kasutab Google'i kaarti värviliselt ja mustvalgelt
# argument "zoom" määrab kaardi suuruse
qmap("Eesti", zoom=7)
qmap("Eesti", zoom=7, color="bw")

tiff(filename="mustvalgeVarviline.tiff", width = 10, height = 6, units = "cm", compression = "lzw", res = 600)
eestiVarviline <- qmap("Eesti", zoom=7)
eestiMustValge <- qmap("Eesti", zoom=7, color="bw")
grid.arrange(eestiVarviline, eestiMustValge, ncol=2)
dev.off()

# loeme sisse verbiühendite sagedusandmestiku (fail tudSaama.csv)
tudSaama <- read.csv("tudSaama.csv", header=T, sep=";")

# ja seejärel illustreerime neid sagedusi nn üleminekukaardil (gradience map) ja salvestame tulemuse tiff-fromaadis faili
tiff(filename="tud-saama.tiff", width = 10, height = 10, units = "cm", compression = "lzw", res = 600)
plot(lat~lon, type="n", data=koordinaadid, axes=F, ylab="", xlab="")
for (i in 1: length(unique(koordinaadid$murre))) {
          temp=koordinaadid$murre==unique(koordinaadid$murre)[i]
          polygon(x=koordinaadid$lon[temp], y=koordinaadid$lat[temp],
                  col=gray.colors(max(tudSaama$tudSaama))[max(tudSaama$tudSaama)-tudSaama$tudSaama[tudSaama$murre==unique(koordinaadid$murre)[i]]+1]
          )
}
box()
text(x=min(koordinaadid$lon,na.rm=T), y=max(koordinaadid$lat,na.rm=T)-0.1,labels="tud + saama",pos=4, cex=1)
dev.off()

# loeme sisse verbiühendite sagedusandmestiku (fail tudKonstr.csv)
tudKonstr <- read.csv("tudKonstr.csv", header=T, sep=";")

# kaartide salvestamine tiff-formaadis, graafiku mõõdud on siin esitatud sentimeetrites
# argument "compression" vähendab faili suurust
# argument "res" (resolution) määrab graafiku resolutsiooni (300 või 600 on sobiv trükiformaadi esitamiseks)
# parameetrit "par" kasutame kahe pildi esitamiseks samal graafikul ("mfrow"), kui kasutame graafiku tegemiseks plot-funktsiooni, praegu kasutamegi plot-funktsiooni, mitte ggploti-paketi võimalus, seega ei saa siin kasutada grid.arrange-funktsiooni
# argument "mar" määrab graafiku veerised
# funktsioon dev.off() suleb tiff-käsu
tiff(filename="konstruktsioonid.tiff", width = 6, height = 10, units = "cm", compression = "lzw", res = 300)
par(mar=c(1.1, 1.1, 1.1, 1.1), mfrow = c (2, 1))
plot(lat~lon, type="n", data=koordinaadid, axes=F, ylab="", xlab="")
for (i in 1: length(unique(koordinaadid$murre))) {
          temp=koordinaadid$murre==unique(koordinaadid$murre)[i]
          polygon(x=koordinaadid$lon[temp], y=koordinaadid$lat[temp],
                  col=gray.colors(max(tudKonstr$imp))[max(tudKonstr$imp)-tudKonstr$imp[tudKonstr$murre==unique(koordinaadid$murre)[i]]+1]
          )
}
box()
text(x=min(koordinaadid$lon,na.rm=T), y=max(koordinaadid$lat,na.rm=T)-0.1,labels="impersonaal", pos=4, cex=1)
plot(lat~lon, type="n", data=koordinaadid, axes=F, ylab="", xlab="")
for (i in 1: length(unique(koordinaadid$murre))) {
          temp=koordinaadid$murre==unique(koordinaadid$murre)[i]
          polygon(x=koordinaadid$lon[temp], y=koordinaadid$lat[temp],
                  col=gray.colors(max(tudKonstr$pass))[max(tudKonstr$pass)-tudKonstr$pass[tudKonstr$murre==unique(koordinaadid$murre)[i]]+1]
          )
}
box()
text(x=min(koordinaadid$lon,na.rm=T), y=max(koordinaadid$lat,na.rm=T)-0.1,labels="passiiv", pos=4, cex=1)
dev.off()

# loeme sisse analüütiliste ja sünteetiliste konstruktsioonide andmestikud (failid vastavalt kvotAn.csv ja kvotSyn.csv), andmestikud esitavad külad ja nende koordinaadid, kust vastavalt kas analüütilsi või sünteetilisi konstruktsioone on tuvastatud
kvotAn <- read.csv("kvotAn.csv", header=T, sep=";")
kvotSyn <- read.csv("kvotSyn.csv", header=T, sep=";")

# põhjana kasutame eelnevalt esitatud Google'i kaarte, kuid nüüd salvestame need kaardid eelnevalt eraldi objektiks nimega "eesti", et neid oleks hiljem võimalik kasutada
# siin anname ette kaardi keskpunkti koordinaatidega (argumendid lat ja lon), mitte kohanimega
# argument "zoom" määrab kaardi suuruse
eesti <- get_map(c(lon = 25.01361, lat = 58.59527), zoom=7, color="bw")

# eelneva sammuga salvestame Eesti kaardi (objekt eesti), millele kanname andmestiku külade kohta, kust vaadeldavaid vorme tuvastasime      
# funktsioon geom_point() tähistab külad punktidena
# argument "size" määrab selle punkti suuruse
eesti_pohi <- ggmap(eesti)
eesti_pohi + geom_point(data = kvotAn, aes(x = lon, y = lat), size=1)
eesti_pohi + geom_point(data = kvotSyn, aes(x = lon, y = lat), size=1)

# nüüd loome samad graafikud, kuid salvestame need ka eraldi tiff-formaadis faili
# lisaks defineerime, et telgedel lisainfot ei esitata (funktsioonid "labs" ja "theme")
tiff(filename="kvotatiivid.tiff", width = 10, height = 6, units = "cm", compression = "lzw", res = 600, point = 12)
analyytilised <- eesti_pohi + geom_point(data = kvotAn, aes(x = lon, y = lat), size=.4, fill="black") + labs(x = "", y = "") + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) 
synteetilised <- eesti_pohi + geom_point(data = kvotSyn, aes(x = lon, y = lat), size=.4, shape=24, fill="black") + labs(x = "", y = "") + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
grid.arrange(analyytilised, synteetilised, ncol=2)
dev.off()

# loeme sisse vadja keele andmestiku, mis sisaldab külade koordinaate ja sõnade arvu, mis on kogutud murdekorpuse vadja keele alamkorpusest (fail vadja.csv)
# vadja keele alamkorpuse sagedusandmestik on failis vadja.csv, mille loeme järgnev käsuga R-i
vad <- read.csv("vadja.csv", header=T, sep=";", dec=",")

# esitame külad punktidena, kust andmeid on kogutud
# salvestame andmed taas tiff-formaadis faili
tiff(filename="vadja.tiff", width = 10, height = 10, units = "cm", compression = "lzw", res = 600, point=12)
vadja <- get_map("Habolovo", zoom=9, color="bw")
vadja_pohi <- ggmap(vadja)
vadja_pohi + geom_point(data = vad, aes(x = lon, y = lat), size=1) + labs(x = "", y = "") + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
dev.off()

# lisaks eelnevale kasutame sõnade arvu, et määrata punkti suurust
# salvestame tulemuse tiff-faili
tiff(filename="vadjaSonad.tiff", width = 10, height = 10, units = "cm", compression = "lzw", res = 300)
vadja <- get_map("Habolovo", zoom=9, color="bw")
vadja_pohi <- ggmap(vadja)
vadja_pohi + geom_point(data = vad, aes(x = lon, y = lat, size=sonu)) +
          scale_size_continuous(range = c(1, 4), name="Sõnade arv") +
  labs(x = "", y = "") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
dev.off()

# kasutame eelnevalt sisseloetud andmestikku failist tudSaama.csv
# klasteranalüüsi jaoks määrame, et ridade nimed on murrete nimed (row.names)
klaster <- read.csv("tudSaama.csv", header=T, sep=";", row.names=1)

# arvuta kaugusmaatriks, vajalik klasteranalüüsi sisendiks
# seejärel teeme klasteranalüüsi
andmed.kaugus <- dist(klaster, method="euclidean", upper=TRUE, diag=TRUE)
klasteranalyys <- hclust(andmed.kaugus, method="ward.D")

# estiame klasteranalüüsi tulemuse dendrogrammina
tiff(filename="klaster.tiff", width = 10, height = 10, units = "cm", compression = "lzw", res = 600, point=12)
plot(klasteranalyys, main = " ", sub = NULL, ann = F, axes = F)
rect.hclust(klasteranalyys, 2, border=8)
dev.off()

# klasteranalüüsi tulemus esitas kaks teineteisest selgemalt eristuvat rühma
# need rühmad on määratud andmestikus, mis on failis klasterTulemus.csv
klusterVis <- read.csv("klasterTulemus.csv", header=T, sep=";")

# visualiseerime klasteranalüüsi tulemused kaardil
# argument "fill" määrab, et muutuja murre täidab polügooni
# muutuja klaster on muudetud kategoriaalseks muutujaks (as.factor)
# argument "alpha" määrab polügooni läbipaistvuse taseme
tiff(filename="klasterKaart.tiff", width = 16, height = 12, units = "cm", compression = "lzw", res = 600)
qmap("Eesti", zoom=7, color="bw") +
          geom_polygon(aes(x = lon, y = lat, group = murre, fill = as.factor(klaster), alpha = 1/2), data=klusterVis) +
          scale_fill_grey(name = "Rühm") +
          theme_bw() +
          scale_alpha(guide = F) + labs(x="", y="") + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
dev.off()