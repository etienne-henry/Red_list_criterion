# Script defining functions to address taxonomic mismatches between bird taxonomies. 

# Author: Etienne Henry
# Date: 10/2022

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)

# Taxonomic crosswalk from AVONET (Tobias et al. 2022) to Birdlife taxonomy -------------------------------------------------------- 

crosswalk_avonet_BL <- function(avo.traits ){
  
  avo.traits$sps[avo.traits$sps=="Polihierax insignis"] <- "Neohierax insignis"
  avo.traits$sps[avo.traits$sps=="Arborophila charltonii"] <- "Tropicoperdix charltonii"
  avo.traits$sps[avo.traits$sps=="Arborophila chloropus"] <- "Tropicoperdix chloropus"
  avo.traits$sps[avo.traits$sps=="Arborophila graydoni"] <- "Tropicoperdix graydoni"
  avo.traits$sps[avo.traits$sps=="Arborophila tonkinensis"] <- "Tropicoperdix tonkinensis"
  avo.traits$sps[avo.traits$sps=="Antigone antigone"] <- "Grus antigone"
  avo.traits$sps[avo.traits$sps=="Antigone canadensis"] <- "Grus canadensis"
  avo.traits$sps[avo.traits$sps=="Antigone rubicunda"] <- "Grus rubicunda"
  avo.traits$sps[avo.traits$sps=="Antigone vipio"] <- "Grus vipio"
  avo.traits$sps[avo.traits$sps=="Calendulauda alopex"] <- "Calendulauda africanoides"
  avo.traits$sps[avo.traits$sps=="Mirafra ashi"] <- "Mirafra somalica"
  avo.traits$sps[avo.traits$sps=="Mirafra gilletti"] <- "Calendulauda gilletti"
  avo.traits$sps[avo.traits$sps=="Mirafra rufa"] <- "Calendulauda rufa"
  avo.traits$sps[avo.traits$sps=="Edolisoma parvulum"] <- "Coracina parvula"
  avo.traits$sps[avo.traits$sps=="Aphrastura masafucrae"] <- "Aphrastura masafuerae"
  avo.traits$sps[avo.traits$sps=="Corvinella corvina"] <- "Lanius corvinus"
  avo.traits$sps[avo.traits$sps=="Urolestes melanoleucus"] <- "Lanius melanoleucus"
  avo.traits$sps[avo.traits$sps=="Chatarrhaea gularis"] <- "Argya gularis"
  avo.traits$sps[avo.traits$sps=="Chatarrhaea longirostris"] <- "Argya longirostris"
  avo.traits$sps[avo.traits$sps=="Garrulax albogularis"] <- "Pterorhinus albogularis"
  avo.traits$sps[avo.traits$sps=="Garrulax berthemyi"] <- "Pterorhinus berthemyi"
  avo.traits$sps[avo.traits$sps=="Garrulax bieti"] <- "Ianthocincla bieti"
  avo.traits$sps[avo.traits$sps=="Garrulax caerulatus"] <- "Pterorhinus caerulatus"
  avo.traits$sps[avo.traits$sps=="Garrulax calvus"] <- "Melanocichla calva"
  avo.traits$sps[avo.traits$sps=="Garrulax chinensis"] <- "Pterorhinus chinensis"
  avo.traits$sps[avo.traits$sps=="Garrulax cineraceus"] <- "Ianthocincla cineracea"
  avo.traits$sps[avo.traits$sps=="Garrulax cinereiceps"] <- "Ianthocincla cinereiceps"
  avo.traits$sps[avo.traits$sps=="Garrulax cinereifrons"] <- "Argya cinereifrons"
  avo.traits$sps[avo.traits$sps=="Garrulax courtoisi"] <- NA #No range map, CR
  avo.traits$sps[avo.traits$sps=="Garrulax davidi"] <- "Pterorhinus davidi"
  avo.traits$sps[avo.traits$sps=="Garrulax delesserti"] <- "Pterorhinus delesserti"
  avo.traits$sps[avo.traits$sps=="Garrulax galbanus"] <- "Pterorhinus galbanus"
  avo.traits$sps[avo.traits$sps=="Garrulax gularis"] <- "Pterorhinus gularis"
  avo.traits$sps[avo.traits$sps=="Garrulax konkakinhensis"] <- "Ianthocincla konkakinhensis"
  avo.traits$sps[avo.traits$sps=="Garrulax koslowi"] <- "Pterorhinus koslowi"
  avo.traits$sps[avo.traits$sps=="Garrulax lanceolatus"] <- "Pterorhinus lanceolatus"
  avo.traits$sps[avo.traits$sps=="Garrulax lugubris"] <- "Melanocichla lugubris"
  avo.traits$sps[avo.traits$sps=="Garrulax lunulatus"] <- "Ianthocincla lunulata"
  avo.traits$sps[avo.traits$sps=="Garrulax maximus"] <- "Ianthocincla maxima"
  avo.traits$sps[avo.traits$sps=="Garrulax mitratus"] <- "Pterorhinus mitratus"
  avo.traits$sps[avo.traits$sps=="Garrulax monachus"] <- "Pterorhinus monachus"
  avo.traits$sps[avo.traits$sps=="Garrulax nuchalis"] <- "Pterorhinus nuchalis"
  avo.traits$sps[avo.traits$sps=="Garrulax ocellatus"] <- "Ianthocincla ocellata"
  avo.traits$sps[avo.traits$sps=="Garrulax pectoralis"] <- "Pterorhinus pectoralis"
  avo.traits$sps[avo.traits$sps=="Garrulax perspicillatus"] <- "Pterorhinus perspicillatus"
  avo.traits$sps[avo.traits$sps=="Garrulax poecilorhynchus"] <- "Pterorhinus poecilorhynchus"
  avo.traits$sps[avo.traits$sps=="Garrulax ruficeps"] <- "Pterorhinus ruficeps"
  avo.traits$sps[avo.traits$sps=="Garrulax ruficollis"] <- "Pterorhinus ruficollis"
  avo.traits$sps[avo.traits$sps=="Garrulax rufogularis"] <- "Ianthocincla rufogularis"
  avo.traits$sps[avo.traits$sps=="Garrulax sannio"] <- "Pterorhinus sannio"
  avo.traits$sps[avo.traits$sps=="Garrulax sukatschewi"] <- "Ianthocincla sukatschewi"
  avo.traits$sps[avo.traits$sps=="Garrulax treacheri"] <- "Pterorhinus treacheri"
  avo.traits$sps[avo.traits$sps=="Garrulax vassali"] <- "Pterorhinus vassali"
  avo.traits$sps[avo.traits$sps=="Garrulax waddelli"] <- "Pterorhinus waddelli"
  avo.traits$sps[avo.traits$sps=="Garrulax woodi"] <- "Pterorhinus woodi"
  avo.traits$sps[avo.traits$sps=="Turdoides affinis"] <- "Argya affinis"
  avo.traits$sps[avo.traits$sps=="Turdoides rufescens"] <- "Argya rufescens"
  avo.traits$sps[avo.traits$sps=="Turdoides somervillei"] <- "Argya somervillei"
  avo.traits$sps[avo.traits$sps=="Turdoides striata"] <- "Argya striata"
  avo.traits$sps[avo.traits$sps=="Amphilais seebohmi"] <- "Bradypterus seebohmi"
  avo.traits$sps[avo.traits$sps=="Bradypterus alfredi"] <- "Locustella alfredi"
  avo.traits$sps[avo.traits$sps=="Buettikoferella bivittata"] <- "Cincloramphus bivittatus"
  avo.traits$sps[avo.traits$sps=="Chaetornis striata"] <- "Schoenicola striatus"
  avo.traits$sps[avo.traits$sps=="Locustella amnicola"] <- "Helopsaltes amnicola"
  avo.traits$sps[avo.traits$sps=="Locustella certhiola"] <- "Helopsaltes certhiola"
  avo.traits$sps[avo.traits$sps=="Locustella fasciolata"] <- "Helopsaltes fasciolata"
  avo.traits$sps[avo.traits$sps=="Locustella ochotensis"] <- "Helopsaltes ochotensis"
  avo.traits$sps[avo.traits$sps=="Locustella pleskei"] <- "Helopsaltes pleskei"
  avo.traits$sps[avo.traits$sps=="Locustella portenta"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Locustella pryeri"] <- "Helopsaltes pryeri"
  avo.traits$sps[avo.traits$sps=="Schoenicola brevirostris"] <- "Catriscus brevirostris"
  avo.traits$sps[avo.traits$sps=="Melidectes fuscus"] <- "Melionyx fuscus"
  avo.traits$sps[avo.traits$sps=="Melidectes nouhuysi"] <- "Melionyx nouhuysi"
  avo.traits$sps[avo.traits$sps=="Melidectes princeps"] <- "Melionyx princeps"
  avo.traits$sps[avo.traits$sps=="Myzomela irianawidodoae"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Myzomela prawiradilagae"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Myzomela wahe"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Rimator albostriatus"] <- "Napothera albostriata"
  avo.traits$sps[avo.traits$sps=="Rimator danjoui"] <- "Napothera danjoui"
  avo.traits$sps[avo.traits$sps=="Rimator malacoptilus"] <- "Napothera malacoptila"
  avo.traits$sps[avo.traits$sps=="Rimator naungmungensis"] <- "Napothera naungmungensis"
  avo.traits$sps[avo.traits$sps=="Rimator pasquieri"] <- "Napothera pasquieri"
  avo.traits$sps[avo.traits$sps=="Trichastoma bicolor"] <- "Pellorneum bicolor"
  avo.traits$sps[avo.traits$sps=="Trichastoma buettikoferi"] <- "Pellorneum buettikoferi"
  avo.traits$sps[avo.traits$sps=="Trichastoma celebense"] <- "Pellorneum celebense"
  avo.traits$sps[avo.traits$sps=="Trichastoma cinereiceps"] <- "Pellorneum cinereiceps"
  avo.traits$sps[avo.traits$sps=="Trichastoma malaccense"] <- "Pellorneum malaccense"
  avo.traits$sps[avo.traits$sps=="Trichastoma pyrrogenys"] <- "Pellorneum pyrrogenys"
  avo.traits$sps[avo.traits$sps=="Trichastoma rostratum"] <- "Pellorneum rostratum"
  avo.traits$sps[avo.traits$sps=="Trichastoma tickelli"] <- "Pellorneum tickelli"
  avo.traits$sps[avo.traits$sps=="Turdinus brevicaudatus"] <- "Gypsophila brevicaudata"
  avo.traits$sps[avo.traits$sps=="Turdinus calcicola"] <- "Gypsophila calcicola"
  avo.traits$sps[avo.traits$sps=="Turdinus crassus"] <- "Gypsophila crassa"
  avo.traits$sps[avo.traits$sps=="Turdinus crispifrons"] <- "Gypsophila crispifrons"
  avo.traits$sps[avo.traits$sps=="Turdinus rufipectus"] <- "Gypsophila rufipecta"
  avo.traits$sps[avo.traits$sps=="Phylloscopus emilsalimi"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Phylloscopus suaramerdu"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Scytalopus frankeae"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Scytalopus krabbei"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Scytalopus whitneyi"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Rhipidura habibiei"] <- NA #New
  avo.traits$sps[avo.traits$sps=="Acridotheres tertius"] <- "Acridotheres melanopterus"
  avo.traits$sps[avo.traits$sps=="Acridotheres tricolor"] <- NA #Same as Acridotheres tertius
  avo.traits$sps[avo.traits$sps=="Chleuasicus atrosuperciliaris"] <- "Suthora atrosuperciliaris"
  avo.traits$sps[avo.traits$sps=="Cholornis paradoxus"] <- "Paradoxornis paradoxus"
  avo.traits$sps[avo.traits$sps=="Cholornis unicolor"] <- "Paradoxornis unicolor"
  avo.traits$sps[avo.traits$sps=="Conostoma aemodium"] <- "Paradoxornis aemodium"
  avo.traits$sps[avo.traits$sps=="Neosuthora davidiana"] <- "Suthora davidiana"
  avo.traits$sps[avo.traits$sps=="Parophasma galinieri"] <- "Sylvia galinieri"
  avo.traits$sps[avo.traits$sps=="Psittiparus bakeri"] <- "Paradoxornis bakeri"
  avo.traits$sps[avo.traits$sps=="Psittiparus gularis"] <- "Paradoxornis gularis"
  avo.traits$sps[avo.traits$sps=="Psittiparus margaritae"] <- "Paradoxornis margaritae"
  avo.traits$sps[avo.traits$sps=="Psittiparus ruficeps"] <- "Paradoxornis ruficeps"
  avo.traits$sps[avo.traits$sps=="Sinosuthora alphonsiana"] <- "Suthora alphonsiana"
  avo.traits$sps[avo.traits$sps=="Sinosuthora brunnea"] <- "Suthora brunnea"
  avo.traits$sps[avo.traits$sps=="Sinosuthora conspicillata"] <- "Suthora conspicillata"
  avo.traits$sps[avo.traits$sps=="Sinosuthora przewalskii"] <- "Suthora przewalskii"
  avo.traits$sps[avo.traits$sps=="Sinosuthora ricketti"] <- "Suthora ricketti"
  avo.traits$sps[avo.traits$sps=="Sinosuthora webbiana"] <- "Suthora webbiana"
  avo.traits$sps[avo.traits$sps=="Sinosuthora zappeyi"] <- "Suthora zappeyi"
  avo.traits$sps[avo.traits$sps=="Sylvia balearica"] <- "Curruca balearica"
  avo.traits$sps[avo.traits$sps=="Sylvia boehmi"] <- "Curruca boehmi"
  avo.traits$sps[avo.traits$sps=="Sylvia buryi"] <- "Curruca buryi"
  avo.traits$sps[avo.traits$sps=="Sylvia cantillans"] <- "Curruca cantillans"
  avo.traits$sps[avo.traits$sps=="Sylvia communis"] <- "Curruca communis"
  avo.traits$sps[avo.traits$sps=="Sylvia conspicillata"] <- "Curruca conspicillata"
  avo.traits$sps[avo.traits$sps=="Sylvia crassirostris"] <- "Curruca crassirostris"
  avo.traits$sps[avo.traits$sps=="Sylvia curruca"] <- "Curruca curruca"
  avo.traits$sps[avo.traits$sps=="Sylvia deserti"] <- "Curruca deserti"
  avo.traits$sps[avo.traits$sps=="Sylvia deserticola"] <- "Curruca deserticola"
  avo.traits$sps[avo.traits$sps=="Sylvia hortensis"] <- "Curruca hortensis"
  avo.traits$sps[avo.traits$sps=="Sylvia layardi"] <- "Curruca layardi"
  avo.traits$sps[avo.traits$sps=="Sylvia leucomelaena"] <- "Curruca leucomelaena"
  avo.traits$sps[avo.traits$sps=="Sylvia lugens"] <- "Curruca lugens"
  avo.traits$sps[avo.traits$sps=="Sylvia melanocephala"] <- "Curruca melanocephala"
  avo.traits$sps[avo.traits$sps=="Sylvia melanothorax"] <- "Curruca melanothorax"
  avo.traits$sps[avo.traits$sps=="Sylvia mystacea"] <- "Curruca mystacea"
  avo.traits$sps[avo.traits$sps=="Sylvia nana"] <- "Curruca nana"
  avo.traits$sps[avo.traits$sps=="Sylvia nisoria"] <- "Curruca nisoria"
  avo.traits$sps[avo.traits$sps=="Sylvia ruppeli"] <- "Curruca ruppeli"
  avo.traits$sps[avo.traits$sps=="Sylvia sarda"] <- "Curruca sarda"
  avo.traits$sps[avo.traits$sps=="Sylvia subalpina"] <- "Curruca subalpina"
  avo.traits$sps[avo.traits$sps=="Sylvia subcoerulea"] <- "Curruca subcoerulea"
  avo.traits$sps[avo.traits$sps=="Sylvia undata"] <- "Curruca undata"
  avo.traits$sps[avo.traits$sps=="Rhopocichla atriceps"] <- "Dumetia atriceps"
  avo.traits$sps[avo.traits$sps=="Geomalia heinrichi"] <- "Zoothera heinrichi"
  avo.traits$sps[avo.traits$sps=="Yuhina castaniceps"] <- "Staphida castaniceps"
  avo.traits$sps[avo.traits$sps=="Yuhina diademata"] <- "Parayuhina diademata"
  avo.traits$sps[avo.traits$sps=="Yuhina everetti"] <- "Staphida everetti"
  avo.traits$sps[avo.traits$sps=="Yuhina torqueola"] <- "Staphida torqueola"
  avo.traits$sps[avo.traits$sps=="Dinopium rafflesii"] <- "Chloropicoides rafflesii"
  avo.traits$sps[avo.traits$sps=="Tanygnathus everetti"] <- NA #No range, EN 
  avo.traits$sps[avo.traits$sps=="Gracula robusta"] <- NA #No range, CR
  # 14 NA
  # 3 species for which no distribution map is available, 10 news species, 1 lump (same species)
  
  avo.traits <- avo.traits %>% filter(!is.na(sps))
  
  return(avo.traits)
}

# Taxonomic crosswalk from Tobias et Pigot 2019 to Birdlife -------------------------------------------------------- 

crosswalk_Tobias19_BL <- function(corres.taxo,behaviour ){
  
  behaviour.mod <- behaviour
  
  for (k in 1:nrow(behaviour.mod)){
    for (i in 1:nrow(corres.taxo)){
      if (!is.na(behaviour.mod$Species[k]) & !is.na(corres.taxo$J_name[i]) & !is.na(corres.taxo$BL_name[i]) &
          behaviour.mod$Species[k]==corres.taxo$J_name[i]) {
        behaviour.mod$Species[k] <- corres.taxo$BL_name[i]
      }
    }
  }
  
  behaviour.mod$Species[behaviour.mod$Species=="Acrocephalus astrolabii"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Amphilais seebohmi"] <- "Bradypterus seebohmi"
  behaviour.mod$Species[behaviour.mod$Species=="Anodorhynchus glaucus"] <- NA # Missing in the selected species, assessed as CR in 2019, range = Possibly Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Anthornis melanocephala"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Aphrastura masafucrae"] <- "Aphrastura masafuerae"
  behaviour.mod$Species[behaviour.mod$Species=="Arborophila graydoni"] <- "Tropicoperdix graydoni"
  behaviour.mod$Species[behaviour.mod$Species=="Arborophila chloropus"] <- "Tropicoperdix chloropus"
  behaviour.mod$Species[behaviour.mod$Species=="Atlantisia rogersi"] <- "Laterallus rogersi"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax koslowi"] <- "Pterorhinus koslowi"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax lanceolatus"] <- "Pterorhinus lanceolatus"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax waddelli"] <- "Pterorhinus waddelli"
  behaviour.mod$Species[behaviour.mod$Species=="Caracara lutosa"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Chaetornis striata"] <- "Schoenicola striatus"
  behaviour.mod$Species[behaviour.mod$Species=="Charmosyna diadema"] <- NA # Missing in the selected species, assessed as CR in 2019,range = Possibly Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Chrysuronia oenone"] <- "Amazilia oenone"
  behaviour.mod$Species[behaviour.mod$Species=="Coenocorypha barrierensis"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Conostoma aemodium"] <- "Paradoxornis aemodium"
  behaviour.mod$Species[behaviour.mod$Species=="Edolisoma parvulum"] <- "Coracina parvula"
  behaviour.mod$Species[behaviour.mod$Species=="Corvinella corvina"] <- "Lanius corvinus"
  behaviour.mod$Species[behaviour.mod$Species=="Juliamyia julie"] <- "Amazilia julie"
  behaviour.mod$Species[behaviour.mod$Species=="Dinopium rafflesii"] <- "Chloropicoides rafflesii"
  behaviour.mod$Species[behaviour.mod$Species=="Dromaius minor"] <- NA #Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Eurostopodus exul"] <- NA # Missing in the selected species, assessed as CR in 2018, range = Possibly Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax albogularis"] <- "Pterorhinus albogularis"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax berthemyi"] <- "Pterorhinus berthemyi"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax bieti"] <- "Ianthocincla bieti"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax caerulatus"] <- "Pterorhinus caerulatus"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax calvus"] <- "Melanocichla calva"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax monachus"] <- "Pterorhinus monachus"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax cinereiceps"] <- "Ianthocincla cinereiceps"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax cinereifrons"] <- "Argya cinereifrons"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax courtoisi"] <- NA #No range map, CR under C2a
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax davidi"] <- "Pterorhinus davidi"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax delesserti"] <- "Pterorhinus delesserti"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax galbanus"] <- "Pterorhinus galbanus"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax gularis"] <- "Pterorhinus gularis"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax konkakinhensis"] <- "Ianthocincla konkakinhensis"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax lugubris"] <- "Melanocichla lugubris"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax lunulatus"] <- "Ianthocincla lunulata"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax maximus"] <- "Ianthocincla maxima"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax treacheri"] <- "Pterorhinus treacheri"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax nuchalis"] <- "Pterorhinus nuchalis"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax ocellatus"] <- "Ianthocincla ocellata"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax pectoralis"] <- "Pterorhinus pectoralis"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax perspicillatus"] <- "Pterorhinus perspicillatus"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax poecilorhynchus"] <- "Pterorhinus poecilorhynchus"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax ruficeps"] <- "Pterorhinus ruficeps"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax ruficollis"] <- "Pterorhinus ruficollis"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax rufogularis"] <- "Ianthocincla rufogularis"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax sannio"] <- "Pterorhinus sannio"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax sukatschewi"] <- "Ianthocincla sukatschewi"
  behaviour.mod$Species[behaviour.mod$Species=="Garrulax vassali"] <- "Pterorhinus vassali"
  behaviour.mod$Species[behaviour.mod$Species=="Geomalia heinrichi"] <- "Zoothera heinrichi"
  behaviour.mod$Species[behaviour.mod$Species=="Glaucidium mooreorum"] <- NA # Missing in the selected species, assessed as CR in 2019, range = Possibly Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Gorsachius magnificus"] <- "Oroanassa magnifica"
  behaviour.mod$Species[behaviour.mod$Species=="Antigone antigone"] <- "Grus antigone"
  behaviour.mod$Species[behaviour.mod$Species=="Antigone canadensis"] <- "Grus canadensis"
  behaviour.mod$Species[behaviour.mod$Species=="Antigone rubicunda"] <- "Grus rubicunda"
  behaviour.mod$Species[behaviour.mod$Species=="Antigone vipio"] <- "Grus vipio"
  behaviour.mod$Species[behaviour.mod$Species=="Turdinus calcicola"] <- "Gypsophila calcicola"
  behaviour.mod$Species[behaviour.mod$Species=="Heteroglaux blewitti"] <- "Athene blewitti"
  behaviour.mod$Species[behaviour.mod$Species=="Himatione fraithii"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Hylocharis chrysura"] <- "Amazilia chrysura"
  behaviour.mod$Species[behaviour.mod$Species=="Hylocharis cyanus"] <- "Amazilia cyanus"
  behaviour.mod$Species[behaviour.mod$Species=="Hylocharis eliciae"] <- "Amazilia eliciae"
  behaviour.mod$Species[behaviour.mod$Species=="Arachnothera hypogrammica"] <- "Kurochkinegramma hypogrammica"
  behaviour.mod$Species[behaviour.mod$Species=="Lepidopyga coeruleogularis"] <- "Amazilia coeruleogularis"
  behaviour.mod$Species[behaviour.mod$Species=="Lepidopyga goudoti"] <- "Amazilia goudoti"
  behaviour.mod$Species[behaviour.mod$Species=="Lepidopyga lilliae"] <- "Amazilia lilliae"
  behaviour.mod$Species[behaviour.mod$Species=="Locustella amnicola"] <- "Helopsaltes amnicola"
  behaviour.mod$Species[behaviour.mod$Species=="Locustella certhiola"] <- "Helopsaltes certhiola"
  behaviour.mod$Species[behaviour.mod$Species=="Locustella ochotensis"] <- "Helopsaltes ochotensis"
  behaviour.mod$Species[behaviour.mod$Species=="Locustella pleskei"] <- "Helopsaltes pleskei"
  behaviour.mod$Species[behaviour.mod$Species=="Locustella pryeri"] <- "Helopsaltes pryeri"
  behaviour.mod$Species[behaviour.mod$Species=="Loxops ochraceus"] <- NA # Missing in the selected species, assessed as CR in 2018, range = Possibly Extinct /Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Trichastoma cinereiceps"] <- "Pellorneum cinereiceps"
  behaviour.mod$Species[behaviour.mod$Species=="Melidectes fuscus"] <- "Melionyx fuscus"
  behaviour.mod$Species[behaviour.mod$Species=="Melidectes nouhuysi"] <- "Melionyx nouhuysi"
  behaviour.mod$Species[behaviour.mod$Species=="Melidectes princeps"] <- "Melionyx princeps"
  behaviour.mod$Species[behaviour.mod$Species=="Calendulauda alopex"] <- "Calendulauda africanoides"
  behaviour.mod$Species[behaviour.mod$Species=="Mirafra ashi"] <- "Mirafra somalica"
  behaviour.mod$Species[behaviour.mod$Species=="Mirafra gilletti"] <- "Calendulauda gilletti"
  behaviour.mod$Species[behaviour.mod$Species=="Mirafra rufa"] <- "Calendulauda rufa"
  behaviour.mod$Species[behaviour.mod$Species=="Turdinus brevicaudatus"] <- "Gypsophila brevicaudata"
  behaviour.mod$Species[behaviour.mod$Species=="Turdinus crassus"] <- "Gypsophila crassa"
  behaviour.mod$Species[behaviour.mod$Species=="Hypotaenidia dieffenbachii"] <- NA #Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Sinosuthora alphonsiana"] <- "Suthora alphonsiana"
  behaviour.mod$Species[behaviour.mod$Species=="Chleuasicus atrosuperciliaris"] <- "Suthora atrosuperciliaris"
  behaviour.mod$Species[behaviour.mod$Species=="Sinosuthora brunnea"] <- "Suthora brunnea"
  behaviour.mod$Species[behaviour.mod$Species=="Sinosuthora conspicillata"] <- "Suthora conspicillata"
  behaviour.mod$Species[behaviour.mod$Species=="Neosuthora davidiana"] <- "Suthora davidiana"
  behaviour.mod$Species[behaviour.mod$Species=="Psittiparus gularis"] <- "Paradoxornis gularis"
  behaviour.mod$Species[behaviour.mod$Species=="Psittiparus bakeri"] <- "Paradoxornis bakeri"
  behaviour.mod$Species[behaviour.mod$Species=="Psittiparus margaritae"] <- "Paradoxornis margaritae"
  behaviour.mod$Species[behaviour.mod$Species=="Cholornis paradoxus"] <- "Paradoxornis paradoxus"
  behaviour.mod$Species[behaviour.mod$Species=="Sinosuthora przewalskii"] <- "Suthora przewalskii"
  behaviour.mod$Species[behaviour.mod$Species=="Cholornis unicolor"] <- "Paradoxornis unicolor"
  behaviour.mod$Species[behaviour.mod$Species=="Sinosuthora webbiana"] <- "Suthora webbiana"
  behaviour.mod$Species[behaviour.mod$Species=="Sinosuthora zappeyi"] <- "Suthora zappeyi"
  behaviour.mod$Species[behaviour.mod$Species=="Parophasma galinieri"] <- "Sylvia galinieri"
  behaviour.mod$Species[behaviour.mod$Species=="Leucocarbo carunculatus"] <- NA # Missing in the selected species, assessed as VU in 2018, range = Extant (non-breeding)
  behaviour.mod$Species[behaviour.mod$Species=="Philydor novaesi"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Polihierax insignis"] <- "Neohierax insignis"
  behaviour.mod$Species[behaviour.mod$Species=="Pomarea fluxa"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Pomarea mira"] <- NA # Missing in the selected species, assessed as CR in 2016, range = Possibly Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Pomarea pomarea"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Porphyrio mantelli"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Porphyrio albus"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Hapalocrex flaviventer"] <- "Laterallus flaviventer"
  behaviour.mod$Species[behaviour.mod$Species=="Porzana spiloptera"] <- "Laterallus spilopterus"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula calthrapae"] <- "Nicopsitta calthrapae"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula columboides"] <- "Nicopsitta columboides"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula cyanocephala"] <- "Himalayapsitta cyanocephala"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula eques"] <- "Alexandrinus eques"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula eupatria"] <- "Palaeornis eupatria"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula finschii"] <- "Himalayapsitta finschii"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula himalayana"] <- "Himalayapsitta himalayana"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula krameri"] <- "Alexandrinus krameri"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula longicauda"] <- "Belocercus longicaudus"
  behaviour.mod$Species[behaviour.mod$Species=="Psittacula roseata"] <- "Himalayapsitta roseata"
  behaviour.mod$Species[behaviour.mod$Species=="Psitteuteles iris"] <- "Trichoglossus iris"
  behaviour.mod$Species[behaviour.mod$Species=="Ptilinopus mercierii"] <- NA # Extinct
  behaviour.mod$Species[behaviour.mod$Species=="Pyrocephalus dubius"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Rhopocichla atriceps"] <- "Dumetia atriceps"
  behaviour.mod$Species[behaviour.mod$Species=="Rimator malacoptilus"] <- "Napothera malacoptila"
  behaviour.mod$Species[behaviour.mod$Species=="Schoenicola brevirostris"] <- "Catriscus brevirostris"
  behaviour.mod$Species[behaviour.mod$Species=="Siphonorhis americana"] <- NA # # Missing in the selected species, assessed as CR in 2020, range = Possibly Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia boehmi"] <- "Curruca boehmi"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia buryi"] <- "Curruca buryi"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia curruca"] <- "Curruca curruca"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia subalpina"] <- "Curruca subalpina"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia communis"] <- "Curruca communis"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia conspicillata"] <- "Curruca conspicillata"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia deserticola"] <- "Curruca deserticola"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia crassirostris"] <- "Curruca crassirostris"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia layardi"] <- "Curruca layardi"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia leucomelaena"] <- "Curruca leucomelaena"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia lugens"] <- "Curruca lugens"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia melanocephala"] <- "Curruca melanocephala"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia melanothorax"] <- "Curruca melanothorax"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia mystacea"] <- "Curruca mystacea"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia deserti"] <- "Curruca deserti"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia nisoria"] <- "Curruca nisoria"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia ruppeli"] <- "Curruca ruppeli"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia balearica"] <- "Curruca balearica"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia subcoerulea"] <- "Curruca subcoerulea"
  behaviour.mod$Species[behaviour.mod$Species=="Sylvia undata"] <- "Curruca undata"
  behaviour.mod$Species[behaviour.mod$Species=="Tachybaptus rufolavatus"] <- NA # Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Trichastoma bicolor"] <- "Pellorneum bicolor"
  behaviour.mod$Species[behaviour.mod$Species=="Trichastoma buettikoferi"] <- "Pellorneum buettikoferi"
  behaviour.mod$Species[behaviour.mod$Species=="Trichastoma celebense"] <- "Pellorneum celebense"
  behaviour.mod$Species[behaviour.mod$Species=="Trichastoma pyrrogenys"] <- "Pellorneum pyrrogenys"
  behaviour.mod$Species[behaviour.mod$Species=="Trichastoma rostratum"] <- "Pellorneum rostratum"
  behaviour.mod$Species[behaviour.mod$Species=="Trichastoma tickelli"] <- "Pellorneum tickelli"
  behaviour.mod$Species[behaviour.mod$Species=="Turdinus rufipectus"] <- "Gypsophila rufipecta"
  behaviour.mod$Species[behaviour.mod$Species=="Turdoides affinis"] <- "Argya affinis"
  behaviour.mod$Species[behaviour.mod$Species=="Chatarrhaea gularis"] <- "Argya gularis"
  behaviour.mod$Species[behaviour.mod$Species=="Chatarrhaea longirostris"] <- "Argya longirostris"
  behaviour.mod$Species[behaviour.mod$Species=="Turdoides rufescens"] <- "Argya rufescens"
  behaviour.mod$Species[behaviour.mod$Species=="Turdoides somervillei"] <- "Argya somervillei"
  behaviour.mod$Species[behaviour.mod$Species=="Turnix novaecaledoniae"] <- NA # Missing in the selected species, assessed as CR in 2016, range = Possibily Extinct 
  behaviour.mod$Species[behaviour.mod$Species=="Urolestes melanoleucus"] <- "Lanius melanoleucus"
  behaviour.mod$Species[behaviour.mod$Species=="Yuhina castaniceps"] <- "Staphida castaniceps"
  behaviour.mod$Species[behaviour.mod$Species=="Yuhina diademata"] <- "Parayuhina diademata"
  behaviour.mod$Species[behaviour.mod$Species=="Yuhina everetti"] <- "Staphida everetti"
  behaviour.mod$Species[behaviour.mod$Species=="Yuhina torqueola"] <- "Staphida torqueola"
  behaviour.mod$Species[behaviour.mod$Species=="Lophura hatinhensis"] <- "Lophura edwardsi"
  behaviour.mod$Species[behaviour.mod$Species=="Phyllastrephus leucolepis"] <- "Phyllastrephus icterinus"
  # 25 NA
  # 15 extinct species 
  # 10 with missing information (range not in Extent...)
  
  behaviour.mod <- behaviour.mod %>% filter(!is.na(Species))
  
  return(behaviour.mod)

}



# Taxonomic crosswalk from Wilman et al 2014 to Birdlife -------------------------------------------------------- 

crosswalk_Wilman2014_BL <- function(corres.taxo,noc.traits){

  for (k in 1:nrow(noc.traits)){
    for (i in 1:nrow(corres.taxo)){
      if (!is.na(noc.traits$Species[k]) & !is.na(corres.taxo$J_name[i]) & !is.na(corres.taxo$BL_name[i]) &
          noc.traits$Species[k]==corres.taxo$J_name[i]) {
        noc.traits$Species[k] <- corres.taxo$BL_name[i]
      }
    }
  }
  
  noc.traits$Species[noc.traits$Species=="Acrocephalus astrolabii"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Amphilais seebohmi"] <- "Bradypterus seebohmi"
  noc.traits$Species[noc.traits$Species=="Anodorhynchus glaucus"] <- NA # Missing in the selected species, assessed as CR in 2019, range = Possibly Extinct 
  noc.traits$Species[noc.traits$Species=="Anthornis melanocephala"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Aphrastura masafucrae"] <- "Aphrastura masafuerae"
  noc.traits$Species[noc.traits$Species=="Arborophila graydoni"] <- "Tropicoperdix graydoni"
  noc.traits$Species[noc.traits$Species=="Arborophila chloropus"] <- "Tropicoperdix chloropus"
  noc.traits$Species[noc.traits$Species=="Atlantisia rogersi"] <- "Laterallus rogersi"
  noc.traits$Species[noc.traits$Species=="Garrulax koslowi"] <- "Pterorhinus koslowi"
  noc.traits$Species[noc.traits$Species=="Garrulax lanceolatus"] <- "Pterorhinus lanceolatus"
  noc.traits$Species[noc.traits$Species=="Garrulax waddelli"] <- "Pterorhinus waddelli"
  noc.traits$Species[noc.traits$Species=="Caracara lutosa"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Chaetornis striata"] <- "Schoenicola striatus"
  noc.traits$Species[noc.traits$Species=="Charmosyna diadema"] <- NA # Missing in the selected species, assessed as CR in 2019,range = Possibly Extinct 
  noc.traits$Species[noc.traits$Species=="Chrysuronia oenone"] <- "Amazilia oenone"
  noc.traits$Species[noc.traits$Species=="Coenocorypha barrierensis"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Conostoma aemodium"] <- "Paradoxornis aemodium"
  noc.traits$Species[noc.traits$Species=="Edolisoma parvulum"] <- "Coracina parvula"
  noc.traits$Species[noc.traits$Species=="Corvinella corvina"] <- "Lanius corvinus"
  noc.traits$Species[noc.traits$Species=="Juliamyia julie"] <- "Amazilia julie"
  noc.traits$Species[noc.traits$Species=="Dinopium rafflesii"] <- "Chloropicoides rafflesii"
  noc.traits$Species[noc.traits$Species=="Dromaius minor"] <- NA #Extinct 
  noc.traits$Species[noc.traits$Species=="Eurostopodus exul"] <- NA # Missing in the selected species, assessed as CR in 2018, range = Possibly Extinct 
  noc.traits$Species[noc.traits$Species=="Garrulax albogularis"] <- "Pterorhinus albogularis"
  noc.traits$Species[noc.traits$Species=="Garrulax berthemyi"] <- "Pterorhinus berthemyi"
  noc.traits$Species[noc.traits$Species=="Garrulax bieti"] <- "Ianthocincla bieti"
  noc.traits$Species[noc.traits$Species=="Garrulax caerulatus"] <- "Pterorhinus caerulatus"
  noc.traits$Species[noc.traits$Species=="Garrulax calvus"] <- "Melanocichla calva"
  noc.traits$Species[noc.traits$Species=="Garrulax monachus"] <- "Pterorhinus monachus"
  noc.traits$Species[noc.traits$Species=="Garrulax cinereiceps"] <- "Ianthocincla cinereiceps"
  noc.traits$Species[noc.traits$Species=="Garrulax cinereifrons"] <- "Argya cinereifrons"
  noc.traits$Species[noc.traits$Species=="Garrulax courtoisi"] <- NA #No range map, CR under C2a
  noc.traits$Species[noc.traits$Species=="Garrulax davidi"] <- "Pterorhinus davidi"
  noc.traits$Species[noc.traits$Species=="Garrulax delesserti"] <- "Pterorhinus delesserti"
  noc.traits$Species[noc.traits$Species=="Garrulax galbanus"] <- "Pterorhinus galbanus"
  noc.traits$Species[noc.traits$Species=="Garrulax gularis"] <- "Pterorhinus gularis"
  noc.traits$Species[noc.traits$Species=="Garrulax konkakinhensis"] <- "Ianthocincla konkakinhensis"
  noc.traits$Species[noc.traits$Species=="Garrulax lugubris"] <- "Melanocichla lugubris"
  noc.traits$Species[noc.traits$Species=="Garrulax lunulatus"] <- "Ianthocincla lunulata"
  noc.traits$Species[noc.traits$Species=="Garrulax maximus"] <- "Ianthocincla maxima"
  noc.traits$Species[noc.traits$Species=="Garrulax treacheri"] <- "Pterorhinus treacheri"
  noc.traits$Species[noc.traits$Species=="Garrulax nuchalis"] <- "Pterorhinus nuchalis"
  noc.traits$Species[noc.traits$Species=="Garrulax ocellatus"] <- "Ianthocincla ocellata"
  noc.traits$Species[noc.traits$Species=="Garrulax pectoralis"] <- "Pterorhinus pectoralis"
  noc.traits$Species[noc.traits$Species=="Garrulax perspicillatus"] <- "Pterorhinus perspicillatus"
  noc.traits$Species[noc.traits$Species=="Garrulax poecilorhynchus"] <- "Pterorhinus poecilorhynchus"
  noc.traits$Species[noc.traits$Species=="Garrulax ruficeps"] <- "Pterorhinus ruficeps"
  noc.traits$Species[noc.traits$Species=="Garrulax ruficollis"] <- "Pterorhinus ruficollis"
  noc.traits$Species[noc.traits$Species=="Garrulax rufogularis"] <- "Ianthocincla rufogularis"
  noc.traits$Species[noc.traits$Species=="Garrulax sannio"] <- "Pterorhinus sannio"
  noc.traits$Species[noc.traits$Species=="Garrulax sukatschewi"] <- "Ianthocincla sukatschewi"
  noc.traits$Species[noc.traits$Species=="Garrulax vassali"] <- "Pterorhinus vassali"
  noc.traits$Species[noc.traits$Species=="Geomalia heinrichi"] <- "Zoothera heinrichi"
  noc.traits$Species[noc.traits$Species=="Glaucidium mooreorum"] <- NA # Missing in the selected species, assessed as CR in 2019, range = Possibly Extinct 
  noc.traits$Species[noc.traits$Species=="Gorsachius magnificus"] <- "Oroanassa magnifica"
  noc.traits$Species[noc.traits$Species=="Antigone antigone"] <- "Grus antigone"
  noc.traits$Species[noc.traits$Species=="Antigone canadensis"] <- "Grus canadensis"
  noc.traits$Species[noc.traits$Species=="Antigone rubicunda"] <- "Grus rubicunda"
  noc.traits$Species[noc.traits$Species=="Antigone vipio"] <- "Grus vipio"
  noc.traits$Species[noc.traits$Species=="Turdinus calcicola"] <- "Gypsophila calcicola"
  noc.traits$Species[noc.traits$Species=="Heteroglaux blewitti"] <- "Athene blewitti"
  noc.traits$Species[noc.traits$Species=="Himatione fraithii"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Hylocharis chrysura"] <- "Amazilia chrysura"
  noc.traits$Species[noc.traits$Species=="Hylocharis cyanus"] <- "Amazilia cyanus"
  noc.traits$Species[noc.traits$Species=="Hylocharis eliciae"] <- "Amazilia eliciae"
  noc.traits$Species[noc.traits$Species=="Arachnothera hypogrammica"] <- "Kurochkinegramma hypogrammica"
  noc.traits$Species[noc.traits$Species=="Lepidopyga coeruleogularis"] <- "Amazilia coeruleogularis"
  noc.traits$Species[noc.traits$Species=="Lepidopyga goudoti"] <- "Amazilia goudoti"
  noc.traits$Species[noc.traits$Species=="Lepidopyga lilliae"] <- "Amazilia lilliae"
  noc.traits$Species[noc.traits$Species=="Locustella amnicola"] <- "Helopsaltes amnicola"
  noc.traits$Species[noc.traits$Species=="Locustella certhiola"] <- "Helopsaltes certhiola"
  noc.traits$Species[noc.traits$Species=="Locustella ochotensis"] <- "Helopsaltes ochotensis"
  noc.traits$Species[noc.traits$Species=="Locustella pleskei"] <- "Helopsaltes pleskei"
  noc.traits$Species[noc.traits$Species=="Locustella pryeri"] <- "Helopsaltes pryeri"
  noc.traits$Species[noc.traits$Species=="Loxops ochraceus"] <- NA # Missing in the selected species, assessed as CR in 2018, range = Possibly Extinct /Extinct 
  noc.traits$Species[noc.traits$Species=="Trichastoma cinereiceps"] <- "Pellorneum cinereiceps"
  noc.traits$Species[noc.traits$Species=="Melidectes fuscus"] <- "Melionyx fuscus"
  noc.traits$Species[noc.traits$Species=="Melidectes nouhuysi"] <- "Melionyx nouhuysi"
  noc.traits$Species[noc.traits$Species=="Melidectes princeps"] <- "Melionyx princeps"
  noc.traits$Species[noc.traits$Species=="Calendulauda alopex"] <- "Calendulauda africanoides"
  noc.traits$Species[noc.traits$Species=="Mirafra ashi"] <- "Mirafra somalica"
  noc.traits$Species[noc.traits$Species=="Mirafra gilletti"] <- "Calendulauda gilletti"
  noc.traits$Species[noc.traits$Species=="Mirafra rufa"] <- "Calendulauda rufa"
  noc.traits$Species[noc.traits$Species=="Turdinus brevicaudatus"] <- "Gypsophila brevicaudata"
  noc.traits$Species[noc.traits$Species=="Turdinus crassus"] <- "Gypsophila crassa"
  noc.traits$Species[noc.traits$Species=="Hypotaenidia dieffenbachii"] <- NA #Extinct 
  noc.traits$Species[noc.traits$Species=="Sinosuthora alphonsiana"] <- "Suthora alphonsiana"
  noc.traits$Species[noc.traits$Species=="Chleuasicus atrosuperciliaris"] <- "Suthora atrosuperciliaris"
  noc.traits$Species[noc.traits$Species=="Sinosuthora brunnea"] <- "Suthora brunnea"
  noc.traits$Species[noc.traits$Species=="Sinosuthora conspicillata"] <- "Suthora conspicillata"
  noc.traits$Species[noc.traits$Species=="Neosuthora davidiana"] <- "Suthora davidiana"
  noc.traits$Species[noc.traits$Species=="Psittiparus gularis"] <- "Paradoxornis gularis"
  noc.traits$Species[noc.traits$Species=="Psittiparus bakeri"] <- "Paradoxornis bakeri"
  noc.traits$Species[noc.traits$Species=="Psittiparus margaritae"] <- "Paradoxornis margaritae"
  noc.traits$Species[noc.traits$Species=="Cholornis paradoxus"] <- "Paradoxornis paradoxus"
  noc.traits$Species[noc.traits$Species=="Sinosuthora przewalskii"] <- "Suthora przewalskii"
  noc.traits$Species[noc.traits$Species=="Cholornis unicolor"] <- "Paradoxornis unicolor"
  noc.traits$Species[noc.traits$Species=="Sinosuthora webbiana"] <- "Suthora webbiana"
  noc.traits$Species[noc.traits$Species=="Sinosuthora zappeyi"] <- "Suthora zappeyi"
  noc.traits$Species[noc.traits$Species=="Parophasma galinieri"] <- "Sylvia galinieri"
  noc.traits$Species[noc.traits$Species=="Leucocarbo carunculatus"] <- NA # Missing in the selected species, assessed as VU in 2018, range = Extant (non-breeding)
  noc.traits$Species[noc.traits$Species=="Philydor novaesi"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Polihierax insignis"] <- "Neohierax insignis"
  noc.traits$Species[noc.traits$Species=="Pomarea fluxa"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Pomarea mira"] <- NA # Missing in the selected species, assessed as CR in 2016, range = Possibly Extinct 
  noc.traits$Species[noc.traits$Species=="Pomarea pomarea"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Porphyrio mantelli"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Porphyrio albus"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Hapalocrex flaviventer"] <- "Laterallus flaviventer"
  noc.traits$Species[noc.traits$Species=="Porzana spiloptera"] <- "Laterallus spilopterus"
  noc.traits$Species[noc.traits$Species=="Psittacula calthrapae"] <- "Nicopsitta calthrapae"
  noc.traits$Species[noc.traits$Species=="Psittacula columboides"] <- "Nicopsitta columboides"
  noc.traits$Species[noc.traits$Species=="Psittacula cyanocephala"] <- "Himalayapsitta cyanocephala"
  noc.traits$Species[noc.traits$Species=="Psittacula eques"] <- "Alexandrinus eques"
  noc.traits$Species[noc.traits$Species=="Psittacula eupatria"] <- "Palaeornis eupatria"
  noc.traits$Species[noc.traits$Species=="Psittacula finschii"] <- "Himalayapsitta finschii"
  noc.traits$Species[noc.traits$Species=="Psittacula himalayana"] <- "Himalayapsitta himalayana"
  noc.traits$Species[noc.traits$Species=="Psittacula krameri"] <- "Alexandrinus krameri"
  noc.traits$Species[noc.traits$Species=="Psittacula longicauda"] <- "Belocercus longicaudus"
  noc.traits$Species[noc.traits$Species=="Psittacula roseata"] <- "Himalayapsitta roseata"
  noc.traits$Species[noc.traits$Species=="Psitteuteles iris"] <- "Trichoglossus iris"
  noc.traits$Species[noc.traits$Species=="Ptilinopus mercierii"] <- NA # Extinct
  noc.traits$Species[noc.traits$Species=="Pyrocephalus dubius"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Rhopocichla atriceps"] <- "Dumetia atriceps"
  noc.traits$Species[noc.traits$Species=="Rimator malacoptilus"] <- "Napothera malacoptila"
  noc.traits$Species[noc.traits$Species=="Schoenicola brevirostris"] <- "Catriscus brevirostris"
  noc.traits$Species[noc.traits$Species=="Siphonorhis americana"] <- NA # # Missing in the selected species, assessed as CR in 2020, range = Possibly Extinct 
  noc.traits$Species[noc.traits$Species=="Sylvia boehmi"] <- "Curruca boehmi"
  noc.traits$Species[noc.traits$Species=="Sylvia buryi"] <- "Curruca buryi"
  noc.traits$Species[noc.traits$Species=="Sylvia curruca"] <- "Curruca curruca"
  noc.traits$Species[noc.traits$Species=="Sylvia subalpina"] <- "Curruca subalpina"
  noc.traits$Species[noc.traits$Species=="Sylvia communis"] <- "Curruca communis"
  noc.traits$Species[noc.traits$Species=="Sylvia conspicillata"] <- "Curruca conspicillata"
  noc.traits$Species[noc.traits$Species=="Sylvia deserticola"] <- "Curruca deserticola"
  noc.traits$Species[noc.traits$Species=="Sylvia crassirostris"] <- "Curruca crassirostris"
  noc.traits$Species[noc.traits$Species=="Sylvia layardi"] <- "Curruca layardi"
  noc.traits$Species[noc.traits$Species=="Sylvia leucomelaena"] <- "Curruca leucomelaena"
  noc.traits$Species[noc.traits$Species=="Sylvia lugens"] <- "Curruca lugens"
  noc.traits$Species[noc.traits$Species=="Sylvia melanocephala"] <- "Curruca melanocephala"
  noc.traits$Species[noc.traits$Species=="Sylvia melanothorax"] <- "Curruca melanothorax"
  noc.traits$Species[noc.traits$Species=="Sylvia mystacea"] <- "Curruca mystacea"
  noc.traits$Species[noc.traits$Species=="Sylvia deserti"] <- "Curruca deserti"
  noc.traits$Species[noc.traits$Species=="Sylvia nisoria"] <- "Curruca nisoria"
  noc.traits$Species[noc.traits$Species=="Sylvia ruppeli"] <- "Curruca ruppeli"
  noc.traits$Species[noc.traits$Species=="Sylvia balearica"] <- "Curruca balearica"
  noc.traits$Species[noc.traits$Species=="Sylvia subcoerulea"] <- "Curruca subcoerulea"
  noc.traits$Species[noc.traits$Species=="Sylvia undata"] <- "Curruca undata"
  noc.traits$Species[noc.traits$Species=="Tachybaptus rufolavatus"] <- NA # Extinct 
  noc.traits$Species[noc.traits$Species=="Trichastoma bicolor"] <- "Pellorneum bicolor"
  noc.traits$Species[noc.traits$Species=="Trichastoma buettikoferi"] <- "Pellorneum buettikoferi"
  noc.traits$Species[noc.traits$Species=="Trichastoma celebense"] <- "Pellorneum celebense"
  noc.traits$Species[noc.traits$Species=="Trichastoma pyrrogenys"] <- "Pellorneum pyrrogenys"
  noc.traits$Species[noc.traits$Species=="Trichastoma rostratum"] <- "Pellorneum rostratum"
  noc.traits$Species[noc.traits$Species=="Trichastoma tickelli"] <- "Pellorneum tickelli"
  noc.traits$Species[noc.traits$Species=="Turdinus rufipectus"] <- "Gypsophila rufipecta"
  noc.traits$Species[noc.traits$Species=="Turdoides affinis"] <- "Argya affinis"
  noc.traits$Species[noc.traits$Species=="Chatarrhaea gularis"] <- "Argya gularis"
  noc.traits$Species[noc.traits$Species=="Chatarrhaea longirostris"] <- "Argya longirostris"
  noc.traits$Species[noc.traits$Species=="Turdoides rufescens"] <- "Argya rufescens"
  noc.traits$Species[noc.traits$Species=="Turdoides somervillei"] <- "Argya somervillei"
  noc.traits$Species[noc.traits$Species=="Turnix novaecaledoniae"] <- NA # Missing in the selected species, assessed as CR in 2016, range = Possibily Extinct 
  noc.traits$Species[noc.traits$Species=="Urolestes melanoleucus"] <- "Lanius melanoleucus"
  noc.traits$Species[noc.traits$Species=="Yuhina castaniceps"] <- "Staphida castaniceps"
  noc.traits$Species[noc.traits$Species=="Yuhina diademata"] <- "Parayuhina diademata"
  noc.traits$Species[noc.traits$Species=="Yuhina everetti"] <- "Staphida everetti"
  noc.traits$Species[noc.traits$Species=="Yuhina torqueola"] <- "Staphida torqueola"
  noc.traits$Species[noc.traits$Species=="Lophura hatinhensis"] <- "Lophura edwardsi"
  noc.traits$Species[noc.traits$Species=="Phyllastrephus leucolepis"] <- "Phyllastrephus icterinus"
  noc.traits$Species[noc.traits$Species=="Mitu mitu"] <- NA # Extinct in the wild
  noc.traits$Species[noc.traits$Species=="Zenaida graysoni"] <- NA # Extinct in the wild
  noc.traits$Species[noc.traits$Species=="Claravis geoffroyi"] <- "Paraclaravis geoffroyi"
  noc.traits$Species[noc.traits$Species=="Setopagis maculosa"] <- NA # Missing in the selected species, assessed as DD in 2016, range = Possibily Extant 
  noc.traits$Species[noc.traits$Species=="Caprimulgus centralasicus"] <- NA # Missing in the selected species, assessed as DD in 2016, range = Possibily Extant 
  noc.traits$Species[noc.traits$Species=="Discosura letitiae"] <- NA  # Missing in the selected species, assessed as DD in 2016, range = Possibily extant
  noc.traits$Species[noc.traits$Species=="Eriocnemis godini"] <- NA # Missing in the selected species, assessed as CR in 2020, range = Possibily extinct
  noc.traits$Species[noc.traits$Species=="Pterodroma caribbaea"] <- NA # Missing in the selected species, assessed as CR in 2018, range = Possibily extinct
  noc.traits$Species[noc.traits$Species=="Phalacrocorax bougainvilliorum"] <- "Leucocarbo bougainvilliorum"
  noc.traits$Species[noc.traits$Species=="Numenius borealis"] <- NA # Missing in the selected species, assessed as CR in 2020, range = Possibily extinct
  noc.traits$Species[noc.traits$Species=="Campephilus principalis"] <- NA # Missing in the selected species, assessed as CR in 2020, range = Possibily extinct & possibily extant
  noc.traits$Species[noc.traits$Species=="Campephilus imperialis"] <- NA # Missing in the selected species, assessed as CR in 2020, range = Possibily extinct
  noc.traits$Species[noc.traits$Species=="Hylatomus galeatus"] <- "Celeus galeatus"
  noc.traits$Species[noc.traits$Species=="Cyanopsitta spixii"] <- NA # Extinct in the wild 
  noc.traits$Species[noc.traits$Species=="Drymotoxeres pucherani"] <- "Drymotoxeres pucheranii"
  noc.traits$Species[noc.traits$Species=="Rhipidura hypoxantha"] <- "Chelidorhynx hypoxanthus"
  noc.traits$Species[noc.traits$Species=="Psilorhinus morio"] <- "Cyanocorax morio"
  noc.traits$Species[noc.traits$Species=="Corvus hawaiiensis"] <- NA # Extinct in the wild
  noc.traits$Species[noc.traits$Species=="Eurochelidon sirintarae"] <- NA # Missing in the selected species, assessed as CR in 2021, range = Extant (seasonality uncertain) + Possibily extant
  noc.traits$Species[noc.traits$Species=="Petrochelidon perdita"] <- NA # Missing in the selected species, assessed as DD in 2016, range = Possibily extant
  noc.traits$Species[noc.traits$Species=="Buettikoferella bivittata"] <- "Cincloramphus bivittatus"
  noc.traits$Species[noc.traits$Species=="Bradypterus alfredi"] <- "Locustella alfredi"
  noc.traits$Species[noc.traits$Species=="Acrocephalus sorghophilus"] <- NA # Missing in the selected species, assessed as EN in 2016, range = Extant (non-breeding) + Possibily extant
  noc.traits$Species[noc.traits$Species=="Trichastoma malaccense"] <- "Pellorneum malaccense"
  noc.traits$Species[noc.traits$Species=="Rimator danjoui"] <- "Napothera danjoui"
  noc.traits$Species[noc.traits$Species=="Rimator albostriatus"] <- "Napothera albostriata"
  noc.traits$Species[noc.traits$Species=="Rimator pasquieri"] <- "Napothera pasquieri"
  noc.traits$Species[noc.traits$Species=="Myadestes lanaiensis"] <- NA # Missing in the selected species, assessed as CR in 2019, range = Possibily Extinct
  noc.traits$Species[noc.traits$Species=="Anthus longicaudatus"] <- "Anthus vaalensis"
  noc.traits$Species[noc.traits$Species=="Psittirostra psittacea"] <- NA # Missing in the selected species, assessed as CR in 2016, range = Possibily Extinct
  noc.traits$Species[noc.traits$Species=="Hemignathus affinis"] <- NA # Missing in the selected species, assessed as CR in 2016, range = Possibily Extinct
  noc.traits$Species[noc.traits$Species=="Paroreomyza flammea"] <- NA # Extinct
  noc.traits$Species[noc.traits$Species=="Melamprosops phaeosoma"] <- NA # Extinct
  noc.traits$Species[noc.traits$Species=="Vermivora bachmanii"] <- NA # Missing in the selected species, assessed as DD in 2016, range = Possibily Extinct
  noc.traits$Species[noc.traits$Species=="Melozone fuscus"] <- "Melozone fusca"
  
  noc.traits <- noc.traits %>% filter(!is.na(Species))
  # 46 NA 
  # 21 extinct species
  # 25 with missing information (range not in Extent...)
  
  return(noc.traits)
  
}

# End -----


