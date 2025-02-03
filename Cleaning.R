#     ---- DATA CLEANING ----

boat <- boatraw %>% clean_names()

# Split price column and creating a new converted EUR column -- 
# More efficient for proper price analysis -- 

boat <- boat %>% 
  mutate(Currency= str_split(price, " ", simplify = TRUE)[,1],
         price=str_remove_all(price,"[^0-9]"),
         price=as.numeric(price))

boat <- boat %>% 
  mutate(price_eur=case_when(Currency=="CHF" ~ price * 1.06,
                             Currency=="DKK" ~ price * 0.13,
                             Currency=="Â£" ~ price * 1.2,
                             TRUE ~ price)) %>% 
  select(-price,-Currency)


# Some boats has multiple types, lets split them -- 

boat <- boat %>% 
  separate_rows(boat_type,sep=",") 

#Manufacturer consists of 1405 NA:s , replace with unknown -- 

boat <- boat %>% 
  mutate(manufacturer=ifelse(is.na(manufacturer),"Unknown",manufacturer))

#        Cleaning column type --- 

boat <- boat %>% separate_rows(type,sep=",") %>% 
  mutate(type=ifelse(is.na(type),"Unknown",type))

#   12 NA:s in column length, mean imputation -- 
#   61 NA:s in column width, mean imputation -- 

boat <- boat %>% 
  mutate(length = ifelse(is.na(length), mean(length, na.rm = TRUE), length),
         width= ifelse(is.na(width), mean(width, na.rm = TRUE), width))

#  Material -- Handling NA:s --

boat <- boat %>% 
  mutate(material=ifelse(is.na(material),"Unknown",material))


# Cleaning location column -- Creating a country column --- 

boat <- boat %>% 
  mutate(country=case_when(
    str_detect(location,"Netherlands")~"Netherlands",
    str_detect(location,"Norway")~"Norway",
    str_detect(location,"Philippines")~"Philippines",
    str_detect(location,"Poland")~"Poland",
    str_detect(location,"Portugal")~"Portugal",
    str_detect(location,"Portugal")~"Portugal",
    str_detect(location,"Russian")~"Russia",
    str_detect(location,"Slovak Republic")~"Slovak Republic",
    str_detect(location,"Slovenia")~"Slovenia",
    str_detect(location,"Spain")~"Spain",
    str_detect(location,"Sweden")~"Sweden",
    str_detect(location,"Switzerland")~"Switzerland",
    str_detect(location,"Thailand")~"Thailand",
    str_detect(location,"Turkey")~"Turkey",
    str_detect(location,"Ukraine")~"ukraine",
    str_detect(location,"Arab Emirates")~"Arab Emirates",
    str_detect(location,"United Kingdom")~"United Kingdom",
    str_detect(location,"United States")~"United States",
    str_detect(location,"Germany")~"Germany",
    str_detect(location,"Greece")~"Greece",
    str_detect(location,"Hungary")~"Hungary",
    str_detect(location,"Ibiza")~"Spain",
    str_detect(location,"Ireland")~"Ireland",
    str_detect(location,"Italie")~"Italy",
    str_detect(location,"Italien")~"Italy",
    str_detect(location,"Italy")~"Italy",
    str_detect(location,"Latvia")~"Latvia",
    str_detect(location,"Lithuania")~"Lithuania",
    str_detect(location,"Mallorca")~"Spain",
    str_detect(location,"Malta")~"Malta",
    str_detect(location,"Montenegro")~"Montenegro",
    str_detect(location,"Australia")~"Australia",
    str_detect(location,"Austria")~"Austria",
    str_detect(location,"Belgium")~"Belgium",
    str_detect(location,"Bulgaria")~"Bulgaria",
    str_detect(location,"Croatia")~"Croatia",
    str_detect(location,"Cyprus")~"Cyprus",
    str_detect(location,"Czech Republic")~"Czech Republic",
    str_detect(location,"Denmark")~"Denmark",
    str_detect(location,"Egypt")~"Egypt",
    str_detect(location,"Estonia")~"Estonia",
    str_detect(location,"Finland")~"Finland",
    str_detect(location,"France")~"France",
    TRUE ~ location))

boat <- boat %>% 
  mutate(country=case_when(
    str_detect(location,"BÃ¼delsdorf")~"Germany",
    str_detect(location,"Traunstein")~"Traunsteiny",
    str_detect(location,"Adria")~"Italy",
    str_detect(location,"Angera")~"Italy",
    str_detect(location,"Avenches")~"Switzerland",
    str_detect(location,"Barssel")~"Germany",
    str_detect(location,"Beilngries")~"Germany",
    str_detect(location,"BelgiÃ«")~"Belgium",
    str_detect(location,"Berlin")~"Germany",
    str_detect(location,"Bielefeld")~"Germany",
    str_detect(location,"Brandenburg")~"Germany",
    str_detect(location,"Bremen")~"Germany",
    str_detect(location,"Dalmatien")~"Croatia",
    str_detect(location,"Donau")~"Donau",
    str_detect(location,"Faoug")~"Switzerland",
    str_detect(location,"French")~"France",
    str_detect(location,"FuÃ\u009fach")~"France",
    str_detect(location,"Gibraltar")~"United Kingdom",
    str_detect(location,"Greetsile")~"Germany",
    str_detect(location,"Heilbronn")~"Germany",
    str_detect(location,"Isle of Man")~"United Kingdom",
    str_detect(location,"Izola")~"Slovenia",
    str_detect(location,"Jersey")~"United Kingdom",
    str_detect(location,"Juelsminde Havn")~"Denmark",
    str_detect(location,"Katwijk")~"Netherlands",
    str_detect(location,"Kroatien")~"Croatia",
    str_detect(location,"Juelsminde Havn")~"Denmark",
    str_detect(location,"Castelletto Ticino")~"Italy",
    str_detect(location,"Minusio")~"Switzerland",
    str_detect(location,"78337")~"Germany",
    str_detect(location,"78343")~"Germany",
    str_detect(location,"8253")~"Switzerland",
    str_detect(location,"8560")~"Germany",
    str_detect(location,"Altenrhein")~"Switzerland",
    str_detect(location,"Andelfingen")~"Switzerland",
    str_detect(location,"Sipplingen")~"Germany",
    str_detect(location,"St.Gallen")~"Switzerland",
    str_detect(location,"Founex")~"Switzerland",
    str_detect(location,"Geneva")~"Switzerland",
    str_detect(location,"GenÃ¨ve")~"Switzerland",
    str_detect(location,"Lausanne")~"Switzerland",
    str_detect(location,"Nyon")~"Switzerland",
    str_detect(location,"versoix")~"Switzerland",
    str_detect(location,"Lebanon")~"Lebanon",
    str_detect(location,"Lommel")~"Belgium",
    str_detect(location,"Marina Punat")~"Croatia",
    str_detect(location,"Lebanon")~"Lebanon",
    str_detect(location,"Martinique Â» Le Marin")~"France",
    str_detect(location,"Monaco")~"Monaco",
    str_detect(location,"Morocco")~"Morocco",
    str_detect(location,"Neusiedl am See")~"Austria",
    str_detect(location,"Neustadt in Holstein (Ostsee)")~"Germany",
    str_detect(location,"Niederrhein")~"Germany",
    str_detect(location,"NordseekÃ¼ste")~"Germany",
    str_detect(location,"Novi Vinodolski")~"Croatia",
    str_detect(location,"Wegorzewo")~"Poland",
    str_detect(location,"Opwijk")~"Netherlands",
    str_detect(location,"Ostsee Â» Naantali")~"Finland",
    str_detect(location,"Ostsee Â» Zingst")~"Germany",
    str_detect(location,"PT Ã¸stkysten ellers Esbjerg")~"Denmark",
    str_detect(location,"Porto Rotondo")~"Italy",
    str_detect(location,"Rheinfelden")~"Germany",
    str_detect(location,"Rolle")~"Switzerland",
    str_detect(location,"Romania")~"Romania",
    str_detect(location,"Rostock")~"Germany",
    str_detect(location,"Rovinij")~"Croatia",
    str_detect(location,"RÃ¼gen")~"Germany",
    str_detect(location,"Serbia")~"Serbia",
    str_detect(location,"Seychelles")~"Seychelles",
    str_detect(location,"Split")~"Croatia",
    str_detect(location,"Steinwiesen")~"Germany",
    str_detect(location,"Stralsund")~"Germany",
    str_detect(location,"Taiwan")~"Taiwan",
    str_detect(location,"Thalwil")~"Switzerland",
    str_detect(location,"Thun")~"Switzerland",
    str_detect(location,"Toscana")~"Italy",
    str_detect(location,"TravemÃ¼nde")~"Germany",
    str_detect(location,"Venezuela")~"Venezuela",
    str_detect(location,"VierwaldstÃ¤ttersee - Buochs")~"Switzerland",
    str_detect(location,"Welschenrohr")~"Switzerland",
    str_detect(location,"Wijdenes")~"Netherlands",
    str_detect(location,"Zevenbergen")~"Netherlands",
    str_detect(location,"ZÃ¼richse, 8855 Wangen SZ")~"Switzerland",
    str_detect(location,"annecy")~"France",
    str_detect(location,"baden baden")~"Germany",
    str_detect(location,"espa?a")~"Spain",
    str_detect(location,"lago maggiore")~"Italy",
    str_detect(location,"ukraine")~"Ukraine",
    str_detect(location,"waren mÃ¼ritz")~"Germany",
    str_detect(location,"Lago di Garda")~"Italy",
    str_detect(location,"Steckborn")~"Switzerland",
    str_detect(location,"(Ostsee)")~"Germany",
    str_detect(location,"Tenero, lago Maggiore")~"Switzerland",
    TRUE ~ country))

#   Replace NA:s with unknown -- Remove location -- 

boat <- boat %>% 
  mutate(country=ifelse(is.na(country),"Unknown",country)) %>% 
  select(-location)


#  Change variable type for modeling -- 

boat <- boat %>% 
  mutate_if(is.character,factor)

# Creating size var -- RM LENGTH / WIDTH -- 

boat <- boat %>% 
  mutate(size=length*width) %>% 
  select(-length,-width)