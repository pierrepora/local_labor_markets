#########################################################################################################################
#This program cleans up the data related to municipal budget
#########################################################################################################################

#Load appropriate packages

#install.packages("stringr")
library(stringr)

#install.packages("data.table")
library(data.table)

#install.packages("lubridate")
library(lubridate)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("dplyr")
library(dplyr)

#Data repository

data_repository<-"U:\\Dexia\\Data\\"

#Initialize year

for(year_ref in 2000:2018){
  
  #Load data
  
  budget_data<-data.table(read.csv(paste0(data_repository,"donneesbpse_dobp_",year_ref,".csv"),sep=";"))
  municipality_identifiers<-data.table(read.csv(paste0(data_repository,
                                                       "table_passage_geo2003_geo2020.csv")))
  
  
  
  
   #Create municipality identifier in budget data
  
  budget_data$depcom<-""
  budget_data[str_length(icom)==1]$depcom<-paste0(budget_data[str_length(icom)==1]$dep,
                                                  "00",
                                                  budget_data[str_length(icom)==1]$icom)
  budget_data[str_length(icom)==2]$depcom<-paste0(budget_data[str_length(icom)==2]$dep,
                                                  "0",
                                                  budget_data[str_length(icom)==2]$icom)
  budget_data[str_length(icom)==3]$depcom<-paste0(budget_data[str_length(icom)==3]$dep,
                                                  budget_data[str_length(icom)==3]$icom)
  budget_data[substr(budget_data$depcom,1,1)==0]$depcom<-substr(budget_data[substr(budget_data$depcom,1,1)==0]$depcom,
                                                                2,
                                                                6)
  budget_data[substr(dep,1,1)==1]$depcom<-paste0(97,substr(budget_data[substr(dep,1,1)==1]$depcom,
                                                           4,
                                                           6))
  #Implement municipality changes that occured between 2000 and 2003
  
  budget_data[inom=="PARIS"]$depcom<-"75056"
  budget_data[inom=="LAUX-MONTAUX"]$depcom<-"26091"
  budget_data[inom=="GEANGES"]$depcom<-"71443"
  budget_data[inom=="MARNEFER"]$depcom<-"61136"
  budget_data[inom=="AUBIERS (LES)"]$depcom<-"79195"
  
  
  new_budget_data<-merge(budget_data,
                         municipality_identifiers,
                         by.x="depcom",
                         by.y="CODGEO_INI",
                         all.x=TRUE,
                         all.y=FALSE)
  
  
  final_budget_data<-new_budget_data[CODGEO_2020!="" & is.na(CODGEO_2020)==FALSE,
                                     list(
                                       population=sum(pop1),
                                       produits=sum(prod),
                                       impots_locaux=sum(impo1),
                                       autres_impots=sum(impo2),
                                       dotation_globale_de_fonctionnement=sum(dgf),
                                       charges=sum(charge),
                                       charges_personnel=sum(perso),
                                       achats=sum(achat),
                                       charges_financieres=sum(fin),
                                       contingents=sum(cont),
                                       subventions_versees=sum(subv),
                                       resultat_comptable=sum(res1),
                                       if(year_ref<=2010){
                                         potentiel_fiscal=sum(potfis)
                                       }
                                       else{
                                         potfis=NA
                                       },
                                       produits_taxe_habitation=sum(pth),
                                       produits_foncier_bati=sum(pfb),
                                       produits_foncier_non_bati=sum(pfnb),
                                       if(year_ref<=2010){
                                         produits_tax_pro=sum(ptp)}
                                       else{
                                         produits_tax_pro=NA
                                       },
                                       if(year_ref>=2011){
                                         produits_taxe_additionnelle_proprietes_non_baties=sum(ptafnb)
                                       }
                                       else{
                                         produits_taxe_additionnelle_proprietes_non_baties=NA
                                       },
                                       if(year_ref>=2010){
                                         produits_cotisation_fonciere_entreprises=sum(pcfe)
                                       }
                                       else{
                                         produits_cotisation_fonciere_entreprises=NA
                                       },
                                       if(year_ref>=2011){
                                         produits_cotisation_VA_entreprises=sum(cvaec)
                                         }
                                       else{
                                         produits_cotisation_VA_entreprises=NA
                                       },
                                       if(year_ref>=2011){
                                         imposition_forfaitaire_entreprises_reseau=sum(iferc)
                                         }
                                       else{
                                         imposition_forfaitaire_entreprises_reseau=NA
                                       },
                                       if(year_ref>=2011){
                                         produits_taxe_surfaces_commerciales=sum(tascomc)
                                         }
                                       else{
                                         produits_taxe_surfaces_commerciales=NA
                                       },
                                       ressources_investissement=sum(recinv),
                                       emprunts_banaces=sum(emp),
                                       subventions_recues=sum(subr),
                                       fond_compensation_TVA=sum(fctva),
                                       retour_biens_affectes=sum(raff),
                                       depenses_investissement=sum(depinv),
                                       depenses_equipment=sum(equip),
                                       remboursements_emprunts=sum(remb),
                                       charges_a_repartir=sum(repart),
                                       immobilisations_affectees=sum(daff),
                                       besoin_financement_investissement=sum(bf2),
                                       resultat_ensemble=sum(res2),
                                       capacite_autofinancement=sum(caf),
                                       capacite_autofinancement_nett=sum(cafn),
                                       encours_dette=sum(dette),
                                       annuite_dette=sum(annu),
                                       if(year_ref<=2009){
                                         avance_tresor=sum(avance)
                                       }
                                       else{
                                         avance_tresor=NA
                                       },
                                       taux_taxe_habitation=sum(tth*pop1)/sum(pop1),
                                       taux_foncier_bati=sum(tfb*pop1)/sum(pop1),
                                       taux_foncier_non_bati=sum(tfnb*pop1)/sum(pop1),
                                       if(year_ref>=2011){
                                         taux_tax_additionnelle_foncier_non_bati=sum(tafnb*pop1)/sum(pop1)
                                       }
                                       else{
                                         taux_tax_additionnelle_foncier_non_bati=NA
                                       },
                                       taux_taxe_professionnelle=sum(ttp*pop1)/sum(pop1)
                                     ),
                                     by=CODGEO_2020]
  
  colnames(final_budget_data)[colnames(final_budget_data) %in% c("V13",
                                                                 "V17",
                                                                 "V18",
                                                                 "V19",
                                                                 "V20",
                                                                 "V21",
                                                                 "V22",
                                                                 "V39",
                                                                 "V43")]<-c("potfis",
                                                                            "produits_tax_pro",
                                                                            "produits_taxe_additionnelle_proprietes_non_baties",
                                                                            "produits_cotisation_fonciere_entreprises",
                                                                            "produits_cotisation_VA_entreprises",
                                                                            "imposition_forfaitaire_entreprises_reseau",
                                                                            "produits_taxe_surfaces_commerciales",
                                                                            "avance_tresor",
                                                                            "taux_tax_additionnelle_foncier_non_bati")
  
  write.csv(final_budget_data,paste0(data_repository,"clean_budget_data_",year_ref,".csv"))
  
}



