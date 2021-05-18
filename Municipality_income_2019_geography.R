library(data.table)
library(stringr)

AdminGeog2019<-fread("./Data/v_commune_2019.csv")[,
                                                  c("actual",
                                                    "id_commune",
                                                    "id_bv",
                                                    "id_communeparent")] 

municipal_income<-rbindlist(lapply(2001:2018,
                                   FUN = function(x){fread(paste0("./Data/Municipality-level pre-tax income/Municipality_income_",
                                                                  x,
                                                                  ".csv"))}),
                            fill=TRUE,
                            idcol="year")

#Problem when the information is stored in character variables

col_prob<-c("Q212",
            "Q213",
            "Q214",
            colnames(municipal_income)[grepl("NBUC",
                                             colnames(municipal_income))])

municipal_income[,
                 (col_prob):=lapply(X=.SD,
                                    FUN=function(x){as.numeric(str_replace(x,
                                                                           ",",
                                                                           "."))}),
                 .SDcols=col_prob]

#Function that regroups all the information that is stored across multiple year specific 
# variables into a single variable

SingleVar<-function(cols,
                    varname){
  municipal_income[,
                   (c(varname)):=as.numeric(eval(parse(text=paste0("pmax(",paste(paste0("municipal_income$",
                                                                                        cols),
                                                                                 collapse = ","),
                                                                   ",na.rm=TRUE)"))))]
}

#Recover all median income variables as a single variable

col_med<-colnames(municipal_income)[grepl("Q2",colnames(municipal_income))]
SingleVar(cols=col_med,
          varname="median_income")

#Recover all population variables as a single variable

col_pop<-colnames(municipal_income)[grepl("NBUC",colnames(municipal_income))]
SingleVar(cols=col_pop,
          varname="pop_UC")

#Municipality identifier

municipal_income$id_commune<-str_remove(paste0(municipal_income$COM,
                                               municipal_income$CODGEO),
                                        "NA")

#Correct year

municipal_income[,
                 year:=2000+year]

#Keep only the relevant variables

municipal_income<-municipal_income[!is.na(median_income),
                                   c("year",
                                     "id_commune",
                                     "pop_UC",
                                     "median_income")]

#Use the 2019 administrative geography

municipal_income<-merge(municipal_income,
                        AdminGeog2019[,-c("id_bv")],
                        by.x="id_commune",
                        by.y="id_commune",
                        all.x=TRUE,
                        all.y=FALSE)[actual!=1,
                                id_commune:=id_communeparent]

municipal_income<-merge(municipal_income[,-c("actual",
                                        "id_communeparent")],
                        AdminGeog2019[actual==1
                                      ,c("id_commune",
                                         "id_bv")],
                        by.x="id_commune",
                        by.y="id_commune",
                        all.y=FALSE)

#Regroup at the appropriate level given the new geography

municipal_income<-municipal_income[,
                                   list(median_income=weighted.mean(median_income,
                                                                    weights=pop_UC),
                                        pop_UC=sum(pop_UC)),
                                   by=c("id_commune",
                                        "id_bv",
                                        "year")]

write.csv(x=municipal_income,
          file="./Data/municipal_income_geo2019.csv")
