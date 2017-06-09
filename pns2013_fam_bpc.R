#---------------------------#
# Elegiveis BPC na PNS 2013 #
#---------------------------#

# Variaveis disponiveis
# C00301  - Nº ordem do morador
# C004    - cond dom
# C010    - vive com conjuge
# C011    - Estado Civil
# juntar conjuge de mesmo sexo e sexo distinto


# Abre e instala pacotes necessarios e puxa base de dados  --------------------------------------
  #install.packages("devtools")
  #install.packages("SAScii")
  #install.packages("survey")
  #install.packages("dplyr")
  #install.packages("downloader")
  library(SAScii) 	# load the SAScii package (imports ascii data with a SAS script)
  library(downloader)	# downloads and then runs the source() function on scripts from github
  library(dplyr)
  library(foreign)
  library(survey) 
  library(data.table)
  dir_base <- 'D:/Repositorios/pns2013_fam_bpc'
  #pns_pes2013<-readRDS("D:/Repositorios/pns2013_fam_bpc/pes_pns2013")
 
# # baixa e importa base de dados
#   download_sourceData("PNS", 2013, unzip = T)
#   pns_pes2013 <- read_PNS("pessoas"  , 2013)
#   pns_pes2013 <- read.SAScii( "D:/Repositorios/pns2013_fam_bpc/pns_2013_microdados_2017_03_23/Dados/PESPNS2013.txt" ,
#                               "D:/Repositorios/pns2013_fam_bpc/pns_2013_microdados_2017_03_23/Dicionarios_e_input/input_PESPNS2013.sas" )
  # importa base gerada no stata por datazoom (pessoas e do)
   pns2013 <- read.csv(paste0(dir_base,'/bases_pns/pns2013.csv'), header=TRUE, sep=",")
   setDT(pns2013)
   pns2013 %>%  str
  
  # importa tabela de reclassificação
    tab<-read.csv(paste0(dir_base,'/reclassificacao/reclass.csv'), header = T, sep = ";",
                  stringsAsFactors=FALSE, encoding = "Latin-1")
    setDT(tab)
  
# Cria e renomeia variaveis -----------------------------------------------
  
  # gera chave para domicilio
    pns2013 <- pns2013 %>% mutate(domid = paste(UPA_PNS,V0006_PNS,sep =""))
    pns2013 <- pns2013 %>% rename(idade= c008,
                                          sexo = c006)
  
  # deixa apenas idosos e pessoas com deficiencia idade=65 & g001==1 &g006==1&g014==1&g021==1
    # e marca pessoas com deficiencia
    setDT(pns2013)
    pns2013<-pns2013[idade>=65 | g001==1 | g006==1 | g014==1 | g021==1]
    pns2013[g001==1 | g006==1 | g014==1 | g021==1, deficiencia:=1]
    
  # Gera indicador de deficiencia com algum grau de limitacao (pcad); >=moderado (PCAD_moderado) e >=Intensamente (PCAD_intenso)
    pns2013[deficiencia==1 & (g004>=3 | g009>=3| g017>=3| g026>=3), pcad_moderado:=1]
    pns2013[deficiencia==1 & (g004>=4 | g009>=4| g017>=4| g026>=4), pcad_intenso :=1]
    
  # Marca idoso (idade>=65)
    pns2013[idade>=65, idoso:=1]  
    
  # junta conjuge de mesmo sexo e sexo distintos no cod(2) = conjuge
    pns2013[, c004_original:=c004]
    pns2013[c004==3, c004:=2]
      pns2013[, .N, by=c004][order(c004)]
      
  # junta Outro parente,Agregado(a), Convivente , Pensionista, Empregado(a),
      #dom?stico(a) e Parente do(a) empregado(a)  dom?stico(a)  em outros
      pns2013[c004>=14 & c004<=19, c004:=20]
      
  # Gera pre_elegivel (aplicando todas regras de elegibilidade exceto renda)
    #idoso e nao recebe aposentadoria ou pensao da seguridade social f001==2
      pns2013[idoso==1 & f001==2, pre_elegivel1:=1]
      pns2013[idoso==1 & f001==2, pre_elegivel2:=1]
    # pess com deficiencia e nao recebe aposentadoria ou pensao da seguridade social e trabalho diferente de aprendiz
      pns2013[pcad_moderado==1 & f001==2 & is.na(e014), pre_elegivel1:=1]
      pns2013[pcad_intenso==1  & f001==2 & is.na(e014), pre_elegivel2:=1]  
        pns2013[, .N, by=pre_elegivel1]
        pns2013[, .N, by=pre_elegivel2]
      
  # numero de elegiveis dentro de um mesmo domicilio
    pns2013[, n_elegivel:=sum(pre_elegivel1, na.rm = T), by=domid]
    pns2013[, .N, by=n_elegivel]
      
  # PRE_ELEGIVEIS POR COND NO DOM  
    pns2013[n_elegivel!=0 ,.N, by=c004][order(c004)]   
      
  # Dummy de solteiro = quem e solteiro e nao mora com conjuge ou, pergunta nao se aplica por conta da idade.
    pns2013[c010==2 & c011==5, solteiro:=1]
    pns2013[is.na(c011), solteiro:=1]
    pns2013[is.na(solteiro), solteiro:=0]
      pns2013[n_elegivel!=0, .N, by=solteiro]

  # Gera objeto contendo conviventes do pre_elegivel
    pns2013[, fam_elegivel:=sum((pre_elegivel1==1), na.rm = T), by=domid]
    pns2013_elegivel<-pns2013[fam_elegivel!=0]
      pns2013_elegivel[pre_elegivel1==1, .N]
    
    
    
    
    
# duplica familias que tenham mais de um pre_elegivel ---------------------
    pns2013_elegivel[, .N, by=n_elegivel]
      
    # marca Especie do beneficio (idoso ou deficiencia) especie=1 (idoso) especie=2 (deficiencia)
      pns2013_elegivel[pre_elegivel1==1 & pcad_moderado==1, especie:=2]
      pns2013_elegivel[pre_elegivel1==1 & idoso==1        , especie:=1]
      pns2013_elegivel[, .N, by=especie]
      
    # Replica familia com 2 ou mais beneficiarios
      
      pns2013_elegivel  <- pns2013_elegivel %>%  mutate( unique_id = row_number())
      
      ben <- pns2013_elegivel %>% filter(pre_elegivel1 == 1) %>% 
        mutate(domid2 = unique_id)   %>% 
        select(domid2, domid)
      
      pns2013_elegivel  <- merge(ben, pns2013_elegivel, by = "domid") %>% 
        mutate(pre_elegivel1_2 = as.numeric(unique_id == domid2)) %>% 
        select(-unique_id)
      
      setDT(pns2013_elegivel)
      
    #verifica n benef
      pns2013_elegivel[, n_elegivel2:=sum(pre_elegivel1_2, na.rm = T), by="domid2"]
      pns2013_elegivel[,.N, by=n_elegivel2]
      
    # renomeia para nomes antigos e variaveis com duplicidade de benef = *_dpl
      setnames(pns2013_elegivel, "domid" , "domid_dpl")
      setnames(pns2013_elegivel, "domid2", "domid")
      
      setnames(pns2013_elegivel, "pre_elegivel1" , "pre_elegivel1_dpl")
      setnames(pns2013_elegivel, "pre_elegivel1_2", "pre_elegivel1")
      
     # Replica codigo de parentesco do beneficiario para toda familia
      pns2013_elegivel[pre_elegivel1==1, c004_pre_elegivel:=c004]
      pns2013_elegivel[,c004_pre_elegivel:=max(c004_pre_elegivel, na.rm = TRUE), , by=domid]
      
    # divide pesos pelo numero de replica??o da familia
      pns2013_elegivel[, peso:= v00281/n_elegivel]
    
    # marca Especie do beneficio (idoso ou deficiencia) especie=1 (idoso) especie=2 (deficiencia)
      pns2013_elegivel[pre_elegivel1==1 & pcad_moderado==1, especie_fam:=2]
      pns2013_elegivel[pre_elegivel1==1 & idoso==1        , especie_fam:=1]
        pns2013_elegivel[, .N, by=especie_fam]
        
        pns2013_elegivel[, especie_fam:=sum(especie_fam, na.rm = T), by=domid]
          pns2013_elegivel[, .N, by=especie_fam]
          

# Reclassifica familia do beneficiario de acordo com regras BPC -----------
      # Corrige erro cgenro ou nora  solteiro (um caso)
        pns2013_elegivel[c004==7, solteiro:=0]
          
      # aplica tabela    
      pns2013_elegivel[,pre_elegivel:=pre_elegivel1]
      results<- tab[pns2013_elegivel, on=.(c004_pre_elegivel, pre_elegivel, solteiro, c004)]
        results[, .N, by=vinc_fam_BPC]
        
    # ajusta classifica??o se ? pre_elegivel -> ? o proprio
        results[pre_elegivel==1, vinc_fam_BPC:="O PROPRIO"]
  
  # investiga ambiguidades
    results[vinc_fam_BPC=="AMBIGUO", .(total=sum(v00281)),  by=.(c004_original,c004_pre_elegivel)][order(-total)] %>% View

        
    
    
# Descritivas com pesos ---------------------------------------------------
    
  # Parametros de divisao
    populacao<-200573507
    n.idoso<-      as.numeric(round(pns2013[      idoso==1, .(total=sum(v00281))]))
    n.deficiencia<-as.numeric(round(pns2013[deficiencia==1, .(total=sum(v00281))]))
    
    preeleito<-              as.numeric(round(pns2013_elegivel[pre_elegivel1==1, .(total=sum(peso))]))
    n.idoso_preeleita<-      as.numeric(round(pns2013_elegivel[pre_elegivel1==1 & especie==1, .(total=sum(peso))]))
    n.deficiencia_preeleita<-as.numeric(round(pns2013_elegivel[pre_elegivel1==1 & especie==2 & deficiencia==1, .(total=sum(peso))]))
    
    
  # populacao idosa e deficiencia
    pns2013[idoso==1, .(total=sum(v00281))]
    pns2013[, .(Propor??o=sum(v00281)/populacao ), by=idoso]
    
    pns2013[, .(total=sum(v00281)), by=deficiencia]
    pns2013[, .(propor??o=sum(v00281)/populacao), by=deficiencia]

  # distribui??o percentual da popula??o idosa e deficiente por cond no dom
    distr_cond_dom<-pns2013[idoso==1, .(Idoso=round((sum(v00281)/n.idoso)*100,2)), by=c004_original][order(c004_original)]
    aux<-pns2013[deficiencia==1, .(Deficiencia=round((sum(v00281)/n.deficiencia)*100,2)), by=c004_original][order(c004_original)] 
    
    distr_cond_dom<-merge(aux, distr_cond_dom, by="c004_original", all = T)
    
  
  # Popula??o elegivel total e por especie de beneficio
    pns2013_elegivel[pre_elegivel1==1, .(sum(peso)/preeleito), by=especie] 
    results[pre_elegivel==1, .(Total= sum(peso)), by=especie] %>% View
    
  # distribui??o percentual da popula??o idosa e deficiente por cond no dom PREELEITA
    distr_cond_dom<-pns2013_elegivel[pre_elegivel1==1 & especie==1, .(Idoso      =round((sum(peso)/n.idoso_preeleita)*      100,2)), by=c004][order(c004)]
    aux<-           pns2013_elegivel[pre_elegivel1==1 & especie==2, .(Deficiencia=round((sum(peso)/n.deficiencia_preeleita)*100,2)), by=c004][order(c004)] 
    tot<-           pns2013_elegivel[pre_elegivel1==1             , .(Total      =round((sum(peso)/preeleito)*100,2)), by=c004][order(c004)] 
    distr_cond_dom<-merge(aux, distr_cond_dom, by="c004", all = T)
    distr_cond_dom<-merge(distr_cond_dom, tot, by="c004", all = T)
    
################ Resultado das reclassificoes com pesos
    
  # total por tipo de reclassificacao 
    results[, .(total=round(sum(peso))), by=.(vinc_fam_BPC, especie_fam)][order(especie_fam)] %>% View
    results[, .(total=round(sum(peso)))]
    
  # media de pessoas por familia BPC
    media_fam<-results[vinc_fam_BPC!="AMBIGUO" & vinc_fam_BPC!="N?O ENTRA"]
    media_fam[, pessoas:=1]
    media_fam<-media_fam[, .(tot_pes=sum(pessoas), peso=peso), by=domid]
    media_fam[, .(total=round(sum(peso))), by=tot_pes] %>% View
    
  ########## renda per capita
    # pessoas que recebem alguma renda de "outras rendas"
    results[, .(Total=round(sum(peso))), by=vdf00102]
    
    
    
# Calculo de renda per capita ---------------------------------------------
    sm<-678
    sm_1_4<-sm/4
    sm_1_2<-sm/2
    
    
  # calculo renda total e limpa rendas e outras rendas E01602= trabalho principal E01804= outros trabalhos
    results[, .N, by=vdf00102] # nao tem nao declarado 999999999999
    results[, .N, by=e01602] #5867 na trabalho principal
    results[, .N, by=e01804] #7124 na outros trabalhois
    
    results[is.na(vdf00102), vdf00102:=0] 
    results[is.na(e01602), e01602:=0] 
    results[is.na(e01804), e01804:=0] 
    
    # cria rend_trabalhos soma de renda de todos trabalhos
    results[, rend_trabalhos:= e01602+e01804]
    
  
  # calculo BOLSA FAMILIA
    # composicao de criancas na familia
      results[, temp_cri1:= between(idade, 0, 6)]
      results[, temp_cri2:= between(idade, 7, 15)]
      results[, temp_jov:=  between(idade, 16, 17)]
    
    # quantidade dessas composicoes por domicilio
      results[, ncri0a6 := sum(temp_cri1, na.rm = T), by=domid]
      results[, ncri7a15:= sum(temp_cri2, na.rm = T), by=domid]
      results[, ncri0a15:= ncri0a6+ncri7a15, by=domid]
      results[, njov16e17:= sum(temp_jov, na.rm = T), by=domid]
      
      # Renda dp PBF
      results[, ndom:= .N, by=domid]
      results[, rpbf:= ifelse(!is.na(vdf00102),1,0)]
      results[, rpbf:= ifelse(!is.na(vdf00102) & vdf00102>=30 & vdf00102<=min(70+min(ncri0a15*32,160)+min(njov16e17*38,76),310),vdf00102,0)]
      results[, rpbf:= ifelse(vdf00102>=70 & vdf00102<=ndom*70,vdf00102,rpbf)]
      
      # retira renda do PBF da variavel de outras rendas
      results[, vdf00102:= vdf00102-rpbf]
    
  # calcula as possiveis pessoas que recebem BPC, 1sm
    results[pre_elegivel1_dpl==1 & vdf00102==sm, rbpc1:=1]
      results[, .N, by=rbpc1]
    results[rbpc1==1, rbpc:=vdf00102]
    results[is.na(rbpc1), rbpc:=0]
    results[, vdf00102:=vdf00102-rbpc]
    
    # zera renda do BPC para pre elegiveis
      results[pre_elegivel==1, rbpc:=0]
      results[especie_fam==1 & especie==1, rbpc:=0]
      
    
  # deixa apenas GRUFAM
    results<-results[vinc_fam_BPC!="NÃO ENTRA" & vinc_fam_BPC!="AMBIGUO"]
    
  # Calcula renda percapita
    results[, ndom_bpc:= .N, by=domid]
      results[, .N, by=ndom_bpc]
    
    # renda total
      results[, rend_tot:= rend_trabalhos+vdf00102]  
      results[, rend_tot_fam:= sum(rend_tot, na.rm = T), by=domid]
      results[, rbpc_fam:= sum(rbpc, na.rm = T), by=domid]
      results[, rend_tot_fam_bpc:= renda_tot_fam+rbpc_fam]
        
    # renda per capita
      results[, rend_per:= (rend_tot_fam_bpc/ndom_bpc) ]
        results[pre_elegivel==1, .N, by=rend_per]
    
    # marca estrato de rendas per capita
      results[rend_per<=sm_1_4, per_1_4:=1]
      results[rend_per>=sm_1_4 & rend_per<=sm_1_2, per_1_2:=1]
        results[, .N, by=per_1_2]
        results[, .N, by=per_1_4]
          