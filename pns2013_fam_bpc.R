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
    pns2013[, .(Proporcao=sum(v00281)/populacao ), by=idoso]
    
    pns2013[, .(total=sum(v00281)), by=deficiencia]
    pns2013[, .(proporcao=sum(v00281)/populacao), by=deficiencia]

  # distribuicaoo percentual da populacaoo idosa e deficiente por cond no dom
    distr_cond_dom<-pns2013[idoso==1, .(Idoso=round((sum(v00281)/n.idoso)*100,2)), by=c004_original][order(c004_original)]
    aux<-pns2013[deficiencia==1, .(Deficiencia=round((sum(v00281)/n.deficiencia)*100,2)), by=c004_original][order(c004_original)] 
    
    distr_cond_dom<-merge(aux, distr_cond_dom, by="c004_original", all = T)
    
  
  # Populacaoo elegivel total e por especie de beneficio
    pns2013_elegivel[pre_elegivel1==1, .(sum(peso)/preeleito), by=especie] 
    results[pre_elegivel==1, .(Total= sum(peso)), by=especie] %>% View
    
  # distribuicaoo percentual da popula??o idosa e deficiente por cond no dom PREELEITA
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
    
    # zera renda do BPC para pre elegiveis e idoso em dom de pre eleito idoso
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
      results[, rend_tot_fam_bpc:= rend_tot_fam+rbpc_fam]
        
    # renda per capita
      results[, rend_per:= (rend_tot_fam_bpc/ndom_bpc) ]
        results[pre_elegivel==1, .N, by=rend_per]
    
    # marca estrato de rendas per capita
      results[rend_per<sm_1_4, rend_percapita:=1]
      results[rend_per>=sm_1_4 & rend_per<=sm_1_2, rend_percapita:=2]
      results[, .N, by=.(rend_percapita, especie_fam)] %>%  View
        
          

# Descritivas de renda per capita com pesos -------------------------------

  # proporção em cada extrato by especie fam
      results[, .(Total=sum(peso)), by=.(rend_percapita, especie_fam)][order(especie_fam)]
      
      
      

# Cria indice multidimensional de pobreza 1=sim e 0=nao ---------------------------------

  # Indicador de Vulnerabilidade
    # fecundidade
      # A1 = mulher gravida no domicilio gravida P005=1
        results[, A1:= ifelse(p005==1, 1, 0)][is.na(p005), A1:=0][, A1:=max(A1, na.rm = T), by=domid]
    
    # Atenção e cuidados especiais com crianças e adolecentes 
      # A2 = presença de criança
        results[, A2:= ifelse(idade>=0 & idade<=6, 1,0)][, A2:=max(A2, na.rm = T), by=domid]
      # A3 = presença de criança ou adolescente
        results[, A3:= ifelse(idade>=0 & idade<=15, 1,0)][, A3:=max(A3, na.rm = T), by=domid]
    
    # Dependência demográfica
      # A4 = ausencia de conjuge
        results[, A4:= ifelse(c004!=2, 1, 0)][, A4:=min(A4, na.rm = T), by=domid]
      # A5 = Menos da metade dos membros encontram-se em idade ativa 15 a 64 anos de idade IBGE
        results[, ndom:= .N, by=domid]
        results[, A5:= ifelse(idade>=15 & idade<=64, 1,0)][, A5:=sum(A5, na.rm = T), by=domid]
        results[, A5:= A5/ndom ]
        results[, A5:= ifelse(A5<0.5, 1,0)]

      
  # Indicadores de acesso ao conhecimento
    # Analfabetismo
      # B1 = Presença de adulto analfabeto (sem instrucao)vdd004=1
        results[, B1:=ifelse(vdd004==1 & idade>=18, 1, 0)][, B1:=max(B1, na.rm = T), by=domid]
      # B2 = Presença de adulto analfabeto funcional (nao sabe ler)d001=2
        results[, B2:=ifelse(d001==2 & idade>=18, 1, 0)][, B2:=max(B2, na.rm = T), by=domid]
    # Escolaridade
      # B3 = Ausência de adulto com fundamental completo
        results[, B3:=ifelse(vdd004<3 & idade>=18, 1, 0)][, B3:=min(B3, na.rm = T), by=domid]
      # B4 = Ausência de adulto com ensino médio completo
        results[, B4:=ifelse(vdd004<5 & idade>=18, 1, 0)][, B4:=min(B4, na.rm = T), by=domid]
      # B5 = Ausência de adulto com alguma educação superior
        results[, B5:=ifelse(vdd004<7 & idade>=18, 1, 0)][, B5:=min(B5, na.rm = T), by=domid]
        
  # Indicadores de acesso ao trabalho
    # Disponibilidade de trabalho
      # C1 = Menos da metade dos membros em idade ativa encontram-se ocupados
        results[, ndom:= .N, by=domid]
        results[, C1:= ifelse(idade>=15 & idade<=64 & vde002==1, 1,0)][, C1:=sum(C1, na.rm = T), by=domid]
        results[, C1:= C1/ndom ]
        results[, C1:= ifelse(C1<0.5, 1,0)]
        
    # Qualidade do posto de trabalho
      # C2 = Ausência de ocupado no setor formal
        results[, C2:=ifelse(e014>=6,1,0)]
        results[is.na(e014), C2:=1]
        results[, C2:=min(C2), by=domid]
      # C3 = Ausência de ocupado em atividade não-agrícola 92 COD agrcola variavel e01501=1
        results[, C3:= ifelse(e01501>=1101 & e01501<=1999 , 1, 0)][is.na(e01501), C3:=1][, C3:=min(C3, na.rm = T), by=domid]
        
  # Indicadores de desenvolvimento
    # Acesso à escola
      # D1 = Presença de ao menos uma criança de 0-6 anos fora da escola
        results[idade>=0 & idade<=6 & d002==2, D1:=1]
        results[idade>=0 & idade<=6 & d002==1, D1:=0]
        results[is.na(D1), D1:=0]
        results[, D1:=max(D1, na.rm = T), by=domid]
        
      # D2 = Presença de ao menos uma criança de 7-14 anos fora da escola
        results[idade>=7 & idade<=15 & d002==2, D2:=1]
        results[idade>=7 & idade<=15 & d002==1, D2:=0]
        results[is.na(D2), D2:=0]
        results[, D2:=max(D2, na.rm = T), by=domid]
        
      # D3 = Presença de ao menos uma criança de 7-17 anos fora da escola
        results[idade>=7 & idade<=17 & d002==2, D3:=1]
        results[idade>=7 & idade<=17 & d002==1, D3:=0]
        results[is.na(D3), D3:=0]
        results[, D3:=max(D3, na.rm = T), by=domid]
        
    # Progresso escolar
      # D4 = Presença de ao menos um adolescente de 10 a 14 anos analfabeto
        results[, D4:=ifelse(vdd004==1 & idade>=10 & idade<=14, 1, 0)][, D4:=max(D4, na.rm = T), by=domid]
      # D5 = Presença de ao menos um jovem de 15 a 17 anos analfabeto
        results[, D5:=ifelse(vdd004==1 & idade>=15 & idade<=17, 1, 0)][, D5:=max(D5, na.rm = T), by=domid]
      # D6 = Presença de ao menos uma mãe que tenha algum filho que já tenha morrido
        results[, D6:=ifelse(r046>=1, 1, 0)][is.na(r046), D6:=0][, D6:=max(D6, na.rm = T), by=domid]
      # D7 = Presença de mãe que já teve algum filho nascido morto r043= partos e r045 filhos vivos
        results[, filhos_mortos:=r043-r045]
        results[, D7:=ifelse(filhos_mortos>0, 1, 0)][is.na(filhos_mortos), D7:=0][, D7:=max(D7, na.rm = T), by=domid]
 
  
   # Indicador de acesso e saúde
    # Acesso à plano de saúde
        # F1 = Ausencia de moradores com plano de saúde i001
        results[, F1:= ifelse(i001==2, 1,0)][, F1:=min(F1, na.rm = T), by=domid]
    # Estado de saúde
      # F2 = Presença de ao menos um morador com doença crônica j007
        results[, F2:= ifelse(j007==1, 1,0)][, F2:=max(F2, na.rm = T), by=domid]
      # F3 = Presença de morador que classifica sua saúde como ruim ou muito ruim
        results[, F3:=ifelse(n001>=4, 1, 0)]
        results[is.na(n001), F3:=0]
        results[, F3:=max(F3, na.rm = T), by=domid]
        
        
        
  # CARÊNCIAS HABITACIONAIS
    # Déficit habitacional 
      # E1 =	Densidade de 2 ou mais moradores por dormitório A011= n de dormitorios
        results[, E1:= ndom/A011]
        results[, E1:= ifelse(E1>=2, 1, 0)][, E1:= max(E1, na.rm = T), by=domid]
    # Abrigabilidade
      # E2 = Material de construção não é permanente A002>=4
        results[, E2:= ifelse(A002>=4, 1, 0)][, E2:=max(E2, na.rm = T), by=domid]
    # Acesso a abastecimento de água A005
      # E3 = Acesso inadequado a água
        results[, E3:= ifelse(A005>=4, 1, 0)][, E3:=max(E3, na.rm = T), by=domid]
        
    # Acesso a saneamento
      # E4 = Esgotamento sanitário inadequado
        results[, E4:= ifelse(A015>=3, 1, 0)]
        results[is.na(E4), E4:=1]
        results[, E4:=max(E4, na.rm = T), by=domid]
    # Acesso a coleta de lixo 
      # E5 = Lixo não é coletado A016
        results[, E5:= ifelse(A016>=3, 1, 0)][, E5:=max(E5, na.rm = T), by=domid]
    # Acesso a energia elétrica
      # E6 = Sem acesso a eletricidade A017
        results[, E6:= ifelse(A017==3, 1, 0)][, E6:=max(E6, na.rm = T), by=domid]
      # E7 = Não tem ao menos a um dos itens: fogão ou geladeira A013=fogao A01803 = geladeira
        results[, E7:= ifelse(A013==3 | A013==4 | A013==6 | A013==7 | A01803==2, 1, 0)][, E7:=max(E7, na.rm = T), by=domid]
      # E8 = Não tem ao menos a um dos itens: fogão, geladeira, televisão A01801= televisao a cores
        results[, E8:= ifelse(A013==3 | A013==4 | A013==6 | A013==7 | A01803==2 | A01801==2, 1, 0)][, E8:=max(E8, na.rm = T), by=domid]
      # E9 = Não tem ao menos a um dos itens: fogão, geladeira, televisão, telefone (fixo ou celular) A01809=linha fixa A01811=celular
        results[, E9:= ifelse(A013==3 | A013==4 | A013==6 | A013==7 | A01803==2 | A01801==2 | (A01809==2 & A01811==2), 1, 0)][, E9:=max(E9, na.rm = T), by=domid]
      # E9 = Não tem ao menos a um dos itens: fogão, geladeira, televisão, telefone (fixo ou celular) ou COMPUTADOR # COMPUTADOR=A01815
        results[, E10:= ifelse(A013==3 | A013==4 | A013==6 | A013==7 | A01803==2 | A01801==2 | (A01809==2 & A01811==2) | A01815==2, 1, 0)][, E10:=max(E10, na.rm = T), by=domid]
       
        
        
indicadores<- results[!is.na(rend_percapita) & pre_elegivel==1, .(A1,A2,A3,A4,A5,B1, B2, B3, B4, B5, C1, C2, C3, D1, D2, D3,
                                                        D4,D5,D6,D7,F1,F2,F3,E1,E2,E3,E4,E5,E6,E7,E8,E9,E10, rend_percapita, rend_per, domid, peso, especie_fam)]


# indicador sinteticos de cada componente ------------------------------------------------------

  # vulnerabilidade
    # Fecundidade A1
      indicadores[, d1.1:=rowMeans(indicadores[,c("A1")])]
    # Atenção e cuidados especiais com crianças e adolescentes A2-A3
      indicadores[, d1.2:=rowMeans(indicadores[,c("A2", "A3")])]
    # Dependência demográfica A4-A5
      indicadores[, d1.3:=rowMeans(indicadores[,c("A4", "A5")])]

  # Indicadores de acesso ao conhecimento
    # Analfabetismo B1-B2
      indicadores[, d2.1:=rowMeans(indicadores[,c("B1", "B2")])]
    # Escolaridade B3-B5
      indicadores[, d2.2:=rowMeans(indicadores[,c("B3", "B4", "B5")])]

  # Indicadores de acesso ao trabalho
    # Disponibilidade de trabalho C1
      indicadores[, d3.1:=rowMeans(indicadores[,c("C1")])]
    # Qualidade do posto de trabalho C2-C3
      indicadores[, d3.2:=rowMeans(indicadores[,c("C2","C3")])]

  # Indicadores de desenvolvimento infantil 
    # Acesso à escola D1-D3
      indicadores[, d4.1:=rowMeans(indicadores[,c("D1","D2","D3")])]
    # Progresso escolar D4-D5
      indicadores[, d4.2:=rowMeans(indicadores[,c("D4","D5")])]
    # Mortalidade infantil D6-D7
      indicadores[, d4.3:=rowMeans(indicadores[,c("D6","D7")])]

  # CARÊNCIAS HABITACIONAIS
    # Déficit habitacional E1
      indicadores[, d5.1:=rowMeans(indicadores[,c("E1")])]
    # Abrigabilidade E2
      indicadores[, d5.2:=rowMeans(indicadores[,c("E2")])]
    # Acesso a abastecimento de água E3
      indicadores[, d5.3:=rowMeans(indicadores[,c("E3")])]
    # Acesso a saneamento E4
      indicadores[, d5.4:=rowMeans(indicadores[,c("E4")])]
    # Acesso a coleta de lixo E5
      indicadores[, d5.5:=rowMeans(indicadores[,c("E5")])]
    # Acesso a energia elétrica E6-E10
      indicadores[, d5.6:=rowMeans(indicadores[,c("E6", "E7", "E8", "E9", "E10")])]

  # Saude
    # Seguro saude F1
      indicadores[, d6.1:=rowMeans(indicadores[,c("F1")])]
    # Estado de saude F2-F3
      indicadores[, d6.2:=rowMeans(indicadores[,c("F2", "F3")])]
    

    

# indicador sintetico global ----------------------------------------------
  # vulnerabilidade
    indicadores[, d1:=rowMeans(indicadores[,c("d1.1","d1.2","d1.3")])]  
  # acesso ao conhecimento
    indicadores[, d2:=rowMeans(indicadores[,c("d2.1","d2.2")])]  
      
  # acesso ao trabalho
    indicadores[, d3:=rowMeans(indicadores[,c("d3.1","d3.2")])]  
      
  # desenvolvimento infantil
    indicadores[, d4:=rowMeans(indicadores[,c("d4.1","d4.2", "d4.3")])]  
      
  # CARÊNCIAS HABITACIONAIS
    indicadores[, d5:=rowMeans(indicadores[,c("d5.1","d5.2", "d5.3","d5.4","d5.5", "d5.6")])]  
      
  # Indicador saúde
    indicadores[, d6:=rowMeans(indicadores[,c("d6.1","d6.2")])]  

indicadores[, d1:=d1*100]
indicadores[, d2:=d2*100]
indicadores[, d3:=d3*100]
indicadores[, d4:=d4*100]
indicadores[, d5:=d5*100]
indicadores[, d6:=d6*100]
      
    
# indice geral 
indicadores[, indicador_geral:=rowMeans(indicadores[,c("d1","d2", "d3", "d4", "d5", "d6")])] 

#interaçao especie e renda percapita
indicadores[, rend_especie:=paste(as.character(rend_percapita),as.character(especie_fam),sep =".") ]

# indicadore geral por grupo e total
  # total
    weighted.mean(indicadores$indicador_geral,indicadores$peso)
  # Especie de beneficio
    indicador_geral_by_especie <- 
      indicadores %>% 
      group_by(especie_fam) %>% 
      summarise(weighted_income = weighted.mean(indicador_geral, peso))

  # Estrato de renda
    indicador_geral_by_renda <- 
      indicadores %>% 
      group_by(rend_percapita) %>% 
      summarise(weighted_income = weighted.mean(indicador_geral, peso))
    
  # Estrato de rendae especie
    indicador_geral_by_renda_especie <- 
      indicadores %>% 
      group_by(rend_especie) %>% 
      summarise(weighted_income = weighted.mean(indicador_geral, peso))

# dimensoes  por grupo e total
    # total
    weighted.mean(indicadores$d1,indicadores$peso)
    weighted.mean(indicadores$d2,indicadores$peso)
    weighted.mean(indicadores$d3,indicadores$peso)
    weighted.mean(indicadores$d4,indicadores$peso)
    weighted.mean(indicadores$d5,indicadores$peso)
    weighted.mean(indicadores$d6,indicadores$peso)
    
    # Estrato de renda
    indicadores %>% group_by(rend_percapita) %>% summarise(weighted_income = weighted.mean(d1, peso))
    indicadores %>% group_by(rend_percapita) %>% summarise(weighted_income = weighted.mean(d2, peso))
    indicadores %>% group_by(rend_percapita) %>% summarise(weighted_income = weighted.mean(d3, peso))
    indicadores %>% group_by(rend_percapita) %>% summarise(weighted_income = weighted.mean(d4, peso))
    indicadores %>% group_by(rend_percapita) %>% summarise(weighted_income = weighted.mean(d5, peso))
    indicadores %>% group_by(rend_percapita) %>% summarise(weighted_income = weighted.mean(d6, peso))
    
    # Estrato de renda e especie
      indicadores %>% group_by(rend_especie) %>% summarise(weighted_income = weighted.mean(d1, peso))  
      indicadores %>% group_by(rend_especie) %>% summarise(weighted_income = weighted.mean(d2, peso))  
      indicadores %>% group_by(rend_especie) %>% summarise(weighted_income = weighted.mean(d3, peso))  
      indicadores %>% group_by(rend_especie) %>% summarise(weighted_income = weighted.mean(d4, peso))  
      indicadores %>% group_by(rend_especie) %>% summarise(weighted_income = weighted.mean(d5, peso))  
      indicadores %>% group_by(rend_especie) %>% summarise(weighted_income = weighted.mean(d6, peso))  


write.table(indicadores[, .(total=sum(peso)), by=.(rend_percapita, especie_fam,indicador_geral,rend_especie)],
            "D:/Repositorios/pns2013_fam_bpc/bases_pns/tableu_indicadores_fam.csv", sep = ";", dec = ",")


# Criacao de prop dos indicadores das familias por metodo tosco ---------------
# refazer com function()

   E1[, vulnerabilidade:=rowMeans(E1[,c("e1", "renda_percapita")])]
  # 

# Proporcao de familias com vulnerabilidades no indicador (indicador=1) --------
total_familia_peso_renda1<-as.numeric(indicadores[especie_fam==1 & rend_percapita==1, .(total=round(sum(peso)))])
total_familia_peso_renda2<-as.numeric(indicadores[especie_fam==1 & rend_percapita==2, .(total=round(sum(peso)))])

A1<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(a1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A1][order(A1)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(a1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A1][order(A1)][2,-1])

A2<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(a2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A2][order(A2)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(a2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A2][order(A2)][2,-1])

A3<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(a3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A3][order(A3)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(a3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A3][order(A3)][2,-1])
        
A4<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(a4=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A4][order(A4)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(a4=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A4][order(A4)][2,-1])

A5<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(a5=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A5][order(A5)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(a5=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A5][order(A5)][2,-1])


B1<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(b1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B1][order(B1)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(b1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B1][order(B1)][2,-1])

B2<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(b2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B2][order(B2)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(b2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B2][order(B2)][2,-1])

B3<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(b3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B3][order(B3)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(b3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B3][order(B3)][2,-1])

B4<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(b4=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B4][order(B4)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(b4=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B4][order(B4)][2,-1])

B5<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(b5=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B5][order(B5)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(b5=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B5][order(B5)][2,-1])


C1<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(c1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=C1][order(C1)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(c1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=C1][order(C1)][2,-1])

C2<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(c2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=C2][order(C2)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(c2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=C2][order(C2)][2,-1])

C3<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(c3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=C3][order(C3)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(c3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=C3][order(C3)][2,-1])


D1<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(d1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D1][order(D1)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(d1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D1][order(D1)][2,-1])

D2<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(d2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D2][order(D2)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(d2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D2][order(D2)][2,-1])

D3<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(d3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D3][order(D3)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(d3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D3][order(D3)][2,-1])

D4<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(d4=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D4][order(D4)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(d4=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D4][order(D4)][2,-1])

D5<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(d5=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D5][order(D5)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(d5=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D5][order(D5)][2,-1])

D6<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(d6=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D6][order(D6)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(d6=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D6][order(D6)][2,-1])

D7<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(d7=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D7][order(D7)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(d7=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D7][order(D7)][2,-1])


E1<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E1][order(E1)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E1][order(E1)][2,-1])

E2<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E2][order(E2)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E2][order(E2)][2,-1])

E3<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E3][order(E3)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E3][order(E3)][2,-1])

E4<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e4=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E4][order(E4)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e4=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E4][order(E4)][2,-1])

E5<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e5=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E5][order(E5)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e5=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E5][order(E5)][2,-1])

E6<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e6=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E6][order(E6)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e6=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E6][order(E6)][2,-1])

E7<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e7=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E7][order(E7)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e7=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E7][order(E7)][2,-1])

E8<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e8=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E8][order(E8)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e8=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E8][order(E8)][2,-1])

E9<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e9=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E9][order(E9)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(e9=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E9][order(E9)][2,-1])

E10<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(e10=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E10][order(E10)][2,-1], 
           indicadores[especie_fam==1 & rend_percapita==2, .(e10=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E10][order(E10)][2,-1])



F1<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(F1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=F1][order(F1)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(F1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=F1][order(F1)][2,-1])

F2<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(F2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=F2][order(F2)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(F2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=F2][order(F2)][2,-1])

F3<-rbind(indicadores[especie_fam==1 & rend_percapita==1, .(F3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=F3][order(F3)][2,-1], 
          indicadores[especie_fam==1 & rend_percapita==2, .(F3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=F3][order(F3)][2,-1])

D1[1,d1:=0][1,renda_percapita:=1]
D1[2,d1:=0][2,renda_percapita:=2]
tableu_indicadores<-cbind(A1[,1],A2[,1], A3[,1], A4[,1], A5[,1], B1[,1], B2[,1], B3[,1], B4[,1], B5[,1], C1[,1], C2[,1], C3[,1],
                          D1[,1], D2[,1], D3[,1], D4[,1], D5[,1], D6[,1], D7[,1], E1[,1], E2[,1], E3[,1], E4[,1], E5[,1], E6[,1], E7[,1], 
                          E8[,1], E9[,1], E10[,1], F1[,1], F2[,1], F3)
options(OutDec= ",")
write.table(tableu_indicadores, "D:/Repositorios/pns2013_fam_bpc/bases_pns/tableu1.csv", sep = ";", dec = ",")

######


total_familia_peso_renda1<-as.numeric(indicadores[especie_fam==2 & rend_percapita==1, .(total=round(sum(peso)))])
total_familia_peso_renda2<-as.numeric(indicadores[especie_fam==2 & rend_percapita==2, .(total=round(sum(peso)))])

A1<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(a1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A1][order(A1)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(a1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A1][order(A1)][2,-1])

A2<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(a2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A2][order(A2)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(a2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A2][order(A2)][2,-1])

A3<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(a3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A3][order(A3)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(a3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A3][order(A3)][2,-1])

A4<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(a4=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A4][order(A4)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(a4=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A4][order(A4)][2,-1])

A5<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(a5=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=A5][order(A5)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(a5=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=A5][order(A5)][2,-1])


B1<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(b1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B1][order(B1)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(b1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B1][order(B1)][2,-1])

B2<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(b2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B2][order(B2)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(b2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B2][order(B2)][2,-1])

B3<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(b3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B3][order(B3)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(b3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B3][order(B3)][2,-1])

B4<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(b4=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B4][order(B4)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(b4=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B4][order(B4)][2,-1])

B5<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(b5=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=B5][order(B5)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(b5=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=B5][order(B5)][2,-1])


C1<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(c1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=C1][order(C1)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(c1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=C1][order(C1)][2,-1])

C2<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(c2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=C2][order(C2)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(c2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=C2][order(C2)][2,-1])

C3<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(c3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=C3][order(C3)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(c3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=C3][order(C3)][2,-1])


D1<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(d1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D1][order(D1)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(d1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D1][order(D1)][2,-1])

D2<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(d2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D2][order(D2)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(d2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D2][order(D2)][2,-1])

D3<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(d3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D3][order(D3)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(d3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D3][order(D3)][2,-1])

D4<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(d4=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D4][order(D4)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(d4=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D4][order(D4)][2,-1])

D5<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(d5=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D5][order(D5)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(d5=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D5][order(D5)][2,-1])

D6<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(d6=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D6][order(D6)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(d6=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D6][order(D6)][2,-1])

D7<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(d7=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=D7][order(D7)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(d7=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=D7][order(D7)][2,-1])


E1<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E1][order(E1)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E1][order(E1)][2,-1])

E2<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E2][order(E2)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E2][order(E2)][2,-1])

E3<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E3][order(E3)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E3][order(E3)][2,-1])

E4<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e4=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E4][order(E4)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e4=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E4][order(E4)][2,-1])

E5<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e5=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E5][order(E5)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e5=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E5][order(E5)][2,-1])

E6<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e6=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E6][order(E6)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e6=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E6][order(E6)][2,-1])

E7<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e7=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E7][order(E7)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e7=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E7][order(E7)][2,-1])

E8<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e8=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E8][order(E8)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e8=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E8][order(E8)][2,-1])

E9<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e9=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E9][order(E9)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(e9=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E9][order(E9)][2,-1])

E10<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(e10=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=E10][order(E10)][2,-1], 
           indicadores[especie_fam==2 & rend_percapita==2, .(e10=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=E10][order(E10)][2,-1])



F1<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(F1=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=F1][order(F1)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(F1=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=F1][order(F1)][2,-1])

F2<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(F2=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=F2][order(F2)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(F2=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=F2][order(F2)][2,-1])

F3<-rbind(indicadores[especie_fam==2 & rend_percapita==1, .(F3=round((sum(peso)/total_familia_peso_renda1)*100,2), renda_percapita=1), by=F3][order(F3)][2,-1], 
          indicadores[especie_fam==2 & rend_percapita==2, .(F3=round((sum(peso)/total_familia_peso_renda2)*100,2), renda_percapita=2), by=F3][order(F3)][2,-1])

D1[2,d1:=0][2,renda_percapita:=2]

tableu_indicadores<-cbind(A1[,1],A2[,1], A3[,1], A4[,1], A5[,1], B1[,1], B2[,1], B3[,1], B4[,1], B5[,1], C1[,1], C2[,1], C3[,1],
      D1[,1], D2[,1], D3[,1], D4[,1], D5[,1], D6[,1], D7[,1], E1[,1], E2[,1], E3[,1], E4[,1], E5[,1], E6[,1], E7[,1], 
      E8[,1], E9[,1], E10[,1], F1[,1], F2[,1], F3)
options(OutDec= ",")
write.table(tableu_indicadores, "D:/Repositorios/pns2013_fam_bpc/bases_pns/tableu2.csv", sep = ";", dec = ",")

write.table(indicadores[, .(total=sum(peso)), by=.(rend_per, rend_percapita, especie_fam)], "D:/Repositorios/pns2013_fam_bpc/bases_pns/tableu_indicadores_fam.csv", sep = ";", dec = ",")














