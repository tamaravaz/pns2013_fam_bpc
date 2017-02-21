#---------------------------#
# Elegiveis BPC na PNS 2013 #
#---------------------------#

# Variaveis disponiveis
# C00301  - Nº ordem do morador
# C004    - cond dom
# C010    - vive com conjuge
# C011    - Estado Civil
# casado nao entra os pais
# juntar conjuge de mesmo sexo e sexo distinto


# Abre e instala pacotes necessarios e puxa base de dados  --------------------------------------
  #install.packages("devtools")
  #devtools::install_github("nicolassoarespinto/microdadosBrasil")
  #install.packages("survey")
  #install.packages("dplyr")
  library('microdadosBrasil')
  library(dplyr)
  library(survey) 

 # baixa e importa base de dados
  download_sourceData("PNS", 2013, unzip = T)
  pns_pes2013 <- read_PNS("pessoas"  , 2013)
  
# Cria e renomeia variáveis -----------------------------------------------
  
  # gera chave para domicilio
    pns_pes2013 <- pns_pes2013 %>% mutate(domid = paste(UPA_PNS,V0006_PNS,sep =""))
  
  # tranforma em numeric var character de interesse e renomeia idade
    pns_pes2013 <- pns_pes2013 %>% mutate(C001 = as.numeric(C001),
                                          C008 = as.numeric(C008),
                                          C004 = as.numeric(C004),
                                          C006 = as.numeric(C006))
    pns_pes2013 <- pns_pes2013 %>% rename(idade= C008)
  
  # Gera indicador de deficiencia com algum grau de limitação (pcad); >=moderado (PCAD_moderado) e >=Intensamente (PCAD_intenso)
    pns_pes2013 <- pns_pes2013 %>%  mutate(pcad_algum   =ifelse(G004>=2| G009>=1 | G017>=1 | G026>=1,1,0),
                                           pcad_moderado=ifelse(G004>=3| G009>=3 | G017>=3 | G026>=3,1,0),
                                           pcad_intenso =ifelse(G004>=4| G009>=4 | G017>=4 | G026>=4,1,0))
  
  # junta conjuge de mesmo sexo e dif no cod(2) = conjuge
    pns_pes2013 <- pns_pes2013 %>% mutate(cond_dom = ifelse(cond_dom==3,2,cond_dom))
    
  # Marca idoso (idade>=65 e VDF00102==678) e pessoa com alguma deficiência com outras rendas=678
    pns_pes2013 <- pns_pes2013%>%mutate(idoso_sm        =ifelse(idade>=65        & VDF00102>=.98*678 & VDF00102<=1.02*678,1,0),
                                        pcad_algum_sm   =ifelse(pcad_algum   ==1 & VDF00102>=.98*678 & VDF00102<=1.02*678,1,0),
                                        pcad_moderado_sm=ifelse(pcad_moderado==1 & VDF00102>=.98*678 & VDF00102<=1.02*678,1,0),
                                        pcad_intenso_sm =ifelse(pcad_intenso ==1 & VDF00102>=.98*678 & VDF00102<=1.02*678,1,0))
    pns_pes2013 %>% summarise(count=sum(idoso_sm, na.rm=TRUE))
  
  # 
