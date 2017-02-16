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
  #devtools::install_github("nicolassoarespinto/microdadosBrasil")
  #install.packages("survey")
  #install.packages("dplyr")
  library('microdadosBrasil')
  library(dplyr)
  library(survey) 

  download_sourceData("PNS", 2013, unzip = T)
  pns_pes2013 <- read_PNS("pessoas"  , 2013)
  
# Cria e renomeia variáveis -----------------------------------------------
  # gera chave para domicilio
  pns_pes2013 <- pns_pes2013 %>% mutate(domid = paste(UPA_PNS,V0006_PNS,sep =""))
  
  # tranforma em numeric var character de interesse
  pns_pes2013 <- pns_pes2013 %>% mutate( C001    = as.numeric(C001),
                                         C008    = as.numeric(C008),
                                         C004    = as.numeric(C004),
                                         C006    = as.numeric(C006),)
  
  
  pns_pes2013 <- pns_pes2013 %>% rename(C00301= n0_morador
                                        C004  = cond_dom
                                        C010  = vive_conjuge
                                        C011  = st_civil
                                        C006  = sexo
                                        C008  = idade)
  
  
  

