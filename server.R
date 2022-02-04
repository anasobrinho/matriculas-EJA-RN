
server <- function(input, output) {
  
#_______________________________________ MODELO DOS DADOS ___________________________________________
  
  Modelo <- data.frame(Ano=c(rep(2019,10), rep(2020,10)), 
                       DIRED=c(rep("00001",2),rep("00014",6),"00001","00012",rep("00001",2),
                               rep("00014",6),"00001","00012"),
                       Municipio=rep(c("Natal","Natal","Riacho da Cruz","Serrinha dos Pintos","Martins",
                                       "Umarizal","Olho d'Água do Borges","Viçosa","Natal","Mossoró"),2),
                       Dep.Adm=rep(c("PRIVADA","PRIVADA","MUNICIPAL","MUNICIPAL","ESTADUAL","MUNICIPAL",
                                     "ESTADUAL","ESTADUAL","PRIVADA","PRIVADA"),2),
                       Codigo=rep(c(24074047,24084174,24019399,24023515,24023230,24023582,24023701,24024910,
                                    24085529,24072222),2),
                       Estabelecimento=rep(c("CENTRO AVANCADO DE ENSINO - CADE",
                                             "CADE - CENTRO AVANCADO DE ENSINO",
                                             "ESCOLA MUNICIPAL CAMILA DE LELIS",
                                             "ESCOLA MUNICIPAL EGIDIO FERNANDES DE SOUZA",
                                             "ESCOLA ESTADUAL ANTONIO JOAO DE QUEIROZ",
                                             "ESC MUN LEIS GOMES DE OLIVEIRA",
                                             "ESCOLA ESTADUAL 20 DE SETEMBRO",
                                             "EE PROF ANALIA COSTA",
                                             "ESCOLA POLITECNICA DO NORDESTE LTDA - EPP",
                                             "SESI ESCOLA MOSSORO"),2),
                       Zona=rep(c(rep("URBANA",3),"RURAL","RURAL",rep("URBANA",5)),2),
                       Matriculas=c(147,464,60,50,38,22,30,88,6,12,135,420,58,27,31,18,27,83,4,11),
                       Modalidade.Ensino=rep(c(rep("SemiPresencial",2),rep("Presencial",6),rep("EAD",2)),2),
                       Grau.Ensino=rep(c("Médio","Médio","Fundamental","Fundamental","Médio","Fundamental",
                                         "Médio","Fundamental","Médio","Médio"),2))
  
  
  output$downloadtable = downloadHandler(
    filename = function()
    {paste("Modelo", ".csv", sep = "")},
    content = function(file){
      #frame = read.table("sm15.txt")
      #tab()
      write.csv2(Modelo, file, row.names = FALSE)}
  )
  
  
  #________________________________________ Carregamento dos Dados _____________________________________
  output$TabelaDados <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dados <- read.csv(inFile$datapath, header = TRUE, sep = ";")
    print(dados[1:10,])
  })
  

  
  #________________________________________ Mapas _____________________________________
  output$mapRN <- renderTmap({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dados <- read.csv(inFile$datapath, header = TRUE, sep = ";")
    
    df <- dados %>% group_by(Ano,Municipio,DIRED) %>% mutate(cumsum = cumsum(Matriculas))
    
    df <- df %>%
      group_by(Ano,Municipio,DIRED) %>%
      summarise(Matriculas= max(cumsum))
    
    df <- as.data.frame(df)
    df$DIRED <- as.numeric(df$DIRED)
    df$Matriculas <- as.numeric(df$Matriculas)
    df$Municipio <- toupper(df$Municipio)
    
    #Geometria dos Municípios do RN
    RN <- get_brmap("City", geo.filter = list(State = 24))
    
    #juntando os dados
    Eja_RN <- merge(df, RN, by.x = "Municipio", by.y = "nome")
    
    #Exlcluindo algumas colunas afim de evitar repetições
    Eja_RN$State <- NULL
    Eja_RN$MicroRegion <- NULL
    Eja_RN$MesoRegion <- NULL
    Eja_RN$geometry <- NULL
    Eja_RN$Region <- NULL
    
    
    #transforma em arquivo SF
    rn_map <- get_brmap("City") %>% 
      inner_join(Eja_RN, c("City"))
    
    rn_map$nome <- NULL
    rn_map$City <- NULL
    rn_map$State <- NULL
    rn_map$MicroRegion <- NULL
    rn_map$MesoRegion <- NULL
    rn_map$Region <- NULL
    
    rn_vars <- setdiff(names(rn_map), c("Municipio", "geometry"))
    
    if("Todas as DIREDs" %in% input$listaDIRED){
      rn_map[rn_map$Ano==input$listaAno,]%>% 
        tm_shape() +
        tm_polygons(size = "Matriculas", title="Total de matrículas na EJA no RN",
                    col = rn_vars[3], zindex = 401, style="kmeans", text="Label_N",
                    palette= "viridis"(n=5, direction = -1,option = "D")) +
        tm_text("DIRED", scale=1, col = "black") +
        tm_legend(legend.format = list(text.separator= "-",
                                       fun=function(x) formatC(x, digits=0, format="d")))
    }else{
      rn_map[rn_map$Ano==input$listaAno & rn_map$DIRED==input$listaDIRED,]%>% 
        tm_shape() +
        tm_polygons(size = "Matriculas", title="Total de matrículas na EJA no RN",
                    col = rn_vars[3], zindex = 401, style="kmeans", text="Label_N",
                    palette= "viridis"(n=5, direction = -1,option = "D")) +
        tm_legend(legend.format = list(text.separator= "-",
                                       fun=function(x) formatC(x, digits=0, format="d"))) 
    }
    
    
  })
  
  
  
  #________________________________________ Gráficos _____________________________________
  plotInput = reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dados <- read.csv(inFile$datapath, header = TRUE, sep = ";")
    
    
    # Tabela do total geral
    total <- dados %>% 
      mutate(Ano = as.factor(Ano)) %>% 
      group_by(Ano) %>% 
      summarise(
        TotalMatriculas = sum(Matriculas, na.rm = T))
    
    
    # Tabela para o total de matrículas considerando a variável Dep.Adm
    total.rede <- dados %>% 
      mutate(Ano = as.factor(Ano),
             Dep.Adm = as.factor(Dep.Adm)) %>% 
      group_by(Ano, Dep.Adm) %>% 
      summarise(
        TotalMatriculas = sum(Matriculas, na.rm = T))
    
    
    # Tabela para o total de matrículas considerando a Dep.Adm e o grau de ensino
    total.rede.grau <- dados[dados$Grau.Ensino!=" ",] %>% 
      mutate(Ano = as.factor(Ano),
             Dep.Adm = as.factor(Dep.Adm),
             Grau.Ensino = as.factor(Grau.Ensino)) %>% 
      group_by(Ano, Dep.Adm, Grau.Ensino) %>% 
      summarise(
        TotalMatriculas = sum(Matriculas, na.rm = T))
    
    
    # Tabela para o total de matrículas considerando a variável zona
    total.zona <- dados %>% 
      mutate(Ano = as.factor(Ano),
             Zona = as.factor(Zona)) %>% 
      group_by(Ano, Zona) %>% 
      summarise(
        TotalMatriculas = sum(Matriculas, na.rm = T))
    
    
    # Tabela para o total de matrículas considerando as variáveis zona e Dep.Adm
    total.zona.rede <- dados %>% 
      mutate(Ano = as.factor(Ano),
             Dep.Adm = as.factor(Dep.Adm),
             Zona = as.factor(Zona)) %>% 
      group_by(Ano, Dep.Adm, Zona) %>% 
      summarise(
        TotalMatriculas = sum(Matriculas, na.rm = T))
    
    
    # Tabela considerando o Grau de Ensino e a Zona
    total.zona.grau <- dados %>% 
      mutate(Ano = as.factor(Ano),
             Grau.Ensino = as.factor(Grau.Ensino),
             Zona = as.factor(Zona)) %>% 
      group_by(Ano, Zona, Grau.Ensino) %>% 
      summarise(
        TotalMatriculas = sum(Matriculas, na.rm = T))
    
    
    # Tabela considerando o Grau de Ensino
    total.grauEnsino <- dados %>% 
      mutate(Ano = as.factor(Ano),
             Grau.Ensino = as.factor(Grau.Ensino)) %>% 
      group_by(Ano, Grau.Ensino) %>% 
      summarise(
        TotalMatriculas = sum(Matriculas, na.rm = T))
    
    
    # Tabela considerando a Dependência Administrativa, o Grau de Ensino e a Zona
    total.rede.grau.zona <- dados %>% 
      mutate(Ano = as.factor(Ano),
             Dep.Adm = as.factor(Dep.Adm),
             Grau.Ensino = as.factor(Grau.Ensino),
             Zona = as.factor(Zona)) %>% 
      group_by(Ano, Dep.Adm, Grau.Ensino, Zona) %>% 
      summarise(
        TotalMatriculas = sum(Matriculas, na.rm = T))
    
    
    
    if("Todas" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico1 = ggplot(data = total) + 
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico1)
        }}}
    
    
    if("Municipal" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico2 = ggplot(data = total.rede[total.rede$Dep.Adm=="MUNICIPAL",], aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Municipal de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico2)
        }}}
    
    if("Estadual" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico3 = ggplot(data = total.rede[total.rede$Dep.Adm=="ESTADUAL",], aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Estadual de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico3)
        }}}
    
    if("Federal" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico4 = ggplot(data = total.rede[total.rede$Dep.Adm=="FEDERAL",], aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Federal de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico4)
        }}}
    
    if("Privada" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico5 = ggplot(data = total.rede[total.rede$Dep.Adm=="PRIVADA",], aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Privada de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico5)
        }}}
    
    
    
    if("Municipal" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico6 = ggplot(data = total.rede.grau[total.rede.grau$Grau.Ensino=="Fundamental" & 
                                                     total.rede.grau$Dep.Adm=="MUNICIPAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Municipal de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico6)
        }}}
    
    if("Estadual" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico7 = ggplot(data = total.rede.grau[total.rede.grau$Grau.Ensino=="Fundamental" & 
                                                     total.rede.grau$Dep.Adm=="ESTADUAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Estadual de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico7)
        }}}
    
    if("Federal" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico8 = ggplot(data = total.rede.grau[total.rede.grau$Grau.Ensino=="Fundamental" & 
                                                     total.rede.grau$Dep.Adm=="FEDERAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Federal de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico8)
        }}}
    
    if("Privada" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico9 = ggplot(data = total.rede.grau[total.rede.grau$Grau.Ensino=="Fundamental" & 
                                                     total.rede.grau$Dep.Adm=="PRIVADA",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Privada de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico9)
        }}}
    
    
    if("Municipal" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico10 = ggplot(data = total.rede.grau[total.rede.grau$Grau.Ensino=="Médio" & 
                                                      total.rede.grau$Dep.Adm=="Municipal",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Municipal de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico10)
        }}}
    
    if("Estadual" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico11 = ggplot(data = total.rede.grau[total.rede.grau$Grau.Ensino=="Médio" & 
                                                      total.rede.grau$Dep.Adm=="ESTADUAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Estadual de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico11)
        }}}
    
    if("Federal" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico12 = ggplot(data = total.rede.grau[total.rede.grau$Grau.Ensino=="Médio" & 
                                                      total.rede.grau$Dep.Adm=="FEDERAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Federal de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico12)
        }}}
    
    if("Privada" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico13 = ggplot(data = total.rede.grau[total.rede.grau$Grau.Ensino=="Médio" & 
                                                      total.rede.grau$Dep.Adm=="PRIVADA",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Privada de Ensino") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico13)
        }}}
    
    
    
    if("Todas" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico14 = ggplot(data = total.zona[total.zona$Zona=="RURAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico14)
        }}}
    
    if("Todas" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico15 = ggplot(data = total.zona[total.zona$Zona=="URBANA",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico15)
        }}}
    
    
    if("Municipal" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico16 = ggplot(data = total.zona.rede[total.zona.rede$Zona=="RURAL" &
                                                      total.zona.rede$Dep.Adm=="MUNICIPAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Municipal de Ensino na Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico16)
        }}}
    
    if("Estadual" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico17 = ggplot(data = total.zona.rede[total.zona.rede$Zona=="RURAL" &
                                                      total.zona.rede$Dep.Adm=="ESTADUAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Estadual de Ensino na Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico17)
        }}}
    
    if("Federal" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico18 = ggplot(data = total.zona.rede[total.zona.rede$Zona=="RURAL" &
                                                      total.zona.rede$Dep.Adm=="FEDERAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Federal de Ensino na Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico18)
        }}}
    
    if("Privada" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico19 = ggplot(data = total.zona.rede[total.zona.rede$Zona=="RURAL" &
                                                      total.zona.rede$Dep.Adm=="PRIVADA",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Privada de Ensino na Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico19)
        }}}
    
    
    
    if("Municipal" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico20 = ggplot(data = total.zona.rede[total.zona.rede$Zona=="URBANA" &
                                                      total.zona.rede$Dep.Adm=="MUNICIPAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Municipal de Ensino na Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico20)
        }}}
    
    if("Estadual" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico21 = ggplot(data = total.zona.rede[total.zona.rede$Zona=="URBANA" &
                                                      total.zona.rede$Dep.Adm=="ESTADUAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Estadual de Ensino na Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico21)
        }}}
    
    if("Federal" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico22 = ggplot(data = total.zona.rede[total.zona.rede$Zona=="URBANA" &
                                                      total.zona.rede$Dep.Adm=="FEDERAL",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Federal de Ensino na Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico22)
        }}}
    
    if("Privada" %in% input$depAdm){
      if("Todos" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico23 = ggplot(data = total.zona.rede[total.zona.rede$Zona=="URBANA" &
                                                      total.zona.rede$Dep.Adm=="PRIVADA",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Rede Privada de Ensino na Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico23)
        }}}
    
    
    
    if("Todas" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico24 = ggplot(data = total.zona.grau[total.zona.grau$Zona=="RURAL" &
                                                      total.zona.grau$Grau.Ensino=="Fundamental",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Ensino Fundamental na Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico24)
        }}}
    
    if("Todas" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico25 = ggplot(data = total.zona.grau[total.zona.grau$Zona=="RURAL" &
                                                      total.zona.grau$Grau.Ensino=="Médio",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Ensino Médio na Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico25)
        }}}
    
    if("Todas" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico26 = ggplot(data = total.zona.grau[total.zona.grau$Zona=="URBANA" &
                                                      total.zona.grau$Grau.Ensino=="Fundamental",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Ensino Fundamental na Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico26)
        }}}
    
    if("Todas" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico27 = ggplot(data = total.zona.grau[total.zona.grau$Zona=="URBANA" &
                                                      total.zona.grau$Grau.Ensino=="Médio",]) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Ensino Médio na Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico27)
        }}}
    
    
    
    if("Todas" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico28 = ggplot(data = total.grauEnsino[total.grauEnsino$Grau.Ensino=="Fundamental",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Ensino Fundamental") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico28)
        }}}
    
    if("Todas" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Todas" %in% input$zona){
          grafico29 = ggplot(data = total.grauEnsino[total.grauEnsino$Grau.Ensino=="Médio",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Ensino Médio") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico29)
        }}}
    
    
    
    
    if("Municipal" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico30 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="MUNICIPAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Fundamental"& 
                                                           total.rede.grau.zona$Zona=="RURAL",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Municipal de Ensino - Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico30)
        }}}
    
    
    if("Municipal" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico31 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="MUNICIPAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Fundamental"& 
                                                           total.rede.grau.zona$Zona=="URBANA",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Municipal de Ensino - Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico31)
        }}}
    
    
    
    if("Municipal" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico32 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="MUNICIPAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Médio"& 
                                                           total.rede.grau.zona$Zona=="RURAL",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Municipal de Ensino - Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico32)
        }}}
    
    
    if("Municipal" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico33 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="MUNICIPAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Médio"& 
                                                           total.rede.grau.zona$Zona=="URBANA",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Municipal de Ensino - Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico33)
        }}}
    
    
    
    
    
    if("Estadual" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico34 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="ESTADUAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Fundamental"& 
                                                           total.rede.grau.zona$Zona=="RURAL",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Estadual de Ensino - Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico34)
        }}}
    
    
    if("Estadual" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico35 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="ESTADUAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Fundamental"& 
                                                           total.rede.grau.zona$Zona=="URBANA",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Estadual de Ensino - Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico35)
        }}}
    
    
    
    if("Estadual" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico36 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="ESTADUAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Médio"& 
                                                           total.rede.grau.zona$Zona=="RURAL",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Estadual de Ensino - Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico36)
        }}}
    
    
    if("Estadual" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico37 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="ESTADUAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Médio"& 
                                                           total.rede.grau.zona$Zona=="URBANA",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Estadual de Ensino - Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico37)
        }}}
    
    
    
    
    
    
    if("Federal" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico38 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="FEDERAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Fundamental"& 
                                                           total.rede.grau.zona$Zona=="RURAL",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Federal de Ensino - Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico38)
        }}}
    
    
    if("Federal" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico39 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="FEDERAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Fundamental"& 
                                                           total.rede.grau.zona$Zona=="URBANA",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Federal de Ensino - Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico39)
        }}}
    
    
    
    if("Federal" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico40 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="FEDERAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Médio"& 
                                                           total.rede.grau.zona$Zona=="RURAL",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Federal de Ensino - Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico40)
        }}}
    
    
    if("Federal" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico41 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="FEDERAL"&
                                                           total.rede.grau.zona$Grau.Ensino=="Médio"& 
                                                           total.rede.grau.zona$Zona=="URBANA",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Federal de Ensino - Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico41)
        }}}
    
    
    
    
    
    if("Privada" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico42 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="PRIVADA"&
                                                           total.rede.grau.zona$Grau.Ensino=="Fundamental"& 
                                                           total.rede.grau.zona$Zona=="RURAL",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Privada de Ensino - Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico42)
        }}}
    
    
    if("Privada" %in% input$depAdm){
      if("Fundamental" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico43 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="PRIVADA"&
                                                           total.rede.grau.zona$Grau.Ensino=="Fundamental"& 
                                                           total.rede.grau.zona$Zona=="URBANA",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Fundamental na Rede Privada de Ensino - Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico43)
        }}}
    
    
    
    if("Privada" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Rural" %in% input$zona){
          grafico44 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="PRIVADA"&
                                                           total.rede.grau.zona$Grau.Ensino=="Médio"& 
                                                           total.rede.grau.zona$Zona=="RURAL",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Privada de Ensino - Zona Rural") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico44)
        }}}
    
    
    if("Privada" %in% input$depAdm){
      if("Médio" %in% input$grauEnsino){
        if("Urbana" %in% input$zona){
          grafico45 = ggplot(data = total.rede.grau.zona[total.rede.grau.zona$Dep.Adm=="PRIVADA"&
                                                           total.rede.grau.zona$Grau.Ensino=="Médio"& 
                                                           total.rede.grau.zona$Zona=="URBANA",], 
                             aes(x = Ano, y = TotalMatriculas)) +
            geom_point(aes(x = Ano, y = TotalMatriculas), color = "darkblue", size=2) +
            geom_line(aes(x = as.numeric(Ano), y = TotalMatriculas), color = "darkblue", size=1) +
            ylab("Total de matrículas") +
            ggtitle("Número de matrículas na Educação de Jovens e Adultos no RN") +
            labs(subtitle = "Médio na Rede Privada de Ensino - Zona Urbana") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 16),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=14,face="bold"))
          print(grafico45)
        }}}
  })
  
  
  output$plot <- renderPlot({
    print(plotInput())
  })
  
}


