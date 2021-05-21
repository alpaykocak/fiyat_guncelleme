library(shiny)
library(CBRT)
library(tidyverse)
library(lubridate)
library(shinydashboard)
rm(list = ls())
grinfo <- showGroupInfo("bie_tukfiy4")
grinfo$seriesName[35] <- "09.NEWSPAPERS, BOOKS AND STATIONERY"
series <- grinfo$seriesCode
mySeries <- getDataSeries(series, CBRTKey = "JtHexIURwr")
colnames(mySeries)[-1] <- grinfo$seriesName
data <- as_tibble(mySeries)
trisim <- strsplit("0. GENEL; 01. GIDA VE ALKOLSÜZ İÇECEKLER; 011.GIDA; 012.ALKOLSÜZ İÇECEKLER; 02. ALKOLLÜ İÇECEKLER VE TÜTÜN; 021.ALKOLLÜ İÇECEKLER; 022.TOBACCO; 03.GİYİM VE AYAKKABI; 031. TOPLAMA; 032. GÖMLEK; 04.KONUT, SU, ELEKTRİK, GAZ VE DİĞER YAKITLAR; 041. KONUT İÇİN GERÇEK KİRALAMA; 043. KONUT BAKIM VE ONARIMI; 044. SU TEMİNİ VE KONUT İLE İLGİLİ MUHTELİF HİZMETLER; 045.ELEKTRİK, GAZ VE DİĞER YAKITLAR; 05. MOBİLYALAR, EV EKİPMANLARI, EVİN RUTİN BAKIMI; 051. MOBİLYA VE MOBİLYALAR, HALI VE DİĞER ZEMİN KAPLAMALARI; 052.SANAT TEKSTİLLERİ; 053.SANEHİR ALETLERİ; 054. CAM EŞYA, MASA Gereçleri ve EV Gereçleri; 055. EV VE BAHÇE ARAÇLARI VE EKİPMANLARI; 056. RUTİN HANEHALKI BAKIMINA YÖNELİK MALLAR VE HİZMETLER; 06. SAĞLIK; 061.MEDİKAL ÜRÜNLER, CİHAZLAR VE EKİPMANLAR; 062. HASTA HİZMETLERİ; 063.HASTANE HİZMETLERİ; 07. TAŞIMACILIK; 071. ARAÇ SATIN ALMA; 072. KİŞİSEL TAŞIMA EKİPMANLARININ ÇALIŞMASI; 073. TAŞIMACILIK HİZMETLERİ; 08. İLETİŞİM; 081. POSTA HİZMETLERİ; 082. TELEPHONE VE TELEFAX EKİPMANLARI; 083. TELEPHONE VE TELEFAX HİZMETLERİ; 09. GAZETELER, KİTAPLAR VE KIRTASİYE; 091. SESLİ-GÖRSEL, FOTOĞRAF VE BİLGİ İŞLEME EKİPMANLARI; 092. REKREASYON VE KÜLTÜR İÇİN DİĞER BÜYÜK DAYANIKLILIKLAR; 093. DİĞER DİNLENME MALZEMELERİ VE EKİPMANLARI, BAHÇELER VE EVCİL HAYVANLAR; 094.REKREASYONEL VE ​​KÜLTÜREL HİZMETLER; 095. GAZETELER, KİTAPLAR VE KIRTASİYE; 096. PAKET TATİL; 10.EĞİTİM; 101. İLKÖĞRETİM VE İLKÖĞRETİM; 102. İKİNCİ EĞİTİM; 103. ORTAÖĞRETİM ÜSSÜZ EĞİTİM SONRASI; 104.TERTIER EĞİTİM; 105. SEVİYE İLE TANIMLANAMAYAN EĞİTİM; 11.OTELLER, KAFELER VE RESTORANLAR; 111.CATERING HİZMETLERİ; 112. KONAKLAMA HİZMETLERİ; 12. MUHTELİF MALLAR VE HİZMETLER; 121. KİŞİSEL BAKIM; 123. KİŞİSEL ETKİLER N.E.C .; 124.SOSYAL KORUMA; 125. SİGORTA; 126. FİNANSAL HİZMETLER N.E.C .; 127. DİĞER HİZMETLER N.E.C.",";")
trisim <- trisim[[1]]
colnames(data)[-1] <- trisim
rm(mySeries,series,grinfo)
guncelleme <- function(x,sektor,from,to) {
    if (min(from,to) >= floor_date(head(data$time)[1],"month") & max(from,to) <=  (ceiling_date(tail(data$time)[6],"month")-1) ) {
        
        veri <- data.frame("time" = c( as.Date(paste0(year(as.Date(from)),
                                                      "-",
                                                      month(as.Date(from)),
                                                      "-15")),
                                       as.Date(paste0(year(as.Date(to)),
                                                      "-",
                                                      month(as.Date(to)),
                                                      "-15"))
        ), "sonuc" = c(x,NA)) %>% 
            left_join(by = "time",
                      data %>% filter(  
                          time %in% c( as.Date(paste0(year(as.Date(from)),
                                                      "-",
                                                      month(as.Date(from)),
                                                      "-15")),
                                       as.Date(paste0(year(as.Date(to)),
                                                      "-",
                                                      month(as.Date(to)),
                                                      "-15"))
                          )
                          
                      ) %>% select(time,sektor)
            )
        sonuc <- if (from >= as.Date("2005-01-05") & to < as.Date("2005-01-05")) {
            veri[2,3]*veri[1,2]/veri[1,3]*1000000 } else if  (from < as.Date("2005-01-05") & to >= as.Date("2005-01-05")) {
                veri[2,3]*veri[1,2]/(veri[1,3]*1000000)} else {veri[2,3]*veri[1,2]/veri[1,3]}
        sonuc <<-  format(sonuc, scientific = F, digits = 6)
        rapor <<- 
            paste0(sektor, 
                   " kalemindeki fiyat gelişmeleri dikkate alındığında, ", 
                   from, 
                   " tarihinde ", 
                   format(round(x, 1), scientific = F, nsmall=1, big.mark=","),
                   " Türk Lirası olan tutar ", 
                   to, 
                   " tarihinde ",
                   format(round(sonuc, 1), scientific = F, nsmall=1, big.mark=","),
                   " Türk Lirasına karşılık gelmektedir.")
    } else { 
        rapor <<- NULL
        sonuc <<-
            paste0("Tarihler ", 
                   floor_date(head(data$time)[1],"month"), 
                   " ile ", 
                   (ceiling_date(tail(data$time)[6],"month")-1), 
                   " arasında olmalıdır.")
    }
}

mean_data <- data %>% 
    mutate_if(is.numeric, function(x)(c(rep(NA,11),zoo::rollmean(x, 12)))) %>% 
    fill(where(is.numeric),.direction = "up") %>% select(time,"TUFE" = trisim[1]) 
kira_data <- as_tibble(getDataSeries("TP.TUFE1YI.T1", CBRTKey = "JtHexIURwr",freq = 5,startDate = "2003-01-15")) %>% 
    rename("YI_UFE" = TP.TUFE1YI.T1) %>% 
    right_join(mean_data,"time")
kira_guncelleme <- function(x,to) {
    to <- as.Date(to)
    d <- as.POSIXlt(to)
    d$year <- d$year-1
    l = as.POSIXlt(as.Date(to))
    l$year <- l$year+1
    from <- as.Date(l)
    back <- 
    if (to >= floor_date(head(kira_data$time)[1],"month") & to <=  (ceiling_date(tail(kira_data$time)[6],"month")-1) ) {
        
        veri <- data.frame("time" = c( as.Date(paste0(year(as.Date(from)),
                                                      "-",
                                                      month(as.Date(from)),
                                                      "-15")),
                                       as.Date(paste0(year(as.Date(to)),
                                                      "-",
                                                      month(as.Date(to)),
                                                      "-15"))
        ), "sonuc" = c(NA,x)) %>% 
            full_join(by = "time",
                      kira_data %>% filter(  
                          time %in% c( as.Date(paste0(year(as.Date(d)),
                                                      "-",
                                                      month(as.Date(d)),
                                                      "-15")),
                                       as.Date(paste0(year(as.Date(to)),
                                                      "-",
                                                      month(as.Date(to)),
                                                      "-15"))
                          )
                          
                      ) %>% select(time,everything()) %>%
                          gather("tip","deger",-time) %>% 
                          filter( tip == ifelse(to <= "2019-01-01","YI_UFE","TUFE") ) %>%
                          spread(tip,deger)
            )
        sonuc <- veri[2,3]/veri[3,3]*veri[2,2]
        sonuc <<-  format(sonuc, scientific = F, digits = 6)

        rapor_kira <<- 
            paste0(" Fiyat gelişmeleri dikkate alındığında, ", 
                   to, 
                   " tarihinde ", 
                   format(round(x, 1), scientific = F, nsmall=1, big.mark=","),
                   " Türk Lirası olan kira ", 
                   year(from), 
                   " yılının ",
                   month(from),
                   ". ayına kadar ",
                   format(round(sonuc-x,1), scientific = F, nsmall=1, big.mark=","),
                   " Türk Lirası artışla (%",
                  ifelse(sonuc == 0, "0",round((sonuc/x-1)*100,2) ) ,
                   " artış) birlikte ",
                  format(round(sonuc,1), scientific = F, nsmall=1, big.mark=","),
                   " Türk Lirası olarak uygulanmalıdır.")
    } else { 
      sonuc_kira <<-
        paste0("Tarihler ", 
               floor_date(head(kira_data$time)[1],"month"), 
               " ile ", 
               (ceiling_date(tail(kira_data$time)[6],"month")-1), 
               " arasında olmalıdır.")
        rapor_kira <<- sonuc_kira
        
    }
}




ui <- dashboardPage(
  dashboardHeader(title = "Fiyat Güncelleme Aracı",titleWidth = 500),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Neden Fiyat Güncelleme?", tabName = "definition", icon = icon("book-open")),
      menuItem("Fiyat Güncelleme", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Kira Güncelleme", tabName = "rawdata", icon = icon("th"))
    )  
  ),
  dashboardBody(
    tabItems(
      tabItem("definition",
              fluidRow(
                
                box(status = "info",width = 4,
                    h1("Fiyat Güncelleme Aracı"),
                    
                    p("")),
                
                box(status = "info",width = 4,
                    h1("Detaylı Açıklama"),
                    p(textOutput("exp"))), 
                
                box(status = "info",width = 4,
                    h1("Diğer"),
                    p("")
                )
              )
      ),
      tabItem("dashboard",
              fluidRow(
                selectInput(
                  'sektor', label = 'Sektörler',
                  choices = colnames(data)[-1],
                  selected = colnames(data)[1]
                ),
                dateInput(
                  inputId = "from",
                  label = "Şu tarihteki,",
                  value = floor_date(head(data$time)[1],"month"),
                  min = floor_date(head(data$time)[1],"month"),
                  max = ceiling_date(tail(data$time)[6],"month")-1,
                  format = "yyyy-mm-dd",
                  startview = "month",
                  weekstart = 0,
                  language = "tr",
                  width = NULL,
                  autoclose = TRUE,
                  datesdisabled = NULL,
                  daysofweekdisabled = NULL
                ),
                numericInput(
                  inputId = "x",
                  label = "Değeri",
                  value = 0,
                  min = 0,
                  max = NA,
                  step = NA,
                  width = NULL
                ),
                dateInput(
                  inputId = "to",
                  label = "Bu tarihteki değere dönüştür!",
                  value = ceiling_date(tail(data$time)[6],"month")-1,
                  min = floor_date(head(data$time)[1],"month"),
                  max = ceiling_date(tail(data$time)[6],"month")-1,
                  format = "yyyy-mm-dd",
                  startview = "month",
                  weekstart = 0,
                  language = "tr",
                  width = NULL,
                  autoclose = TRUE,
                  datesdisabled = NULL,
                  daysofweekdisabled = NULL
                ),
                actionButton(inputId = "basla",label = "Hesapla!"),
                h2(wellPanel(textOutput("rapor")))
      )),
      tabItem("rawdata", 
              fluidRow(width=10,
                       numericInput(
                         inputId = "x2",
                         label = "Kiram bu kadar...",
                         value = 0,
                         min = 0,
                         max = NA,
                         step = NA,
                         width = NULL
                       ),
                       dateInput(
                         inputId = "to2",
                         label = "Aşağıdaki tarih itibariyle kiraya artış yapılacak...",
                         value = ceiling_date(tail(data$time)[6],"month")-1,
                         min = floor_date(as.Date("2004-01-01"),"month"),
                         max = ceiling_date(tail(data$time)[6],"month")-1,
                         format = "yyyy-mm-dd",
                         startview = "month",
                         weekstart = 0,
                         language = "tr",
                         width = NULL,
                         autoclose = TRUE,
                         datesdisabled = NULL,
                         daysofweekdisabled = NULL
                       ),
                       actionButton(inputId = "basla2",label = "Hesapla!"),
                       h2(wellPanel(textOutput("rapor_kira")))
              )
              
      )
    )
  )
)

server <- function(input, output) {
    output$exp <- renderText(
        "Kira artış̧ oranında eski̇ Borçlar Kanunu’nda bir üst sınır bulunmamaktadır. Sözleşme serbestîsi̇ geçerlidir. Yeni̇ Borçlar Kanunu, bu konuda yasal bir üst sınır getirdi̇. Kira sözleşmelerinde kira bedeli̇ artışı “Üretici̇ Fiyat Endeksi̇” (ÜFE)’yı̇ geçmeyecek şekilde sınırlandırıldı. Bu madde konutlar için 01.07.2012 tarihinden itibaren yürürlüktedir. Bu madde uygulaması işyerleri için 01.07.2020 tarihine ertelenmiştir. İşyerleri için erteleme tarihine kadar sözleşme serbestisi ile kira artışı yapılabilecektir. Mevcut kira sözleşmelerinizde artış ile ilgili düzenlemeler 2020’ye kadar aynen uygulanmaya devam edecektir. Kira tespit davasında dahi̇ ÜFE üst sınırdır. Mahkeme ÜFE’nin altında da bir kira bedeli̇ tespit edebilir. ( http://www.birlesmismarkalar.org.tr/uploads/Kira_Sozlesmeleri_Sunumu.pdf )

01.2019 tarih ve 30659 sayılı Resmi Gazete’de yayınlanan vergi kanunları ile bazı kanun ve kanun hükmünde kararnamelerde değişiklik yapılmasına dair kanun uyarınca, kira sözleşmelerinde yapılacak artış bir önceki yılın TÜFE ortalaması ile sınırlandırılmıştır. Her türlü konut ve çatılı işyeri vasfındaki taşınmaz kiralamaları (kiracı konumundaki tüzel kişi ve tacirlerin işyeri kiralamaları dahil) ile, kamu kurum ve kuruluşları ile yapılacak her türlü kira sözleşmesinde uygulanacağı değerlendirilmektedir. TÜFE artışı hakkında, son zamanlarda yapılan bir diğer mevzuat değişikliği ise, kamu idarelerinin taşınmaz kiralamalarına ilişkindir. 16 Ocak 2019 tarihli Resmi Gazete’de yayınlanan kamu idarelerinin taşınmaz kiralamalarına ilişkin genelge uyarınca da, kira artışlarının en fazla bir önceki yıl TÜFE ortalaması miktarında olmasına karar verilmiştir. 69/b maddesi uyarınca, 01.01.2019 tarihinden itibaren geçerli olmak uygulanmasının mümkün olduğu anlaşılmaktadır. Sonuç olarak, konut ve çatılı işyerlerine veya kamu ile yapılan kira sözleşmelerine dair 2019 yılında yapılması gereken kira artışlarında, sözleşmenin taraflarınca daha aşağı oranda bir artış yapılmasına dair herhangi bir düzenleme öngörülmemiş ise (veya çok sık olduğu üzere (TÜFE+ÜFE)/2 şeklinde düzenlenmiş ise), en fazla 2018 yılı TÜFE ortalaması oranında artış yapılabilecektir.
  (https://sengunhukukyayinlari.com/kira-bedelinde-tufe-oraninda-artis-yapilmasi-hakkinda-yeni-duzenleme/?utm_source=Mondaq&utm_medium=syndication&utm_campaign=LinkedIn-integration )"
    )
    observeEvent(input$basla,{
        guncelleme(x = input$x, sektor = input$sektor, from = input$from, to = input$to)
        output$rapor <- renderText({rapor})
    })
    observeEvent(input$basla2,{
      kira_guncelleme(x = input$x2, to = input$to2)
      output$rapor_kira <- renderText({rapor_kira})
    })
}

shinyApp(ui = ui, server = server)
