Migrering av historiska data (uppdaterad 220429)
================

Historiska data finns på riksmuseet lagrade i så kallade `prc`-filer,
exempel på en sådan är
[`LIMN_short.prc`](https://raw.githubusercontent.com/NRM-MOC/migrering_prc/main/files/LIMN_short.prc)
som innehåller data från det limniska övervakningsprogrammet fram till
1982. Det finns ett pågående projekt på riksmuseet med att migrera dessa
data till en modern databas, delvis beroende på att formatet är svårt
att anpassa till nya rapporteringskrav. Så länge detta arbete saknar
finansiering finns dock ingen tydlig tidsplan.

Nedanstående R-kod drar ut data ur ovannända `prc`-fil och formaterar
den efter rapportmallar (i `csv`-filer ovan). Notera att vi lagt in
individvariabler som ålder, vikt och längd både i
[`DATA_MATVARDE`](https://github.com/NRM-MOC/migrering_prc/blob/main/DATA_MATVARDE.csv)
(enligt instruktion från datavärd) och tillsammans med kön i
[`PROVDATA_BIOTA`](https://github.com/NRM-MOC/migrering_prc/blob/main/PROVDATA_BIOTA.csv).
Vidare förekommer fettprocent och torrviktsprocent både som egna
kolumner och som rader i
[`DATA_MATVARDE`](https://github.com/NRM-MOC/migrering_prc/blob/main/DATA_MATVARDE.csv).
Vi har även på halvautomatiskt sätt försökt generera en `PROV_KOD_LABB`.
Saknas gör uppgifter om analysmetoder samt översättning av labb-namn
till datavärdens kodlista, även om detta i praktiken antagligen går att
ordna har det låg prioritet hos riksmuseet.

Motsvarande utdrag bör i princip kunna göras även för marina
övervakningsprogrammet, eventuellt efter mindre modifikationer för att
hantera andra matristyper (ägg och musslor).

``` r
library(tidyverse)
library(readxl)
# MoCiS2-paketet behövs för att läsa prc-filer samt innehåller kodlista
# devtools::install_github("https://github.com/NRM-MOC/MoCiS2")
kodlista_path <- system.file("extdata", "codelist.xlsx", package = "MoCiS2")

# Dra parametervärden ur prc-fil
LIMN_prc <- MoCiS2::moc_read_prc("files/LIMN_short.prc") %>% 
  filter(str_sub(raw, 1, 1) != "*") # * markerar felaktiga värden
head(LIMN_prc)
```

    ## # A tibble: 6 x 26
    ##   raw   NRM_CODE VALUE Group LAB   ORGAN PARAMETER  YEAR  WEEK   DAY GENUS LOC  
    ##   <chr> <chr>    <dbl> <chr> <chr> <chr> <chr>     <dbl> <dbl> <dbl> <chr> <chr>
    ## 1 1967~ HEADER      NA <NA>  <NA>  <NA>  HEADER     1967    14     7 ESOX  BOLM 
    ## 2 +TOT~ TOTVH       NA weig~ NRM   <NA>  TOTV       1967    14     7 ESOX  BOLM 
    ## 3 +TOT~ TOTLH       NA meas~ NRM   <NA>  TOTL       1967    14     7 ESOX  BOLM 
    ## 4 +KRP~ KRPLH       NA meas~ NRM   <NA>  KRPL       1967    14     7 ESOX  BOLM 
    ## 5 +ALD~ ALDRH        0 age   NRM   OTOL~ ALDR       1967    14     7 ESOX  BOLM 
    ## 6 +GOV~ GOVKT       NA weig~ NRM   <NA>  GOVKT      1967    14     7 ESOX  BOLM 
    ## # ... with 14 more variables: MYEAR <lgl>, PROVTAG_DAT <chr>, ACCNR <chr>,
    ## #   LAB_KOD <chr>, TOTL <dbl>, KRPL <dbl>, TOTV <dbl>, ALDR <dbl>, ANM <chr>,
    ## #   SEX <dbl>, NHOM <dbl>, FPRC <dbl>, LTPRC <dbl>, MTPRC <dbl>

``` r
# SGUify
LIMN_data <- LIMN_prc %>% 
  left_join(read_excel(kodlista_path, sheet = "STATIONER") %>% select(LOC, PROVPLATS_ID, NAMN_PROVPLATS)) %>% 
  left_join(read_excel(kodlista_path, sheet = "ARTER") %>% select(GENUS, ART, DYNTAXA_TAXON_ID)) %>% 
  left_join(read_excel(kodlista_path, sheet = "PARAMETRAR")  %>%
              select(NRM_PARAMETERKOD, PARAMETERNAMN, UNIK_PARAMETERKOD, ENHET, MATOSAKERHET_ENHET, PROV_LAGR),
            by = c("NRM_CODE" = "NRM_PARAMETERKOD")) %>% 
  filter(!is.na(PARAMETERNAMN), !is.na(VALUE)) %>% # Ta bort parametrar som saknas i kodlista (skall ej rapporteras) samt de som saknar värde
  rename(PROV_KOD_ORIGINAL = ACCNR) %>% 
  mutate(ORGAN = ifelse(str_sub(PARAMETERNAMN, 1, 3) %in% c("Vik", "Län", "Åld"), "HELKROPP", ORGAN)) 


PROVMETADATA <- LIMN_data %>% 
  mutate(PROVPLATS_MILJO = "SJO-SOTV-RINN",
         PROVPLATS_TYP = "Bakgrund",
         PROVTAG_SYFTE = "NMO",
         PROVTAG_ORG = "NRM",
         ACKR_PROV = "Nej",
         PROVTAG_MET = "Natfiske",
         PLATTFORM = "SMABAT",
         DIREKT_BEHA = "FRYST",
         ANTAL_DAGAR = 1,
         POSITION_NK_FKT_PROVPUNKT = NA,
         POSITION_EK_FKT_PROVPUNKT = NA,
         UNDERSOKNINGSTYP = NA,
         KOMMENTAR_PROV = NA) %>% 
  select("PROVPLATS_ID", "NAMN_PROVPLATS","POSITION_NK_FKT_PROVPUNKT", "POSITION_EK_FKT_PROVPUNKT", "PROV_KOD_ORIGINAL", "PROVTAG_SYFTE", "UNDERSOKNINGSTYP",
         "PROVPLATS_MILJO", "PROVPLATS_TYP", "PROVTAG_DAT", "ANTAL_DAGAR",
         "PROVTAG_ORG", "ACKR_PROV", "PLATTFORM", "PROVTAG_MET", 
         "DIREKT_BEHA", "KOMMENTAR_PROV") %>% 
  distinct()
head(PROVMETADATA)
```

    ## # A tibble: 6 x 17
    ##   PROVPLATS_ID NAMN_PROVPLATS POSITION_NK_FKT~ POSITION_EK_FKT~ PROV_KOD_ORIGIN~
    ##   <chr>        <chr>          <lgl>            <lgl>            <chr>           
    ## 1 00103839     Bolmen         NA               NA               C1967/08001-080~
    ## 2 00103839     Bolmen         NA               NA               C1967/08001     
    ## 3 00103839     Bolmen         NA               NA               C1967/08002     
    ## 4 00103839     Bolmen         NA               NA               C1967/08003     
    ## 5 00103839     Bolmen         NA               NA               C1967/08004     
    ## 6 00103839     Bolmen         NA               NA               C1967/08005     
    ## # ... with 12 more variables: PROVTAG_SYFTE <chr>, UNDERSOKNINGSTYP <lgl>,
    ## #   PROVPLATS_MILJO <chr>, PROVPLATS_TYP <chr>, PROVTAG_DAT <chr>,
    ## #   ANTAL_DAGAR <dbl>, PROVTAG_ORG <chr>, ACKR_PROV <chr>, PLATTFORM <chr>,
    ## #   PROVTAG_MET <chr>, DIREKT_BEHA <chr>, KOMMENTAR_PROV <lgl>

``` r
write_csv(PROVMETADATA, "PROVMETADATA.csv", na = "")
```

``` r
PROVDATA_BIOTA <- LIMN_data %>% 
  mutate(KON = case_when(SEX == 1 ~ "M",
                         SEX == 2 ~ "F",
                         (SEX > 1) & (SEX < 2) ~ "X"),
         ANTAL = NHOM, KOMMENTAR_PROV = NA) %>% 
  select("PROV_KOD_ORIGINAL", "ART", "DYNTAXA_TAXON_ID", "KON", "ANTAL", "KOMMENTAR_PROV") %>% 
  distinct()
head(PROVDATA_BIOTA)
```

    ## # A tibble: 6 x 6
    ##   PROV_KOD_ORIGINAL ART   DYNTAXA_TAXON_ID KON   ANTAL KOMMENTAR_PROV
    ##   <chr>             <chr>            <dbl> <chr> <dbl> <lgl>         
    ## 1 C1967/08001-08010 Gadda           206139 <NA>     10 NA            
    ## 2 C1967/08001       Gadda           206139 <NA>      1 NA            
    ## 3 C1967/08002       Gadda           206139 <NA>      1 NA            
    ## 4 C1967/08003       Gadda           206139 <NA>      1 NA            
    ## 5 C1967/08004       Gadda           206139 <NA>      1 NA            
    ## 6 C1967/08005       Gadda           206139 <NA>      1 NA

``` r
write_csv(PROVDATA_BIOTA, "PROVDATA_BIOTA.csv", na = "")
```

``` r
DATA_MATVARDE <- LIMN_data %>% 
  mutate(is_LOQ = (VALUE < 0) & !(NRM_CODE %in% c("D13CUCD", "D15NUCD")),
         LABB = LAB,
         MATVARDETAL_ANM = ifelse(is_LOQ, "<", ""),
         MATV_STD = ifelse(is_LOQ, "q", ""),
         MATVARDETAL = ifelse(is_LOQ, abs(VALUE), VALUE),
         RAPPORTERINGSGRANS_LOQ = ifelse(is_LOQ, MATVARDETAL, NA),
         DETEKTIONSGRANS_LOD = NA,
         UTFOR_LABB = ifelse(NRM_CODE %in% c("D13CUCD", "D15NUCD", "CUCD", "NUCD"), "UC Davies", NA),
         PROV_LAGR = "FRYST",
         RAPPORT_KOD_LABB = paste(PROV_KOD_ORIGINAL, LAB_KOD),
         FETT_PRC = FPRC,
         TORRVIKT_PRC = case_when(ORGAN == "MUSKEL" ~ MTPRC,
                                  ORGAN == "LEVER" ~ LTPRC),
         MATVARDETEXT = NA,
         MATOSAKERHET = NA, MATOSAKERHET_ENHET = NA, MATOSAKERHET_TYP = NA,
         MATVARDESPAR = if_else(is_LOQ, "Ja", ""),
         DATUM_REG = NA, PROV_BERED = NA, PROVKARL = NA, ANALYS_DAT = NA, ANALYS_MET = NA, ACKREDITERAD_MET = NA, ANALYS_INSTR = NA, KOMMENTAR_MATVARDE = NA
  ) %>% 
  select("PARAMETERNAMN", "UNIK_PARAMETERKOD", "PROV_KOD_ORIGINAL", "RAPPORT_KOD_LABB",
         "LABB", "UTFOR_LABB", "ORGAN", "MATVARDETEXT",
         "MATVARDETAL", "MATVARDETAL_ANM", "ENHET", "MATV_STD", "RAPPORTERINGSGRANS_LOQ", "DETEKTIONSGRANS_LOD", "MATOSAKERHET", "MATOSAKERHET_ENHET", "MATOSAKERHET_TYP", 
         "MATVARDESPAR", "DATUM_REG",
         "PROV_LAGR", "PROV_BERED", "PROVKARL", "ANALYS_DAT", "ANALYS_MET", "ACKREDITERAD_MET", "ANALYS_INSTR", "KOMMENTAR_MATVARDE") %>% 
  distinct()
head(DATA_MATVARDE)
```

    ## # A tibble: 6 x 27
    ##   PARAMETERNAMN         UNIK_PARAMETERK~ PROV_KOD_ORIGIN~ RAPPORT_KOD_LABB LABB 
    ##   <chr>                 <chr>            <chr>            <chr>            <chr>
    ## 1 Ålder (medelvärde)    CH12/241         C1967/08001-080~ C1967/08001-080~ NRM  
    ## 2 2,2',4,4',5,6'-Hexab~ CH07/139         C1967/08001-080~ C1967/08001-080~ NSL  
    ## 3 2,2',4,4',5,5'-Hexab~ CH07/137         C1967/08001-080~ C1967/08001-080~ NSL  
    ## 4 Vikt                  CH12/232         C1967/08001      C1967/08001 NA   NRM  
    ## 5 Kvicksilver           CH01/86          C1967/08001      C1967/08001 ZT55 TRC  
    ## 6 Fettvikt, %           CH12/68          C1967/08001      C1967/08001 820~ NSL  
    ## # ... with 22 more variables: UTFOR_LABB <lgl>, ORGAN <chr>,
    ## #   MATVARDETEXT <lgl>, MATVARDETAL <dbl>, MATVARDETAL_ANM <chr>, ENHET <chr>,
    ## #   MATV_STD <chr>, RAPPORTERINGSGRANS_LOQ <dbl>, DETEKTIONSGRANS_LOD <lgl>,
    ## #   MATOSAKERHET <lgl>, MATOSAKERHET_ENHET <lgl>, MATOSAKERHET_TYP <lgl>,
    ## #   MATVARDESPAR <chr>, DATUM_REG <lgl>, PROV_LAGR <chr>, PROV_BERED <lgl>,
    ## #   PROVKARL <lgl>, ANALYS_DAT <lgl>, ANALYS_MET <lgl>, ACKREDITERAD_MET <lgl>,
    ## #   ANALYS_INSTR <lgl>, KOMMENTAR_MATVARDE <lgl>

``` r
write_csv(DATA_MATVARDE, "DATA_MATVARDE.csv", na = "")
```

``` r
sessionInfo()
```

    ## R version 4.1.2 (2021-11-01)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 22000)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Swedish_Sweden.1252  LC_CTYPE=Swedish_Sweden.1252   
    ## [3] LC_MONETARY=Swedish_Sweden.1252 LC_NUMERIC=C                   
    ## [5] LC_TIME=Swedish_Sweden.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] readxl_1.3.1    forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7    
    ##  [5] purrr_0.3.4     readr_2.1.2     tidyr_1.2.0     tibble_3.1.6   
    ##  [9] ggplot2_3.3.5   tidyverse_1.3.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.1 xfun_0.29        haven_2.4.3      colorspace_2.0-2
    ##  [5] vctrs_0.3.8      generics_0.1.2   htmltools_0.5.2  yaml_2.2.2      
    ##  [9] utf8_1.2.2       rlang_1.0.0      pillar_1.7.0     withr_2.4.3     
    ## [13] glue_1.6.1       DBI_1.1.2        bit64_4.0.5      dbplyr_2.1.1    
    ## [17] modelr_0.1.8     lifecycle_1.0.1  munsell_0.5.0    gtable_0.3.0    
    ## [21] cellranger_1.1.0 rvest_1.0.2      evaluate_0.14    knitr_1.37      
    ## [25] tzdb_0.2.0       fastmap_1.1.0    parallel_4.1.2   MoCiS2_0.1.0    
    ## [29] fansi_1.0.2      broom_0.7.12     Rcpp_1.0.8       backports_1.4.1 
    ## [33] scales_1.1.1     vroom_1.5.7      jsonlite_1.7.3   bit_4.0.4       
    ## [37] fs_1.5.2         hms_1.1.1        digest_0.6.29    stringi_1.7.6   
    ## [41] grid_4.1.2       cli_3.1.1        tools_4.1.2      magrittr_2.0.2  
    ## [45] crayon_1.4.2     pkgconfig_2.0.3  ellipsis_0.3.2   xml2_1.3.3      
    ## [49] reprex_2.0.1     lubridate_1.8.0  assertthat_0.2.1 rmarkdown_2.11  
    ## [53] httr_1.4.2       rstudioapi_0.13  R6_2.5.1         compiler_4.1.2
