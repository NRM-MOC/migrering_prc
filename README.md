Migrering av historiska data
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
den efter rapportmallar. Notera att vi lagt in individvariabler som
ålder, vikt och längd både i `DATA_MATVARDE` och tillsammans med kön i
`PROVDATA_BIOTA`. Vidare förekommer fettprocent och torrviktsprocent
både som egna kolumner och rader i `DATA_MATVARDE`. Vi har även på 
halvautomatiskt sätt försökt generera en `PROV_KOD_LABB`.

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
    ## # ... with 14 more variables: MYEAR <lgl>, PROVTAG_DAT <date>, ACCNR <chr>,
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
         DIREKT_BEHA = "FRYST") %>% 
  select("PROVPLATS_ID", "NAMN_PROVPLATS", "PROV_KOD_ORIGINAL", "PROVTAG_SYFTE", 
         "PROVPLATS_MILJO", "PROVPLATS_TYP", "PROVTAG_DAT", 
         "PROVTAG_ORG", "ACKR_PROV", "PLATTFORM", "PROVTAG_MET", 
         "DIREKT_BEHA") %>% 
  distinct()
head(PROVMETADATA)
```

    ## # A tibble: 6 x 12
    ##   PROVPLATS_ID NAMN_PROVPLATS PROV_KOD_ORIGIN~ PROVTAG_SYFTE PROVPLATS_MILJO
    ##   <chr>        <chr>          <chr>            <chr>         <chr>          
    ## 1 00103839     Bolmen         C1967/08001-080~ NMO           SJO-SOTV-RINN  
    ## 2 00103839     Bolmen         C1967/08001      NMO           SJO-SOTV-RINN  
    ## 3 00103839     Bolmen         C1967/08002      NMO           SJO-SOTV-RINN  
    ## 4 00103839     Bolmen         C1967/08003      NMO           SJO-SOTV-RINN  
    ## 5 00103839     Bolmen         C1967/08004      NMO           SJO-SOTV-RINN  
    ## 6 00103839     Bolmen         C1967/08005      NMO           SJO-SOTV-RINN  
    ## # ... with 7 more variables: PROVPLATS_TYP <chr>, PROVTAG_DAT <date>,
    ## #   PROVTAG_ORG <chr>, ACKR_PROV <chr>, PLATTFORM <chr>, PROVTAG_MET <chr>,
    ## #   DIREKT_BEHA <chr>

``` r
write_csv(PROVMETADATA, "PROVMETADATA.csv")
```

``` r
PROVDATA_BIOTA <- LIMN_data %>% 
  mutate(KON = case_when(SEX == 1 ~ "M",
                         SEX == 2 ~ "F",
                         (SEX > 1) & (SEX < 2) ~ "X"),
         ANTAL = NHOM) %>% 
  select("PROV_KOD_ORIGINAL", "ART", "DYNTAXA_TAXON_ID", "KON", "ALDER_AR" = "ALDR", 
         "LANGD_CM" = "TOTL", "VIKT_G" = "TOTV", "ANTAL") %>% 
  distinct()
head(PROVDATA_BIOTA)
```

    ## # A tibble: 6 x 8
    ##   PROV_KOD_ORIGINAL ART   DYNTAXA_TAXON_ID KON   ALDER_AR LANGD_CM VIKT_G ANTAL
    ##   <chr>             <chr>            <dbl> <chr>    <dbl>    <dbl>  <dbl> <dbl>
    ## 1 C1967/08001-08010 Gadda           206139 <NA>         0       NA     NA    10
    ## 2 C1967/08001       Gadda           206139 <NA>        NA       NA   1180     1
    ## 3 C1967/08002       Gadda           206139 <NA>        NA       NA    845     1
    ## 4 C1967/08003       Gadda           206139 <NA>        NA       NA   1075     1
    ## 5 C1967/08004       Gadda           206139 <NA>        NA       NA   1790     1
    ## 6 C1967/08005       Gadda           206139 <NA>        NA       NA   1760     1

``` r
write_csv(PROVDATA_BIOTA, "PROVDATA_BIOTA.csv")
```

``` r
DATA_MATVARDE <- LIMN_data %>% 
  mutate(is_LOQ = (VALUE < 0) & !(NRM_CODE %in% c("D13CUCD", "D15NUCD")),
         LABB = LAB,
         MATVARDETAL_ANM = ifelse(is_LOQ, "<", ""),
         MATV_STD = ifelse(is_LOQ, "q", ""),
         MATVARDETAL = ifelse(is_LOQ, abs(VALUE), VALUE),
         RAPPORTERINGSGRANS_LOQ = ifelse(is_LOQ, MATVARDETAL, NA),
         UTFOR_LABB = ifelse(NRM_CODE %in% c("D13CUCD", "D15NUCD", "CUCD", "NUCD"), "UC Davies", NA),
         PROV_LAGR = "FRYST",
         PROV_KOD_LABB = paste(PROV_KOD_ORIGINAL, LAB_KOD),
         FETT_PRC = FPRC,
         TORRVIKT_PRC = case_when(ORGAN == "MUSKEL" ~ MTPRC,
                                  ORGAN == "LEVER" ~ LTPRC)
  ) %>% 
  select("PARAMETERNAMN", "UNIK_PARAMETERKOD", "PROV_KOD_ORIGINAL", "PROV_KOD_LABB",
         "LABB", "UTFOR_LABB", "ORGAN", 
         "MATVARDETAL", "MATVARDETAL_ANM", "ENHET", "MATV_STD", "RAPPORTERINGSGRANS_LOQ", 
         "PROV_LAGR", "FETT_PRC", "TORRVIKT_PRC") %>% 
  distinct()
head(DATA_MATVARDE)
```

    ## # A tibble: 6 x 15
    ##   PARAMETERNAMN UNIK_PARAMETERK~ PROV_KOD_ORIGIN~ PROV_KOD_LABB LABB  UTFOR_LABB
    ##   <chr>         <chr>            <chr>            <chr>         <chr> <lgl>     
    ## 1 Ålder (medel~ CH12/241         C1967/08001-080~ C1967/08001-~ NRM   NA        
    ## 2 2,2',4,4',5,~ CH07/139         C1967/08001-080~ C1967/08001-~ NSL   NA        
    ## 3 2,2',4,4',5,~ CH07/137         C1967/08001-080~ C1967/08001-~ NSL   NA        
    ## 4 Vikt          CH12/232         C1967/08001      C1967/08001 ~ NRM   NA        
    ## 5 Kvicksilver   CH01/86          C1967/08001      C1967/08001 ~ TRC   NA        
    ## 6 Fettvikt, %   CH12/68          C1967/08001      C1967/08001 ~ NSL   NA        
    ## # ... with 9 more variables: ORGAN <chr>, MATVARDETAL <dbl>,
    ## #   MATVARDETAL_ANM <chr>, ENHET <chr>, MATV_STD <chr>,
    ## #   RAPPORTERINGSGRANS_LOQ <dbl>, PROV_LAGR <chr>, FETT_PRC <dbl>,
    ## #   TORRVIKT_PRC <dbl>

``` r
write_csv(DATA_MATVARDE, "DATA_MATVARDE.csv")
```

``` r
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 18363)
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
    ##  [1] readxl_1.3.1    forcats_0.5.0   stringr_1.4.0   dplyr_1.0.2    
    ##  [5] purrr_0.3.4     readr_1.4.0     tidyr_1.1.2     tibble_3.0.4   
    ##  [9] ggplot2_3.3.2   tidyverse_1.3.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.0 xfun_0.18        haven_2.3.1      colorspace_1.4-1
    ##  [5] vctrs_0.3.4      generics_0.0.2   htmltools_0.5.0  yaml_2.2.1      
    ##  [9] utf8_1.1.4       blob_1.2.1       rlang_0.4.8      pillar_1.4.6    
    ## [13] glue_1.4.2       withr_2.3.0      DBI_1.1.0        dbplyr_1.4.4    
    ## [17] modelr_0.1.8     lifecycle_0.2.0  munsell_0.5.0    gtable_0.3.0    
    ## [21] cellranger_1.1.0 rvest_0.3.6      evaluate_0.14    knitr_1.30      
    ## [25] MoCiS2_0.1.0     fansi_0.4.1      broom_0.7.1      Rcpp_1.0.5      
    ## [29] scales_1.1.1     backports_1.1.10 jsonlite_1.7.1   fs_1.5.0        
    ## [33] hms_0.5.3        digest_0.6.25    stringi_1.5.3    grid_4.0.2      
    ## [37] cli_2.1.0        tools_4.0.2      magrittr_1.5     crayon_1.3.4    
    ## [41] pkgconfig_2.0.3  ellipsis_0.3.1   xml2_1.3.2       reprex_0.3.0    
    ## [45] lubridate_1.7.9  assertthat_0.2.1 rmarkdown_2.4    httr_1.4.2      
    ## [49] rstudioapi_0.11  R6_2.4.1         compiler_4.0.2
