library(tidyverse)
library(sf)
library(magrittr)

#HSDA 
HDSA <- 
st_read("HSDA_2018/HSDA_2018.shp") %>%
  st_drop_geometry() %>% 
  distinct(HSDA_Name) %$%
  HSDA_Name

cancer_df_males <-
  tibble(
    HSDA_Name = 
      rep(HDSA, times = 5*4),
    Sex = "Males",
    ASIR = 
      c(sample(50:100, 16*3, replace = T), rep(NA, 16*2),
        sample(50:100, 16*3, replace = T), rep(NA, 16*2),
        sample(50:100, 16*3, replace = T), rep(NA, 16*2),
        sample(50:100, 16*3, replace = T), rep(NA, 16*2)),
    Cancer = 
      rep(
        c(
      rep("Lung", times = 16),
      rep("Colorectal", times = 16),
      rep("Prostate", times = 16),
      rep("Cervix", times = 16),
      rep("Breast", times = 16)
    ), 
    4),
    Year = c(
      rep(2017, 16*5),
      rep(2016, 16*5),
      rep(2015, 16*5),
      rep(2014, 16*5)
  )
  )
  

cancer_df_females <-
  tibble(
    HSDA_Name = 
      rep(HDSA, times = 5*4),
    Sex = "Females",
    ASIR = 
      c(sample(50:100, 16*2, replace = T), rep(NA, 16*1), sample(50:100, 16*2, replace = T),
        sample(50:100, 16*2, replace = T), rep(NA, 16*1), sample(50:100, 16*2, replace = T),
        sample(50:100, 16*2, replace = T), rep(NA, 16*1), sample(50:100, 16*2, replace = T),
        sample(50:100, 16*2, replace = T), rep(NA, 16*1), sample(50:100, 16*2, replace = T)),
    Cancer = 
      rep(
        c(
          rep("Lung", times = 16),
          rep("Colorectal", times = 16),
          rep("Prostate", times = 16),
          rep("Cervix", times = 16),
          rep("Breast", times = 16)
        ), 
        4),
    Year = c(
      rep(2017, 16*5),
      rep(2016, 16*5),
      rep(2015, 16*5),
      rep(2014, 16*5)
    )
  )

cancer_df_hsda <- 
  rbind(
    cancer_df_females,
    cancer_df_males
  )

rm(
  cancer_df_females,
  cancer_df_males
)

#HA
HA <- 
  st_read("HA_2018/HA_2018.shp") %>%
  st_drop_geometry() %>% 
  distinct(HA_Name) %$%
  HA_Name

cancer_df_males <-
  tibble(
    HA_Name = 
      rep(HA, times = 5*4),
    Sex = "Males",
    ASIR = 
        c(sample(50:100, 5*3, replace = T), rep(NA, 5*2),
          sample(50:100, 5*3, replace = T), rep(NA, 5*2),
          sample(50:100, 5*3, replace = T), rep(NA, 5*2),
          sample(50:100, 5*3, replace = T), rep(NA, 5*2)),
    Cancer = 
      rep(
        c(
          rep("Lung", times = 5),
          rep("Colorectal", times = 5),
          rep("Prostate", times = 5),
          rep("Cervix", times = 5),
          rep("Breast", times = 5)
        ),
        4),
    Year = c(
      rep(2017, 5*5),
      rep(2016, 5*5),
      rep(2015, 5*5),
      rep(2014, 5*5)
    )
  )

cancer_df_females <-
  tibble(
    HA_Name = 
      rep(HA, times = 5*4),
    Sex = "Females",
    ASIR = 
      c(sample(50:100, 5*2, replace = T), rep(NA, 5*1),sample(50:100, 5*2, replace = T),
        sample(50:100, 5*2, replace = T), rep(NA, 5*1),sample(50:100, 5*2, replace = T),
        sample(50:100, 5*2, replace = T), rep(NA, 5*1),sample(50:100, 5*2, replace = T),
        sample(50:100, 5*2, replace = T), rep(NA, 5*1),sample(50:100, 5*2, replace = T)),
    Cancer = 
      rep(
        c(
          rep("Lung", times = 5),
          rep("Colorectal", times = 5),
          rep("Prostate", times = 5),
          rep("Cervix", times = 5),
          rep("Breast", times = 5)
        ),
        4),
    Year = c(
      rep(2017, 5*5),
      rep(2016, 5*5),
      rep(2015, 5*5),
      rep(2014, 5*5)
    )
  )

cancer_df_ha <- 
  rbind(
    cancer_df_females,
    cancer_df_males
  )

rm(
  cancer_df_females,
  cancer_df_males
)


#LHA
LHA <-
  st_read("LHA_2018/LHA_2018.shp") %>%
  st_drop_geometry() %>% 
  distinct(LHA_Name) %$%
    LHA_Name

cancer_df_males <-
  tibble(
    LHA_Name = 
      rep(LHA, times = 5*4),
    Sex = "Males",
    ASIR = 
      c(sample(50:100, 89*3, replace = T), rep(NA, 89*2),
        sample(50:100, 89*3, replace = T), rep(NA, 89*2),
        sample(50:100, 89*3, replace = T), rep(NA, 89*2),
        sample(50:100, 89*3, replace = T), rep(NA, 89*2)),
    Cancer = 
      rep(
        c(
          rep("Lung", times = 89),
          rep("Colorectal", times = 89),
          rep("Prostate", times = 89),
          rep("Cervix", times = 89),
          rep("Breast", times = 89)
        ),
        4),
    Year = c(
      rep(2017, 89*5),
      rep(2016, 89*5),
      rep(2015, 89*5),
      rep(2014, 89*5)
    )
  )

cancer_df_females <-
  tibble(
    LHA_Name = 
      rep(LHA, times = 5*4),
    Sex = "Females",
    ASIR = 
      c(sample(50:100, 89*2, replace = T), rep(NA, 89*1),sample(50:100, 89*2, replace = T),
        sample(50:100, 89*2, replace = T), rep(NA, 89*1),sample(50:100, 89*2, replace = T),
        sample(50:100, 89*2, replace = T), rep(NA, 89*1),sample(50:100, 89*2, replace = T),
        sample(50:100, 89*2, replace = T), rep(NA, 89*1),sample(50:100, 89*2, replace = T)),
    Cancer = 
      rep(
        c(
          rep("Lung", times = 89),
          rep("Colorectal", times = 89),
          rep("Prostate", times = 89),
          rep("Cervix", times = 89),
          rep("Breast", times = 89)
        ),
        4),
    Year = c(
      rep(2017, 89*5),
      rep(2016, 89*5),
      rep(2015, 89*5),
      rep(2014, 89*5)
    )
  )

cancer_df_lha <-
  rbind(
    cancer_df_females,
    cancer_df_males
  )

rm(
  cancer_df_females,
  cancer_df_males
)

#CHSA
CHSA <-
  st_read("CHSA_2018/CHSA_2018.shp") %>%
  st_drop_geometry() %>% 
  distinct(CHSA_Name) %$%
  CHSA_Name

cancer_df_males <-
  tibble(
    CHSA_Name = 
      rep(CHSA, times = 5*4),
    Sex = "Males",
    ASIR = 
      c(sample(50:100, 218*3, replace = T), rep(NA, 218*2),
        sample(50:100, 218*3, replace = T), rep(NA, 218*2),
        sample(50:100, 218*3, replace = T), rep(NA, 218*2),
        sample(50:100, 218*3, replace = T), rep(NA, 218*2)),
    Cancer = 
      rep(
        c(
          rep("Lung", times = 218),
          rep("Colorectal", times = 218),
          rep("Prostate", times = 218),
          rep("Cervix", times = 218),
          rep("Breast", times = 218)
        ),
        4),
    Year = c(
      rep(2017, 218*5),
      rep(2016, 218*5),
      rep(2015, 218*5),
      rep(2014, 218*5)
    )
  )

cancer_df_females <-
  tibble(
    CHSA_Name = 
      rep(CHSA, times = 5*4),
    Sex = "Females",
    ASIR = 
      c(sample(50:100, 218*2, replace = T), rep(NA, 218*1),sample(50:100, 218*2, replace = T),
        sample(50:100, 218*2, replace = T), rep(NA, 218*1),sample(50:100, 218*2, replace = T),
        sample(50:100, 218*2, replace = T), rep(NA, 218*1),sample(50:100, 218*2, replace = T),
        sample(50:100, 218*2, replace = T), rep(NA, 218*1),sample(50:100, 218*2, replace = T)),
    Cancer = 
      rep(
        c(
          rep("Lung", times = 218),
          rep("Colorectal", times = 218),
          rep("Prostate", times = 218),
          rep("Cervix", times = 218),
          rep("Breast", times = 218)
        ),
        4),
    Year = c(
      rep(2017, 218*5),
      rep(2016, 218*5),
      rep(2015, 218*5),
      rep(2014, 218*5)
    )
  )

cancer_df_chsa <-
  rbind(
    cancer_df_females,
    cancer_df_males
  )

rm(
  cancer_df_females,
  cancer_df_males
)
