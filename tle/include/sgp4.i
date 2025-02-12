        ! ----------------------- Common Blocks -----------------------
        COMMON  /ElsetRec/  SatName,                                    &
     &          SatNum, ELNO  , EPHTYP, REVI  , EpochYr,                &
     &          BStar , Ecco  , Inclo , nodeo , Argpo , no_kozai , Mo , &
     &          NDot  , NDDot , alta  , altp  , a     ,                 &
     &          JDSatEpoch    , JDSatEpochF   , EpochDays, no_unkozai , &
     &          Aycof , CON41 , Cc1   , Cc4   , Cc5   , D2    , D3    , &
     &          D4    , Delmo , Eta   , ArgpDot,Omgcof, Sinmao,         &
     &          T2cof , T3cof , T4cof , T5cof , X1mth2, X7thm1, MDot  , &
     &          nodeDot,Xlcof, Xmcof , Xnodcf,                          &

     &          D2201 , D2211 , D3210 , D3222 , D4410 , D4422 , D5220 , &
     &          D5232 , D5421 , D5433 , Dedt  , Del1  , Del2  , Del3  , &
     &          Didt  , Dmdt  , Dnodt , Domdt , E3    , Ee2   , Peo   , &
     &          Pgho  , Pho   , Pinco , Plo   , Se2   , Se3   , Sgh2  , &
     &          Sgh3  , Sgh4  , Sh2   , Sh3   , Si2   , Si3   , Sl2   , &
     &          Sl3   , Sl4   , GSTo  , Xfact , Xgh2  , Xgh3  , Xgh4  , &
     &          Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   , Xl4   , &
     &          Xlamo , Zmol  , Zmos  , Atime , Xli   , Xni   , IRez  , &
     &          Isimp , Init  , Method, Opsmode

        CHARACTER*12  SatName
        INTEGER SatNum, ELNO  , EPHTYP, REVI  , EpochYr
        REAL*8  BStar , Ecco  , Inclo , nodeo , Argpo , no_kozai , Mo , &
     &          NDot  , NDDot , alta  , altp  , a     ,                 &
     &          JDSatEpoch    , JDSatEpochF   , EpochDays, no_unkozai

        REAL*8  Aycof , CON41 , Cc1   , Cc4   , Cc5   , D2    , D3    , &
     &          D4    , Delmo , Eta   , ArgpDot,Omgcof, Sinmao,         &
     &          T2cof , T3cof , T4cof , T5cof , X1mth2, X7thm1, MDot  , &
     &          nodeDot,Xlcof, Xmcof , Xnodcf

        ! DS values
        REAL*8  D2201 , D2211 , D3210 , D3222 , D4410 , D4422 , D5220 , &
     &          D5232 , D5421 , D5433 , Dedt  , Del1  , Del2  , Del3  , &
     &          Didt  , Dmdt  , Dnodt , Domdt , E3    , Ee2   , Peo   , &
     &          Pgho  , Pho   , Pinco , Plo   , Se2   , Se3   , Sgh2  , &
     &          Sgh3  , Sgh4  , Sh2   , Sh3   , Si2   , Si3   , Sl2   , &
     &          Sl3   , Sl4   , GSTo  , Xfact , Xgh2  , Xgh3  , Xgh4  , &
     &          Xh2   , Xh3   , Xi2   , Xi3   , Xl2   , Xl3   , Xl4   , &
     &          Xlamo , Zmol  , Zmos  , Atime , Xli   , Xni
        INTEGER IRez

        ! NE values
        INTEGER Isimp
        CHARACTER Init, Method, Opsmode
        


