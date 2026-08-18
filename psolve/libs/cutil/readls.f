      SUBROUTINE READLS(INAME,LAT,SITHIT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.   READLS PROGRAM SPECIFICATION
!
! 1.1 Look up a station's geodetic latitude and elevation above
!     the geoid, in tables of values from DBCAL (DBCL) and a
!     simulation of DBCAL (DSIM). The simulation program is called
!     REFIN. The tables will include all the stations listed in
!     #BLOKQ, whether fixed or mobile.
!
! 1.2 REFERENCES:
!
! 2.  READLS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 INAME(4)
!
! INAME - Name of station to be looked up
!
! 2.3 OUTPUT Variables:
!
      REAL*8 LAT,SITHIT
!
! LAT - Geodetic latitude
! SITHIT - Elevation above the geoid
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 MAX_ST,j,k
      PARAMETER (MAX_ST=78)
      INTEGER*2 NAMES(4,MAX_ST),I,NUM
      CHARACTER*8 CNAMES(MAX_ST)
      character*80 bufstr
      EQUIVALENCE (NAMES,CNAMES)
      REAL*8 LATITUDES(MAX_ST)
      REAL*8 HEIGHTS(MAX_ST)
!
! CNAMES(NAMES) - Table of station names
! HEIGHTS - Table of elevations above the geoid (meters)_
! LATITUDES - Table of geodetic latitudes (radians)
! I - Loop index
! MAX_ST - Maximum number of stations allowed
! NUM - Index of station found in list, used for indexing HEIGHTS and LATITUDES
!
!                                       REFERENCE
      DATA CNAMES /'ALGOPARK','CHLBOLTN', &   ! DSIM,DSIM  (Fixed stations)
     &            'EFLSBERG','GILCREEK', &   ! DSIM,DBCL
     &            'HATCREEK','HAYSTACK', &   ! DBCL,DSIM
     &            'OVR 7853','HRAS 085', &   ! fake,DBCL
     &            'KASHIMA ','KAUAI   ', &   ! DBCL,DBCL
     &            'KWAJAL26','MARPOINT', &   ! DSIM,DSIM
     &            'MOJAVE12','NRAO 140', &   ! DBCL,DSIM
     &            'ONSALA60','OVRO 130', &   ! DBCL,DBCL
     &            'PENTICTN','PLATTVIL', &   ! DSIM,DSIM
     &            'RICHMOND','ROBLED32', &   ! DBCL,DSIM
     &            'MOJ 7287','VNDNBERG', &   ! fake,DBCL
     &            'VNDNBRG1','WESTFORD', &   ! DBCL,DBCL
     &            'WETTZELL','YELLOWKN', &   ! DBCL,DSIM
     &            'ALASKANO','BLKBUTTE', &   ! DSIM,DSIM  (MOBILE STATIONS)
     &            'CEBRER26','DEADMANL', &   ! DSIM,DSIM
     &            'DSS45   ','ELY     ', &   ! DSIM,DSIM
     &                       'FLAGSTAF', &   !      DSIM
     &            'FORT ORD','GOLDECHO', &   ! DSIM,DSIM
     &            'GOLDMARS','GOLDPION', &   ! DSIM,DSIM
     &            'GOLDVENU','GORMAN  ', &   ! DSIM,DSIM
     &            'DSS15   ','HARTRAO ', &   !      DSIM
     &            'JPL MV1 ','JPL MV2 ', &   ! DSIM,DSIM
     &            'JPL MV3 ','KODIAK  ', &   ! DSIM,DSIM
     &            'LAJOLLA ','MADRID64', &   ! DSIM,DSIM
     &                       'MALIBU  ', &   !      DSIM
     &            'MAMMOTHL','MON PEAK', &   ! DSIM,DSIM
     &            'NOME    ','OCOTILLO', &   ! DSIM,DSIM
     &            'ONSALA85','OTAY    ', &   ! DSIM,DSIM
     &                       'OVRO 90 ', &   !      DSIM
     &            'PBLOSSOM','PINFLATS', &   ! DSIM,DSIM
     &            'PRESIDIO','PT REYES', &   ! DSIM,DSIM
     &            'PVERDES ','QUINCY  ', &   ! DSIM,DSIM
     &                       'SADDLEPK', &   !      DSIM
     &            'SANFRANC','SANPAULA', &   ! DSIM,DSIM
     &            'SHANGHAI','SNDPOINT', &   ! DSIM,DSIM
     &            'SOURDOGH','TIDBIN64', &   ! DSIM,DSIM
     &                       'VACAVILL', &   !      DSIM
     &            'VERNAL  ','VLA     ', &   ! DSIM,DSIM
     &            'WEEMAL26','WERTHOVN', &   ! DSIM,DSIM
     &            'WHTHORSE','YAKATAGA', &   ! DSIM,DSIM
     &                       'YUMA    ', &   !      DSIM
     &            'MEDICINA','SESHAN25'/  ! DSIM,DSIM  (added)
!
      DATA LATITUDES /    .8020747590D0,      .8926485001D0, &  !Fixed stations
     &                    .8818245416D0,     1.1340875380D0, &
     &                    .7123971466D0,      .7439168430D0, &
     &                    .6498117046  ,      .5347118294D0, &  !OVR 7853 faked
     &                    .6275179532D0,      .3861769317D0, &
     &                    .1640393188D0,      .6697569763D0, &
     &                    .6166534544D0,      .6708665955D0, &
     &                   1.0017461692D0,      .6498117046D0, &
     &                    .8608418352D0,      .7013221504D0, &
     &                    .4470444475D0,      .7056146089D0, &
     &                    .6166534544  ,      .6031175024D0, &  !MOJ 7287 faked
     &                    .6031175024D0,      .7437362236D0, &
     &                    .8577420796D0,     1.0904706490D0, &
     &                   1.1340876130D0,      .5875434060D0, &  !MOBILE STATIONS
     &                    .7060391504D0,      .5978628459D0, &
     &                   -.6178199242D0,      .6857954981D0, &
     &                                        .6146125539D0, &
     &                    .6400090275D0,      .6161001869D0, &
     &                    .6182987347D0,      .6176637224D0, &
     &                    .6151885690D0,      .6091823061D0, &
     &                    .6182987347  ,     -.4518614592D0, &  !DSS15 faked
     &                    .5969907834D0,      .5969927852D0, &
     &                    .5969865931D0,     1.0077532570D0, &
     &                    .5736125985D0,      .7056577401D0, &
     &                                        .5944471895D0, &
     &                    .6569708912D0,      .5740687833D0, &
     &                   1.1268325290D0,      .5722953734D0, &
     &                   1.0016978900D0,      .5689898308D0, &
     &                                        .6498118191D0, &
     &                    .6023508750D0,      .5865915968D0, &
     &                    .6598271627D0,      .6650321768D0, &
     &                    .5889398770D0,      .6976880464D0, &
     &                                        .5947803014D0, &
     &                    .6598277272D0,      .6001816779D0, &
     &                    .5443716150D0,      .9660806199D0, &
     &                   1.0936918990D0,     -.6178891375D0, &
     &                                        .6697713610D0, &
     &                    .7038384883D0,      .5947923056D0, &
     &                   -.6178586163D0,      .8834264300D0, &
     &                   1.0596113230D0,     1.0486197110D0, &
     &                                        .5748965113D0, &
     &                    .7770290604D0,      .5427830483D0/  !added
!
      DATA HEIGHTS /            217.4D0,            140.4D0, & !Fixed stations
     &                          410.2D0,            324.4D0, &
     &                         1001.7D0,            110.2D0, &
     &                         1193.   ,           1587.9D0, & !OVR 7853 faked
     &                           71.0D0,           1159.3D0, &
     &                           47.6D0,            -20.1D0, &
     &                          902.8D0,            805.9D0, &
     &                           52.7D0,           1193.4D0, &
     &                          525.3D0,           1498.4D0, &
     &                          -20.2D0,            832.2D0, &
     &                          902.   ,            -13.3D0, &  !MOJ 7287 faked
     &                          -13.3D0,             80.3D0, &
     &                          662.6D0,            174.1D0, &
     &                          324.8D0,            484.7D0, &  !Mobile stations
     &                          779.6D0,            829.9D0, &
     &                          666.4D0,           1878.8D0, &
     &                                             2141.1D0, &
     &                           20.6D0,            954.0D0, &
     &                          992.9D0,            998.9D0, &
     &                         1055.4D0,           1431.9D0, &
     &                          992.   ,           1408.6D0, &  !DSS15 faked
     &                          416.2D0,            416.9D0, &
     &                          416.2D0,             26.6D0, &
     &                           41.5D0,            856.6D0, &
     &                                              442.5D0, &
     &                         2306.1D0,           1835.8D0, &
     &                          328.3D0,            -41.2D0, &
     &                           51.9D0,            988.2D0, &
     &                                             1183.4D0, &
     &                          887.8D0,           1234.6D0, &
     &                          -34.1D0,             -5.7D0, &
     &                           68.7D0,           1102.6D0, &
     &                                              814.0D0, &
     &                          -18.2D0,            180.1D0, &
     &                            9.1D0,             87.7D0, &
     &                          745.3D0,            681.2D0, &
     &                                                6.7D0, &
     &                         1583.1D0,           2133.0D0, &
     &                          666.8D0,            328.5D0, &
     &                          705.5D0,             16.6D0, &
     &                                              233.9D0, &
     &                           60.7D0,             20.2D0/  !added
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  870707  Created
!   JWR  880211  OVR 7853 added with OVRO 130 values (a kluge)
!                MOJ 7287 added with MOJAVE12 values (a kluge)
!                DS15 added with GOLDMARS values (a kluge)
!
! 5.  READLS PROGRAM STRUCTURE
!
! Find which slot in the table this station occupies.
!
      NUM = 0
      DO I = 1, MAX_ST
        DO J = 1,4
          IF (INAME(J) .NE. NAMES(J,I)) GO TO 50
        END DO
        NUM = I
        GO TO 100
 50   END DO
!
! If station name was found, pull out corresponding latitude and height
!
 100  IF (NUM .NE. 0) THEN
        LAT = LATITUDES(NUM)
        SITHIT = HEIGHTS(NUM)
      ELSE
        WRITE(bufstr, &
     &    "('COULD NOT FIND NAME ',4A2,' IN NAME TABLE')")(INAME(K),K=1,4)
        call ferr( INT2(209), 'READLS ERROR: '//bufstr, INT2(0), INT2(0) )
      END IF
!
      RETURN
      END
