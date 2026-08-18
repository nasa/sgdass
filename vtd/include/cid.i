!
! >>>>> cid.i   2006.01.06   v 0.95  --  2025.06.30_20:39:47
!
        INTEGER*4    CID__MSTA, CID__MMOD
        PARAMETER  ( CID__MSTA = 128 )
        PARAMETER  ( CID__MMOD = 128*1024 )
        CHARACTER    CID__MOD_VERA1000*8, CID__MOD_VERA2000*8
	PARAMETER  ( CID__MOD_VERA1000 = 'VERA1000' )
	PARAMETER  ( CID__MOD_VERA2000 = 'VERA2000' )
!
        TYPE     CODA_IFMOD__HEADER
            CHARACTER  VERSION*4
            CHARACTER  EXP_NAME*32
            INTEGER*4  NPOLY
            INTEGER*4  VERS
            INTEGER*4  NSS
            INTEGER*4  NDAT
            INTEGER*4  IND_ANT
            INTEGER*4  STA_IND
        END TYPE CODA_IFMOD__HEADER
!
        TYPE     CODA_IFMOD__DATA
           REAL*8     MJD_R8
           REAL*8     PP
           CHARACTER  SOU_NAM*32
           INTEGER*4  ANT_NUM
           REAL*8     FAR_ROT
           REAL*8     PDELAY
           REAL*8     GDELAY
           REAL*8     PRATE
           REAL*8     GRATE
           REAL*8     DDELAY
           REAL*8     DRATE
           REAL*8     P2ND
           REAL*8     G2ND
           REAL*8     P3RD
           REAL*8     G3RD
           REAL*8     P4TH
        END TYPE     CODA_IFMOD__DATA
