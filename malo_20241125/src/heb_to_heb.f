      PROGRAM    HEB_TO__HEB
! ************************************************************************
! *                                                                      *
! *   Program HEB_TO_HEB  re-grids a two-dimensional array at the sphere *
! *   in lon/lat represetnations. User specifies new dimesions that can  *
! *   be smaller or bigger the previous one. NB: program does not check  *
! *   whether the contensts of the HEB file ha a global lon/lat dataset. *
! *   If not, HEB_TO_HEB will generate *wrong results*.                  *
! *   Regridding js done with the expansion of the input datafile into   *
! *   2D 3rd degree B-spline basis and then computing new values using   *
! *   results of this expansion.                                         *
! *                                                                      *
! *   Usage: heb_to_heb input_file dim1 dim2 output_file                 *
! *                                                                      *
! *   dim1 is the new dimension along longitude;                         *
! *   dim2 is the new dimension along latitude.                          *
! *                                                                      *
! *   If the output file has extension .heb.bz2 , HEB_TO_HEB will        *
! *   generate compressed output file.                                   *
! *                                                                      *
! *  ### 22-SEP-2013   HEB_TO_HEB  v1.0 (c)  L. Petrov  22-SEP-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*128, FILOUT*128, FILTMP*128, STR*128, COMPR_COM*64, &
     &           COM_STR*512
      INTEGER*4  MAX_DIM, M__DEG
      PARAMETER  ( MAX_DIM = 16384  )
      PARAMETER  ( M__DEG  = 3      )
      LOGICAL*1  LEX
      REAL*4,    ALLOCATABLE :: ARR_R4(:,:), LAT_ARR(:), LON_ARR(:)
      INTEGER*4, ALLOCATABLE :: IND_LON(:)
      REAL*4     LAT_VAL, LON_VAL, EPS
      PARAMETER  ( EPS = 1.E-6 )
      INTEGER*8  NEW_DIMS(4)
      INTEGER*4  M_LON, M_LAT, J1, J2, J3, J4, J5, J6, IND_LAT, &
     &           IS, IL, IUER
      REAL*4,    EXTERNAL :: VAL_2D_BSPL4 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN4, RENAME, SYSTEM 
!
      NEW_DIMS = 0
      COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: heb_to_heb filin dim1 dim2 filout'
           CALL EXIT ( 1 )
         ELSE
!
! -------- Parse input arguments and perform some checks
!
           CALL GETARG ( 1, FILIN )
           INQUIRE ( FILE=FILIN, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                IUER = -2
                CALL ERR_LOG ( 6301, IUER, 'HEB_TO_HEB', 'Cannot find the '// &
     &              'input file '//FILIN )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 2, STR   )
           READ ( UNIT=STR, FMT='(I8)', IOSTAT=IUER ) NEW_DIMS(1)
           IF ( IUER .NE. 0 .OR. NEW_DIMS(1) < 1 .OR. NEW_DIMS(1) > MAX_DIM ) THEN
                IUER = -2
                CALL ERR_LOG ( 6302, IUER, 'HEB_TO_HEB', 'Error in reading '// &
     &              'the first new dimension: '//STR(1:I_LEN(STR))//' while '// &
     &              ' an integer in a range [1, 65536] was expected' )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 3, STR   )
           READ ( UNIT=STR, FMT='(I8)', IOSTAT=IUER ) NEW_DIMS(2)
           IF ( IUER .NE. 0 .OR. NEW_DIMS(2) < 1 .OR. NEW_DIMS(2) > MAX_DIM ) THEN
                IUER = -2
                CALL ERR_LOG ( 6303, IUER, 'HEB_TO_HEB', 'Error in reading '// &
     &              'the first new dimension: '//STR(1:I_LEN(STR))//' while '// &
     &              ' an integer in a range [1, 65536] was expected' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 4, FILOUT )
      END IF
!
! --- Read input file in HEB-format
!
      IUER = -1
      CALL READ_HEB ( FILIN, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6304, IUER, 'HEB_TO_HEB', 'Error in reading '// &
     &         'input heb-file '//FILIN )
           CALL EXIT ( 1 )
      END IF 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  if ( new_dims(1) == new_dims(2) ) then
!!!       heb%sds_name = 'Site displacement |Q1- c|Q1- s|Q1  c|Q1  s|O1- c|O1- s|O1  c|O1  s|P1  c|P1  s|S1  c|S1  s|K1- c|K1- s|K1  c|K1  s|K1+ c|K1+ s|N2  c|N2  s|M2- c|M2- s|M2  c|M2  s|S2  c|S2  s|K2  c|K2  s|K2+ c|K2+ s|M4  c|M4  s|'
!       heb%sds_name = 'Site displacement |SSA c|SSA s|MM  c|MM  s|MSF c|MSF s|MF  c|MF  s|MTM c|MTM s|Q1- c|Q1- s|Q1  c|Q1  s|O1- c|O1- s|O1  c|O1  s|P1  c|P1  s|S1  c|S1  s|K1- c|K1- s|K1  c|K1  s|K1+ c|K1+ s|J1  c|J1  s|J1+ c|J1+ s|2N2 c|2N2 s|MU2 c|MU2 s|N2- c|N2- s|N2  c|N2  s|M2- c|M2- s|M2  c|M2  s|LA2 c|LA2 s|L2  c|L2  s|T2  c|T2  s|S2  c|S2  s|R2  c|R2  s|K2  c|K2  s|K2+ c|K2+ s|N4  c|N4  s|S4  c|S4  s|M4  c|M4  s|M6  c|M6  s|M8  c|M8  s|'
!       goto 810
!  end if
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      IF ( HEB%DIMS(3) .NE. 1 .OR. HEB%DIMS(4) .NE. 1  ) THEN
           WRITE ( 6, * ) 'DIMS= ', HEB%DIMS
           IUER = -2
           CALL ERR_LOG ( 6305, IUER, 'HEB_TO_HEB', 'It turned out the '// &
     &         'data in the input heb-file '//FILIN(1:I_LEN(FILIN))// &
     &         ' are not two-dimensional' )
           CALL EXIT ( 1 )
      END IF
!
! --- Allocate memory for coefficents of expansion of the contents of HEB-fil into 
! --- B-spline basis.
!
      M_LAT = HEB%DIMS(2)
      M_LON = HEB%DIMS(1)+3
      ALLOCATE ( ARR_R4(1-M__DEG:M_LON,1-M__DEG:M_LAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(4)*INT8(M_LAT+M__DEG)*INT8(M_LON+M__DEG), STR )
           CALL ERR_LOG ( 6306, IUER, 'HEB_TO_HEB', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           CALL EXIT ( 1 )
      END IF
      ARR_R4 = 0.0
      ALLOCATE ( LAT_ARR(M_LAT) )
      ALLOCATE ( LON_ARR(M_LON) )
      ALLOCATE ( IND_LON(NEW_DIMS(1)) )
!
! --- Fill array ARR_R4 with the values of th input file. 
! --- I order to avoid discontinuity in 1st dervative over the last longitude
! --- bounday, we wrap three extra value over the longitude
!
      DO 410 J1=1,M_LAT
         LAT_ARR(J1) = -P2I + (J1-1)*PI__NUM/(M_LAT-1)
         DO 420 J2=1,HEB%DIMS(1)
            IF ( J1 == 1 ) THEN
                 LON_ARR(J2) = (J2-1)*PI2/M_LON
            END IF
            ARR_R4(J2,J1) = HEB%VAL(J2,J1,1,1)
 420     CONTINUE 
         DO 430 J3=1,3
            IF ( J1 == 1 ) THEN
                 LON_ARR(HEB%DIMS(1)+J3) = PI2 + (J3-1)*PI2/M_LON
            END IF
            ARR_R4(HEB%DIMS(1)+J3,J1) = HEB%VAL(J3,J1,1,1)
 430     CONTINUE 
 410  CONTINUE 
!
! --- Adjust arguments array on the edges in order to avoid
! --- wrong index due to possible rounding errors
!
      LAT_ARR(1)     = LAT_ARR(1)     - EPS
      LAT_ARR(M_LAT) = LAT_ARR(M_LAT) + EPS
      LON_ARR(1)     = LON_ARR(1)     - EPS
      LON_ARR(M_LON) = LON_ARR(M_LON) + EPS
!
! --- Perform expansion of array ARR_R4 into B-spline basis. It will be 
! --- replaced with the coefficients of B-spline expansion.
!
      IUER = -1
      CALL BSPL4_2D_CMP ( M__DEG, 0, M_LON, M_LAT, LON_ARR, LAT_ARR, ARR_R4, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6307, IUER, 'HEB_TO_HEB', 'Failure in 2D B-spline '// &
     &         'interpolation' )
           CALL EXIT ( 1 )
      END IF
!
! --- Deallocate HEB%VAL and allocate it again with the new dimensions
!
      DEALLOCATE ( HEB%VAL )
      HEB%DIMS(1) = NEW_DIMS(1)
      HEB%DIMS(2) = NEW_DIMS(2)
      ALLOCATE   ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),1,1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(4)*HEB%DIMS(2)*HEB%DIMS(1), STR )
           CALL ERR_LOG ( 6308, IUER, 'HEB_TO_HEB', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           CALL EXIT ( 1 )
      END IF
!
! --- Pre-compute indices of pivotal elements to save time
!
      DO 440 J4=1,HEB%DIMS(1) 
         LON_VAL = (J4-1)*PI2/HEB%DIMS(1) 
         IND_LON(J4) = IXMN4 ( M_LON, LON_ARR, LON_VAL )
 440  CONTINUE 
!
! --- Computing new values of HEB-data in new grid using expansion 
! --- of the old HEB%VAL over 2D B-spline basis
!
      DO 450 J5=1,HEB%DIMS(2) 
         LAT_VAL = -P2I + (J5-1)*PI__NUM/(HEB%DIMS(2)-1)
!
! ------ IND_LAT -- the index of the pivotal element along latitude
!
         IND_LAT = IXMN4 ( M_LAT, LAT_ARR, LAT_VAL )
         DO 460 J6=1,HEB%DIMS(1) 
            LON_VAL = (J6-1)*PI2/HEB%DIMS(1) 
!
! --------- IND_LON(J6) -- the index of the pivotal element along longitude
!
            HEB%VAL(J6,J5,1,1) = VAL_2D_BSPL4 ( LON_VAL, LAT_VAL, M_LON, M_LAT, &
     &                           M__DEG, IND_LON(J6), IND_LAT, LON_ARR, LAT_ARR, &
     &                           ARR_R4 )
 460     CONTINUE 
 450  CONTINUE 
      DEALLOCATE ( ARR_R4  )
      DEALLOCATE ( LAT_ARR )
      DEALLOCATE ( LON_ARR )
      DEALLOCATE ( IND_LON )
 810  CONTINUE 
!
! --- Write the output HEB-dataset into the output file FILOUT
!
      IUER = -1
      CALL WRITE_HEB ( HEB, HEB%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6309, IUER, 'HEB_TO_HEB', 'Failure to write '// &
     &         'the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      IL = ILEN(FILOUT) 
      IF ( FILOUT(IL-3:IL) == '.bz2' ) THEN
           FILTMP = FILOUT(1:IL-4) 
           IS = RENAME ( FILOUT(1:I_LEN(FILOUT))//CHAR(0), &
     &                   FILTMP(1:I_LEN(FILTMP))//CHAR(0)  )
           IF ( IS .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 6310, IUER, 'HEB_TO_HEB', 'Failure to rename '// &
     &              'the output file '//FILOUT(1:I_LEN(FILOUT))//' to the '// &
     &              'temporary file '//FILTMP )
                CALL EXIT ( 1 )
           END IF 
!
! -------- Now compress the output file 
!
           COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILTMP(1:I_LEN(FILTMP))
           IS = SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 6311, IUER, 'HEB_TO_HEB', 'Failure to compress '// &
     &              'the output file '//FILTMP )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      END  PROGRAM    HEB_TO__HEB  !#!#
