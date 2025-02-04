       SUBROUTINE PARSE_ERM( FILERM, ERM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARSE_ERM
! *                                                                      *
! *  ### 29-JUN-2021   PARSE_ERM  v3.0 (c)  L. Petrov  26-OCT-2021  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'erm.i'
      TYPE     ( ERM__TYPE   ) :: ERM
      CHARACTER  FILERM*(*)
      INTEGER*4  IUER
      INTEGER*4  M_BUF, MIND
      PARAMETER  ( M_BUF = 10*ERM__MKNOT )
      PARAMETER  ( MIND  = 64 )
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      CHARACTER  STR*256
      REAL*8     SEC, ERR_EPS
      INTEGER*4  J1, LIND, IND(2,MIND), NP, IND_CMP, IND_KNOT, IDER, MJD, IER
!
      ERR_EPS = 1.D-20
      CALL NOUT ( SIZEOF(ERM), ERM )
!
      ALLOCATE ( BUF(M_BUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_BUF, STR )
           CALL ERR_LOG ( 5141, IUER, 'PARSE_ERM', 'Error in an attempt '// &
     &         'to allocate '//TRIM(STR)//' bytes of memory for BUF' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILERM, M_BUF, BUF, NP, IER)
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5142, IUER, 'PARSE_ERM', 'Error in an reading '// &
     &         'input ERM file '//FILERM )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
      IF ( BUF(1)(1:LEN(ERM_FMT__LABEL)) .NE. ERM_FMT__LABEL ) THEN
           CALL ERR_LOG ( 5143, IUER, 'PARSE_ERM', 'Input file '// &
     &          TRIM(FILERM)//' is not recognized as a valid ERM file '// &
     &         'because it first line does not contain ERM magic' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      DO 410 J1=2,NP
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J1)(1:7) == 'ERM SOL' ) THEN
              ERM%SOL_ID = BUF(J1)(IND(1,4):IND(2,4))
           ELSE IF ( BUF(J1)(1:7) == 'ERM UZM' ) THEN
              IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'DICKMAN1993' ) THEN
                   ERM%UZM = UZT__DICKMAN1993
                ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'DICKMAN_PRINCIPLE' ) THEN
                   ERM%UZM = UZT__DICKMAN_PRINCIPLE
                ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'DICKMAN_SHORT' ) THEN
                   ERM%UZM = UZT__DICKMAN_SHORT
                ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'NONE' ) THEN
                   ERM%UZM = UZT__NONE
                ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'RE2014' ) THEN
                   ERM%UZM = UZT__RE2014
                ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'RE2014_SHORT' ) THEN
                   ERM%UZM = UZT__RE2014_SHORT
                ELSE
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5144, IUER, 'PARSE_ERM', 'Error in processing '// &
     &                 'line '//TRIM(STR)//' in the input file '//TRIM(FILERM)// &
     &                 ' -- UT1 Zonal tide model '//BUF(J1)(IND(1,4):IND(2,4))// &
     &                 ' is not supported' )
                   RETURN 
              END IF
           ELSE IF ( BUF(J1)(1:7) == 'ERM UZU' ) THEN
              IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'INTERPOLATE' ) THEN
                   ERM%UZU = UZT__INTERPOLATE
                 ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'ADD' ) THEN
                   ERM%UZU = UZT__ADD
                 ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'SUBTRACT' ) THEN
                   ERM%UZU = UZT__SUBTRACT
                 ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'NONE' ) THEN
                   ERM%UZU = UZT__NONE
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5145, IUER, 'PARSE_ERM', 'Error in processing '// &
     &                 'line '//TRIM(STR)//' in the input file '//TRIM(FILERM)// &
     &                 ' -- UT1 Zonal tide usage method '//BUF(J1)(IND(1,4):IND(2,4))// &
     &                 ' is not supported' )
                   RETURN 
              END IF
           ELSE IF ( BUF(J1)(1:7) == 'ERM APR' ) THEN
              ERM%FIL_APR_EOP = BUF(J1)(IND(1,4):IND(2,4))
           ELSE IF ( BUF(J1)(1:7) == 'ERM CNS' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), IND_CMP )
              IF ( BUF(J1)(18:26) == 'Cnstr der' ) THEN
                   CALL CHIN ( BUF(J1)(29:29), IDER )
                   READ ( UNIT=BUF(J1)(IND(1,9):IND(2,9)),   FMT='(G11.4)' ) ERM%CNS_DER_SIGMA(IDER,IND_CMP)
                 ELSE IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'MEAN' ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)),   FMT='(G11.4)' ) ERM%CNS_MEAN_SIGMA(IND_CMP)
                   READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)), FMT='(G14.7)' ) ERM%CNS_MEAN_RTP(IND_CMP)
                 ELSE IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'RATE' ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,8):IND(2,8)),   FMT='(G11.4)' ) ERM%CNS_RATE_SIGMA(IND_CMP)
                   READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)), FMT='(G14.7)' ) ERM%CNS_RATE_RTP(IND_CMP)
              END IF
           ELSE IF ( BUF(J1)(1:7) == 'ERM DIM' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), IND_CMP )
              CALL CHIN ( BUF(J1)(IND(1,8):IND(2,8)), ERM%NKNOTS(IND_CMP) )
              READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)), FMT='(F10.2)' ) ERM%TIME_EST_SPAN(IND_CMP)
           ELSE IF ( BUF(J1)(1:13) == 'ERM TIM  Beg:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I5)'   ) ERM%MJD_BEG
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F7.1)' ) ERM%TAI_BEG
           ELSE IF ( BUF(J1)(1:13) == 'ERM TIM  End:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I5)'   ) ERM%MJD_END
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F7.1)' ) ERM%TAI_END
           ELSE IF ( BUF(J1)(1:19) == 'ERM TIM  Trend Beg:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(I5)'   ) ERM%MJD_BEG_RANGE_CNS 
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F7.1)' ) ERM%TAI_BEG_RANGE_CNS
           ELSE IF ( BUF(J1)(1:19) == 'ERM TIM  Trend End:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(I5)'   ) ERM%MJD_END_RANGE_CNS 
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F7.1)' ) ERM%TAI_END_RANGE_CNS
           ELSE IF ( BUF(J1)(1:19) == 'ERM TIM  Trend Ref:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(I5)'   ) ERM%MJD_REF_CNS 
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F7.1)' ) ERM%TAI_REF_CNS
           ELSE IF ( BUF(J1)(1:7) == 'ERM EST' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,5):IND(2,5)), IND_CMP  )
              CALL CHIN ( BUF(J1)(IND(1,6):IND(2,6)), IND_KNOT )
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,8):IND(2,8)), MJD, SEC, IUER )
              ERM%TIM(IND_KNOT,IND_CMP) = (MJD - J2000__MJD)*86400.0D0 + SEC
              READ ( UNIT=BUF(J1)(IND(1,10):IND(2,10)), FMT='(G16.9)' ) ERM%APR(IND_KNOT,IND_CMP) 
              READ ( UNIT=BUF(J1)(IND(1,12):IND(2,12)), FMT='(G16.9)' ) ERM%VAL(IND_KNOT,IND_CMP) 
              READ ( UNIT=BUF(J1)(IND(1,14):IND(2,14)), FMT='(G16.9)' ) ERM%ERR(IND_KNOT,IND_CMP) 
           ELSE IF ( BUF(J1)(1:7) == 'ERM COV' ) THEN
              CONTINUE 
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_ERM  !#!  
