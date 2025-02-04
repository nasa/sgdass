      SUBROUTINE AGD_PARSE ( AGD_FILE, AGD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine AGD_PARSE  pasrses the file with information about antenna *
! *   gravity deformations.                                              *
! *                                                                      *
! *  ### 25-APR-2008   AGD_PARSE   v1.0 (c)  L. Petrov  08-AUG-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'agd.i'
      CHARACTER  AGD_FILE*(*)
      INTEGER*4  IUER
      TYPE     ( AGD_DATA__TYPE ) :: AGD
      REAL*8     VAL, TMP_ARR(AGD__M_POI)
      CHARACTER  C_STA(AGD__M_STA)*8, STR*128
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      LOGICAL*4  LEX
      INTEGER*4  MBUF, NBUF, IND_ANT, J1, J2, J3, J4, J5, IER
#ifdef SUN
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
#endif
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF 
!
      CALL NOUT ( SIZEOF(AGD), AGD ) 
!
      INQUIRE ( FILE=AGD_FILE, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3841, IUER, 'AGD_PARSE', 'Cannot find file '// &
     &         'with antenna information '//AGD_FILE )
           RETURN 
      END IF
!
      MBUF = 256*AGD__M_POI 
      ALLOCATE ( BUF(MBUF), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 256*MBUF, STR )
           CALL ERR_LOG ( 3842, IUER, 'AGD_PARSE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'a temporary buffer' )
           RETURN 
      END IF
!
! --- Read the AGD file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( AGD_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL ERR_LOG ( 3843, IUER, 'AGD_PARSE', 'Failure in reading '// &
     &         'the file with antenna information '//AGD_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Check the first line for the label. It should present, otherwise 
! --- the file is considered unrecognized
!
      IF ( BUF(1)(1:LEN(AGD__LABEL)) == AGD__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL CLRCH ( STR )
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 3844, IUER, 'AGD_PARSE', 'Input file '// &
     &          AGD_FILE(1:I_LEN(AGD_FILE))//' has wrong format: '// &
     &         'its first line is '//STR(1:LEN(AGD__LABEL))// &
     &         ' while '//AGD__LABEL//' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 3845, IUER, 'AGD_PARSE', 'File with antenna '// &
     &         'information '//AGD_FILE(1:I_LEN(AGD_FILE))//' is damaged '// &
     &         'the last line should have a footer, but it contains '// &
     &         'something different' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- First pass throught the file and collect the infromation about 
! --- station names
!
      AGD%N_ANT = 0
      DO 410 J1=2,NBUF-1
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '!' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( BUF(J1)(1:3) == 'AGD' ) THEN
              IND_ANT = ADD_CLIST ( AGD__M_STA, AGD%N_ANT, C_STA, &
     &                              BUF(J1)(6:13), IER )
              AGD%N_ANT = IND_ANT 
         END IF
 410  CONTINUE 
!
      IF ( AGD%N_ANT == 0 ) THEN
!
! -------- No stations was found? Nothing to do!
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      ALLOCATE ( AGD%STA_NAM(AGD%N_ANT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*AGD%N_ANT, STR )
           CALL ERR_LOG ( 3846, IUER, 'AGD_PARSE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'AGD%STA_NAM' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
#ifdef SUN
      CALL LIB$MOVC3 ( 8*AGD%N_ANT, %VAL(LOC__SUN$$_STR(C_STA)), 
                       %VAL(LOC__SUN$$_STR(AGD%STA_NAM)) )
#else
      CALL LIB$MOVC3 ( 8*AGD%N_ANT, %REF(C_STA), %REF(AGD%STA_NAM) )
!!      AGD%STA_NAM = C_STA
#endif
!
      ALLOCATE ( AGD%INFO(AGD%N_ANT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(AGD%INFO(1))*AGD%N_ANT, STR )
           CALL ERR_LOG ( 3847, IUER, 'AGD_PARSE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'AGD%INFO' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Initialization the datastructure with information about AGD
!
      CALL NOUT ( AGD%N_ANT*SIZEOF(AGD%INFO(1)), AGD%INFO )
!
      DO 420 J2=2,NBUF-1
         IF ( BUF(J2)(1:1)  == '#' ) GOTO 420
         IF ( BUF(J2)(1:1)  == '!' ) GOTO 420
         IF ( ILEN(BUF(J2)) ==  0  ) GOTO 420
         IF ( BUF(J2)(1:3) == 'AGD' ) THEN
              IND_ANT = LTM_DIF ( 1, AGD%N_ANT, AGD%STA_NAM, BUF(J2)(6:13) )
              AGD%INFO(IND_ANT)%NAME = BUF(J2)(6:13)
!
              IF ( BUF(J2)(16:22) == AGD__FO_PRIM_CH ) THEN
                   AGD%INFO(IND_ANT)%FOCUS_TYPE = AGD__FO_PRIM 
                 ELSE IF ( BUF(J2)(16:22) == AGD__FO_SECN_CH ) THEN
                   AGD%INFO(IND_ANT)%FOCUS_TYPE = AGD__FO_SECN 
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 3848, IUER, 'AGD_PARSE', 'Failure in '// &
     &                 'an attempt to parse antenna gravity deformation '// &
     &                 ' file '//AGD_FILE(1:I_LEN(AGD_FILE))// &
     &                 ' line '//STR(1:I_LEN(STR))//' -- unsupported '// &
     &                 ' focus type: '//BUF(J2)(16:22) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              AGD%INFO(IND_ANT)%N_POI = AGD%INFO(IND_ANT)%N_POI + 1
         END IF
 420  CONTINUE 
!
! --- Now process  the data for each antenna
!
      DO 430 J3=1,AGD%N_ANT
         ALLOCATE ( AGD%INFO(J3)%ELEV(AGD%INFO(J3)%N_POI),    STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*AGD%INFO(J3)%N_POI, STR )
              CALL ERR_LOG ( 3849, IUER, 'AGD_PARSE', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &            'AGD%INFO%ELEV' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
         ALLOCATE ( AGD%INFO(J3)%FOC_LEN(AGD%INFO(J3)%N_POI), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*AGD%INFO(J3)%N_POI, STR )
              CALL ERR_LOG ( 3850, IUER, 'AGD_PARSE', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &            'AGD%INFO%FOC_LEN' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
         ALLOCATE ( AGD%INFO(J3)%FOC_SPL(AGD%INFO(J3)%N_POI), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*AGD%INFO(J3)%N_POI, STR )
              CALL ERR_LOG ( 3851, IUER, 'AGD_PARSE', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &            'AGD%INFO%FOC_SPL' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
         AGD%INFO(J3)%N_POI = 0
 430  CONTINUE 
!
      DO 440 J4=2,NBUF-1
         IF ( BUF(J4)(1:1)  == '#' ) GOTO 440
         IF ( BUF(J4)(1:1)  == '!' ) GOTO 440
         IF ( ILEN(BUF(J4)) ==  0  ) GOTO 440
         IF ( BUF(J4)(1:3) == 'AGD' ) THEN
              IND_ANT = LTM_DIF ( 1, AGD%N_ANT, AGD%STA_NAM, BUF(J4)(6:13) )
              AGD%INFO(IND_ANT)%N_POI = AGD%INFO(IND_ANT)%N_POI + 1
!
              READ ( UNIT=BUF(J4)(25:28), FMT=*, IOSTAT=IER ) VAL
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_LOG ( 3852, IUER, 'AGD_PARSE', 'Failure to '// &
     &                 'parse the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'antenna deformation file '//AGD_FILE )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              AGD%INFO(IND_ANT)%ELEV(AGD%INFO(IND_ANT)%N_POI) = VAL*DEG__TO__RAD
!                                    
              READ ( UNIT=BUF(J4)(31:37), FMT=*, IOSTAT=IER ) VAL
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_LOG ( 3853, IUER, 'AGD_PARSE', 'Failure to '// &
     &                 'parse the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'antenna deformation file '//AGD_FILE )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              AGD%INFO(IND_ANT)%FOC_LEN(AGD%INFO(IND_ANT)%N_POI) = VAL*1.D-3
         END IF
 440  CONTINUE 
!
      DO 450 J5=1,AGD%N_ANT
         CALL ERR_PASS ( IUER, IER ) 
         CALL MAKE_SPLINE ( 3, AGD%INFO(J5)%N_POI, &
     &                         AGD%INFO(J5)%ELEV, &
     &                         AGD%INFO(J5)%FOC_LEN, &
     &                         TMP_ARR(1), TMP_ARR(AGD__M_POI), &
     &                         AGD%INFO(J5)%FOC_SPL, TMP_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3853, IUER, 'AGD_PARSE', 'Failure to '// &
     &            'compute interpolation spline for the station '// &
     &             AGD%STA_NAM(J5)//' of the antenna deformation file '// &
     &             AGD_FILE )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
 450  CONTINUE 
!
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE !#!#  AGD_PARSE  #!#!
