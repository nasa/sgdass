      PROGRAM    PLOT_STA_TABLE
! ************************************************************************
! *                                                                      *
! *   PRogram PLOT_STA_TABLE
! *                                                                      *
! *  ### 11-MAR-2014 PLOT_STA_TABLE v1.1 (c)  L. Petrov  01-DEC-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      INTEGER*4  MP, MIND
      PARAMETER  ( MP    = 64*1024 )
      PARAMETER  ( MIND  =      64 )
      CHARACTER  FILIN*128, FILOUT*128, BUF(MP)*256, ZAG*128, UNIT*128, &
     &           SDS_NAME*128, STR_YMIN*32, STR_YMAX*32
      REAL*8     TIM, TIM_BEG, TIM_ARR(MP), VAL_ARR(MP), YMIN, YMAX, &
     &           VAL_MIN, VAL_MAX
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, &
     &           ICL1, ICL2, ICL3, DIAGI_LEN
      INTEGER*4  NP, J1, J2, J3, J4, MJD, MJD_BEG, LIND, IND(2,MIND), IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: plot_sta_table filin filout [ymin] [ymax]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN  ) 
           CALL GETARG ( 2, FILOUT ) 
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, STR_YMIN ) 
                IF ( INDEX ( STR_YMIN, '.' ) < 1 ) STR_YMIN = TRIM(STR_YMIN)//'.0'
                READ ( UNIT=STR_YMIN, FMT='(F10.5)' ) YMIN
              ELSE 
                CALL CLRCH ( STR_YMIN )
           END IF
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, STR_YMAX ) 
                IF   ( INDEX ( STR_YMAX, '.' ) < 1 ) STR_YMAX = TRIM(STR_YMAX)//'.0'
                READ ( UNIT=STR_YMAX, FMT='(F10.5)' ) YMAX
              ELSE 
                CALL CLRCH ( STR_YMAX )
           END IF
      END IF 
      IUER = -1
      CALL RD_TEXT ( FILIN, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 410 J1=1,NP
         IUER = -1
         CALL DATE_TO_TIME ( BUF(J1)(1:19), MJD, TIM, IUER )
         IF ( J1 == 1 ) THEN
              MJD_BEG = MJD
              TIM_BEG = TIM
         END IF
         TIM_ARR(J1) = (MJD - MJD_BEG) + (TIM - TIM_BEG)/86400.0D0
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         READ ( UNIT=BUF(J1)(IND(1,7):IND(2,7)), FMT='(F10.5)' ) VAL_ARR(J1)
         IF ( J1 == 1 ) THEN
              VAL_MIN = VAL_ARR(J1)
              VAL_MAX = VAL_ARR(J1)
            ELSE 
              VAL_MIN = MIN ( VAL_ARR(J1), VAL_MIN ) 
              VAL_MAX = MAX ( VAL_ARR(J1), VAL_MAX ) 
         END IF
         SDS_NAME = BUF(J1)(IND(1,8):IND(2,LIND))//' at '//BUF(J1)(IND(1,2):IND(2,5))
         IF ( SDS_NAME(1:15) == 'Air temperature'  ) SDS_NAME = 'Air temp '//SDS_NAME(17:)
         IF ( SDS_NAME(1:17) == 'relative humidity' ) SDS_NAME = 'Rel. hum. '//SDS_NAME(19:)
 410  CONTINUE 
!
! --- Clear DIAGI_S object
!
      DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL NOUT ( DIAGI_LEN, DIAGI_S )
!
! --- Setting defaults values of the plotting parameters
!
      IUER = -1
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4151, IUER, 'PLOT_STA_TABLE', 'Error in setting '// &
     &         'default values for the plot' )
           CALL EXIT ( 1 )
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%NCLR      = 1
      DIAGI_S%NPOI(1)   = NP
      DIAGI_S%ADR_X8(1) = LOC(TIM_ARR)
      DIAGI_S%ADR_Y8(1) = LOC(VAL_ARR)
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = ILST
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = 5
      DIAGI_S%IWST(1)   = IWST
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = TIM_ARR(1)  - DIAGI_FIE*(TIM_ARR(NP) - TIM_ARR(1))
      DIAGI_S%XMAX      = TIM_ARR(NP) + DIAGI_FIE*(TIM_ARR(NP) - TIM_ARR(1))
      IF ( ILEN(STR_YMIN) == 0 ) THEN
           DIAGI_S%YMIN = 1.0
           DIAGI_S%YMAX = 0.0
         ELSE IF ( ILEN(STR_YMAX) == 0 ) THEN
           DIAGI_S%YMIN = YMIN
           DIAGI_S%YMAX = VAL_MAX + DIAGI_FIE*(VAL_MAX - YMIN)
         ELSE
           DIAGI_S%YMIN = YMIN
           DIAGI_S%YMAX = YMAX
      END IF
      DIAGI_S%ZAG       = SDS_NAME
      DIAGI_S%NAME      = FILOUT
      DIAGI_S%ARG_UNITS = 'Time in days since '//BUF(1)(1:16)
      DIAGI_S%ITRM      = 0
      DIAGI_S%STATUS    = DIA__DEF
      IF ( FILOUT(1:3) == '/xw' .OR. FILOUT(1:3) == '/XW' ) THEN
           DIAGI_S%IBATCH = 0
           DIAGI_S%IDEV   = IDEV
         ELSE 
           DIAGI_S%IBATCH = 1
           IF ( INDEX ( FILOUT, '.ps' ) > 0 ) THEN
                DIAGI_S%IDEV = 6
              ELSE IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
                DIAGI_S%IDEV = 9
              ELSE 
                CALL ERR_LOG ( 4152, IUER, 'PLOT_STA_TABLE', 'Wrong extnesion '// &
     &              'in the output file '//FILOUT(1:I_LEN(FILOUT))// &
     &              ' . Supported extensions: .gif, .ps' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Calling the main routine of DiaGI
!
      IUER = -1
      CALL DIAGI ( DIAGI_S, IUER )
!
      END  !#!#  PROGRAM    PLOT_STA_TABLE  #!#!
