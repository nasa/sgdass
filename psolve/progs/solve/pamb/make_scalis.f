      SUBROUTINE MAKE_SCALIS ( L_OBS, LIS_OBS, DBOBJ, OBSBAS, L_STA, LIS_STA, &
     &                         L_BAS, LIS_BAS, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine  MAKE_SCALIS  scans the list of observations for *
! *   the same scan, builds the list of stations, baselines and then     *
! *   sorts them.                                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   L_OBS ( INTEGER*4 ) -- Number of observations in the list LIS_OBS. *
! * LIS_OBS ( INTEGER*4 ) -- List of observations which belong to the    *
! *                          the same scan. The k-th element of the list *
! *                          LIS_OBS is the index of the observation in  *
! *                          the database to be under consideration.     *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *  OBSBAS ( RECORD    ) -- Array of data structures which keeps        *
! *                          baseline dependent information about        *
! *                          the session.                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   L_STA ( INTEGER*4 ) -- Number of stations which participated in    *
! *                          the observations from the list LIS_OBS.     *
! * LIS_STA ( INTEGER*4 ) -- The list of stations which participated in  *
! *                          the observations from the list LIS_OBS.     *
! *                          The list is sorted in increasing the        *
! *                          station codes.                              *
! *   L_BAS ( INTEGER*4 ) -- Number of baselines which participated in   *
! *                          the observations from the list LIS_OBS.     *
! * LIS_BAS ( INTEGER*4 ) -- The list of baselines which participated in *
! *                          the observations from the list LIS_OBS.     *
! *                          The list is sorted in increasing the        *
! *                          station codes.                              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  02-OCT-98   MAKE_SCALIS  v1.0  (c)  L. Petrov  02-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      INTEGER*4  L_OBS, LIS_OBS(L_OBS), L_STA, LIS_STA(MO_STA), &
     &           L_BAS, LIS_BAS(MO_BAS), IUER
!
      CHARACTER  STR*20, STR1*20, STR2*20
      INTEGER*4  J1, ISTA1, ISTA2, IBAS, IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: NSTBA, I_LEN
!
      IF ( L_OBS .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( L_OBS, STR )
           CALL ERR_LOG ( 5301, IUER, 'MAKE_STALIS', 'Wrong value of '// &
     &         'parameter L_OBS: '//STR )
           RETURN
      END IF
!
      L_STA = 0
      L_BAS = 0
!
! --- Scan all observations from the list
!
      DO 410 J1=1,L_OBS
         IF ( LIS_OBS(J1) .LE. 0  .OR.  LIS_OBS(J1) .GT. DBOBJ%L_OBS ) THEN
              CALL CLRCH ( STR  )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1  )
              CALL INCH  ( LIS_OBS(J1), STR1 )
              CALL CLRCH ( STR2 )
              CALL INCH  ( DBOBJ%L_OBS, STR2 )
              CALL ERR_LOG ( 5302, IUER, 'MAKE_STALIS', 'Wrong value of '// &
     &            'the '//STR(1:I_LEN(STR))//'-th parameter if the array '// &
     &            'LIS_OBS: '//STR1(1:I_LEN(STR1))//' what is out of the '// &
     &            'range of valid values: [1, '//STR2(1:I_LEN(STR2))//'] ' )
              RETURN
         END IF
!
         ISTA1 = INT4( OBSBAS(LIS_OBS(J1))%ISITE(1) )
         ISTA2 = INT4( OBSBAS(LIS_OBS(J1))%ISITE(2) )
         IBAS  = NSTBA ( ISTA1, ISTA2 )
         IF ( OBSBAS(LIS_OBS(J1))%IND_SCA .NE. OBSBAS(LIS_OBS(1))%IND_SCA ) THEN
              WRITE ( 6, * ) 'J1=',J1,' LIS_OBS(1)=',LIS_OBS(1),' LIS_OBS(J1)=', &
     &                LIS_OBS(J1)
              WRITE ( 6, * ) ' OBSBAS(LIS_OBS(J1))%IND_SCA = ', &
     &                 OBSBAS(LIS_OBS(J1))%IND_SCA, &
     &               ' OBSBAS(LIS_OBS(1))%IND_SCA = ', &
     &                 OBSBAS(LIS_OBS(1))%IND_SCA
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5302, IUER, 'MAKE_SCALIS', STR(1:I_LEN(STR))// &
     &            '-th observation of the list LIS_OBS belongs to another '// &
     &            'scan than the first' )
              RETURN
         END IF
!
! ------ Add the station code of the first station to the list of stations
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADD_LIS  ( MO_STA, L_STA, LIS_STA, ISTA1, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5303, IUER, 'MAKE_SCALIS', 'Error in '// &
     &                 'adding a station to the list of stations' )
              RETURN
         END IF
!
! ------ Add the station code of the second to the list of stations
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADD_LIS  ( MO_STA, L_STA, LIS_STA, ISTA2, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5304, IUER, 'MAKE_SCALIS', 'Error in '// &
     &            'adding a station to the list of stations' )
              RETURN
         END IF
!
! ------ Add the baseline code to the list of baselines
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADD_LIS  ( MO_BAS, L_BAS, LIS_BAS, IBAS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5305, IUER, 'MAKE_SCALIS', 'Error in '// &
     &            'adding a baseline to the list of baselines' )
              RETURN
         END IF
 410  CONTINUE
!
! --- Sorting baselines in increasing order of modules of baseline indices
!
      CALL SORT_IA ( L_BAS, LIS_BAS )
!
! --- Sorting list of statitions
!
      CALL SORT_I  ( L_STA, LIS_STA )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MAKE_SCALIS  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION CBAST_NUM ( IBAS )
! ************************************************************************
! *                                                                      *
! *   Character function  CBAST_NUM  returns a string with numerical     *
! *   codes of the stations forming the baselines IBAS. Line is adjusted *
! *   to the left edge. Station codes are enclosed in parenthesis and    *
! *   separated by comma.                                                *
! *                                                                      *
! *  ###  06-OCT-98    CBAST_NUM   v1.0  (c)  L. Petrov  06-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  CBAST_NUM*9
      INTEGER*4  IBAS, IST1, IST2, IL
      INTEGER*4, EXTERNAL :: ILEN
!
      CALL NBAST ( IBAS, IST1, IST2 )
      CALL CLRCH ( CBAST_NUM )
      CBAST_NUM(1:1) = '('
      IL=2
      CALL INCH ( IST1, CBAST_NUM(IL:) )
      CALL CHASHL ( CBAST_NUM )
      IL = ILEN(CBAST_NUM)+1
      CBAST_NUM(IL:IL) = ','
      IL = ILEN(CBAST_NUM)+1
      CALL INCH ( IST2, CBAST_NUM(IL:) )
      CALL CHASHL ( CBAST_NUM )
      IL = ILEN(CBAST_NUM)+1
      IF ( IL .GT. LEN(CBAST_NUM) ) IL = LEN(CBAST_NUM)
      CBAST_NUM(IL:IL) = ')'
!
      RETURN
      END  !#!  CBAST_NUM  #!#
