      SUBROUTINE PICK_REFCLO ( NUMSTA, SITNAMES, STA_BRK, BM_REF_CL, &
     &                         CLOCK_REF_BITS )
! ************************************************************************
! *                                                                      *
! *   Routine  PICK_REFCLO  select clock reference station(s) in the     *
! *   interactive mode. It asks user to enter the number of clock        *
! *   reference satation. If more than one reference station is used     *
! *   then station numbers should be entered seprated by blank or comma. *
! *                                                                      *
! *   Deselected stations or stations with clock breaks cannot be set up *
! *   as clock reference stations.                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   NUMSTA  ( INTEGER*2 ) -- total number of stations in the database. *
! *  SITNAMES ( CHARACTER ) -- array of staation names.                  *
! *   STA_BRK ( LOGICAL*4 ) -- Array of attribute of clock breaks at     *
! *                            the stations. IF STA_BRK(I) is TRUE then  *
! *                            I-th station has clock break.             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   BM_REF_CL     ( INTEGER*2 ) -- The number of the clock reference   *
! *                                  station. 0 value is acceptable.     *
! *                                  If more than one stations were      *
! *                                  chosed as reference stations        *
! *                                  BM_REF_CL is the the the number of  *
! *                                  the last chosen station.            *
! *  CLOCK_REF_BITS ( INTEGER*2 ) -- Array of bits for the clock         *
! *                                  reference stations. If the I-th bit *
! *                                  in bit field CLOCK_REF_BITS is on   *
! *                                  then I-th stations is used as clock *
! *                                  reference. Bits are counted from 1. *
! *                                                                      *
! *  ###  17-JAN-1998  PICK_REFCLO  v1.1  (c) L. Petrov 20-SEP-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*2  NUMSTA, BM_REF_CL, CLOCK_REF_BITS(ARC_STA_BIT_WORDS)
      CHARACTER  SITNAMES(NUMSTA)*8, STR*255, MES*128, REG*6, ST2*10
      LOGICAL*4  STA_BRK(NUMSTA), CHECK_STABIT
      INTEGER*4  MIND, ISTA, J1, J2, J3
      PARAMETER  ( MIND = 1024 )
      INTEGER*4  LIND, IND(2,MIND), IUER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REG = CHAR(0)//' ,/|\' ! delimiters
!
 910  CONTINUE
!
! --- Initialisation
!
      CLOCK_REF_BITS = 0
!
! --- Infinte cycle to prompt user to enter the text
!
      DO 410 J1=1,128
         CALL ADDSTR_F ( "Pick a reference station(s). Enter 0 for default, "// &
     &                   "or station number(s) >> " )
!
! ------ Getting the line form the keyboard
!
         CALL CLRCH    ( STR )
         CALL GETSTR_F ( STR )
!
! ------ If nothing has been entered
!
         IF ( ILEN(STR) .EQ. 0 ) STR='0'
!
! ------ Parsing it at words
!
         CALL EXWORD ( STR, MIND, LIND, IND, REG, IUER )
!
! ------ Cycles on words
!
         DO 420 J2=1,LIND
!
! --------- Transformation CHARACTER  --> INTEGER*4
!
            CALL CHIN ( STR(IND(1,J2):IND(2,J2)), ISTA )
            IF ( ISTA .LT. 0  .OR.  ISTA .GT. NUMSTA ) THEN
!
! -------------- Check the validity of the number of the stations
!
                 CALL CLRCH ( MES )
                 CALL CLRCH ( ST2 )
                 CALL INCH  ( INT4(NUMSTA), ST2 )
                 MES = 'Number of the station "'//STR(IND(1,J2):IND(2,J2))// &
     &                 '" is invalid since it is out of range [1,'// &
     &                  ST2(1:I_LEN(ST2))//']'
                 CALL NL_MN()
                 CALL ADDSTR_F ( MES(1:I_LEN(MES)) )
                 CALL NL_MN()
                 GOTO 410
            END IF
!
            IF ( .NOT. CHECK_STABIT ( INT2(ISTA) ) ) THEN
!
! -------------- Station was deselected
!
                 CALL ADDSTR_F ( 'Station '//SITNAMES(ISTA)//' is deslected '// &
     &               'from solution and cannot be used as reference station' )
                 CALL NL_MN()
                 GOTO 410
            END IF
!
            IF ( ISTA .EQ. 0  .AND.  LIND .GT. 1 ) THEN
                 CALL ADDSTR_F ( 'Station 0 cannot be put in the list of '// &
     &               'clock reference stations' )
                 CALL NL_MN()
                 GOTO 410
            END IF
!
            IF ( ISTA .GT. 0  ) THEN
                 IF ( STA_BRK(ISTA) ) THEN
!
! ------------------- Station has clock breaks
!
                      CALL ADDSTR_F ( 'Station '//SITNAMES(ISTA)//' has '// &
     &                    'clock breaks and cannot be used as reference '// &
     &                    'station' )
                      CALL NL_MN()
                      GOTO 410
                 END IF
!
                 BM_REF_CL = ISTA
                 CALL SBIT ( CLOCK_REF_BITS, BM_REF_CL, INT2(1) )
            END IF
 420     CONTINUE
         GOTO 810
 410  CONTINUE
 810  CONTINUE
!
      IF ( LIND .GT. 1 ) THEN
!
! -------- Printing the message for confirming the choice of clok reference
! -------- station in multi-clock reference mode
!
           CALL CLRCH ( ST2 )
           CALL INCH  ( LIND, ST2 )
           CALL CLRCH ( MES )
           MES = ST2(1:I_LEN(ST2))//' clock reference stations are used: '
           DO 430 J3=1,LIND
              CALL CHIN ( STR(IND(1,J3):IND(2,J3)), ISTA )
              MES(I_LEN(MES)+2:) = SITNAMES(ISTA)
 430       CONTINUE
           CALL ADDSTR_F ( MES(1:I_LEN(MES)) )
           CALL NL_MN()
           CALL ADDSTR_F ( 'Please confirm you choice (Y/N) [N] ' )
           CALL GETSTR_F ( STR )
           IF ( STR(1:1) .NE. 'Y'  .AND.  STR(1:1) .NE. 'y' ) THEN
!
! ------------- The choice has not been confirmed. Let's start the business
! ------------- again
!
                CALL NL_MN()
                GOTO 910
           END IF
      END IF
!
      RETURN
      END  !#!  PICK_REFCLO  #!#
