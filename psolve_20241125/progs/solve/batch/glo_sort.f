      SUBROUTINE GLO_SORT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_SORT  sorts the list of stations and/or sources in    *
! *   according with criterion specified by variables SORT_SOU, SORT_STA *
! *   kept in common block area glbc4.i . Lists in the common areas      *
! *   prfil.i and socom.i  are reordered.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  WHO WHEN       WHAT                                                 *
! *  pet 2002.09.27 Updated according with changes of prfil block:       *
! *                 added sorting STA_FJD_BEG, STA_FJD_END, STA_FJD_MID, *
! *                 SRC_FJD_BEG, SRC_FJD_END, SRC_FJD_MID fields.        *
! *                                                                      *
! *  ###  13-JAN-1999   GLO_SORT  v1.3  (c)  L. Petrov  01-JUN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'astro_constants.i'
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'prfil.i'
      INCLUDE    'q_prfil.i'
      INCLUDE    'socom.i'
      INCLUDE    'q_socom.i'
      INTEGER*4  IUER
      INTEGER*4  M_STA, M_SOU
      PARAMETER  ( M_STA = MAX_STA )
      PARAMETER  ( M_SOU = MAX_SOU )
      CHARACTER  C_STA(M_STA)*28, C_SOU(M_SOU)*20, STR*20, STA_DATE*8
      REAL*8     LAMBDA
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IP
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: LTM_DIF
!
      IF ( SORT_STA .EQ. NO__SRT  .AND.  SORT_SOU .EQ. NO__SRT ) THEN
!
! -------- No lists have to be sorted. Go back.
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Copy SOCOM to Q_SOCOM  and  PRFIL to Q_PRFIL
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  PI_VAR,   Q_PI_VAR   )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, VAXOF(1), Q_VAXOF(1) )
!
! --- Sort stations.
!     ~~~~~~~~~~~~~~
!
      IF ( SORT_STA .EQ. ALPHB__SRT  .OR.  SORT_STA .EQ. LONG__SRT ) THEN
!
! -------- Get the list of stations C_STA. It contains the station name in the
! -------- fields 13:20 and a key for sorting in the fields 1:12
!
           DO 410 J1=1,INT4(NUMSTA)
              CALL CLRCH ( C_STA(J1) )
              CALL CLRCH ( STA_DATE )
!
              IF ( PSITED(J1) .NE. 0 ) THEN
!
! ---------------- Set Julian date of the epoch of the node since position of
! ---------------- this station is modelled as piece-wise linear function
!
                   WRITE ( UNIT=STA_DATE(1:7), FMT='(I7)') &
     &                                         IDINT( PSITED(J1) + 0.499D0 )
                   CALL CHASHR ( STA_DATE )
              END IF
!
              IF ( VSITED(J1) .NE. 0 ) THEN
!
! ---------------- Set Julian date of the epoch of the node since position of
! ---------------- this station is modelled as a rupture function to accomodate
! ---------------- possible episodic station motion
!
                   WRITE ( UNIT=STA_DATE(1:7), FMT='(I7)') &
     &                                         IDINT( VSITED(J1) + 0.499D0 )
                   CALL CHASHR ( STA_DATE )
              END IF
!
              IF ( SORT_STA .EQ. ALPHB__SRT ) THEN
!
! ---------------- Key is a station name
!
                   C_STA(J1)(1:8) = Q_ISITN_CHR(J1)
                ELSE IF ( SORT_STA .EQ. LONG__SRT ) THEN
!
! ---------------- Computation of the longitude of the J1-th station
! ---------------- (in radians)
!
                   IF ( DABS ( VSITEC(1,J1) ) .GT. 1.D-8 ) THEN
                        LAMBDA = DATAN ( VSITEC(2,J1)/VSITEC(1,J1) )
                      ELSE
                        LAMBDA = PI__NUM/2.D0
                   END IF
                   IF ( VSITEC(1,J1)   .LT. 0.D0 ) LAMBDA = PI__NUM + LAMBDA
                   IF ( LAMBDA .LT. 0.D0 ) LAMBDA = 2.0*PI__NUM + LAMBDA
!
! ---------------- Write longitude to the 1:11 position of C_STA (in phase
! ---------------- turns) -- it will be used as a sorting key
!
                   WRITE ( UNIT=C_STA(J1)(1:11), FMT='(F11.9)' ) LAMBDA/(2*PI__NUM)
              END IF
              C_STA(J1)(13:20) = Q_ISITN_CHR(J1)
              C_STA(J1)(21:28) = STA_DATE
 410       CONTINUE
!
! -------- Sort array C_STA in according with increaing the keys
!
           CALL SORT_CH ( INT4(NUMSTA), C_STA )
!
! -------- Setting new station lists in socom, prfil
!
           DO 420 J2=1,INT4(NUMSTA)
!
! ----------- Compute IP -- index of the J2-th element in the sorted list
! ----------- in the unsorted list
!
              IP = 0
              DO 430 J3=1,INT4(NUMSTA)
                 CALL CLRCH ( STA_DATE )
!
                 IF ( Q_PSITED(J3) .NE. 0 ) THEN
                      WRITE ( UNIT=STA_DATE(1:7), FMT='(I7)') &
     &                                          IDINT( Q_PSITED(J3) + 0.499D0 )
                      CALL CHASHR ( STA_DATE )
                 END IF
!
                 IF ( Q_VSITED(J3) .NE. 0 ) THEN
                      WRITE ( UNIT=STA_DATE(1:7), FMT='(I7)') &
     &                                          IDINT( Q_VSITED(J3) + 0.499D0 )
                      CALL CHASHR ( STA_DATE )
                 END IF
!
! -------------- We compare both: station name and station date. Station date
! -------------- is empty unless station was modelled with episodic motion or
! -------------- by piece-wise linear function
!
                 IF ( Q_ISITN_CHR(J3) .EQ. C_STA(J2)(13:20) .AND. &
     &                       STA_DATE .EQ. C_STA(J2)(21:28)       ) THEN
                      IP = J3
                      GOTO 830
                 END IF
 430          CONTINUE
!
              IF ( IP .LE. 0 ) THEN
                   CALL ERR_LOG ( 4181, IUER, 'GLO_SORT', 'Trap of '// &
     &                 'internal control: station '//C_STA(J2)(13:20)// &
     &                 ' was not found in the sorted list' )
                   RETURN
              ENDIF
 830          CONTINUE
!
! ----------- Moving parameters related to the station from q_prfil to prfil
! ----------- and from q_socom to socom
!
              ISITN_CHR(J2)     = Q_ISITN_CHR(IP)
!
              VAXOF(J2)         = Q_VAXOF(IP)
!
              MONUMENTS_CHR(J2) = Q_MONUMENTS_CHR(IP)
!
              VSITED(J2)        = Q_VSITED(IP)
!
              PSITED(J2)        = Q_PSITED(IP)
!
              STA_FJD_BEG(J2)   = Q_STA_FJD_BEG(IP)
              STA_FJD_END(J2)   = Q_STA_FJD_END(IP)
              STA_FJD_MID(J2)   = Q_STA_FJD_MID(IP)
!
              DO 440 J4=1,3
                 VSITEC(J4,J2)      = Q_VSITEC(J4,IP)
                 VSITEV(J4,J2)      = Q_VSITEV(J4,IP)
!
                 IF ( KBIT(Q_LSITEC(1,J4), INT2(IP))  ) THEN
                      CALL SBIT ( LSITEC(1,J4), INT2(J2), INT2(1) )
                    ELSE
                      CALL SBIT ( LSITEC(1,J4), INT2(J2), INT2(0) )
                 END IF
!
                 IF ( KBIT(Q_LSITEV(1,J4), INT2(IP))  ) THEN
                      CALL SBIT ( LSITEV(1,J4), INT2(J2), INT2(1) )
                    ELSE
                      CALL SBIT ( LSITEV(1,J4), INT2(J2), INT2(0) )
                 END IF
!
                 IF ( KBIT(Q_LTIDE(1,J4), INT2(IP))  ) THEN
                      CALL SBIT ( LTIDE(1,J4), INT2(J2), INT2(1) )
                    ELSE
                      CALL SBIT ( LTIDE(1,J4), INT2(J2), INT2(0) )
                 END IF
 440          CONTINUE
!
              IF ( KBIT(Q_LAXOF, INT2(IP))  ) THEN
                   CALL SBIT ( LAXOF, INT2(J2), INT2(1) )
                 ELSE
                   CALL SBIT ( LAXOF, INT2(J2), INT2(0) )
              END IF
 420       CONTINUE
         ELSE IF ( SORT_STA .EQ. NO__SRT   ) THEN
           CONTINUE
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( SORT_STA, STR )
           CALL ERR_LOG ( 4182, IUER, 'GLO_SORT', 'Global parameter '// &
     &         'SORT_STA has unsupported value '//STR )
           RETURN
      END IF
!
! --- Sort sources.
!     ~~~~~~~~~~~~~
!
      IF ( SORT_SOU .EQ. ALPHB__SRT  .OR.  SORT_SOU .EQ. ASCEN__SRT ) THEN
!
! -------- Get the list of sources C_SOU. It contains the source name in the
! -------- fields 13:20 and a key for sorting in the fields 1:12
!
           DO 450 J5=1,INT4(NUMSTR)
              CALL CLRCH ( C_SOU(J5) )
              IF ( SORT_SOU .EQ. ALPHB__SRT ) THEN
                   C_SOU(J5)(1:8) = Q_ISTRN_CHR(J5) ! Key is a source name
                ELSE IF ( SORT_SOU .EQ. ASCEN__SRT ) THEN
!
! ---------------- Write right ascension in the 1:11 position of C_SOU (in phase
! ---------------- turns) -- it will be used as a sorting key
!
                   WRITE ( UNIT=C_SOU(J5)(1:11), FMT='(F11.9)') &
     &                     Q_VSTARC(1,J5)/(2*PI__NUM)
              END IF
              C_SOU(J5)(13:20) = Q_ISTRN_CHR(J5)
 450       CONTINUE
!
! -------- Sort array C_SOU in accordancewith increasing keys
!
           CALL SORT_CH ( INT4(NUMSTR), C_SOU )
!
           DO 460 J6=1,INT4(NUMSTR)
!
! ----------- Compute IP -- index of the J6-th element in the sorted list
! ----------- in the unsorted list
!
              IP = LTM_DIF ( 1, INT4(NUMSTR), Q_ISTRN_CHR, C_SOU(J6)(13:20) )
              IF ( IP .LE. 0 ) THEN
                   CALL ERR_LOG ( 4183, IUER, 'GLO_SORT', 'Trap of '// &
     &                 'internal control: source '//C_SOU(J6)(13:20)// &
     &                 ' was not found in the sorted list' )
                   RETURN
              ENDIF
!
! ----------- Moving parameters related with sources from q_prfil to prfil
! ----------- and from q_socom to socom
!
              ISTRN_CHR(J6) = Q_ISTRN_CHR(IP)
              JNAME(J6)     = Q_JNAME(IP)
!
              SRC_FJD_BEG(J6)   = Q_SRC_FJD_BEG(IP)
              SRC_FJD_END(J6)   = Q_SRC_FJD_END(IP)
              SRC_FJD_MID(J6)   = Q_SRC_FJD_MID(IP)
!
              DO 470 J7=1,2
                 VSTARC(J7,J6)  = Q_VSTARC(J7,IP)
!
                 IF ( KBIT(Q_LSTAR(1,J7), INT2(IP))  ) THEN
                      CALL SBIT ( LSTAR(1,J7), INT2(J6), INT2(1) )
                    ELSE
                      CALL SBIT ( LSTAR(1,J7), INT2(J6), INT2(0) )
                 END IF
!
                 IF ( KBIT(Q_LPROP(1,J7), INT2(IP))  ) THEN
                      CALL SBIT ( LPROP(1,J7), INT2(J6), INT2(1) )
                    ELSE
                      CALL SBIT ( LPROP(1,J7), INT2(J6), INT2(0) )
                 END IF
 470          CONTINUE
!
              IF ( KBIT(Q_ISRSEL, INT2(IP))  ) THEN
                   CALL SBIT ( ISRSEL, INT2(J6), INT2(1) )
                 ELSE
                   CALL SBIT ( ISRSEL, INT2(J6), INT2(0) )
              END IF
 460       CONTINUE
         ELSE IF ( SORT_SOU .EQ. NO__SRT ) THEN
         ELSE
           CALL CLRCH   ( STR )
           CALL INCH    ( SORT_SOU, STR )
           CALL ERR_LOG ( 4184, IUER, 'GLO_SORT', 'Global parameter '// &
     &         'SORT_SOU has unsupported value '//STR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLO_SORT  #!#
