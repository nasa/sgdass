      SUBROUTINE CHANGE_SUPMET ( F_SUPMET )
! ************************************************************************
! *                                                                      *
! *   Routine  CHANGE_SUPMET  changes suppression strategy in            *
! *   the interactive mode.                                              *
! *                                                                      *
! *  ###  30-APR-98  CHANGE_SUPMET  v1.3  (c) L. Petrov 18-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INTEGER*4  ML
      PARAMETER  ( ML = 30 )
      LOGICAL*4  F_SUPMET
      CHARACTER  UPLINE*(ML), DOWNLINE*(ML), LINE*(ML), CCH*4, STRLET*35
      INTEGER*4  IHT, IH_FR, IH_SC, IW_SC, IFL, ILL, ILB, IDAT
      INTEGER*4  J1, J2, J3, IX, IY, ICH, INEW_SUPMET, OLD_SUPMET
      LOGICAL*4  FL_GVF_DATABASE 
      LOGICAL*2, EXTERNAL :: KBIT
      EQUIVALENCE ( ICH, CCH )
      DATA STRLET / '123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      PARAMETER  ( IH_SC = 21 ) ! min acceptable height of the screen
      PARAMETER  ( IW_SC = 79 ) ! min acceptable width  of the screen
!
      DO 410 J1=1,LEN(UPLINE)
         UPLINE(J1:J1)   = '-' ! char(176)
         DOWNLINE(J1:J1) = '_'
 410  CONTINUE
      UPLINE(1:1) = '|'
      UPLINE(LEN(UPLINE):LEN(UPLINE)) = '|'
      DOWNLINE(1:1) = '|'
      DOWNLINE(LEN(DOWNLINE):LEN(DOWNLINE)) = '|'
!
      ILB   = (IW_SC - ML)/2
!
      IHT = SUPMET__LAST - SUPMET__FIRST + 1
      IH_FR = IHT + 2
      IFL = IH_SC - IH_FR + 1
      ILL = IH_SC
!
      OLD_SUPMET = SUPMET
 910  CONTINUE
!
! --- Printing upper line
!
      CALL SETCR_MN ( ILB-1, IFL )
      CALL ADDSTR_F ( ' ' )
      CALL SETCR_MN ( ILB, IFL )
      CALL ADDSTR_F ( UPLINE )
      CALL CHIN ( DBNAME_CH(1:8), IDAT )
      IF ( IDAT > 19700000 .AND. IDAT < 20700000 ) THEN
           FL_GVF_DATABASE = .TRUE.
         ELSE 
           FL_GVF_DATABASE = .FALSE.
      END IF
!
! --- Printing the table of selection for choice
!
      DO 420 J2=1,IHT
         CALL CLRCH ( LINE )
         LINE(3:3) = STRLET(J2:J2)
         CALL SUPMET_SHOW ( INT2(SUPMET__FIRST+INT2(J2-1)), LINE(6:26) )
         IF ( LINE(6:17) == 'SUPMET__META'  .AND.  .NOT. FL_GVF_DATABASE ) THEN
!
! ----------- Bypass SUPMET__META unless we are in the GVF mode
!
              CALL CLRCH ( LINE(3:17) )
         END IF
         LINE(1:1) = '|'
         LINE(LEN(LINE):LEN(LINE)) = '|'
         CALL SETCR_MN  ( ILB-1, IFL+J2 )
         CALL ADDSTR_F  ( ' ' )
         CALL SETCR_MN  ( ILB, IFL+J2 )
!
! ------ The current suppression method we print by reverse colour
!
         IF ( J2-1 .EQ. (SUPMET - SUPMET__FIRST) ) CALL REVERSE_ON_MN
         CALL ADDSTR_F  ( LINE        )
         IF ( J2-1 .EQ. (SUPMET - SUPMET__FIRST) ) CALL REVERSE_OFF_MN
 420  CONTINUE
!
! --- Printing bottom line
!
      CALL SETCR_MN ( ILB-1, ILL )
      CALL ADDSTR_F ( ' ' )
      CALL SETCR_MN ( ILB, ILL )
      CALL ADDSTR_F ( DOWNLINE )
!
! --- Setting cursor and awaiting user response
!
      IX = ILB-1
      IY = IFL + (SUPMET - SUPMET__FIRST) + 1
      CALL SETCR_MN ( IX, IY      )
      CALL SENKR_MN ( IX, IY, ICH )
!
! --- First try to interprete response as entered letter
!
      INEW_SUPMET = SUPMET__FIRST + INDEX ( STRLET, CCH(4:4) ) - 1
!
! --- Then try interprete response as pointing the item by cursor
!
      IF ( INDEX ( STRLET, CCH(4:4) ) .LE. 0 ) THEN
           INEW_SUPMET = SUPMET__FIRST + IY - (IFL+1)
      END IF
!
! --- Check validity of the response
!
      IF ( INEW_SUPMET .LT. SUPMET__FIRST  .OR. &
     &     INEW_SUPMET .GT. SUPMET__LAST   .OR. &
     &     INEW_SUPMET .EQ. SUPMET              ) THEN
!
! -------- Suppression method has not been changed since cursor pointed on
! -------- eighter invalid position or old suppression method
!
           F_SUPMET = .FALSE.
           RETURN
         ELSE
           IF ( INEW_SUPMET == SUPMET__META .AND. .NOT. FL_GVF_DATABASE ) THEN
!
! ------------- SUPMET__META method can be set up only in GVF mode
!
                F_SUPMET = .FALSE.
                RETURN
           END IF
!
! -------- Suppression method may be potentially changed
!
           CALL CLRCH ( LINE )
           CALL SUPMET_SHOW ( INT2(INEW_SUPMET), LINE )
!
! -------- We check: does line with identificator of the suppression method
! -------- contain asterisk
!
           IF ( INDEX ( LINE, '*' ) .LE. 0 ) THEN
!
! ------------- Suppression is changed, since identificator
! ------------- doesn't have asterisk
!
                SUPMET = INT2(INEW_SUPMET)
                F_SUPMET = .TRUE.
              ELSE
                F_SUPMET = .FALSE.
           END IF
      END IF
!
      IF ( SUPMET == SUPMET__META .AND. .NOT. META_SUP ) THEN
           SUPMET = OLD_SUPMET
           CALL INIT_META_SUP ( )
           SUPMET = INT2(INEW_SUPMET)
      END IF
!
      END  !#!  CHANGE_SUPMET  #!#
