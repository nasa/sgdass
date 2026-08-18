      SUBROUTINE EOP_SHARE ( THIS_TROT, EOPTRACE, IA, WHO, THIS_NUM, OORR, MAT, &
     &                       LBUF_LEN, LBUF, IPTR, PAGEWID )
      IMPLICIT NONE
!
! 1.  EOP_SHARE PROGRAM SPECIFICATION
!
! 1.1 Determine EOP constraint share for degrees of freedom.
!
! 1.2 REFERENCES:
!
! 2.  EOP_SHARE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IA(3),THIS_NUM
      REAL*8 THIS_TROT,mat(*)
      CHARACTER*20 who(3)
      Character*(*) OorR
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
! IA - List of earth orientation offset deriv array positions
! OorR - String indicating whether we are handling offset or rate
! THIS_TROT - Epoch for which shorter position and parameter names lists
!             should be generated
! THIS_NUM - Number of earth orientation offset parameters for this epoch
! WHO - List of earth orientation offset parameter names for this eopch
!
! 2.3 OUTPUT Variables:
!
      REAL*8 EOPTRACE
!
!     EOPTRACE - Sum of earth orientation constraint share
!
! 2.4 COMMON BLOCKS USED
!      include '../include/nrmcm.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a2jst
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,NUM,INC
      INTEGER*4 INDX4,IAIIAJ
      REAL*8 ALCL(6),FSTJD,CVINF(6),TRACE
      INTEGER*4 JA
      INTEGER*2 INT2_NO, INT2_NE
      PARAMETER  ( INT2_NO = 2HNO )
      PARAMETER  ( INT2_NE = 2HNE )
!
! 4.  HISTORY
!  WHO  WHEN     WHAT
!  KDB  09/18/90 Updated to support both old and new styles of earth
!                orientation.
!  pet 1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                  eliminated common block adj_buf
!  pet 2001.05.29  Added a check: whether diagonal elements of the apriori
!                  covarince matrix for contraints imposed on EOP rate are
!                  non-zero. If at least one element is zero it means that
!                  no constraints on EOP rate was imposed.
!
! 5.  EOP_SHARE PROGRAM STRUCTURE
!
      JA = 3*M_GPA
      DO I=1,3
         DO J=1,I
            ALCL(INDX4(I,J))=0.0D0
         ENDDO
      ENDDO
!
! --- Recover the covariance matrix from the solution
!
      DO I=1,3
         IF ( IA(I) .NE. 0 ) THEN
              DO J=1,I
                 IF ( IA(J) .NE. 0 ) THEN
                      IAIIAJ = INDX4(IA(I),IA(J))
                      ALCL(INDX4(I,J)) = MAT(JA+IAIIAJ)
                 ENDIF
              ENDDO
          ENDIF
      ENDDO
!
! --- Get the constraint matrix
!
      FSTJD = UT1INV(1)
      INC   = UT1INV(2) + 0.1
      NUM   = UT1INV(3) + 0.1
      IF ( INC .EQ. 0  .OR. &
     &     ( EOPDLY(1) .EQ. INT2_NO .AND. EOPDLY(2) .EQ. INT2_NE ) ) THEN
!
           FSTJD=UT1INB(1)
           INC = UT1INB(2) + 0.1
           NUM = UT1INB(3) + 0.1
      ENDIF
!
      IF ( OORR .EQ. 'Offset') THEN
           CALL COV_EOP ( THIS_TROT, FSTJD, INC, NUM, PI_VAR, CVINF, WHO, &
     &                    EOPCONS )
         ELSE
           IF ( EOPRCONS(1) .GT. 0.0D0  .AND. &
     &          EOPRCONS(2) .GT. 0.0D0  .AND. &
     &          EOPRCONS(3) .GT. 0.0D0         ) THEN
!
                CALL COV_REOP ( THIS_TROT, FSTJD, INC, NUM, PI_VAR, CVINF, WHO, &
     &                          EOPRCONS)
              ELSE
                TRACE = 0.0
                RETURN
           ENDIF
      ENDIF
!
! --- Get the trace of the product for this epoch
!
      TRACE=0.0
      DO I=1,3
         DO J=1,3
            TRACE = TRACE + ALCL(INDX4(I,J))*CVINF(INDX4(J,I))
         ENDDO
      ENDDO
!
! --- Add it to the overall trace for this data base
!
      EOPTRACE=EOPTRACE+TRACE
!
      IF ( KSPOOL ) THEN
!
! -------- (this quantity provides a measure of the fraction of the final
! -------- covariance contributed by the earth orientation constraints)
!
           WRITE(23,9910) OORR,TRACE/THIS_NUM
9910       FORMAT(30X,' Earth Orientation ',A6,' Constraint Share ',F3.1)
      ENDIF
!
      IF ( KSCREEN ) THEN
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), 9910 ) TRACE/THIS_NUM
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
      ENDIF
!
      RETURN
      END  !#!  EOP_SHARE  #!#
