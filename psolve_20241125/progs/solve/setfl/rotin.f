      SUBROUTINE ROTIN(IIROT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  ROTIN PROGRAM SPECIFICATION
!
! 1.1 Insert or delete polar motion or UT1 epoch(s).
!
! 1.2 REFERENCES:
!
! 2.  ROTIN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IIROT
!
! IIROT - Flag indicating action to take; I = insert, D = delete,
!         1 = delete first epoch only
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: parxc,rmflg,six
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
!
!
      INTEGER*2 IPOS,ITYP,J, IROT, IDDM, IFLAG, IROTT, K, I, IFLAGO, &
     &          IFLAGN, IMIN, IH, IY, ID, IM, IQ, IYR, II, IROTF, &
     &          ILAST_LINE(3), ITTYP, IDUM(2)
      INTEGER*4 IOS, IFIRST_LINE(3)
      INTEGER*2 INT2_H1, INT2_HD, INT2_HE
      PARAMETER  ( INT2_H1 = 1H1 )
      PARAMETER  ( INT2_HD = 1HD )
      PARAMETER  ( INT2_HE = 1HE )
      LOGICAL*2 DFUZZ
      LOGICAL*2 INTRP_RATES
      REAL*8 XJD,FJLDY,RDUM(3)
      character*80 bufstr
      INTEGER*4  I_LEN
!
      DATA INTRP_RATES /.FALSE./
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  890502  Modified to use INTRP_EROT from CUTIL rather than
!                Tony's INTRP
!   KDB  901206  Now use INTRP_EOVR, a newer version of INTRP_EROT
!   KDB  990125  Y2K fixes.
!
! 5.  ROTIN PROGRAM STRUCTURE
!
!     IF DELETING ONLY THE FIRST EPOCH, DELETE THE APPROPRIATE LINES FROM THE
!     SCREEN, OTHERWISE CLEAR THE SCREEN
!
!
      IF ( IIROT.EQ. INT2_H1 )THEN  !delete the lines for the first epoch, for x,y ,ut1
           CALL EOYFL(IFIRST_LINE,ILAST_LINE )
           DO II=3,1,-1
              IF ( II.LE.2) THEN
                   ITTYP = 1
                 ELSE
                   ITTYP = 2
              END IF
              IF ( EOP_STYLE(ITTYP) .EQ. 0) THEN
                   CALL SETCR_MN ( 0, IFIRST_LINE(II) )
              END IF
           END DO
           CALL REFRESH_MN()
         ELSE  !clear the screen
           CALL SETCR_MN ( 0, 0 )
           CALL CLEAR_MN()  
      END IF  !clear the screen
!
!     TEST IIROT TO DETERMINE WHETHER TO INSERT, OR DELETE, OR
!     TO DELETE 1ST EPOCH
!
      IF ( (IIROT.EQ.INT2_HD .OR. IIROT.EQ.INT2_H1).AND. NROT.EQ. 1 ) THEN
            RETURN
      END IF
!
!     1. SECTION TO DELETE EPOCHS
!
      IF ( IIROT.EQ.INT2_HD .OR. IIROT.EQ. INT2_H1 )THEN  !delete epochs
           IQ = -1
           DO WHILE (IQ.NE. 0 .AND. NROT.NE.1 .AND. IIROT.NE.INT2_HE)
!
! ----------- delete another epoch
!
              IF  (IIROT .EQ. INT2_HD )THEN  !interact with user to get deletes
                  DO I=1,NROT
!
! ------------------ list epochs for user
!
                     CALL EPOC(IM,ID,IYR,IH,IMIN,TROT(I) )
                     WRITE(bufstr,1001) I, IYR,IM,ID,IH,IMIN
 1001                FORMAT(I3,". ",2(I2,"/"),I2,I3,":",I2)
                     CALL ADDSTR_F ( BUFSTR )
                     CALL NL_MN()
                  END DO  ! list epochs for user
                  call addstr_f ( " DELETE EPOCH NUMBER? (0 TO QUIT) " )
                  IQ = 0
                  CALL GETSTR_F(BUFSTR )
                  READ ( BUFSTR(1:I_LEN(BUFSTR)), *, IOSTAT=IOS ) IQ
                  CALL FERR ( INT2(IOS), "Invalid epoch number", INT2(0), &
     &                        INT2(0) )
                  IF ( IQ.LE.0 .OR. IQ.GT.NROT ) THEN
                       IQ = 0
                  END IF
                ELSE  !delete only 1st epoch
                  IQ = 1
                  IIROT = INT2_HE
              END IF  !delete only 1st epoch
!
              IF ( IQ .NE. 0 )THEN  !now delete an epoch
                   IF ( IQ .LT. NROT)THEN  !move the stack down
                        DO J = IQ+1,NROT
!
! ------------------------ move it
!
                           TROT(J-1) = TROT(J)
                           DO K=1,4
                              ROTAP(J-1,K) = ROTAP(J,K)
                           END DO
!
! ------------------------ Move down the flags
!
                           DO ITYP = 1,3
!
! --------------------------- running over the 3 types
!
                              DO IPOS = 1,4
!
! ------------------------------ running over the 4 postions
!
                                 IFLAGN = IROTT(J  ,ITYP,IPOS,LROT)
                                 IFLAGO = IROTT( INT2(J-1), ITYP, IPOS, LROT)
                                 IF ( IFLAGN .NE. IFLAGO ) THEN
                                      IDDM = IROTF( INT2(J-1), ITYP, IPOS, LROT)
                                 END IF
                              END DO  !running over the 4 postions
                           END DO  !running over the 3 types
                        END DO  !move it
                   END IF  !move the stack down
                   NROT = NROT-1
              END IF  !now delete an epoch
           END DO  !delete another epoch
           RETURN
      END IF  !delete epochs
!
!
!     2. SECTION TO INSERT EPOCHS
!
      IY = 0
      DO WHILE (IY.GE. 0)
!
! ------ insert epochs
!
 2000    call addstr_f("INSERT A ROTATION EPOCH")
         call nl_mn()
         call addstr_f(" YY MM DD HH MM (/ to quit)" )
         call nl_mn()
         IY = -1
         call getstr_f(bufstr )
         READ(bufstr,*,ERR=2000)  IY,IM,ID,IH,IMIN
         IF  ( IY .LT.0 )THEN  !quit
               RETURN
         END IF  !quit
!
! ------ Not done, so lets go to work
!
         XJD = FJLDY(IM,ID,IY) + IH/24.D0 + IMIN/1440.D0
         J = 1
         IF ( NROT .EQ. MAX_ROT ) CALL FERR ( INT2(259), 'TOO MANY EOP EPOCHS', &
     &                                        INT2(0), INT2(0) )
         DO WHILE (J.LE.NROT .AND. TROT(J).LT.XJD)
            J = J+1
         END DO
         IF ( J .GT. NROT ) THEN  !add at the end
              TROT(J) = XJD
              CALL INTRP_EOVR ( TROT(J), ROTAP(J,1), ROTAP(J,2), &
     &                          ROTAP(J,3), ROTAP(J,4), INTRP_RATES, &
     &                          RDUM(1), RDUM(2), RDUM(3), IDUM )
              NROT = NROT+1
            ELSE  !add in the middle
              IF ( DFUZZ ( TROT(J), XJD ) )THEN
                   RETURN
              END IF
              DO  I = NROT,J,-1
!
! --------------- create a hole in the stack
!
                  TROT(I+1) = TROT(I)
                  DO K=1,4
                     ROTAP(I+1,K) = ROTAP(I,K)
                  END DO
                  DO ITYP = 1,3
!
! ------------------ running over the three types
!
                     DO IPOS = 1,4
!
! ---------------------- running over the 4 positions
!
                         IFLAGN = IROTT( INT2(I+1), ITYP, IPOS, LROT)
                         IFLAGO = IROTT(I  ,ITYP,IPOS,LROT)
                         IF ( IFLAGN .NE. IFLAGO ) THEN
                              IDDM = IROTF( INT2(I+1), ITYP, IPOS, LROT)
                         END IF
                     END DO  !running over the 4 positions
                  END DO  !running over the three types
              END DO  !create a hole in the stack
              TROT(J) = XJD
              CALL INTRP_EOVR ( TROT(J), ROTAP(J,1), ROTAP(J,2), &
     &                           ROTAP(J,3), ROTAP(J,4), INTRP_RATES, &
     &                           RDUM(1), RDUM(2), RDUM(3), IDUM )
              NROT = NROT+1
              DO ITYP =1,3
!
! -------------- running over 3 types
!
                 DO IPOS=1,4
!
! ----------------- running over 4 positions
!
                    IFLAG = IROTT(J,ITYP,IPOS,LROT)
                    IF ( IFLAG .NE. 0 ) IDDM = IROTF(J,ITYP,IPOS,LROT)
                 END DO  !running over 4 positions
              END DO  !running over 3 types
!
          END IF  !add in the middle
      END DO  !insert epochs
!
      RETURN
      END
