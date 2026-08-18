      SUBROUTINE INDEX_PARM ( PARMTYP, PARMIDX, NPARM, NPARAMS, IWDS, END_IDX, &
     &                        WHO, KWHO, STA )
      IMPLICIT NONE
!
! 1.  INDEX_PARM PROGRAM SPECIFICATION
!
! 1.1 Set up cross-referencing between parameter sublists and
!       the complete list.
!
! 1.2 REFERENCES:
!
! 2.  INDEX_PARM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2  IWDS, NPARM(IWDS,NPARAMS)
      INTEGER*4  NPARAMS
      CHARACTER*(*) PARMTYP,STA
      LOGICAL*2 KWHO
!
! IWDS - Length of parameter names
! KWHO - True if we are to put list of parameter names in WHO
! NPARAMS - Total number of parameters
! NPARM - List of all parameter names
! PARMTYP - Parameter type (e.g. EOP, AT1, etc.)
! STA - Array of station names
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 PARMIDX(M_GPA),END_IDX
      CHARACTER*20 WHO(*)
!
! END_IDX - Number of parameters of the specified type
! PARMIDX - Cross reference to parameter numbers (from parmidx to nparm)
! WHO - Array of parameter names of specified type
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cnstr,do_atm,do_clk
!       CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  I, J, K, IARCS, IGLBLS
      INTEGER*2  R, LINE, IDUM(10), LENGTH, TRIMlEN, INDEX
      INTEGER*4  INUM
      CHARACTER*20 PARM
      EQUIVALENCE (IDUM(1),PARM)
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   KDB   09/18/90  Was indx_parm under proc subdirectory.  Moved to cutil
!                   for use by adjst as well
!   MWH   05/24/91  Modify to accommodate new clock/atmosphere parameterization
!   dsm   05/6/94   Modify to accommodate new gradient parameterization
!   pet   23-OCT-2017  Converted PARMIDX,NPARM,NPARAMS to INTEGER*4
!   pet   2019.12.30   Added support of proper motions
!
! 5.  INDEX_PARM PROGRAM STRUCTURE
!
!   initialize parameter sublist
!
      DO I=1,M_GPA
          PARMIDX(I)=0
      ENDDO
      I=0
      LENGTH=TRIMLEN(PARMTYP)
!
!   decide on what to do by looking at what's in PARMTYP
!
! First handle station parameters
!
      IF(PARMTYP(1:3) .eq. 'STA') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
!
             IF ( PARM(12:20) .EQ. 'COMPONENT' ) THEN
                  PARMIDX(I) = K
                  I = I + 1
             END IF
!
             CALL CHIN ( PARM(11:16), INUM )
             IF ( PARM(17:20) .EQ. '-COO'       .AND. &
     &            ( PARM(10:10) .EQ. 'X' .OR. PARM(10:10) .EQ. 'Y' .OR. &
     &              PARM(10:10) .EQ. 'Z'      ) .AND. &
     &              INUM .GT. 0  .AND.  INUM .LT. 999999 ) THEN
!
                  PARMIDX(I) = K
                  I = I + 1
             END IF
!
          End do
!
! Next handle site velocity parameters
!
      Else IF(PARMTYP(1:3) .eq. 'VEX') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF(((INDEX(PARM,STA(1:8)//' X').NE.0).or. &
     &        (INDEX(PARM,STA(1:8)//' U').NE.0)).and. &
     &           INDEX(PARM,'VELOCITY').NE.0) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
      Else IF(PARMTYP(1:3) .eq. 'VEY') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF(((INDEX(PARM,STA(1:8)//' Y').NE.0).or. &
     &        (INDEX(PARM,STA(1:8)//' E').NE.0)).and. &
     &           INDEX(PARM,'VELOCITY').NE.0) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
      Else IF(PARMTYP(1:3) .eq. 'VEZ') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF(((INDEX(PARM,STA(1:8)//' Z').NE.0).or. &
     &        (INDEX(PARM,STA(1:8)//' N').NE.0)).and. &
     &           INDEX(PARM,'VELOCITY').NE.0) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
!
! Next handle source parameters
!
       ELSE IF( PARMTYP(1:3) .EQ. 'SOU' ) THEN
          I = 1
          DO K = 1, NPARAMS
              DO R = 1, IWDS
                 IDUM(R) = NPARM(R,K)
              END DO
              IF ( ( INDEX  ( PARM, 'RIGHT ASCEN' ) .NE. 0 ) .OR. &
     &             ( INDEX  ( PARM, 'DECLINATION' ) .NE. 0 )      ) THEN
                     PARMIDX(I) = K
                     I = I + 1
              END IF
          END DO
!
! Next handle source proper motion paramaters
!
       ELSE IF( PARMTYP(1:3) .EQ. 'PRP' ) THEN
          I = 1
          DO K = 1, NPARAMS
              DO R = 1, IWDS
                 IDUM(R) = NPARM(R,K)
              END DO
              IF ( ( INDEX  ( PARM, 'RIGHT ASC V' ) .NE. 0 ) .OR. &
     &             ( INDEX  ( PARM, 'DEC VELO'    ) .NE. 0 )      ) THEN
                     PARMIDX(I) = K
                     I = I + 1
              END IF
          END DO
!
! Next handle baseline clock parameters
!
      Else IF(PARMTYP(1:3) .eq. 'BCL') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF(parm(9:9).eq.'-'.and.parm(20:20).eq.'C') then
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
!
! Handle nutation parameters
!
      Else IF(PARMTYP(1:3) .eq. 'NUT') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF((INDEX(PARM,'NUT.').NE.0) .or. &
     &            (INDEX(PARM,'NUTATI').NE.0)) THEN
                  PARMIDX(I) = K
                  IF(KWHO) WHO(I) = PARM
                  I = I + 1
              End if
          End do
!
! Handle earth orientation offset parameters
!
      Else IF(PARMTYP(1:3) .eq. 'EOP') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF(((INDEX(PARM,'UT1').NE.0) .or. &
     &            (INDEX(PARM,'WOBBLE').NE.0)) .and. &
     &            (parm(10:10).eq.'0')) THEN
                  PARMIDX(I) = K
                  IF(KWHO) WHO(I) = PARM
                  I = I + 1
              End if
          End do
!
! Handle earth orientation rate parameters
!
      Else IF(PARMTYP(1:3) .eq. 'EOR') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF(((INDEX(PARM,'UT1').NE.0) .or. &
     &            (INDEX(PARM,'WOBBLE').NE.0)) .and. &
     &            (parm(10:10).eq.'1')) THEN
                  PARMIDX(I) = K
                  IF(KWHO) WHO(I) = PARM
                  I = I + 1
              End if
          End do
!
! Handle atmosphere parameters
!
      Else IF(PARMTYP(1:3).EQ.'AT1') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF((INDEX(PARM,STA(1:8)//'A0').NE.0) .OR. &
     &          (INDEX(PARM,STA(1:8)//'a0').NE.0)) THEN
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
! Handle gradient parameters
!
      Else IF(PARMTYP(1:3).EQ.'GRA') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF((INDEX(PARM,STA(1:8)//'N').NE.0) .OR. &
     &          (INDEX(PARM,STA(1:8)//'E').NE.0)) THEN
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
! Handle clock parameters
!
      Else IF(PARMTYP(1:3).EQ.'CL1') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF((INDEX(PARM,STA(1:8)//'C0').NE.0) .OR. &
     &          (INDEX(PARM,STA(1:8)//'c0').NE.0)) THEN
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
! Handle continuous station position parameters (X)
!
      Else IF(PARMTYP(1:3).EQ.'STX') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF(INDEX(PARM,STA(1:8)//' X').NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
! Handle continuous station position parameters (Y)
!
      Else IF(PARMTYP(1:3).EQ.'STY') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF(INDEX(PARM,STA(1:8)//' Y').NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
! Handle continuous station position parameters (Z)
!
      Else IF(PARMTYP(1:3).EQ.'STZ') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF(INDEX(PARM,STA(1:8)//' Z').NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
! Handle the subset containing all arc parameters
!
      Else IF(PARMTYP(1:3) .eq. 'LOC') THEN
          Do K = 1, IARCS
              PARMIDX(K) = K
          End do
          I = K + 1
!
! Handle the subset containing all global parameters
!
      Else IF(PARMTYP(1:3) .eq. 'GLB') THEN
          Do K = 1, IGLBLS
              PARMIDX(K) = IARCS + K
          End do
          I = K + 1
!
! Handle the entire set of parameters
!
      Else IF(PARMTYP(1:3) .eq. 'ALL') THEN
          Do K = 1, NPARAMS
              PARMIDX(K) = K
          End do
          I = K + 1
!
! Handle an individual parameter
!
      Else !THEN what's specified is a parameter
          Do I = 1, LENGTH
              IF(PARMTYP(I:I) .eq. '_') THEN
                  PARMTYP(I:I) = ' '
              End if
          End do
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF(INDEX(PARM,PARMTYP(1:length)).NE.0) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
      End if
      END_IDX = I - 1
      RETURN
      END
