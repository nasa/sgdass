      SUBROUTINE INDX_PARM(COVPARM,IND_COV,LPARM,NPARAMS, &
     &                     MAX_PAR,PERMUT,IARCS,IGLBLS,END_IDX)
      IMPLICIT NONE
!
! 1.  INDX_PARM PROGRAM SPECIFICATION
!
! 1.1 Get position of a set of parameters within a list:
!     NOTE that if 'ALL' is specified, then all ARC parameters
!     are determined, assuming that they are in the first IARCS
!     positions in the list.  The rest of the cases operate on
!     'search and destroy.'
!
! 1.2 REFERENCES:
!
! 2.  INDX_PARM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 MAX_PAR,NPARAMS,IARCS,IGLBLS
!      INTEGER*2 NPARM(IWDS, NPARAMS)
      character*(*) LPARM(nparams)
      CHARACTER*20 COVPARM
!
! COVPARM - Type of J_ARC parameter to correlate
! IARCS - Number of arc parameters in an individual solution
! IGLBLS - Number of global parameters
! MAX_PAR - Maximum number of parameters
! NPARAMS - IARCS + IGLBLS
! LPARM -  Parameter list
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IND_COV(MAX_PAR),END_IDX
      LOGICAL*2 PERMUT
!
! END_IDX - Last index
! IND_COV - List for index of covarriance
! PERMUT - False if what is specified in COVPARM is a parameter
!          True if anything else
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arc_j
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K,R,LINE
      INTEGER*4  INUM
      INTEGER*2 LENGTH
      INTEGER*2 TRIMLEN,INDEX
      CHARACTER*20 ldum
      LOGICAL*2 FOUND
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!  jmg  960610 Remove holleriths.
!
! 5.  INDX_PARM PROGRAM STRUCTURE
!
      DO I=1,MAX_PAR
       IND_COV(I)=0
      ENDDO
      I=0
      PERMUT=.TRUE.
      LENGTH=TRIMLEN(COVPARM)
!
!   decide on what to do by looking at what's in COVPARM
!
      IF(COVPARM(1:3).EQ.'STA') THEN
       I=1
       DO K=1,NPARAMS
          CALL CHIN ( LPARM(K)(11:16), INUM )
          IF (   LPARM(K)(12:20) .EQ. 'COMPONENT'           .OR. &
     &           LPARM(K)(12:20) .EQ. 'VELOCITY '           .OR. &
     &         ( LPARM(K)(17:20) .EQ. '-COO'       .AND. &
     &           ( LPARM(K)(10:10) .EQ. 'X' .OR. &
     &             LPARM(K)(10:10) .EQ. 'Y' .OR. &
     &             LPARM(K)(10:10) .EQ. 'Z'     )  .AND. &
     &             INUM .GT. 0  .AND.  INUM .LT. 999999   )       ) THEN
!
               IND_COV(I)=K
               I=I+1
          END IF
!         if ( ( index(lparm(k),'COMPONENT').ne.0) .or.
!     >            (index(lparm(k),'VELOCITY' ).ne.0)) then
!         IND_COV(I)=K
!         I=I+1
!        ENDIF
       ENDDO
      ELSE IF(COVPARM(1:3).EQ.'SOU') THEN
       I=1
       DO K=1,NPARAMS
        IF((INDEX(LPARM(k),'RIGHT A').NE.0).OR. &
     &     (INDEX(LPARM(k),'DEC VEL').NE.0).or. &
     &     (INDEX(LPARM(k),'DECLINA').NE.0)) THEN
         IND_COV(I)=K
         I=I+1
        ENDIF
       ENDDO
      ELSE IF(COVPARM(1:3).EQ.'NUT') THEN
       I=1
       DO K=1,NPARAMS
        IF(INDEX(LPARM(k),'NUT.').NE.0.OR. &
     &     INDEX(lPARM(k),'NUTATI').NE.0) THEN
         IND_COV(I)=K
         I=I+1
        ENDIF
       ENDDO
      ELSE IF(COVPARM(1:3).EQ.'EOP') THEN
       I=1
       DO K=1,NPARAMS
        IF(INDEX(LPARM(k),'UT1-').NE.0.OR. &
     &     INDEX(lPARM(k),'WOBBLE').NE.0.or. &
     &     index(lparm(k),'NUT.').ne.0.or. &
     &     index(lparm(k),'NUTATI').ne.0) THEN
         IND_COV(I)=K
         I=I+1
        ENDIF
       ENDDO
      ELSE IF(COVPARM(1:3).EQ.'GLB') THEN
       DO K=1,IGLBLS
        IND_COV(K)=IARCS+K
       ENDDO
       I=K
      ELSE IF(COVPARM(1:3).EQ.'ALL') THEN
       DO K=1,IARCS
        IND_COV(K)=K
       ENDDO
       I=K
      ELSE !then what's specified is a parameter
       PERMUT=.FALSE.
       DO I=1,LENGTH
        IF(COVPARM(I:I).EQ.'_') THEN
         COVPARM(I:I)=' '
        ENDIF
       ENDDO
       I=1
       FOUND=.FALSE.
       DO WHILE((I.LE.NPARAMS).AND.(.NOT.FOUND))
        IF(INDEX(lPARM(i),COVPARM(1:LENGTH)).NE.0) THEN
         IND_COV(1)=I
         FOUND=.TRUE.
        ELSE
         IND_COV(1)=0
        ENDIF
        I=I+1
       ENDDO
      ENDIF
      END_IDX=I-1
      RETURN
      END
