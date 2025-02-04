      SUBROUTINE CXEPAR_OPT20 ( LPARM1, IX1T2, NPARM1, LPARM2, NPARM2 )
! ************************************************************************
! *                                                                      *
! *   Quick routine for generation of a cross reference list from LPARM1 *
! *   to LPARM2. It checks dimensions. For low dimensions it uses        *
! *   a simple, straitforward algorithm. For large dimensions it copies  *
! *   arrays of parameter names to temporary files and sorts these files *
! *   in alphabetic order.                                               *
! *                                                                      *
! *  ### 14-AUG-2003  CXEPAR_OPT20  v1.1 (c) L. Petrov  24-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INTEGER*4 NPARM1, NPARM2
      CHARACTER LPARM1(NPARM1)*20, LPARM2(NPARM2)*20
!
      TYPE      PARM_AND_IND
          CHARACTER PAR_NAME*20  ! Parameter name
          INTEGER*4 PAR_IND      ! Index of the parameter in the unsorted list
      END TYPE  PARM_AND_IND
      TYPE(PARM_AND_IND), POINTER :: ARR1(:), ARR2(:)
!
      INTEGER*4 IX1T2(NPARM1)
      INTEGER*4 MIN_SORT
      PARAMETER  ( MIN_SORT = 100 )
      INTEGER*4 NBEG, NF, IP, J1, J2, J3, J4, J5, J6
      INTEGER*4 STRNCMP 
#ifdef GNU
      INTEGER*4, EXTERNAL :: COMPAR_CXEPAR
#else
      INTEGER*2, EXTERNAL :: COMPAR_CXEPAR
#endif
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Initialization
!
      CALL NOUT_I4 ( NPARM1, IX1T2 )
!
      IF ( NPARM1 .LT. MIN_SORT  .AND.  NPARM2 .LT. MIN_SORT ) THEN
!
! -------- Low dimenstion? Let's use straitforward algorithm
!
           DO 410 J1=1,NPARM1
              DO 420 J2=1,NPARM2
                 IF ( LPARM1(J1) .EQ. LPARM2(J2) ) THEN
                      IX1T2(J1) = J2
                      GOTO 410  ! We found -- we do not need any more
                    ELSE 
                      GOTO 420  ! not  found? look again
                 END IF
 420          CONTINUE
 410       CONTINUE
         ELSE
!
! -------- In the case of large dimensions, it is cheaper first to sort both
! -------- arrays in alphabetic order
! 
! -------- Allocate memory for arrays of parameter names and their indexes
!
           ALLOCATE ( ARR1(NPARM1) )
           ALLOCATE ( ARR2(NPARM2) )
           DO 430 J3=1,NPARM1
              ARR1(J3)%PAR_NAME = LPARM1(J3)
              ARR1(J3)%PAR_IND  = J3
 430       CONTINUE 
           DO 440 J4=1,NPARM2
              ARR2(J4)%PAR_NAME = LPARM2(J4)
              ARR2(J4)%PAR_IND  = J4
 440       CONTINUE 
!
! -------- Now let's sort both arrays in alphabetic order
!
           CALL FOR_QSORT ( ARR1, NPARM1, 24, COMPAR_CXEPAR )
           CALL FOR_QSORT ( ARR2, NPARM2, 24, COMPAR_CXEPAR )
!
           NBEG = 1
           DO 450 J5=1,NPARM1
              NF = NBEG
              DO 460 J6=NF,NPARM2
!
! -------------- Compare parameter names of the arrays
!
                 IP = STRNCMP ( %REF(ARR2(J6)%PAR_NAME), &
     &                          %REF(ARR1(J5)%PAR_NAME), %VAL(20) )
                 IF ( IP .LT. 0 ) THEN
!
! ------------------- Less? Look at the next element of the ARR1
!
                    
                      NBEG = NBEG + 1
                   ELSE IF ( IP .EQ. 0 ) THEN
!
! ------------------- Equal? Hurra! Set the elment of the cross reference table
!
                      IX1T2(ARR1(J5)%PAR_IND) = ARR2(J6)%PAR_IND
                      NBEG = NBEG + 1 ! This element ofthe ARR2 will not be 
!                                     ! looked any more
                      GOTO 450
                    ELSE IF ( IP .GT. 0 ) THEN
!
! ------------------- Greater?! Hmm... No counterpart of the ARR1(J5) is found
! ------------------- in ARR2 array. 
!
                      GOTO 450
                 END IF
 460          CONTINUE 
 450       CONTINUE 
!
! -------- Deallocate memory
!
           DEALLOCATE ( ARR1 )
           DEALLOCATE ( ARR2 )
      END IF
      RETURN
      END  !#!  CXEPAR_OPT #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   COMPAR_CXEPAR ( STR1, STR2 )
! ************************************************************************
! *                                                                      *
! *   Auxiliary function for comparsion of two elements used by          *
! *   CXEPAR_OPT.                                                        *
! *                                                                      *
! *  ### 14-AUG-2003  COMPAR_CXEPAR  v2.0 (c) L. Petrov 22-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
#ifdef GNU
      INTEGER*4  COMPAR_CXEPAR
#else
      INTEGER*2  COMPAR_CXEPAR
#endif
      TYPE      PARM_AND_IND
          CHARACTER PAR_NAME*20  ! Parameter name
          INTEGER*4 PAR_IND      ! Index of the parameter in the unsorted list
      END TYPE  PARM_AND_IND
      TYPE   ( PARM_AND_IND ) :: STR1, STR2
!@      INTEGER*4  STRNCMP
!@      CHARACTER  STR1*24, STR2*24
!@!
!@      COMPAR_CXEPAR = STRNCMP ( %REF(STR1), %REF(STR2), %VAL(20) )
!
      IF ( STR1%PAR_NAME > STR2%PAR_NAME ) THEN
           COMPAR_CXEPAR =  1
         ELSE IF ( STR1%PAR_NAME < STR2%PAR_NAME ) THEN
           COMPAR_CXEPAR = -1
         ELSE 
           COMPAR_CXEPAR =  0
      END IF
!
      RETURN
      END  FUNCTION  COMPAR_CXEPAR  !#!#
