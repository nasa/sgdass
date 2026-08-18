      SUBROUTINE SPHE_FF ( FSH, DEG, NORM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPHE_FF precompute multiplicative factors used             *
! *   in recursion relationships                                         *
! *                                                                      *
! *   Plmbar(l,m) = x*f1(l,m)*Plmbar(l-1,m) - Plmbar(l-2,m)*f2(l,m)      *
! *             k = l*(l+1)/2 + m + 1                                    *
! *                                                                      *
! *   Note that prefactors are not used for the case when m=l and m=l-1, *
! *   as a different recursion is used for these two values.             *
! *                                                                      *
! *   Output is written in FSH%F1 and FSH%F2                             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- internal data stgrcuture that keeps internal   *
! *                       arrays and their status for possible re-use.   *
! *  DEG ( INTEGER*4 ) -- Degree of the expansion                        *
! * NORM ( INTEGER*4 ) -- Normalization to be used when calculating      *
! *                       Legendre functions                             *
! *                       1 -- "geodesy";                                *
! *                       2 -- Schmidt;                                  *
! *                       3 -- unnormalized;                             *
! *                       4 -- orthonormalized;                          *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 2006-2011, Mark A. Wieczorek                         *
! *   All rights reserved.                                               *
! *                                                                      *
! * ### 21-AUG-2012   SPHE_FF v1.0 modified by L. Petrov 21-AUG-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  DEG, NORM, IUER
      INTEGER*4  J1, J2, IER
      CHARACTER  STR*128, STR1*128
      LOGICAL*1  FL_ERROR 
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_NUM_THREADS
!
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6211, IUER, 'SPHE_FF', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6212, IUER, 'SPHE_FF', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6213, IUER, 'SPHE_FF', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( FSH%FF_STATUS == FSH__COMP .AND. &
     &     FSH%FF_DEG    == DEG       .AND. &
     &     FSH%FF_NORM   == NORM            ) THEN
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE 
           FL_ERROR = .FALSE.
!$OMP      CRITICAL
           IF ( ASSOCIATED ( FSH%F1 ) ) THEN
                DEALLOCATE ( FSH%F1 )
           END IF
           ALLOCATE ( FSH%F1(0:DEG,0:DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*(DEG+1)**2, STR )
                CALL ERR_LOG ( 6214, IUER, 'SPHE_FF', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%F1' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%F1= 0.0D0
!
           IF ( ASSOCIATED ( FSH%F2 ) ) THEN
                DEALLOCATE ( FSH%F2 )
           END IF
           ALLOCATE ( FSH%F2(0:DEG,0:DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*(DEG+1)**2, STR )
                CALL ERR_LOG ( 6215, IUER, 'SPHE_FF', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%F2' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%F2= 0.0D0
!
           IF ( ASSOCIATED ( FSH%F3 ) ) THEN
                DEALLOCATE ( FSH%F3 )
           END IF
           ALLOCATE ( FSH%F3(0:DEG,0:DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*(DEG+1)**2, STR )
                CALL ERR_LOG ( 6216, IUER, 'SPHE_FF', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%F3' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%F2= 0.0D0
!
           FSH%FF_STATUS = FSH__ALLO
           FSH%FF_DEG  = DEG
           FSH%FF_NORM = NORM
 820       CONTINUE 
!$OMP      END CRITICAL
           IF ( FL_ERROR ) RETURN 
      END IF
!
      DO 410 J1=0,DEG ! l
         DO 420 J2=0,J1 ! m
            IF ( J1 .GE. 1 ) THEN
                 IF ( J2 == 0 ) THEN
                      IF ( J1 == 1 ) THEN
                           IF ( NORM == 1 .OR. NORM == 4 ) THEN
                                FSH%F1(J1,J2) = SQRI(3)
                                FSH%F2(J1,J2) = 0.0D0
                             ELSE IF ( NORM == 2 ) THEN
                                FSH%F1(J1,J2) = 1.0D0
                                FSH%F2(J1,J2) = 0.0D0
                              ELSE IF ( NORM == 3 ) THEN
                                FSH%F1(J1,J2) = DBLE(2*J1-1)/ DBLE(J1)
                                FSH%F2(J1,J2) = DBLE(J1-1)/ DBLE(J1)
                           END IF
                         ELSE 
                           IF ( NORM == 1 .OR. NORM == 4 ) THEN
                                FSH%F1(J1,J2) = SQRI(2*J1-1)* ( SQRI(2*J1+1)/ DBLE(J1) )
                                FSH%F2(J1,J2) = ( DBLE(J1-1)/ DBLE(J1) )* ( SQRI(2*J1+1)/ SQRI(2*J1-3) )
                              ELSE IF ( NORM == 2 ) THEN
                                FSH%F1(J1,J2) = DBLE(2*J1-1)/ DBLE(J1)
                                FSH%F2(J1,J2) = DBLE(J1-1)/ DBLE(J1)
                              ELSE IF ( NORM == 3 ) THEN
                                FSH%F1(J1,J2) = DBLE(2*J1-1)/ DBLE(J1)
                                FSH%F2(J1,J2) = DBLE(J1-1)/ DBLE(J1)
                           END IF
                      END IF
                    ELSE IF ( J2 .LE. J1-2 ) THEN
                      IF ( NORM == 1 .OR. NORM == 4 ) THEN
                           FSH%F1(J1,J2) = ( SQRI(2*J1+1)/ SQRI(J1+J2)  )* (SQRI(2*J1-1)/ SQRI(J1-J2) )
                           FSH%F2(J1,J2) = ( SQRI(2*J1+1)/ SQRI(2*J1-3) )* ( SQRI(J1-J2-1)/SQRI(J1-J2) )* &
     &                                     ( SQRI(J1+J2-1)/ SQRI(J1+J2) )
                         ELSE IF ( NORM == 2 ) THEN
                           FSH%F1(J1,J2) =   DBLE(2*J1-1)/ SQRI(J1+J2)/ SQRI(J1-J2)
                           FSH%F2(J1,J2) = ( SQRI(J1+J2-1)/ SQRI(J1+J2) )* ( SQRI(J1-J2-1)/ SQRI(J1-J2) )
                         ELSE IF ( NORM == 3 ) THEN
                           FSH%F1(J1,J2) = DBLE(2*J1-1)/  DBLE(J1-J2)
                           FSH%F2(J1,J2) = DBLE(J1+J2-1)/ DBLE(J1-J2)
                      END IF
                    ELSE IF ( J2 .EQ. J1-1 ) THEN
                      IF ( NORM == 1 .OR. NORM == 4 ) THEN
                           FSH%F1(J1,J2) = ( SQRI(2*J1+1)/ SQRI(J1+J2) )* ( SQRI(2*J1-1)/ SQRI(J1-J2) )
                           FSH%F2(J1,J2) = 0.0D0
                         ELSE IF ( NORM == 2 ) THEN
                           FSH%F1(J1,J2) = DBLE(2*J1-1)/ SQRI(J1+J2)/ SQRI(J1-J2)
                           FSH%F2(J1,J2) = 0.0D0
                         ELSE IF ( NORM == 3 ) THEN
                           FSH%F1(J1,J2) = DBLE(2*J1-1)/  DBLE(J1-J2)
                           FSH%F2(J1,J2) = DBLE(J1+J2-1)/ DBLE(J1-J2)
                      END IF 
                 END IF 
            END IF
!
            IF ( J2 == 1 ) THEN
                 FSH%F3(J1,J2) = SQRI(J1+J2)*SQRI(J1-J2+1)/SQRI(2)
               ELSE
                 FSH%F3(J1,J2) = SQRI(J1+J2)*SQRI(J1-J2+1)/2.0D0
            END IF
 420     CONTINUE 
 410  CONTINUE 
      FSH%FF_STATUS = FSH__COMP
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_FF  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPHE_FF_X ( FSH, DEG, NORM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPHE_FF precompute multiplicative factors used             *
! *   in recursion relationships                                         *
! *                                                                      *
! *   Plmbar(l,m) = x*f1(l,m)*Plmbar(l-1,m) - Plmbar(l-2,m)*f2(l,m)      *
! *             k = l*(l+1)/2 + m + 1                                    *
! *                                                                      *
! *   Note that prefactors are not used for the case when m=l and m=l-1, *
! *   as a different recursion is used for these two values.             *
! *                                                                      *
! *   Output is written in FSH%F1 and FSH%F2                             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- internal data stgrcuture that keeps internal   *
! *                       arrays and their status for possible re-use.   *
! *  DEG ( INTEGER*4 ) -- Degree of the expansion                        *
! * NORM ( INTEGER*4 ) -- Normalization to be used when calculating      *
! *                       Legendre functions                             *
! *                       1 -- "geodesy";                                *
! *                       2 -- Schmidt;                                  *
! *                       3 -- unnormalized;                             *
! *                       4 -- orthonormalized;                          *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 2006-2011, Mark A. Wieczorek                         *
! *   All rights reserved.                                               *
! *                                                                      *
! * ### 21-AUG-2012   SPHE_FF v2.0 modified by L. Petrov 18-NOV-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  DEG, NORM, IUER
      INTEGER*4  J1, J2, J3, J4, NTHR, NUM_THR_SAVED, IER
      CHARACTER  STR*128, STR1*128
      LOGICAL*1  FL_ERROR 
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_NUM_THREADS
!
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6221, IUER, 'SPHE_FF_X', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6222, IUER, 'SPHE_FF_X', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6223, IUER, 'SPHE_FF_X', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( FSH%FF_STATUS == FSH__COMP .AND. &
     &     FSH%FF_DEG    == DEG       .AND. &
     &     FSH%FF_NORM   == NORM            ) THEN
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE 
           FL_ERROR = .FALSE.
!$OMP      CRITICAL
           IF ( ASSOCIATED ( FSH%F1 ) ) THEN
                DEALLOCATE ( FSH%F1 )
           END IF
           ALLOCATE ( FSH%F1(0:DEG,0:DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*(DEG+1)**2, STR )
                CALL ERR_LOG ( 6224, IUER, 'SPHE_FF_X', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%F1' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%F1= 0.0D0
!
           IF ( ASSOCIATED ( FSH%F2 ) ) THEN
                DEALLOCATE ( FSH%F2 )
           END IF
           ALLOCATE ( FSH%F2(0:DEG,0:DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*(DEG+1)**2, STR )
                CALL ERR_LOG ( 6225, IUER, 'SPHE_FF_X', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%F2' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%F2= 0.0D0
!
           IF ( ASSOCIATED ( FSH%F3 ) ) THEN
                DEALLOCATE ( FSH%F3 )
           END IF
           ALLOCATE ( FSH%F3(0:DEG,0:DEG), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*(DEG+1)**2, STR )
                CALL ERR_LOG ( 6226, IUER, 'SPHE_FF_X', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%F3' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%F2= 0.0D0
!
           FSH%FF_STATUS = FSH__ALLO
           FSH%FF_DEG  = DEG
           FSH%FF_NORM = NORM
 820       CONTINUE 
!$OMP      END CRITICAL
           IF ( FL_ERROR ) RETURN 
      END IF
!
      IF ( DEG > 128 .AND. .NOT. OMP_IN_PARALLEL() ) THEN
           NUM_THR_SAVED = OMP_GET_NUM_THREADS()
           CALL OMP_SET_NUM_THREADS ( %VAL(FSH%NUM_THR) )
           NTHR = FSH%NUM_THR
         ELSE 
           NTHR = 1
      END IF
!
      IF ( NORM == 1 .OR. NORM == 4 ) THEN
!
! -------- Fast version
!
!$OMP      PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&            PRIVATE ( J1, J2 ), &
!$OMP&                    SCHEDULE ( DYNAMIC )
           DO 410 J1=0,DEG ! l
              DO 420 J2=0,J1 ! m
                 IF ( J1 .GE. 1 ) THEN
                      IF ( J2 == 0 ) THEN
                           IF ( J1 == 1 ) THEN
                                FSH%F1(J1,J2) = SQRI(3)
                                FSH%F2(J1,J2) = 0.0D0
                              ELSE 
                                FSH%F1(J1,J2) = SQRI(2*J1-1)* ( SQRI(2*J1+1)/ DBLE(J1) )
                                FSH%F2(J1,J2) = ( DBLE(J1-1)/ DBLE(J1) )* ( SQRI(2*J1+1)/ SQRI(2*J1-3) )
                           END IF
                         ELSE IF ( J2 .LE. J1-2 ) THEN
                           FSH%F1(J1,J2) = ( SQRI(2*J1+1)/ SQRI(J1+J2)  )* (SQRI(2*J1-1)/ SQRI(J1-J2) )
                           FSH%F2(J1,J2) = ( SQRI(2*J1+1)/ SQRI(2*J1-3) )* ( SQRI(J1-J2-1)/SQRI(J1-J2) )* &
     &                                     ( SQRI(J1+J2-1)/ SQRI(J1+J2) )
                         ELSE IF ( J2 .EQ. J1-1 ) THEN
                           FSH%F1(J1,J2) = ( SQRI(2*J1+1)/ SQRI(J1+J2) )* ( SQRI(2*J1-1)/ SQRI(J1-J2) )
                           FSH%F2(J1,J2) = 0.0D0
                      END IF 
                 END IF
!
                 IF ( J2 == 1 ) THEN
                      FSH%F3(J1,J2) = SQRI(J1+J2)*SQRI(J1-J2+1)/SQRI(2)
                    ELSE
                      FSH%F3(J1,J2) = SQRI(J1+J2)*SQRI(J1-J2+1)/2.0D0
                 END IF
  420         CONTINUE 
  410      CONTINUE 
!$OMP END PARALLEL DO
        ELSE 
           DO 430 J3=0,DEG ! l
              DO 440 J4=0,J3 ! m
                 IF ( J3 .GE. 1 ) THEN
                      IF ( J4 == 0 ) THEN
                           IF ( J3 == 1 ) THEN
                                IF ( NORM == 2 ) THEN
                                     FSH%F1(J3,J4) = 1.0D0
                                     FSH%F2(J3,J4) = 0.0D0
                                   ELSE IF ( NORM == 3 ) THEN
                                     FSH%F1(J3,J4) = DBLE(2*J3-1)/ DBLE(J3)
                                     FSH%F2(J3,J4) = DBLE(J3-1)/ DBLE(J3)
                                END IF
                              ELSE 
                                IF ( NORM == 2 ) THEN
                                     FSH%F1(J3,J4) = DBLE(2*J3-1)/ DBLE(J3)
                                     FSH%F2(J3,J4) = DBLE(J3-1)/ DBLE(J3)
                                   ELSE IF ( NORM == 3 ) THEN
                                     FSH%F1(J3,J4) = DBLE(2*J3-1)/ DBLE(J3)
                                     FSH%F2(J3,J4) = DBLE(J3-1)/ DBLE(J3)
                                END IF
                           END IF
                         ELSE IF ( J4 .LE. J3-2 ) THEN
                           IF ( NORM == 2 ) THEN
                                FSH%F1(J3,J4) =   DBLE(2*J3-1)/ SQRI(J3+J4)/ SQRI(J3-J4)
                                FSH%F2(J3,J4) = ( SQRI(J3+J4-1)/ SQRI(J3+J4) )* ( SQRI(J3-J4-1)/ SQRI(J3-J4) )
                              ELSE IF ( NORM == 3 ) THEN
                                FSH%F1(J3,J4) = DBLE(2*J3-1)/  DBLE(J3-J4)
                                FSH%F2(J3,J4) = DBLE(J3+J4-1)/ DBLE(J3-J4)
                           END IF
                         ELSE IF ( J4 .EQ. J3-1 ) THEN
                           IF ( NORM == 2 ) THEN
                                FSH%F1(J3,J4) = DBLE(2*J3-1)/ SQRI(J3+J4)/ SQRI(J3-J4)
                                FSH%F2(J3,J4) = 0.0D0
                              ELSE IF ( NORM == 3 ) THEN
                                FSH%F1(J3,J4) = DBLE(2*J3-1)/  DBLE(J3-J4)
                                FSH%F2(J3,J4) = DBLE(J3+J4-1)/ DBLE(J3-J4)
                           END IF 
                      END IF 
                 END IF
!
                 IF ( J4 == 1 ) THEN
                      FSH%F3(J3,J4) = SQRI(J3+J4)*SQRI(J3-J4+1)/SQRI(2)
                    ELSE
                      FSH%F3(J3,J4) = SQRI(J3+J4)*SQRI(J3-J4+1)/2.0D0
                 END IF
 440          CONTINUE 
 430       CONTINUE 
      END IF
      IF ( DEG > 128 .AND. .NOT. OMP_IN_PARALLEL() ) THEN
           CALL OMP_SET_NUM_THREADS ( %VAL(NUM_THR_SAVED) )
      END IF
      FSH%FF_STATUS = FSH__COMP
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_FF_X  !#!#
