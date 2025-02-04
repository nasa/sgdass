      PROGRAM    HEB_DIFF
! ************************************************************************
! *                                                                      *
! *   Program  HEB_DIFF
! *                                                                      *
! *  ### 21-NOV-2013    HEB_DIFF   v2.0 (c)  L. Petrov  25-APR-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB1, HEB2, HEBO
      CHARACTER  FIL1*128, FIL2*128, FILOUT*128, STR*128
      INTEGER*8  LEN_ELEM
      INTEGER*4  J1, J2, J3, J4, IDIM, IUER
      INTEGER*4, EXTERNAL ::ILEN, I_LEN
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: heb_diff fil1 fil2 filout [max_dim]'
           CALL EXIT ( 1 )
         ELSE
!
! -------- Parse input arguments and perform some checks
!
           CALL GETARG ( 1, FIL1   )
           CALL GETARG ( 2, FIL2   )
           CALL GETARG ( 3, FILOUT )
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, STR )
                CALL CHIN   ( STR, IDIM )
             ELSE 
                IDIM = 10
           END IF
      END IF
      IUER = -1
      CALL READ_HEB ( FIL1, HEB1, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5801, -2, 'HEB_DIFF', 'Error in reading the '// &
     &         'first HEB-file '//FIL1 )
           CALL EXIT ( 1  )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL2, HEB2, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5802, -2, 'HEB_DIFF', 'Error in reading the '// &
     &         'second HEB-file '//FIL2 )
           CALL EXIT ( 1  )
      END IF
!  heb1%fill_value = -9999
!  heb2%fill_value = -9999
      CALL GETENVAR ( 'HEB_DIMS', STR )
      IF ( IDIM .GE. 3 ) HEB2%DIMS(4) = HEB1%DIMS(4) 
      IF ( IDIM .GE. 2 ) HEB2%DIMS(3) = HEB1%DIMS(3) 
      IF ( IDIM .GE. 1 ) HEB2%DIMS(2) = HEB1%DIMS(2) 
!
      IF ( HEB1%DIMS(1) .NE. HEB2%DIMS(1) .OR. &
     &     HEB1%DIMS(2) .NE. HEB2%DIMS(2) .OR. &
     &     HEB1%DIMS(3) .NE. HEB2%DIMS(3) .OR. &
     &     HEB1%DIMS(4) .NE. HEB2%DIMS(4)      ) THEN
!
           CALL ERR_LOG ( 5803, -2, 'HEB_DIFF', 'Two heb files have differnet '// &
     &         'dimensions. Their comparison does not have sense' )
           CALL EXIT ( 1  )
      END IF
      IF ( ASSOCIATED ( HEB1%VAL8 ) ) THEN
           ALLOCATE ( HEB1%VAL(HEB1%DIMS(1),HEB1%DIMS(2),HEB1%DIMS(3),HEB1%DIMS(4)) )
           HEB1%VAL = HEB1%VAL8
           DEALLOCATE ( HEB1%VAL8 )
      END IF
      IF ( ASSOCIATED ( HEB2%VAL8 ) ) THEN
           ALLOCATE ( HEB2%VAL(HEB2%DIMS(1),HEB2%DIMS(2),HEB2%DIMS(3),HEB2%DIMS(4)) )
           HEB2%VAL = HEB2%VAL8
           DEALLOCATE ( HEB2%VAL8 )
      END IF
!
      DO 410 J1=1,HEB1%DIMS(4)
         DO 420 J2=1,HEB1%DIMS(3)
            DO 430 J3=1,HEB1%DIMS(2)
               DO 440 J4=1,HEB1%DIMS(1)
                  IF ( ABS(HEB1%VAL(J4,J3,J2,J1)) > ABS(HEB1%FILL_VALUE/2.0) ) THEN
                       HEB1%VAL(J4,J3,J2,J1) = 0
                  END IF
                  IF ( ABS(HEB2%VAL(J4,J3,J2,J1)) > ABS(HEB2%FILL_VALUE/2.0) ) THEN
                       HEB2%VAL(J4,J3,J2,J1) = 0
                  END IF
 440           CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      HEBO = HEB1
      HEBO%VAL => NULL()
      ALLOCATE ( HEBO%VAL(HEBO%DIMS(1),HEBO%DIMS(2),HEBO%DIMS(3),HEBO%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           LEN_ELEM = 4
           CALL CLRCH   ( STR )
           CALL IINCH8  ( LEN_ELEM*HEBO%DIMS(1)*HEBO%DIMS(2)*HEBO%DIMS(3)*HEBO%DIMS(4), STR )
           CALL ERR_LOG ( 5804, -2, 'HEB_DIFF', 'Error in '// &
     &         'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array HEBO%VAL' )
           CALL EXIT ( 1 ) 
      END IF
      HEBO%VAL = HEB1%VAL(1:HEB1%DIMS(1),1:HEB1%DIMS(2),1:HEB1%DIMS(3),1:HEB1%DIMS(4)) -  &
     &           HEB2%VAL(1:HEB2%DIMS(1),1:HEB2%DIMS(2),1:HEB2%DIMS(3),1:HEB2%DIMS(4))
! %%%%%%%%%%
!         call plot_grid_r4 ( 1, 7, 0, 1, int(hebo%dims(1),kind=4), int(hebo%dims(2),kind=4), &
!     &                       hebo%val, 'Diff', '?', '/tmp/foo', iuer )
! %%%%%%%%%%
!
      CALL HEB_MINMAX ( HEBO, HEBO%VAL, HEBO%FILL_VALUE/2.0 )
      HEBO%OFFSET    = 0.0
      HEBO%SCALE_FACTOR = 1.0
      HEBO%VALID_RANGE(1) = HEBO%MIN_VALUE 
      HEBO%VALID_RANGE(2) = HEBO%MAX_VALUE 
      HEBO%SDS_NAME = 'Differences in '//HEBO%SDS_NAME 
      HEBO%FILE_NAME = FILOUT
      HEBO%DATA_TRANSFORM = HEB__NONE
      HEBO%DATA_FORMAT    = HEB__R4
!
! --- Write the output HEB-dataset into the output file FILOUT
!
      IUER = -1
      CALL WRITE_HEB ( HEBO, HEBO%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5805, -2, 'HEB_DIFF', 'Failure to write '// &
     &         'the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM    HEB_DIFF  !#!#
