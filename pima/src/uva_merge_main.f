       PROGRAM    UVA_MERGE__MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL UVA_MERGE_MAIN()
       END  PROGRAM  UVA_MERGE__MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE UVA_MERGE_MAIN
! ************************************************************************
! *                                                                      *
! *   Program UVA_MERGE is for mering two or more input files with       *
! *   visibilities in FITS format into one consolidated file in FITS     *
! *   format.                                                            *
! *                                                                      *
! * ### 17-SEP-2012    UVA_MERGE    v1.1 (c) L. Petrov  06-OCT-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ), POINTER :: PIM(:)
      INTEGER*4  MFIL
      PARAMETER  ( MFIL = 128 )
      CHARACTER  FILIN(MFIL)*128, FILOUT*128, STR*128, COM*8192
      LOGICAL*1  LEX
      INTEGER*4  NFIL, J1, IS, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, UNLINK
!
      CALL CLRCH ( COM )
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: output_fits input_fits1 input_fits2 ...'
           CALL EXIT ( 1 ) 
        ELSE IF ( IARGC() > MFIL ) THEN
           WRITE ( 6, '(A)' ) 'Too many input files: more than 31'
           CALL EXIT ( 1 )
        ELSE
           CALL GETARG ( 0, COM    )
           CALL GETARG ( 1, FILOUT )
           INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
           IF ( LEX ) THEN
                IS = UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
                IF ( IS < 0 ) THEN
                     CALL CLRCH  ( STR )
                     CALL GERROR ( STR )
                     CALL ERR_LOG ( 1702, IUER, 'UVA_MERGE_MAIN', 'Failure '// &
     &                   'in an attempt to remove the output file '// &
     &                   FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
                     CALL EXIT ( 1 ) 
                END IF
           END IF
           NFIL = IARGC() - 1
           COM = TRIM(COM)//' '//FILOUT
           DO 410 J1=1,NFIL
              CALL GETARG ( J1+1, FILIN(J1) ) 
              INQUIRE ( FILE=FILIN(J1), EXIST=LEX ) 
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 1703, IUER, 'UVA_MERGE_MAIN', 'Cannot '// &
     &                 'find input file '//FILIN(J1) )
                   CALL EXIT ( 1 ) 
              END IF
              COM = TRIM(COM)//' '//FILIN(J1)
 410       CONTINUE 
      END IF     
!
      ALLOCATE ( PIM(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(PIM), STR )
           IUER = -1
           CALL ERR_LOG ( 1704, IUER, 'PIMA', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for PIMA '// &
     &         'internal data structures. What is going on? Do you really '// &
     &         'have so few memory?' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL UVA_MERGE ( PIM(1), NFIL, FILIN, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1705, IUER, 'UVA_MERGE_MAIN', 'Failure '// &
     &         'in an attempt to merge input FITS uva files' )
           WRITE ( 6, '(A)' ) 'Failed command: '//TRIM(COM)
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL PIMA_EXIT ( PIM(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1706, IUER, 'UVA_MERGE_MAIN', 'Trap of '// &
     &         'internal control: failure to close open files and '// &
     &         'deallocate dynamic memory used by PIMA' )
           CALL EXIT ( 1 )
      END IF
      DEALLOCATE ( PIM )
      CALL EXIT ( 0 )
!
      END  SUBROUTINE  UVA_MERGE_MAIN  !#!#
