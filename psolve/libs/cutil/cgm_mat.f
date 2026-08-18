      SUBROUTINE CGM_MAT ( IDIRECT, FNAME, FILDES, ARR, NP, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CGM_MAT PROGRAM SPECIFICATION
!
! 1.1 Low level CGM MAT utility (read and write)
!
! 1.2 REFERENCES:
!
! 2.  CGM_MAT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4  NP
      INTEGER*2  IDIRECT(*)
      REAL*8     ARR(*)
      INTEGER*4  FILDES
      CHARACTER  FNAME*(*), STRING*(*)
!
! FILDES - File descriptor of CGM file
! FNAME - Name of CGM file
! IDIRECT - Contains information about size of chunks to read or write
! ARR - Array containing matrix
! NP - Number of parameters
! STRING - Type of access requested
!
! 2.3 OUTPUT Variables:
!
! ARR - The matrix
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: use_file
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   MPAR_I2
      INTEGER*4   JL, JB, JRND_BLOCKS, MPAR_I4, MX_PAR, LEN_TAIL
      INTEGER*8   I4, NUMEL
      INTEGER*4   LSEEK_SET, LN, LUN, IUER
      CHARACTER   STR*128, STR1*128, STR2*128
      INTEGER*4   MAT_E
      INTEGER*8   BYTES_TO_READ, BYTES_TAIL, IS8
      CHARACTER   ME*7
      DATA ME     / 'CGM_MAT' /
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
      INTEGER*8,  EXTERNAL :: READ64, WRITE64, LSEEK
!
!
! 4.  HISTORY
!     WHO   WHEN         WHAT
!     pet   2000.08.04   Converted to INTEGER*4, since MAX_PAR is now 16384
!     pet   2001.05.10   Fixed an important bug: the subroutine reads to ARR
!                        slightly more than it is necessary since it uses
!                        block-oriented input. Amount of read memory is
!                        computed as the nearest multilple of 256 block
!                        exceeding the length of ARR. the new version is
!                        zeroing the portion of memory which was read from
!                        the file which just follows the last element ARR.
!
!                        ... AAAAAAAAAAAAAAAAAAAtttt
!                        ...     |        |        |
!     pet   2017.10.23   Transformed for support of 64 bits and files larger 4Gb
!
!
! 5.  CGM_MAT PROGRAM STRUCTURE
!
      JL=IDIRECT(3)
      IF ( IDIRECT(4) .NE. 0 ) THEN
           MPAR_I2 = IDIRECT(4)
         ELSE
           MPAR_I2 = 192
      ENDIF
!
      IF ( IDIRECT(6) .NE. 0 .OR. IDIRECT(7) .NE. 0 ) THEN
           CALL MEMCPY ( MPAR_I4, IDIRECT(6), %VAL(4) )
         ELSE 
           MPAR_I4 = INT4(MPAR_I2)
      END IF
#ifdef DEBUG
   write ( 6, * ) 'CGM_MAT-89 MPAR_I4= ', MPAR_I4, ' JL = ', JL, ' NP= ', NP ! %%%%%%
#endif
!
      MX_PAR  = M_GPA
      IF ( STRING .EQ. 'W'   .AND.   MPAR_I4 .NE. MX_PAR ) THEN
           WRITE ( 6, * ) ' MPAR_I4 = ',MPAR_I4,'  MX_PAR = ',MX_PAR
           CALL FERR ( INT2(150), '(CGM_MAT) -- Size mismatch', INT2(0), &
     &                 INT2(0) )
      ENDIF
#ifndef pre_2017
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', LSEEK_SET, LN ) 
      IS8 = LSEEK ( %VAL(FILDES), %VAL(256*INT8((JL-1))), %VAL(LSEEK_SET) )
      IF ( IS8 < 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL FILE_REPORT ( FNAME, 'CGM_MAT', 'Error in seeking the '// &
     &         'position to read or write: '//STR(1:I_LEN(STR)) )
           CALL EXIT ( 1 )
      END IF
      BYTES_TO_READ = 8*(3*INT8(MPAR_I4) + (INT8(NP)*INT8(NP+1))/2)
      BYTES_TAIL = 256 - MOD(BYTES_TO_READ,256)
      IF ( BYTES_TAIL == 256 ) THEN
           BYTES_TAIL = 0
         ELSE 
           BYTES_TO_READ = BYTES_TO_READ + BYTES_TAIL
      END IF
      IF ( STRING(1:1) == 'R' ) THEN
           IS8 = READ64 ( FILDES, ARR, BYTES_TO_READ )
!
           IF ( IS8 .GE. 0 .AND. IS8 .NE. BYTES_TO_READ ) THEN
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                CALL IINCH8 ( IS8, STR1 )
                CALL IINCH8 ( BYTES_TO_READ, STR2 )
                CALL FILE_REPORT ( FNAME, 'CGM_MAT', 'Not all bytes '// &
     &              'were read from the input file '//FNAME(1:I_LEN(FNAME))// &
     &              ' only '//STR1(1:I_LEN(STR1))//' out of '// &
     &              STR2(1:I_LEN(STR2)) )
                CALL EXIT ( 1 )
           END IF
           IF ( IS8 < 0 ) THEN
                CALL GERROR ( STR )
                CALL FILE_REPORT ( FNAME, 'CGM_MAT', 'Failure in '// &
     &              'reading from the input file '//FNAME(1:I_LEN(FNAME))// &
     &              ' -- '//STR(1:LEN(STR)) )
                CALL EXIT ( 1 )
           END IF
         ELSE IF ( STRING(1:1) == 'W' ) THEN
           IS8 = WRITE64 ( FILDES, ARR, BYTES_TO_READ )
           IF ( IS8 .GE. 0 .AND. IS8 .NE. BYTES_TO_READ ) THEN
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                CALL IINCH8 ( IS8, STR1 )
                CALL IINCH8 ( BYTES_TO_READ, STR2 )
                CALL FILE_REPORT ( FNAME, 'CGM_MAT', 'Not all bytes '// &
     &              'were written to the output file '//FNAME(1:I_LEN(FNAME))// &
     &              ' only '//STR1(1:I_LEN(STR1))//' out of '// &
     &              STR2(1:I_LEN(STR2)) )
                CALL EXIT ( 1 )
           END IF
           IF ( IS8 < 0 ) THEN
                CALL GERROR ( STR )
                CALL FILE_REPORT ( FNAME, 'CGM_MAT', 'Failure in '// &
     &              'writing to the output file '//FNAME(1:I_LEN(FNAME))// &
     &              ' -- '//STR(1:LEN(STR)) )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( BYTES_TAIL == 0 ) THEN
           JB = (BYTES_TO_READ - BYTES_TAIL)/256
         ELSE
           JB = (BYTES_TO_READ - BYTES_TAIL)/256 + 1
      END IF
      IF ( BYTES_TAIL > 0 ) THEN
           CALL NOUT8 ( BYTES_TAIL, ARR((BYTES_TO_READ-BYTES_TAIL)/8+1) )
      END IF
!      IF ( MPAR_I2 < 32760 ) THEN
!           JB = JRND_BLOCKS ( MAT_E(MPAR_I2,NP)*REALL_WORDS )
!      END IF
#else
!
! --- Physical reading/writing of JB 256-byte-long blocks
!
      JB = JRND_BLOCKS ( MAT_E(MPAR_I2,NP)*REALL_WORDS )
      CALL USE_FILE ( FNAME, FILDES, ARR, JB, JL, STRING )
!
! --- Compute the lenght of the tail
!
      LEN_TAIL = JB*INT4(BLOCK_BYTES) - MAT_E(MPAR_I2,NP)*8
      IF ( LEN_TAIL .GT. 0 ) THEN
!
! -------- Zero elemets of ARR which just follow the last legitimate element
!
           CALL NOUT ( LEN_TAIL, ARR(MAT_E(MPAR_I2,NP)+1) )
      END IF
#endif
!
! --- Fix up CGM if it was created with MX_PAR less than current
!
      IF ( MPAR_I4 .NE. MX_PAR ) THEN
           NUMEL = INT8(JB)*32
           DO I4=NUMEL,MPAR_I4*3+1,-1
              ARR(I4+3*(MX_PAR-MPAR_I4)) = ARR(I4)
           ENDDO
!
           DO I4=3*MPAR_I4,2*MPAR_I4+1,-1
              ARR(I4+2*(MX_PAR-MPAR_I4)) = ARR(I4)
           ENDDO
!
           DO I4=2*MPAR_I4,MPAR_I4+1,-1
              ARR(I4+MX_PAR-MPAR_I4) = ARR(I4)
           ENDDO
!
           DO I4=MPAR_I4+1,MX_PAR
              ARR(I4) = 0.D0
           ENDDO
!
           DO I4=MX_PAR+MPAR_I4+1,2*MX_PAR
              ARR(I4) = 0.D0
           ENDDO
!
           DO I4=2*MX_PAR+MPAR_I4+1,3*MX_PAR
              ARR(I4) = 0.D0
           ENDDO
      ENDIF
!
      RETURN
      END  !#!  CGM_MAT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION JRND_BLOCKS(WORDS)
      IMPLICIT NONE
!
! 1.  JRND_BLOCKS PROGRAM SPECIFICATION
!
! 1.1 Determine how many blocks are required to hold the
!     specified number of words.
!
! 1.2 REFERENCES:
!
! 2.  JRND_BLOCKS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! NOTE: BLOCK_WORDS, the number of words per block, is defined
!       as a parameter in solve.i
!
! 2.2 INPUT Variables:
!
      INTEGER*4 WORDS
!
! WORDS - Number of words
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 JRND_BLOCKS
!
! JRND_BLOCKS - Number of blocks required to hold WORDS
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  JRND_BLOCKS PROGRAM STRUCTURE
!
      JRND_BLOCKS=(WORDS+BLOCK_WORDS-1)/BLOCK_WORDS
!
      RETURN
      END
