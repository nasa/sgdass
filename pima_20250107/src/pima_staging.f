      SUBROUTINE PIMA_STAGING ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_STAGING 
! *                                                                      *
! *  ### 27-DEC-2012  PIMA_STAGING  v1.1 (c)  L. Petrov 08-MAR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      INTEGER*4  LEV
      INTEGER*8  DIR_DESC(16), IP8
      CHARACTER  FILNAM*128, STR*128
      LOGICAL*1  FL_FOUND(PIM__MFIL)
      INTEGER*8  ORIG_FILE_SIZE_I8, STAG_FILE_SIZE_I8
      INTEGER*4  IS, IPB, IPL, IRB, IRL, LF, UNIX_DATE, J1, J2, J3, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, LINDEX, FILE_INFO
!
! --- Remove stale files
!
      FL_FOUND = .FALSE.
!
! --- Scan the staging directory
!
      LEV = 0
      DO 410 J1=1,1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, PIM%CONF%STAGING_DIR, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6861, IUER, 'PIMA_STAGING', 'Error in '// &
     &            'reading input directory '// &
     &             PIM%CONF%STAGING_DIR(1:I_LEN(PIM%CONF%STAGING_DIR))// &
     &            '  '//FILNAM )
              RETURN 
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
!
! ------ Search for last "/" do separate file name from directory nme
!
         IPB = LINDEX ( FILNAM, '/' ) + 1
         IPL = ILEN(FILNAM)
!
! ------ Search the from the list of UV-file names without directotries
!
         DO 420 J2=1,PIM%L_FIL
            IRB = LINDEX ( PIM%FILE(J2)%NAME, '/' ) + 1
            IRL = ILEN   ( PIM%FILE(J2)%NAME      ) 
            IS = FILE_INFO ( FILNAM(1:I_LEN(FILNAM))//CHAR(0), &
     &                       UNIX_DATE, STAG_FILE_SIZE_I8 )
            IS = FILE_INFO ( PIM%FILE(J2)%NAME(1:I_LEN(PIM%FILE(J2)%NAME))//CHAR(0), &
     &                       UNIX_DATE, ORIG_FILE_SIZE_I8 )
!
            IF ( FILNAM(IPB:IPL) == PIM%FILE(J2)%NAME(IRB:IRL) .AND. &
     &           STAG_FILE_SIZE_I8 == ORIG_FILE_SIZE_I8 ) THEN
!
! -------------- The same name (except diretory)? The same size? Fine!
! -------------- Mark it as "found" and go to another 
! -------------- file in the staging directory
!
                 PIM%FILE(J2)%NAME = FILNAM
                 FL_FOUND(J2) = .TRUE.
                 GOTO 410
            END IF
 420     CONTINUE 
!
! ------ Remove the file 
!
         IS = UNLINK ( FILNAM(1:I_LEN(FILNAM))//CHAR(0) )
         IF ( IS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL GERROR ( STR )
              CALL ERR_LOG ( 6862, IUER, 'PIMA_STAGING', 'Failure in '// &
     &            'an attempt to remove stale file '//FILNAM(1:I_LEN(FILNAM))// &
     &            'in the staging directory: '//STR )
              RETURN 
         END IF
 410  CONTINUE 
 810  CONTINUE 
      LF = 0
!
! --- Now go through the list of UV files
!
      DO 430 J3=1,PIM%L_FIL
         IF ( .NOT. FL_FOUND(J3) ) THEN
!
! ----------- This UV file was not found in the staging directory?
! ----------- Copy it!
!
              LF = LF + 1
              IRB = LINDEX ( PIM%FILE(J3)%NAME, '/' ) 
              FILNAM = PIM%CONF%STAGING_DIR(1:I_LEN(PIM%CONF%STAGING_DIR))// &
     &                 '/'//PIM%FILE(J3)%NAME(IRB+1:)
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   WRITE ( 6, '(A)' ) 'PIMA_STAGING:  Copy file '// &
     &                                 PIM%FILE(J3)%NAME(1:I_LEN(PIM%FILE(J3)%NAME))
              END IF
!
! ----------- Copy the file
!
              CALL ERR_PASS  ( IUER, IER ) 
              CALL COPY_FILE ( PIM%FILE(J3)%NAME, FILNAM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6863, IUER, 'PIMA_STAGING', 'Failure in '// &
     &                 'an attempt to copy file with visibilities '// &
     &                 PIM%FILE(J3)%NAME(1:I_LEN(PIM%FILE(J3)%NAME))// &
     &                 ' into in the staging directory: '// &
     &                 PIM%CONF%STAGING_DIR )
                   RETURN 
              END IF
!
! ----------- And replace the file name 
!
              PIM%FILE(J3)%NAME = FILNAM 
         END IF
 430  CONTINUE 
      IF ( LF > 0 .AND. PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I4,A)' ) 'PIMA_STAGING:  ', LF, ' have been copied'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_STAGING  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COPY_FILE ( FILIN, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine copes input file FILIN to ouyt put file FILOUT.  *
! *   Copying is done with large chunks.                                 *
! *                                                                      *
! *  ### 27-DEC-2012   COPY_FILE   v1.0 (c)  L. Petrov  27-DEC-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILIN*128, FILOUT*128
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  UNIX_DATE, IS, LIN, LON, NB, BUF_LEN, J1, IER
      INTEGER*1, ALLOCATABLE :: BUF(:)
      INTEGER*8  CHUNK, SIZE_I8
      PARAMETER  ( CHUNK = 256*1024*1024 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, READ, WRITE
!
! --- Learn the file size
!
      IS = FILE_INFO ( FILIN(1:I_LEN(FILIN))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6891, IUER, 'COPY_FILE', 'Error '// &
     &         'an attempt to open to collect information about '// &
     &         'input file '//FILIN )
           RETURN 
      END IF
!
! --- Determine NB -- the number of chunks to be written.
! --- The last chunk may be smaller
!
      NB = SIZE_I8/CHUNK
      IF ( NB*CHUNK < SIZE_I8 ) NB = NB + 1
!
! --- Open input file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LIN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6892, IUER, 'COPY_FILE', 'Error '// &
     &                    'an attempt to open for reading '// &
     &                    'input file '//FILIN )
           RETURN 
      END IF
!
! --- Open output file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'NEW', LON, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6893, IUER, 'COPY_FILE', 'Error '// &
     &         'an attempt to open for writing output file '//FILOUT )
           RETURN 
      END IF
!
! --- Allocate memory for a buffer
!
      BUF_LEN = MIN ( CHUNK, SIZE_I8 ) 
      ALLOCATE ( BUF(BUF_LEN), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( BUF_LEN, STR )
           CALL ERR_LOG ( 6894, IUER, 'COPY_FILE', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for a temporary buffer' )
           RETURN 
      END IF
!
! --- Cycle over NB chunks
!
      DO 410 J1=1,NB
         BUF_LEN = CHUNK
         IF ( INT8(J1)*CHUNK > SIZE_I8 ) THEN
!
! ----------- The last chunk may be of less size
!
              BUF_LEN = SIZE_I8 - INT8(J1-1)*CHUNK
         END IF
!
! ------ Read the chunk from the input file
!
         IS = READ ( %VAL(LIN), BUF, %VAL(BUF_LEN) )
         IF ( IS == -1 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 6895, IUER, 'COPY_FILE', 'Error '// &
     &            STR(1:I_LEN(STR))//' in reading the input file '//FILIN )
              DEALLOCATE ( BUF )
              RETURN
            ELSE IF ( IS .NE. BUF_LEN ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 6896, IUER, 'COPY_FILE', 'Not all the data '// &
     &                       'have been read from the input file '//FILIN )
              DEALLOCATE ( BUF )
              RETURN
         END IF
!
! ------ Write the chunk in hte output file
!
         IS = WRITE ( %VAL(LON), BUF, %VAL(BUF_LEN) )
         IF ( IS == -1 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 6897, IUER, 'COPY_FILE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in writing the output file '//FILOUT )
              DEALLOCATE ( BUF )
              RETURN
            ELSE IF ( IS .NE. BUF_LEN ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 6898, IUER, 'COPY_FILE', 'Not all the data '// &
     &            'have been written into the  output file '//FILOUT )
              DEALLOCATE ( BUF )
              RETURN
         END IF
 410  CONTINUE 
!
! --- Close input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LIN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6899, IUER, 'COPY_FILE', 'Error in an attempt '// &
     &         'to close input file '//FILIN )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Close output file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LON, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6900, IUER, 'COPY_FILE', 'Error in an attempt '// &
     &         'to close output file '//FILOUT )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COPY_FILE  !#!  
