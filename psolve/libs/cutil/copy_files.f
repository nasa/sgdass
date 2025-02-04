      SUBROUTINE COPY_FILE ( SOURCE_NAME, DESTIN_NAME, QUIET_MODE, BLOCKS )
! ************************************************************************
! *                                                                      *
! *   This is a simple copy routine to copy a source file to             *
! *   a destination file.                                                *
! *                                                                      *
! *   If the destination file exists, it had better be the correct type  *
! *   and size to hold the input, it doesn't work for type 2 files or    *
! *   files of larger extent than 32767 blocks, but could be easily      *
! *   fixed to do so.                                                    *
! *                                                                      *
! *   The call is:                                                       *
! *                                                                      *
! *   CALL COPY_FILES ( SOURCE_NAME, DESTIN_NAME, QUIET_MODE, BLOCKS )   *
! *                                                                      *
! * ____________________________ INPUT PARAMETERS: _____________________ *
! *                                                                      *
! *   SOURCE_NAME ( CHARACTER ) -- source namr                           *
! *   DESTIN_NAME ( CHARACTER ) -- destination namr                      *
! *    QUIET_MODE ( CHARACTER ) -- if present this means quiet           *
! *        BLOCKS ( INTEGER*8 ) -- blocks to transfer                    *
! *                                                                      *
! *  ###  ???        COPY_FILE     v2.0  (c)  ???        ???        ###  *
! *  HISTORY:                                                            *
! *  jlr  921215       replaced nJ's with I4Pn's                         *
! *  pet  13-FEB-1997  made call-interface for old code, petified        *
! *                    comments.                                         *
! *                                                                      *
! ************************************************************************
      IMPLICIT  NONE
      INCLUDE  'solve.i'
      INTEGER*4 BLOCKS
!
! parameters
!
      INTEGER*4 JMEM_BLOCKS,JMEM_WORDS
      PARAMETER ( &
     &           JMEM_BLOCKS=256, &
     &           JMEM_WORDS=JMEM_BLOCKS*BLOCK_WORDS)
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2 TRIMLEN
!
!  VARIABLES
!
      CHARACTER SOURCE_NAME*(*), DESTIN_NAME*(*), QUIET_MODE*(*)
      CHARACTER BUFSTR*80
      INTEGER*2 MEM(JMEM_WORDS), LENS, LEND
      INTEGER*4 JTRANS, JMOVE, FILDES, FILSRC
      INTEGER*4 I4P0
!
!  STORAGE MANIPULATION
!
      DATA I4P0 /0/
!
      IF ( QUIET_MODE .NE. 'Q' ) then
           call start_mn()
           call setcr_mn ( I4P0, I4P0 )
           call reverse_on_mn()
           call addstr_f ( "COPY_FILE")
           call reverse_off_mn()
           write ( bufstr, * ) '  v2.0  13-FEB-97  Buffer size: ', &
     &                          jmem_blocks,' Blocks'
           call addstr_f ( bufstr )
           call nl_mn()
      endif
!
! --- Openning files
!
      CALL BIN_OPEN ( SOURCE_NAME, FILSRC, 'O' )
      CALL BIN_OPEN ( DESTIN_NAME, FILDES, 'O' )
!
! --- Loop for reading writing portions of JMOVE words?? or bytes??
!
      JMOVE=BLOCKS
      DO WHILE ( JMOVE.GT.0 )
         JTRANS = MIN ( JMEM_BLOCKS, JMOVE )
         CALL BIN_READ  ( SOURCE_NAME, FILSRC, MEM, JTRANS )
         CALL BIN_WRITE ( DESTIN_NAME, FILDES, MEM, JTRANS )
         JMOVE=JMOVE-JTRANS
      ENDDO
!
      if ( quiet_mode .ne. 'Q' ) THEN
!
           lens=max( int2(1), trimlen(source_name) )
           lend=max( int2(1), trimlen(destin_name) )
           write ( bufstr, 9955 ) source_name(1:lens), destin_name(1:lend)
 9955      format(" ",A," COPIED TO ",A)
           call addstr_f ( bufstr )
           call nl_mn()
           call end_mn()
      endif
!
! --- And closing files
!
      CALL BIN_CLOSE ( SOURCE_NAME, FILSRC )
      CALL BIN_CLOSE ( DESTIN_NAME, FILDES )
!
      RETURN
      END  !#!  COPY_FILES  #!#
