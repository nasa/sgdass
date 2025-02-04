      SUBROUTINE GVH_READ_DB ( FILENV, GVF_DB_DIR, GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVH_READ_DB reads database either in GVF or VDA format     *
! *   and puts results of parsing in GVH object.                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *                                                                      *
! *     FILENV ( CHARACTER ) -- File name. It interpretation depends     *
! *                             on GVF_DB_DIR argument.                  *
! *                             If GVF_DB_DIR is "vda" or "-vda", then   *
! *                             FILENEV is the full path name of the     *
! *                             database file in VGOSDA format.          *
! *                             If not, then FILENV is the full path     *
! *                             name of the database envelop. In that    *
! *                             case argument GVF_DB_DIR defines the     *
! *                             directory name for binary database files.*
! * GVF_DB_DIR ( CHARACTER ) -- either "vda" or "-vda" or the directory  *
! *                             name for binary database files.          *
! *                             In the first case GVF_DB_DIR just        *
! *                             indicates that a file in ascii vgosda    *
! *                             format will be processed. In the second  *
! *                             case it supplies additional information  *
! *                             to locate files to be read.              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     GVH ( GVH__STRU      ) -- Data structure which keeps internal    *
! *                               information related to the database of *
! *                               an astro/geo VLBI experiment.          *
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
! *   
! *                                                                      *
! *  ### 15-JUN-2020  GVH_READ_DB  v1.0 (c)  L. Petrov  15-JUN-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU  ) :: GVH
      CHARACTER  FILENV*(*), GVF_DB_DIR*(*)
      INTEGER*4  IUER
      INTEGER*4    M_FIL, MBUF
      PARAMETER  ( M_FIL =  32 ) 
      PARAMETER  ( MBUF  = 8192 ) 
      CHARACTER  FILIN(M_FIL)*128, STR*80, DBNAME*10, BUF(MBUF)*128
      INTEGER*4  J1, J2, IB, IE, L_FIL, IL, ISL_MAX, ISEG_SL, NBUF, &
     &           ISL, REMAINED_BYTES, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1211, IUER, 'GVH_READ_DB', 'Error in an attempt to '// &
     &         'initialize GVH' )
           RETURN
      END IF
!
      IF ( GVF_DB_DIR == 'vda' .OR. GVF_DB_DIR == '-vda' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_READ_AGV ( GVH, 0, FILENV, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1212, IUER, 'GVH_READ_DB', 'Error in an attempt to '// &
     &              'read database envelope file '//FILENV )
                RETURN 
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      ENDIF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILENV, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1213, IUER, 'GVH_READ_DB', 'Error in an attempt to '// &
     &         'read database envelope file '//FILENV )
           RETURN 
      END IF
      GVH%FILEENV = FILENV
!
      IB = LINDEX ( FILENV, '/' ) + 1
      IE = IB + 9
!
      L_FIL = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '!' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '$' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         L_FIL = L_FIL + 1
         IF ( BUF(J1)(20:20) .EQ. ' ' .AND. BUF(J1)(22:22) .EQ. ' ' ) THEN
!
! ----------- Post-Oct2019 scheme
!
              FILIN(L_FIL) = TRIM(GVF_DB_DIR)//'/'// &
     &                       FILENV(IB:IE-1)//BUF(J1)(21:21)//'_'//BUF(J1)(23:I_LEN(BUF(J1)))
            ELSE 
              CALL ERR_LOG ( 1214, IUER, 'GVH_READ_DB', 'Error in an parsing '// &
     &            'envelop file '//TRIM(FILENV)//' -- wrong format' )
              RETURN 
         END IF
         CALL BLANK_TO_ZERO ( BUF(J1)(13:15))
         FILIN(L_FIL) = TRIM(FILIN(L_FIL))//'_'// &
     &                  BUF(J1)(9:11)//'_v'//BUF(J1)(13:15)//'.'// &
     &                  BUF(J1)(17:19)
 410  CONTINUE
!
      ISL_MAX = 0
      ISEG_SL = 0
      DO 420 J2=1,L_FIL
!!         write ( 6, * ) 'j2= ', int2(j2), ' filin= ', trim(filin(J2)) ! %%%%%%%%
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_READ_BGV ( GVH, 1, FILIN(J2), REMAINED_BYTES, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1216, IUER, 'GVH_READ_DB', 'Error in an attempt '// &
     &            'to read input database file '//FILIN(J2) )
              RETURN 
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              CALL ERR_LOG ( 1217, IUER, 'GVH_READ_DB', 'The number of '// &
     &            'remaining bytes after reading input databae file '// &
     &             FILIN(J2)(1:I_LEN(FILIN(J2)))//' is not 0, but '//STR )
              RETURN 
         END IF
         IL = ILEN(FILIN(J2))
         IF ( FILIN(J2)(IL-12:IL-10) == '_sl' ) THEN
              CALL CHIN ( FILIN(J2)(IL-9:IL-9), ISL )
              ISL_MAX = MAX ( ISL_MAX, ISL )
              ISEG_SL = J2
         END IF
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREGET ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1218, IUER, 'GVH_READ_DB', 'Error in an attempt to '// &
     &         'execute GVH_PREGET' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVH_READ_DB  !#!#
