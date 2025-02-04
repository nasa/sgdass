      PROGRAM    SC_TRANSFORM
! ************************************************************************
! *                                                                      *
! *   Program  SC_TRANSFORM
! *                                                                      *
! *  ### 05-FEB-2016  SC_TRANSFORM  v1.0 (c)  L. Petrov  07-FEB-2016 ### *
! *                                                                      *
! ************************************************************************
      USE ISO_C_BINDING
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INCLUDE   'heb_c.inc'
      TYPE     ( HEB__TYPE   ) :: HEB_IN, HEB_OUT
      CHARACTER  FILIN*128, FILOUT*128, STR*128
      REAL*8     PRES_MIN
      INTEGER*4  IVRB, DEG, J1, J2, J3, J4, IR, IND_LON, IND_LAT, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      IVRB    =  2
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: filin deg filout' 
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, STR    )
           CALL CHIN   ( STR, DEG  )
           CALL GETARG ( 3, FILOUT )
      END IF
      IF ( DEG < 1 .OR. DEG > MALO__MDIM ) THEN
           IUER = -1
           WRITE ( 6, * ) 'deg = ', deg 
           CALL ERR_LOG ( 7401, IUER, 'SC_TRANSFORM', 'Wrong the second '// &
     &         'parameter '//STR(1:I_LEN(STR))//' -- should be in range '// &
     &         '[1, 33000]' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) 'Read input loading file'
           CALL FLUSH ( 6 ) 
      END IF
!    
      CALL READ_HEB ( FILIN, HEB_IN, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7402, IUER, 'SC_TRANSFORM', 'Failed to read '// &
     &         'the input loading file '//FILIN )
           RETURN 
      END IF
!
      HEB_OUT = HEB_IN
      HEB_OUT%DIMS(1) = (DEG+1)*4
      HEB_OUT%DIMS(2) = (DEG+1)*2+1
      HEB_OUT%DIMS(3) = HEB_IN%DIMS(3) 
      HEB_OUT%DIMS(4) = HEB_IN%DIMS(4) 
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*INT8(HEB_OUT%DIMS(1))*INT8(HEB_OUT%DIMS(2))* &
     &                           INT8(HEB_OUT%DIMS(3))*INT8(HEB_OUT%DIMS(4)), STR )
           IUER = -1
           CALL ERR_LOG ( 7403, IUER, 'SC_TRANSFORM', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array HEB_OUT%VAL' )
           CALL EXIT ( 1 )
      END IF
      IR = IDNINT ( (1.0D0*HEB_IN%DIMS(1))/HEB_OUT%DIMS(1) )
!
      DO 410 J1=1,HEB_OUT%DIMS(4)
         DO 420 J2=1,HEB_OUT%DIMS(3)
            DO 430 J3=1,HEB_OUT%DIMS(2)
               IND_LAT = (J3-1)*IR + 1
               DO 440 J4=1,HEB_OUT%DIMS(1)
                  IND_LON = (J4-1)*IR + 1
                  HEB_OUT%VAL(J4,J3,J2,J1) = HEB_IN%VAL(IND_LON,IND_LAT,J2,J1)
 440           CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( IVRB .GE. 1  ) THEN
           WRITE ( 6, 220 ) FILOUT(1:I_LEN(FILOUT))
 220       FORMAT ( ' Writing output file: ', A )
           CALL FLUSH ( 6 ) 
      END IF
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7408, IUER, 'SC_TRANSFORM', 'Failure in '// &
     &         'writing sampling correction into the output file '// &
     &          FILOUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, * ) 'Wrote output file '//FILOUT(1:I_LEN(FILOUT))
      CALL FLUSH ( 6 )
      CALL EXIT ( 0 )
      END  PROGRAM  SC_TRANSFORM  !#!#
