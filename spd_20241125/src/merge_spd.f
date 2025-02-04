      PROGRAM  MERGE_SPD
! ************************************************************************
! *                                                                      *
! *   Prgogram MERGE_SPD merges slant path delays from two files into    *
! *   one. It is considered two files were computed for the same epoch   *
! *   using the same model, but for different station list. The station  *
! *   lists will be merged into one combined list.                       *
! *                                                                      *
! *  ### 15-APR-2015    MERGE_SPD  v1.0 (c)  L. Petrov  15-APR-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( SPD__ASCII__TYPE ), POINTER :: SAT(:)
      TYPE     ( SPD_3D__TYPE     ) :: SPD
      INTEGER*4  M_MOD, M_INP, M_STA
      PARAMETER  ( M_MOD = 8192 )
      PARAMETER  ( M_INP = 8192 )
      PARAMETER  ( M_STA = SPD__M_STA )
      CHARACTER  FIL1*128, FIL2*128, FILO*128, C_STA(M_STA)*8, &
     &           C_STA_SRT(M_STA)*8, SPD_CONF*128
      INTEGER*4  L_MOD(2), L_INP(2), IREF1_STA(M_STA), IREF2_STA(M_STA), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           IS, L_STA, IND, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_CLIST, LTM_DIF
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: merge_spd spd_conf fil1 fil2 filout'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, SPD_CONF )
           CALL GETARG ( 2, FIL1     )
           CALL GETARG ( 3, FIL2     )
           CALL GETARG ( 4, FILO     )
      END IF
!
! --- Read the SPD configuration file. In fact, only teh file name with
! --- format description is taken from the configuration file
!
      IUER = -1
      CALL SPD_3D_CONF ( SPD_CONF, SPD, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6301, IUER, 'MERGE_SPD', 'Error in parsing '// &
     &         'SPD configuration file '//SPD_CONF ) 
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( SAT(0:2), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6302, IUER, 'MERGE_SPD', 'Failure to allocate '// &
     &         'dynamic memore for SAT' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the first path delay file
!
      IUER = -1
      CALL SPD_3D_READ_ASCII ( FIL1, SAT(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6303, IUER, 'MERGE_SPD', 'Error in reading input '// &
     &         'SPD file '//FIL1 )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the first path delay file
!
      IUER = -1
      CALL SPD_3D_READ_ASCII ( FIL2, SAT(2), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6304, IUER, 'MERGE_SPD', 'Error in reading input '// &
     &         'SPD file '//FIL2 )
           CALL EXIT ( 1 )
      END IF
!
      L_STA = 0
      IREF1_STA = 0
      IREF2_STA = 0
!
! --- Get the station list from the 1st file
!
      DO 410 J1=1,SAT(1)%NS
         IUER = -1
         IS = ADD_CLIST ( M_STA, L_STA, C_STA, SAT(1)%SLINE(J1)%STA_NAME, IUER )
         IREF1_STA(J1) = J1 ! Cross-refrencing array
 410  CONTINUE 
!      WRITE ( 6, * ) 'SAT(1)%NS = ', SAT(1)%NS, ' SAT(2)%NS = ', SAT(2)%NS
!      WRITE ( 6, * ) 'L_STA= ', L_STA
!
! --- Get the satainolist from the second file
!
      DO 420 J2=1,SAT(2)%NS
         IUER = -1
         IF ( LTM_DIF ( 1, L_STA, C_STA, SAT(2)%SLINE(J2)%STA_NAME ) < 1 ) THEN
!
! ----------- A new station? Add it to the list!
!
              IS = ADD_CLIST ( M_STA, L_STA, C_STA, SAT(2)%SLINE(J2)%STA_NAME, IUER )
              IREF2_STA(L_STA) = J2
         END IF
 420  CONTINUE 
!
! --- Sort station list. We keep two lists: unsorted (C_STA) and sorted (C_STA_SRT)
!
      C_STA_SRT = C_STA
      CALL SORT_CH ( L_STA, C_STA_SRT )
!!      WRITE ( 6, * ) 'L_STA= ', L_STA
!
! --- Initialization
!
      CALL NOUT ( SIZEOF(SAT(0)), SAT(0) )
      SAT(0)%NM = SAT(1)%NM 
      SAT(0)%NI = SAT(1)%NI 
      SAT(0)%NS = L_STA
      SAT(0)%NE = SAT(1)%NE 
      SAT(0)%NA = SAT(1)%NA 
      SAT(0)%NF = SAT(1)%NF 
      SAT(0)%NLINE = SAT(1)%NLINE
      WRITE ( UNIT=SAT(0)%NLINE%N_STA, FMT='(I7)' ) SAT(0)%NS
!
      ALLOCATE ( SAT(0)%MLINE(SAT(0)%NM), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6305, IUER, 'MERGE_SPD', 'Failure to '// &
     &         'allocate memory for SAT%MLINE' )
           RETURN 
      END IF
!
      IF ( SAT(0)%NF > 0 ) THEN
           ALLOCATE ( SAT(0)%FLINE(SAT(0)%NF), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6306, IUER, 'MERGE_SPD', 'Failure to '// &
     &              'allocate memory for SAT(0)%FLINE' )
                RETURN 
           END IF
      END IF
!
      ALLOCATE ( SAT(0)%ILINE(SAT(0)%NI), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6307, IUER, 'MERGE_SPD', 'Failure to '// &
     &         'allocate memory for SAT(0)%ILINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT(0)%SLINE(SAT(0)%NS), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6308, IUER, 'MERGE_SPD', 'Failure to '// &
     &         'allocate memory for SAT(0)%SLINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT(0)%ELINE(SAT(0)%NE), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6309, IUER, 'MERGE_SPD', 'Failure to '// &
     &         'allocate memory for SAT(0)%ELINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT(0)%ALINE(SAT(0)%NA), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6310, IUER, 'MERGE_SPD', 'Failure to '// &
     &         'allocate memory for SAT(0)%ALINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT(0)%PLINE(SAT(0)%NS), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6311, IUER, 'MERGE_SPD', 'Failure to '// &
     &         'allocate memory for SAT(0)%PLINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT(0)%DLINE(SAT(0)%NE,SAT(0)%NA,SAT(0)%NS), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6312, IUER, 'MERGE_SPD', 'Failure to '// &
     &         'allocate memory for SAT(0)%DLINE' )
           RETURN 
      END IF
!
      IF ( SAT(0)%NF > 0 ) THEN
           ALLOCATE ( SAT(0)%OLINE(SAT(0)%NF,SAT(0)%NE,SAT(0)%NA,SAT(0)%NS), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6313, IUER, 'MERGE_SPD', 'Failure to '// &
     &              'allocate memory for SAT(0)%OLINE' )
                RETURN 
           END IF
      END IF
!
! --- Copy the model description and the input file description
!
      DO 430 J3=1,SAT(0)%NM 
         SAT(0)%MLINE(J3) = SAT(1)%MLINE(J3)
 430  CONTINUE 
!
      DO 440 J4=1,SAT(0)%NI
         SAT(0)%ILINE(J4) = SAT(1)%ILINE(J4)
 440  CONTINUE 
!
      SAT(0)%ULINE = SAT(1)%ULINE 
!
      IF ( SAT(0)%NF > 0 ) THEN
           DO 450 J5=1,SAT(0)%NF
              SAT(0)%FLINE(J5) = SAT(1)%FLINE(J5) 
 450       CONTINUE 
      END IF
!
! --- Copy station-related information
!
      DO 460 J6=1,SAT(0)%NS
!
! ------ Find the index of the J6-station of the unsorted list into the sorted station name list
!
         IND = LTM_DIF ( 1, L_STA, C_STA_SRT, C_STA(J6) )
         IF ( IREF1_STA(J6) > 0 ) THEN
!
! ----------- This station is from the 1st file
!
              SAT(0)%SLINE(IND) = SAT(1)%SLINE(IREF1_STA(J6)) 
              SAT(0)%PLINE(IND) = SAT(1)%PLINE(IREF1_STA(J6)) 
              SAT(0)%DLINE(1:SAT(0)%NE,1:SAT(0)%NA,IND) = SAT(1)%DLINE(1:SAT(0)%NE,1:SAT(0)%NA,IREF1_STA(J6)) 
              IF ( SAT(0)%NF > 0 ) THEN
                   SAT(0)%OLINE(1:SAT(0)%NF,1:SAT(0)%NE,1:SAT(0)%NA,IND) = SAT(1)%OLINE(1:SAT(0)%NF,1:SAT(0)%NE,1:SAT(0)%NA,IREF1_STA(J6)) 
              END IF
            ELSE
!
! ----------- This station is from the 2nd file
!
              SAT(0)%PLINE(IND) = SAT(2)%PLINE(IREF2_STA(J6)) 
              SAT(0)%SLINE(IND) = SAT(2)%SLINE(IREF2_STA(J6)) 
              SAT(0)%DLINE(1:SAT(0)%NE,1:SAT(0)%NA,IND) = SAT(2)%DLINE(1:SAT(0)%NE,1:SAT(0)%NA,IREF2_STA(J6))
              IF ( SAT(0)%NF > 0 ) THEN
                   SAT(0)%OLINE(1:SAT(0)%NF,1:SAT(0)%NE,1:SAT(0)%NA,IND) = SAT(2)%OLINE(1:SAT(0)%NF,1:SAT(0)%NE,1:SAT(0)%NA,IREF2_STA(J6)) 
              END IF
         END IF
!
! ------ Update station indices
!
	 WRITE ( UNIT=SAT(0)%SLINE(IND)%STA_IND_STR, FMT='(I7)' ) IND
	 WRITE ( UNIT=SAT(0)%PLINE(IND)%STA_IND, FMT='(I7)'     ) IND
         DO 470 J7=1,SAT(0)%NA
            DO 480 J8=1,SAT(0)%NE
	       WRITE ( UNIT=SAT(0)%DLINE(J8,J7,IND)%STA_IND, FMT='(I7)' ) IND
               IF ( SAT(0)%NF > 0 ) THEN
                    DO 490 J9=1,SAT(0)%NF
	                WRITE ( UNIT=SAT(0)%OLINE(J9,J8,J7,IND)%STA_IND, FMT='(I7)' ) IND
 490                CONTINUE 
               END IF
 480        CONTINUE 
 470     CONTINUE 
 460  CONTINUE 
!
! --- Copy elevation and azimuth lines
!
      DO 4100 J10=1,SAT(0)%NE
         SAT(0)%ELINE(J10) = SAT(1)%ELINE(J10)
 4100 CONTINUE 
!
      DO 4110 J11=1,SAT(0)%NA
         SAT(0)%ALINE(J11) = SAT(1)%ALINE(J11)
 4110 CONTINUE 
!
      SAT(0)%TLINE = SAT(1)%TLINE 
!
! --- Finally, write down the merged dataset into the output file
!
      IUER = -1
      CALL SPD_3D_ASCII_WRITE ( SPD, SAT(0), FILO, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6314, IUER, 'MERGE_SPD', 'Failure to '// &
     &         'write into the output file with slant path '// &
     &         'delays '//FILO )
           RETURN 
      END IF
      WRITE ( 6, '(A)' ) 'Written output file '//FILO(1:I_LEN(FILO))
!
      END  PROGRAM  MERGE_SPD   !#!#
