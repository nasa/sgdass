        FUNCTION ADD_LIS ( M, L, LIS, IPAR, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  ADD_LIS  scans the integer list and searches there      *
! *   the element IPAR. If it does not find IPAR in the list, ADD_LIS    *
! *   adds to the end of the list:                                       *
! *        L := L+1                                                      *
! *        LIS(L) := IPAR                                                *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *        M ( INTEGER*4 )  --  maximal length of the list LIS.          *
! *     IPAR ( INTEGER*4 )  --  the element under investigation.         *
! *                                                                      *
! * _________________________ Modified paramters: ______________________ *
! *                                                                      *
! *        L ( INTEGER*4 )  --  The number of elements in the list LIS.  *
! *      LIS ( INTEGER*4 )  --  The list under investigation.            *
! *                             Dimension: M.                            *
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
! *  ###  31-JAN-1992    ADD_LIS   v2.1  (c)  L. Petrov 14-MAY-1998  ### *
! *                                                                      *
! ************************************************************************
        INTEGER*4  ADD_LIS, LIS(M), I_LEN
        CHARACTER  STR*20, STR1*20
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT
!        INTEGER*4 NUM$ARG, NA, N_ARG
!!
!        PARAMETER ( N_ARG=5 )  !  Number of formal arguments
!
! ----- Check: whether the number of formal argumenhts is equation to the
! ----- teh number of actual arguments
!
!        NA=NUM$ARG()  !  The nnumber of actual arguments
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL  VER$ARG ( N_ARG )
!!                        ///
!! ......................///  ...   End of check
!!
        IF ( L .GT. M ) THEN
             CALL CLRCH (    STR  )
             CALL INCH  ( L, STR  )
             CALL CLRCH (    STR1 )
             CALL INCH  ( M, STR1 )
             CALL ERR_LOG ( 1, IUER, 'ADD_LIS', 'List overflow: specified '// &
     &           'length of the list ('//STR(1:I_LEN(STR))// &
     &           ') appeared larger than max length ('//STR1(1:I_LEN(STR1))// &
     &           ') ' )
             RETURN
        END IF
        IF ( L.EQ.0 ) THEN
             L=1
          ELSE
             DO 410 J1=1,L
!
! ------------- Scan the list
!
                IF ( LIS(J1).EQ.IPAR ) THEN
                     ADD_LIS = J1
                     CALL ERR_LOG ( 0, IUER )
                     RETURN
                END IF
  410        CONTINUE
             L=L+1
        END IF
        IF ( L.GT.M ) THEN
             ADD_LIS = -1
             CALL CLRCH ( STR )
             CALL INCH  ( M, STR )
             CALL ERR_LOG ( 1, IUER, 'ADD_LIS', 'List overflow. Length of '// &
     &           'the list ('//STR(1:I_LEN(STR))//' appeared too short' )
             RETURN
        END IF
        ADD_LIS = L
        LIS(L)=IPAR
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  ADD_LIS  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION   IFIND_PL ( L, LIS, NEL )
! ************************************************************************
! *                                                                      *
! *     Finction  IFIND_PL  scans the list LIS which has L elements, and *
! *   search the element NEL. If it finds the element NEL, IFIND_PL      *
! *   returns its index, if not IFIND_PL returns -1.                     *
! *                                                                      *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *       L ( INTEGER*4 )  --  length of the list LIS.                   *
! *     LIS ( INTEGER*4 )  --  The sorted list where the serach is       *
! *                            performed.                                *
! *     NEL ( INTEGER*4 )  --  Element under consideration.              *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! *  <IFIND_PL> ( INTEGER*4 )  --  Index of NEL in the list LIS. If not  *
! *                                found, then -1.                       *
! *                                                                      *
! *  ###  31-JAN-1992   IFIND_PL   v2.1 (c)  L. Petrov 04-JAN-1994  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4  IFIND_PL, L, LIS(*), NEL
        IF ( L.LT.1 ) THEN
             IFIND_PL=-2
             RETURN
        END IF
        DO 410 J1=1,L
           IF ( LIS(J1).EQ.NEL ) THEN
                IFIND_PL=J1
                RETURN
           END IF
  410   CONTINUE
        IFIND_PL=-1
        RETURN
        END  !#!  IFIND_PL  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION   IFIND_PL8 ( M8, LIS8, NEL8 )
! ************************************************************************
! *                                                                      *
! *     Finction  IFIND_PL8 scans the list LIS which has L elements, and *
! *   search the element NEL. If it finds the element NEL, IFIND_PL      *
! *   returns its index, if not IFIND_PL returns -1.                     *
! *                                                                      *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *      M8 ( INTEGER*8 )  --  length of the list LIS.                   *
! *    LIS8 ( INTEGER*8 )  --  The sorted list where the serach is       *
! *                            performed.                                *
! *    NEL8 ( INTEGER*8 )  --  Element under consideration.              *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! *  <IFIND_PL> ( INTEGER*8 )  --  Index of NEL in the list LIS. If not  *
! *                                found, then -1.                       *
! *                                                                      *
! *  ###  31-JAN-1992   IFIND_PL8  v2.2 (c)  L. Petrov 01-NOV-2005  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*8  IFIND_PL8
        INTEGER*8  M8, LIS8(*), NEL8
        INTEGER*8  J1
        IF ( M8 .LT. 1 ) THEN
             IFIND_PL8=-2
             RETURN
        END IF
        DO 410 J1=1,M8
           IF ( LIS8(J1) .EQ. NEL8 ) THEN
                IFIND_PL8=J1
                RETURN
           END IF
  410   CONTINUE
        IFIND_PL8=-1
        RETURN
        END  !#!  IFIND_PL  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   IFIND_SORT_PL ( L, LIS, NEL )
! ************************************************************************
! *                                                                      *
! *     Finction  IFIND_SORT_PL  scans the sorted list LIS which has  L  *
! *   elements, and search the element NEL using binary search           *
! *   algorithm. If it finds the element NEL, IFIND_SORT returns its     *
! *   index, if not IFIND_SORT_PL returns -1 .                           *
! *                                                                      *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *       L ( INTEGER*4 )  --  length of the list LIS.                   *
! *     LIS ( INTEGER*4 )  --  The sorted list where the serach is       *
! *                            performed.                                *
! *     NEL ( INTEGER*4 )  --  Element under consideration.              *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! *  <IFIND_SORT_PL> ( INTEGER*4 )  --  Index of NEL in the list LIS.    *
! *                                     If not found, then -1.           *
! *                                                                      *
! *  ### 07-JAN-2006  IFIND_SORT_PL  v1.1 (c) L. Petrov 23-NOV-2008  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IFIND_SORT_PL, L, LIS(*), NEL
      INTEGER*4  IL, IR, IM
      IF ( L.LT.1 ) THEN
           IFIND_SORT_PL = -2
           RETURN
      END IF
!
! --- Binary search
!
      IL = 1
      IR = L
 910  CONTINUE 
         IM = IL + (IR - IL)/2
         IF ( IL .GT. IR ) THEN
              IFIND_SORT_PL = -1
              GOTO 810
         END IF
         IF ( NEL .LT. LIS(IM) ) THEN
              IR=IM-1
              GOTO 910
            ELSE IF ( NEL .GT. LIS(IM) ) THEN
              IL=IM+1
              GOTO 910
            ELSE IF ( NEL .EQ. LIS(IM) ) THEN
              IFIND_SORT_PL = IM
              GOTO 810
         END IF               
 810  CONTINUE 
      RETURN
      END  FUNCTION  IFIND_SORT_PL  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   IFIND_SORT_PL8 ( L8, LIS8, NEL8 )
! ************************************************************************
! *                                                                      *
! *     Finction  IFIND_SORT_PL8  scans the sorted list LIS8 which has   *
! *   L4 elements, and searches for the element NEL using the binary     *
! *   search algorithm. If it finds the element NEL, IFIND_SORT returns  *
! *   its index, if not IFIND_SORT_PL returns -1 .                       *
! *                                                                      *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *      L8 ( INTEGER*8 )  --  length of the list LIS.                   *
! *    LIS8 ( INTEGER*8 )  --  The sorted list where the serach is       *
! *                            performed.                                *
! *    NEL8 ( INTEGER*8 )  --  Element under consideration.              *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! *  <IFIND_SORT_PL8> ( INTEGER*8 )  --  Index of NEL in the list LIS.   *
! *                                      If not found, then -1.          *
! *                                                                      *
! * ### 07-JAN-2006  IFIND_SORT_PL8  v1.1 (c) L. Petrov 23-NOV-2008  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  IFIND_SORT_PL8, L8
      INTEGER*8  LIS8(*), NEL8
      INTEGER*8  IL, IR, IM
      IF ( L8 .LT. 1 ) THEN
           IFIND_SORT_PL8 = -2
           RETURN
      END IF
!
! --- Binary search
!
      IL = 1
      IR = L8
 910  CONTINUE 
         IM = IL + (IR - IL)/2
         IF ( IL .GT. IR ) THEN
              IFIND_SORT_PL8 = -1
              GOTO 810
         END IF
         IF ( NEL8 .LT. LIS8(IM) ) THEN
              IR=IM-1
              GOTO 910
            ELSE IF ( NEL8 .GT. LIS8(IM) ) THEN
              IL=IM+1
              GOTO 910
            ELSE IF ( NEL8 .EQ. LIS8(IM) ) THEN
              IFIND_SORT_PL8 = IM
              GOTO 810
         END IF               
 810  CONTINUE 
      RETURN
      END  FUNCTION  IFIND_SORT_PL8  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   IFIND_SORT_CH ( L, LIS_CH, STR )
! ************************************************************************
! *                                                                      *
! *     Finction  IFIND_SORT_CH  scans the sorted list LIS which has  L  *
! *   elements, and searches for the element STR using binary search     *
! *   algorithm. If it finds the element NEL, IFIND_SORT_SH returns its  *
! *   index, if not IFIND_SORT_CH returns -1 .                           *
! *                                                                      *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *       L ( INTEGER*4 )  --  Length of the list LIS.                   *
! *  LIS_CH ( CHARACTER )  --  The sorted list where the serach is       *
! *                            performed.                                *
! *     STR ( CHARACTER )  --  Element under consideration.              *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! *  <IFIND_SORT_CH> ( INTEGER*4 ) -- Index of STR in the list LIS_CH.   *
! *                                   If not found, then -1.             *
! *                                                                      *
! * ###  08-FEB-2008  IFIND_SORT_CH  v1.0 (c) L. Petrov 08-FEB-2008  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  L
      INTEGER*4  IFIND_SORT_CH
      CHARACTER  LIS_CH(L)*(*), STR*(*)
      INTEGER*4  IL, IR, IM
      IF ( L .LT. 1 ) THEN
           IFIND_SORT_CH = -2
           RETURN
      END IF
!
! --- Binary search
!
      IL = 1
      IR = L
 910  CONTINUE 
         IM = IL + (IR - IL)/2
         IF ( IL .GT. IR ) THEN
              IFIND_SORT_CH = -1
              GOTO 810
         END IF
         IF ( STR .LT. LIS_CH(IM) ) THEN
              IR=IM-1
              GOTO 910
            ELSE IF ( STR .GT. LIS_CH(IM) ) THEN
              IL=IM+1
              GOTO 910
            ELSE IF ( STR .EQ. LIS_CH(IM) ) THEN
              IFIND_SORT_CH = IM
              GOTO 810
         END IF               
 810  CONTINUE 
      RETURN
      END  FUNCTION  IFIND_SORT_CH !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE MER_LIS ( M, L, LIS, L_ACC, LIS_ACC, IUER )
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαννα  MER_LIS  ποπομξρετ σπισολ  LIS, σοδεςφαύικ  L      *
! *     όμενεξτοχ  ( ναλσιναμψξαρ δμιξα σπισλα  --  M όμενεξτοχ ), τενι  *
! *     όμενεξτανι σπισλα LIS_ACC ( σοδεςφαύιν L_ACC όμενεξτοχ ),        *
! *     λοτοςωε ξε χστςεώαΰτσρ χ σπισλε  LIS. εσμι I-ωκ όμενεξτ σπισλα   *
! *     LIS_ACC ξε σοδεςφιτσρ  χ σπισλε  LIS, το ότοτ όμενεξτ            *
! *     δοβαχμρετσρ χ σπισολ LIS, ι παςανετς L ιξλςενεξτιςυετσρ.         *
! *                                                                      *
! * ________________________ χθοδξωε παςανετςω: ________________________ *
! *                                                                      *
! *        M ( INTEGER*4 )  --  ναλισαναμψξαρ δμιξα σπισλα  LIS.         *
! *    L_ACC ( INTEGER*4 )  --  δμιξα σπισλα  LIS_ACC.                   *
! *  LIS_ACC ( INTEGER*4 )  --  χτοιςώξωκ σπισολ.                        *
! *                                                                      *
! * ___________________ νοδιζιγιςυενωε παςανετςω: ______________________ *
! *                                                                      *
! *        L ( INTEGER*4 )  --  δμιξα σπισλα  LIS.                       *
! *      LIS ( INTEGER*4 )  --  ποπομξρενωκ σπισολ.                       *
! *     IUER ( INTEGER*4, OPT )  -- παςανετς οϋιβλι:                     *
! *             χθοδξοε ϊξαώεξιε  --  ςεφιν οβςαβοτλι οϋιβλι:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- χοϊχςαύεξιε λοδα οϋιβλι.                             *
! *      IUER=-1 -- χοϊχςαύεξιε λοδα IUER=0 χ σμυώαε ξοςναμψξοηο         *
! *                 ϊαχεςϋεξιρ ι χωχοδ διαηξοστιώεσλοηο σοοβύεξιρ        *
! *                 χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.                       *
! *      IUER<-1 -- χοϊχςαύεξιε λοδα IUER=0 χ σμυώαε ξοςναμψξοηο         *
! *                 ϊαχεςϋεξιρ, χωχοδ διαηξοστιώεσλοηο σοοβύεξιρ ι       *
! *                 ϊαχεςϋεξιε οβςαϊα χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.     *
! *      εσμι IUER οπυύεξ, το χθοδξοε ϊξαώεξιε πςιξιναετσρ ςαχξων -1     *
! *             χωθοδξοε ϊξαώεξιε  --  λοδ οϋιβλι ( εσμι IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             δοστυπεξ δμρ ϊαπισι ):                                   *
! *      IUER=0  --  ξοςναμψξοε ϊαχεςϋεξιε.                              *
! *      IUER=1  --  σπισολ LIS σμιϋλον λοςοτλικ ι ξε νοφετ χνεστιτψ     *
! *                  χσε ξοχωε όμενεξτω.                                 *
! *      IUER=2  --  σπισολ LIS σμιϋλον λοςοτλικ ι ξε νοφετ χνεστιτψ     *
! *                  χσε ξοχωε όμενεξτω.                                 *
! *                                                                      *
! *  ###  ποδπςοηςαννυ   MER_LIS   ξαπισαμ  πετςοχ μ.ΰ.  12-MAR-92  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4 LIS(M), LIS_ACC(L_ACC)
        LOGICAL   INC
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=8 )  !  Number of formal arguments
!
! ----- Check: whether the number of formal argumenhts is equation to the
! ----- teh number of actual arguments
!
!        NA=NUM$ARG()  !  The nnumber of actual arguments
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL  VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   End of check
!C
!
        IF ( L.EQ.0 ) THEN
!
! ---------- the lsit LIS turned out to be empty
!
             IF ( M.LT.L_ACC ) THEN
                  CALL ERR_LOG ( 2, IUER, 'MER_LIS', 'M=0' )
                  RETURN
             END IF
!
! ---------- Copy the list LIS_ACC  to the list LIS
!
             L=L_ACC
             CALL LIB$MOVC3 ( L_ACC*4, LIS_ACC, LIS )
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
!
! ----- The list LIS  contains something
!
        DO 410 J1=1,L_ACC
           INC=.FALSE.
!
! -------- Scan the list LIS_ACC
!
           DO 420 J2=1,L
!
! ----------- Scan the list LIS
!
              IF ( LIS_ACC(J1).EQ.LIS(J2) ) INC=.TRUE.
  420      CONTINUE
!
           IF ( .NOT. INC ) THEN
!
! -------------- We did not find the element LIS_ACC(J1) in the list LIS
!
                 L=L+1
                 IF ( L.GT.M ) THEN
                      CALL ERR_LOG ( 1, IUER, 'MER_LIS', 'The list LIST is '// &
     &                    'overflown' )
                      RETURN
                 END IF
                 LIS(L)=LIS_ACC(J1)
            END IF
  410   CONTINUE
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  MER_LIS #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   ADD_CLIST ( M_ARR, L_ARR, C_ARR, STR, IUER )
! ************************************************************************
! *                                                                      *
! *   Function ADD_CLIST  checks whether th string STR is in the string  *
! *   list C_ARR, which contains L_ARR elements. Trailing blanks in both *
! *   list C_ARR and in examined string STR are not ignored. If the list *
! *   does not contains it ADD_CLIST adds the string STR to the end of   *
! *   the list. Finction ADD_CLIST returns  the index of the string STR  *
! *   in the list C_ARR after completion the operation.                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * M_ARR ( INTEGER*4 ) -- Maximal number of elements in the array C_ARR *
! *                                                                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * L_ARR ( INTEGER*4 ) -- Number of elements in the array C_ARR.        *
! * C_ARR ( CHARACTER ) -- Character list.                               *
! *   STR ( CHARACTER ) -- Examined string.                              *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-JUN-1999   ADD_CLIST   v1.1 (c) L. Petrov 13-JUN-2006  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  ADD_CLIST, M_ARR, L_ARR, IUER
      CHARACTER  C_ARR(M_ARR)*(*), STR*(*)
      INTEGER*4  IP, LN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( L_ARR .GT. M_ARR ) THEN
!
! -------- List is already overflown
!
           ADD_CLIST = -1
           CALL ERR_LOG ( 401, IUER, 'ADD_CLIST', 'L_ARR > M_ARR' )
           RETURN
      END IF
!
      LN = MIN( LEN(C_ARR(1)), LEN(STR) )
      IF ( LN == 0 ) THEN
!
! -------- Zero string length
!
           ADD_CLIST = -1
           CALL ERR_LOG ( 402, IUER, 'ADD_CLIST', 'LEN(STR)==0' )
           RETURN
      END IF
!
      IF ( L_ARR .LE. 0 ) THEN
!
! -------- The list is empty. Add there the first element
!
           L_ARR = 1
           CALL CLRCH ( C_ARR(L_ARR) )
           C_ARR(L_ARR) = STR
           ADD_CLIST = L_ARR
         ELSE
!
! -------- Search the string in the list C_ARR. Trailing blanks are ignored
!
           IP = LTM_DIF ( 1, L_ARR, C_ARR, STR(1:LN) )
           IF ( IP .LE. 0 ) THEN
!
! ------------- The string has not been found
!
                IF ( L_ARR .EQ. M_ARR ) THEN
                     ADD_CLIST = -1
                     CALL ERR_LOG ( 403, IUER, 'ADD_CLIST', 'M_ARR is too '// &
     &                             'small' )
                     RETURN
                  ELSE
!
! ------------------ Then add it to the end of the list
!
                     L_ARR = L_ARR + 1
                     CALL CLRCH ( C_ARR(L_ARR) )
                     C_ARR(L_ARR) = STR
                     ADD_CLIST = L_ARR
                END IF
              ELSE
!
! ------------- String has been found. Return its index
!
                ADD_CLIST = IP
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ADD_CLIST  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUB_CLIST ( IREG, L_AR1, C_AR1, L_AR2, C_AR2, M_AR3, L_AR3, &
     &                       C_AR3, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUB_CLIST  subtract character list AR2 from the character *
! *   list AR1 and computes list AR3. List AR3 contains elements which   *
! *   are in the list AR1, but not in the list AR2. Order of elements in *
! *   AR3 is the same as in AR2. List AR2 may contains wild-card symbols *
! *   which depdneing on parameter IREG may be interpreted as the        *
! *   wild-symbol elements.                                              *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    IREG ( INTEGER*4, OPT ) -- Mode switcher:                         *
! *                      IREG=0 -- (default) Trailing blanks are taken   *
! *                                into account in comparing strings.    *
! *                      IREG=1 -- the same as 0.                        *
! *                      IREG=2 -- Wild card symbols * and ? in C_AR2    *
! *                                are supported. It is assumed that no  *
! *                                line in  C_AR1 contains * or ?.       *
! *                                Trailing blanks are ignored in both   *
! *                                C_AR1 and C_AR2.                      *
! *                      IREG=3 -- Wild card symbols * and ? in C_AR1    *
! *                                are supported. It is assumed that     *
! *                                C_AR2 doesn't contains * or ?.        *
! *                                Trailing blanks are ignored in both   *
! *                                C_AR1 and C_AR2.                      *
! *                      IREG not 0, 1, 2 or 3 is interpreted as 0.      *
! *   L_AR1 ( INTEGER*4 ) -- Number of elements in array AR1             *
! *   C_AR1 ( CHARACTER ) -- Character array which contains list AR1.    *
! *                          Dimenstion: L_AR1.                          *
! *   L_AR2 ( INTEGER*4 ) -- Number of elements in array AR2             *
! *   C_AR2 ( CHARACTER ) -- Character array which contains list AR2.    *
! *                          Dimenstion: L_AR2.                          *
! *   M_AR3 ( INTEGER*4 ) -- Maximal number of elements in AR3 array.    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   L_AR3 ( INTEGER*4 ) -- Number of elements in array AR3             *
! *   C_AR3 ( CHARACTER ) -- Character array which contains list AR3.    *
! *                          Dimenstion: M_AR3. This list contains       *
! *                          elements which belong to AR1, but don't     *
! *                          belong to AR2.                              *
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
! *  ### 16-AUG-2000   SUB_CLIST   v1.0 (c)  L. Petrov  16-AUG-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IREG, L_AR1, L_AR2, M_AR3, L_AR3, IUER
      CHARACTER  C_AR1(L_AR1)*(*), C_AR2(L_AR2)*(*), C_AR3(M_AR3)*(*)
      CHARACTER  STR*80
      INTEGER*4  J1, J2, IL1, IL2, IL, IRG
      LOGICAL*4  MATCH_WILD
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      L_AR3 = 0
      IF ( M_AR3 .LT. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_AR3, STR )
           CALL ERR_LOG ( 601, IUER, 'SUB_CLIST', 'Wrong argument M_AR3' )
           RETURN
      END IF
!
      IRG = IREG
      IF ( IRG .LT. 0  .OR.  IRG .GT. 3 ) IRG = 0
!
      DO 410 J1=1,L_AR1
!
! ------ Compute IL -- the length of the strings under comparison
!
         IL1=LEN(C_AR1(1))
         DO 420 J2=1,L_AR2
            IF ( IRG.NE.1 ) IL2=ILEN(C_AR2(J2))
            IF ( IRG.EQ.1 ) IL2= LEN(C_AR2(J2))
            IL=MIN ( IL1, IL2 )
            IF ( IRG.EQ.0 .OR. IRG.EQ.1 ) THEN
!
! -------------- Exact comparison
!
                 IF ( C_AR1(J1)(1:IL) .EQ. C_AR2(J2)(1:IL) ) GOTO 410
              ELSE IF ( IRG.EQ.2 ) THEN
!
! -------------- Comparion including wild card symbols in C_AR2
!
                IF ( MATCH_WILD ( C_AR1(J1)(1:I_LEN(C_AR1(J1))), &
     &                            C_AR2(J2)(1:IL) ) ) GOTO 410
             ELSE IF ( IRG.EQ.3 ) THEN
!
! ------------- Comparion including wild card symbols in C_AR1
!
                IF ( MATCH_WILD ( C_AR2(J2)(1:IL), &
     &                            C_AR1(J1)(1:I_LEN(C_AR1(J1))) ) ) GOTO 410
           END IF
 420     CONTINUE
!
! ------ No matching was found
!
         L_AR3 = L_AR3 + 1
         IF ( L_AR3 .GT. M_AR3 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_AR3, STR )
              CALL ERR_LOG ( 602, IUER, 'SUB_CLIST', 'List AR3 is exceed. '// &
     &            'Parameter M_AR3 was too small: '//STR )
         END IF
         C_AR3(L_AR3) = C_AR1(J1)
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SUB_CLIST  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUM_CLIST ( L_AR1, C_AR1, L_AR2, C_AR2, M_AR3, L_AR3, &
     &                       C_AR3, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUM_CLIST  summs character list AR1 and the character     *
! *   list AR2. The new generated list AR3 contains all elements AR1     *
! *   and the elements of the array AR2 which are not in the array AR1   *
! *   added to the end of the list.                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   L_AR1 ( INTEGER*4 ) -- Number of elements in array AR1             *
! *   C_AR1 ( CHARACTER ) -- Character array which contains list AR1.    *
! *                          Dimenstion: L_AR1.                          *
! *   L_AR2 ( INTEGER*4 ) -- Number of elements in array AR2             *
! *   C_AR2 ( CHARACTER ) -- Character array which contains list AR2.    *
! *                          Dimenstion: L_AR2.                          *
! *   M_AR3 ( INTEGER*4 ) -- Maximal number of elements in AR3 array.    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   L_AR3 ( INTEGER*4 ) -- Number of elements in array AR3             *
! *   C_AR3 ( CHARACTER ) -- Character array which contains list AR3.    *
! *                          Dimenstion: M_AR3. This list contains       *
! *                          elements which belong to AR1 and AR2        *
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
! *  ### 13-MAR-2002   SUM_CLIST   v2.0 (c)  L. Petrov  01-NOV-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  L_AR1, L_AR2, M_AR3, L_AR3, IUER
      CHARACTER  C_AR1(L_AR1)*(*), C_AR2(L_AR2)*(*), C_AR3(M_AR3)*(*)
      CHARACTER  STR*80
      INTEGER*4  J1, J2, J3
!
      L_AR3 = 0
      IF ( M_AR3 .LT. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_AR3, STR )
           CALL ERR_LOG ( 621, IUER, 'SUM_CLIST', 'Wrong argument M_AR3: '//STR)
           RETURN
      END IF
!
      IF ( M_AR3 .LT. L_AR1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_AR3, STR )
           CALL ERR_LOG ( 622, IUER, 'SUM_CLIST', 'Too small argument '// &
     &                   'M_AR3: '//STR )
           RETURN
      END IF
!
      L_AR3 = L_AR1
      DO 410 J1=1,L_AR3
         CALL CLRCH ( C_AR3(J1) )
         C_AR3(J1) = C_AR1(J1)
 410  CONTINUE
!
      DO 420 J2=1,L_AR2
         DO 430 J3=1,L_AR1
            IF ( C_AR2(J2) .EQ. C_AR3(J3) ) GOTO 420
 430     CONTINUE
         L_AR3 = L_AR3 + 1
         IF ( L_AR3 .GT. M_AR3 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_AR3, STR )
              CALL ERR_LOG ( 623, IUER, 'SUM_CLIST', 'Too small argument'// &
     &                      'M_AR3: '//STR )
              RETURN
         END IF
         C_AR3(L_AR3) = C_AR2(J2)
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SUM_CLIST  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION ADC_LIS ( M, L, LIS, KV, IPAR, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  ADC_LIS  scans the integetr list and searches there     *
! *   the element IPAR. If it find IPAR there as the IP-th element       *
! *   of the list LIS, then it increments KV(IP). If IPAR is not the     *
! *   element of the list LIS, then it adds IPAR to the end of the       *
! *   list LIS:                                                          *
! *        L := L+1                                                      *
! *        LIS(L) := IPAR                                                *
! *        KV(L)  := 1                                                   *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *        M ( INTEGER*4 )  --  maximal length of the list LIS.          *
! *     IPAR ( INTEGER*4 )  --  the element under investigation.         *
! *                                                                      *
! * _________________________ Modified paramters: ______________________ *
! *                                                                      *
! *        L ( INTEGER*4 )  --  The number of elements in the list LIS.  *
! *      LIS ( INTEGER*4 )  --  The list under investigation.            *
! *                             Dimension: M.                            *
! *       KV ( INTEGER*4 )  --  Array of the length M associated with    *
! *                             the list LIS. Meaning of KV is           *
! *                             a multiplicity of every element.         *
! *                             If the element LIS(IP) was included in   *
! *                             the list N times, only one replic of the *
! *                             element LIS(IP) will be in the list, but *
! *                             KV(IP) = N.                              *
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
! *  ### 12-MAR-1992    ADC_LIS    v3.1 (c) L. Petrov  14-MAY-1998  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4  L, M
        INTEGER*4  ADC_LIS, LIS(M), KV(M), I_LEN
        CHARACTER  STR*20, STR1*20
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=6 )  !  Number of formal arguments
!
! ----- Check: whether the number of formal argumenhts is equation to the
! ----- teh number of actual arguments
!
!        NA=NUM$ARG()  !  The nnumber of actual arguments
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL  VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   End of check
!C
!
        IF ( L .GT. M ) THEN
             CALL CLRCH (    STR  )
             CALL INCH  ( L, STR  )
             CALL CLRCH (    STR1 )
             CALL INCH  ( M, STR1 )
             CALL ERR_LOG ( 1, IUER, 'ADC_LIS', 'List overflow: specified '// &
     &           'length of the list ('//STR(1:I_LEN(STR))// &
     &           ') appeared larger than max length ('//STR1(1:I_LEN(STR1))// &
     &           ') ' )
             RETURN
        END IF
!
        IF ( L.EQ.0 ) THEN
             L=1
          ELSE
             DO 410 J1=1,L
!
! ------------- Scan the list
!
                IF ( LIS(J1).EQ.IPAR ) THEN
                     KV(J1)=KV(J1)+1
                     ADC_LIS = J1
                     CALL ERR_LOG ( 0, IUER )
                     RETURN
                END IF
  410        CONTINUE
             L=L+1
        END IF
        IF ( L.GT.M ) THEN
             ADC_LIS = -1
             CALL CLRCH ( STR )
             CALL INCH  ( M, STR )
             CALL ERR_LOG ( 2, IUER, 'ADC_LIS', 'List overflow. Length of '// &
     &           'the list ('//STR(1:I_LEN(STR))//' appeared too short' )
             RETURN
        END IF
        ADC_LIS = L
        LIS(L)=IPAR
        KV(L)=1
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  ADC_LIS  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE MERC_LIS ( M, L, LIS, KV, L_ACC, LIS_ACC, KV_ACC, IUER )
! ************************************************************************
! *                                                                      *
! *     Finction  MERC_LIS  merges the integer list LIS which contains   *
! *   contains L elements with  the list LIS_ACC, which contains L_ACC   *
! *   elements. The elements of the list LIS_ACC which are not in the    *
! *   list LIS are added to the end of the list LIS. Each list has       *
! *   the associated array KV and KV_ACC which keeps multiplicity of     *
! *   each element. If the element LIS(IP) was added N times, then only  *
! *   one replic of the element is kept in the list, but KV(IP) = N.     *
! *   For each element which is in both lists, LIS and LIS_ACC, the      *
! *   multiplicity factor KV(IP) is updated as                           *
! *   KV(IP):= KV(IP) + KV_ACC(IP_ACC)                                   *
! *                                                                      *
! * ________________________ Input paramters ___________________________ *
! *                                                                      *
! *        M ( INTEGER*4 )  --  maximal length of the list LIS.          *
! *    L_ACC ( INTEGER*4 )  --  The number of elements in the list       *
! *                             LIS_ACC.                                 *
! *  LIS_ACC ( INTEGER*4 )  --  The secondary list.                      *
! *   KV_ACC ( INTEGER*4 )  --  Array of the length M associated with    *
! *                             the list LIS_ACC. Meaning of KV_ACC is   *
! *                             a multiplicity of every element.         *
! *                             If the element LIS_ACC(IP_ACC) was       *
! *                             included in the list N times, only one   *
! *                             replic of the element LIS_ACC(IP_ACC)    *
! *                             will be in the list, but                 *
! *                             KV_ACC(IP_ACC) = N.                      *
! *                                                                      *
! * ___________________ νοδιζιγιςυενωε παςανετςω: ______________________ *
! *                                                                      *
! *        L ( INTEGER*4 )  --  The number of elements in the list LIS.  *
! *      LIS ( INTEGER*4 )  --  The list under investigation.            *
! *                             Dimension: M.                            *
! *       KV ( INTEGER*4 )  --  Array of the length M associated with    *
! *                             the list LIS. Meaning of KV is           *
! *                             a multiplicity of every element.         *
! *                             If the element LIS(IP) was included in   *
! *                             the list N times, only one replic of the *
! *                             element LIS(IP) will be in the list, but *
! *                             KV(IP) = N.                              *
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
! *  ### 12-MAR-1992    MERC_LIS   v1.1 (c) L. Petrov  14-MAY-1998  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4 LIS(M), KV(M), LIS_ACC(L_ACC), KV_ACC(L_ACC)
        LOGICAL   INC
!
! .....................\\\
!                       \\\
!        LOGICAL PRESENT
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=8 )  !  Number of formal arguments
!
! ----- Check: whether the number of formal argumenhts is equation to the
! ----- teh number of actual arguments
!
!        NA=NUM$ARG()  !  The nnumber of actual arguments
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL  VER$ARG ( N_ARG )
!C                        ///
!C ......................///  ...   End of check
!C
!
        IF ( L.EQ.0 ) THEN
!
! ---------- We learned the the list LIS_ACC is empty
!
             IF ( M .LT. L_ACC ) THEN
                  CALL ERR_LOG ( 2, IUER, 'MERC_LIS', 'M=0' )
                  RETURN
             END IF
!
! ---------- Copy LIS_ACC to the list LIS
!
             L=L_ACC
             CALL LIB$MOVC3 ( L_ACC*4, LIS_ACC, LIS )
             CALL LIB$MOVC3 ( L_ACC*4, KV_ACC,  KV  )
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
!
! ----- Well, the list LIS is not empty
!
        DO 410 J1=1,L_ACC
           INC=.FALSE.
!
! -------- Scanning the list LIS_ACC
!
           DO 420 J2=1,L
!
! ----------- Scanning the list LIS
!
              IF ( LIS_ACC(J1).EQ.LIS(J2) ) THEN
!
! ---------------- Increment the value of the KV array
!
                   KV(J2)=KV(J2)+KV_ACC(J1)
                   INC=.TRUE.  !  We found LIS_ACC(J1) in the list LIS
              END IF
  420      CONTINUE
!
           IF ( .NOT. INC ) THEN
!
! -------------- Mmmm. Element LIS_ACC(J1) was not found in the list LIS
!
                 L=L+1
                 IF ( L.GT.M ) THEN
                      CALL ERR_LOG ( 1, IUER, 'MERC_LIS', 'The list is '// &
     &                    'in overflow. Paramter M is too small' )
                      RETURN
                 END IF
!
! -------------- ... then add to the end of the list
!
                 LIS(L)=LIS_ACC(J1)
                 KV(L)=KV_ACC(J1)
            END IF
  410   CONTINUE
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  MERC_LIS #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   MAX_LIST_I4 ( L, ARR )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine MAX_LIST_I4  returns the index of the maximal   *
! *   element of the array ARR of dimension L. If the array has several  *
! *   equal elements which are maxumim, the index of the first such      *
! *   element is returned.                                               *
! *                                                                      *
! *  ### 11-OCT-2005  MAX_LIST_I4  v1.0 (c)  L. Petrov  11-OCT-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MAX_LIST_I4 
      INTEGER*4  L, ARR(L)
      INTEGER*4  J1, MAX_VAL 
!
      MAX_LIST_I4 = 1
      MAX_VAL = ARR(1)
      IF ( L > 1 ) THEN
           DO 410 J1=2,L
              IF ( ARR(J1) > MAX_VAL ) THEN
                   MAX_VAL = ARR(J1)
                   MAX_LIST_I4 = J1
              END IF
 410       CONTINUE 
      END IF
      RETURN
      END  FUNCTION  MAX_LIST_I4 !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   MIN_LIST_I4 ( L, ARR )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine MIN_LIST_I4  returns the index of the maximal   *
! *   element of the array ARR of dimension L. If the array has several  *
! *   equal elements which are maxumim, the index of the first such      *
! *   element is returned.                                               *
! *                                                                      *
! *  ### 11-OCT-2005  MIN_LIST_I4  v1.0 (c)  L. Petrov  11-OCT-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MIN_LIST_I4 
      INTEGER*4  L, ARR(L)
      INTEGER*4  J1, MIN_VAL 
!
      MIN_LIST_I4 = 1
      MIN_VAL = ARR(1)
      IF ( L > 1 ) THEN
           DO 410 J1=2,L
              IF ( ARR(J1) < MIN_VAL ) THEN
                   MIN_VAL =ARR(J1)
                   MIN_LIST_I4 = J1
              END IF
 410       CONTINUE 
      END IF
      RETURN
      END  FUNCTION  MIN_LIST_I4 !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   MAX_LIST_R8 ( L, ARR )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine MAX_LIST_R8  returns the index of the maximal   *
! *   element of the array ARR of dimension L. If the array has several  *
! *   equal elements which are maxumim, the index of the first such      *
! *   element is returned.                                               *
! *                                                                      *
! *  ### 11-OCT-2005  MAX_LIST_R8  v1.0 (c)  L. Petrov  11-OCT-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MAX_LIST_R8 
      INTEGER*4  L
      REAL*8     ARR(L)
      INTEGER*4  J1
      REAL*8     MAX_VAL 
!
      MAX_LIST_R8 = 1
      MAX_VAL = ARR(1)
      IF ( L > 1 ) THEN
           DO 410 J1=2,L
              IF ( ARR(J1) > MAX_VAL ) THEN
                   MAX_VAL =ARR(J1)
                   MAX_LIST_R8 = J1
              END IF
 410       CONTINUE 
      END IF
      RETURN
      END  FUNCTION  MAX_LIST_R8 !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   MIN_LIST_R8 ( L, ARR )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine MIN_LIST_R8  returns the index of the maximal   *
! *   element of the array ARR of dimension L. If the array has several  *
! *   equal elements which are maxumim, the index of the first such      *
! *   element is returned.                                               *
! *                                                                      *
! *  ### 11-OCT-2005  MIN_LIST_R8  v1.0 (c)  L. Petrov  11-OCT-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MIN_LIST_R8 
      INTEGER*4  L
      REAL*8     ARR(L)
      INTEGER*4  J1
      REAL*8     MIN_VAL 
!
      MIN_LIST_R8 = 1
      MIN_VAL = ARR(1)
      IF ( L > 1 ) THEN
           DO 410 J1=2,L
              IF ( ARR(J1) < MIN_VAL ) THEN
                   MIN_VAL =ARR(J1)
                   MIN_LIST_R8 = J1
              END IF
 410       CONTINUE 
      END IF
      RETURN
      END  FUNCTION  MIN_LIST_R8 !#!  
