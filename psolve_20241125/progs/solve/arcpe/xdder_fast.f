      SUBROUTINE XDDER_FAST ( L, G, ARR_GLO1, ARR_GLO2, A_GG, B_G )
! ************************************************************************
! *                                                                      *
! *   Routine  XDDER_FAST  reads form file CFGM. Then it rearranges      *
! *   contribution of global-global normal matrix with eliminated        *
! *   influence of local (and segmented) parameters and normal vector of *
! *   this arc to CGM in order of CGM and adds it to CGM. Then it writes *
! *   down updated CGM.                                                  *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      L ( INTEGER*4  ) -- the number of local and segmented parameters*
! *      G ( INTEGER*4  ) -- the number of global parameters.            *
! *   A_GG ( REAL*8     ) -- contribution of global-global normal matrix *
! *                          with eliminated influence of local          *
! *                          (and segmented) parameters of the arc.      *
! *   A_GG ( REAL*8     ) -- contribution of global-global normal vector *
! *                          with eliminated influence of local          *
! *                          (and segmented) parameters of the arc.      *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! * ARR_GLO1 ( REAL*8     ) -- Working array for keeping CGM.            *
! * ARR_GLO2 ( REAL*8     ) -- Working array for keeping CGM.            *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! * B3DOBJ ( RECORD     ) -- Object with data structure for B3D          *
! *                            extension of SOLVE.                       *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *          Input: swicth IUER=0 -- no error messages will be           *
! *                                  generated even in the case of error.*
! *                        IUER=-1 - in the case of error the message    *
! *                                  will pe put on stdout.              *
! *          Default input value = -1                                    *
! *          Output: 0 in the case of successful completion and error    *
! *                    code in the case of error.                        *
! *                                                                      *
! *  ###  12-FEB-97   XDDDER_FAST   v2.1  (c)  L. Petrov  06-APR-99  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
!
      INCLUDE 'solve.i'
      INCLUDE 'precm.i'
      INCLUDE 'addcm.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'plist.i'
      INCLUDE 'glbc4.i'
!
      CHARACTER CNAME*(NAME_SIZE)
!
      INTEGER*4 L, G, JA, JB, FILDES
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      REAL*8    ARR_GLO1(*), ARR_GLO2(*), A_GG(*), B_G(*)
      COMMON  / SAVCGM / FILDES, IDIRECT
      INTEGER*4  J1, J2, POS_CGM1, POS_CGM2
      INTEGER*8  IND_CGM, IND_GG
      INTEGER*8, EXTERNAL :: INDX8
!
!   MWH  920113  Expanded to handle 1536 parameters (enlarge buffer read
!                 from ADDER)
!   mwh  940201  Implement dynamic memory allocation for large matrices
!   jmg  960610  Speed up.  (Read cgm directly into final array if there are
!                no new global parameters for the current arc.)
!   jmg  961126  Get rid of unused varialbes.
!   jmg          Cleaned up
!   pet  960218  Rewrote it and adapted for the case when XDDER_B1D
!                1) called from ARCPE 2) B1D order of wirting elements in
!                   matrices
!CCCCC
      JB= 1 + 2*M_GPA
      JA= 1 + 3*M_GPA
!
! --- NPARMC -- the number of lobal parameters collected in CGM before
! ---           treating this arc
! --- NPARMT -- total number of parameters in this arc
! --- NPARMF -- number of global parameters in CGM after treating this arc
! --- L      -- number of local  parameters in this arc
! --- G      -- number of global parameters in this arc
!
      IF ( CGMINN(1:1) .NE. ' ' ) THEN
!
! -------- Fill ARR_GLO1 with normal equations & residuals from the CGM
!
           CNAME=CGMINN
!
! -------- Openning CGM file of name CNAME
!
           CALL ACS_CGMFIL ( CNAME, 'O' )
           IF ( NPARMF .EQ. NPARMC ) THEN
!
! ------------- Put array with combined global matrix and vector in ARR_GLO1
! ------------- since no reordering is coming
!
                CALL USE_CGMF_MAT ( ARR_GLO1, NPARMC, 'R' )
             ELSE
!
! ------------- Put array with combined global matrix and vector in temporary
! ------------- array ARR_GLO2 for consecutive reordering
!
                CALL USE_CGMF_MAT ( ARR_GLO2, NPARMC, 'R' )
!
! ------------- Changing order of the elements in combined global matrix and
! ------------- combined global vector
!
                CALL REFORMAT ( ARR_GLO1(JA), ARR_GLO2(JA), ARR_GLO1(JB), &
     &                          ARR_GLO2(JB), IXCTF, NPARMC )
           END IF
!
! -------- Closing file with CGM
!
           CALL ACS_CGMFIL ( CNAME, 'C' )
      ENDIF
!
! --- Updating global-global normal matrix and normal vector
! --- A_GG     contains global-global matrix for current arc where influence
! ---          of local parameters has been eliminated
! --- ARR_GLO1 contains accumulated combined global global-global matrix for
! ---          the current arc where influence of local parameters has been
! ---          eliminated
!
      DO 410 J1=1,NPARMT
         POS_CGM1 = IXTTF(J1)
         IF ( POS_CGM1 .NE. 0 ) THEN
!
! --------- Update approporiate element of combined global vector
!
            ARR_GLO1(JB-1 +POS_CGM1) = ARR_GLO1(JB-1 +POS_CGM1) + B_G(J1-L)
            DO 420 J2=1,J1
               POS_CGM2 = IXTTF(J2)
               IF ( POS_CGM2 .NE. 0 ) THEN
                  IND_CGM = INDX8 ( POS_CGM1, POS_CGM2 )
                  IND_GG  = INDX8 ( J1-L, J2-L )
!
! --------------- Update approporiate element of combined global matrix
!
                  ARR_GLO1(JA-1 +IND_CGM) = ARR_GLO1(JA-1 +IND_CGM) + &
     &                                      A_GG(IND_GG)
               END IF
 420        CONTINUE
         END IF
 410  CONTINUE
!
! --- Write the result out in CGM file CNAME
!
      CNAME =  IONAM
      CALL ACS_CGMFIL   ( CNAME, 'O' )
      CALL USE_CGMF_MAT ( ARR_GLO1, NPARMF, 'W' )
      CALL ACS_CGMFIL   ( CNAME, 'C' )
!
      RETURN
      END  !#!  XDDER_FAST  #!#
