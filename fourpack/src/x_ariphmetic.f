!
! ======================================
!
! --- Internal procedures that implement
! --- arithmetic with X-numbers
! 
! ======================================
!
          SUBROUTINE FP_ZERO_X ( X )
!
! ------- Zeroing an X-number
!
          REAL*8     F
          TYPE ( X__TYPE ) :: X
          X%F = 0.0D0
          X%E = 0
          END SUBROUTINE FP_ZERO_X
!
! =====================================
!
          SUBROUTINE FP_F_TO_X ( F, X )
!
! ------- Transform an F-number to an X-number
!
          REAL*8     F
          TYPE ( X__TYPE ) :: X
          X%F = F
          X%E = 0
          END SUBROUTINE FP_F_TO_X
!
! ================================
!
          FUNCTION FP_X_TO_F ( X )
!
! ------- Transform an X-number to an F-number
!
          REAL*8   FP_X_TO_F
          TYPE ( X__TYPE ) :: X
          IF ( X%E == 0 ) THEN
               FP_X_TO_F = X%F
            ELSE IF ( X%E < 0 ) THEN
               FP_X_TO_F = X%F*BIGU_FF
            ELSE
               FP_X_TO_F = X%F*BIGO_FF
          END IF
          END FUNCTION FP_X_TO_F
!
! ============================================
!
          SUBROUTINE FP_AYBZ ( A, Y, B, Z, X )
!
! ------- X := A*Y + B*Z
! ------- where A and B are F-numbers and 
! -------       Y and Z and X are X-numbers
!
          REAL*8     A, B, W
          TYPE ( X__TYPE ) :: Y, Z, X
          INTEGER*4   ID
          ID = Y%E - Z%E
          IF ( ID == 0 ) THEN
               X%F = A*Y%F + B*Z%F
               X%E = Y%E
            ELSE IF ( ID ==  1 ) THEN
               X%F = A*Y%F + B*(Z%F*BIGU_FF)
               X%E = Y%E
            ELSE IF ( ID == -1 ) THEN
               X%F = A*(Y%F*BIGU_FF) + B*Z%F
               X%E = Z%E
            ELSE IF ( ID  >  1 ) THEN
               X%F = A*Y%F
               X%E = Y%E
            ELSE
               X%F = B*Z%F
               X%E = Z%E
          END IF
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE FP_AYBZ
!
! =======================================
!
          SUBROUTINE FP_YZ ( Y, Z, X, F )
!
! ------- X := Y*Z
! ------- where X, Y and Z are X numbers
! ------- F is the F-number that corresponds to X
!
          REAL*8     F, W
          TYPE ( X__TYPE ) :: Y, Z, X
          INTEGER*4   ID
!
          X%F = Y%F * Z%F
          X%E = Y%E + Z%E
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          IF ( X%E == 0 ) THEN
               F = X%F
            ELSE IF ( X%E < 0 ) THEN
               F = 0.0D0
            ELSE
               F = X%F*BIGO_FF
          END IF
          END SUBROUTINE FP_YZ
!
! =====================================
!
          SUBROUTINE FP_XPA ( X, A, Y )
!
! ------- X := A*Y
! -------      where A is an F-number
! -------            Y and X are X-numbers
!
          REAL*8     A, W
          TYPE ( X__TYPE ) :: X, Y
          INTEGER*4  ID
          X%F = A*Y%F
          X%E = Y%E
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE FP_XPA
!
! ==================================
!
          SUBROUTINE FP_XFF ( X, F )
!
! ------- X := X + F*F
! -------      where X is an X-number
! -------            F is an F-number
!
!
          REAL*8     W, F
          TYPE ( X__TYPE ) :: X, R
          INTEGER*4  ID
          R%F = F * F
          R%E = 0
          W = ABS(R%F)
          IF ( W .GE. BIGHO_FF ) THEN
               R%F = R%F*BIGU_FF
               R%E = R%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               R%F = R%F*BIGO_FF
               R%E = R%E - 1
          END IF
!
          ID = X%E - R%E
          IF ( ID == 0 ) THEN
               X%F = X%F + R%F
               X%E = X%E
            ELSE IF ( ID ==  1 ) THEN
               X%F = X%F + R%F*BIGU_FF
               X%E = X%E
            ELSE IF ( ID == -1 ) THEN
               X%F = X%F*BIGU_FF + R%F
               X%E = R%E
            ELSE IF ( ID  >  1 ) THEN
               CONTINUE 
            ELSE
               X%F = R%F
               X%E = R%E
          END IF
          END SUBROUTINE FP_XFF
!
! ==================================
!
          SUBROUTINE FP_XZZ ( X, Z )
!
! ------- X := X + Z*Z
! -------      where X and Z are X-numbers
!
          REAL*8     W, F
          TYPE ( X__TYPE ) :: X, Z, R
          INTEGER*4  ID
          R%F = Z%F * Z%F
          R%E = 2*Z%E
          W = ABS(R%F)
          IF ( W .GE. BIGHO_FF ) THEN
               R%F = R%F*BIGU_FF
               R%E = R%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               R%F = R%F*BIGO_FF
               R%E = R%E - 1
          END IF
!
          ID = X%E - R%E
          IF ( ID == 0 ) THEN
               X%F = X%F + R%F
               X%E = X%E
            ELSE IF ( ID ==  1 ) THEN
               X%F = X%F + R%F*BIGU_FF
               X%E = X%E
            ELSE IF ( ID == -1 ) THEN
               X%F = X%F*BIGU_FF + R%F
               X%E = R%E
            ELSE IF ( ID  >  1 ) THEN
               CONTINUE 
            ELSE
               X%F = R%F
               X%E = R%E
          END IF
          END SUBROUTINE FP_XZZ
!
! =====================================
!
          SUBROUTINE FP_AX ( A, X )
!
! ------- X := A*X
! -------      where X is an X-number
! -------            A is an F-number
!
          REAL*8     A, W
          TYPE ( X__TYPE ) :: X
          X%F = A*X%F
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE FP_AX
!
! ==================================
!
          SUBROUTINE FP_XAZ ( X, A, Z )
!
! ------- Z := X + A*Z
! -------      where X and Z are X-numbers
! -------            A is an F-number
!
          REAL*8     A, W
          TYPE ( X__TYPE ) :: X, Z
          INTEGER*4   ID
          ID = X%E - Z%E
          IF ( ID == 0 ) THEN
               X%F = X%F + A*Z%F
               X%E = X%E
            ELSE IF ( ID ==  1 ) THEN
               X%F = X%F + A*(Z%F*BIGU_FF)
               X%E = X%E
            ELSE IF ( ID == -1 ) THEN
               X%F = X%F*BIGU_FF + A*Z%F
               X%E = Z%E
            ELSE IF ( ID  >  1 ) THEN
               CONTINUE 
            ELSE
               X%F = A*Z%F
               X%E = Z%E
          END IF
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE FP_XAZ
!
! ================================
!
          SUBROUTINE FP_NRML ( X )
!
! ------- Normalization of the X number
!
          TYPE ( X__TYPE ) :: X
          REAL*8     W
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE FP_NRML
