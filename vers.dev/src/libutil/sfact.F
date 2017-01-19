* downloaded by GF from site http://ftp.unina.it:21006/napack/sfact.f
* Note: changes are in lower case
C      ________________________________________________________
C     |                                                        |
C     |       FACTOR A SYMMETRIC MATRIX WITHOUT PIVOTING       |
C     |                                                        |
C     |    INPUT:                                              |
C     |                                                        |
C     |        A     --ARRAY PACKED WITH ELEMENTS CONTAINED IN |
C     |                EACH ROW, ON DIAGONAL AND TO RIGHT, OF  |
C     |                COEFFICIENT MATRIX                      |
C     |                (LENGTH AT LEAST 3 + N(N+1)/2)          |
C     |                                                        |
C     |        N     --MATRIX DIMENSION                        |
C     |                                                        |
C     |        W     --WORK ARRAY WITH AT LEAST N ELEMENTS     |
C     |                                                        |
C     |    OUTPUT:                                             |
C     |                                                        |
C     |        A     --FACTORED MATRIX                         |
C     |                                                        |
C     |    BUILTIN FUNCTIONS: ABS                              |
C     |________________________________________________________|
C
      SUBROUTINE SFACT(A,N,W)
      implicit none
      REAL*8 A(1),W(1),R,S,T
      INTEGER G,H,I,J,K,L,M,N
C     ------------------------
C     |*** COMPUTE 1-NORM ***|
C     ------------------------
      DO 10 I = 1,N
10         W(I) = 0.
      I = -N
      K = 0
      R = 0.
      S = 0.
20    I = I + N - K
      K = K + 1
      J = K
      S = ABS(A(I+J))
30    IF ( J .EQ. N ) GOTO 40
      J = J + 1
      T = ABS(A(I+J))
      S = S + T
      W(J) = W(J) + T
      GOTO 30
40    S = S + W(K)
      IF ( R .LT. S ) R = S
      IF ( K .LT. N ) GOTO 20
      J = 3 + (N+N*N)/2
C     -----------------------------------
C     |*** SHIFT MATRIX DOWN 3 SLOTS ***|
C     -----------------------------------
50    A(J) = A(J-3)
      J = J - 1
      IF ( J .GT. 3 ) GOTO 50
      A(1) = 1233
      A(2) = N
      A(3) = R
      H = N
      K = 4
60    IF ( H .EQ. 1 ) GOTO 90
C     --------------------------
C     |*** SAVE PIVOT ENTRY ***|
C     --------------------------
      S = A(K)
      K = K + H
      G = K
      H = H - 1
      M = H
      IF ( S .EQ. 0. ) GOTO 100
      J = 0
70    J = J - M
      M = M - 1
      L = G + M
      T = A(G+J)/S
C     ---------------------------
C     |*** ELIMINATE BY ROWS ***|
C     ---------------------------
      DO 80 I = G,L
80         A(I) = A(I) - T*A(I+J)
      G = L + 1
      IF ( M .GT. 0 ) GOTO 70
      GOTO 60
90    IF ( A(K) .NE. 0. ) RETURN
      A(1) = -1233
      RETURN
100   A(1) = -1233
      GOTO 60
      END
