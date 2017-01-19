* downloaded by GF from site http://ftp.unina.it:21006/napack/ssolve.f
* Note: changes are in lower case
C      ________________________________________________________
C     |                                                        |
C     |   SOLVE A FACTORED SYMMETRIC SYSTEM WITHOUT PIVOTING   |
C     |                                                        |
C     |    INPUT:                                              |
C     |                                                        |
C     |         A     --SFACT'S OUTPUT                         |
C     |                                                        |
C     |         B     --RIGHT SIDE                             |
C     |                                                        |
C     |    OUTPUT:                                             |
C     |                                                        |
C     |         X     --SOLUTION (CAN BE IDENTIFIED WITH B     |
C     |                 ALTHOUGH THE RIGHT SIDE IS DESTROYED)  |
C     |                                                        |
C     |    BUILTIN FUNCTIONS: ABS                              |
C     |________________________________________________________|
C
      SUBROUTINE SSOLVE(X,A,B)
      implicit none
      REAL*8 A(1),B(1),X(1),T
      INTEGER I,J,K,L,N
      T = A(1)
      IF ( ABS(T) .EQ. 1233 ) GOTO 10
      WRITE(6,*) 'ERROR: MUST FACTOR WITH SFACT BEFORE SOLVING'
      STOP
10    N = A(2)
      L = 3
      K = 1
      IF ( T .LT. 0. ) GOTO 80
      DO 20 I = 1,N
20         X(I) = B(I)
C     -----------------------------
C     |*** FORWARD ELIMINATION ***|
C     -----------------------------
30    IF ( K .EQ. N ) GOTO 50
      T = X(K)/A(K+L)
      J = L
      L = L + N - K
      K = K + 1
      IF ( T .EQ. 0. ) GOTO 30
      DO 40 I = K,N
40         X(I) = X(I) - T*A(I+J)
      GOTO 30
C     -----------------------------------
C     |*** BACK SUBSTITUTION BY ROWS ***|
C     -----------------------------------
50    X(N) = X(N)/A(K+L)
60    IF ( K .EQ. 1 ) RETURN
      J = K
      K = K - 1
      L = L + K - N
      T = X(K)
      DO 70 I = J,N
70         T = T - X(I)*A(I+L)
      X(K) = T/A(K+L)
      GOTO 60
C     -----------------------------
C     |*** COMPUTE NULL VECTOR ***|
C     -----------------------------
80    IF ( A(K+L) .EQ. 0. ) GOTO 90
      L = L + N - K
      K = K + 1
      GOTO 80
90    DO 100 I = 1,N
100        X(I) = 0.
      X(K) = 1.
      GOTO 60
      END
