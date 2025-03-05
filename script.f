      SUBROUTINE script (OAP,M,N,MD,ND)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION OAP(MD,ND)
      nnn=10
      NN=N/nnn
      IF(NN.EQ.0) GOTO 85
      DO 83 KK=1,NN
      N1=nnn*(KK-1)+1
      N2=nnn*KK
      IF(N2.LT.N1) RETURN
      print*,' '
      WRITE(6,1024) (I,I=N1,N2)
      print*,' '
      DO 83 JJ=1,M
83    WRITE(6,1025) JJ,(OAP(JJ,II),II=N1,N2)
      print*,' '
85    N1=nnn*NN+1
      N2=N
      IF(N2.LT.N1) RETURN
      print*,' '
      WRITE(6,1024) (I,I=N1,N2)
      print*,' '
      DO 84 JJ=1,M
84    WRITE(6,1025) JJ,(OAP(JJ,II),II=N1,N2)
      print*,' '
1024  FORMAT(1X,10(3X,I5))
1025  FORMAT(I5,10F8.4)
      RETURN
      END
       subroutine scri1(nsym,r8,isym)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension isym(8),r8(*)
      idep=1
      do k=1,nsym
         call scri8(r8(idep),isym(k))
         idep=idep+isym(k)*(isym(k)+1)/2
      enddo
      return
      end
       subroutine scri8(r8,n)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension r8(*)
      ifin=0
      do k=1,n
      ideb=ifin+1
      ifin=ideb+k-1
      write(6,'(10f8.4)')(r8(kk),kk=ideb,ifin)
      enddo
      return
      end
       SUBROUTINE scripi (mat,M,N,MD,ND,iform)
      IMPLICIT REAL*8(A-H,O-Z)
      character*20 f1025,f1024
      DIMENSION mat(MD,ND)
      nnn=80/iform
      if(iform.lt.10) then
         write(f1024,'(''(4X,'',i2,''i'',i1,'')'')') nnn,iform
         write(f1025,'(''(I4,'',i2,''i'',i1,'')'')') nnn,iform
      else
         write(f1024,'(''(4X,'',i2,''i'',i2,'')'')') nnn,iform
         write(f1025,'(''(I4,'',i2,''i'',i2,'')'')') nnn,iform
      endif
      print*,'f1024',f1024
      print*,'f1025',f1025
c      f1024='(3X,10I8)'
c      f1025='(i4,10I8)'
c      nnn=10
      NN=N/nnn
      IF(NN.EQ.0) GOTO 85
      DO 83 KK=1,NN
      N1=nnn*(KK-1)+1
      N2=nnn*KK
      IF(N2.LT.N1) RETURN
c      WRITE(6,1023)
      WRITE(6,f1024) (I,I=N1,N2)
c      WRITE(6,1022)
      DO 83 JJ=1,M
83    WRITE(6,f1025) JJ,(mat(JJ,II),II=N1,N2)
c      WRITE(6,1022)
85    N1=nnn*NN+1
      N2=N
      IF(N2.LT.N1) RETURN
      WRITE(6,f1024) (I,I=N1,N2)
      DO 84 JJ=1,M
84    WRITE(6,f1025) JJ,(mat(JJ,II),II=N1,N2)
c      WRITE(6,1022)
c1022  FORMAT(/)
c1023  FORMAT(/)
1024  FORMAT(4X,10(6X,I2))
1025  FORMAT(3X,I2,10i8)
      RETURN
      END 
      SUBROUTINE script6 (OAP,M,N,MD,ND)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION OAP(MD,ND)
      nnn=10
      NN=N/nnn
      IF(NN.EQ.0) GOTO 85
      DO 83 KK=1,NN
      N1=nnn*(KK-1)+1
      N2=nnn*KK
      IF(N2.LT.N1) RETURN
      print*,' '
      WRITE(6,1024) (I,I=N1,N2)
      print*,' '
      DO 83 JJ=1,M
83    WRITE(6,1025) JJ,(OAP(JJ,II),II=N1,N2)
      print*,' '
85    N1=nnn*NN+1
      N2=N
      IF(N2.LT.N1) RETURN
      print*,' '
      WRITE(6,1024) (I,I=N1,N2)
      print*,' '
      DO 84 JJ=1,M
84    WRITE(6,1025) JJ,(OAP(JJ,II),II=N1,N2)
      print*,' '
1024  FORMAT(1X,10(3X,I7))
1025  FORMAT(I5,10F10.6)
      RETURN
      END
       subroutine scritri(h,n)
      IMPLICIT REAL*8(A-H,O-Z)
      dimension h(n*(n+1)/2)
      nn=n*(n+1)/2
      ifin=0
       print*,'n',n
      do m=1,n,10
         print*,' '
         print'(4x,10i8)',(k,k=m,m+10-1)
         print*,' '
         ideb=ifin+1
         ifin=ifin+10
         jfin=(m+1)*m/2-1
         do i=m,min(m+10-1,n)
            jdeb=jfin+1
            jfin=jfin+i-m+1
            print'(i4,10f8.4)',i,(h(k),k=jdeb,jfin)
            jfin=i*(i+1)/2+m-1
         enddo
         do i=m+10,n
            jdeb=jfin+1
            jfin=min(jfin+10,nn)
            !print*,'i',i,jdeb,jfin
            print'(i4,10f8.4)',i,(h(k),k=jdeb,jfin)
            jfin=i*(i+1)/2+m-1
         enddo
      enddo
      return
      end
