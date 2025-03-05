      subroutine ecris(ifil0,s,norb,file,prog,entete,nsym,isym,iord)
      IMPLICIT real(8) (A-H,O-x,Z),logical*1(y)
      character(*) file,prog
      character(80) aa
      real(8) s(norb,norb),s1(norb,norb)
      allocatable sl(:)
      logical yw,entete
      character(21) cdate
      character(7) mat1
      character(80) tete(5)
      dimension isym(8)
      dimension iord(norb)
      yw=.false.


      write(6,*) 'ifil0=',ifil0
      if(ifil0<0) then
      print*,'ecriS ONEINT '
      call flush(6)
         mat1='ONEINT'
         ifil=-ifil0
      else if(ifil0<100) then
         mat1='OVERLAP'
         ifil=ifil0
      else
         mat1='AOONEI'  ! Atomic One-Electron Integrals
         ifil=ifil0-100
      endif

c entete du fichier

      if(entete) then
         write(tete(1),'(a)')'=================================='
         write(tete(2),'(2a)')'File: ', trim(file)
         write(tete(3),'(2a)')'created by ',trim(prog)
         write(tete(4),'(2a)')'date: ',cdate
         write(tete(5),'(a)')'=================================='
         write(ifil) tete(1:5)
c         !write(ifil, '(a)') tete(1:5)
      endif

c
c     write the overlap (or OneInt) matrix (sym by sym)
c     The orbitals are ordered: sym1 - sym2 - ...
c
      write(aa,'(a)')'=='//trim(mat1)//' MATRIX (SYM)'

      !write(ifil,'(a)') aa
      write(ifil) aa
      ifin=0
      do k=1,nsym
         ideb=ifin+1
         ifin=ifin+isym(k)*(isym(k)+1)/2
      enddo
      allocate(sl(ifin))
      ifin=0
      k=0
      do ks=1,nsym
         do kk=1,isym(ks)
            k=k+1
            ideb=ifin+1
            ifin=ifin+k-sum(isym(1:ks-1))
            sl(ideb:ifin)=(/(s(k,j),j=sum(isym(1:ks-1))+1,k)/)
         enddo
      enddo
      write(ifil) ifin,nsym,isym(1:nsym)
      write(ifil) sl(1:ifin)
      deallocate(sl)
c      write(ifil,'(3(I5))') ifin,nsym,isym(1:nsym)
c      write(ifil,'(F12.6)') sl(1:ifin)
c
c     write the overlap (or OneInt)  matrix (total)
c     The orbitals are reordered according to iord
c
      write(aa,'(a)')'=='//trim(mat1)//' MATRIX (TOTAL)'
      !write(ifil,'(a)') aa
      write(ifil) aa
      l=norb*(norb+1)/2
      allocate(sl(l))
      s1(iord(1:norb),iord(1:norb))=s
      ifin=0
      do k=1,norb
         ideb=ifin+1
         ifin=ifin+k
         sl(ideb:ifin)=(/(s1(k,j),j=1,k)/)
      enddo
      write(ifil) ifin,nsym,isym(1:nsym)
      write(ifil) sl(1:ifin)
c      write(ifil,'(3(I5))') ifin,nsym,isym(1:nsym)
c      write(ifil,'(F12.6)') sl(1:ifin)
      deallocate(sl)
      s(iord(1:norb),iord(1:norb))=s1
      return
      end
