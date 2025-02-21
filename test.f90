program test
      use trexio

      implicit none
      character*(32) :: input_filename
      character*(128) :: err_msg

      integer :: nucleus_num
      integer :: i,j,k,l,m
      integer :: n
      integer :: max_str_len
      double precision, allocatable :: coord(:,:)
      double precision :: E_nuclear_repulsion
      character(len=4), allocatable :: label(:)
      character(len=6), allocatable :: label_c(:)
      character(len=6), allocatable :: label_l(:)
      character(len=4) :: j_char, k_char, l_char,kl_char
      character(len=6)::s, px, py, pz, dxx, dxy, dxz, dyy, dyz, dzz
      integer :: c
      integer :: shell_num
      integer, allocatable :: shell_ang_mom(:),nucleus_index(:)
      !character(len=6), allocatable :: tab(:,:)
      character(len=12), allocatable :: labels(:)
      character(len=6) :: shell_labels(6,0:2)

      integer(trexio_t) :: trexio_file
      integer(trexio_exit_code) :: rc

      input_filename = 'h2o.h5'


      !Ouverture du fichier :

      trexio_file = trexio_open(input_filename, 'r', TREXIO_AUTO, rc)
      !Gestion de l'erreur :
      if(rc /= TREXIO_SUCCESS) then
              call trexio_string_of_error(rc,err_msg)
             !print*, 'Erreur d''ouverture '//input_filename
             !stop -1
             print*,'Error: '//trim(err_msg)
             call exit(-1)
      endif
      !ENTETE
      print '(A, I45)','============================================='
      print '(A, I45)','File:'
      print '(A, I45)','Created by trexio'
      print '(A, I45)', 'date:'
      print '(A, I45)','============================================='
      print '(A)', ' &cost_AO'
      print '(A)', ' nsym=1'
      !Nombre d'orbitales atomiques:
      rc = trexio_read_ao_num(trexio_file,n)
      if(rc /= TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*,'Error:'//trim(err_msg)
              call exit(-1)
      endif
      print '(A, I5)', ' norb=', n
      print '(A, I5, A)', ' isym=', n, ','

      !Nombre d'atomes :
      rc = trexio_read_nucleus_num(trexio_file, nucleus_num)
      if(rc/= TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*,'Error: '//trim(err_msg)
              call exit(-1)
      endif
      print '(A, I4)',' natom=', nucleus_num

      !shell_num
      rc = trexio_read_basis_shell_num(trexio_file, shell_num)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif
      print*, 'shell_num=', shell_num

      !shell_ang_mom
      allocate(shell_ang_mom(shell_num))
      rc = trexio_read_basis_shell_ang_mom(trexio_file, shell_ang_mom)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif

      !nucleus_index
      allocate(nucleus_index(shell_num))
      rc = trexio_read_basis_nucleus_index(trexio_file, nucleus_index)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif
      print*, 'nucleus_index='
      print '(12(I4))', nucleus_index

     !------------------je lis les atomes----------
      !Labels des atomes :
      allocate(label(nucleus_num))
      rc = trexio_read_nucleus_label(trexio_file, label, 4)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif

      !print '(A, I6)', ' atom='
      allocate(label_c(nucleus_num))
      do i=1, nucleus_num
        c=1
        do j=1, i
            if(label(i)==label(j)) then
                    if(i==j) then
                           write(j_char, '(I4)') c
                           label_c(i)=trim(adjustl(label(i)))//trim(adjustl(j_char))
                           !print*, labels_c(i)
                   else
                            c=c+1
                    endif
            endif
          enddo
       enddo
      allocate(label_l(nucleus_num))
      do i=1, nucleus_num
          label_l(i) = ''''//label_c(i)//''''
      enddo
!     deallocate(label_c)
      !-------------je lis les orbits------------------------
      !Labels et orbites :
      print '(A, I4)',' label='

      shell_labels(:,:) = 'ERROR'
      shell_labels(1,0) = 's'
      shell_labels(1:3,1) = (/ 'px', 'py', 'pz' /)
      shell_labels(1:6,2) = (/ 'dxx', 'dxy', 'dxz', 'dyy', 'dyz', 'dzz' /)

      j=0
      allocate(labels(25))
      do i=1, shell_num
        l=shell_ang_mom(i)
           do k=1, ((l+1)*(l+2))/2
                j=j+1
                labels(j) = '''' // shell_labels(k,l) //''','
!                write(k_char, '(I4)') k
!                write(l_char, '(I4)') l
!                kl_char=trim(adjustl(k_char))//trim(adjustl(l_char))
!                labels(j) = trim(kl_char)
!                if(labels(j)=='10') then
!                        labels(j)='s'
!               elseif(labels(j)=='11') then
!                         labels(j)='px'
!                elseif(labels(j)=='21')then
!                         labels(j)='py'
!                elseif(labels(j)=='31')then
!                         labels(j)='pz'
!                elseif(labels(j)=='12')then
!                         labels(j)='dxx'
!               elseif(labels(j)=='22')then
!                        labels(j)='dxy'
!                elseif(labels(j)=='32')then
!                         labels(j)='dxz'
!               elseif(labels(j)=='42')then
!                         labels(j)='dyy'
!                elseif(labels(j)=='52')then
!                         labels(j)='dyz'
!                 else
!                         labels(j)='dzz'
!                endif
            print*, labels(j)
          enddo
       enddo
!-----------------------------------
      !Labels des atomes :
!      allocate(label(nucleus_num))
!      rc = trexio_read_nucleus_label(trexio_file, label, 4)
!      if(rc/=TREXIO_SUCCESS) then
!              call trexio_string_of_error(rc, err_msg)
!              print*, 'Error: '//trim(err_msg)
!              call exit(-1)
!      endif

!      allocate(labels_c(nucleus_num))
!      do i=1, nucleus_num
!        c=1
!        do j=1, i
!            if(label(i)==label(j)) then
!                    if(i==j) then
!                           write(j_char, '(I4)') c
!                           labels_c(i)=trim(adjustl(label(i)))//trim(adjustl(j_char))
                           !print*, labels_c(i)
!                   else
!                            c=c+1
!                    endif
!            endif
!          enddo
!       enddo
!      allocate(labels_l(nucleus_num))
!      do i=1, nucleus_num
!          labels_l(i) = ''''//labels_c(i)//''''
!      enddo


      !Atomes
      print '(A, I6)', ' atom='
      print '(A, 4(A5,'''''',''))',' ', (label_l(i), i=1, nucleus_num)

      !Coordonnées
      allocate(coord(3, nucleus_num))
      rc = trexio_read_nucleus_coord(trexio_file, coord)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif

      print '(A, I6)',' coor='
      do j=1, nucleus_num
        print '(3(F20.10,'',''))', coord(1:3, j)
      enddo

      !Energie nucleaire de répulsion :
      rc = trexio_read_nucleus_repulsion(trexio_file, E_nuclear_repulsion)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif
      print '(A, F20.12)',' enuc=', E_nuclear_repulsion

      print '(A, I6)', ' its='
      print '(A, I6)', '      1,0,0,0,0,0,0,0,'
      print '(A, I6)', ' jtsr='
      print '(A, I6)', '       1, 0, 0, 0, 0, 0, 0, 0,'

      print '(A, I6)', ' /'
      rc=trexio_close(trexio_file)
 end




