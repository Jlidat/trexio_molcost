program test
      use trexio

      implicit none
      character*(32) :: input_filename
      character*(128) :: err_msg

      integer :: nucleus_num
      integer :: i,j
      integer :: n
      integer :: max_str_len
      double precision, allocatable :: coord(:,:)
      double precision :: E_nuclear_repulsion
      character(len=4), allocatable :: label(:)
      character(len=6), allocatable :: labels_c(:)
      character(len=6), allocatable :: labels_l(:)
      character(len=4) :: j_char
      integer :: c
      integer :: k

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


      
      !Labels et orbites :
      print '(A, I4)',' label='

      
      
      
      !Labels des atomes :
      allocate(label(nucleus_num))
      rc = trexio_read_nucleus_label(trexio_file, label, 4)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif
      
      print '(A, I6)', ' atom='
      allocate(labels_c(nucleus_num))
      do i=1, nucleus_num
        c=1
        do j=1, i 
            if(label(i)==label(j)) then
                    if(i==j) then
                           write(j_char, '(I4)') c 
                           labels_c(i)=trim(adjustl(label(i)))//trim(adjustl(j_char))
                           !print*, labels_c(i)
                   else
                            c=c+1
                    endif
            endif
          enddo
       enddo
      allocate(labels_l(nucleus_num))
      do i=1, nucleus_num
          labels_l(i) = ''''//labels_c(i)//''''
      enddo
          print '(A, 4(A5,'''''',''))',' ', (labels_l(i), i=1, nucleus_num) 
      
      !Coordonnées
      allocate(coord(3, nucleus_num))
      rc = trexio_read_nucleus_coord(trexio_file, coord)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif
      
      print '(A, I6)',' coor='
      do i=1, nucleus_num
        print '(3(F20.10,'',''))', coord(1:3, i)
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




