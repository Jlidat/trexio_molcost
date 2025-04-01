    subroutine t_Info(input_filename) 
      use trexio

      implicit none
      character*(32) :: input_filename
      character*(128) :: err_msg
      character*(37) :: output_filename

      integer :: nucleus_num
      integer :: i,j,k,l
      integer :: n
      double precision, allocatable :: coord(:,:)
      double precision :: E_nuclear_repulsion
      character(len=4), allocatable :: label(:)
      character(len=14), allocatable :: label_c(:)
      character(len=16), allocatable :: label_l(:)
      character(len=4) :: j_char, k_char, l_char,kl_char
      integer :: c
      integer :: shell_num
      integer, allocatable :: shell_ang_mom(:),nucleus_index(:), len_labelindex(:)
      character(len=14), allocatable :: labels(:)
      character(len=14), allocatable :: labelindex(:)

      integer(trexio_t) :: trexio_file
      integer(trexio_exit_code) :: rc
      integer :: output_unit
      
!Déclaration explicite de la subroutine maj(str,n)
      interface
              subroutine maj(str,n)
                      character(len=4),dimension(:) :: str
                      integer :: n
              end subroutine maj

        end interface
      
      output_unit=10
      !input_filename = 'h2o.h5'
      !output_filename ='h2oal3h.Info'!'benzene.Info'!'H2O.Info'

!      call getarg(1, input_filename)
      call Info_output_filename(input_filename, output_filename)
     !write(*,*) output_filename 
     
      !Ouverture du fichier input :

      trexio_file = trexio_open(input_filename, 'r', TREXIO_AUTO, rc)
      !Gestion de l'erreur :
      if(rc /= TREXIO_SUCCESS) then
              call trexio_string_of_error(rc,err_msg)
             !print*, 'Erreur d''ouverture '//input_filename
             !stop -1
             print*,'Error: '//trim(err_msg)
             call exit(-1)
      endif
      
      ! Ouverture du fichier de sortie
      open(unit=output_unit, file=output_filename, status='replace', action='write')
      
      !ENTETE

      print '(A, I45)','============================================='
      print '(A, I45)','File:'
      print '(A, I45)','Created by trexio'
      print '(A, I45)', 'date:'
      print '(A, I45)','============================================='
      print '(A)', ' &cost_AO'
      print '(A)', ' nsym=1'
      
      write(output_unit, '(A46)')'============================================='
      write(output_unit, '(A5)')'File:'
      write(output_unit, '(A17)')'Created by trexio'
      write(output_unit, '(A5)') 'date:'
      write(output_unit, '(A46)')'============================================='
      write(output_unit, '(A)') ' &cost_AO'
      write(output_unit, '(A)') ' nsym=1'
      !Nombre d'orbitales atomiques:
      rc = trexio_read_ao_num(trexio_file,n)
      if(rc /= TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*,'Error:'//trim(err_msg)
              call exit(-1)
      endif
      print '(A, I5)', ' norb=', n
      print '(A, I5, A)', ' isym=', n, ','
      
      write(output_unit, '(A, I5)') ' norb=', n
      write(output_unit, '(A, I5, A)') ' isym=', n, ','

      !Nombre d'atomes :
      rc = trexio_read_nucleus_num(trexio_file, nucleus_num)
      if(rc/= TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*,'Error: '//trim(err_msg)
              call exit(-1)
      endif
      print '(A, I4)',' natom=', nucleus_num
      
      write(output_unit, '(A, I4)')' natom=', nucleus_num

      !shell_num
      rc = trexio_read_basis_shell_num(trexio_file, shell_num)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif      
    
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
!      print*, 'nucleus_index='
!      print '(12(I4))', nucleus_index

     !---------------Lecture des labeles des atomes----------
      !Labels des atomes :
      allocate(label(nucleus_num))
      rc = trexio_read_nucleus_label(trexio_file, label, 4)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif
!Ecrire les labels d'atomes en majuscule : label(nucleus_num)
        call maj(label, nucleus_num)

!      print '(A, I6)', ' atom='
      allocate(label_c(nucleus_num))!Résultat affiché en colonne des atomes numérotés
      do i=1, nucleus_num
        c=1
        do j=1, i
            if(label(i)==label(j)) then
                    if(i==j) then
                           write(j_char, '(I4)') c
                           label_c(i)=trim(adjustl(label(i)))//trim(adjustl(j_char))
                         !  len_label_c(i)=len_trim(label_c(i))
                         !  print*, label_c(i), len_label_c(i)
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
!    deallocate(label_c)

      !-Lecture des noms des atomes de label_c et les faire correspondre aux nucleus_index--
!    print*, 'labelindex='
    allocate(labelindex(shell_num))
    allocate(len_labelindex(shell_num)) !Calcul de longueur de label_c
      do i=1, shell_num 
          do j=1, nucleus_num
                if(nucleus_index(i)==j)then
                        labelindex(i)=label_c(j)
                        len_labelindex(i)=len_trim(labelindex(i))
                endif
           enddo
!                print*, labelindex(i)
       enddo

      !-----Lecture des orbits--------
      !Labels et orbites :    
      j=0
      allocate(labels(n))
      do i=1, shell_num
        l=shell_ang_mom(i)
          do k=1, ((2*l)+1)!((l+1)*(l+2))/2
             j=j+1
             write(k_char, '(I4)') k
             write(l_char, '(I4)') l
             kl_char=trim(adjustl(k_char))//trim(adjustl(l_char))
             labels(j) = trim(kl_char)
           !-------------------------------------------------------
             if(len_labelindex(i)==4)then
               if(labels(j)=='10') then
                 labels(j)=''''//trim(labelindex(i))//'1s'//'  '//''''
               elseif(labels(j)=='11') then
                 labels(j)=''''//trim(labelindex(i))//'2pz'//' '//''''
               elseif(labels(j)=='21')then
                 labels(j)=''''//trim(labelindex(i))//'2px'//' '//''''
               elseif(labels(j)=='31')then
                 labels(j)=''''//trim(labelindex(i))//'2py'//' '//''''
               elseif(labels(j)=='12')then
                 labels(j)=''''//trim(labelindex(i))//'3d0'//' '//''''
               elseif(labels(j)=='22')then
                 labels(j)=''''//trim(labelindex(i))//'3d1+'//''//''''
               elseif(labels(j)=='32')then
                 labels(j)=''''//trim(labelindex(i))//'3d1-'//''//''''
               elseif(labels(j)=='42')then
                 labels(j)=''''//trim(labelindex(i))//'3d2+'//''//''''
               else
                 labels(j)=''''//trim(labelindex(i))//'3d2-'//''//''''
               endif
            
            elseif(len_labelindex(i)==3)then
               if(labels(j)=='10') then
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'1s'//'  '//''''
               elseif(labels(j)=='11') then
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'2pz'//' '//''''
               elseif(labels(j)=='21')then
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'2px'//' '//''''
               elseif(labels(j)=='31')then
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'2py'//' '//''''
               elseif(labels(j)=='12')then
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'3d0'//' '//''''
               elseif(labels(j)=='22')then
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'3d1+'//''//''''
               elseif(labels(j)=='32')then
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'3d1-'//''//''''
               elseif(labels(j)=='42')then
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'3d2+'//''//''''
               else
                 labels(j)=''''//trim(adjustl(labelindex(i)))//' '//'3d2-'//''//''''
               endif
            else
               if(labels(j)=='10') then
                 labels(j)=''''//trim(labelindex(i))//'  '//'1s'//'  '//''''
               elseif(labels(j)=='11') then
                 labels(j)=''''//trim(labelindex(i))//'  '//'2pz'//' '//''''
               elseif(labels(j)=='21')then
                 labels(j)=''''//trim(labelindex(i))//'  '//'2px'//' '//''''
               elseif(labels(j)=='31')then
                 labels(j)=''''//trim(labelindex(i))//'  '//'2py'//' '//''''
               elseif(labels(j)=='12')then
                 labels(j)=''''//trim(labelindex(i))//'  '//'3d0'//' '//''''
               elseif(labels(j)=='22')then
                 labels(j)=''''//trim(labelindex(i))//'  '//'3d1+'//''//''''
               elseif(labels(j)=='32')then
                 labels(j)=''''//trim(labelindex(i))//'  '//'3d1-'//''//''''
               elseif(labels(j)=='42')then
                 labels(j)=''''//trim(labelindex(i))//'  '//'3d2+'//''//''''
               else
                 labels(j)=''''//trim(labelindex(i))//'  '//'3d2-'//''//''''
               endif
            endif
          enddo
       enddo
!   print*,(label_c(i), i=1, nucleus_num), (len_label_c(i), i=1,nucleus_num)
!   write(*,*) (label_c(i), i=1, nucleus_num), (len_label_c(i), i=1,nucleus_num)
       !----
       print '(A, I4)',' label='
       print '(5(A10, '',''))',(labels(i), i=1, n)
       
       write(output_unit, '(A, I4)') ' label='
       write(output_unit, '(5(A10, '',''))') (labels(i), i=1, n)
   
      !Atomes
      print '(A, I6)', ' atom='
      print '(A, 4(A5,'''''',''))', ' ', (label_l(i), i=1, nucleus_num)
      
      write(output_unit, '(A, I6)') ' atom='
      write(output_unit, '(A, 4(A5,'''''',''))')' ', (label_l(i), i=1, nucleus_num)

      !Coordonnées
      allocate(coord(3, nucleus_num))
      rc = trexio_read_nucleus_coord(trexio_file, coord)
      if(rc/=TREXIO_SUCCESS) then
              call trexio_string_of_error(rc, err_msg)
              print*, 'Error: '//trim(err_msg)
              call exit(-1)
      endif

      print '(A, I6)',' coor='
      write(output_unit, '(A, I6)') ' coor='
      do j=1, nucleus_num
        print '(3(F20.10,'',''))', coord(1:3, j)
        write(output_unit, '(3(F20.10,'',''))') coord(1:3, j)
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
      
      write(output_unit, '(A, F20.12)')' enuc=', E_nuclear_repulsion
      write(output_unit, '(A, I6)') ' its='
      write(output_unit, '(A, I6)') '      1,0,0,0,0,0,0,0,'
      write(output_unit, '(A, I6)') ' jtsr='
      write(output_unit, '(A, I6)') '       1, 0, 0, 0, 0, 0, 0, 0,'
      write(output_unit, '(A, I6)') ' /'


      rc=trexio_close(trexio_file)
      close(output_unit)
  end
!-----------------------------------------------------
subroutine Info_output_filename(ifilename, ofilename)
        implicit none
        character(*) ::ifilename
        character(*) ::ofilename
        integer :: kk
        character(32)::base_name
        integer ::len_input


       ! call getarg(1, ifilename)
        len_input=len_trim(ifilename)
        !Extraire le nom de base du fichier sans l'extension
        base_name=''
        do kk=len_input, 1, -1
                if(ifilename(kk:kk)=='.')then
                        base_name=ifilename(1:kk-1)
                        exit
                endif
        enddo

        !construire le nom de fichier de sortie
        if(base_name/='')then
                write(ofilename, '(A,A)') trim(base_name),'.Info'
        else
                ofilename=ifilename//'.Info'
        endif
end
!----------------------------------------------
Subroutine maj(str,n)
        implicit none
        character(len=*), dimension(:) :: str
        integer :: i,j
        integer :: n
    ! Parcourir chaque caractère de la chaîne et le convertir en majuscule
        do i=1,n
           do j=1, len(trim(adjustl(str(i))))
              if(iachar(str(i)(j:j))>=iachar('a').and.iachar(str(i)(j:j))<=iachar('z')) then
                      str(i)(j:j)=achar(iachar(str(i)(j:j))-iachar('a')+iachar('A'))
              endif
           enddo
        enddo
end
