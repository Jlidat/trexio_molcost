program INPORB
        use trexio

        implicit none
        character*(32) :: input_filename
        character*(128) :: err_msg
   
        integer(trexio_t) :: trexio_file
        integer(trexio_exit_code) :: rc
        
        integer :: ao_num, mo_num
        integer :: i,j
        real(8), allocatable :: coeff(:,:)
        real(8), allocatable :: coefficient(:)
       ! real(8), allocatable :: coefficients(:,:)
        real(8) :: format_fichier
        !character(len=20), allocatable :: coefficient_str(:)


        input_filename = 'h2o.h5'
        ! Ouverture du fichier
        trexio_file=trexio_open(input_filename, 'r', TREXIO_AUTO, rc)
        ! Gestion de l'erreur
        if(rc /= TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
format_fichier=1.1
print '((A7),(F4.1))', '#INPORB', format_fichier
print '(A5)', '#INFO'
print '(A14)', '* SCF orbitals'
print '(3(I8))', 0, 1, 2


!--------------------------------------
        ! Nombre d'orbitales atomiques:
        rc=trexio_read_ao_num(trexio_file, ao_num)
        if(rc /= TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        !print*, 'ao_num = '
        print '(I8)', ao_num

        ! Nombre d'orbitales moléculaires:
        rc=trexio_read_mo_num(trexio_file, mo_num)
        if(rc /= TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
       ! print*, 'mo_num = '
        print '(I8)', mo_num
        
print*,'*BC:HOST  cnxv3-4 PID 39350 DATE Jour Mois 13 heure année'
!----------------------------------------------------------
!#ORB
!Les coefficients 
        allocate(coeff( ao_num, mo_num))
        allocate(coefficient(mo_num))
        rc=trexio_read_mo_coefficient(trexio_file, coeff)
        if(rc /= TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        print*, 'Les coefficients : '
        do j=1, mo_num
                print*,'* ORBITAL    1    ',j
                do i=1, ao_num 
                       coefficient(j)=coeff(i,j)  
                       print '(E18.12)', coefficient(j)   
                enddo
        enddo

!----------------------------
print '(A4)', '#OCC'
print '(A21)', '* OCCUPATIONS NUMBERS'



!-----------------------------
print '(A6)', '#INDEX'
print '(A12)','* 1234567890'
print '(A12)','0 iiiiisssss'
print '(A12)','1 ssssssssss'
print '(A6)','2 ssss'
end

