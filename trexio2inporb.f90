  subroutine trexio2inporb
        use trexio

        implicit none
        character*(32) :: input_filename
        character*(128) :: err_msg
        character(256) :: output_filename
   
        integer(trexio_t) :: trexio_file
        integer(trexio_exit_code) :: rc
        integer :: output_unit
        
        integer :: ao_num, mo_num
        integer :: i,j
        real(8), allocatable :: coefficient(:,:)
        real(8) :: format_fichier
        real(8), allocatable :: occ_1e(:,:)
        real(8), allocatable :: occupation(:,:)

        character(len=10) :: date, time
        character(len=7) :: day, month, year

        output_unit=11
        input_filename = 'h2o.h5'
        output_filename = 'INPORB'

        ! Ouverture du fichier
        trexio_file=trexio_open(input_filename, 'r', TREXIO_AUTO, rc)
        ! Gestion de l'erreur
        if(rc /= TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        
        ! Ouverture du fichier de sortie
        open(unit=output_unit, file=output_filename, status='replace', action='write')
        
        format_fichier=1.1
        print '((A7),(F4.1))', '#INPORB', format_fichier
        print '(A5)', '#INFO'
        print '(A14)', '* SCF orbitals'
        print '(3(I8))', 0, 1, 2
        
        write(output_unit, '((A7),(F4.1))') '#INPORB', format_fichier
        write(output_unit, '(A5)') '#INFO'
        write(output_unit, '(A14)') '* SCF orbitals'
        write(output_unit, '(3(I8))') 0, 1, 2


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
        write(output_unit, '(I8)') ao_num

        ! Nombre d'orbitales moléculaires:
        rc=trexio_read_mo_num(trexio_file, mo_num)
        if(rc /= TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
       ! print*, 'mo_num = '
        print '(I8)', mo_num
        write(output_unit, '(I8)') mo_num

        call date_and_time(date=date, time=time)
        call system("echo $$")
        day=date(1:4)
        month=date(5:6)
        year=date(7:8)
        print '(A16,x,A4,x,A2,x,A2,x,A8/)','*BC:HOST cnxv3-4', day, month, year, time
        write(output_unit, '(A16,x,A4,x,A2,x,A2,x,A8/)')'*BC:HOST cnxv3-4', day, month, year, time
!----------------------------------------------------------
        print '(A4)', '#ORB'
        write(output_unit,'(A4)') '#ORB'
!Les coefficients
        allocate(coefficient(ao_num, mo_num))
        rc=trexio_read_mo_coefficient(trexio_file, coefficient)
        if(rc /= TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
     !   print*, 'Les coefficients : '
           do j=1, mo_num
               print '(A14, I4)', '* ORBITAL    1', j
               print '(4D18.12)',(coefficient(i,j), i=1,ao_num)
               write(output_unit, '(A9,2I5)') '* ORBITAL',1, j
               write(output_unit, '(4D18.12)') (coefficient(i,j), i=1,ao_num)
               enddo      
        
!----------------------------
        print '(A4)', '#OCC'
        print '(A21)', '* OCCUPATION NUMBERS'
        
        write(output_unit, '(A4)') '#OCC'
        write(output_unit, '(A20)') '* OCCUPATION NUMBERS'
        allocate(occ_1e(mo_num,mo_num))
        allocate(occupation(mo_num,mo_num))
        rc=trexio_read_rdm_1e(trexio_file, occ_1e)
        if(rc /= TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        
        !do i=1, mo_num
        !    do j=1, mo_num
        !         occupation(i,i)=occ_1e(i,i)
         !   enddo
       ! enddo
                 !print '(4(D18.12))', (occupation(i,i),i=1,mo_num)
                 print '(4(D18.12))', (occ_1e(i,i),i=1,mo_num)
                 write(output_unit, '(4(D18.12))') (occ_1e(i,i),i=1,mo_num)

!-----------------------------------------
        print '(A6)', '#INDEX'
        print '(A12)','* 1234567890'
        print '(A12)','0 iiiiisssss'
        print '(A12)','1 ssssssssss'
        print '(A6)','2 ssss'
        
        write(output_unit, '(A6)') '#INDEX'
        write(output_unit, '(A12)') '* 1234567890'
        write(output_unit, '(A12)') '0 iiiiisssss'
        write(output_unit, '(A12)') '1 ssssssssss'
        write(output_unit, '(A6)') '2 ssss'


        rc=trexio_close(trexio_file)
        close(output_unit)
end

