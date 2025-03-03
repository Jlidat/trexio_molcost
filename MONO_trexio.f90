program trexio2mono
        use trexio

        implicit none
        character*(32) :: input_filename
        character*(128) :: err_msg

        integer(trexio_t):: trexio_file
        integer(trexio_exit_code):: rc

        real(8), allocatable, dimension(:,:) :: overlap
        real(8), allocatable, dimension(:) :: value
        integer(4), allocatable :: isym(:), idx(:,:)
        integer(4) :: norb, nsym
        integer(4) :: howmany, offset
        integer :: k

        character(len=15) :: mono1
        integer :: if_mono

        character(2) :: ante
        !character(300) :: q5file !!!
        !integer(hid_t) :: file_id
        !integer(4):: error

        input_filename ='h2o.h5'
        ! Ouverture du fichier 
        trexio_file=trexio_open(input_filename, 'r', TREXIO_AUTO, rc)
        ! Gestion de l'erreur
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        
        !lire la symetrie
        nsym=1
 !       allocate(isym(nsym))
        
        !Nombre d'orbitales atomiques:
        rc = trexio_read_ao_num(trexio_file, norb)
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        allocate(value(norb*(norb+1)/2))
        allocate(overlap(norb, norb))
        allocate(idx(norb*(norb+1)/2, 2))
!        print*, 'isym', isym
        print*, 'norb', norb
! Lecture des recouvrements

        howmany = norb*(norb+1)/2
        offset=0

       rc=trexio_read_ao_1e_int_overlap(trexio_file, value)
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        overlap=0.d0
        do k=1, howmany
                overlap(idx(k,1), idx(k,2))= value(k)
                overlap(idx(k,2), idx(k,1))= value(k)
        enddo
        print*, 'overlap'

        call script(overlap,norb,norb,norb,norb)
        call ecriS(if_mono, overlap, norb, mono1, 'TREXIO', .true., nsym, isym,(/(k,k=1,norb)/))


        if(ante=='OM')then
                rc=trexio_read_mo_1e_int_overlap(trexio_file, value)
                if(rc/=TREXIO_SUCCESS) then
                        call trexio_string_of_error(rc, err_msg)
                        print*, 'Error:'//trim(err_msg)
                        call exit(-1)
                 endif
                 
                 overlap=0.d0
                 do k=1, howmany
                        overlap(idx(k,1), idx(k,2))=value(k)
                        overlap(idx(k,2), idx(k,1))=value(k)
                 enddo
                 print*, 'OneInt'
                call script(overlap,norb,norb,norb,norb)
                call ecriS(-if_mono, overlap, norb, mono1, 'TREXIO', .false., nsym, isym,(/(k, k=1, norb)/))
                rewind if_mono
        endif

        rc=trexio_close(trexio_file)
end
