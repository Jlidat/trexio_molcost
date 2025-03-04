program trexio2mono
        use trexio
        !use script_trexio

        implicit none
        character*(32) :: input_filename
        character*(128) :: err_msg

        integer(trexio_t):: trexio_file
        integer(trexio_exit_code):: rc

        real(8), allocatable, dimension(:,:) :: overlap
       ! real(8), allocatable, dimension(:) :: value
        integer(4), allocatable :: isym(:) !idx(:,:)
        integer(4) :: norb, nsym
        integer(4) :: howmany, offset
        integer :: k,i

        character(len=30) :: mono1
        character(len=30) :: prefix
        character(len=2) :: cote
        integer :: if_mono
        character(2) :: ante
        
        !character(300) :: q5file !!!
        !integer(hid_t) :: file_id
        !integer(4):: error

        !init
        if(mono1==' ')mono1=trim(prefix)//'MONO'
        !cote=''''
        !q5file=trim(workdir)//trim(prefix)//'q5'


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
        
        !Nombre d'orbitales atomiques:
        rc = trexio_read_ao_num(trexio_file, norb)
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
 
        allocate(overlap(norb, norb))

        allocate(isym(nsym))
        do i=1, nsym
            isym(i)=norb
        enddo
 
        print*, 'isym', isym
        print*, 'norb', norb
! Lecture des recouvrements

        howmany = norb*(norb+1)/2
        offset=0

       rc=trexio_read_ao_1e_int_overlap(trexio_file, overlap)
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        print*, 'overlap'

        call script(overlap,norb,norb,norb,norb)
        call ecriS(if_mono, overlap, norb, mono1, 'TREXIO', .true., nsym, isym,(/(k,k=1,norb)/))


        rc=trexio_close(trexio_file)
end
