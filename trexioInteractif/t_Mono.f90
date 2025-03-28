subroutine t_Mono(input_filename)
        use trexio

        implicit none
        character*(32) :: input_filename
        character*(37) :: output_filename
        character*(80) :: aa
        character*(128) :: err_msg

        integer(trexio_t):: trexio_file
        integer(trexio_exit_code):: rc

        real(8), allocatable, dimension(:,:) :: overlap
        integer(4), allocatable :: isym(:) 
        integer(4) :: norb, nsym
        integer(4) :: howmany, offset
        integer :: k,i

        character(len=30) :: mono1
        integer :: output_unit


        mono1='MONO'
        output_unit=12
!        input_filename ='h2o.h5'
!        call getarg(1, input_filename)
!        output_filename ='h2oal3h.mono'!'benzene.mono'!'H2O.mono'
        call Mono_output_filename(input_filename, output_filename)        

        ! Ouverture du fichier d'entrée
        trexio_file=trexio_open(input_filename, 'r', TREXIO_AUTO, rc)
        ! Gestion de l'erreur
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
        
        ! Ouverture du fichier de sortie
        open(unit=output_unit, file=output_filename, status='replace', action='write', form='unformatted')
      
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
!       write(output_unit) 'isym', isym
        print*, 'norb', norb
!       write(output_unit) 'norb', norb
        
        ! Lecture des recouvrements

        howmany = norb*(norb+1)/2
        offset=0

       rc=trexio_read_ao_1e_int_overlap(trexio_file, overlap)
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc, err_msg)
                print*, 'Error:'//trim(err_msg)
                call exit(-1)
        endif
!        print*, 'OVERLAP'
!       write(output_unit) ''
        aa='==OVERLAP MATRIX (SYM)'
!       write(output_unit) aa
         
        call script(overlap,norb,norb,norb,norb)
        call ecriS(output_unit, overlap, norb, mono1, 'TREXIO', .true., nsym, isym,(/(k,k=1,norb)/))


        rc=trexio_close(trexio_file)
        close(output_unit)
end
!----------------------------------------
subroutine Mono_output_filename(ifilename, ofilename)
        implicit none
        character(*) :: ifilename, ofilename
        integer :: kk
        character(32) :: base_name
        integer::len_input

        
!        call getarg(1, ifilename)

        len_input = len_trim(ifilename)

        !Extraire le nom de base du fichier sans l'extension
        base_name=''
        do kk=len_input, 1, -1
                if(ifilename(kk:kk)=='.')then
                        base_name=ifilename(1:kk-1)
                        exit
                 endif
        enddo
        !Construre le nom de fichier de sortie
        if(base_name/='')then
                write(ofilename, '(A,A)') trim(base_name),'.Mono'
         else
                ofilename = ifilename//'.Mono'
         endif
end

              
