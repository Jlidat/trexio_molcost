program locorb2trexio
        use trexio
        implicit none
        character(256) :: ifilename, ofilename

        call copy_coef(ifilename, ofilename)
contains
     subroutine copy_coef(input_filename, output_filename)
             use trexio
        implicit none
        integer(trexio_t) :: trexio_file
        integer(trexio_exit_code) ::rc
        character*(128) :: err_msg

        character(*) :: input_filename, output_filename
        integer :: input_unit , output_unit
        
        integer :: ios,i,j
        integer :: mo_num, ao_num,pas
        character(len=72)::line
        real(8), allocatable:: mo_coefficient(:,:)
        real(8) ::c1, c2, c3, c4, c5,c11, c22,c33,C111,C222

!--------Nom des fichiers d'entrée et de sortie
       ! input_filename='LOCORB'
       call getarg(1, input_filename)
!        output_filename='h2o.h5'
        call getarg(2, output_filename)
        input_unit=10
        output_unit=11

!-------Ouvrir le fichier input
        open(unit=input_unit,file=input_filename,access='sequential',&
                status='old',position='asis',iostat=ios)
        if(ios/=0)then
               print*, 'Erreur d''ouverture du fichier source', input_filename
              stop
        endif

!-------Ouvrir de trexio en mode écriture
        trexio_file=trexio_open(output_filename,'u', TREXIO_HDF5, rc)
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc,err_msg)
                print*,'Error: '//trim(err_msg)
                call exit(-1)
        endif
!-------Lire le fichier LACORB ligne par ligne; les 5 lignes premieres ne sont pas écrites
        do i=1, 4
               read(input_unit,fmt='(A)')line
              !read(line, fmt='(A)') temp 
        enddo
!-------lire le nombre d'orbitals atomiques
        read(input_unit,fmt='(A)') line
        read(line, fmt='(I8)') ao_num
        !print *, 'ao_num=', ao_num
!-------lire le nombre d'orbitals moléculaires
        read(input_unit,fmt='(A)') line
        read(line, fmt='(I8)') mo_num
      !print *, 'mo_num=', mo_num 
!-----------------------------------------------
       read(input_unit,fmt='(A)') line
      !print *, 'tempon2', line
!-------pas=6----------------------------------------
       pas=int(mo_num/4)
!---------------------------------------------------------------------------------------------------
        allocate(mo_coefficient(ao_num,mo_num))
     do j=1, mo_num
        read(input_unit,fmt= '(A72)')line
        do i=1, pas
             read(input_unit, fmt='(4(D18.12))') c1, c2, c3, c4 
             mo_coefficient(1+(4*(i-1)),j)=c1
             mo_coefficient(2+(4*(i-1)),j)=c2
             mo_coefficient(3+(4*(i-1)),j)=c3
             mo_coefficient(4+(4*(i-1)),j)=c4
        enddo
           if(mod(ao_num,4)/=0) then
             if(mod(ao_num,4)==3)then
                  read(input_unit, fmt='(3(D18.12))') c11, c22, c33 
                  mo_coefficient(ao_num-2,j)=c11
                  mo_coefficient(ao_num-1,j)=c22
                  mo_coefficient(ao_num,j)=c33
             elseif(mod(ao_num,4)==2)then
                  read(input_unit, fmt='(2(D18.12))') c111, c222 
                  mo_coefficient(ao_num-1,j)=c111
                  mo_coefficient(ao_num,j)=c222
             else
                 read(input_unit, fmt='(D18.12)')c5
                 mo_coefficient(ao_num,j)=c5
              endif
           endif
     enddo
!-----affichage de mo_coefficient avant écriture dans Trexio------------     
     do j=1, mo_num
        print '(D18.12)', (mo_coefficient(i,j), i=1,ao_num)
     enddo
!        !enddo
!---------------------------------------------------------------------------------------------------
        rc=trexio_write_mo_coefficient(trexio_file,mo_coefficient)
        if (rc/=TREXIO_SUCCESS) then 
                call trexio_string_of_error(rc, err_msg)
                print *, 'Error: '//trim(err_msg)
                call exit(-1)
        endif

!---------------------------------------------------------------------------------------------------
       rc=trexio_close(trexio_file)
       close(input_unit)
    end
end
