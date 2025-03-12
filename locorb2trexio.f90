program locorb2trexio
        use trexio
        call copy_coef
contains
     subroutine copy_coef
             use trexio
        implicit none
        integer(trexio_t) :: trexio_file
        integer(trexio_exit_code) ::rc
        character*(128) :: err_msg

        character(256) :: input_filename, output_filename
        integer :: input_unit , output_unit
        
        integer :: ios,i,j,k
        integer ::n,m,m_14,m_16, num_mo, num_ao,pas,nt_mo
        character(len=72)::line
        real(8), allocatable:: mo_coefficient(:)
        real(8) ::c1, c2, c3, c4, c5,c11, c22,c33,C44,C55

!--------Nom des fichiers d'entrée et de sortie
        input_filename='LOCORB'
        output_filename='h2o.h5'
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
       ! trexio_file=trexio_open_unsafe(input_filename, 'w', TREXIO_TEXT, rc)
        trexio_file=trexio_open(output_filename,'u', TREXIO_HDF5, rc)
        if(rc/=TREXIO_SUCCESS) then
                call trexio_string_of_error(rc,err_msg)
                print*,'Error: '//trim(err_msg)
                call exit(-1)
        endif
!-------Lire le fichier LACORB ligne par ligne; les 5 lignes premieres ne sont pas écrites
        do i=1, 4
               read(input_unit,fmt='(A)')line
       !        read(line, fmt='(A)') temp 
        enddo
!-------lire le nombre d'orbitals atomiques
        read(input_unit,fmt='(A)') line
        read(line, fmt='(I8)') num_ao
        !print *, 'num_ao=', num_ao 
!-------lire le nombre d'orbitals moléculaires
        read(input_unit,fmt='(A)') line
        read(line, fmt='(I8)') num_mo
       ! print *, 'num_mo=', num_mo 
!-----------------------------------------------
       read(input_unit,fmt='(A)') line
      ! print *, 'tempon2', line
!-----------------------------------------------
      ! print *, 'mo_coefficient'
      ! nombre de lignes remplies dans un bloc om : partie entière de 25/4=6,+1ligne incomplète
       n=int(num_mo/4)
       nt_mo=num_ao*num_mo ! nombre total des coefficients
       pas=n+2             !nombre total de lignes dans un bloc om 
       m=(num_mo-1)*pas    !nombre total des lignes lu. le -1 pour la ligne avec le commentaire
       m_14=(14-1)*pas     ! lire que les 14 blocs premiers
       allocate(mo_coefficient(nt_mo-25))
       !Supprimer le 15e mo
       !lire les 14 1ers mo
       do j=0,m_14, pas 
              read(input_unit,fmt='(A)') line
          do i=1+j,n+j !num_mo 
              read(input_unit,fmt='(A72)')line
              read(line, fmt='(D18.12, D18.12, D18.12, D18.12)') c1, c2, c3,c4
                mo_coefficient(j+1)=c1
                mo_coefficient(j+2)=c2
                mo_coefficient(j+3)=c3
                mo_coefficient(j+4)=c4
            ! print '(D18.12)', (mo_coefficient(j+k), k=1,4)
             write(output_unit, '(D18.12)') (mo_coefficient(j+k),k=1,4)
          enddo  
              read(input_unit,fmt='(A72)')line
              read(line, fmt='(D18.12)')c5
              mo_coefficient(j+5)=c5
             !print '(D18.12)', (mo_coefficient(j+5))
             write(output_unit, '(D18.12)') mo_coefficient(j+5)
       enddo
!--------------------------------------------------------------------
       m_16=m_14+pas
        do j=m_14+1, m_16
                read(input_unit, fmt='(A)') line
        enddo
       !lire à partir 16e bloc mo jusqu'à la fin
       do j=m_16+1,m, pas 
              read(input_unit,fmt='(A)') line
          do i=1+j,n+j !num_mo 
              read(input_unit,fmt='(A72)')line
              read(line, fmt='(D18.12, D18.12, D18.12, D18.12)') c11, c22, c33,c44
                mo_coefficient(j+1)=c11
                mo_coefficient(j+2)=c22
                mo_coefficient(j+3)=c33
                mo_coefficient(j+4)=c44
            ! print '(D18.12)', (mo_coefficient(j+k), k=1,4)
             write(output_unit, '(D18.12)') (mo_coefficient(j+k),k=1,4)
          enddo  
              read(input_unit,fmt='(A72)')line
              read(line, fmt='(D18.12)')c55
              mo_coefficient(j+5)=c55
            ! print '(D18.12)', (mo_coefficient(j+5))
             write(output_unit, '(D18.12)') mo_coefficient(j+5)
       enddo


!-----Ecrire les "mo_coefficient" dans Trexio------------
              rc = trexio_write_mo_coefficient(trexio_file,mo_coefficient)
              if(rc/=TREXIO_SUCCESS) then
                      call trexio_string_of_error(rc, err_msg)
                      print*, 'Error: '//trim(err_msg)
                      call exit(-1)
             endif
              !trexio.write_mo_coefficient(trexio_file,mo_coefficient)
       rc=trexio_close(trexio_file)
       close(input_unit)
    end

end
