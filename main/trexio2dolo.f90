program trexio2dolo 
      use trexio
      implicit none
      character(len=100)::input_filename
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     integer :: user_input

!     Demander une entrée numérique et la valider
!     print*, "Entrez un nombre entier entre 1 et 100 :"
!     read*, user_input

!     Valdation de la plage
!     if(user_input < 1.or.user_input > 100)then
!        print*, "Entrée invalide ! le nombre doit être entre 1 et 100"
!        stop
!        endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!       Demander le nom du fichier d'entrée : "
        print*, "Donnez le nom du fichier d'entrée :"
        read*, input_filename

!       Valider le nom de fichier d'entrée pour éviter les attaques d'injection de chemin
        if(.not.validate_filename(input_filename))then
                print*, "Erreur : Nom de fichier invalide."
                call exit(-1)
         endif

        call t_Info(input_filename)
        call t_Inporb(input_filename)
        call t_Mono(input_filename)


  contains
          ! fonction de validation de nom de fichier
          logical function validate_filename(filename)
                  character(len=*) :: filename

          ! Vérifier que le fichier ne contient pas de caractères dangereux comme ../ ou /
          if(index(filename, '../')>0.or.index(filename, '/')>0)then
                  validate_filename=.false.
          else
                  validate_filename=.true.
          endif
    end

end




