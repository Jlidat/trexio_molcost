program trexio2dolo 
      use trexio


        implicit none
        integer :: user_input
        character(len=100) ::input_filename


        !demander une entrée numérique et la valider
        print*, "Entrez un nombre entier entre 1 et 100 :"
        read*, user_input

        !validation de la plage
        if(user_input<1.or.user_input>100)then
                print*,"Entrée invalide! le nombre doit être entre 1 et 100"
                stop
        endif

        !demander le nom du fichier d'entrée
        print*, "Donnez le nom du fichier d'entrée :"
        read*, input_filename
        
        !Valider le nom de fichier d'entrée pour éviter les attaques par injection de chemin
        if(.not.validate_filename(input_filename)) then
                print*,"Erreur : Nom de fichier invalide."
                call exit(-1)
        endif



        call t_Info(input_filename)
        call t_Inporb(input_filename)
        call t_Mono(input_filename)

contains
        !Fonction de validation du nom de fichier pour éviter les injections de chemins
        logical function validate_filename(filename)
                character(len=*) :: filename
               ! logical :: validate_filename

                !Vérification que le fichier ne contient pas de caractère dangereux comme ../ ou /
                if(index(filename, '../')>0.or.index(filename, '/')>0)then
                        validate_filename=.false.
                else
                        validate_filename=.true.
                endif
        end

end




