# trexio_molcost
Reproduction du fichier H2O.info

*Ouverture et lecture du fichier h2o.h5 se fait l'appel à la focntion trexio_open. Cette fonction prend quatre arguments; 
	-le nom du fichier d'entrée le h2o.h5,
	-le mode; en lecture 'r' ou écriture 'w',
	-TREXIO_AUTO !!!
	-rc return code pour la vérification des erreurs
bloc pour le gestion d'erreur 
	TREXIO_SUCCESS renvoi la valeur 1	 si rc est différent de 1 alors
	appel à la fonction trexio_string_of_error pour récupérer le message d'erreur

//trim pour supprimer l'espace à gauche

Lecture des informations dont on a besoin:
- ao_num: num -> Nombre d'orbitales atomiques
	   ao -> Nom du groupe ou existe num

Lecture de nombre d'atomes:
- nucleus_num : num -> Nombre d'atomes
		nucleus -> le nom du groupe

Lecture de nombre de shells :

- basis_shell_num : shell_num -> le nombre de sells
		    basis -> le nom du groupe 

Lecture de shell_ang_mom, les moment angulaires
- basis_shell_ang_mom

Lecture de nucleus_index, l'indexe du noyau
- basis_nucleus_index

Lecture des noms des atomes 
- nucleus_label

Dans label_c le résultat est affiché sous forme d'une matrice d'une colonne

trim(adjustl(j_char)) est pour supprimer les espaces à gauche; trim() et à droite; adjustl(). Les deux fonctions prennent en argument une chaine de caractère.

pour convertir un entier en une chaine de caractère :write(j_char, '(I4)')c 

Lecture des noms d'atomes : label_l, est une relecture de label_c pour un affichage en une ligne

Lecture des coordonnées en cartisiennes: nucleux_coord
Lecture de l'énergie de répulsion atomique : nucleus_repulsion     	
