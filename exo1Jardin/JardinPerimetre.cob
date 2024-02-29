           IDENTIFICATION DIVISION.
           PROGRAM-ID. CalculePerimetre.
           ENVIRONMENT DIVISION.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 longueur PIC 9(10) VALUE ZERO.
           01 largeur PIC 9(10) VALUE ZERO.
           01 resultat PIC 9(10) VALUE ZERO.
           01 perimetre PIC 9(10) VALUE ZERO.


           PROCEDURE DIVISION.
           DISPLAY "Entrez une longueur: ".
           ACCEPT longueur.
           DISPLAY "Entrez une largeur: ".
           ACCEPT largeur.
           ADD longueur TO largeur GIVING resultat.
           MULTIPLY resultat BY 2 GIVING perimetre. 
           DISPLAY "Le périmètre est de: "perimetre.

               


               *> commentaire

           