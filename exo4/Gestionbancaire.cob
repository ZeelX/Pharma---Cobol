       IDENTIFICATION DIVISION.
       PROGRAM-ID. GestionCompteBancaire.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       

       WORKING-STORAGE SECTION.
       01 ws-file-status PIC XX.
       01 SoldeCompte PIC 9(5)V99 VALUE 1000.00.
       01 MontantDepot PIC 9(5)V99. 
       01 MontantVirement PIC 9(5)V99.
       01 MontantRetrait PIC 9(5)V99.
       01 SoldeCompteSecond PIC 9(5)V99 VALUE 500.00.
       01 Choix PIC 9 VALUE ZERO.

       01 MenuOptions PIC X(250).
       01  saut-de-ligne           PIC X VALUE X'0A'.
       

       01 Transaction.
           02 Montant PIC 9(5)V99.
           02 Action PIC X(20).

       01 soldes-record.
           05 Soldecompte1 PIC  9(5)V99.
           05 MYFILLER  PIC X VALUE ":".
           05 Soldecompte2 PIC  9(5)V99.

       01 historique-record.
           05 H-Action PIC X.
           05 CSV-FILLER PIC X VALUE ",".
           05 H-Montant PIC 9(5)V99. 

       01 rec_choice PIC  9.

       77 F-Depot PIC X VALUE "D".
       77 F-Retrait PIC X VALUE "R".
       77 F-Virement PIC X VALUE "V".

       PROCEDURE DIVISION.
       STRING "**********MENU**********", saut-de-ligne
        "1. Afficher Solde", saut-de-ligne
        "2. Faire un dépôt", saut-de-ligne
        "3. Faire un retrait", saut-de-ligne
        "4. Faire un virement", saut-de-ligne
        "5. Quitter"
           INTO MenuOptions.




       TRAITEMENT-PRINCIPAL.
           PERFORM MENU-OPERATIONS.



      
       DEPOT.
           DISPLAY "Donnez le montant de votre dépôt"
           ACCEPT MontantDepot.
           COMPUTE SoldeCompte = SoldeCompte + MontantDepot.
           MOVE MontantDepot TO H-Montant.
           MOVE F-DEPOT TO H-Action.
           MOVE SoldeCompte TO Soldecompte1
           MOVE 1 TO rec_choice
           DISPLAY historique-record
           CALL 'subprogram' USING rec_choice, historique-record.
           MOVE 2 TO rec_choice
           CALL 'subprogram' USING rec_choice, historique-record, soldes-record .
          

       

       RETRAIT.
           DISPLAY "Donnez le montant de votre retrait"
           ACCEPT MontantRetrait
           IF SoldeCompte >= MontantRetrait  THEN
               COMPUTE SoldeCompte = SoldeCompte - MontantDepot
               MOVE MontantRetrait TO H-Montant
               MOVE F-Retrait TO H-Action
               MOVE SoldeCompte TO Soldecompte1

               MOVE 1 TO rec_choice
               CALL 'subprogram' USING rec_choice, historique-record, 
               MOVE 2 TO rec_choice
               CALL 'subprogram' USING rec_choice, historique-record, soldes-record 
           ELSE 
               DISPLAY "Erreur: Solde Insuffisant"
           END-IF.


       VIREMENT.
           DISPLAY "Donnez le montant de votre virement "
           ACCEPT MontantVirement.
           IF SoldeCompte >= MontantVirement THEN
               COMPUTE SoldeCompte = SoldeCompte - MontantVirement
               COMPUTE SoldeCompteSecond = SoldeCompteSecond + MontantVirement
               MOVE MontantVirement TO H-Montant
               MOVE  F-Virement TO H-ACTION
               MOVE SoldeCompte TO Soldecompte1
               MOVE SoldeCompteSecond TO Soldecompte2
               
               MOVE 1 TO rec_choice
               CALL 'subprogram' USING rec_choice, historique-record 
               MOVE 2 TO rec_choice
               CALL 'subprogram' USING rec_choice, historique-record, soldes-record 
           ELSE 
               DISPLAY "Erreur: Solde insuffisant"
           END-IF.


       AFFICHER-SOLDE.
           DISPLAY SoldeCompte.
           DISPLAY SoldeCompteSecond.


       MENU-OPERATIONS.
       DISPLAY MenuOptions

                           

       ACCEPT Choix.
       EVALUATE Choix
           WHEN 1 PERFORM AFFICHER-SOLDE
           WHEN 2 PERFORM DEPOT
           WHEN 3 PERFORM RETRAIT
           WHEN 4 PERFORM VIREMENT
           WHEN 5 DISPLAY "Good bye !"
           WHEN OTHER
               DISPLAY "Choix Invalide."
       END-EVALUATE
       PERFORM MENU-OPERATIONS.

