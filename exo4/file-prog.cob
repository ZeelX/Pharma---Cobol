       IDENTIFICATION DIVISION.
       PROGRAM-ID. SubProgram.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT soldes_file ASSIGN TO 'sold.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ws-file-status.


           SELECT historique_file ASSIGN TO 'historique.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ws-file-status.
       DATA DIVISION.
       FILE SECTION.
       FD soldes_file.
       01 soldes.
            05 Soldecompte1 PIC  9(5)V99.
            05 MYFILLER  PIC X VALUE ":".
            05 Soldecompte2 PIC  9(5)V99.

       FD historique_file.
       01 historique.
            05 H-Action PIC X.
            05 CSV-FILLER PIC X VALUE ",".
            05 H-Montant PIC 9(5)V99.
       
       WORKING-STORAGE SECTION.
            01 ws-file-status PIC XX.

       
       LINKAGE SECTION.
          

        01 rec-choice PIC 9.
        
        01 ls_soldes_record.
           05 ls_Soldecompte1 PIC  9(5)V99.
           05 ls_MYFILLER  PIC X VALUE ":".
           05 ls_Soldecompte2 PIC  9(5)V99.

        01 ls_historique_record.
           05 ls_H-Action PIC X.
           05 ls_CSV-FILLER PIC X VALUE ",".
           05 ls_H-Montant PIC 9(5)V99. 

       PROCEDURE DIVISION USING  rec-choice, ls_historique_record, ls_soldes_record.

           IF rec-choice = 1 THEN
               MOVE ls_H-Action TO H-Action OF historique
               MOVE ls_H-Montant TO H-Montant OF historique
               PERFORM ENREGISTRER-HISTORIQUE
           ELSE IF rec-choice = 2 THEN 
               MOVE ls_Soldecompte1  TO Soldecompte1 OF soldes 
               MOVE ls_MYFILLER TO MYFILLER OF soldes 
               MOVE ls_Soldecompte2 TO Soldecompte2 OF soldes 
               PERFORM ENREGISTRER-SOLD
           END-IF.

           STOP RUN.



       ENREGISTRER-HISTORIQUE.
           OPEN INPUT historique_file
           IF ws-file-status = '35'
               CLOSE historique_file
               OPEN OUTPUT historique_file
           END-IF.
           WRITE historique
           CLOSE historique_file.

       ENREGISTRER-SOLD.
           OPEN INPUT soldes_file
           IF ws-file-status = '35'
               CLOSE soldes_file
               OPEN OUTPUT soldes_file
           END-IF.
           WRITE soldes
           CLOSE soldes_file.

       END PROGRAM SubProgram.
