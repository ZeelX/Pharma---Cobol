           IDENTIFICATION DIVISION.
           PROGRAM-ID. Librairy.
           ENVIRONMENT DIVISION.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 Books.
               02 Book occurs 5 TIMES INDEXED BY Idx.
                   03 book-title PIC X(20).
                   03 book-autor PIC X(20).
                   03 book-year PIC 9(4).
                   03 nb-copy PIC 9(3).

           PROCEDURE DIVISION.
            MOVE "1984" TO book-title OF Book (1).
            MOVE "George Orwell" TO book-autor OF  Book (1).
            MOVE 1949 TO book-year OF Book (1).
            MOVE 5 TO nb-copy OF  Book (1).

            MOVE "Le Petit Prince" TO book-title OF Book (2).
            MOVE "Antoine de Saint-Exupéry" TO book-autor OF  Book (2).
            MOVE 1943 TO book-year OF Book (2).
            MOVE 3 TO nb-copy OF  Book (2).

            MOVE "Le Seigneurs des Moineaux" TO book-title OF Book (3).
            MOVE "J.R.R Tolkien" TO book-autor OF  Book (3).
            MOVE 1954 TO book-year OF Book (3).
            MOVE 7 TO nb-copy OF  Book (3).

            MOVE "Fondation" TO book-title OF Book (4).
            MOVE "Isaac Asimov" TO book-autor OF  Book (4).
            MOVE 1951 TO book-year OF Book (4).
            MOVE 4 TO nb-copy OF  Book (4).

            MOVE "Dune" TO book-title OF Book (5).
            MOVE "Frank Herbert" TO book-autor OF  Book (5).
            MOVE 1965 TO book-year OF Book (5).
            MOVE 6 TO nb-copy OF  Book (5).
      
      
      
             PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 5
                 DISPLAY "Book title : " book-title (Idx)       
                 DISPLAY "Autor : " book-autor (Idx)       
                 DISPLAY "Publication Year : " book-year (Idx)       
                 DISPLAY "Number of available copy : " nb-copy (Idx) 
             END-PERFORM.

      

               *> commentaire

           