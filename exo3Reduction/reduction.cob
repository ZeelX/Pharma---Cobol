     IDENTIFICATION DIVISION.
           PROGRAM-ID. reduction.
           ENVIRONMENT DIVISION.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 total PIC 9(10)V99 VALUE ZERO.
           01 applicable-reduction PIC 9(3)V99 VALUE 1.
           01 final-price PIC 9(10)V99 VALUE ZERO.
           PROCEDURE DIVISION.

           DISPLAY "Give your price".
           ACCEPT total.
           
            IF total > 500 THEN
               MOVE 0.10 TO applicable-reduction
            ELSE IF total <= 500 AND total > 100 THEN
               MOVE 0.05 TO applicable-reduction
            END-IF.

            IF applicable-reduction < 1 THEN 
                MULTIPLY total BY applicable-reduction GIVING final-price
                SUBTRACT final-price FROM total GIVING final-price
            ELSE
                MOVE total TO final-price
           
            DISPLAY "Final price is ", final-price.
            