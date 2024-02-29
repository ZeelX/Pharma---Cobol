IDENTIFICATION DIVISION.
PROGRAM-ID. Librairy.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
    01 nb_times PIC 9 VALUE ZERO.
        01 Books.
            02 Book OCCURS 100 TIMES INDEXED BY Idx.
                03 book-title PIC X(20).
                03 book-autor PIC X(20).
                03 book-year PIC 9(4).
                03 nb-copy PIC 9(3).

PROCEDURE DIVISION.
 DISPLAY "How many book(s) do you want to add ?".
 ACCEPT  nb_times.
    PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > nb_times
        DISPLAY "Book title? "
        ACCEPT book-title (Idx)
        DISPLAY "Autor? "
        ACCEPT book-autor (Idx)
        DISPLAY "Publication Year? "
        ACCEPT book-year (Idx)
        DISPLAY "Number of available? "
        ACCEPT nb-copy(Idx) 
    END-PERFORM.

STOP RUN.
