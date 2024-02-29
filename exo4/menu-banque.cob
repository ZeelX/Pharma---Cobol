           IDENTIFICATION DIVISION.
           PROGRAM-ID. menu-banque.
           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
                SELECT historique ASSIGN TO "historique.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
           
                SELECT sold ASSIGN TO "sold.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
           DATA DIVISION.
           FILE SECTION.
            FD historique.
            01 historique-record.
                05 h_description PIC X(50).
                05 h_value PIC 9(10)V99.
            FD sold.
            01 sold-record.
                05 s_value PIC 9(10)V99.
           WORKING-STORAGE SECTION.
           01 account-sold PIC 9(10)V99 VALUE 200.
           01 account-sold_second PIC 9(10)V99 VALUE 1000.
           01 account-depot PIC 9(4)V99 VALUE ZERO.
           01 account-withdraw PIC 9(4)V99 VALUE ZERO.
           01 account-virement PIC 9(4)V99 VALUE ZERO.
           01 choice PIC 9 VALUE 0.
           PROCEDURE DIVISION.

           OPEN OUTPUT historique.

           PERFORM MAIN-PARA.
           CLOSE historique.
           STOP RUN.


           Depot-Para.
                 DISPLAY "Enter your deposite"
                 ACCEPT account-depot
                 COMPUTE account-sold = account-depot + account-sold
                 DISPLAY "Account sold: ", account-sold
                 MOVE "depot" TO h_description
                 MOVE account-depot TO h_value
                 MOVE account-sold TO s_value
                 WRITE historique-record
                 
                 OPEN OUTPUT sold
                 MOVE account-sold TO s_value
                 WRITE sold-record
                 END-WRITE
                 CLOSE sold.
                
               
               
           Withdraw-Para.
                 DISPLAY "Enter your withdraw"
                 ACCEPT account-withdraw
                 COMPUTE account-sold = account-sold - account-withdraw
                 DISPLAY "Account sold: ", account-sold
                 MOVE "withdraw" TO h_description
                 MOVE account-withdraw TO h_value
                 WRITE historique-record
                 OPEN OUTPUT sold
                 MOVE account-sold TO s_value
                 WRITE sold-record
                 END-WRITE
                 CLOSE sold.
                 

               
           Virement-Para.
                 DISPLAY "How many do you want to transfer ?"
                 ACCEPT account-virement
                 COMPUTE account-sold = account-sold - account-virement
                 COMPUTE account-sold_second = account-virement + account-sold_second.
                 MOVE "virement" TO h_description
                 MOVE account-virement TO h_value
                 WRITE historique-record
                 OPEN OUTPUT sold
                 MOVE account-sold TO s_value
                 WRITE sold-record
                 END-WRITE
                 CLOSE sold.
               
        
           Show-Para.
                  DISPLAY "Your account is currently at ", account-sold. 
            
            
            MAIN-PARA.
                PERFORM UNTIL choice = 5
                         
                    DISPLAY "Menu options"
                    DISPLAY "1: Deposite"
                    DISPLAY "2: Withdraw"
                    DISPLAY "3: Virement"
                    DISPLAY "4: Show"
                    DISPLAY "5: Quit"

                    DISPLAY "Choose an option (1-5): "
                    ACCEPT choice
         
                    EVALUATE choice
                        WHEN 1 
                            PERFORM Depot-Para
                        WHEN 2
                            PERFORM Withdraw-Para
                        WHEN 3
                            PERFORM Virement-Para
                        WHEN 4
                            PERFORM Show-Para
                        WHEN 5
                            DISPLAY "Good Bye!"
                        WHEN OTHER
                            DISPLAY "Invalide choice"
                    END-EVALUATE
                END-PERFORM.

           