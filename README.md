# FUNDLY

      *We have an Identification division
      *And a program ID, which is the name of our app "FUNDLY"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNDLY.

      *We used working storage device. 
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01 MAX-RECORDS            PIC 99 VALUE 99.     
         01 EXP-RECORDS            PIC 99 VALUE 0.
         01 ITEM-NUMBER   OCCURS 1 TO 99 TIMES DEPENDING ON MAX-RECORDS.
               05 ITEM-NO          PIC X(3).
         01 DESC          OCCURS 1 TO 99 TIMES DEPENDING ON MAX-RECORDS.                  
               05 DESC-NAME        PIC X(30).
         01 AMOUNT        OCCURS 1 TO 99 TIMES DEPENDING ON MAX-RECORDS.
               05 EXP-AMOUNT       PIC 9(5).
         01 PERSON-NUMBER OCCURS 1 TO 99 TIMES DEPENDING ON MAX-RECORDS.
               05 PHONE-NUMBER     PIC X(15).
         01 USER-NAME              PIC X(20).
         01 ITEMS                  PIC X(3).
         01 NUM                    PIC 99 VALUE 0.
         01 X                      PIC 99 VALUE 0.
         01 CHOICE                 PIC 99.

      *In procedure division occurs the process of our app.
       PROCEDURE DIVISION.
       
      *First is the display "WELCOME TO FUNDLY!!"
      *So that it appear in the console as a welcome to the user. 
           DISPLAY "______________________"
           DISPLAY " WELCOME TO FUNDLY!!"
           DISPLAY "______________________"

           DISPLAY "Enter you name: " WITH NO ADVANCING
           ACCEPT USER-NAME.
      
           PERFORM MAIN-MENU
      
      *STOP the program.
           STOP RUN.

      *After the perform main-menu, the program will execute 
      *the MAIN-MENU paragraph.
       MAIN-MENU.
           DISPLAY "______________________"
           DISPLAY "    Hi " USER-NAME
           DISPLAY "You can choose a function below:"

           PERFORM UNTIL CHOICE = 5
              DISPLAY "______________________"
              DISPLAY "   Main Menu:"
              DISPLAY " 1. Add a record"
              DISPLAY " 2. Remove a record"
              DISPLAY " 3. See all records"
              DISPLAY " 4. Search a record"
              DISPLAY " 5. Exit"
              DISPLAY " Enter your choice (1-5) : " WITH NO ADVANCING
              ACCEPT CHOICE
              EVALUATE CHOICE
                  WHEN 1
                      PERFORM ADD-RECORD
                  WHEN 2
                      PERFORM REMOVE-RECORD
                  WHEN 3
                      PERFORM SEE-ALL-RECORDS
                  WHEN 4
                      PERFORM SEARCH-RECORD
                  WHEN 5
                      DISPLAY " "
                      DISPLAY "== Thank you for using FUNDLY!! =="
                      STOP RUN
                  WHEN OTHER
                       DISPLAY " Invalid choice."
                       DISPLAY " Please select a valid option (1-5)."
              END-EVALUATE
           END-PERFORM.

      *PERFORM THE ADD-RECORD
      *THIS PROGRAM IS FOR ADDING AN EXPENSES
       ADD-RECORD.
           IF EXP-RECORDS < MAX-RECORDS
              ADD 1 TO EXP-RECORDS
              DISPLAY " "
              DISPLAY " Enter details to add a record:"
              DISPLAY " Item no. " WITH NO ADVANCING
              ACCEPT ITEM-NUMBER(EXP-RECORDS)
              DISPLAY " Expenses Name: " WITH NO ADVANCING
              ACCEPT DESC(EXP-RECORDS)
              DISPLAY " Amount: " WITH NO ADVANCING
              ACCEPT EXP-AMOUNT(EXP-RECORDS)

              DISPLAY " "
              DISPLAY "==Expenses added successfully.=="
           ELSE
              DISPLAY "==No space available to add more expenses.=="
           END-IF.

      *PERFORM REMOVE-RECORD
      *THIS PROGRAM IS FOR REMOVING AN EXPENSES IN THE LIST.
       REMOVE-RECORD.
           DISPLAY " "
           DISPLAY " Enter ID to remove record: " WITH NO ADVANCING
           ACCEPT ITEMS
           PERFORM VARYING NUM FROM 1 BY 1 UNTIL NUM > EXP-RECORDS
              IF ITEMS = ITEM-NUMBER(NUM)
                  MOVE SPACES TO ITEM-NUMBER(NUM)
                  MOVE SPACES TO DESC(NUM)
                  MOVE 0 TO EXP-AMOUNT(NUM)
                  PERFORM VARYING X FROM NUM BY 1 UNTIL X = EXP-RECORDS
                      MOVE ITEM-NUMBER(X + 1) TO ITEM-NUMBER(X)
                      MOVE DESC(X + 1) TO DESC(X)
                      MOVE EXP-AMOUNT(X + 1) TO EXP-AMOUNT(X)
                  END-PERFORM
                  SUBTRACT 1 FROM EXP-RECORDS
                  DISPLAY " "
                  DISPLAY " Expenses removed."
                  EXIT PERFORM
              ELSE
                  DISPLAY " "
                  DISPLAY " Expenses not found."
              END-IF 
           END-PERFORM.

      *PERFORM SEE-ALL-RECORDS
      *THIS PROGRAM IS FOR DISPLAYING ALL THE EXPENSES.
       SEE-ALL-RECORDS.
         DISPLAY " "
         DISPLAY USER-NAME
         DISPLAY "ITEMS | EXPENSES                      | AMOUNT"
         PERFORM VARYING NUM FROM 1 BY 1 UNTIL NUM > EXP-RECORDS
         DISPLAY "  " ITEM-NUMBER(NUM) " | "DESC(NUM)"| "EXP-AMOUNT(NUM)
         END-PERFORM.

      *PERFORM SEARCH-RECORD
      *THIS PROGRAM IS FOR SEARCHING AN EXPENSES IN THE LIST.
       SEARCH-RECORD.
       DISPLAY " "
       DISPLAY" Enter the Item of expense to search: " WITH NO ADVANCING
       ACCEPT ITEMS

           PERFORM VARYING NUM FROM 1 BY 1 UNTIL NUM > EXP-RECORDS
              IF ITEMS = ITEM-NUMBER(NUM)
                  DISPLAY " "
                  DISPLAY " Expenses found:"
                  DISPLAY "  " ITEM-NUMBER(NUM) " | " DESC(NUM) " | " 
                  EXP-AMOUNT(NUM)
                  EXIT PERFORM
               ELSE
                  DISPLAY " "
                  DISPLAY " Expenses not found."
               END-IF
           END-PERFORM.
