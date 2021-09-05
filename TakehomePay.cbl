           IDENTIFICATION DIVISION.
           PROGRAM-ID. TAKEHOME_PAY.

           ENVIRONMENT DIVISION.
               CONFIGURATION SECTION.
               SPECIAL-NAMES.
                  CONSOLE IS DATA-INPUT.
           
           DATA DIVISION.
               WORKING-STORAGE SECTION.
               01 FINYEAR PIC X(9) VALUE "2021/2022".
               01 REGION PIC X(3) VALUE "AU".
               01 ANNUAL_PAY PIC 9(9).
               01 HECS PIC X(1).
               01 SUB_TOTAL PIC 9(9).
               01 TAX_TOTAL PIC 9(9) VALUE 0.
               01 TAKEHOME_PAY_ANNUAL PIC 9(9).
               01 TAKEHOME_PAY_WEEKLY PIC 9(9).

           PROCEDURE DIVISION.
               DISPLAY "TAKEHOME PAY CALCULATOR".
               DISPLAY "REGION: "REGION.
               DISPLAY "YEAR: "FINYEAR.
               DISPLAY "SINGLE HOUSEHOLD".
               DISPLAY "ENTER ANNUAL PAY:".
               ACCEPT ANNUAL_PAY FROM DATA-INPUT.
               DISPLAY "ENTER HECS STATUS:".
               DISPLAY "('T' FOR HAS, 'F' FOR DOES NOT HAVE)".
               ACCEPT HECS FROM DATA-INPUT.
      *        INCOME TAX CALCULATION
               IF ANNUAL_PAY IS > 18201 AND ANNUAL_PAY IS < 45000 THEN
                   COMPUTE TAX_TOTAL = ( ANNUAL_PAY - 18201 ) * .019.
               IF ANNUAL_PAY IS > 45001 AND ANNUAL_PAY IS < 120000 THEN
                   COMPUTE TAX_TOTAL = 5092 + ( ( ANNUAL_PAY - 45001 )
      -                * .0325 ).
               IF ANNUAL_PAY IS > 120001 AND ANNUAL_PAY IS < 180000 THEN
                   COMPUTE TAX_TOTAL = 29467 + ( ( ANNUAL_PAY - 120001 )
      -                * .037 ).
               IF ANNUAL_PAY IS > 180001 THEN
                   COMPUTE TAX_TOTAL = 51667 + ( ( ANNUAL_PAY - 180001 )
      -                * .045 ).
      *        MEDICARE CALCULATION
               IF ANNUAL_PAY IS >= 90000 THEN
                     COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .002
      -              ).
               IF HECS IS EQUAL TO 'T' AND ANNUAL_PAY IS > 47014 THEN
                   IF ANNUAL_PAY > 47014 AND < 54282 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                01).
                   IF ANNUAL_PAY > 54283 AND < 57538 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                02).
                   IF ANNUAL_PAY > 57539 AND < 60991 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                025).
                   IF ANNUAL_PAY > 60992 AND < 64651 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                03).
                   IF ANNUAL_PAY > 64652 AND < 68529 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                035).
                   IF ANNUAL_PAY > 68530 AND < 72641 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                04).
                   IF ANNUAL_PAY > 72642 AND < 77001 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                045).
                   IF ANNUAL_PAY > 77002 AND < 81620 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                05).
                   IF ANNUAL_PAY > 81621 AND < 86518 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                055).
                  IF ANNUAL_PAY > 86519 AND < 91709 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                06).
                   IF ANNUAL_PAY > 91710 AND < 97212 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                065).
                   IF ANNUAL_PAY > 97213 AND < 103045 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                07).
                   IF ANNUAL_PAY > 103046 AND < 109227 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                075).
                   IF ANNUAL_PAY > 109228 AND < 115781 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                08).
                   IF ANNUAL_PAY > 115782 AND < 122728 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                085).
                   IF ANNUAL_PAY > 122729 AND < 130092 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                09).
                   IF ANNUAL_PAY > 130093 AND < 137897 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                095).
                   IF ANNUAL_PAY > 137898 THEN
                       COMPUTE TAX_TOTAL = TAX_TOTAL + ( ANNUAL_PAY * .0
      -                1).
               COMPUTE TAKEHOME_PAY_ANNUAL = ANNUAL_PAY - TAX_TOTAL.
               COMPUTE TAKEHOME_PAY_ANNUAL = TAKEHOME_PAY_ANNUAL / 52.

               DISPLAY "YOUR TAX TOTAL IS "TAX_TOTAL.
               DISPLAY "YOUR ANNUAL TAKEHOME PAY IS "TAKEHOME_PAY_ANNUAL
      -        .         
               DISPLAY "YOUR WEEKLY TAKEHOME PAY IS "TAKEHOME_PAY_WEEKLY
      -        .
           END PROGRAM TAKEHOME_PAY.