           IDENTIFICATION DIVISION.
           PROGRAM-ID. TAKEHOME_PAY.
           
           DATA DIVISION.
               WORKING-STORAGE SECTION.
               01 FINYEAR PIC X(9) VALUE "2021/2022".
               01 REGION PIC X(3) VALUE "AU".
               01 ANNUAL_PAY PIC 9(9).
               01 HECS PIC X(1).
               01 MCARE_LEVY PIC V(8).
               01 SUB_TOTAL PIC X(9).
               01 TAX_TOTAL PIC X9(9) VALUE 0.
               
               
           PROCEDURE DIVISION.
               DISPLAY "TAKEHOME PAY CALCULATOR".
               DISPLAY "REGION: "REGION.
               DISPLAY "YEAR: "FINYEAR.
               DISPLAY "ENTER ANNUAL PAY:".
               READ ANNUAL_PAY.
               DISPLAY "ENTER HECS STATUS:".
               DISPLAY "('T' FOR HAS, 'F' FOR DOES NOT HAVE)".
               READ HECS.
               IF ANNUAL_PAY IS > 18201 AND ANNUAL_PAY IS < 45000 THEN
                   TAX_TOTAL = TAX_TOTAL + ( ( ANNUAL_PAY - 18201 )
      -                * .019 )
               IF ANNUAL_PAY IS > 45001 AND ANNUAL_PAY IS < 120000 THEN
                   TAX_TOTAL = 5092 + ( ( ANNUAL_PAY - 45001 )
      -                * .0325 )
                   ELSE
                       TAX_TOTAL = TAX_TOTAL + 3900.
               IF ANNUAL_PAY IS > 120001 AND ANNUAL_PAY IS < 180000 THEN
                   TAX_TOTAL = 29467 + ( ( ANNUAL_PAY - 120001 )
      -                * .037 )
               IF ANNUAL_PAY IS > 180001 THEN
                   TAX_TOTAL = 51667 + ( ( ANNUAL_PAY - 180001 )
      -                * .045 )    
               DISPLAY "YOUR TAX TOTAL IS "TAX_TOTAL.
           END PROGRAM TAKEHOME_PAY.