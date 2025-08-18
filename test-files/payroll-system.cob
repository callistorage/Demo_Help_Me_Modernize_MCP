      * SAMPLE GOVERNMENT PAYROLL SYSTEM
      * PROGRAM: PAYROLL-CALC
      * AUTHOR: GOVERNMENT DEVELOPER
      * DATE: 1985 (LAST MODIFIED: 1997)
      * PURPOSE: CALCULATE EMPLOYEE PAYROLL AND BENEFITS
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-CALC.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPDATA.DAT'
               ORGANIZATION IS SEQUENTIAL.
           SELECT PAYROLL-REPORT ASSIGN TO 'PAYROLL.RPT'
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05  EMP-ID              PIC 9(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-GRADE           PIC 99.
           05  EMP-STEP            PIC 99.
           05  EMP-HOURS-WORKED    PIC 999V99.
           05  EMP-OVERTIME-HOURS  PIC 999V99.
           05  EMP-STATUS          PIC X.
               88  ACTIVE-EMPLOYEE VALUE 'A'.
               88  RETIRED-EMPLOYEE VALUE 'R'.
               88  TERMINATED-EMPLOYEE VALUE 'T'.
       
       FD  PAYROLL-REPORT.
       01  PAYROLL-LINE            PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-COUNTERS.
           05  WS-EMPLOYEE-COUNT   PIC 9(4) VALUE 0.
           05  WS-TOTAL-PAYROLL    PIC 9(8)V99 VALUE 0.
       
       01  WS-PAY-CALCULATION.
           05  WS-BASE-PAY         PIC 9(6)V99.
           05  WS-OVERTIME-PAY     PIC 9(5)V99.
           05  WS-GROSS-PAY        PIC 9(7)V99.
           05  WS-FEDERAL-TAX      PIC 9(6)V99.
           05  WS-STATE-TAX        PIC 9(5)V99.
           05  WS-FICA-TAX         PIC 9(5)V99.
           05  WS-NET-PAY          PIC 9(7)V99.
       
       01  WS-GRADE-PAY-TABLE.
           05  FILLER              PIC X(10) VALUE 'GS-05$28000'.
           05  FILLER              PIC X(10) VALUE 'GS-07$32000'.
           05  FILLER              PIC X(10) VALUE 'GS-09$38000'.
           05  FILLER              PIC X(10) VALUE 'GS-11$45000'.
           05  FILLER              PIC X(10) VALUE 'GS-12$54000'.
           05  FILLER              PIC X(10) VALUE 'GS-13$64000'.
           05  FILLER              PIC X(10) VALUE 'GS-14$76000'.
           05  FILLER              PIC X(10) VALUE 'GS-15$89000'.
       
       01  WS-GRADE-TABLE REDEFINES WS-GRADE-PAY-TABLE.
           05  WS-GRADE-ENTRY OCCURS 8 TIMES.
               10  WS-GRADE-CODE   PIC X(5).
               10  WS-ANNUAL-PAY   PIC 9(5).
       
       PROCEDURE DIVISION.
       MAIN-PROCESSING.
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT PAYROLL-REPORT
           
           PERFORM PROCESS-EMPLOYEES UNTIL END-OF-FILE
           
           PERFORM PRINT-SUMMARY
           
           CLOSE EMPLOYEE-FILE
           CLOSE PAYROLL-REPORT
           
           STOP RUN.
       
       PROCESS-EMPLOYEES.
           READ EMPLOYEE-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END PERFORM CALCULATE-PAY
           END-READ.
       
       CALCULATE-PAY.
           IF ACTIVE-EMPLOYEE
               PERFORM GET-BASE-PAY
               PERFORM CALCULATE-OVERTIME
               PERFORM CALCULATE-TAXES
               PERFORM CALCULATE-NET-PAY
               PERFORM PRINT-PAY-STUB
               ADD 1 TO WS-EMPLOYEE-COUNT
               ADD WS-GROSS-PAY TO WS-TOTAL-PAYROLL
           END-IF.
       
       GET-BASE-PAY.
      * LOOKUP GRADE IN PAY TABLE - LEGACY HARDCODED LOGIC
           EVALUATE EMP-GRADE
               WHEN 05 MOVE 28000 TO WS-ANNUAL-PAY
               WHEN 07 MOVE 32000 TO WS-ANNUAL-PAY  
               WHEN 09 MOVE 38000 TO WS-ANNUAL-PAY
               WHEN 11 MOVE 45000 TO WS-ANNUAL-PAY
               WHEN 12 MOVE 54000 TO WS-ANNUAL-PAY
               WHEN 13 MOVE 64000 TO WS-ANNUAL-PAY
               WHEN 14 MOVE 76000 TO WS-ANNUAL-PAY
               WHEN 15 MOVE 89000 TO WS-ANNUAL-PAY
               WHEN OTHER MOVE 25000 TO WS-ANNUAL-PAY
           END-EVALUATE
           
           COMPUTE WS-BASE-PAY = WS-ANNUAL-PAY / 2080 * EMP-HOURS-WORKED.
       
       CALCULATE-OVERTIME.
      * OVERTIME CALCULATION - COMPLEX GOVERNMENT RULES
           IF EMP-HOURS-WORKED > 40
               COMPUTE WS-OVERTIME-PAY = 
                   (WS-ANNUAL-PAY / 2080) * 1.5 * EMP-OVERTIME-HOURS
           ELSE
               MOVE 0 TO WS-OVERTIME-PAY
           END-IF.
           
           ADD WS-BASE-PAY TO WS-OVERTIME-PAY GIVING WS-GROSS-PAY.
       
       CALCULATE-TAXES.
      * TAX CALCULATION - OUTDATED TAX BRACKETS FROM 1985
           COMPUTE WS-FEDERAL-TAX = WS-GROSS-PAY * 0.28
           COMPUTE WS-STATE-TAX = WS-GROSS-PAY * 0.05
           COMPUTE WS-FICA-TAX = WS-GROSS-PAY * 0.0765.
       
       CALCULATE-NET-PAY.
           COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-FEDERAL-TAX
                               - WS-STATE-TAX - WS-FICA-TAX.
       
       PRINT-PAY-STUB.
           MOVE SPACES TO PAYROLL-LINE
           STRING EMP-ID DELIMITED BY SIZE
                  ' ' DELIMITED BY SIZE
                  EMP-NAME DELIMITED BY SIZE
                  ' GROSS: $' DELIMITED BY SIZE
                  WS-GROSS-PAY DELIMITED BY SIZE
                  ' NET: $' DELIMITED BY SIZE
                  WS-NET-PAY DELIMITED BY SIZE
                  INTO PAYROLL-LINE
           END-STRING
           WRITE PAYROLL-LINE.
       
       PRINT-SUMMARY.
           MOVE SPACES TO PAYROLL-LINE
           STRING 'TOTAL EMPLOYEES: ' DELIMITED BY SIZE
                  WS-EMPLOYEE-COUNT DELIMITED BY SIZE
                  ' TOTAL PAYROLL: $' DELIMITED BY SIZE
                  WS-TOTAL-PAYROLL DELIMITED BY SIZE
                  INTO PAYROLL-LINE
           END-STRING
           WRITE PAYROLL-LINE.
