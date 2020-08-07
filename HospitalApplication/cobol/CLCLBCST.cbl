       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID.  CLCLBCST.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  MISC-FIELDS.
           05 TEMP-COST                   PIC S9(9)V99 COMP-3.

       LINKAGE SECTION.
       01  CALC-COSTS-REC.
           05  CALC-TYPE-SW               PIC X.
               88 LAB-TEST VALUE "L".
               88 EQUIPMENT VALUE "E".
           05  PATIENT-ID                 PIC X(8).
           05  LAB-TEST-ID                PIC X(8).
           05  PATIENT-DEDUCTIBLE-REM     PIC 9(4) COMP.
           05  PATIENT-COPAY              PIC 9(3) COMP-3.
           05  REIMBURSE-PCT              PIC 9(3) COMP-3.
           05  PROCEDURE-BASE-COST        PIC 9(7)V99 COMP-3.
           05  ANCILLARY-COSTS            PIC 9(5)V99 COMP-3.
           05  VENIPUNCTURE-COSTS         PIC 9(5)V99 COMP-3.
           05  NET-PATIENT-COSTS          PIC 9(7)V99 COMP-3.
           05  EQUIPMENT-COSTS            PIC 9(7)V99 COMP-3.

       01  RETURN-CD                      PIC 9(4) COMP.

       PROCEDURE DIVISION USING CALC-COSTS-REC, RETURN-CD.
           IF LAB-TEST
               PERFORM 100-CALC-LAB-COSTS
           ELSE IF EQUIPMENT                                            020497MM
      *** ADDED ENHANEMENT TO HANDLE EQUIPMENT CHARGES
               PERFORM 200-CALC-EQUIP-COSTS.

           MOVE ZERO TO RETURN-CD.
           GOBACK.


       100-CALC-LAB-COSTS.
      **  Remove for 0CB bug introduced for ABEND condition
           IF PATIENT-COPAY = ZERO
                  MOVE +1 TO PATIENT-COPAY.
           COMPUTE TEMP-COST =
                (  VENIPUNCTURE-COSTS + ANCILLARY-COSTS +
                 ( PROCEDURE-BASE-COST * 2.2 ) )
                 * (REIMBURSE-PCT / PATIENT-COPAY  ).

           SUBTRACT PATIENT-DEDUCTIBLE-REM FROM TEMP-COST GIVING
               NET-PATIENT-COSTS.

       200-CALC-EQUIP-COSTS.
      **  Remove for 0CB bug introduced for ABEND condition
           IF PATIENT-COPAY = ZERO
                  MOVE +1 TO PATIENT-COPAY.
           COMPUTE TEMP-COST =
                (  VENIPUNCTURE-COSTS + ANCILLARY-COSTS +
                 ( EQUIPMENT-COSTS * 1.28 ) )
                 * (REIMBURSE-PCT / PATIENT-COPAY ).

           SUBTRACT PATIENT-DEDUCTIBLE-REM FROM TEMP-COST GIVING
               NET-PATIENT-COSTS.