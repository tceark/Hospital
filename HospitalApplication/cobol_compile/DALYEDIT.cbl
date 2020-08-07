       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DALYEDIT.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/23/88.
       DATE-COMPILED. 01/23/88.
       SECURITY. NON-CONFIDENTIAL.

      ******************************************************************
      *REMARKS.
      *
      *          THIS PROGRAM EDITS A DAILY PATIENT/ROOM CHARGES FILE
      *          PRODUCED BY DATA ENTRY OPERATORS FROM CICS SCREENS
      *
      *          IT CONTAINS A SINGLE RECORD FOR EVERY IN-PATIENT IN THE
      *          HOSPITAL.
      *
      *          THE PROGRAM EDITS EACH RECORD AGAINST A NUMBER OF
      *          CRITERIA, BALANCES FINAL RECORDS-READ VERSUS A TRAILER
      *          REC, AND WRITES A "GOOD" PATIENT RECORDS OUTPUT FILE
      *
      ******************************************************************

               INPUT FILE              -   DDS0001.PATDATA

               VSAM MASTER FILE        -   DDS0001.PATMASTR

               INPUT ERROR FILE        -   DDS0001.PATERR

               OUTPUT FILE PRODUCED    -   DDS001.PATEDIT

               DUMP FILE               -   SYSOUT

      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSOUT
           ASSIGN TO UT-S-SYSOUT
             ORGANIZATION IS SEQUENTIAL.

           SELECT PATDATA
           ASSIGN TO UT-S-PATDATA
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATEDIT
           ASSIGN TO UT-S-PATEDIT
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATERR
           ASSIGN TO UT-S-PATERR
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATMSTR
                  ASSIGN       to PATMSTR
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is RANDOM
                  RECORD KEY   is PATIENT-KEY
                  FILE STATUS  is PATMSTR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SYSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-Rec.
       01  SYSOUT-REC  PIC X(130).

      ****** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM
      ****** IT CONSISTS OF ALL PATIENT RECORDS ENTERED
      ****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS
      ****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND
       FD  PATDATA
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 993 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-DATA.
       01  INPATIENT-DAILY-REC-DATA PIC X(993).

      ****** THIS FILE IS WRITTEN FOR ALL PATIENT RECORDS THAT PASS
      ****** THE PROGRAM'S EDIT ROUTINES
      ****** THE TRAILER RECORD SHOULD ONLY CARRY THE NUMBER OF
      ****** RECORDS IN THE FILE ON TO THE NEXT JOB STEP
       FD  PATEDIT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 993 CHARACTERS
           DATA RECORD IS INPATIENT-DAILY-REC-EDIT.
       01  INPATIENT-DAILY-REC-EDIT PIC X(993).

       FD  PATERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-ERR.
       01  INPATIENT-DAILY-REC-ERR.
           05  ERR-MSG                     PIC X(40).
           05  REST-OF-REC                 PIC X(993).

       FD  PATMSTR
           RECORD CONTAINS 2964 CHARACTERS
           DATA RECORD IS PATIENT-MASTER-REC.
       01  PATMSTR-REC.
           05 PATIENT-KEY      PIC X(06).
           05 FILLER           PIC X(2958).

      ** QSAM FILE
       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  PATMSTR-STATUS          PIC X(2).
               88 RECORD-FOUND    VALUE "00".
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       COPY PATDALY.
      ** QSAM FILE

       01  WS-TRAILER-REC.
           05  FILLER                  PIC X(1).
           05  IN-RECORD-COUNT         PIC 9(9).
           05  FILLER                  PIC X(1).
           05  IN-TOTAL-ROOM-CHARGE    PIC S9(9)V99.
           05  IN-BASE-ROOM-CHARGE     PIC S9(9)V99.
           05  IN-EQUIPMENT-CHARGES    PIC S9(9)V99.
       77  HEX-VAL       PIC X(1) VALUE ''.

       01  WS-OUTPUT-REC.
           05  PATIENT-NBR-O           PIC 9(6).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-NAME-O          PIC X(20).
           05  PATIENT-PHONE-O         PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-TYPE-O          PIC X(2).
           05  BED-IDENTITY-O          PIC ZZZ9.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  CURR-DATE-O             PIC X(6).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-AMT-PER-DAY-O   PIC $$,$$9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-COVERAGE-PERC-O     PIC 999.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-TYPE-O              PIC X(4).
           05  HOSPITAL-STAY-LTH-O     PIC 999.
           05  FILLER                  PIC X(7) VALUE SPACES.

      ** VSAM FILE
       COPY PATMSTR.

       01  WS-SYSOUT-REC.
           05  MSG                     PIC X(80).

       77  WS-DATE                     PIC 9(6).

       01  COUNTERS-IDXS-AND-ACCUMULATORS.
           05 RECORDS-WRITTEN          PIC 9(7) COMP.
           05 RECORDS-IN-ERROR         PIC 9(7) COMP.
           05 RECORDS-READ             PIC 9(9) COMP.
           05 WS-BASE-ROOM-CHARGE      PIC S9(9)V99 COMP-3.
           05 WS-TOTAL-ROOM-CHARGE     PIC S9(9)V99 COMP-3.
           05 WS-EQUIPMENT-COST        PIC S9(7)V99 COMP-3.
           05 HOLD-EQUIPMENT-COST      PIC S9(7)V99 COMP-3.
           05 ROW-SUB                  PIC 9(2).

       01  MISC-WS-FLDS.
           05 STR-LTH                  PIC 9(04) VALUE 0.
           05 RETURN-CD                PIC S9(04) VALUE 0.
           05 TABLE-SIZE               PIC 9(02) VALUE 12.
           05 MORE-TABLE-ROWS          PIC X(01).
              88 NO-MORE-TABLE-ROWS  VALUE "N".


       01  FLAGS-AND-SWITCHES.
           05 MORE-DATA-SW             PIC X(01) VALUE "Y".
               88 NO-MORE-DATA VALUE "N".
           05 ERROR-FOUND-SW           PIC X(01) VALUE "Y".
               88 RECORD-ERROR-FOUND VALUE "Y".
               88 VALID-RECORD  VALUE "N".

       COPY ABENDREC.
      ** QSAM FILE

      * COPY DIAGCODE.
      ******************************************************************
      ***** DB2 TABLE DCLGENS
       01  DCLDIAG-CODES.
           10 DIAG-CODE                      PIC X(05).
           10 INS-TYPE                       PIC X(03).
           10 COPAY                          PIC S9(4) COMP.
           10 DEDUCTIBLE                     PIC S9(4) COMP.

       01  DCLWARD-CODES.
           10 WARD-ID                        PIC X(04).
           10 PRIMARY-PHYSICIAN-ID           PIC X(08).
           10 SUPERVISE-NURSE-ID             PIC X(08).
           10 LOCATION                       PIC X(08).
           10 NUMBER-OF-BEDS                 PIC S9(4) COMP.
           10 BASE-ROOM-CHARGE               PIC S9(5)V99 COMP-3.

       01  DCLHOSP-BED.
           10 BED-ID                         PIC X(04).
           10 ROOM-ID                        PIC X(08).
           10 WARD-ID                        PIC X(08).
           10 SPECIAL-CHARGES                PIC S9(5)V99 COMP-3.

       01  DCLMEDICATION.
           10 MEDICATION-ID                  PIC X(04).
           10 MED-NAME                       PIC X(08).
           10 SHORT-DESCRIPTION              PIC X(08).
           10 COST                           PIC S9(5)V99 COMP-3.
           10 PHARMACY-COST                  PIC S9(3)V99 COMP-3.

       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-DATA OR
      ******* Balancing logic put in by TGD 02/12/92
                   TRAILER-REC.
           PERFORM 999-CLEANUP THRU 999-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "******** BEGIN JOB DALYEDIT ********".
      *  Code your statement here to OPEN files
           ACCEPT  WS-DATE FROM DATE.
           INITIALIZE COUNTERS-IDXS-AND-ACCUMULATORS, WS-TRAILER-REC.
           PERFORM 800-OPEN-FILES THRU 800-EXIT.
           PERFORM 900-READ-PATDATA THRU 900-EXIT.
           IF NO-MORE-DATA
               MOVE "EMPTY INPUT FILE" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.
       000-EXIT.
           EXIT.

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
      *  Validate patient type and insurance coverage
           PERFORM 300-FIELD-EDITS THRU 300-EXIT.

           IF RECORD-ERROR-FOUND
               ADD +1 TO RECORDS-IN-ERROR
               PERFORM 710-WRITE-PATERR THRU 710-EXIT
           ELSE
               ADD +1 TO RECORDS-WRITTEN
               PERFORM 700-WRITE-PATEDIT THRU 700-EXIT.
           PERFORM 900-READ-PATDATA THRU 900-EXIT.
       100-EXIT.
           EXIT.

       300-FIELD-EDITS.
           MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.
           MOVE "300-FIELD-EDITS" TO PARA-NAME.
      ******** Numeric fields
           IF NOT VALID-WARD IN WARD-NBR
              MOVE "*** INVALID PATIENT WARD" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF BASE-ROOM-CHARGE IN INPATIENT-DAILY-REC NOT NUMERIC
              MOVE "*** INVALID BASE ROOM CHARGE" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC NOT NUMERIC
              MOVE "*** INVALID TOTAL ROOM CHARGE" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF CURR-DTE IN INPATIENT-DAILY-REC = SPACES
              MOVE "*** INVALID CURR-DTE" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF ROOM-DATE-FROM IN INPATIENT-DAILY-REC = SPACES
              MOVE "*** INVALID ROOM-DATE-FROM" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF ROOM-DATE-TO IN INPATIENT-DAILY-REC = SPACES
              MOVE "*** INVALID ROOM-DATE-TO" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF PATIENT-ID IN INPATIENT-DAILY-REC NOT NUMERIC
              MOVE "*** NON-NUMERIC PATIENT-ID" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF BED-IDENTITY IN INPATIENT-DAILY-REC NOT NUMERIC
              MOVE "*** NON-NUMERIC BED-IDENTITY" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF ROOM-IDENTITY IN INPATIENT-DAILY-REC NOT NUMERIC
              MOVE "*** NON-NUMERIC ROOM-IDENTITY" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           IF PRIMARY-DIAGNOSTIC-CODE IN INPATIENT-DAILY-REC = SPACES
              MOVE "*** INVALID PRIMARY DIAGNOSTIC CODE" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           CALL 'DTEVAL' USING CURR-DTE, RETURN-CD.
           IF RETURN-CD < 0
              MOVE "*** BAD DATE CURR-DTE" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           CALL 'DTEVAL' USING ROOM-DATE-FROM, RETURN-CD.
           IF RETURN-CD < 0
              MOVE "*** BAD DATE: ROOM-DATE-FROM" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           CALL 'DTEVAL' USING ROOM-DATE-TO, RETURN-CD.
           IF RETURN-CD < 0
              MOVE "*** BAD DATE: ROOM-DATE-TO" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           MOVE "Y" TO MORE-TABLE-ROWS.
           PERFORM 350-CHECK-EQUIPMENT-CHARGES THRU 350-EXIT
                  VARYING ROW-SUB FROM 1 BY 1 UNTIL
                  NO-MORE-TABLE-ROWS OR ROW-SUB = 12.

           IF VALID-RECORD
              PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-EXIT.

       300-EXIT.
           EXIT.

       350-CHECK-EQUIPMENT-CHARGES.
           IF EQUIPMENT-ID(ROW-SUB) = SPACES
              MOVE "N" TO MORE-TABLE-ROWS
              GO TO 350-EXIT.

           IF EQUIPMENT-SHORT-DESC(ROW-SUB) = SPACES
              MOVE "*** BLANK EQUIPMENT-SHORT-DESC" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 350-EXIT.

           IF NOT VALID-CATEGORY(ROW-SUB)
              MOVE "*** INVALID EQUIPMENT CATEGORY" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 350-EXIT.

           IF EQUIPMENT-PRES-PHYS(ROW-SUB) = SPACES
              MOVE "*** BLANK EQUIPMENT PRESCRIBING MD" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 350-EXIT.

           IF EQUIPMENT-REASON-CDE(ROW-SUB) = SPACES
              MOVE "*** BLANK EQUIPMENT REASON-CODE" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 350-EXIT.

           IF EQUIPMENT-COST(ROW-SUB) IS NOT NUMERIC
              MOVE "*** NON-NUMERIC EQUIPMENT COST" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 350-EXIT.

           IF EQUIPMENT-COST(ROW-SUB) = 0
              MOVE "*** NON-NUMERIC EQUIPMENT COST" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 350-EXIT.

       350-EXIT.
           EXIT.

       400-NUMERIC-RANGE-EDITS.
           MOVE "400-NUMERIC-RANGE-EDITS" TO PARA-NAME.
      ******** Call to VSAM file to read record
           IF  BASE-ROOM-CHARGE IN INPATIENT-DAILY-REC > 2800.99        010399JS
           OR  BASE-ROOM-CHARGE IN INPATIENT-DAILY-REC < 99.01
               MOVE "*** INVALID BASE ROOM-CHARGE IN RECORD" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-EXIT.

      **** CURRENTLY THE TOTAL CHARGES SHOULD EXCEED $100k
           IF  TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC > 100000.00     081492AK
           OR  TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC < 99.01
               MOVE "*** INVALID TOTAL ROOM-CHARGE IN RECORD" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 400-EXIT.
      *** NEEDED?
           IF VALID-RECORD
               PERFORM 450-CROSS-FIELD-EDITS THRU 450-EXIT.

       400-EXIT.
           EXIT.

       450-CROSS-FIELD-EDITS.
           MOVE "450-CROSS-FIELD-EDITS" TO PARA-NAME.
      ******** Specific requirements for certain procedures
           IF  (INTENSIVE-CARE IN INPATIENT-DAILY-REC
                 AND TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC > 99000)
           OR  (INTENSIVE-CARE IN INPATIENT-DAILY-REC
                 AND TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC < 299.50)
               MOVE "*** INVALID ROOM-CHARGE FOR INTENSIVE CARE" TO
                  ERR-MSG IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           IF  (CARDIO-THORACIC IN INPATIENT-DAILY-REC
                 AND TOTAL-ROOM-CHARGE  IN INPATIENT-DAILY-REC > 99000)
           OR  (CARDIO-THORACIC IN INPATIENT-DAILY-REC
                 AND TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC < 309.50)
               MOVE "*** INVALID ROOM-CHARGE FOR CARDIO CARE" TO
                  ERR-MSG IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           IF  (OBSTETRICS IN INPATIENT-DAILY-REC
                 AND TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC > 88000)
           OR  (OBSTETRICS IN INPATIENT-DAILY-REC
                 AND TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC < 158.75)
               MOVE "*** INVALID ROOM-CHARGE FOR OSBSTETRICS" TO
                  ERR-MSG IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           IF  ( (PEDIATRICS IN INPATIENT-DAILY-REC
                AND TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC > 77000)
           OR  (PEDIATRICS IN INPATIENT-DAILY-REC
                AND TOTAL-ROOM-CHARGE IN INPATIENT-DAILY-REC < 119.15))
              MOVE "*** INVALID ROOM-CHARGE FOR PEDIATRICS" TO
                  ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 450-EXIT.

           IF  NOT GENERAL IN WARD-NBR
              CALL 'STRLTH' USING DAILY-CHARGES-COMMENTS, STR-LTH
              IF STR-LTH < 20
               MOVE "*** INVALID PATIENT COMMENT LENGTH" TO
                  ERR-MSG IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 450-EXIT.

           MOVE ZERO TO HOLD-EQUIPMENT-COST.
           MOVE "Y"  TO MORE-TABLE-ROWS.
           PERFORM 475-CHECK-EQUIP-CROSS-EDITS THRU 475-EXIT
                  VARYING ROW-SUB FROM 1 BY 1 UNTIL
                  NO-MORE-TABLE-ROWS OR ROW-SUB = 12.

           COMPUTE TOTAL-ROOM-CHARGE =
                      ( BASE-ROOM-CHARGE IN INPATIENT-DAILY-REC
                       + HOLD-EQUIPMENT-COST ).

           IF VALID-RECORD
              PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.

       450-EXIT.
           EXIT.

       475-CHECK-EQUIP-CROSS-EDITS.
           IF EQUIPMENT-ID(ROW-SUB) = SPACES
              MOVE "N" TO MORE-TABLE-ROWS
              GO TO 475-EXIT.

           ADD EQUIPMENT-COST(ROW-SUB) TO HOLD-EQUIPMENT-COST.

           IF DRIP(ROW-SUB) OR MONITOR(ROW-SUB)
              IF EQUIPMENT-COST(ROW-SUB) < 19.75
              MOVE "*** INSUFFICIENT MONITOR OR DRIP COST" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 475-EXIT.

           IF HEATING-PAD(ROW-SUB) OR SCOPE(ROW-SUB)
              IF EQUIPMENT-COST(ROW-SUB) < 30.01
              MOVE "*** INSUFFICIENT HEATING PAD/SCOPE COST" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 475-EXIT.

           IF AUTOCLAVE(ROW-SUB)
              IF EQUIPMENT-COST(ROW-SUB) <  29.99
              MOVE "*** INSUFFICIENT AUTOCLAVE COST" TO
              ERR-MSG IN INPATIENT-DAILY-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 475-EXIT.

       475-EXIT.
           EXIT.

       500-CROSS-FILE-EDITS.
           MOVE "500-CROSS-FILE-EDITS" TO PARA-NAME.
      ******** Call to VSAM file to read record
           MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO
                  PATIENT-KEY.
           READ PATMSTR.
           IF  NOT RECORD-FOUND
               MOVE "*** PATIENT NOT-FOUND ON MASTER FILE" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 500-EXIT.

           PERFORM 600-DB2-TABLE-EDITS THRU 600-EXIT.

       500-EXIT.
           EXIT.

       600-DB2-TABLE-EDITS.
           MOVE "600-DB2-TABLE-EDITS" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE PRIMARY-DIAGNOSTIC-CODE TO
                DIAG-CODE IN DCLDIAG-CODES.

           EXEC SQL
              SELECT DIAG_CODE INTO :DIAG-CODE
              FROM DDS0001.DIAG_CODES
              WHERE DIAG_CODE = :DIAG-CODE
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** DIAGNOSTIC CODE NOT-FOUND IN DIAG_CODES" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               move sqlcode to  EXPECTED-VAL
               move PATIENT-ID IN INPATIENT-DAILY-REC
                               to ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 600-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "*** FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               move sqlcode to  EXPECTED-VAL
               move PATIENT-ID IN INPATIENT-DAILY-REC
                               to ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

           MOVE BED-IDENTITY TO BED-ID.
           MOVE WARD-NBR TO WARD-ID IN DCLHOSP-BED
           MOVE ROOM-IDENTITY TO ROOM-ID.
           EXEC SQL
              SELECT BED_ID INTO :BED-ID
              FROM DDS0001.HOSP_BED
              WHERE BED_ID = :BED-ID AND
              WARD_ID = :DCLHOSP-BED.WARD-ID AND
              ROOM_ID = :ROOM-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** WARD/ROOM/BED NOT-FOUND IN HOSP_BED" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               move sqlcode to  EXPECTED-VAL
               move PATIENT-ID IN INPATIENT-DAILY-REC
                               to ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 600-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "*** FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               move sqlcode to  EXPECTED-VAL
               move PATIENT-ID IN INPATIENT-DAILY-REC
                               to ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

       600-EXIT.
           EXIT.

       700-WRITE-PATEDIT.
           WRITE INPATIENT-DAILY-REC-EDIT
               FROM INPATIENT-DAILY-REC.
           ADD HOLD-EQUIPMENT-COST  TO WS-EQUIPMENT-COST.
           ADD BASE-ROOM-CHARGE IN INPATIENT-DAILY-REC
                                 TO WS-BASE-ROOM-CHARGE.
           ADD TOTAL-ROOM-CHARGE  TO WS-TOTAL-ROOM-CHARGE.
       700-EXIT.
           EXIT.

       710-WRITE-PATERR.
           MOVE INPATIENT-DAILY-REC TO REST-OF-REC.
           WRITE INPATIENT-DAILY-REC-ERR.
           ADD +1 TO RECORDS-IN-ERROR.
       710-EXIT.
           EXIT.

       800-OPEN-FILES.
           MOVE "800-OPEN-FILES" TO PARA-NAME.
           OPEN INPUT PATDATA.
           OPEN OUTPUT PATEDIT, SYSOUT, PATERR.
           OPEN I-O PATMSTR.
       800-EXIT.
           EXIT.

       850-CLOSE-FILES.
           MOVE "850-CLOSE-FILES" TO PARA-NAME.
           CLOSE PATDATA,
                 PATEDIT, SYSOUT, PATERR,
                 PATMSTR.
       850-EXIT.
           EXIT.

       900-READ-PATDATA.
      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ PATDATA  INTO INPATIENT-DAILY-REC
               AT END MOVE "N" TO MORE-DATA-SW
               GO TO 900-EXIT
           END-READ.
           MOVE "N" TO ERROR-FOUND-SW.
           ADD +1 TO RECORDS-READ.
       900-EXIT.
           EXIT.

       999-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF NOT TRAILER-REC
               MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           MOVE INPATIENT-DAILY-REC-DATA TO WS-TRAILER-REC.
           ADD +1 TO RECORDS-WRITTEN.
           IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
               MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
                                     TO ABEND-REASON
               MOVE RECORDS-READ     TO ACTUAL-VAL
               MOVE IN-RECORD-COUNT  TO EXPECTED-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               DISPLAY "** RECORDS READ **"
               DISPLAY RECORDS-READ
               DISPLAY "** RECORD-IN EXPECTED **"
               DISPLAY  IN-RECORD-COUNT
               GO TO 1000-ABEND-RTN.

           MOVE "T" TO PATIENT-RECORD-TYPE.
           MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.
           MOVE WS-BASE-ROOM-CHARGE  TO IN-BASE-ROOM-CHARGE.
           MOVE WS-TOTAL-ROOM-CHARGE TO IN-TOTAL-ROOM-CHARGE.
           MOVE WS-EQUIPMENT-COST TO IN-EQUIPMENT-CHARGES.
           WRITE INPATIENT-DAILY-REC-EDIT FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.


           DISPLAY "** RECORDS READ **".
           DISPLAY RECORDS-READ.
           DISPLAY "** RECORD-IN EXPECTED **".
           DISPLAY  IN-RECORD-COUNT.
           DISPLAY "** RECORDS WRITTEN **".
           DISPLAY  RECORDS-WRITTEN.
           DISPLAY "** ERROR RECORDS FOUND **".
           DISPLAY  RECORDS-IN-ERROR.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "******** NORMAL END OF JOB DALYEDIT ********".
       999-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB- DALYEDIT ***" UPON CONSOLE.
           DIVIDE ZERO-VAL INTO ONE-VAL.

       1000-DB2-ERROR-RTN.
      ************************************************************
      *       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *
      ************************************************************

            DISPLAY '**** WE HAVE A SERIOUS PROBLEM HERE *****'.
            DISPLAY '999-ERROR-TRAP-RTN '.
            MULTIPLY SQLCODE BY -1 GIVING SQLCODE.
            DISPLAY 'SQLCODE ==> ' SQLCODE.
            DISPLAY SQLCA.
            DISPLAY SQLERRM.
            EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
            EXEC SQL ROLLBACK WORK END-EXEC.
            GO TO 1000-ABEND-RTN.