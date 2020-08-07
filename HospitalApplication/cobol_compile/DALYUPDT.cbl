       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DALYUPDT.
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
       FILE-CONTROL.
           SELECT SYSOUT
           ASSIGN TO UT-S-SYSOUT
             ORGANIZATION IS SEQUENTIAL.

           SELECT PATSRCH-FILE
           ASSIGN TO UT-S-PATSRCH
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
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-Rec.
       01  SYSOUT-REC  PIC X(100).

      ****** THIS FILE IS WRITTEN FOR ALL TREATMENT RECORDS THAT PASS
      ****** THE PROGRAM'S SEARCH ROUTINES
      ****** THE TRAILER RECORD SHOULD ONLY CARRY THE NUMBER OF
      ****** RECORDS IN THE FILE ON TO THE NEXT JOB STEP
       FD  PATSRCH-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 993 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-SRCH.
       01  INPATIENT-DAILY-REC-SRCH PIC X(993).


       FD  PATMSTR
           RECORD CONTAINS 2964 CHARACTERS
           DATA RECORD IS PATIENT-MASTER-REC.
       01  PATMSTR-REC.
           05 PATIENT-KEY      PIC X(06).
           05 FILLER           PIC X(2958).

      ** QSAM FILE
       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  IFCODE                  PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  EFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  RFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  DFCODE                  PIC X(2).
               88 DIAG-READ    VALUE SPACES.
               88 NO-MORE-DIAG  VALUE "10".
           05  PATMSTR-STATUS          PIC X(2).
               88 RECORD-FOUND    VALUE "00".
           05  TRTMNT-CODE    PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

      ** QSAM FILE
       COPY PATDALY.

       01  WS-TRAILER-REC.
           05  FILLER                     PIC X(1).
           05  IN-RECORD-COUNT            PIC 9(9).
           05  FILLER                     PIC X(1).
           05  IN-MEDICATION-CHARGES      PIC S9(9)V99.
           05  IN-PHARMACY-CHARGES        PIC S9(7)V99.
           05  IN-ANCILLARY-CHARGES       PIC S9(5)V99.
           05  IN-LABTEST-CHARGES         PIC S9(9)V99.
           05  IN-EQUIP-ANCILLARY-CHARGES PIC S9(7)V99.
           05  IN-VENIPUNCTURE-CHARGES    PIC S9(7)V99.

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

       77  INS-COVERAGE-PERC            PIC 9(3) VALUE 10.

       77  WS-DATE                      PIC 9(6).
       01  MORE-TRANSRCH-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-TRANSRCH-RECS  VALUE "N".
           88 MORE-TRANSRCH-RECS     VALUE " ".

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ             PIC S9(9) COMP.
           05 RECORDS-WRITTEN          PIC S9(9) COMP.
           05 ERROR-RECS               PIC S9(7) COMP.
           05 NBR-INPATIENTS           PIC S9(7) COMP.
           05 NBR-OUTPATIENTS          PIC S9(7) COMP.
           05 NBR-HMO                  PIC S9(4) COMP.
           05 NBR-STATE-FED            PIC S9(4) COMP.
           05 NBR-NO-COVERAGE          PIC S9(4) COMP.
           05 PAT-TOTAL-AMT-NET        PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-GROSS          PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-NET            PIC S9(7)V99 COMP-3.
           05 ROW-SUB                  PIC 9(02) VALUE ZERO.
           05 PAT-SUB                  PIC 9(02) VALUE ZERO.
           05 CALC-CALL-RET-CODE       PIC S9(4) COMP.
           05 WS-EQUIPMENT-CHARGES     PIC 9(9)V99 COMP-3.
           05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.
           05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.
           05 WS-VENIPUNCTURE-CHARGES  PIC S9(5)V99 COMP-3.
           05 HOLD-PHYSICIAN-ID        PIC X(8).
           05 HOLD-EQUIP-TEST-ID         PIC X(8).
           05 HOLD-SHORT-DESC-ID       PIC X(30).
           05 HOLD-DIAG-CD             PIC X(5).
           05 HOLD-FIELD               PIC X(255).
           05 HOLD-TALLY               PIC 9(4) COMP.
           05 HOLD-TALLY-END           PIC 9(4) COMP.
           05 MORE-TABLE-ROWS          PIC X(1).
              88 NO-MORE-LABS   VALUE "N".
           05 CURRENT-ROW-SW          PIC X(1).
              88 CURRENT-ROW    VALUE "Y".

       COPY ABENDREC.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-TRANSRCH-RECS OR TRAILER-REC.
           PERFORM 900-CLEANUP THRU 900-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "******** BEGIN JOB DALYEDIT ********".
      *  Code your statement here to OPEN files
           ACCEPT  WS-DATE FROM DATE.
           OPEN INPUT PATSRCH-FILE.
           OPEN I-O PATMSTR.
           OPEN OUTPUT SYSOUT.

      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ PATSRCH-FILE INTO INPATIENT-DAILY-REC
               AT END
               MOVE 'N' TO MORE-TRANSRCH-SW
               GO TO 000-EXIT
           END-READ

           INITIALIZE  COUNTERS-AND-ACCUMULATORS.
           ADD +1 TO RECORDS-READ.
           MOVE 1 TO ROW-SUB.
       000-EXIT.
           EXIT.

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
      *  Validate patient type and insurance coverage

           MOVE "Y"  TO MORE-TABLE-ROWS.
           PERFORM 200-CALCULATE-EQUIP-CHARGES THRU 200-EXIT.

           PERFORM 400-APPLY-UPDATES THRU 400-EXIT.

           READ PATSRCH-FILE INTO INPATIENT-DAILY-REC
               AT END
               MOVE 'N' TO MORE-TRANSRCH-SW
               GO TO 100-EXIT
           END-READ

           IF MORE-TRANSRCH-RECS
                ADD +1 TO RECORDS-READ.

           IF TRAILER-REC
                MOVE INPATIENT-DAILY-REC TO WS-TRAILER-REC.

      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
       100-EXIT.
           EXIT.

       200-CALCULATE-EQUIP-CHARGES.
           MOVE "200-CALCULATE-EQUIP-CHARGES-RTN" TO PARA-NAME.
           MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO
               PATIENT-KEY.

           READ PATMSTR INTO PATIENT-MASTER-REC.
           IF NOT RECORD-FOUND
                 MOVE "** PROBLEM READING PATMSTR" TO ABEND-REASON
                 MOVE PATMSTR-STATUS TO EXPECTED-VAL
                 MOVE  PATIENT-KEY TO ACTUAL-VAL IN ABEND-REC
                 GO TO 1000-ABEND-RTN.

           MOVE ZERO TO WS-EQUIPMENT-CHARGES.
           MOVE "N" TO CURRENT-ROW-SW.
           PERFORM 225-POSITION-PAT-TABLE-IDX THRU 225-EXIT
                  VARYING PAT-SUB FROM 1 BY 1 UNTIL
                  PAT-SUB = 20 OR
                  CURRENT-ROW.

           MOVE "Y" TO MORE-TABLE-ROWS.
           PERFORM 250-PROCESS-EQUIP-CHARGES THRU 250-EXIT
                  VARYING ROW-SUB FROM 1 BY 1 UNTIL
                  ROW-SUB = 12 OR
                  NO-MORE-LABS.

      **** MAYBE PUT JOB-BALANCING BUG IN HERE?

       200-EXIT.
           EXIT.

       225-POSITION-PAT-TABLE-IDX.
           MOVE "250-PROCESS-EQUIP-CHARGES-TABLE" TO PARA-NAME.
           IF LAB-TEST-S-ID(PAT-SUB) = SPACES
              MOVE "Y" TO CURRENT-ROW-SW
              GO TO 225-EXIT.

       225-EXIT.
           EXIT.

       250-PROCESS-EQUIP-CHARGES.
           MOVE "250-PROCESS-EQUIP-CHARGES-TABLE" TO PARA-NAME.
           IF EQUIPMENT-ID IN ADDITIONAL-EQUIP-CHARGES(ROW-SUB) = SPACE
              MOVE "N" TO MORE-TABLE-ROWS
              GO TO 250-EXIT.

      *** ADD TO HOLD-WS-VALUES
           ADD EQUIPMENT-COST(ROW-SUB) TO WS-EQUIPMENT-CHARGES.
           PERFORM 300-RECONCILE-DIAGNOSTIC-CODES THRU 300-EXIT.

       250-EXIT.
           EXIT.

       300-RECONCILE-DIAGNOSTIC-CODES.
           MOVE "300-RECONCILE-DIAGNOSTIC-CODES" TO PARA-NAME.

           IF EQUIP-DIAG-CODE(ROW-SUB) EQUAL TO DIAGNOSTIC-CODE-PRIMARY
            OR DIAGNOSTIC-CODE-SECONDARY OR DIAGNOSTIC-CODE-TERTIARY
            GO TO 300-EXIT.

           IF EQUIP-DIAG-CODE(ROW-SUB) NOT EQUAL TO
                                       DIAGNOSTIC-CODE-PRIMARY
              AND DIAGNOSTIC-CODE-SECONDARY = SPACES
              MOVE EQUIP-DIAG-CODE(ROW-SUB) TO DIAGNOSTIC-CODE-SECONDARY
              GO TO 300-EXIT.

           IF EQUIP-DIAG-CODE(ROW-SUB) NOT EQUAL TO
                                       DIAGNOSTIC-CODE-SECONDARY
              AND DIAGNOSTIC-CODE-TERTIARY  = SPACES
              MOVE EQUIP-DIAG-CODE(ROW-SUB) TO
                                   DIAGNOSTIC-CODE-TERTIARY
              GO TO 300-EXIT.

           IF DIAGNOSTIC-CODE-TERTIARY = SPACES
              MOVE EQUIP-DIAG-CODE(ROW-SUB) TO
                           DIAGNOSTIC-CODE-TERTIARY
              GO TO 300-EXIT
           ELSE
              UNSTRING PATIENT-ADMIT-COMMENTS DELIMITED BY ","
                INTO HOLD-FIELD TALLYING IN HOLD-TALLY
                ADD +1 TO HOLD-TALLY
                ADD +23 TO HOLD-TALLY GIVING HOLD-TALLY-END
                MOVE "*** NEW DIAG-CODE ***"
                       TO HOLD-FIELD(HOLD-TALLY : HOLD-TALLY-END)
                ADD +25 TO HOLD-TALLY
                ADD +30 TO HOLD-TALLY GIVING HOLD-TALLY-END
                MOVE EQUIP-DIAG-CODE(ROW-SUB)
                       TO HOLD-FIELD(HOLD-TALLY : HOLD-TALLY-END)
                MOVE HOLD-FIELD TO PATIENT-ADMIT-COMMENTS.

       300-EXIT.
           EXIT.

       400-APPLY-UPDATES.
           MOVE "400-APPLY-UPDATES" TO PARA-NAME.

           ADD WS-EQUIPMENT-CHARGES TO TEST-CHARGES (PAT-SUB).
           ADD TOTAL-ROOM-CHARGE    TO PATIENT-TOT-AMT.
           ADD +1                   TO HOSPITAL-STAY-LTH.

           MOVE HOLD-EQUIP-TEST-ID  TO LAB-TEST-S-ID(PAT-SUB).
           MOVE HOLD-SHORT-DESC-ID  TO TEST-SHORT-S-DESC(PAT-SUB).
           MOVE HOLD-DIAG-CD        TO TEST-DIAG-CODE(PAT-SUB).
           MOVE WS-DATE             TO LAB-TEST-DATE(PAT-SUB).
           MOVE PRIMARY-DIAGNOSTIC-CODE IN INPATIENT-DAILY-REC
                TO PRIMARY-CARE-PHYSICIAN-ID IN PATIENT-MASTER-REC.
           MOVE BED-IDENTITY        TO BED-IDENTITY-PRIMARY.


           REWRITE PATMSTR-REC FROM PATIENT-MASTER-REC
             INVALID KEY
                 MOVE "** PROBLEM REWRITING PATMSTR" TO ABEND-REASON
                 MOVE PATMSTR-STATUS TO EXPECTED-VAL
                 GO TO 1000-ABEND-RTN
           END-REWRITE.

       400-EXIT.
           EXIT.


       700-CLOSE-FILES.
           MOVE "700-CLOSE-FILES" TO PARA-NAME.
      *  Code the statement to close all files
           CLOSE PATSRCH-FILE,
                  SYSOUT, PATMSTR.
       700-EXIT.
           EXIT.

       900-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF NOT TRAILER-REC
               MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
               MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
                                     TO ABEND-REASON
               WRITE SYSOUT-REC FROM ABEND-REC.
      *         GO TO 1000-ABEND-RTN.

           MOVE "T" TO PATIENT-RECORD-TYPE.
           MOVE RECORDS-READ TO IN-RECORD-COUNT.

           WRITE SYSOUT-REC FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 700-CLOSE-FILES THRU 700-EXIT.

           DISPLAY "** PATIENT RECORDS READ **".
           DISPLAY RECORDS-READ.
           DISPLAY "** PATIENT RECORDS EXPECTED **".
           DISPLAY  IN-RECORD-COUNT.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "******** NORMAL END OF JOB PATEDIT ********".
       900-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 700-CLOSE-FILES THRU 700-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB-TRTMTSRCH ***" UPON CONSOLE.
           DIVIDE ZERO-VAL INTO ONE-VAL. 