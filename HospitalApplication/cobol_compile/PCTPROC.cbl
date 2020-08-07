      ***************************************************
      *COBOL Stored Procedure PCTPROC
      *System Long Name:  ZSERVEROS.DEMOS.IBM.COM
      *System Short Name:  Lab7
      *Data Set:  DDS0017.TEST.COBOL1(PCTPROC)
      * @param PLANID
      * @param REIMBURSEPCT
      * @param SQLCODEOUT
      ***************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PCTPROC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NAM              PIC X(18) VALUE 'PCTPROC'.
       01 SCHE             PIC X(8).
       01 W-SQLCODE        PIC S9(3).
       COPY HLTHPLAN.
            EXEC SQL INCLUDE SQLCA END-EXEC.
       LINKAGE SECTION.
       01 PLANID           PIC X(20).
       01 INOUTNETWORK     PIC X(1).
       01 REIMBURSEPCT     PIC S9(4) COMP-5.
       01 SQLCODEOUT       PIC S9(9) COMP-5.
       PROCEDURE DIVISION USING
            PLANID
            INOUTNETWORK
            REIMBURSEPCT
            SQLCODEOUT.
           EXEC SQL
            SELECT COPAYMENT, COVERAGE_LIMITS, DEDUCTIBLE,
                   IN_NETWORK_REQ, OOP_MAX
            INTO
             :COPAYMENT              ,
             :COVERAGE-LIMITS        ,
             :DEDUCTIBLE             ,
             :IN-NETWORK-REQ         ,
             :OOP-MAX
            FROM DDS0001.HEALTH_PLAN
            WHERE PLAN_ID = :PLANID
           END-EXEC.
           IF SQLCODE = +100
              MOVE 10 TO REIMBURSEPCT.
           IF SQLCODE = +0
              IF DEDUCTIBLE > 0 AND
	 		             COPAYMENT > 0  AND
			              OOP-MAX < 1000  AND
			              COVERAGE-LIMITS > 10000 AND
			              IN-NETWORK-REQ = 'Y' AND INOUTNETWORK = 'Y'
		               MOVE 80 TO REIMBURSEPCT
		            ELSE
	                IF (DEDUCTIBLE > 0 AND COPAYMENT = 0)  OR
                    (DEDUCTIBLE = 0 AND COPAYMENT > 0) AND
			                  OOP-MAX < 1000  AND
			                  COVERAGE-LIMITS > 10000 AND
			               IN-NETWORK-REQ = 'Y' AND INOUTNETWORK = 'Y'
		                MOVE 60 TO REIMBURSEPCT
		            ELSE
		               IF IN-NETWORK-REQ = 'Y' AND INOUTNETWORK = 'N'
			                 MOVE 40 TO REIMBURSEPCT
		               ELSE
			                 MOVE 30 TO REIMBURSEPCT.
           MOVE SQLCODE TO W-SQLCODE.
           MOVE W-SQLCODE TO SQLCODEOUT.
           GOBACK.