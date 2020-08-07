       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID.  STRLTH.
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
           05 L         PIC  S9(4) COMP.
           05 TEMP-TXT  PIC X(254).

       LINKAGE SECTION.
       01  TEXT1        PIC X(255).
       01  RETURN-LTH   PIC S9(4).

       PROCEDURE DIVISION USING TEXT1, RETURN-LTH.
           MOVE 0 TO L.
           MOVE FUNCTION REVERSE(TEXT1) TO TEMP-TXT.
           INSPECT TEMP-TXT
                     REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT TEMP-TXT
                          TALLYING L FOR LEADING SPACES.
           COMPUTE L  = LENGTH OF TEXT1 - L.
           ADD L TO RETURN-LTH.
           GOBACK.