       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCEDI837.
      *================================================================*
      * PROGRAM:    WCEDI837                                           *
      * AUTHOR:     MAINFRAMEMOD TPA SYSTEMS                           *
      * DATE:       2024-01-15                                         *
      * PURPOSE:    EDI 837 (HEALTH CARE CLAIM) INBOUND PROCESSING     *
      *             PARSES 837P (PROFESSIONAL) AND 837I (INSTITUTIONAL)*
      *             TRANSACTIONS AND MAPS TO INTERNAL CLAIM FORMAT     *
      * PLATFORM:   HPE NONSTOP / GUARDIAN                             *
      *================================================================*
      * CHANGE LOG:                                                    *
      * 2024-01-15  INITIAL DEVELOPMENT - 837P SUPPORT                 *
      * 2024-03-22  ADDED 837I INSTITUTIONAL PROCESSING                *
      * 2024-06-10  ADDED SNIP LEVEL VALIDATION                        *
      * 2024-09-01  TA1/999 ACKNOWLEDGMENT GENERATION                  *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EDI-INBOUND-FILE
               ASSIGN TO EDIINB
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-EDI-FILE-STATUS.
           SELECT CLAIM-OUTPUT-FILE
               ASSIGN TO CLMOUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CLM-FILE-STATUS.
           SELECT ACK-OUTPUT-FILE
               ASSIGN TO ACKOUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ACK-FILE-STATUS.
           SELECT ERROR-LOG-FILE
               ASSIGN TO ERRLOG
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ERR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EDI-INBOUND-FILE
           RECORDING MODE IS V
           RECORD CONTAINS 1 TO 4096 CHARACTERS.
       01  EDI-INBOUND-RECORD          PIC X(4096).

       FD  CLAIM-OUTPUT-FILE.
       01  CLAIM-OUTPUT-RECORD         PIC X(512).

       FD  ACK-OUTPUT-FILE.
       01  ACK-OUTPUT-RECORD           PIC X(256).

       FD  ERROR-LOG-FILE.
       01  ERROR-LOG-RECORD            PIC X(256).

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCPRVCPY
       01  WS-FILE-STATUSES.
           05  WS-EDI-FILE-STATUS      PIC XX.
           05  WS-CLM-FILE-STATUS      PIC XX.
           05  WS-ACK-FILE-STATUS      PIC XX.
           05  WS-ERR-FILE-STATUS      PIC XX.

       01  WS-PROGRAM-CONTROLS.
           05  WS-EOF-FLAG             PIC X VALUE 'N'.
               88  END-OF-FILE         VALUE 'Y'.
               88  NOT-END-OF-FILE     VALUE 'N'.
           05  WS-PARSE-POSITION       PIC 9(5) VALUE 0.
           05  WS-SEGMENT-ID           PIC X(3).
           05  WS-ELEMENT-DELIM        PIC X VALUE '*'.
           05  WS-SUB-DELIM            PIC X VALUE ':'.
           05  WS-SEGMENT-TERM         PIC X VALUE '~'.
           05  WS-RECORD-LENGTH        PIC 9(5).

       01  WS-TRANSACTION-TYPE.
           05  WS-TRANS-TYPE-CODE      PIC X(5).
               88  IS-837P             VALUE '837P '.
               88  IS-837I             VALUE '837I '.
           05  WS-IMPL-GUIDE-VER      PIC X(12).

       01  WS-SEGMENT-IDS.
           05  FILLER                  PIC X(3) VALUE 'ISA'.
           05  FILLER                  PIC X(3) VALUE 'GS '.
           05  FILLER                  PIC X(3) VALUE 'ST '.
           05  FILLER                  PIC X(3) VALUE 'BHT'.
           05  FILLER                  PIC X(3) VALUE 'NM1'.
           05  FILLER                  PIC X(3) VALUE 'N3 '.
           05  FILLER                  PIC X(3) VALUE 'N4 '.
           05  FILLER                  PIC X(3) VALUE 'REF'.
           05  FILLER                  PIC X(3) VALUE 'CLM'.
           05  FILLER                  PIC X(3) VALUE 'DTP'.
           05  FILLER                  PIC X(3) VALUE 'HI '.
           05  FILLER                  PIC X(3) VALUE 'SV1'.
           05  FILLER                  PIC X(3) VALUE 'SV2'.
           05  FILLER                  PIC X(3) VALUE 'SE '.
           05  FILLER                  PIC X(3) VALUE 'GE '.
           05  FILLER                  PIC X(3) VALUE 'IEA'.
       01  WS-SEGMENT-TABLE REDEFINES WS-SEGMENT-IDS.
           05  WS-VALID-SEG            PIC X(3) OCCURS 16 TIMES.

       01  WS-CURRENT-SEGMENT         PIC X(2048).
       01  WS-ELEMENT-BUFFER          PIC X(256).
       01  WS-ELEMENT-COUNT           PIC 99 VALUE 0.
       01  WS-ELEMENT-TABLE.
           05  WS-ELEMENT             PIC X(128) OCCURS 30 TIMES.

       01  WS-ISA-ENVELOPE.
           05  WS-ISA-AUTH-QUAL       PIC XX.
           05  WS-ISA-AUTH-INFO       PIC X(10).
           05  WS-ISA-SEC-QUAL        PIC XX.
           05  WS-ISA-SEC-INFO        PIC X(10).
           05  WS-ISA-SENDER-QUAL     PIC XX.
           05  WS-ISA-SENDER-ID       PIC X(15).
           05  WS-ISA-RECV-QUAL       PIC XX.
           05  WS-ISA-RECV-ID         PIC X(15).
           05  WS-ISA-DATE            PIC X(6).
           05  WS-ISA-TIME            PIC X(4).
           05  WS-ISA-REP-SEP         PIC X.
           05  WS-ISA-VERSION         PIC X(5).
           05  WS-ISA-CONTROL-NUM     PIC X(9).
           05  WS-ISA-ACK-REQ         PIC X.
           05  WS-ISA-USAGE-IND       PIC X.
               88  ISA-PRODUCTION     VALUE 'P'.
               88  ISA-TEST           VALUE 'T'.

       01  WS-GS-ENVELOPE.
           05  WS-GS-FUNC-CODE       PIC X(2).
           05  WS-GS-SENDER-CODE     PIC X(15).
           05  WS-GS-RECV-CODE       PIC X(15).
           05  WS-GS-DATE            PIC X(8).
           05  WS-GS-TIME            PIC X(8).
           05  WS-GS-CONTROL-NUM     PIC X(9).
           05  WS-GS-RESP-AGENCY     PIC X(2).
           05  WS-GS-VERSION         PIC X(12).

       01  WS-BILLING-PROVIDER.
           05  WS-BP-LAST-NAME       PIC X(60).
           05  WS-BP-FIRST-NAME      PIC X(35).
           05  WS-BP-NPI             PIC X(10).
           05  WS-BP-TAX-ID          PIC X(9).
           05  WS-BP-TAX-QUAL        PIC XX.
               88  BP-EIN            VALUE 'EI'.
               88  BP-SSN            VALUE 'SY'.
           05  WS-BP-ADDR-LINE1      PIC X(55).
           05  WS-BP-ADDR-LINE2      PIC X(55).
           05  WS-BP-CITY            PIC X(30).
           05  WS-BP-STATE           PIC XX.
           05  WS-BP-ZIP             PIC X(9).

       01  WS-SUBSCRIBER.
           05  WS-SUB-LAST-NAME      PIC X(60).
           05  WS-SUB-FIRST-NAME     PIC X(35).
           05  WS-SUB-MEMBER-ID      PIC X(20).
           05  WS-SUB-SSN            PIC X(9).
           05  WS-SUB-DOB            PIC X(8).
           05  WS-SUB-GENDER         PIC X.
               88  SUB-MALE          VALUE 'M'.
               88  SUB-FEMALE        VALUE 'F'.

       01  WS-CLAIM-DATA.
           05  WS-CLM-SUBMITTER-ID   PIC X(38).
           05  WS-CLM-TOTAL-CHARGE   PIC S9(7)V99 COMP-3.
           05  WS-CLM-PLACE-OF-SVC   PIC XX.
               88  POS-OFFICE        VALUE '11'.
               88  POS-INPATIENT     VALUE '21'.
               88  POS-OUTPATIENT    VALUE '22'.
               88  POS-ER            VALUE '23'.
           05  WS-CLM-FREQUENCY      PIC X.
               88  FREQ-ORIGINAL     VALUE '1'.
               88  FREQ-REPLACE      VALUE '7'.
               88  FREQ-VOID         VALUE '8'.
           05  WS-CLM-SIGNATURE      PIC X.
           05  WS-CLM-ASSIGN-BENE    PIC X.
           05  WS-CLM-RELEASE-INFO   PIC X.
           05  WS-CLM-ONSET-DATE     PIC X(8).
           05  WS-CLM-FROM-DATE      PIC X(8).
           05  WS-CLM-THRU-DATE      PIC X(8).
           05  WS-CLM-ADMIT-DATE     PIC X(8).
           05  WS-CLM-DISCH-DATE     PIC X(8).
           05  WS-CLM-AUTH-NUM       PIC X(30).

       01  WS-DIAGNOSIS-DATA.
           05  WS-DIAG-COUNT         PIC 99 VALUE 0.
           05  WS-DIAG-ENTRY OCCURS 12 TIMES.
               10  WS-DIAG-QUAL      PIC X(3).
                   88  DIAG-ICD10    VALUE 'ABK' 'ABF'.
                   88  DIAG-ICD9     VALUE 'BK ' 'BF '.
               10  WS-DIAG-CODE      PIC X(7).

       01  WS-SERVICE-LINE.
           05  WS-SVC-LINE-COUNT     PIC 99 VALUE 0.
           05  WS-SVC-LINE OCCURS 50 TIMES.
               10  WS-SL-PROC-CODE   PIC X(5).
               10  WS-SL-MODIFIER    PIC X(2) OCCURS 4.
               10  WS-SL-CHARGE      PIC S9(7)V99 COMP-3.
               10  WS-SL-UNITS       PIC S9(5)V9 COMP-3.
               10  WS-SL-POS         PIC XX.
               10  WS-SL-FROM-DATE   PIC X(8).
               10  WS-SL-THRU-DATE   PIC X(8).
               10  WS-SL-DIAG-PTR    PIC 99 OCCURS 4.
               10  WS-SL-REV-CODE    PIC X(4).
               10  WS-SL-NDC-CODE    PIC X(11).

       01  WS-COUNTERS.
           05  WS-CLAIMS-READ        PIC 9(7) VALUE 0.
           05  WS-CLAIMS-ACCEPTED    PIC 9(7) VALUE 0.
           05  WS-CLAIMS-REJECTED    PIC 9(7) VALUE 0.
           05  WS-LINES-READ         PIC 9(9) VALUE 0.
           05  WS-SEGMENTS-PARSED    PIC 9(9) VALUE 0.
           05  WS-TRANS-SETS         PIC 9(5) VALUE 0.
           05  WS-FUNC-GROUPS       PIC 9(5) VALUE 0.

       01  WS-VALIDATION.
           05  WS-SNIP-LEVEL         PIC 9.
               88  SNIP-1-INTEGRITY  VALUE 1.
               88  SNIP-2-REQUIREMENT VALUE 2.
               88  SNIP-3-BALANCING  VALUE 3.
               88  SNIP-4-SITUATIONAL VALUE 4.
               88  SNIP-5-EXTERNAL   VALUE 5.
               88  SNIP-6-PRODUCT    VALUE 6.
               88  SNIP-7-PARTNER    VALUE 7.
           05  WS-ERROR-COUNT        PIC 9(5) VALUE 0.
           05  WS-ERROR-TABLE.
               10  WS-ERR-ENTRY OCCURS 100 TIMES.
                   15  WS-ERR-SEGMENT PIC X(3).
                   15  WS-ERR-ELEMENT PIC 99.
                   15  WS-ERR-CODE   PIC X(4).
                   15  WS-ERR-DESC   PIC X(80).

       01  WS-ACK-DATA.
           05  WS-ACK-TYPE           PIC X(3).
               88  ACK-TA1           VALUE 'TA1'.
               88  ACK-999           VALUE '999'.
           05  WS-ACK-STATUS         PIC X.
               88  ACK-ACCEPTED      VALUE 'A'.
               88  ACK-REJECTED      VALUE 'R'.
               88  ACK-ACCEPTED-ERR  VALUE 'E'.
           05  WS-ST-SEGMENT-COUNT   PIC 9(6) VALUE 0.
           05  WS-SE-CONTROL-NUM     PIC X(9).

       01  WS-WORK-FIELDS.
           05  WS-WORK-STRING        PIC X(4096).
           05  WS-TALLY-FIELD        PIC 9(5).
           05  WS-SUB-IDX            PIC 99.
           05  WS-ELEM-IDX           PIC 99.
           05  WS-LINE-IDX           PIC 99.
           05  WS-DIAG-IDX           PIC 99.
           05  WS-PTR-START          PIC 9(5).
           05  WS-PTR-END            PIC 9(5).
           05  WS-CURRENT-LOOP       PIC X(5).
               88  IN-LOOP-2000A     VALUE '2000A'.
               88  IN-LOOP-2000B     VALUE '2000B'.
               88  IN-LOOP-2010AA    VALUE '210AA'.
               88  IN-LOOP-2010AB    VALUE '210AB'.
               88  IN-LOOP-2010BA    VALUE '210BA'.
               88  IN-LOOP-2300      VALUE '2300 '.
               88  IN-LOOP-2310      VALUE '2310 '.
               88  IN-LOOP-2400      VALUE '2400 '.

       01  WS-TIMESTAMP.
           05  WS-TS-DATE            PIC 9(8).
           05  WS-TS-TIME            PIC 9(8).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
      *
      *    Inter-program communication calls
           CALL "WCMEDFEE"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-EDI-FILE
           PERFORM 9000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT  EDI-INBOUND-FILE
           OPEN OUTPUT CLAIM-OUTPUT-FILE
                       ACK-OUTPUT-FILE
                       ERROR-LOG-FILE
           IF WS-EDI-FILE-STATUS NOT = '00'
               DISPLAY 'WCEDI837: FATAL - CANNOT OPEN EDI INPUT '
                       WS-EDI-FILE-STATUS
               STOP RUN
           END-IF
           INITIALIZE WS-COUNTERS
           INITIALIZE WS-VALIDATION
           ACCEPT WS-TS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TS-TIME FROM TIME
           DISPLAY 'WCEDI837: STARTED AT ' WS-TS-DATE ' ' WS-TS-TIME.

       2000-PROCESS-EDI-FILE.
           READ EDI-INBOUND-FILE INTO WS-WORK-STRING
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-LINES-READ
               PERFORM 2100-PARSE-SEGMENTS
               READ EDI-INBOUND-FILE INTO WS-WORK-STRING
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

       2100-PARSE-SEGMENTS.
           MOVE 1 TO WS-PARSE-POSITION
           PERFORM UNTIL WS-PARSE-POSITION >= FUNCTION LENGTH(
               FUNCTION TRIM(WS-WORK-STRING))
               PERFORM 2110-EXTRACT-SEGMENT
               IF WS-CURRENT-SEGMENT NOT = SPACES
                   ADD 1 TO WS-SEGMENTS-PARSED
                   PERFORM 2200-ROUTE-SEGMENT
               END-IF
           END-PERFORM.

       2110-EXTRACT-SEGMENT.
           MOVE SPACES TO WS-CURRENT-SEGMENT
           INSPECT WS-WORK-STRING TALLYING WS-TALLY-FIELD
               FOR CHARACTERS BEFORE INITIAL WS-SEGMENT-TERM
           IF WS-TALLY-FIELD > 0
               MOVE WS-WORK-STRING(WS-PARSE-POSITION:
                   WS-TALLY-FIELD) TO WS-CURRENT-SEGMENT
               ADD WS-TALLY-FIELD TO WS-PARSE-POSITION
               ADD 1 TO WS-PARSE-POSITION
           ELSE
               MOVE FUNCTION LENGTH(WS-WORK-STRING)
                   TO WS-PARSE-POSITION
           END-IF.

       2200-ROUTE-SEGMENT.
           MOVE WS-CURRENT-SEGMENT(1:3) TO WS-SEGMENT-ID
           EVALUATE WS-SEGMENT-ID
               WHEN 'ISA'
                   PERFORM 3000-PROCESS-ISA
               WHEN 'GS '
                   PERFORM 3100-PROCESS-GS
               WHEN 'ST '
                   PERFORM 3200-PROCESS-ST
               WHEN 'BHT'
                   PERFORM 3300-PROCESS-BHT
               WHEN 'NM1'
                   PERFORM 4000-PROCESS-NM1
               WHEN 'N3 '
                   PERFORM 4100-PROCESS-N3
               WHEN 'N4 '
                   PERFORM 4200-PROCESS-N4
               WHEN 'REF'
                   PERFORM 4300-PROCESS-REF
               WHEN 'CLM'
                   PERFORM 5000-PROCESS-CLM
               WHEN 'DTP'
                   PERFORM 5100-PROCESS-DTP
               WHEN 'HI '
                   PERFORM 5200-PROCESS-HI
               WHEN 'SV1'
                   PERFORM 5300-PROCESS-SV1
               WHEN 'SV2'
                   PERFORM 5400-PROCESS-SV2
               WHEN 'SE '
                   PERFORM 6000-PROCESS-SE
               WHEN 'GE '
                   PERFORM 6100-PROCESS-GE
               WHEN 'IEA'
                   PERFORM 6200-PROCESS-IEA
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       2300-PARSE-ELEMENTS.
           MOVE 0 TO WS-ELEMENT-COUNT
           MOVE 4 TO WS-PTR-START
           PERFORM VARYING WS-ELEM-IDX FROM 1 BY 1
               UNTIL WS-ELEM-IDX > 30
               MOVE SPACES TO WS-ELEMENT(WS-ELEM-IDX)
               UNSTRING WS-CURRENT-SEGMENT
                   DELIMITED BY WS-ELEMENT-DELIM
                   INTO WS-ELEMENT(WS-ELEM-IDX)
                   WITH POINTER WS-PTR-START
               END-UNSTRING
               ADD 1 TO WS-ELEMENT-COUNT
               IF WS-PTR-START > FUNCTION LENGTH(
                   FUNCTION TRIM(WS-CURRENT-SEGMENT))
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       3000-PROCESS-ISA.
           PERFORM 2300-PARSE-ELEMENTS
           IF WS-ELEMENT-COUNT < 16
               PERFORM 8100-LOG-SNIP1-ERROR
           ELSE
               MOVE WS-ELEMENT(1)  TO WS-ISA-AUTH-QUAL
               MOVE WS-ELEMENT(2)  TO WS-ISA-AUTH-INFO
               MOVE WS-ELEMENT(3)  TO WS-ISA-SEC-QUAL
               MOVE WS-ELEMENT(4)  TO WS-ISA-SEC-INFO
               MOVE WS-ELEMENT(5)  TO WS-ISA-SENDER-QUAL
               MOVE WS-ELEMENT(6)  TO WS-ISA-SENDER-ID
               MOVE WS-ELEMENT(7)  TO WS-ISA-RECV-QUAL
               MOVE WS-ELEMENT(8)  TO WS-ISA-RECV-ID
               MOVE WS-ELEMENT(9)  TO WS-ISA-DATE
               MOVE WS-ELEMENT(10) TO WS-ISA-TIME
               MOVE WS-ELEMENT(11) TO WS-ISA-REP-SEP
               MOVE WS-ELEMENT(12) TO WS-ISA-VERSION
               MOVE WS-ELEMENT(13) TO WS-ISA-CONTROL-NUM
               MOVE WS-ELEMENT(14) TO WS-ISA-ACK-REQ
               MOVE WS-ELEMENT(15) TO WS-ISA-USAGE-IND
               IF WS-ISA-VERSION NOT = '00501'
                   PERFORM 8200-LOG-VERSION-ERROR
               END-IF
           END-IF.

       3100-PROCESS-GS.
           PERFORM 2300-PARSE-ELEMENTS
           ADD 1 TO WS-FUNC-GROUPS
           MOVE WS-ELEMENT(1) TO WS-GS-FUNC-CODE
           MOVE WS-ELEMENT(2) TO WS-GS-SENDER-CODE
           MOVE WS-ELEMENT(3) TO WS-GS-RECV-CODE
           MOVE WS-ELEMENT(4) TO WS-GS-DATE
           MOVE WS-ELEMENT(5) TO WS-GS-TIME
           MOVE WS-ELEMENT(6) TO WS-GS-CONTROL-NUM
           MOVE WS-ELEMENT(7) TO WS-GS-RESP-AGENCY
           MOVE WS-ELEMENT(8) TO WS-GS-VERSION
           IF WS-GS-FUNC-CODE NOT = 'HC'
               PERFORM 8100-LOG-SNIP1-ERROR
           END-IF.

       3200-PROCESS-ST.
           PERFORM 2300-PARSE-ELEMENTS
           ADD 1 TO WS-TRANS-SETS
           MOVE 0 TO WS-ST-SEGMENT-COUNT
           INITIALIZE WS-CLAIM-DATA
           INITIALIZE WS-BILLING-PROVIDER
           INITIALIZE WS-SUBSCRIBER
           INITIALIZE WS-DIAGNOSIS-DATA
           MOVE 0 TO WS-SVC-LINE-COUNT
           MOVE WS-ELEMENT(2) TO WS-SE-CONTROL-NUM
           IF WS-ELEMENT(1) = '837'
               CONTINUE
           ELSE
               PERFORM 8100-LOG-SNIP1-ERROR
           END-IF
           IF WS-ELEMENT(3) = '005010X222A1'
               MOVE '837P ' TO WS-TRANS-TYPE-CODE
               MOVE WS-ELEMENT(3) TO WS-IMPL-GUIDE-VER
           ELSE IF WS-ELEMENT(3) = '005010X223A3'
               MOVE '837I ' TO WS-TRANS-TYPE-CODE
               MOVE WS-ELEMENT(3) TO WS-IMPL-GUIDE-VER
           ELSE
               PERFORM 8200-LOG-VERSION-ERROR
           END-IF.

       3300-PROCESS-BHT.
           PERFORM 2300-PARSE-ELEMENTS
           IF WS-ELEMENT(1) NOT = '0019'
               PERFORM 8100-LOG-SNIP1-ERROR
           END-IF
           IF WS-ELEMENT(2) = '00'
               CONTINUE
           ELSE IF WS-ELEMENT(2) = '18'
               CONTINUE
           ELSE
               PERFORM 8100-LOG-SNIP1-ERROR
           END-IF.

       4000-PROCESS-NM1.
           PERFORM 2300-PARSE-ELEMENTS
           EVALUATE WS-ELEMENT(1)
               WHEN '85'
                   MOVE '2000A' TO WS-CURRENT-LOOP
                   MOVE WS-ELEMENT(3) TO WS-BP-LAST-NAME
                   MOVE WS-ELEMENT(4) TO WS-BP-FIRST-NAME
                   IF WS-ELEMENT(8) = 'XX'
                       MOVE WS-ELEMENT(9) TO WS-BP-NPI
                   END-IF
               WHEN '87'
                   MOVE '210AB' TO WS-CURRENT-LOOP
               WHEN 'IL'
                   MOVE '2000B' TO WS-CURRENT-LOOP
                   MOVE WS-ELEMENT(3) TO WS-SUB-LAST-NAME
                   MOVE WS-ELEMENT(4) TO WS-SUB-FIRST-NAME
                   IF WS-ELEMENT(8) = 'MI'
                       MOVE WS-ELEMENT(9) TO WS-SUB-MEMBER-ID
                   END-IF
               WHEN '82'
                   MOVE '2310 ' TO WS-CURRENT-LOOP
               WHEN '77'
                   CONTINUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       4100-PROCESS-N3.
           PERFORM 2300-PARSE-ELEMENTS
           IF IN-LOOP-2000A OR IN-LOOP-2010AA
               MOVE WS-ELEMENT(1) TO WS-BP-ADDR-LINE1
               IF WS-ELEMENT-COUNT >= 2
                   MOVE WS-ELEMENT(2) TO WS-BP-ADDR-LINE2
               END-IF
           END-IF.

       4200-PROCESS-N4.
           PERFORM 2300-PARSE-ELEMENTS
           IF IN-LOOP-2000A OR IN-LOOP-2010AA
               MOVE WS-ELEMENT(1) TO WS-BP-CITY
               MOVE WS-ELEMENT(2) TO WS-BP-STATE
               MOVE WS-ELEMENT(3) TO WS-BP-ZIP
           END-IF.

       4300-PROCESS-REF.
           PERFORM 2300-PARSE-ELEMENTS
           EVALUATE WS-ELEMENT(1)
               WHEN 'EI'
                   IF IN-LOOP-2000A
                       MOVE WS-ELEMENT(2) TO WS-BP-TAX-ID
                       MOVE 'EI' TO WS-BP-TAX-QUAL
                   END-IF
               WHEN 'SY'
                   IF IN-LOOP-2000A
                       MOVE WS-ELEMENT(2) TO WS-BP-TAX-ID
                       MOVE 'SY' TO WS-BP-TAX-QUAL
                   END-IF
               WHEN 'EA'
                   CONTINUE
               WHEN 'G2'
                   CONTINUE
               WHEN 'D9'
                   MOVE WS-ELEMENT(2) TO WS-CLM-AUTH-NUM
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       5000-PROCESS-CLM.
           PERFORM 2300-PARSE-ELEMENTS
           ADD 1 TO WS-CLAIMS-READ
           MOVE '2300 ' TO WS-CURRENT-LOOP
           MOVE WS-ELEMENT(1) TO WS-CLM-SUBMITTER-ID
           COMPUTE WS-CLM-TOTAL-CHARGE =
               FUNCTION NUMVAL(WS-ELEMENT(2))
           MOVE WS-ELEMENT(5)(1:2) TO WS-CLM-PLACE-OF-SVC
           MOVE WS-ELEMENT(5)(3:1) TO WS-CLM-FREQUENCY
           MOVE WS-ELEMENT(6) TO WS-CLM-SIGNATURE
           MOVE WS-ELEMENT(7) TO WS-CLM-ASSIGN-BENE
           MOVE WS-ELEMENT(8) TO WS-CLM-RELEASE-INFO
           IF WS-CLM-TOTAL-CHARGE <= 0
               PERFORM 8300-LOG-CLAIM-ERROR
           END-IF
           PERFORM 7100-VALIDATE-CLAIM.

       5100-PROCESS-DTP.
           PERFORM 2300-PARSE-ELEMENTS
           EVALUATE WS-ELEMENT(1)
               WHEN '431'
                   MOVE WS-ELEMENT(3) TO WS-CLM-ONSET-DATE
               WHEN '472'
                   IF WS-ELEMENT(2) = 'D8'
                       MOVE WS-ELEMENT(3) TO WS-CLM-FROM-DATE
                       MOVE WS-ELEMENT(3) TO WS-CLM-THRU-DATE
                   ELSE IF WS-ELEMENT(2) = 'RD8'
                       MOVE WS-ELEMENT(3)(1:8)
                           TO WS-CLM-FROM-DATE
                       MOVE WS-ELEMENT(3)(10:8)
                           TO WS-CLM-THRU-DATE
                   END-IF
               WHEN '435'
                   MOVE WS-ELEMENT(3) TO WS-CLM-ADMIT-DATE
               WHEN '096'
                   MOVE WS-ELEMENT(3) TO WS-CLM-DISCH-DATE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       5200-PROCESS-HI.
           PERFORM 2300-PARSE-ELEMENTS
           PERFORM VARYING WS-DIAG-IDX FROM 1 BY 1
               UNTIL WS-DIAG-IDX > WS-ELEMENT-COUNT
               OR WS-DIAG-IDX > 12
               IF WS-ELEMENT(WS-DIAG-IDX) NOT = SPACES
                   ADD 1 TO WS-DIAG-COUNT
                   UNSTRING WS-ELEMENT(WS-DIAG-IDX)
                       DELIMITED BY WS-SUB-DELIM
                       INTO WS-DIAG-QUAL(WS-DIAG-COUNT)
                            WS-DIAG-CODE(WS-DIAG-COUNT)
                   END-UNSTRING
               END-IF
           END-PERFORM.

       5300-PROCESS-SV1.
           IF NOT IS-837P
               PERFORM 8100-LOG-SNIP1-ERROR
           ELSE
               PERFORM 2300-PARSE-ELEMENTS
               ADD 1 TO WS-SVC-LINE-COUNT
               MOVE WS-SVC-LINE-COUNT TO WS-LINE-IDX
               UNSTRING WS-ELEMENT(1) DELIMITED BY WS-SUB-DELIM
                   INTO WS-WORK-FIELDS
                        WS-SL-PROC-CODE(WS-LINE-IDX)
                        WS-SL-MODIFIER(WS-LINE-IDX, 1)
                        WS-SL-MODIFIER(WS-LINE-IDX, 2)
                        WS-SL-MODIFIER(WS-LINE-IDX, 3)
                        WS-SL-MODIFIER(WS-LINE-IDX, 4)
               END-UNSTRING
               COMPUTE WS-SL-CHARGE(WS-LINE-IDX) =
                   FUNCTION NUMVAL(WS-ELEMENT(2))
               MOVE WS-ELEMENT(3) TO WS-SL-UNITS(WS-LINE-IDX)
               MOVE WS-ELEMENT(4) TO WS-SL-POS(WS-LINE-IDX)
               PERFORM 7200-VALIDATE-SERVICE-LINE
           END-IF.

       5400-PROCESS-SV2.
           IF NOT IS-837I
               PERFORM 8100-LOG-SNIP1-ERROR
           ELSE
               PERFORM 2300-PARSE-ELEMENTS
               ADD 1 TO WS-SVC-LINE-COUNT
               MOVE WS-SVC-LINE-COUNT TO WS-LINE-IDX
               MOVE WS-ELEMENT(1) TO WS-SL-REV-CODE(WS-LINE-IDX)
               UNSTRING WS-ELEMENT(2) DELIMITED BY WS-SUB-DELIM
                   INTO WS-WORK-FIELDS
                        WS-SL-PROC-CODE(WS-LINE-IDX)
               END-UNSTRING
               COMPUTE WS-SL-CHARGE(WS-LINE-IDX) =
                   FUNCTION NUMVAL(WS-ELEMENT(3))
               MOVE WS-ELEMENT(4) TO WS-SL-UNITS(WS-LINE-IDX)
           END-IF.

       6000-PROCESS-SE.
           PERFORM 2300-PARSE-ELEMENTS
           ADD 1 TO WS-ST-SEGMENT-COUNT
           IF WS-ERROR-COUNT = 0
               ADD 1 TO WS-CLAIMS-ACCEPTED
               PERFORM 7000-WRITE-CLAIM-RECORD
           ELSE
               ADD 1 TO WS-CLAIMS-REJECTED
           END-IF
           PERFORM 8000-GENERATE-999-ACK.

       6100-PROCESS-GE.
           PERFORM 2300-PARSE-ELEMENTS
           CONTINUE.

       6200-PROCESS-IEA.
           PERFORM 2300-PARSE-ELEMENTS
           IF WS-ISA-ACK-REQ = '1'
               PERFORM 8500-GENERATE-TA1
           END-IF.

       7000-WRITE-CLAIM-RECORD.
           STRING
               WS-CLM-SUBMITTER-ID DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               WS-BP-NPI DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               WS-SUB-MEMBER-ID DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               WS-CLM-FROM-DATE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-CLM-THRU-DATE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-TRANS-TYPE-CODE DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               WS-CLM-PLACE-OF-SVC DELIMITED BY SIZE
               INTO CLAIM-OUTPUT-RECORD
           END-STRING
           WRITE CLAIM-OUTPUT-RECORD.

       7100-VALIDATE-CLAIM.
           MOVE 0 TO WS-ERROR-COUNT
           IF WS-CLM-SUBMITTER-ID = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'CLM' TO WS-ERR-SEGMENT(WS-ERROR-COUNT)
               MOVE 1 TO WS-ERR-ELEMENT(WS-ERROR-COUNT)
               MOVE '1   ' TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'MISSING CLAIM SUBMITTER ID'
                   TO WS-ERR-DESC(WS-ERROR-COUNT)
           END-IF
           IF WS-CLM-TOTAL-CHARGE <= 0
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'CLM' TO WS-ERR-SEGMENT(WS-ERROR-COUNT)
               MOVE 2 TO WS-ERR-ELEMENT(WS-ERROR-COUNT)
               MOVE '6   ' TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'INVALID TOTAL CHARGE AMOUNT'
                   TO WS-ERR-DESC(WS-ERROR-COUNT)
           END-IF
           IF WS-BP-NPI = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'NM1' TO WS-ERR-SEGMENT(WS-ERROR-COUNT)
               MOVE 9 TO WS-ERR-ELEMENT(WS-ERROR-COUNT)
               MOVE '8   ' TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'MISSING BILLING PROVIDER NPI'
                   TO WS-ERR-DESC(WS-ERROR-COUNT)
           END-IF.

       7200-VALIDATE-SERVICE-LINE.
           IF WS-SL-PROC-CODE(WS-LINE-IDX) = SPACES
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'SV1' TO WS-ERR-SEGMENT(WS-ERROR-COUNT)
               MOVE 1 TO WS-ERR-ELEMENT(WS-ERROR-COUNT)
               MOVE '1   ' TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'MISSING PROCEDURE CODE'
                   TO WS-ERR-DESC(WS-ERROR-COUNT)
           END-IF
           IF WS-SL-CHARGE(WS-LINE-IDX) <= 0
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'SV1' TO WS-ERR-SEGMENT(WS-ERROR-COUNT)
               MOVE 2 TO WS-ERR-ELEMENT(WS-ERROR-COUNT)
               MOVE '6   ' TO WS-ERR-CODE(WS-ERROR-COUNT)
               MOVE 'INVALID LINE CHARGE AMOUNT'
                   TO WS-ERR-DESC(WS-ERROR-COUNT)
           END-IF.

       8000-GENERATE-999-ACK.
           MOVE '999' TO WS-ACK-TYPE
           IF WS-ERROR-COUNT = 0
               MOVE 'A' TO WS-ACK-STATUS
           ELSE
               MOVE 'R' TO WS-ACK-STATUS
           END-IF
           STRING
               'ST*999*0001~' DELIMITED BY SIZE
               'AK1*HC*' DELIMITED BY SIZE
               WS-GS-CONTROL-NUM DELIMITED BY '  '
               '~' DELIMITED BY SIZE
               INTO ACK-OUTPUT-RECORD
           END-STRING
           WRITE ACK-OUTPUT-RECORD
           INITIALIZE ACK-OUTPUT-RECORD
           STRING
               'AK2*837*' DELIMITED BY SIZE
               WS-SE-CONTROL-NUM DELIMITED BY '  '
               '~' DELIMITED BY SIZE
               INTO ACK-OUTPUT-RECORD
           END-STRING
           WRITE ACK-OUTPUT-RECORD
           IF WS-ERROR-COUNT > 0
               PERFORM VARYING WS-SUB-IDX FROM 1 BY 1
                   UNTIL WS-SUB-IDX > WS-ERROR-COUNT
                   INITIALIZE ACK-OUTPUT-RECORD
                   STRING
                       'IK3*' DELIMITED BY SIZE
                       WS-ERR-SEGMENT(WS-SUB-IDX) DELIMITED BY SIZE
                       '*1**' DELIMITED BY SIZE
                       WS-ERR-CODE(WS-SUB-IDX) DELIMITED BY '  '
                       '~' DELIMITED BY SIZE
                       INTO ACK-OUTPUT-RECORD
                   END-STRING
                   WRITE ACK-OUTPUT-RECORD
               END-PERFORM
           END-IF.

       8100-LOG-SNIP1-ERROR.
           MOVE 1 TO WS-SNIP-LEVEL
           ADD 1 TO WS-ERROR-COUNT
           MOVE WS-SEGMENT-ID TO WS-ERR-SEGMENT(WS-ERROR-COUNT)
           MOVE 0 TO WS-ERR-ELEMENT(WS-ERROR-COUNT)
           MOVE 'S1  ' TO WS-ERR-CODE(WS-ERROR-COUNT)
           MOVE 'SNIP LEVEL 1 - INTEGRITY FAILURE'
               TO WS-ERR-DESC(WS-ERROR-COUNT)
           STRING
               'ERR|SNIP1|' DELIMITED BY SIZE
               WS-SEGMENT-ID DELIMITED BY SIZE
               '|INTEGRITY FAILURE|'  DELIMITED BY SIZE
               WS-ISA-CONTROL-NUM DELIMITED BY '  '
               INTO ERROR-LOG-RECORD
           END-STRING
           WRITE ERROR-LOG-RECORD.

       8200-LOG-VERSION-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           MOVE WS-SEGMENT-ID TO WS-ERR-SEGMENT(WS-ERROR-COUNT)
           MOVE 0 TO WS-ERR-ELEMENT(WS-ERROR-COUNT)
           MOVE 'VER ' TO WS-ERR-CODE(WS-ERROR-COUNT)
           MOVE 'UNSUPPORTED VERSION/IMPL GUIDE'
               TO WS-ERR-DESC(WS-ERROR-COUNT).

       8300-LOG-CLAIM-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           MOVE 'CLM' TO WS-ERR-SEGMENT(WS-ERROR-COUNT)
           MOVE 2 TO WS-ERR-ELEMENT(WS-ERROR-COUNT)
           MOVE 'AMT ' TO WS-ERR-CODE(WS-ERROR-COUNT)
           MOVE 'CLAIM TOTAL CHARGE IS ZERO OR NEGATIVE'
               TO WS-ERR-DESC(WS-ERROR-COUNT).

       8500-GENERATE-TA1.
           MOVE 'TA1' TO WS-ACK-TYPE
           INITIALIZE ACK-OUTPUT-RECORD
           STRING
               'ISA*00*          *00*          *'
                   DELIMITED BY SIZE
               WS-ISA-RECV-QUAL DELIMITED BY SIZE
               '*' DELIMITED BY SIZE
               WS-ISA-RECV-ID DELIMITED BY SIZE
               '*' DELIMITED BY SIZE
               WS-ISA-SENDER-QUAL DELIMITED BY SIZE
               '*' DELIMITED BY SIZE
               WS-ISA-SENDER-ID DELIMITED BY SIZE
               INTO ACK-OUTPUT-RECORD
           END-STRING
           WRITE ACK-OUTPUT-RECORD
           INITIALIZE ACK-OUTPUT-RECORD
           STRING
               'TA1*' DELIMITED BY SIZE
               WS-ISA-CONTROL-NUM DELIMITED BY '  '
               '*' DELIMITED BY SIZE
               WS-ISA-DATE DELIMITED BY SIZE
               '*' DELIMITED BY SIZE
               WS-ISA-TIME DELIMITED BY SIZE
               '*A~' DELIMITED BY SIZE
               INTO ACK-OUTPUT-RECORD
           END-STRING
           WRITE ACK-OUTPUT-RECORD.

       9000-FINALIZE.
           CLOSE EDI-INBOUND-FILE
                 CLAIM-OUTPUT-FILE
                 ACK-OUTPUT-FILE
                 ERROR-LOG-FILE
           ACCEPT WS-TS-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TS-TIME FROM TIME
           DISPLAY 'WCEDI837: COMPLETED AT ' WS-TS-DATE
                   ' ' WS-TS-TIME
           DISPLAY 'WCEDI837: LINES READ      = ' WS-LINES-READ
           DISPLAY 'WCEDI837: SEGMENTS PARSED  = ' WS-SEGMENTS-PARSED
           DISPLAY 'WCEDI837: TRANS SETS       = ' WS-TRANS-SETS
           DISPLAY 'WCEDI837: CLAIMS READ      = ' WS-CLAIMS-READ
           DISPLAY 'WCEDI837: CLAIMS ACCEPTED  = ' WS-CLAIMS-ACCEPTED
           DISPLAY 'WCEDI837: CLAIMS REJECTED  = ' WS-CLAIMS-REJECTED.
