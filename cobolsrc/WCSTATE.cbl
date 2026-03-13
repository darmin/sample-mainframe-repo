       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCSTATE.
      *================================================================*
      * PROGRAM:    WCSTATE                                            *
      * AUTHOR:     MAINFRAMEMOD TPA SYSTEMS                           *
      * DATE:       2024-03-10                                         *
      * PURPOSE:    STATE FILING AND COMPLIANCE REPORTING              *
      *             FROI/SROI GENERATION PER IAIABC CLM EDI REL 3.1   *
      *             JURISDICTION-SPECIFIC FORMATTING AND DEADLINES     *
      * PLATFORM:   HPE NONSTOP / GUARDIAN                             *
      *================================================================*
      * CHANGE LOG:                                                    *
      * 2024-03-10  INITIAL DEVELOPMENT - FROI SUPPORT                 *
      * 2024-05-22  ADDED SROI TRANSACTIONS                            *
      * 2024-08-14  ADDED JURISDICTION DEADLINE TRACKING               *
      * 2024-11-01  ENHANCED MTC PROCESSING (00/01/02/AU)              *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIM-INPUT-FILE
               ASSIGN TO CLMIN
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CLM-FILE-STATUS.
           SELECT FILING-OUTPUT-FILE
               ASSIGN TO FILOUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FIL-FILE-STATUS.
           SELECT DEADLINE-REPORT-FILE
               ASSIGN TO DLNRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-DLN-FILE-STATUS.
           SELECT ACK-INPUT-FILE
               ASSIGN TO ACKIN
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ACK-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIM-INPUT-FILE.
       01  CLAIM-INPUT-RECORD.
           05  CI-CLAIM-NUM           PIC X(20).
           05  CI-JURISDICTION        PIC X(2).
           05  CI-INJURY-DATE         PIC X(8).
           05  CI-REPORT-DATE         PIC X(8).
           05  CI-EMPLOYER-FEIN       PIC X(9).
           05  CI-EMPLOYER-NAME       PIC X(60).
           05  CI-EMPLOYER-ADDR       PIC X(55).
           05  CI-EMPLOYER-CITY       PIC X(30).
           05  CI-EMPLOYER-STATE      PIC XX.
           05  CI-EMPLOYER-ZIP        PIC X(9).
           05  CI-EMPLOYER-SIC        PIC X(4).
           05  CI-CLAIMANT-SSN        PIC X(9).
           05  CI-CLAIMANT-LAST       PIC X(35).
           05  CI-CLAIMANT-FIRST      PIC X(25).
           05  CI-CLAIMANT-DOB        PIC X(8).
           05  CI-CLAIMANT-GENDER     PIC X.
           05  CI-CLAIMANT-ADDR       PIC X(55).
           05  CI-CLAIMANT-CITY       PIC X(30).
           05  CI-CLAIMANT-STATE      PIC XX.
           05  CI-CLAIMANT-ZIP        PIC X(9).
           05  CI-INJURY-DESC         PIC X(80).
           05  CI-BODY-PART-CODE      PIC X(2).
           05  CI-NATURE-CODE         PIC X(2).
           05  CI-CAUSE-CODE          PIC X(2).
           05  CI-OCCUPATION-CODE     PIC X(6).
           05  CI-WAGE-AMOUNT         PIC S9(5)V99 COMP-3.
           05  CI-WAGE-PERIOD         PIC X.
               88  WAGE-WEEKLY        VALUE 'W'.
               88  WAGE-BIWEEKLY      VALUE 'B'.
               88  WAGE-MONTHLY       VALUE 'M'.
           05  CI-DAYS-LOST           PIC 9(3).
           05  CI-RETURN-TO-WORK      PIC X(8).
           05  CI-CLAIM-STATUS        PIC X(2).
               88  STATUS-OPEN        VALUE 'OP'.
               88  STATUS-CLOSED      VALUE 'CL'.
               88  STATUS-REOPENED    VALUE 'RO'.
           05  CI-MTC-CODE            PIC X(2).
               88  MTC-ORIGINAL       VALUE '00'.
               88  MTC-CANCEL         VALUE '01'.
               88  MTC-CHANGE         VALUE '02'.
               88  MTC-ACQUIRED       VALUE 'AU'.
           05  CI-FILING-TYPE         PIC X(4).
               88  FILING-FROI        VALUE 'FROI'.
               88  FILING-SROI        VALUE 'SROI'.
           05  CI-INSURER-FEIN        PIC X(9).
           05  CI-INSURER-NAME        PIC X(60).
           05  CI-POLICY-NUM          PIC X(20).
           05  CI-PREV-FILING-DATE    PIC X(8).
           05  FILLER                 PIC X(20).

       FD  FILING-OUTPUT-FILE
           RECORDING MODE IS V
           RECORD CONTAINS 1 TO 2048 CHARACTERS.
       01  FILING-OUTPUT-RECORD        PIC X(2048).

       FD  DEADLINE-REPORT-FILE.
       01  DEADLINE-REPORT-RECORD      PIC X(132).

       FD  ACK-INPUT-FILE.
       01  ACK-INPUT-RECORD.
           05  AI-CLAIM-NUM           PIC X(20).
           05  AI-JURISDICTION        PIC X(2).
           05  AI-STATUS-CODE         PIC X(2).
               88  AI-ACCEPTED        VALUE 'TA'.
               88  AI-REJECTED        VALUE 'TR'.
               88  AI-DUPLICATE       VALUE 'TD'.
           05  AI-ERROR-CODE          PIC X(4).
           05  AI-ERROR-DESC          PIC X(80).
           05  AI-FILING-DATE         PIC X(8).
           05  FILLER                 PIC X(16).

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCCLMCPY
           COPY WCEMPCPY
       01  WS-FILE-STATUSES.
           05  WS-CLM-FILE-STATUS     PIC XX.
           05  WS-FIL-FILE-STATUS     PIC XX.
           05  WS-DLN-FILE-STATUS     PIC XX.
           05  WS-ACK-FILE-STATUS     PIC XX.

       01  WS-FLAGS.
           05  WS-EOF-FLAG            PIC X VALUE 'N'.
               88  END-OF-FILE        VALUE 'Y'.
           05  WS-ACK-EOF             PIC X VALUE 'N'.
               88  ACK-END-OF-FILE    VALUE 'Y'.

       01  WS-DATES.
           05  WS-CURRENT-DATE        PIC X(8).
           05  WS-FILING-DEADLINE     PIC X(8).
           05  WS-DAYS-REMAINING      PIC S9(5) COMP-3.
           05  WS-JULIAN-CURRENT      PIC 9(7).
           05  WS-JULIAN-DEADLINE     PIC 9(7).
           05  WS-JULIAN-INJURY       PIC 9(7).
           05  WS-WORK-YYYY           PIC 9(4).
           05  WS-WORK-MM             PIC 9(2).
           05  WS-WORK-DD             PIC 9(2).

       01  WS-JURISDICTION-TABLE.
           05  WS-JURIS-ENTRY OCCURS 55 TIMES.
               10  WS-JURIS-CODE      PIC XX.
               10  WS-JURIS-FROI-DAYS PIC 9(3).
               10  WS-JURIS-SROI-DAYS PIC 9(3).
               10  WS-JURIS-ELECT-REQ PIC X.
                   88  JURIS-ELECT-ONLY  VALUE 'E'.
                   88  JURIS-PAPER-OK    VALUE 'P'.
                   88  JURIS-BOTH-OK     VALUE 'B'.
               10  WS-JURIS-PENALTY    PIC S9(5)V99 COMP-3.
       01  WS-JURIS-COUNT             PIC 99 VALUE 0.

       01  WS-JURIS-DATA-INIT.
           05  FILLER PIC X(13) VALUE 'AL01003000E00000'.
           05  FILLER PIC X(13) VALUE 'AK01003000E00000'.
           05  FILLER PIC X(13) VALUE 'AZ01003000E00000'.
           05  FILLER PIC X(13) VALUE 'CA00503000E10000'.
           05  FILLER PIC X(13) VALUE 'CO01003000E00000'.
           05  FILLER PIC X(13) VALUE 'FL00703000E05000'.
           05  FILLER PIC X(13) VALUE 'GA02103000E01000'.
           05  FILLER PIC X(13) VALUE 'IL01403000E00000'.
           05  FILLER PIC X(13) VALUE 'IN00703000E00500'.
           05  FILLER PIC X(13) VALUE 'KY01403000B00000'.
           05  FILLER PIC X(13) VALUE 'LA01003000E00000'.
           05  FILLER PIC X(13) VALUE 'MA00703000E00000'.
           05  FILLER PIC X(13) VALUE 'MI02803000B00000'.
           05  FILLER PIC X(13) VALUE 'MN01403000E00000'.
           05  FILLER PIC X(13) VALUE 'MO01403000E00000'.
           05  FILLER PIC X(13) VALUE 'NC01403000E00000'.
           05  FILLER PIC X(13) VALUE 'NJ02103000E00000'.
           05  FILLER PIC X(13) VALUE 'NY01003000E02500'.
           05  FILLER PIC X(13) VALUE 'OH00703000E00000'.
           05  FILLER PIC X(13) VALUE 'PA02103000B00000'.
           05  FILLER PIC X(13) VALUE 'TX00803000E00000'.
           05  FILLER PIC X(13) VALUE 'VA01003000E00000'.
           05  FILLER PIC X(13) VALUE 'WA01003000E00000'.

       01  WS-IAIABC-HEADER.
           05  WS-HDR-TRANS-ID        PIC X(6).
           05  WS-HDR-VERSION         PIC X(4) VALUE '3.10'.
           05  WS-HDR-SENDER-ID       PIC X(15).
           05  WS-HDR-RECEIVER-ID     PIC X(15).
           05  WS-HDR-CREATION-DATE   PIC X(8).
           05  WS-HDR-CREATION-TIME   PIC X(6).
           05  WS-HDR-BATCH-NUM       PIC 9(6).

       01  WS-FILING-RECORD.
           05  WS-FR-MTC              PIC X(2).
           05  WS-FR-JURIS-CODE       PIC X(2).
           05  WS-FR-CLAIM-NUM        PIC X(20).
           05  WS-FR-INSURER-FEIN     PIC X(9).
           05  WS-FR-EMPLOYER-FEIN    PIC X(9).
           05  WS-FR-EMP-SIC          PIC X(4).
           05  WS-FR-SSN              PIC X(9).
           05  WS-FR-LAST-NAME        PIC X(35).
           05  WS-FR-FIRST-NAME       PIC X(25).
           05  WS-FR-DOB              PIC X(8).
           05  WS-FR-GENDER           PIC X.
           05  WS-FR-INJURY-DATE      PIC X(8).
           05  WS-FR-INJURY-DESC      PIC X(80).
           05  WS-FR-BODY-PART        PIC X(2).
           05  WS-FR-NATURE           PIC X(2).
           05  WS-FR-CAUSE            PIC X(2).
           05  WS-FR-WAGE-AMT         PIC S9(5)V99 COMP-3.
           05  WS-FR-WAGE-PERIOD      PIC X.
           05  WS-FR-DAYS-LOST        PIC 9(3).
           05  WS-FR-RTW-DATE         PIC X(8).
           05  WS-FR-CLAIM-STATUS     PIC X(2).

       01  WS-COUNTERS.
           05  WS-CLAIMS-READ         PIC 9(7) VALUE 0.
           05  WS-FROI-GENERATED      PIC 9(7) VALUE 0.
           05  WS-SROI-GENERATED      PIC 9(7) VALUE 0.
           05  WS-PAST-DEADLINE       PIC 9(5) VALUE 0.
           05  WS-ACKS-PROCESSED      PIC 9(7) VALUE 0.
           05  WS-ACKS-ACCEPTED       PIC 9(7) VALUE 0.
           05  WS-ACKS-REJECTED       PIC 9(7) VALUE 0.

       01  WS-JURIS-IDX              PIC 99.
       01  WS-FOUND-FLAG             PIC X VALUE 'N'.
           88  JURIS-FOUND           VALUE 'Y'.
       01  WS-RPT-LINE               PIC X(132).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
      *
      *    Inter-program communication calls
           CALL "WCJURIS"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CLAIMS
           PERFORM 5000-PROCESS-ACKNOWLEDGMENTS
           PERFORM 6000-GENERATE-DEADLINE-REPORT
           PERFORM 9000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT  CLAIM-INPUT-FILE
                       ACK-INPUT-FILE
           OPEN OUTPUT FILING-OUTPUT-FILE
                       DEADLINE-REPORT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           PERFORM 1100-LOAD-JURISDICTION-TABLE
           INITIALIZE WS-COUNTERS
           ADD 1 TO WS-HDR-BATCH-NUM
           MOVE WS-CURRENT-DATE TO WS-HDR-CREATION-DATE
           DISPLAY 'WCSTATE: STARTED - STATE FILING GENERATION'.

       1100-LOAD-JURISDICTION-TABLE.
           MOVE 23 TO WS-JURIS-COUNT
           PERFORM VARYING WS-JURIS-IDX FROM 1 BY 1
               UNTIL WS-JURIS-IDX > 23
               EVALUATE WS-JURIS-IDX
                   WHEN 1  MOVE 'AL' TO WS-JURIS-CODE(1)
                            MOVE 010 TO WS-JURIS-FROI-DAYS(1)
                            MOVE 030 TO WS-JURIS-SROI-DAYS(1)
                   WHEN 2  MOVE 'CA' TO WS-JURIS-CODE(2)
                            MOVE 005 TO WS-JURIS-FROI-DAYS(2)
                            MOVE 030 TO WS-JURIS-SROI-DAYS(2)
                   WHEN 3  MOVE 'FL' TO WS-JURIS-CODE(3)
                            MOVE 007 TO WS-JURIS-FROI-DAYS(3)
                            MOVE 030 TO WS-JURIS-SROI-DAYS(3)
                   WHEN 4  MOVE 'GA' TO WS-JURIS-CODE(4)
                            MOVE 021 TO WS-JURIS-FROI-DAYS(4)
                            MOVE 030 TO WS-JURIS-SROI-DAYS(4)
                   WHEN 5  MOVE 'IL' TO WS-JURIS-CODE(5)
                            MOVE 014 TO WS-JURIS-FROI-DAYS(5)
                            MOVE 030 TO WS-JURIS-SROI-DAYS(5)
                   WHEN 6  MOVE 'NY' TO WS-JURIS-CODE(6)
                            MOVE 010 TO WS-JURIS-FROI-DAYS(6)
                            MOVE 030 TO WS-JURIS-SROI-DAYS(6)
                   WHEN 7  MOVE 'TX' TO WS-JURIS-CODE(7)
                            MOVE 008 TO WS-JURIS-FROI-DAYS(7)
                            MOVE 030 TO WS-JURIS-SROI-DAYS(7)
                   WHEN OTHER
                       MOVE 'XX' TO WS-JURIS-CODE(WS-JURIS-IDX)
                       MOVE 014 TO
                           WS-JURIS-FROI-DAYS(WS-JURIS-IDX)
                       MOVE 030 TO
                           WS-JURIS-SROI-DAYS(WS-JURIS-IDX)
               END-EVALUATE
           END-PERFORM.

       2000-PROCESS-CLAIMS.
           READ CLAIM-INPUT-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-CLAIMS-READ
               EVALUATE TRUE
                   WHEN FILING-FROI
                       PERFORM 3000-GENERATE-FROI
                   WHEN FILING-SROI
                       PERFORM 4000-GENERATE-SROI
               END-EVALUATE
               READ CLAIM-INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

       3000-GENERATE-FROI.
           PERFORM 3100-BUILD-FROI-HEADER
           PERFORM 3200-BUILD-EMPLOYER-RECORD
           PERFORM 3300-BUILD-CLAIMANT-RECORD
           PERFORM 3400-BUILD-INJURY-RECORD
           PERFORM 3500-CHECK-FILING-DEADLINE
           ADD 1 TO WS-FROI-GENERATED.

       3100-BUILD-FROI-HEADER.
           MOVE 'FROIHD' TO WS-HDR-TRANS-ID
           INITIALIZE FILING-OUTPUT-RECORD
           STRING
               'FROI|' DELIMITED BY SIZE
               CI-MTC-CODE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-JURISDICTION DELIMITED BY SIZE
               '|3.10|' DELIMITED BY SIZE
               CI-CLAIM-NUM DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-INSURER-FEIN DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-INSURER-NAME DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-POLICY-NUM DELIMITED BY '  '
               INTO FILING-OUTPUT-RECORD
           END-STRING
           WRITE FILING-OUTPUT-RECORD.

       3200-BUILD-EMPLOYER-RECORD.
           INITIALIZE FILING-OUTPUT-RECORD
           STRING
               'EMPR|' DELIMITED BY SIZE
               CI-EMPLOYER-FEIN DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-EMPLOYER-NAME DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-EMPLOYER-ADDR DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-EMPLOYER-CITY DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-EMPLOYER-STATE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-EMPLOYER-ZIP DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-EMPLOYER-SIC DELIMITED BY SIZE
               INTO FILING-OUTPUT-RECORD
           END-STRING
           WRITE FILING-OUTPUT-RECORD.

       3300-BUILD-CLAIMANT-RECORD.
           INITIALIZE FILING-OUTPUT-RECORD
           STRING
               'CLMT|' DELIMITED BY SIZE
               CI-CLAIMANT-SSN DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-LAST DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-FIRST DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-DOB DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-GENDER DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-ADDR DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-CITY DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-STATE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-ZIP DELIMITED BY SIZE
               INTO FILING-OUTPUT-RECORD
           END-STRING
           WRITE FILING-OUTPUT-RECORD.

       3400-BUILD-INJURY-RECORD.
           INITIALIZE FILING-OUTPUT-RECORD
           STRING
               'INJR|' DELIMITED BY SIZE
               CI-INJURY-DATE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-INJURY-DESC DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-BODY-PART-CODE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-NATURE-CODE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-CAUSE-CODE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-OCCUPATION-CODE DELIMITED BY '  '
               INTO FILING-OUTPUT-RECORD
           END-STRING
           WRITE FILING-OUTPUT-RECORD.

       3500-CHECK-FILING-DEADLINE.
           PERFORM 7000-LOOKUP-JURISDICTION
           IF JURIS-FOUND
               PERFORM 7100-CALC-DEADLINE
               IF WS-DAYS-REMAINING < 0
                   ADD 1 TO WS-PAST-DEADLINE
                   DISPLAY 'WCSTATE: WARNING - PAST DEADLINE '
                       CI-CLAIM-NUM ' JURIS=' CI-JURISDICTION
                       ' DAYS OVER=' WS-DAYS-REMAINING
               END-IF
           ELSE
               DISPLAY 'WCSTATE: UNKNOWN JURISDICTION '
                   CI-JURISDICTION ' FOR ' CI-CLAIM-NUM
           END-IF.

       4000-GENERATE-SROI.
           PERFORM 4100-BUILD-SROI-HEADER
           PERFORM 4200-BUILD-SROI-BENEFIT-DATA
           PERFORM 4300-BUILD-SROI-MEDICAL-DATA
           ADD 1 TO WS-SROI-GENERATED.

       4100-BUILD-SROI-HEADER.
           INITIALIZE FILING-OUTPUT-RECORD
           STRING
               'SROI|' DELIMITED BY SIZE
               CI-MTC-CODE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-JURISDICTION DELIMITED BY SIZE
               '|3.10|' DELIMITED BY SIZE
               CI-CLAIM-NUM DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-INSURER-FEIN DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIM-STATUS DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-PREV-FILING-DATE DELIMITED BY SIZE
               INTO FILING-OUTPUT-RECORD
           END-STRING
           WRITE FILING-OUTPUT-RECORD.

       4200-BUILD-SROI-BENEFIT-DATA.
           INITIALIZE FILING-OUTPUT-RECORD
           INITIALIZE WS-FILING-RECORD
           MOVE CI-WAGE-AMOUNT TO WS-FR-WAGE-AMT
           MOVE CI-WAGE-PERIOD TO WS-FR-WAGE-PERIOD
           MOVE CI-DAYS-LOST TO WS-FR-DAYS-LOST
           MOVE CI-RETURN-TO-WORK TO WS-FR-RTW-DATE
           STRING
               'BNFT|' DELIMITED BY SIZE
               WS-FR-WAGE-AMT DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-FR-WAGE-PERIOD DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-FR-DAYS-LOST DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-FR-RTW-DATE DELIMITED BY SIZE
               INTO FILING-OUTPUT-RECORD
           END-STRING
           WRITE FILING-OUTPUT-RECORD.

       4300-BUILD-SROI-MEDICAL-DATA.
           INITIALIZE FILING-OUTPUT-RECORD
           STRING
               'MEDC|' DELIMITED BY SIZE
               CI-BODY-PART-CODE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-NATURE-CODE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-CAUSE-CODE DELIMITED BY SIZE
               INTO FILING-OUTPUT-RECORD
           END-STRING
           WRITE FILING-OUTPUT-RECORD.

       5000-PROCESS-ACKNOWLEDGMENTS.
           MOVE 'N' TO WS-ACK-EOF
           READ ACK-INPUT-FILE
               AT END SET ACK-END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL ACK-END-OF-FILE
               ADD 1 TO WS-ACKS-PROCESSED
               EVALUATE TRUE
                   WHEN AI-ACCEPTED
                       ADD 1 TO WS-ACKS-ACCEPTED
                   WHEN AI-REJECTED
                       ADD 1 TO WS-ACKS-REJECTED
                       DISPLAY 'WCSTATE: REJECTED - '
                           AI-CLAIM-NUM ' JURIS=' AI-JURISDICTION
                           ' ERR=' AI-ERROR-CODE
                           ' ' AI-ERROR-DESC
                   WHEN AI-DUPLICATE
                       DISPLAY 'WCSTATE: DUPLICATE - '
                           AI-CLAIM-NUM
               END-EVALUATE
               READ ACK-INPUT-FILE
                   AT END SET ACK-END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

       6000-GENERATE-DEADLINE-REPORT.
           INITIALIZE WS-RPT-LINE
           STRING
               'STATE FILING DEADLINE COMPLIANCE REPORT'
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE DEADLINE-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING
               'RUN DATE: ' WS-CURRENT-DATE
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE DEADLINE-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           MOVE ALL '-' TO WS-RPT-LINE
           WRITE DEADLINE-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING
               'JURIS  FROI-DAYS  SROI-DAYS  STATUS'
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE DEADLINE-REPORT-RECORD FROM WS-RPT-LINE
           PERFORM VARYING WS-JURIS-IDX FROM 1 BY 1
               UNTIL WS-JURIS-IDX > WS-JURIS-COUNT
               INITIALIZE WS-RPT-LINE
               STRING
                   WS-JURIS-CODE(WS-JURIS-IDX) DELIMITED BY SIZE
                   '     ' DELIMITED BY SIZE
                   WS-JURIS-FROI-DAYS(WS-JURIS-IDX)
                       DELIMITED BY SIZE
                   '       ' DELIMITED BY SIZE
                   WS-JURIS-SROI-DAYS(WS-JURIS-IDX)
                       DELIMITED BY SIZE
                   '      ACTIVE' DELIMITED BY SIZE
                   INTO WS-RPT-LINE
               END-STRING
               WRITE DEADLINE-REPORT-RECORD FROM WS-RPT-LINE
           END-PERFORM.

       7000-LOOKUP-JURISDICTION.
           MOVE 'N' TO WS-FOUND-FLAG
           PERFORM VARYING WS-JURIS-IDX FROM 1 BY 1
               UNTIL WS-JURIS-IDX > WS-JURIS-COUNT
               OR JURIS-FOUND
               IF WS-JURIS-CODE(WS-JURIS-IDX) = CI-JURISDICTION
                   SET JURIS-FOUND TO TRUE
               END-IF
           END-PERFORM.

       7100-CALC-DEADLINE.
           COMPUTE WS-JULIAN-INJURY =
               FUNCTION INTEGER-OF-DATE(
                   FUNCTION NUMVAL(CI-INJURY-DATE))
           COMPUTE WS-JULIAN-CURRENT =
               FUNCTION INTEGER-OF-DATE(
                   FUNCTION NUMVAL(WS-CURRENT-DATE))
           IF FILING-FROI
               COMPUTE WS-JULIAN-DEADLINE =
                   WS-JULIAN-INJURY +
                   WS-JURIS-FROI-DAYS(WS-JURIS-IDX)
           ELSE
               COMPUTE WS-JULIAN-DEADLINE =
                   WS-JULIAN-INJURY +
                   WS-JURIS-SROI-DAYS(WS-JURIS-IDX)
           END-IF
           COMPUTE WS-DAYS-REMAINING =
               WS-JULIAN-DEADLINE - WS-JULIAN-CURRENT.

       9000-FINALIZE.
           CLOSE CLAIM-INPUT-FILE
                 FILING-OUTPUT-FILE
                 DEADLINE-REPORT-FILE
                 ACK-INPUT-FILE
           DISPLAY 'WCSTATE: COMPLETED'
           DISPLAY 'WCSTATE: CLAIMS READ         = ' WS-CLAIMS-READ
           DISPLAY 'WCSTATE: FROI GENERATED      = ' WS-FROI-GENERATED
           DISPLAY 'WCSTATE: SROI GENERATED      = ' WS-SROI-GENERATED
           DISPLAY 'WCSTATE: PAST DEADLINE       = ' WS-PAST-DEADLINE
           DISPLAY 'WCSTATE: ACKS PROCESSED      = ' WS-ACKS-PROCESSED
           DISPLAY 'WCSTATE: ACKS ACCEPTED       = ' WS-ACKS-ACCEPTED
           DISPLAY 'WCSTATE: ACKS REJECTED       = ' WS-ACKS-REJECTED.
