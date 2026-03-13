       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCMSA.
      *================================================================*
      * PROGRAM:    WCMSA                                              *
      * AUTHOR:     MAINFRAMEMOD TPA SYSTEMS                           *
      * DATE:       2024-05-12                                         *
      * PURPOSE:    MEDICARE SET-ASIDE (MSA) PROCESSING                *
      *             APPLICABILITY, ALLOCATION, CMS SUBMISSION,         *
      *             SECTION 111 MMSEA MANDATORY REPORTING              *
      * PLATFORM:   HPE NONSTOP / GUARDIAN                             *
      *================================================================*
      * CHANGE LOG:                                                    *
      * 2024-05-12  INITIAL DEVELOPMENT                                *
      * 2024-07-20  ADDED SECTION 111 MMSEA REPORTING                  *
      * 2024-09-15  ADDED FUND DEPLETION MONITORING                    *
      * 2024-12-01  ENHANCED CMS SUBMISSION FORMATTING                 *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIM-INPUT-FILE
               ASSIGN TO CLMIN
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CLM-FILE-STATUS.
           SELECT MSA-MASTER-FILE
               ASSIGN TO MSAMST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MM-MSA-KEY
               FILE STATUS IS WS-MSA-FILE-STATUS.
           SELECT CMS-SUBMISSION-FILE
               ASSIGN TO CMSSUB
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CMS-FILE-STATUS.
           SELECT SEC111-OUTPUT-FILE
               ASSIGN TO S111OUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-S111-FILE-STATUS.
           SELECT MSA-REPORT-FILE
               ASSIGN TO MSARPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-RPT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIM-INPUT-FILE.
       01  CLAIM-INPUT-RECORD.
           05  CI-CLAIM-NUM           PIC X(20).
           05  CI-CLAIMANT-SSN        PIC X(9).
           05  CI-CLAIMANT-HICN       PIC X(12).
           05  CI-CLAIMANT-MBI        PIC X(11).
           05  CI-CLAIMANT-LAST       PIC X(35).
           05  CI-CLAIMANT-FIRST      PIC X(25).
           05  CI-CLAIMANT-DOB        PIC X(8).
           05  CI-CLAIMANT-GENDER     PIC X.
           05  CI-CLAIMANT-ADDR       PIC X(55).
           05  CI-CLAIMANT-CITY       PIC X(30).
           05  CI-CLAIMANT-STATE      PIC XX.
           05  CI-CLAIMANT-ZIP        PIC X(5).
           05  CI-INJURY-DATE         PIC X(8).
           05  CI-SETTLEMENT-AMT      PIC S9(9)V99 COMP-3.
           05  CI-SETTLEMENT-DATE     PIC X(8).
           05  CI-TOTAL-PAID-MED      PIC S9(9)V99 COMP-3.
           05  CI-TOTAL-PAID-IND      PIC S9(9)V99 COMP-3.
           05  CI-FUTURE-MED-COST     PIC S9(9)V99 COMP-3.
           05  CI-MEDICARE-ELIGIBLE   PIC X.
               88  ALREADY-ON-MEDICARE VALUE 'Y'.
               88  NOT-ON-MEDICARE    VALUE 'N'.
           05  CI-SSDI-ELIGIBLE       PIC X.
               88  RECEIVING-SSDI     VALUE 'Y'.
               88  NOT-ON-SSDI        VALUE 'N'.
           05  CI-AGE-AT-SETTLEMENT   PIC 99.
           05  CI-LIFE-EXPECTANCY     PIC 99.
           05  CI-ICD-CODES           PIC X(7) OCCURS 8 TIMES.
           05  CI-INSURER-FEIN        PIC X(9).
           05  CI-INSURER-NAME        PIC X(60).
           05  CI-POLICY-NUM          PIC X(20).
           05  CI-RRE-ID              PIC X(9).
           05  FILLER                 PIC X(20).

       FD  MSA-MASTER-FILE.
       01  MSA-MASTER-RECORD.
           05  MM-MSA-KEY.
               10  MM-CLAIM-NUM       PIC X(20).
           05  MM-CLAIMANT-SSN        PIC X(9).
           05  MM-CLAIMANT-MBI        PIC X(11).
           05  MM-MSA-STATUS          PIC X(2).
               88  MSA-APPLICABLE     VALUE 'AP'.
               88  MSA-NOT-APPLICABLE VALUE 'NA'.
               88  MSA-SUBMITTED      VALUE 'SB'.
               88  MSA-APPROVED       VALUE 'AV'.
               88  MSA-DENIED         VALUE 'DN'.
               88  MSA-ADMIN-SETUP    VALUE 'AS'.
               88  MSA-ACTIVE         VALUE 'AC'.
               88  MSA-DEPLETED       VALUE 'DP'.
           05  MM-MSA-AMOUNT          PIC S9(9)V99 COMP-3.
           05  MM-CMS-APPROVED-AMT    PIC S9(9)V99 COMP-3.
           05  MM-ANNUAL-DEPOSIT      PIC S9(7)V99 COMP-3.
           05  MM-FUND-BALANCE        PIC S9(9)V99 COMP-3.
           05  MM-TOTAL-DISBURSED     PIC S9(9)V99 COMP-3.
           05  MM-ADMIN-NAME          PIC X(60).
           05  MM-ADMIN-TAX-ID        PIC X(9).
           05  MM-CREATION-DATE       PIC X(8).
           05  MM-SUBMISSION-DATE     PIC X(8).
           05  MM-APPROVAL-DATE       PIC X(8).
           05  MM-LAST-ATTEST-DATE    PIC X(8).
           05  MM-NEXT-ATTEST-DATE    PIC X(8).
           05  MM-SETTLEMENT-AMT      PIC S9(9)V99 COMP-3.
           05  MM-YEARS-ALLOCATED     PIC 99.
           05  MM-COND-PAY-AMT        PIC S9(9)V99 COMP-3.
           05  MM-COND-PAY-STATUS     PIC X.
               88  COND-PAY-NONE     VALUE 'N'.
               88  COND-PAY-PENDING  VALUE 'P'.
               88  COND-PAY-RESOLVED VALUE 'R'.
           05  MM-SEC111-REPORTED     PIC X VALUE 'N'.
               88  SEC111-YES        VALUE 'Y'.
               88  SEC111-NO         VALUE 'N'.
           05  FILLER                 PIC X(30).

       FD  CMS-SUBMISSION-FILE
           RECORDING MODE IS V
           RECORD CONTAINS 1 TO 2048 CHARACTERS.
       01  CMS-SUBMISSION-RECORD       PIC X(2048).

       FD  SEC111-OUTPUT-FILE.
       01  SEC111-OUTPUT-RECORD        PIC X(750).

       FD  MSA-REPORT-FILE.
       01  MSA-REPORT-RECORD           PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-CLM-FILE-STATUS     PIC XX.
           05  WS-MSA-FILE-STATUS     PIC XX.
           05  WS-CMS-FILE-STATUS     PIC XX.
           05  WS-S111-FILE-STATUS    PIC XX.
           05  WS-RPT-FILE-STATUS     PIC XX.

       01  WS-FLAGS.
           05  WS-EOF-FLAG            PIC X VALUE 'N'.
               88  END-OF-FILE        VALUE 'Y'.

       01  WS-MSA-THRESHOLDS.
           05  WS-SETTLEMENT-MIN      PIC S9(9)V99 COMP-3
               VALUE 25000.00.
           05  WS-MEDICARE-AGE-65     PIC 99 VALUE 65.
           05  WS-SSDI-WAIT-MONTHS    PIC 99 VALUE 29.
           05  WS-AGE-62-THRESHOLD    PIC 99 VALUE 62.
           05  WS-REVIEW-THRESHOLD    PIC S9(9)V99 COMP-3
               VALUE 250000.00.
           05  WS-ATTEST-INTERVAL-DAYS PIC 9(3) VALUE 365.

       01  WS-MSA-CALCULATION.
           05  WS-FUTURE-MED-RATED   PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-ANNUAL-ALLOCATION  PIC S9(7)V99 COMP-3 VALUE 0.
           05  WS-TOTAL-MSA-AMT      PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-YEARS-TO-COVER     PIC 99 VALUE 0.
           05  WS-DEPLETION-DATE     PIC X(8).
           05  WS-MONTHS-TO-DEPLETION PIC 9(3) VALUE 0.

       01  WS-SEC111-RECORD.
           05  WS-S111-REC-TYPE       PIC X(2).
           05  WS-S111-ACTION-TYPE    PIC X.
               88  S111-ADD           VALUE '0'.
               88  S111-DELETE        VALUE '1'.
               88  S111-UPDATE        VALUE '2'.
           05  WS-S111-RRE-ID         PIC X(9).
           05  WS-S111-CLAIM-NUM      PIC X(20).
           05  WS-S111-SSN            PIC X(9).
           05  WS-S111-MBI            PIC X(11).
           05  WS-S111-LAST-NAME      PIC X(35).
           05  WS-S111-FIRST-NAME     PIC X(25).
           05  WS-S111-DOB            PIC X(8).
           05  WS-S111-GENDER         PIC X.
           05  WS-S111-INJURY-DATE    PIC X(8).
           05  WS-S111-STATE          PIC X(2).
           05  WS-S111-NO-FAULT-IND   PIC X VALUE 'N'.
           05  WS-S111-PLAN-TYPE      PIC X(3) VALUE 'WC '.
           05  WS-S111-ICD-CODES      PIC X(7) OCCURS 8.
           05  WS-S111-SETTLEMENT-AMT PIC S9(9)V99 COMP-3.
           05  WS-S111-SETTLEMENT-DT  PIC X(8).
           05  WS-S111-EXHAUST-DATE   PIC X(8).
           05  WS-S111-ORM-IND        PIC X.
               88  S111-ORM-YES      VALUE 'Y'.
               88  S111-ORM-NO       VALUE 'N'.
           05  WS-S111-TPOC-AMT       PIC S9(9)V99 COMP-3.
           05  WS-S111-TPOC-DATE      PIC X(8).

       01  WS-COUNTERS.
           05  WS-CLAIMS-READ         PIC 9(7) VALUE 0.
           05  WS-MSA-APPLICABLE      PIC 9(5) VALUE 0.
           05  WS-MSA-NOT-APPLICABLE  PIC 9(5) VALUE 0.
           05  WS-CMS-SUBMISSIONS     PIC 9(5) VALUE 0.
           05  WS-SEC111-RECORDS      PIC 9(5) VALUE 0.
           05  WS-ATTEST-DUE          PIC 9(5) VALUE 0.
           05  WS-FUND-DEPLETED       PIC 9(5) VALUE 0.
           05  WS-TOTAL-MSA-VALUE     PIC S9(11)V99 COMP-3 VALUE 0.

       01  WS-WORK-FIELDS.
           05  WS-WORK-AMT            PIC -(9)9.99.
           05  WS-WORK-DATE           PIC X(8).
           05  WS-JULIAN-CURRENT      PIC 9(7).
           05  WS-JULIAN-ATTEST       PIC 9(7).
           05  WS-IDX                 PIC 99.
           05  WS-RPT-LINE            PIC X(132).
           05  WS-CURRENT-DATE        PIC X(8).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CLAIMS
           PERFORM 5000-MONITOR-ACTIVE-MSAS
           PERFORM 6000-GENERATE-REPORT
           PERFORM 9000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT  CLAIM-INPUT-FILE
           OPEN I-O    MSA-MASTER-FILE
           OPEN OUTPUT CMS-SUBMISSION-FILE
                       SEC111-OUTPUT-FILE
                       MSA-REPORT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           INITIALIZE WS-COUNTERS
           DISPLAY 'WCMSA: STARTED - MSA PROCESSING'.

       2000-PROCESS-CLAIMS.
           READ CLAIM-INPUT-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-CLAIMS-READ
               PERFORM 3000-DETERMINE-APPLICABILITY
               PERFORM 4000-GENERATE-SEC111
               READ CLAIM-INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

       3000-DETERMINE-APPLICABILITY.
           IF CI-SETTLEMENT-AMT < WS-SETTLEMENT-MIN
               ADD 1 TO WS-MSA-NOT-APPLICABLE
               EXIT PARAGRAPH
           END-IF
           IF ALREADY-ON-MEDICARE
               PERFORM 3100-MSA-IS-APPLICABLE
           ELSE IF RECEIVING-SSDI
               PERFORM 3100-MSA-IS-APPLICABLE
           ELSE IF CI-AGE-AT-SETTLEMENT >= WS-AGE-62-THRESHOLD
               AND CI-SETTLEMENT-AMT >= WS-REVIEW-THRESHOLD
               PERFORM 3100-MSA-IS-APPLICABLE
           ELSE
               ADD 1 TO WS-MSA-NOT-APPLICABLE
               MOVE CI-CLAIM-NUM TO MM-CLAIM-NUM
               READ MSA-MASTER-FILE
               IF WS-MSA-FILE-STATUS = '00'
                   IF NOT MSA-NOT-APPLICABLE
                       MOVE 'NA' TO MM-MSA-STATUS
                       REWRITE MSA-MASTER-RECORD
                   END-IF
               ELSE
                   INITIALIZE MSA-MASTER-RECORD
                   MOVE CI-CLAIM-NUM TO MM-CLAIM-NUM
                   MOVE CI-CLAIMANT-SSN TO MM-CLAIMANT-SSN
                   MOVE CI-CLAIMANT-MBI TO MM-CLAIMANT-MBI
                   MOVE 'NA' TO MM-MSA-STATUS
                   MOVE WS-CURRENT-DATE TO MM-CREATION-DATE
                   WRITE MSA-MASTER-RECORD
               END-IF
           END-IF.

       3100-MSA-IS-APPLICABLE.
           ADD 1 TO WS-MSA-APPLICABLE
           PERFORM 3200-CALCULATE-MSA-ALLOCATION
           MOVE CI-CLAIM-NUM TO MM-CLAIM-NUM
           READ MSA-MASTER-FILE
           IF WS-MSA-FILE-STATUS = '00'
               MOVE 'AP' TO MM-MSA-STATUS
               MOVE WS-TOTAL-MSA-AMT TO MM-MSA-AMOUNT
               MOVE WS-ANNUAL-ALLOCATION TO MM-ANNUAL-DEPOSIT
               MOVE WS-YEARS-TO-COVER TO MM-YEARS-ALLOCATED
               MOVE CI-SETTLEMENT-AMT TO MM-SETTLEMENT-AMT
               REWRITE MSA-MASTER-RECORD
           ELSE
               INITIALIZE MSA-MASTER-RECORD
               MOVE CI-CLAIM-NUM TO MM-CLAIM-NUM
               MOVE CI-CLAIMANT-SSN TO MM-CLAIMANT-SSN
               MOVE CI-CLAIMANT-MBI TO MM-CLAIMANT-MBI
               MOVE 'AP' TO MM-MSA-STATUS
               MOVE WS-TOTAL-MSA-AMT TO MM-MSA-AMOUNT
               MOVE WS-ANNUAL-ALLOCATION TO MM-ANNUAL-DEPOSIT
               MOVE WS-YEARS-TO-COVER TO MM-YEARS-ALLOCATED
               MOVE CI-SETTLEMENT-AMT TO MM-SETTLEMENT-AMT
               MOVE WS-CURRENT-DATE TO MM-CREATION-DATE
               MOVE 0 TO MM-FUND-BALANCE
               MOVE 0 TO MM-TOTAL-DISBURSED
               MOVE 'N' TO MM-COND-PAY-STATUS
               WRITE MSA-MASTER-RECORD
           END-IF
           ADD WS-TOTAL-MSA-AMT TO WS-TOTAL-MSA-VALUE
           PERFORM 3300-PREPARE-CMS-SUBMISSION.

       3200-CALCULATE-MSA-ALLOCATION.
           MOVE CI-LIFE-EXPECTANCY TO WS-YEARS-TO-COVER
           IF WS-YEARS-TO-COVER < 1
               MOVE 1 TO WS-YEARS-TO-COVER
           END-IF
           MOVE CI-FUTURE-MED-COST TO WS-FUTURE-MED-RATED
           IF WS-FUTURE-MED-RATED <= 0
               COMPUTE WS-FUTURE-MED-RATED =
                   CI-TOTAL-PAID-MED * 1.03
           END-IF
           COMPUTE WS-TOTAL-MSA-AMT =
               WS-FUTURE-MED-RATED * WS-YEARS-TO-COVER
           IF WS-TOTAL-MSA-AMT > CI-SETTLEMENT-AMT
               MOVE CI-SETTLEMENT-AMT TO WS-TOTAL-MSA-AMT
           END-IF
           COMPUTE WS-ANNUAL-ALLOCATION =
               WS-TOTAL-MSA-AMT / WS-YEARS-TO-COVER.

       3300-PREPARE-CMS-SUBMISSION.
           ADD 1 TO WS-CMS-SUBMISSIONS
           INITIALIZE CMS-SUBMISSION-RECORD
           STRING
               'MSA-PROPOSAL|' DELIMITED BY SIZE
               CI-CLAIM-NUM DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-SSN DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-MBI DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-LAST DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-FIRST DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               CI-CLAIMANT-DOB DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               CI-INJURY-DATE DELIMITED BY SIZE
               INTO CMS-SUBMISSION-RECORD
           END-STRING
           WRITE CMS-SUBMISSION-RECORD
           INITIALIZE CMS-SUBMISSION-RECORD
           MOVE WS-TOTAL-MSA-AMT TO WS-WORK-AMT
           STRING
               'MSA-AMOUNT|' DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-AMT) DELIMITED BY SIZE
               '|YEARS=' DELIMITED BY SIZE
               WS-YEARS-TO-COVER DELIMITED BY SIZE
               INTO CMS-SUBMISSION-RECORD
           END-STRING
           WRITE CMS-SUBMISSION-RECORD
           INITIALIZE CMS-SUBMISSION-RECORD
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 8
               IF CI-ICD-CODES(WS-IDX) NOT = SPACES
                   STRING
                       CMS-SUBMISSION-RECORD DELIMITED BY '  '
                       CI-ICD-CODES(WS-IDX) DELIMITED BY '  '
                       '|' DELIMITED BY SIZE
                       INTO CMS-SUBMISSION-RECORD
                   END-STRING
               END-IF
           END-PERFORM
           WRITE CMS-SUBMISSION-RECORD.

       4000-GENERATE-SEC111.
           IF CI-SETTLEMENT-AMT <= 0
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-SEC111-RECORDS
           INITIALIZE WS-SEC111-RECORD
           MOVE 'CD' TO WS-S111-REC-TYPE
           MOVE '0' TO WS-S111-ACTION-TYPE
           MOVE CI-RRE-ID TO WS-S111-RRE-ID
           MOVE CI-CLAIM-NUM TO WS-S111-CLAIM-NUM
           MOVE CI-CLAIMANT-SSN TO WS-S111-SSN
           MOVE CI-CLAIMANT-MBI TO WS-S111-MBI
           MOVE CI-CLAIMANT-LAST TO WS-S111-LAST-NAME
           MOVE CI-CLAIMANT-FIRST TO WS-S111-FIRST-NAME
           MOVE CI-CLAIMANT-DOB TO WS-S111-DOB
           MOVE CI-CLAIMANT-GENDER TO WS-S111-GENDER
           MOVE CI-INJURY-DATE TO WS-S111-INJURY-DATE
           MOVE CI-CLAIMANT-STATE TO WS-S111-STATE
           MOVE 'WC ' TO WS-S111-PLAN-TYPE
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 8
               MOVE CI-ICD-CODES(WS-IDX)
                   TO WS-S111-ICD-CODES(WS-IDX)
           END-PERFORM
           MOVE CI-SETTLEMENT-AMT TO WS-S111-TPOC-AMT
           MOVE CI-SETTLEMENT-DATE TO WS-S111-TPOC-DATE
           MOVE 'N' TO WS-S111-ORM-IND
           INITIALIZE SEC111-OUTPUT-RECORD
           STRING
               WS-S111-REC-TYPE DELIMITED BY SIZE
               WS-S111-ACTION-TYPE DELIMITED BY SIZE
               WS-S111-RRE-ID DELIMITED BY SIZE
               WS-S111-CLAIM-NUM DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               WS-S111-SSN DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-S111-MBI DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               WS-S111-LAST-NAME DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               WS-S111-FIRST-NAME DELIMITED BY '  '
               '|' DELIMITED BY SIZE
               WS-S111-DOB DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-S111-GENDER DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-S111-INJURY-DATE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-S111-STATE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-S111-PLAN-TYPE DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-S111-TPOC-AMT DELIMITED BY SIZE
               '|' DELIMITED BY SIZE
               WS-S111-TPOC-DATE DELIMITED BY SIZE
               INTO SEC111-OUTPUT-RECORD
           END-STRING
           WRITE SEC111-OUTPUT-RECORD
           MOVE CI-CLAIM-NUM TO MM-CLAIM-NUM
           READ MSA-MASTER-FILE
           IF WS-MSA-FILE-STATUS = '00'
               MOVE 'Y' TO MM-SEC111-REPORTED
               REWRITE MSA-MASTER-RECORD
           END-IF.

       5000-MONITOR-ACTIVE-MSAS.
           MOVE LOW-VALUES TO MM-MSA-KEY
           START MSA-MASTER-FILE KEY > MM-MSA-KEY
           MOVE 'N' TO WS-EOF-FLAG
           READ MSA-MASTER-FILE NEXT
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               IF MSA-ACTIVE
                   PERFORM 5100-CHECK-ATTESTATION
                   PERFORM 5200-CHECK-FUND-BALANCE
               END-IF
               READ MSA-MASTER-FILE NEXT
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

       5100-CHECK-ATTESTATION.
           IF MM-NEXT-ATTEST-DATE = SPACES
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-JULIAN-CURRENT =
               FUNCTION INTEGER-OF-DATE(
                   FUNCTION NUMVAL(WS-CURRENT-DATE))
           COMPUTE WS-JULIAN-ATTEST =
               FUNCTION INTEGER-OF-DATE(
                   FUNCTION NUMVAL(MM-NEXT-ATTEST-DATE))
           IF WS-JULIAN-CURRENT >= WS-JULIAN-ATTEST
               ADD 1 TO WS-ATTEST-DUE
               DISPLAY 'WCMSA: ATTESTATION DUE FOR '
                   MM-CLAIM-NUM ' DUE=' MM-NEXT-ATTEST-DATE
           END-IF.

       5200-CHECK-FUND-BALANCE.
           IF MM-FUND-BALANCE <= 0
               AND MM-TOTAL-DISBURSED > 0
               ADD 1 TO WS-FUND-DEPLETED
               IF NOT MSA-DEPLETED
                   MOVE 'DP' TO MM-MSA-STATUS
                   REWRITE MSA-MASTER-RECORD
                   DISPLAY 'WCMSA: FUND DEPLETED FOR '
                       MM-CLAIM-NUM
               END-IF
           ELSE
               IF MM-ANNUAL-DEPOSIT > 0
                   COMPUTE WS-MONTHS-TO-DEPLETION =
                       (MM-FUND-BALANCE / MM-ANNUAL-DEPOSIT)
                       * 12
                   IF WS-MONTHS-TO-DEPLETION < 6
                       DISPLAY 'WCMSA: LOW FUND WARNING '
                           MM-CLAIM-NUM
                           ' MONTHS=' WS-MONTHS-TO-DEPLETION
                   END-IF
               END-IF
           END-IF.

       6000-GENERATE-REPORT.
           INITIALIZE WS-RPT-LINE
           STRING
               'MEDICARE SET-ASIDE PROCESSING REPORT'
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING 'RUN DATE: ' WS-CURRENT-DATE
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           MOVE ALL '=' TO WS-RPT-LINE
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE

           INITIALIZE WS-RPT-LINE
           STRING
               'CLAIMS EVALUATED:       ' WS-CLAIMS-READ
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING
               'MSA APPLICABLE:         ' WS-MSA-APPLICABLE
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING
               'MSA NOT APPLICABLE:     ' WS-MSA-NOT-APPLICABLE
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING
               'CMS SUBMISSIONS PREP:   ' WS-CMS-SUBMISSIONS
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING
               'SEC 111 RECORDS SENT:   ' WS-SEC111-RECORDS
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING
               'ATTESTATIONS DUE:       ' WS-ATTEST-DUE
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           INITIALIZE WS-RPT-LINE
           STRING
               'FUNDS DEPLETED:         ' WS-FUND-DEPLETED
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE
           MOVE WS-TOTAL-MSA-VALUE TO WS-WORK-AMT
           INITIALIZE WS-RPT-LINE
           STRING
               'TOTAL MSA VALUE:        ' WS-WORK-AMT
               DELIMITED BY SIZE INTO WS-RPT-LINE
           END-STRING
           WRITE MSA-REPORT-RECORD FROM WS-RPT-LINE.

       9000-FINALIZE.
           CLOSE CLAIM-INPUT-FILE
                 MSA-MASTER-FILE
                 CMS-SUBMISSION-FILE
                 SEC111-OUTPUT-FILE
                 MSA-REPORT-FILE
           DISPLAY 'WCMSA: COMPLETED'
           DISPLAY 'WCMSA: CLAIMS READ           = ' WS-CLAIMS-READ
           DISPLAY 'WCMSA: MSA APPLICABLE        = ' WS-MSA-APPLICABLE
           DISPLAY 'WCMSA: MSA NOT APPLICABLE    = '
                   WS-MSA-NOT-APPLICABLE
           DISPLAY 'WCMSA: CMS SUBMISSIONS       = '
                   WS-CMS-SUBMISSIONS
           DISPLAY 'WCMSA: SEC111 RECORDS        = '
                   WS-SEC111-RECORDS
           DISPLAY 'WCMSA: TOTAL MSA VALUE       = '
                   WS-TOTAL-MSA-VALUE.
