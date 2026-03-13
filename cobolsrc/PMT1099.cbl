       IDENTIFICATION DIVISION.
       PROGRAM-ID. PMT1099.
      *================================================================
      * PMT1099 — 1099 Processing Program
      *
      * Accumulates reportable payments by payee TIN for IRS 1099
      * reporting. Classifies payments by type (1099-MISC for medical
      * providers, 1099-NEC for attorneys), validates TINs, generates
      * annual 1099 forms, handles corrections, produces IRS electronic
      * filing per Publication 1220, and generates state copies.
      *
      * Workers' Compensation TPA — Annual Tax Reporting Subsystem
      *
      * Run modes:
      *   1 = Accumulate (run after each payment cycle)
      *   2 = Preview (pre-filing review report)
      *   3 = Generate (produce 1099 forms and electronic file)
      *   4 = Correction (generate corrected 1099)
      *   5 = State copy generation
      *================================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS PERIOD.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYMENT-FILE
               ASSIGN TO "$DATA1.WCDATA.PMTHIST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS PH-KEY
               FILE STATUS IS WS-PH-STATUS.

           SELECT TIN-ACCUM-FILE
               ASSIGN TO "$DATA1.WCDATA.TIN1099"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TA-KEY
               FILE STATUS IS WS-TA-STATUS.

           SELECT IRS-EFILE
               ASSIGN TO "$DATA2.WCTAX.IRSEFILE"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-IE-STATUS.

           SELECT FORM-1099-FILE
               ASSIGN TO "$S.#1099PRT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FP-STATUS.

           SELECT STATE-COPY-FILE
               ASSIGN TO "$DATA2.WCTAX.STCOPY"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-SC-STATUS.

           SELECT ERROR-REPORT
               ASSIGN TO "$S.#1099ERR"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ER-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  PAYMENT-FILE.
       01  PAYMENT-REC.
           05  PH-KEY.
               10  PH-CLAIM-NUMBER     PIC X(12).
               10  PH-PROVIDER-ID      PIC X(10).
               10  PH-SERVICE-DATE     PIC 9(8).
           05  PH-PAYMENT-DATE         PIC 9(8).
           05  PH-AMOUNT               PIC S9(9)V99 COMP-3.
           05  PH-PAYMENT-TYPE         PIC 9(1).
               88  PH-MEDICAL          VALUE 1.
               88  PH-INDEMNITY        VALUE 2.
               88  PH-LEGAL            VALUE 3.
               88  PH-REHAB            VALUE 4.
               88  PH-EXPENSE          VALUE 5.
           05  PH-PAYEE-TIN            PIC X(9).
           05  PH-PAYEE-NAME           PIC X(40).
           05  PH-PAYEE-ADDR-1         PIC X(35).
           05  PH-PAYEE-ADDR-2         PIC X(35).
           05  PH-PAYEE-CITY           PIC X(25).
           05  PH-PAYEE-STATE          PIC X(2).
           05  PH-PAYEE-ZIP            PIC X(9).
           05  PH-STATUS               PIC 9(1).

       FD  TIN-ACCUM-FILE.
       01  TIN-ACCUM-REC.
           05  TA-KEY.
               10  TA-TIN              PIC X(9).
               10  TA-TAX-YEAR         PIC 9(4).
           05  TA-PAYEE-NAME           PIC X(40).
           05  TA-PAYEE-ADDR-1         PIC X(35).
           05  TA-PAYEE-ADDR-2         PIC X(35).
           05  TA-PAYEE-CITY           PIC X(25).
           05  TA-PAYEE-STATE          PIC X(2).
           05  TA-PAYEE-ZIP            PIC X(9).
           05  TA-TIN-TYPE             PIC X(1).
               88  TA-TIN-SSN          VALUE "S".
               88  TA-TIN-EIN          VALUE "E".
           05  TA-AMOUNTS.
               10  TA-BOX-1-RENTS      PIC S9(9)V99 COMP-3.
               10  TA-BOX-2-ROYALTIES  PIC S9(9)V99 COMP-3.
               10  TA-BOX-3-OTHER      PIC S9(9)V99 COMP-3.
               10  TA-BOX-4-FED-TAX    PIC S9(9)V99 COMP-3.
               10  TA-BOX-5-FISHING    PIC S9(9)V99 COMP-3.
               10  TA-BOX-6-MEDICAL    PIC S9(9)V99 COMP-3.
               10  TA-BOX-7-NONEMPLOY  PIC S9(9)V99 COMP-3.
               10  TA-BOX-10-CROP      PIC S9(9)V99 COMP-3.
               10  TA-BOX-14-ATTORNEY  PIC S9(9)V99 COMP-3.
           05  TA-NEC-AMOUNTS.
               10  TA-NEC-BOX-1        PIC S9(9)V99 COMP-3.
               10  TA-NEC-BOX-4        PIC S9(9)V99 COMP-3.
           05  TA-TOTAL-REPORTED       PIC S9(9)V99 COMP-3.
           05  TA-FORM-TYPE            PIC X(4).
               88  TA-FORM-MISC        VALUE "MISC".
               88  TA-FORM-NEC         VALUE "NEC ".
               88  TA-FORM-BOTH        VALUE "BOTH".
               88  TA-FORM-NONE        VALUE "NONE".
           05  TA-FILED-FLAG           PIC 9(1).
               88  TA-NOT-FILED        VALUE 0.
               88  TA-FILED            VALUE 1.
               88  TA-CORRECTED        VALUE 2.
           05  TA-TIN-VALID            PIC 9(1).
               88  TA-TIN-OK           VALUE 1.
               88  TA-TIN-BAD          VALUE 0.
           05  TA-PAYMENT-COUNT        PIC 9(5).

       FD  IRS-EFILE.
       01  IRS-EFILE-REC              PIC X(750).

       FD  FORM-1099-FILE.
       01  FORM-1099-REC              PIC X(132).

       FD  STATE-COPY-FILE.
       01  STATE-COPY-REC             PIC X(750).

       FD  ERROR-REPORT.
       01  ERROR-REPORT-REC           PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUSES.
           05  WS-PH-STATUS           PIC X(2).
           05  WS-TA-STATUS           PIC X(2).
           05  WS-IE-STATUS           PIC X(2).
           05  WS-FP-STATUS           PIC X(2).
           05  WS-SC-STATUS           PIC X(2).
           05  WS-ER-STATUS           PIC X(2).

       01  WS-RUN-MODE                PIC 9(1).
           88  WS-MODE-ACCUMULATE     VALUE 1.
           88  WS-MODE-PREVIEW        VALUE 2.
           88  WS-MODE-GENERATE       VALUE 3.
           88  WS-MODE-CORRECT        VALUE 4.
           88  WS-MODE-STATE          VALUE 5.

       01  WS-TAX-YEAR                PIC 9(4).
       01  WS-CURRENT-DATE            PIC 9(8).
       01  WS-THRESHOLD-AMOUNT        PIC S9(9)V99 COMP-3
                                      VALUE 600.00.

      * IRS Pub 1220 record types
       01  WS-IRS-TRANSMITTER-REC     PIC X(750).
       01  WS-IRS-PAYER-REC           PIC X(750).
       01  WS-IRS-PAYEE-REC           PIC X(750).
       01  WS-IRS-END-PAYER-REC       PIC X(750).
       01  WS-IRS-STATE-REC           PIC X(750).
       01  WS-IRS-END-TRANS-REC       PIC X(750).

      * Payer (TPA) information
       01  WS-PAYER-INFO.
           05  WS-PAYER-TIN           PIC X(9) VALUE "123456789".
           05  WS-PAYER-NAME          PIC X(40)
               VALUE "WORKERS COMP TPA SERVICES INC".
           05  WS-PAYER-ADDR          PIC X(40)
               VALUE "100 MAIN STREET SUITE 500".
           05  WS-PAYER-CITY          PIC X(25) VALUE "DALLAS".
           05  WS-PAYER-STATE         PIC X(2) VALUE "TX".
           05  WS-PAYER-ZIP           PIC X(9) VALUE "752010000".
           05  WS-PAYER-PHONE         PIC X(15) VALUE "2145551234".
           05  WS-TRANSMITTER-CODE    PIC X(5) VALUE "12345".

      * Counters
       01  WS-COUNTERS.
           05  WS-PAYMENTS-READ       PIC 9(7) VALUE 0.
           05  WS-TINS-ACCUMULATED    PIC 9(7) VALUE 0.
           05  WS-FORMS-GENERATED     PIC 9(7) VALUE 0.
           05  WS-MISC-COUNT          PIC 9(7) VALUE 0.
           05  WS-NEC-COUNT           PIC 9(7) VALUE 0.
           05  WS-BELOW-THRESHOLD     PIC 9(7) VALUE 0.
           05  WS-TIN-ERRORS          PIC 9(7) VALUE 0.
           05  WS-CORRECTIONS         PIC 9(7) VALUE 0.
           05  WS-TOTAL-REPORTED-AMT  PIC S9(11)V99 COMP-3 VALUE 0.

      * TIN validation work fields
       01  WS-TIN-WORK                PIC X(9).
       01  WS-TIN-DIGITS REDEFINES WS-TIN-WORK.
           05  WS-TIN-D PIC 9(1) OCCURS 9 TIMES.
       01  WS-TIN-NUMERIC             PIC 9(1).
           88  WS-TIN-IS-NUMERIC      VALUE 1.
           88  WS-TIN-NOT-NUMERIC     VALUE 0.
       01  WS-TIN-AREA                PIC 9(3).

      * State filing thresholds (some states require filing below $600)
       01  WS-STATE-THRESHOLDS.
           05  WS-STATE-ENTRY OCCURS 10 TIMES.
               10  WS-ST-CODE         PIC X(2).
               10  WS-ST-THRESHOLD    PIC S9(9)V99 COMP-3.
               10  WS-ST-REQUIRES     PIC 9(1).

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           EVALUATE TRUE
               WHEN WS-MODE-ACCUMULATE
                   PERFORM 2000-ACCUMULATE-PAYMENTS
               WHEN WS-MODE-PREVIEW
                   PERFORM 3000-PREVIEW-REPORT
               WHEN WS-MODE-GENERATE
                   PERFORM 4000-GENERATE-1099S
               WHEN WS-MODE-CORRECT
                   PERFORM 5000-PROCESS-CORRECTIONS
               WHEN WS-MODE-STATE
                   PERFORM 6000-GENERATE-STATE-COPIES
           END-EVALUATE
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           ACCEPT WS-RUN-MODE FROM ENVIRONMENT "RUN_MODE"
           ACCEPT WS-TAX-YEAR FROM ENVIRONMENT "TAX_YEAR"
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE

           OPEN INPUT PAYMENT-FILE
           OPEN I-O TIN-ACCUM-FILE

           IF WS-MODE-GENERATE
               OPEN OUTPUT IRS-EFILE
               OPEN OUTPUT FORM-1099-FILE
           END-IF
           IF WS-MODE-STATE
               OPEN OUTPUT STATE-COPY-FILE
           END-IF
           OPEN OUTPUT ERROR-REPORT

      *    Initialize state thresholds
           MOVE "CA" TO WS-ST-CODE(1)
           MOVE 600.00 TO WS-ST-THRESHOLD(1)
           MOVE 1 TO WS-ST-REQUIRES(1)
           MOVE "NY" TO WS-ST-CODE(2)
           MOVE 600.00 TO WS-ST-THRESHOLD(2)
           MOVE 1 TO WS-ST-REQUIRES(2)
           MOVE "IL" TO WS-ST-CODE(3)
           MOVE 600.00 TO WS-ST-THRESHOLD(3)
           MOVE 1 TO WS-ST-REQUIRES(3).

       2000-ACCUMULATE-PAYMENTS.
      *    Read payment history and accumulate by TIN
           READ PAYMENT-FILE
               AT END GO TO 2000-EXIT
           END-READ
           PERFORM UNTIL WS-PH-STATUS NOT = "00"
      *        Skip indemnity — not reportable on 1099
               IF NOT PH-INDEMNITY AND PH-STATUS = 0
                   PERFORM 2100-ACCUMULATE-TIN
               END-IF
               ADD 1 TO WS-PAYMENTS-READ
               READ PAYMENT-FILE
                   AT END GO TO 2000-EXIT
               END-READ
           END-PERFORM.
       2000-EXIT.
           DISPLAY "Payments read: " WS-PAYMENTS-READ
           DISPLAY "TINs accumulated: " WS-TINS-ACCUMULATED.

       2100-ACCUMULATE-TIN.
      *    Validate TIN first
           MOVE PH-PAYEE-TIN TO WS-TIN-WORK
           PERFORM 2200-VALIDATE-TIN
           IF WS-TIN-NOT-NUMERIC
               ADD 1 TO WS-TIN-ERRORS
               STRING "BAD TIN: " PH-PAYEE-TIN
                   " PAYEE: " PH-PAYEE-NAME
                   DELIMITED SIZE INTO ERROR-REPORT-REC
               WRITE ERROR-REPORT-REC
               GO TO 2100-EXIT
           END-IF

      *    Read or create TIN accumulation record
           MOVE PH-PAYEE-TIN TO TA-TIN
           MOVE WS-TAX-YEAR TO TA-TAX-YEAR
           READ TIN-ACCUM-FILE
               INVALID KEY
                   PERFORM 2300-INIT-TIN-RECORD
           END-READ

      *    Classify and accumulate payment
           EVALUATE TRUE
               WHEN PH-MEDICAL
      *            Medical payments go to 1099-MISC Box 6
                   ADD PH-AMOUNT TO TA-BOX-6-MEDICAL
                   IF NOT TA-FORM-NEC
                       SET TA-FORM-MISC TO TRUE
                   ELSE
                       SET TA-FORM-BOTH TO TRUE
                   END-IF
               WHEN PH-LEGAL
      *            Attorney fees go to 1099-NEC Box 1
                   ADD PH-AMOUNT TO TA-NEC-BOX-1
                   ADD PH-AMOUNT TO TA-BOX-14-ATTORNEY
                   IF NOT TA-FORM-MISC
                       SET TA-FORM-NEC TO TRUE
                   ELSE
                       SET TA-FORM-BOTH TO TRUE
                   END-IF
               WHEN PH-REHAB
      *            Rehabilitation goes to 1099-MISC Box 3 (other)
                   ADD PH-AMOUNT TO TA-BOX-3-OTHER
                   IF NOT TA-FORM-NEC
                       SET TA-FORM-MISC TO TRUE
                   ELSE
                       SET TA-FORM-BOTH TO TRUE
                   END-IF
               WHEN PH-EXPENSE
      *            Expense payments — non-employee compensation
                   ADD PH-AMOUNT TO TA-NEC-BOX-1
                   IF NOT TA-FORM-MISC
                       SET TA-FORM-NEC TO TRUE
                   ELSE
                       SET TA-FORM-BOTH TO TRUE
                   END-IF
           END-EVALUATE

           ADD PH-AMOUNT TO TA-TOTAL-REPORTED
           ADD 1 TO TA-PAYMENT-COUNT
           MOVE PH-PAYEE-NAME TO TA-PAYEE-NAME
           MOVE PH-PAYEE-ADDR-1 TO TA-PAYEE-ADDR-1
           MOVE PH-PAYEE-ADDR-2 TO TA-PAYEE-ADDR-2
           MOVE PH-PAYEE-CITY TO TA-PAYEE-CITY
           MOVE PH-PAYEE-STATE TO TA-PAYEE-STATE
           MOVE PH-PAYEE-ZIP TO TA-PAYEE-ZIP

           REWRITE TIN-ACCUM-REC
               INVALID KEY
                   WRITE TIN-ACCUM-REC
           END-REWRITE.

       2100-EXIT.
           EXIT.

       2200-VALIDATE-TIN.
      *    Validate TIN is 9 numeric digits and area code is valid
           SET WS-TIN-IS-NUMERIC TO TRUE
           INSPECT WS-TIN-WORK TALLYING WS-TIN-NUMERIC
               FOR ALL SPACES
           IF WS-TIN-WORK = SPACES OR WS-TIN-WORK = "000000000"
               SET WS-TIN-NOT-NUMERIC TO TRUE
               GO TO 2200-EXIT
           END-IF
      *    Check first 3 digits (area number) — 000, 666, 900-999 invalid for SSN
           COMPUTE WS-TIN-AREA =
               WS-TIN-D(1) * 100 + WS-TIN-D(2) * 10 + WS-TIN-D(3)
           IF WS-TIN-AREA = 0 OR WS-TIN-AREA = 666
               OR WS-TIN-AREA >= 900
      *        Could be EIN — check EIN format (first 2 digits 10-99)
               IF WS-TIN-D(1) = 0 AND WS-TIN-D(2) = 0
                   SET WS-TIN-NOT-NUMERIC TO TRUE
               END-IF
           END-IF.
       2200-EXIT.
           EXIT.

       2300-INIT-TIN-RECORD.
           MOVE PH-PAYEE-TIN TO TA-TIN
           MOVE WS-TAX-YEAR TO TA-TAX-YEAR
           INITIALIZE TA-AMOUNTS
           INITIALIZE TA-NEC-AMOUNTS
           MOVE 0 TO TA-TOTAL-REPORTED
           SET TA-FORM-NONE TO TRUE
           SET TA-NOT-FILED TO TRUE
           SET TA-TIN-OK TO TRUE
           MOVE 0 TO TA-PAYMENT-COUNT
           ADD 1 TO WS-TINS-ACCUMULATED.

       3000-PREVIEW-REPORT.
      *    Generate pre-filing review report
           MOVE SPACES TO FORM-1099-REC
           DISPLAY "=== 1099 PRE-FILING REVIEW ==="
           DISPLAY "Tax Year: " WS-TAX-YEAR

           MOVE LOW-VALUES TO TA-KEY
           START TIN-ACCUM-FILE KEY >= TA-KEY
               INVALID KEY GO TO 3000-EXIT
           END-START

           READ TIN-ACCUM-FILE NEXT
               AT END GO TO 3000-EXIT
           END-READ

           PERFORM UNTIL WS-TA-STATUS NOT = "00"
               IF TA-TAX-YEAR = WS-TAX-YEAR
                   IF TA-TOTAL-REPORTED >= WS-THRESHOLD-AMOUNT
                       DISPLAY "TIN: " TA-TIN
                           " NAME: " TA-PAYEE-NAME
                           " TOTAL: " TA-TOTAL-REPORTED
                           " FORM: " TA-FORM-TYPE
                       ADD 1 TO WS-FORMS-GENERATED
                       ADD TA-TOTAL-REPORTED
                           TO WS-TOTAL-REPORTED-AMT
                   ELSE
                       ADD 1 TO WS-BELOW-THRESHOLD
                   END-IF
               END-IF
               READ TIN-ACCUM-FILE NEXT
                   AT END GO TO 3000-EXIT
               END-READ
           END-PERFORM.
       3000-EXIT.
           DISPLAY "Forms to generate: " WS-FORMS-GENERATED
           DISPLAY "Below threshold: " WS-BELOW-THRESHOLD
           DISPLAY "Total amount: " WS-TOTAL-REPORTED-AMT.

       4000-GENERATE-1099S.
      *    Generate IRS electronic file per Pub 1220
           PERFORM 4100-WRITE-TRANSMITTER-REC
           PERFORM 4200-WRITE-PAYER-REC

           MOVE LOW-VALUES TO TA-KEY
           START TIN-ACCUM-FILE KEY >= TA-KEY
               INVALID KEY GO TO 4000-EXIT
           END-START

           READ TIN-ACCUM-FILE NEXT
               AT END GO TO 4000-EXIT
           END-READ

           PERFORM UNTIL WS-TA-STATUS NOT = "00"
               IF TA-TAX-YEAR = WS-TAX-YEAR
                   AND TA-TOTAL-REPORTED >= WS-THRESHOLD-AMOUNT
                   AND TA-TIN-OK
                   PERFORM 4300-WRITE-PAYEE-REC
                   PERFORM 4400-PRINT-1099-FORM
                   ADD 1 TO WS-FORMS-GENERATED
                   ADD TA-TOTAL-REPORTED TO WS-TOTAL-REPORTED-AMT
      *            Mark as filed
                   SET TA-FILED TO TRUE
                   REWRITE TIN-ACCUM-REC
               END-IF
               READ TIN-ACCUM-FILE NEXT
                   AT END GO TO 4000-EXIT
               END-READ
           END-PERFORM.

       4000-EXIT.
           PERFORM 4500-WRITE-END-PAYER-REC
           PERFORM 4600-WRITE-END-TRANS-REC
           DISPLAY "1099s generated: " WS-FORMS-GENERATED.

       4100-WRITE-TRANSMITTER-REC.
      *    Pub 1220: "T" record — Transmitter information
           MOVE SPACES TO WS-IRS-TRANSMITTER-REC
           MOVE "T" TO WS-IRS-TRANSMITTER-REC(1:1)
           MOVE WS-TAX-YEAR TO WS-IRS-TRANSMITTER-REC(2:4)
           MOVE WS-TRANSMITTER-CODE
               TO WS-IRS-TRANSMITTER-REC(6:5)
           MOVE WS-PAYER-TIN TO WS-IRS-TRANSMITTER-REC(11:9)
           MOVE WS-PAYER-NAME TO WS-IRS-TRANSMITTER-REC(20:40)
           MOVE WS-PAYER-ADDR TO WS-IRS-TRANSMITTER-REC(60:40)
           MOVE WS-PAYER-CITY TO WS-IRS-TRANSMITTER-REC(100:25)
           MOVE WS-PAYER-STATE TO WS-IRS-TRANSMITTER-REC(125:2)
           MOVE WS-PAYER-ZIP TO WS-IRS-TRANSMITTER-REC(127:9)
           MOVE WS-IRS-TRANSMITTER-REC TO IRS-EFILE-REC
           WRITE IRS-EFILE-REC.

       4200-WRITE-PAYER-REC.
      *    Pub 1220: "A" record — Payer information
           MOVE SPACES TO WS-IRS-PAYER-REC
           MOVE "A" TO WS-IRS-PAYER-REC(1:1)
           MOVE WS-TAX-YEAR TO WS-IRS-PAYER-REC(2:4)
           MOVE WS-PAYER-TIN TO WS-IRS-PAYER-REC(12:9)
           MOVE WS-PAYER-NAME TO WS-IRS-PAYER-REC(21:40)
           MOVE WS-IRS-PAYER-REC TO IRS-EFILE-REC
           WRITE IRS-EFILE-REC.

       4300-WRITE-PAYEE-REC.
      *    Pub 1220: "B" record — Payee information
           MOVE SPACES TO WS-IRS-PAYEE-REC
           MOVE "B" TO WS-IRS-PAYEE-REC(1:1)
           MOVE WS-TAX-YEAR TO WS-IRS-PAYEE-REC(2:4)
           MOVE TA-TIN TO WS-IRS-PAYEE-REC(12:9)
           MOVE TA-PAYEE-NAME TO WS-IRS-PAYEE-REC(21:40)
           MOVE TA-PAYEE-ADDR-1 TO WS-IRS-PAYEE-REC(61:40)
           MOVE TA-PAYEE-CITY TO WS-IRS-PAYEE-REC(101:25)
           MOVE TA-PAYEE-STATE TO WS-IRS-PAYEE-REC(126:2)
           MOVE TA-PAYEE-ZIP TO WS-IRS-PAYEE-REC(128:9)
           MOVE WS-IRS-PAYEE-REC TO IRS-EFILE-REC
           WRITE IRS-EFILE-REC.

       4400-PRINT-1099-FORM.
      *    Print formatted 1099 copy for payee
           MOVE SPACES TO FORM-1099-REC
           STRING "1099-" TA-FORM-TYPE " TAX YEAR " WS-TAX-YEAR
               DELIMITED SIZE INTO FORM-1099-REC
           WRITE FORM-1099-REC
           MOVE SPACES TO FORM-1099-REC
           STRING "PAYER: " WS-PAYER-NAME
               DELIMITED SIZE INTO FORM-1099-REC
           WRITE FORM-1099-REC
           STRING "PAYER TIN: " WS-PAYER-TIN
               DELIMITED SIZE INTO FORM-1099-REC
           WRITE FORM-1099-REC
           MOVE SPACES TO FORM-1099-REC
           STRING "RECIPIENT: " TA-PAYEE-NAME
               DELIMITED SIZE INTO FORM-1099-REC
           WRITE FORM-1099-REC
           STRING "RECIPIENT TIN: XXX-XX-"
               TA-TIN(6:4)
               DELIMITED SIZE INTO FORM-1099-REC
           WRITE FORM-1099-REC
           MOVE SPACES TO FORM-1099-REC
           IF TA-BOX-6-MEDICAL > 0
               STRING "Box 6 - Medical payments: "
                   TA-BOX-6-MEDICAL
                   DELIMITED SIZE INTO FORM-1099-REC
               WRITE FORM-1099-REC
           END-IF
           IF TA-BOX-14-ATTORNEY > 0
               MOVE SPACES TO FORM-1099-REC
               STRING "Box 14 - Gross attorney fees: "
                   TA-BOX-14-ATTORNEY
                   DELIMITED SIZE INTO FORM-1099-REC
               WRITE FORM-1099-REC
           END-IF
           IF TA-NEC-BOX-1 > 0
               MOVE SPACES TO FORM-1099-REC
               STRING "NEC Box 1 - Nonemployee compensation: "
                   TA-NEC-BOX-1
                   DELIMITED SIZE INTO FORM-1099-REC
               WRITE FORM-1099-REC
           END-IF
           MOVE SPACES TO FORM-1099-REC
           WRITE FORM-1099-REC.

       4500-WRITE-END-PAYER-REC.
      *    Pub 1220: "C" record — End of payer
           MOVE SPACES TO WS-IRS-END-PAYER-REC
           MOVE "C" TO WS-IRS-END-PAYER-REC(1:1)
           MOVE WS-FORMS-GENERATED TO WS-IRS-END-PAYER-REC(2:7)
           MOVE WS-IRS-END-PAYER-REC TO IRS-EFILE-REC
           WRITE IRS-EFILE-REC.

       4600-WRITE-END-TRANS-REC.
      *    Pub 1220: "F" record — End of transmission
           MOVE SPACES TO WS-IRS-END-TRANS-REC
           MOVE "F" TO WS-IRS-END-TRANS-REC(1:1)
           MOVE 1 TO WS-IRS-END-TRANS-REC(2:7)
           MOVE WS-IRS-END-TRANS-REC TO IRS-EFILE-REC
           WRITE IRS-EFILE-REC.

       5000-PROCESS-CORRECTIONS.
      *    Generate corrected 1099s for previously filed
           DISPLAY "=== 1099 CORRECTION PROCESSING ===".

       6000-GENERATE-STATE-COPIES.
      *    Generate state-specific 1099 copies
           MOVE LOW-VALUES TO TA-KEY
           START TIN-ACCUM-FILE KEY >= TA-KEY
               INVALID KEY GO TO 6000-EXIT
           END-START

           READ TIN-ACCUM-FILE NEXT
               AT END GO TO 6000-EXIT
           END-READ

           PERFORM UNTIL WS-TA-STATUS NOT = "00"
               IF TA-TAX-YEAR = WS-TAX-YEAR AND TA-FILED
                   PERFORM 6100-CHECK-STATE-REQUIREMENT
               END-IF
               READ TIN-ACCUM-FILE NEXT
                   AT END GO TO 6000-EXIT
               END-READ
           END-PERFORM.
       6000-EXIT.
           EXIT.

       6100-CHECK-STATE-REQUIREMENT.
      *    Check if state requires copy
           PERFORM VARYING WS-TIN-NUMERIC FROM 1 BY 1
               UNTIL WS-TIN-NUMERIC > 10
               IF TA-PAYEE-STATE = WS-ST-CODE(WS-TIN-NUMERIC)
                   AND WS-ST-REQUIRES(WS-TIN-NUMERIC) = 1
      *            Write state record with same Pub 1220 format
                   MOVE SPACES TO WS-IRS-STATE-REC
                   MOVE "B" TO WS-IRS-STATE-REC(1:1)
                   MOVE TA-TIN TO WS-IRS-STATE-REC(12:9)
                   MOVE TA-PAYEE-NAME TO WS-IRS-STATE-REC(21:40)
                   MOVE WS-IRS-STATE-REC TO STATE-COPY-REC
                   WRITE STATE-COPY-REC
               END-IF
           END-PERFORM.

       9000-TERMINATE.
           CLOSE PAYMENT-FILE
           CLOSE TIN-ACCUM-FILE
           IF WS-MODE-GENERATE
               CLOSE IRS-EFILE
               CLOSE FORM-1099-FILE
           END-IF
           IF WS-MODE-STATE
               CLOSE STATE-COPY-FILE
           END-IF
           CLOSE ERROR-REPORT
           DISPLAY "=== PMT1099 COMPLETE ==="
           DISPLAY "Forms generated: " WS-FORMS-GENERATED
           DISPLAY "TIN errors: " WS-TIN-ERRORS
           DISPLAY "Total reported: " WS-TOTAL-REPORTED-AMT.
