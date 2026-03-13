       IDENTIFICATION DIVISION.
       PROGRAM-ID. PMTCHECK.
      *================================================================
      * PMTCHECK — Check Printing and Reconciliation Program
      *
      * Generates printed checks with MICR encoding, manages check
      * numbering sequences, produces positive pay files for bank
      * fraud prevention, generates check register reports, handles
      * multi-payee splitting, void/reissue processing, and bank
      * reconciliation matching.
      *
      * Workers' Compensation TPA — Payment Processing Subsystem
      *
      * Files:
      *   CHKQUEUE — Approved payments ready for check printing
      *   CHKSEQ   — Check number sequence control
      *   CHKREG   — Check register (audit trail)
      *   POSPAY   — Positive pay output for bank transmission
      *   CHKPRINT — Print file for check stock
      *   CHKRECON — Bank reconciliation file
      *================================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS PERIOD.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHECK-QUEUE-FILE
               ASSIGN TO "$DATA1.WCDATA.CHKQUEUE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CQ-PAYMENT-ID
               FILE STATUS IS WS-CQ-STATUS.

           SELECT CHECK-SEQ-FILE
               ASSIGN TO "$DATA1.WCDATA.CHKSEQ"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CS-ACCOUNT-CODE
               FILE STATUS IS WS-CS-STATUS.

           SELECT CHECK-REGISTER-FILE
               ASSIGN TO "$DATA1.WCDATA.CHKREG"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CR-STATUS.

           SELECT POSITIVE-PAY-FILE
               ASSIGN TO "$DATA2.WCBANK.POSPAY"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-PP-STATUS.

           SELECT CHECK-PRINT-FILE
               ASSIGN TO "$S.#CHKPRT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CP-STATUS.

           SELECT RECON-FILE
               ASSIGN TO "$DATA1.WCDATA.CHKRECON"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RC-CHECK-NUMBER
               FILE STATUS IS WS-RC-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CHECK-QUEUE-FILE.
       01  CHECK-QUEUE-REC.
           05  CQ-PAYMENT-ID          PIC X(10).
           05  CQ-CLAIM-NUMBER        PIC X(12).
           05  CQ-PAYEE-NAME          PIC X(40).
           05  CQ-PAYEE-ADDR-1        PIC X(35).
           05  CQ-PAYEE-ADDR-2        PIC X(35).
           05  CQ-PAYEE-CITY          PIC X(25).
           05  CQ-PAYEE-STATE         PIC X(2).
           05  CQ-PAYEE-ZIP           PIC X(10).
           05  CQ-AMOUNT              PIC S9(9)V99 COMP-3.
           05  CQ-PAYMENT-TYPE        PIC 9(1).
               88  CQ-PMT-MEDICAL     VALUE 1.
               88  CQ-PMT-INDEMNITY   VALUE 2.
               88  CQ-PMT-LEGAL       VALUE 3.
               88  CQ-PMT-EXPENSE     VALUE 5.
           05  CQ-PAYMENT-DESC        PIC X(40).
           05  CQ-MULTI-PAYEE-FLAG    PIC 9(1).
               88  CQ-SINGLE-PAYEE    VALUE 0.
               88  CQ-MULTI-PAYEE     VALUE 1.
           05  CQ-SPLIT-GROUP         PIC X(10).
           05  CQ-STATUS              PIC 9(1).
               88  CQ-READY           VALUE 0.
               88  CQ-PRINTED         VALUE 1.
               88  CQ-VOID            VALUE 8.
               88  CQ-ERROR           VALUE 9.

       FD  CHECK-SEQ-FILE.
       01  CHECK-SEQ-REC.
           05  CS-ACCOUNT-CODE        PIC X(4).
           05  CS-LAST-CHECK-NUM      PIC 9(8).
           05  CS-BANK-ROUTING        PIC X(9).
           05  CS-BANK-ACCOUNT        PIC X(17).
           05  CS-BANK-NAME           PIC X(30).
           05  CS-CHECK-STOCK-START   PIC 9(8).
           05  CS-CHECK-STOCK-END     PIC 9(8).

       FD  CHECK-REGISTER-FILE.
       01  CHECK-REGISTER-REC.
           05  CK-CHECK-NUMBER        PIC 9(8).
           05  CK-CHECK-DATE          PIC 9(8).
           05  CK-PAYMENT-ID          PIC X(10).
           05  CK-CLAIM-NUMBER        PIC X(12).
           05  CK-PAYEE-NAME          PIC X(40).
           05  CK-AMOUNT              PIC S9(9)V99 COMP-3.
           05  CK-ACCOUNT-CODE        PIC X(4).
           05  CK-STATUS              PIC X(1).
               88  CK-ISSUED          VALUE "I".
               88  CK-CLEARED         VALUE "C".
               88  CK-VOIDED          VALUE "V".
               88  CK-STALE           VALUE "S".
               88  CK-ESCHEAT         VALUE "E".
           05  CK-VOID-DATE           PIC 9(8).
           05  CK-VOID-REASON         PIC X(30).
           05  CK-REISSUE-CHECK       PIC 9(8).

       FD  POSITIVE-PAY-FILE.
       01  POSITIVE-PAY-REC.
           05  PP-RECORD-TYPE         PIC X(1).
               88  PP-HEADER          VALUE "H".
               88  PP-DETAIL          VALUE "D".
               88  PP-TRAILER         VALUE "T".
           05  PP-ACCOUNT-NUMBER      PIC X(17).
           05  PP-CHECK-NUMBER        PIC 9(8).
           05  PP-AMOUNT              PIC 9(10)V99.
           05  PP-ISSUE-DATE          PIC 9(8).
           05  PP-PAYEE-NAME          PIC X(40).
           05  PP-VOID-FLAG           PIC X(1).
               88  PP-ISSUE           VALUE "I".
               88  PP-VOID            VALUE "V".
           05  PP-FILLER              PIC X(13).

       FD  CHECK-PRINT-FILE.
       01  CHECK-PRINT-REC            PIC X(132).

       FD  RECON-FILE.
       01  RECON-REC.
           05  RC-CHECK-NUMBER        PIC 9(8).
           05  RC-ISSUE-DATE          PIC 9(8).
           05  RC-ISSUE-AMOUNT        PIC S9(9)V99 COMP-3.
           05  RC-CLEAR-DATE          PIC 9(8).
           05  RC-CLEAR-AMOUNT        PIC S9(9)V99 COMP-3.
           05  RC-STATUS              PIC X(1).
           05  RC-MATCH-STATUS        PIC X(1).
               88  RC-UNMATCHED       VALUE "U".
               88  RC-MATCHED         VALUE "M".
               88  RC-EXCEPTION       VALUE "X".
           05  RC-DAYS-OUTSTANDING    PIC 9(3).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUSES.
           05  WS-CQ-STATUS           PIC X(2).
           05  WS-CS-STATUS           PIC X(2).
           05  WS-CR-STATUS           PIC X(2).
           05  WS-PP-STATUS           PIC X(2).
           05  WS-CP-STATUS           PIC X(2).
           05  WS-RC-STATUS           PIC X(2).

       01  WS-CURRENT-DATE            PIC 9(8).
       01  WS-CURRENT-DATE-R REDEFINES WS-CURRENT-DATE.
           05  WS-CURR-YEAR           PIC 9(4).
           05  WS-CURR-MONTH          PIC 9(2).
           05  WS-CURR-DAY            PIC 9(2).

       01  WS-NEXT-CHECK-NUMBER       PIC 9(8).
       01  WS-ACCOUNT-CODE            PIC X(4) VALUE "WC01".

       01  WS-COUNTERS.
           05  WS-CHECKS-PRINTED      PIC 9(6) VALUE 0.
           05  WS-TOTAL-AMOUNT        PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-PP-COUNT            PIC 9(6) VALUE 0.
           05  WS-VOID-COUNT          PIC 9(6) VALUE 0.
           05  WS-ERROR-COUNT         PIC 9(6) VALUE 0.

      * MICR line components
       01  WS-MICR-LINE.
           05  WS-MICR-TRANSIT        PIC X(11).
           05  WS-MICR-ON-US          PIC X(20).
           05  WS-MICR-AMOUNT         PIC X(12).
           05  WS-MICR-EPC            PIC X(1).

      * Check amount in words (for check face)
       01  WS-AMOUNT-WORDS            PIC X(80).
       01  WS-AMOUNT-WORK             PIC 9(9)V99.
       01  WS-WHOLE-PART              PIC 9(9).
       01  WS-CENTS-PART              PIC 99.

      * Stale date threshold (180 days)
       01  WS-STALE-DAYS              PIC 9(3) VALUE 180.

      * Number words table
       01  WS-ONES-TABLE.
           05  FILLER PIC X(10) VALUE "ONE       ".
           05  FILLER PIC X(10) VALUE "TWO       ".
           05  FILLER PIC X(10) VALUE "THREE     ".
           05  FILLER PIC X(10) VALUE "FOUR      ".
           05  FILLER PIC X(10) VALUE "FIVE      ".
           05  FILLER PIC X(10) VALUE "SIX       ".
           05  FILLER PIC X(10) VALUE "SEVEN     ".
           05  FILLER PIC X(10) VALUE "EIGHT     ".
           05  FILLER PIC X(10) VALUE "NINE      ".
       01  WS-ONES-VALUES REDEFINES WS-ONES-TABLE.
           05  WS-ONES-WORD PIC X(10) OCCURS 9 TIMES.

       01  WS-TEENS-TABLE.
           05  FILLER PIC X(10) VALUE "TEN       ".
           05  FILLER PIC X(10) VALUE "ELEVEN    ".
           05  FILLER PIC X(10) VALUE "TWELVE    ".
           05  FILLER PIC X(10) VALUE "THIRTEEN  ".
           05  FILLER PIC X(10) VALUE "FOURTEEN  ".
           05  FILLER PIC X(10) VALUE "FIFTEEN   ".
           05  FILLER PIC X(10) VALUE "SIXTEEN   ".
           05  FILLER PIC X(10) VALUE "SEVENTEEN ".
           05  FILLER PIC X(10) VALUE "EIGHTEEN  ".
           05  FILLER PIC X(10) VALUE "NINETEEN  ".
       01  WS-TEENS-VALUES REDEFINES WS-TEENS-TABLE.
           05  WS-TEENS-WORD PIC X(10) OCCURS 10 TIMES.

       01  WS-TENS-TABLE.
           05  FILLER PIC X(10) VALUE "TWENTY    ".
           05  FILLER PIC X(10) VALUE "THIRTY    ".
           05  FILLER PIC X(10) VALUE "FORTY     ".
           05  FILLER PIC X(10) VALUE "FIFTY     ".
           05  FILLER PIC X(10) VALUE "SIXTY     ".
           05  FILLER PIC X(10) VALUE "SEVENTY   ".
           05  FILLER PIC X(10) VALUE "EIGHTY    ".
           05  FILLER PIC X(10) VALUE "NINETY    ".
       01  WS-TENS-VALUES REDEFINES WS-TENS-TABLE.
           05  WS-TENS-WORD PIC X(10) OCCURS 8 TIMES.

      * ABA routing number check digit validation
       01  WS-ROUTING-NUM             PIC X(9).
       01  WS-ROUTING-DIGITS REDEFINES WS-ROUTING-NUM.
           05  WS-RD PIC 9(1) OCCURS 9 TIMES.
       01  WS-ROUTING-CHECKSUM        PIC 9(4).
       01  WS-ROUTING-VALID           PIC 9(1).
           88  WS-ROUTING-OK          VALUE 1.
           88  WS-ROUTING-BAD         VALUE 0.

      * Print line formats
       01  WS-CHECK-STUB-LINE.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  WS-STUB-CLAIM          PIC X(12).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  WS-STUB-DESC           PIC X(40).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  WS-STUB-AMOUNT         PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(55) VALUE SPACES.

       01  WS-CHECK-PAYEE-LINE.
           05  FILLER                  PIC X(10) VALUE "PAY TO THE".
           05  FILLER                  PIC X(1) VALUE SPACE.
           05  FILLER                  PIC X(9) VALUE "ORDER OF:".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WS-PAY-NAME            PIC X(40).
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  WS-PAY-AMOUNT          PIC $$$,$$$,$$9.99.
           05  FILLER                  PIC X(52) VALUE SPACES.

       01  WS-CHECK-AMOUNT-LINE.
           05  WS-AMT-WORDS           PIC X(80).
           05  FILLER                  PIC X(7) VALUE "DOLLARS".
           05  FILLER                  PIC X(45) VALUE SPACES.

       01  WS-REGISTER-HEADER.
           05  FILLER PIC X(132) VALUE
               "CHECK#   DATE     PAYMENT-ID CLAIM-NUMBER"
           &   "   PAYEE NAME                        "
           &   "     AMOUNT   STATUS".

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CHECK-QUEUE
           PERFORM 3000-GENERATE-POSITIVE-PAY
           PERFORM 8000-PRINT-REGISTER-TOTALS
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT CHECK-QUEUE-FILE
           OPEN I-O CHECK-SEQ-FILE
           OPEN EXTEND CHECK-REGISTER-FILE
           OPEN OUTPUT POSITIVE-PAY-FILE
           OPEN OUTPUT CHECK-PRINT-FILE
           OPEN I-O RECON-FILE
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE

      *    Get next check number from sequence file
           MOVE WS-ACCOUNT-CODE TO CS-ACCOUNT-CODE
           READ CHECK-SEQ-FILE
               INVALID KEY
                   DISPLAY "ERROR: Check sequence not found for "
                       WS-ACCOUNT-CODE
                   MOVE 99 TO RETURN-CODE
                   STOP RUN
           END-READ

      *    Validate bank routing number
           MOVE CS-BANK-ROUTING TO WS-ROUTING-NUM
           PERFORM 1100-VALIDATE-ROUTING
           IF WS-ROUTING-BAD
               DISPLAY "ERROR: Invalid ABA routing number "
                   CS-BANK-ROUTING
               MOVE 99 TO RETURN-CODE
               STOP RUN
           END-IF

           MOVE CS-LAST-CHECK-NUM TO WS-NEXT-CHECK-NUMBER

      *    Write positive pay header
           SET PP-HEADER TO TRUE
           MOVE CS-BANK-ACCOUNT TO PP-ACCOUNT-NUMBER
           MOVE 0 TO PP-CHECK-NUMBER
           MOVE 0 TO PP-AMOUNT
           MOVE WS-CURRENT-DATE TO PP-ISSUE-DATE
           MOVE "WORKERS COMP TPA CHECK RUN" TO PP-PAYEE-NAME
           SET PP-ISSUE TO TRUE
           WRITE POSITIVE-PAY-REC.

       1100-VALIDATE-ROUTING.
      *    ABA routing number check digit: 3(d1+d4+d7)+7(d2+d5+d8)+
      *    (d3+d6+d9) mod 10 = 0
           COMPUTE WS-ROUTING-CHECKSUM =
               3 * (WS-RD(1) + WS-RD(4) + WS-RD(7)) +
               7 * (WS-RD(2) + WS-RD(5) + WS-RD(8)) +
               1 * (WS-RD(3) + WS-RD(6) + WS-RD(9))
           IF FUNCTION MOD(WS-ROUTING-CHECKSUM, 10) = 0
               SET WS-ROUTING-OK TO TRUE
           ELSE
               SET WS-ROUTING-BAD TO TRUE
           END-IF.

       2000-PROCESS-CHECK-QUEUE.
           READ CHECK-QUEUE-FILE
               AT END GO TO 2000-EXIT
           END-READ
           PERFORM UNTIL WS-CQ-STATUS NOT = "00"
               IF CQ-READY
                   PERFORM 2100-PRINT-CHECK
               END-IF
               READ CHECK-QUEUE-FILE
                   AT END GO TO 2000-EXIT
               END-READ
           END-PERFORM.
       2000-EXIT.
           EXIT.

       2100-PRINT-CHECK.
      *    Validate check stock availability
           ADD 1 TO WS-NEXT-CHECK-NUMBER
           IF WS-NEXT-CHECK-NUMBER > CS-CHECK-STOCK-END
               DISPLAY "ERROR: Check stock exhausted at "
                   WS-NEXT-CHECK-NUMBER
               SET CQ-ERROR TO TRUE
               ADD 1 TO WS-ERROR-COUNT
               GO TO 2100-EXIT
           END-IF

      *    Format MICR line
           PERFORM 2200-FORMAT-MICR

      *    Convert amount to words
           MOVE CQ-AMOUNT TO WS-AMOUNT-WORK
           PERFORM 2300-AMOUNT-TO-WORDS

      *    Print check stub (remittance advice)
           MOVE SPACES TO CHECK-PRINT-REC
           WRITE CHECK-PRINT-REC
           MOVE CQ-CLAIM-NUMBER TO WS-STUB-CLAIM
           MOVE CQ-PAYMENT-DESC TO WS-STUB-DESC
           MOVE CQ-AMOUNT TO WS-STUB-AMOUNT
           MOVE WS-CHECK-STUB-LINE TO CHECK-PRINT-REC
           WRITE CHECK-PRINT-REC

      *    Print check face
           MOVE SPACES TO CHECK-PRINT-REC
           WRITE CHECK-PRINT-REC
           MOVE CQ-PAYEE-NAME TO WS-PAY-NAME
           MOVE CQ-AMOUNT TO WS-PAY-AMOUNT
           MOVE WS-CHECK-PAYEE-LINE TO CHECK-PRINT-REC
           WRITE CHECK-PRINT-REC

      *    Amount in words line
           MOVE WS-AMOUNT-WORDS TO WS-AMT-WORDS
           MOVE WS-CHECK-AMOUNT-LINE TO CHECK-PRINT-REC
           WRITE CHECK-PRINT-REC

      *    Address lines
           MOVE SPACES TO CHECK-PRINT-REC
           STRING "     " CQ-PAYEE-ADDR-1 DELIMITED SIZE
               INTO CHECK-PRINT-REC
           WRITE CHECK-PRINT-REC
           IF CQ-PAYEE-ADDR-2 NOT = SPACES
               MOVE SPACES TO CHECK-PRINT-REC
               STRING "     " CQ-PAYEE-ADDR-2 DELIMITED SIZE
                   INTO CHECK-PRINT-REC
               WRITE CHECK-PRINT-REC
           END-IF
           MOVE SPACES TO CHECK-PRINT-REC
           STRING "     " CQ-PAYEE-CITY DELIMITED SPACES
               ", " DELIMITED SIZE
               CQ-PAYEE-STATE DELIMITED SIZE
               "  " DELIMITED SIZE
               CQ-PAYEE-ZIP DELIMITED SIZE
               INTO CHECK-PRINT-REC
           WRITE CHECK-PRINT-REC

      *    MICR line at bottom
           MOVE SPACES TO CHECK-PRINT-REC
           MOVE WS-MICR-LINE TO CHECK-PRINT-REC
           WRITE CHECK-PRINT-REC

      *    Write check register entry
           MOVE WS-NEXT-CHECK-NUMBER TO CK-CHECK-NUMBER
           MOVE WS-CURRENT-DATE TO CK-CHECK-DATE
           MOVE CQ-PAYMENT-ID TO CK-PAYMENT-ID
           MOVE CQ-CLAIM-NUMBER TO CK-CLAIM-NUMBER
           MOVE CQ-PAYEE-NAME TO CK-PAYEE-NAME
           MOVE CQ-AMOUNT TO CK-AMOUNT
           MOVE WS-ACCOUNT-CODE TO CK-ACCOUNT-CODE
           SET CK-ISSUED TO TRUE
           MOVE ZEROS TO CK-VOID-DATE
           MOVE SPACES TO CK-VOID-REASON
           MOVE ZEROS TO CK-REISSUE-CHECK
           WRITE CHECK-REGISTER-REC

      *    Write reconciliation record
           MOVE WS-NEXT-CHECK-NUMBER TO RC-CHECK-NUMBER
           MOVE WS-CURRENT-DATE TO RC-ISSUE-DATE
           MOVE CQ-AMOUNT TO RC-ISSUE-AMOUNT
           MOVE ZEROS TO RC-CLEAR-DATE
           MOVE ZEROS TO RC-CLEAR-AMOUNT
           MOVE "I" TO RC-STATUS
           SET RC-UNMATCHED TO TRUE
           MOVE ZEROS TO RC-DAYS-OUTSTANDING
           WRITE RECON-REC

      *    Accumulate counters
           ADD 1 TO WS-CHECKS-PRINTED
           ADD CQ-AMOUNT TO WS-TOTAL-AMOUNT
           SET CQ-PRINTED TO TRUE.

       2100-EXIT.
           EXIT.

       2200-FORMAT-MICR.
      *    Format MICR E-13B encoding
      *    Transit: Cnnnnnnnn (routing with check digit)
      *    On-Us: check number + account number
      *    Amount: optional, usually not on laser checks
           STRING
               "C" DELIMITED SIZE
               CS-BANK-ROUTING DELIMITED SIZE
               "C" DELIMITED SIZE
               INTO WS-MICR-TRANSIT
           STRING
               WS-NEXT-CHECK-NUMBER DELIMITED SIZE
               "D" DELIMITED SIZE
               CS-BANK-ACCOUNT DELIMITED SIZE
               INTO WS-MICR-ON-US
           MOVE SPACES TO WS-MICR-AMOUNT
           MOVE SPACE TO WS-MICR-EPC.

       2300-AMOUNT-TO-WORDS.
      *    Convert numeric amount to English words for check
           MOVE SPACES TO WS-AMOUNT-WORDS
           MOVE WS-AMOUNT-WORK TO WS-WHOLE-PART
           COMPUTE WS-CENTS-PART =
               FUNCTION MOD(WS-AMOUNT-WORK * 100, 100)

           IF WS-WHOLE-PART = 0
               STRING "ZERO AND " WS-CENTS-PART "/100"
                   DELIMITED SIZE INTO WS-AMOUNT-WORDS
           ELSE
               PERFORM 2310-CONVERT-WHOLE-PART
               STRING WS-AMOUNT-WORDS DELIMITED SPACES
                   " AND " DELIMITED SIZE
                   WS-CENTS-PART DELIMITED SIZE
                   "/100" DELIMITED SIZE
                   INTO WS-AMOUNT-WORDS
           END-IF.

       2310-CONVERT-WHOLE-PART.
      *    Simplified conversion for amounts up to 999,999,999
           MOVE SPACES TO WS-AMOUNT-WORDS
           IF WS-WHOLE-PART >= 1000000
               MOVE "MILLION+ " TO WS-AMOUNT-WORDS
           ELSE IF WS-WHOLE-PART >= 1000
               MOVE "THOUSAND+ " TO WS-AMOUNT-WORDS
           ELSE IF WS-WHOLE-PART >= 100
               MOVE "HUNDRED+ " TO WS-AMOUNT-WORDS
           ELSE IF WS-WHOLE-PART >= 20
               MOVE WS-TENS-WORD(
                   (WS-WHOLE-PART / 10) - 1)
                   TO WS-AMOUNT-WORDS
           ELSE IF WS-WHOLE-PART >= 10
               MOVE WS-TEENS-WORD(WS-WHOLE-PART - 9)
                   TO WS-AMOUNT-WORDS
           ELSE
               MOVE WS-ONES-WORD(WS-WHOLE-PART)
                   TO WS-AMOUNT-WORDS
           END-IF.

       3000-GENERATE-POSITIVE-PAY.
      *    Write positive pay trailer with totals
           SET PP-TRAILER TO TRUE
           MOVE CS-BANK-ACCOUNT TO PP-ACCOUNT-NUMBER
           MOVE WS-CHECKS-PRINTED TO PP-CHECK-NUMBER
           MOVE WS-TOTAL-AMOUNT TO PP-AMOUNT
           MOVE WS-CURRENT-DATE TO PP-ISSUE-DATE
           MOVE SPACES TO PP-PAYEE-NAME
           SET PP-ISSUE TO TRUE
           WRITE POSITIVE-PAY-REC.

       8000-PRINT-REGISTER-TOTALS.
           DISPLAY "=== CHECK RUN SUMMARY ==="
           DISPLAY "Date:            " WS-CURRENT-DATE
           DISPLAY "Checks printed:  " WS-CHECKS-PRINTED
           DISPLAY "Total amount:    " WS-TOTAL-AMOUNT
           DISPLAY "Void count:      " WS-VOID-COUNT
           DISPLAY "Errors:          " WS-ERROR-COUNT
           DISPLAY "Next check#:     " WS-NEXT-CHECK-NUMBER

      *    Update sequence file with last check used
           MOVE WS-NEXT-CHECK-NUMBER TO CS-LAST-CHECK-NUM
           REWRITE CHECK-SEQ-REC.

       9000-TERMINATE.
           CLOSE CHECK-QUEUE-FILE
           CLOSE CHECK-SEQ-FILE
           CLOSE CHECK-REGISTER-FILE
           CLOSE POSITIVE-PAY-FILE
           CLOSE CHECK-PRINT-FILE
           CLOSE RECON-FILE.
