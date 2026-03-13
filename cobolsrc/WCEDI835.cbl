       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCEDI835.
      *================================================================*
      * PROGRAM:    WCEDI835                                           *
      * AUTHOR:     MAINFRAMEMOD TPA SYSTEMS                           *
      * DATE:       2024-02-20                                         *
      * PURPOSE:    EDI 835 (PAYMENT/REMITTANCE ADVICE) GENERATION     *
      *             BUILDS OUTBOUND 835 TRANSACTIONS FROM PAYMENT DATA *
      *             FOR CLEARINGHOUSE SUBMISSION                        *
      * PLATFORM:   HPE NONSTOP / GUARDIAN                             *
      *================================================================*
      * CHANGE LOG:                                                    *
      * 2024-02-20  INITIAL DEVELOPMENT                                *
      * 2024-05-15  ADDED PLB PROVIDER-LEVEL BALANCE SUPPORT           *
      * 2024-07-30  ADDED CARC/RARC CODE TABLES                        *
      * 2024-10-12  ENHANCED GROUPING LOGIC FOR MULTI-PROVIDER RUNS    *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYMENT-INPUT-FILE
               ASSIGN TO PAYIN
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PAY-FILE-STATUS.
           SELECT EDI-OUTPUT-FILE
               ASSIGN TO EDIOUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-EDI-FILE-STATUS.
           SELECT CONTROL-FILE
               ASSIGN TO CTLFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CTL-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYMENT-INPUT-FILE.
       01  PAYMENT-INPUT-RECORD.
           05  PI-RECORD-TYPE          PIC X(2).
               88  PI-PROVIDER-HDR     VALUE 'PH'.
               88  PI-CLAIM-REC        VALUE 'CL'.
               88  PI-SERVICE-REC      VALUE 'SV'.
               88  PI-ADJUSTMENT-REC   VALUE 'AJ'.
               88  PI-PLB-REC          VALUE 'PL'.
           05  PI-PROVIDER-NPI         PIC X(10).
           05  PI-PROVIDER-TAX-ID      PIC X(9).
           05  PI-PROVIDER-NAME        PIC X(60).
           05  PI-CLAIM-NUM            PIC X(20).
           05  PI-CLAIMANT-NAME        PIC X(60).
           05  PI-MEMBER-ID            PIC X(20).
           05  PI-FROM-DATE            PIC X(8).
           05  PI-THRU-DATE            PIC X(8).
           05  PI-PROC-CODE            PIC X(5).
           05  PI-MODIFIER             PIC X(2) OCCURS 4.
           05  PI-BILLED-AMT          PIC S9(7)V99 COMP-3.
           05  PI-ALLOWED-AMT         PIC S9(7)V99 COMP-3.
           05  PI-PAID-AMT            PIC S9(7)V99 COMP-3.
           05  PI-ADJ-GROUP           PIC X(2).
           05  PI-ADJ-REASON          PIC X(5).
           05  PI-ADJ-AMT             PIC S9(7)V99 COMP-3.
           05  PI-REMARK-CODE         PIC X(5).
           05  PI-PLB-ADJ-REASON      PIC X(5).
           05  PI-PLB-AMT             PIC S9(9)V99 COMP-3.
           05  PI-PLB-REF-ID          PIC X(20).
           05  FILLER                  PIC X(40).

       FD  EDI-OUTPUT-FILE
           RECORDING MODE IS V
           RECORD CONTAINS 1 TO 4096 CHARACTERS.
       01  EDI-OUTPUT-RECORD           PIC X(4096).

       FD  CONTROL-FILE.
       01  CONTROL-RECORD.
           05  CTL-LAST-ISA-NUM       PIC 9(9).
           05  CTL-LAST-GS-NUM        PIC 9(9).
           05  CTL-LAST-ST-NUM        PIC 9(9).

       WORKING-STORAGE SECTION.
      *
      *    Cross-program copybook references
           COPY WCPMTCPY
           COPY WCCLMCPY
       01  WS-FILE-STATUSES.
           05  WS-PAY-FILE-STATUS     PIC XX.
           05  WS-EDI-FILE-STATUS     PIC XX.
           05  WS-CTL-FILE-STATUS     PIC XX.

       01  WS-PROGRAM-FLAGS.
           05  WS-EOF-FLAG            PIC X VALUE 'N'.
               88  END-OF-FILE        VALUE 'Y'.
               88  NOT-END-OF-FILE    VALUE 'N'.
           05  WS-PROV-BREAK          PIC X VALUE 'N'.
               88  PROVIDER-CHANGED   VALUE 'Y'.

       01  WS-CONTROL-NUMBERS.
           05  WS-ISA-CONTROL-NUM     PIC 9(9) VALUE 0.
           05  WS-GS-CONTROL-NUM      PIC 9(9) VALUE 0.
           05  WS-ST-CONTROL-NUM      PIC 9(9) VALUE 0.
           05  WS-SEGMENT-COUNT       PIC 9(6) VALUE 0.
           05  WS-TRANS-SET-COUNT     PIC 9(5) VALUE 0.

       01  WS-CURRENT-PROVIDER.
           05  WS-CURR-NPI            PIC X(10).
           05  WS-CURR-TAX-ID         PIC X(9).
           05  WS-CURR-PROV-NAME      PIC X(60).
           05  WS-PROV-PAID-TOTAL     PIC S9(9)V99 COMP-3 VALUE 0.
           05  WS-PROV-CLAIM-COUNT    PIC 9(5) VALUE 0.

       01  WS-CURRENT-CLAIM.
           05  WS-CURR-CLAIM-NUM      PIC X(20).
           05  WS-CURR-MEMBER-ID      PIC X(20).
           05  WS-CURR-CLAIMANT       PIC X(60).
           05  WS-CLM-STATUS          PIC X(2).
               88  CLM-PAID           VALUE '1 '.
               88  CLM-DENIED         VALUE '4 '.
               88  CLM-PARTIAL        VALUE '2 '.
               88  CLM-REVERSED       VALUE '22'.
           05  WS-CLM-BILLED-TOTAL    PIC S9(7)V99 COMP-3 VALUE 0.
           05  WS-CLM-PAID-TOTAL      PIC S9(7)V99 COMP-3 VALUE 0.
           05  WS-CLM-SVC-COUNT       PIC 99 VALUE 0.
           05  WS-CLM-FROM-DATE       PIC X(8).
           05  WS-CLM-THRU-DATE       PIC X(8).

       01  WS-CARC-TABLE.
           05  WS-CARC-ENTRY OCCURS 20 TIMES.
               10  WS-CARC-GROUP      PIC X(2).
                   88  CARC-CO         VALUE 'CO'.
                   88  CARC-OA         VALUE 'OA'.
                   88  CARC-PI         VALUE 'PI'.
                   88  CARC-PR         VALUE 'PR'.
               10  WS-CARC-CODE       PIC X(5).
               10  WS-CARC-AMT        PIC S9(7)V99 COMP-3.
           05  WS-CARC-COUNT          PIC 99 VALUE 0.

       01  WS-RARC-TABLE.
           05  WS-RARC-ENTRY OCCURS 10 TIMES.
               10  WS-RARC-CODE       PIC X(5).
           05  WS-RARC-COUNT          PIC 99 VALUE 0.

       01  WS-PLB-TABLE.
           05  WS-PLB-ENTRY OCCURS 20 TIMES.
               10  WS-PLB-REASON      PIC X(5).
               10  WS-PLB-AMT         PIC S9(9)V99 COMP-3.
               10  WS-PLB-REF         PIC X(20).
           05  WS-PLB-COUNT           PIC 99 VALUE 0.

       01  WS-COUNTERS.
           05  WS-PROVIDERS-PROC      PIC 9(5) VALUE 0.
           05  WS-CLAIMS-PROC         PIC 9(7) VALUE 0.
           05  WS-SERVICES-PROC       PIC 9(9) VALUE 0.
           05  WS-TOTAL-PAID          PIC S9(11)V99 COMP-3 VALUE 0.
           05  WS-RECORDS-READ        PIC 9(9) VALUE 0.

       01  WS-OUTPUT-BUFFER           PIC X(4096).
       01  WS-ELEMENT-DELIM           PIC X VALUE '*'.
       01  WS-SEGMENT-TERM            PIC X VALUE '~'.
       01  WS-WORK-AMT                PIC -(7)9.99.
       01  WS-WORK-AMT-EDITED         PIC X(12).
       01  WS-WORK-NUM9               PIC 9(9).
       01  WS-WORK-NUM6               PIC 9(6).
       01  WS-SUB-IDX                 PIC 99.

       01  WS-PAYER-INFO.
           05  WS-PAYER-NAME          PIC X(60)
               VALUE 'ACME WORKERS COMP TPA'.
           05  WS-PAYER-ID            PIC X(10)
               VALUE '1234567890'.
           05  WS-PAYER-ADDR          PIC X(55)
               VALUE '100 CLAIMS PLAZA'.
           05  WS-PAYER-CITY          PIC X(30) VALUE 'DALLAS'.
           05  WS-PAYER-STATE         PIC XX VALUE 'TX'.
           05  WS-PAYER-ZIP           PIC X(9) VALUE '752010000'.

       01  WS-DATES.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YYYY       PIC 9(4).
               10  WS-CURR-MM         PIC 9(2).
               10  WS-CURR-DD         PIC 9(2).
           05  WS-ISA-DATE            PIC X(6).
           05  WS-ISA-TIME            PIC X(4).
           05  WS-GS-DATE             PIC X(8).
           05  WS-CHECK-DATE          PIC X(8).
           05  WS-CURRENT-TIME        PIC 9(8).

       01  WS-PREV-CLAIM-NUM          PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
      *
      *    Inter-program communication calls
           CALL "PMTPROC"
           PERFORM 1000-INITIALIZE
           PERFORM 2000-BUILD-INTERCHANGE-HEADER
           PERFORM 3000-PROCESS-PAYMENTS
           PERFORM 7000-BUILD-INTERCHANGE-TRAILER
           PERFORM 9000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT  PAYMENT-INPUT-FILE
           OPEN OUTPUT EDI-OUTPUT-FILE
           OPEN I-O    CONTROL-FILE
           IF WS-PAY-FILE-STATUS NOT = '00'
               DISPLAY 'WCEDI835: FATAL - CANNOT OPEN PAYMENT FILE '
                       WS-PAY-FILE-STATUS
               STOP RUN
           END-IF
           READ CONTROL-FILE INTO CONTROL-RECORD
           MOVE CTL-LAST-ISA-NUM TO WS-ISA-CONTROL-NUM
           MOVE CTL-LAST-GS-NUM TO WS-GS-CONTROL-NUM
           MOVE CTL-LAST-ST-NUM TO WS-ST-CONTROL-NUM
           ADD 1 TO WS-ISA-CONTROL-NUM
           ADD 1 TO WS-GS-CONTROL-NUM
           ADD 1 TO WS-ST-CONTROL-NUM
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           STRING WS-CURR-YYYY(3:2) WS-CURR-MM WS-CURR-DD
               DELIMITED BY SIZE INTO WS-ISA-DATE
           MOVE WS-CURRENT-TIME(1:4) TO WS-ISA-TIME
           STRING WS-CURR-YYYY WS-CURR-MM WS-CURR-DD
               DELIMITED BY SIZE INTO WS-GS-DATE
           MOVE WS-GS-DATE TO WS-CHECK-DATE
           INITIALIZE WS-COUNTERS
           DISPLAY 'WCEDI835: STARTED - GENERATING 835 TRANSACTIONS'.

       2000-BUILD-INTERCHANGE-HEADER.
           MOVE WS-ISA-CONTROL-NUM TO WS-WORK-NUM9
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'ISA*00*          *00*          *'
               'ZZ*' WS-PAYER-ID '     *'
               'ZZ*CLEARINGHOUSE *'
               WS-ISA-DATE '*' WS-ISA-TIME
               '*^*00501*' WS-WORK-NUM9 '*0*P*:~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           MOVE WS-GS-CONTROL-NUM TO WS-WORK-NUM9
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'GS*HP*' WS-PAYER-ID '*CLEARINGHS*'
               WS-GS-DATE '*'
               WS-ISA-TIME '*' WS-WORK-NUM9 '*X*005010X221A1~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT.

       2100-BUILD-ST-HEADER.
           MOVE WS-ST-CONTROL-NUM TO WS-WORK-NUM9
           MOVE 0 TO WS-SEGMENT-COUNT
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'ST*835*' WS-WORK-NUM9 '*005010X221A1~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'BPR*I*'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           MOVE WS-PROV-PAID-TOTAL TO WS-WORK-AMT
           STRING
               WS-OUTPUT-BUFFER DELIMITED BY '  '
               FUNCTION TRIM(WS-WORK-AMT)
               '*C*ACH*01*091000019*DA*1234567890**'
               '01*091000019*DA*9876543210*' WS-CHECK-DATE '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'TRN*1*' FUNCTION TRIM(WS-CURR-NPI)
               '*1' WS-PAYER-ID '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'DTM*405*' WS-CHECK-DATE '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           PERFORM 2200-BUILD-PAYER-ID
           PERFORM 2300-BUILD-PAYEE-ID.

       2200-BUILD-PAYER-ID.
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'N1*PR*' FUNCTION TRIM(WS-PAYER-NAME) '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'N3*' FUNCTION TRIM(WS-PAYER-ADDR) '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'N4*' FUNCTION TRIM(WS-PAYER-CITY) '*'
               WS-PAYER-STATE '*' WS-PAYER-ZIP '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'REF*2U*' WS-PAYER-ID '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT.

       2300-BUILD-PAYEE-ID.
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'N1*PE*' FUNCTION TRIM(WS-CURR-PROV-NAME)
               '*XX*' FUNCTION TRIM(WS-CURR-NPI) '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'REF*TJ*' WS-CURR-TAX-ID '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT.

       3000-PROCESS-PAYMENTS.
           READ PAYMENT-INPUT-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-RECORDS-READ
               EVALUATE TRUE
                   WHEN PI-PROVIDER-HDR
                       PERFORM 3100-HANDLE-PROVIDER
                   WHEN PI-CLAIM-REC
                       PERFORM 3200-HANDLE-CLAIM
                   WHEN PI-SERVICE-REC
                       PERFORM 3300-HANDLE-SERVICE
                   WHEN PI-ADJUSTMENT-REC
                       PERFORM 3400-HANDLE-ADJUSTMENT
                   WHEN PI-PLB-REC
                       PERFORM 3500-HANDLE-PLB
               END-EVALUATE
               READ PAYMENT-INPUT-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM
           IF WS-CURR-NPI NOT = SPACES
               PERFORM 5000-CLOSE-CLAIM
               PERFORM 6000-BUILD-PLB
               PERFORM 6500-BUILD-SE-TRAILER
           END-IF.

       3100-HANDLE-PROVIDER.
           IF WS-CURR-NPI NOT = SPACES
               PERFORM 5000-CLOSE-CLAIM
               PERFORM 6000-BUILD-PLB
               PERFORM 6500-BUILD-SE-TRAILER
           END-IF
           ADD 1 TO WS-PROVIDERS-PROC
           MOVE PI-PROVIDER-NPI  TO WS-CURR-NPI
           MOVE PI-PROVIDER-TAX-ID TO WS-CURR-TAX-ID
           MOVE PI-PROVIDER-NAME TO WS-CURR-PROV-NAME
           MOVE 0 TO WS-PROV-PAID-TOTAL
           MOVE 0 TO WS-PROV-CLAIM-COUNT
           MOVE 0 TO WS-PLB-COUNT
           MOVE SPACES TO WS-PREV-CLAIM-NUM
           ADD 1 TO WS-ST-CONTROL-NUM
           PERFORM 2100-BUILD-ST-HEADER.

       3200-HANDLE-CLAIM.
           IF WS-PREV-CLAIM-NUM NOT = SPACES
               AND PI-CLAIM-NUM NOT = WS-PREV-CLAIM-NUM
               PERFORM 5000-CLOSE-CLAIM
           END-IF
           ADD 1 TO WS-CLAIMS-PROC
           ADD 1 TO WS-PROV-CLAIM-COUNT
           MOVE PI-CLAIM-NUM TO WS-CURR-CLAIM-NUM
           MOVE PI-MEMBER-ID TO WS-CURR-MEMBER-ID
           MOVE PI-CLAIMANT-NAME TO WS-CURR-CLAIMANT
           MOVE 0 TO WS-CLM-BILLED-TOTAL
           MOVE 0 TO WS-CLM-PAID-TOTAL
           MOVE 0 TO WS-CLM-SVC-COUNT
           MOVE 0 TO WS-CARC-COUNT
           MOVE 0 TO WS-RARC-COUNT
           MOVE PI-FROM-DATE TO WS-CLM-FROM-DATE
           MOVE PI-THRU-DATE TO WS-CLM-THRU-DATE
           MOVE PI-CLAIM-NUM TO WS-PREV-CLAIM-NUM.

       3300-HANDLE-SERVICE.
           ADD 1 TO WS-SERVICES-PROC
           ADD 1 TO WS-CLM-SVC-COUNT
           ADD PI-BILLED-AMT TO WS-CLM-BILLED-TOTAL
           ADD PI-PAID-AMT TO WS-CLM-PAID-TOTAL

           INITIALIZE WS-OUTPUT-BUFFER
           MOVE PI-PAID-AMT TO WS-WORK-AMT
           STRING
               'SVC*HC:' PI-PROC-CODE
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           IF PI-MODIFIER(1) NOT = SPACES
               STRING WS-OUTPUT-BUFFER DELIMITED BY '  '
                   ':' PI-MODIFIER(1)
                   DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
               END-STRING
           END-IF
           MOVE PI-BILLED-AMT TO WS-WORK-AMT
           STRING WS-OUTPUT-BUFFER DELIMITED BY '  '
               '*' FUNCTION TRIM(WS-WORK-AMT)
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           MOVE PI-PAID-AMT TO WS-WORK-AMT
           STRING WS-OUTPUT-BUFFER DELIMITED BY '  '
               '*' FUNCTION TRIM(WS-WORK-AMT) '**'
               PI-FROM-DATE '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'DTM*472*' PI-FROM-DATE '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT.

       3400-HANDLE-ADJUSTMENT.
           ADD 1 TO WS-CARC-COUNT
           MOVE PI-ADJ-GROUP
               TO WS-CARC-GROUP(WS-CARC-COUNT)
           MOVE PI-ADJ-REASON
               TO WS-CARC-CODE(WS-CARC-COUNT)
           MOVE PI-ADJ-AMT
               TO WS-CARC-AMT(WS-CARC-COUNT)
           IF PI-REMARK-CODE NOT = SPACES
               ADD 1 TO WS-RARC-COUNT
               MOVE PI-REMARK-CODE
                   TO WS-RARC-CODE(WS-RARC-COUNT)
           END-IF.

       3500-HANDLE-PLB.
           ADD 1 TO WS-PLB-COUNT
           MOVE PI-PLB-ADJ-REASON TO WS-PLB-REASON(WS-PLB-COUNT)
           MOVE PI-PLB-AMT TO WS-PLB-AMT(WS-PLB-COUNT)
           MOVE PI-PLB-REF-ID TO WS-PLB-REF(WS-PLB-COUNT).

       5000-CLOSE-CLAIM.
           IF WS-PREV-CLAIM-NUM = SPACES
               EXIT PARAGRAPH
           END-IF
           IF WS-CLM-PAID-TOTAL > 0
               MOVE '1 ' TO WS-CLM-STATUS
           ELSE IF WS-CLM-BILLED-TOTAL > 0
               AND WS-CLM-PAID-TOTAL = 0
               MOVE '4 ' TO WS-CLM-STATUS
           ELSE
               MOVE '2 ' TO WS-CLM-STATUS
           END-IF

           INITIALIZE WS-OUTPUT-BUFFER
           MOVE WS-CLM-BILLED-TOTAL TO WS-WORK-AMT
           STRING
               'CLP*' FUNCTION TRIM(WS-CURR-CLAIM-NUM)
               '*' WS-CLM-STATUS
               '*' FUNCTION TRIM(WS-WORK-AMT)
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           MOVE WS-CLM-PAID-TOTAL TO WS-WORK-AMT
           STRING WS-OUTPUT-BUFFER DELIMITED BY '  '
               '*' FUNCTION TRIM(WS-WORK-AMT) '***WC*'
               WS-CLM-FROM-DATE '*' WS-CLM-THRU-DATE '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT
           ADD WS-CLM-PAID-TOTAL TO WS-PROV-PAID-TOTAL
           ADD WS-CLM-PAID-TOTAL TO WS-TOTAL-PAID

           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'NM1*QC*1*' FUNCTION TRIM(WS-CURR-CLAIMANT)
               '****MI*' FUNCTION TRIM(WS-CURR-MEMBER-ID) '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT

           PERFORM 5100-WRITE-CAS-SEGMENTS

           MOVE SPACES TO WS-PREV-CLAIM-NUM.

       5100-WRITE-CAS-SEGMENTS.
           IF WS-CARC-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-WORK-AMT-EDITED
           PERFORM VARYING WS-SUB-IDX FROM 1 BY 1
               UNTIL WS-SUB-IDX > WS-CARC-COUNT
               INITIALIZE WS-OUTPUT-BUFFER
               MOVE WS-CARC-AMT(WS-SUB-IDX) TO WS-WORK-AMT
               STRING
                   'CAS*' WS-CARC-GROUP(WS-SUB-IDX)
                   '*' FUNCTION TRIM(WS-CARC-CODE(WS-SUB-IDX))
                   '*' FUNCTION TRIM(WS-WORK-AMT) '~'
                   DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
               END-STRING
               WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
               ADD 1 TO WS-SEGMENT-COUNT
           END-PERFORM
           IF WS-RARC-COUNT > 0
               INITIALIZE WS-OUTPUT-BUFFER
               STRING 'LQ*HE'
                   DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
               END-STRING
               PERFORM VARYING WS-SUB-IDX FROM 1 BY 1
                   UNTIL WS-SUB-IDX > WS-RARC-COUNT
                   STRING WS-OUTPUT-BUFFER DELIMITED BY '  '
                       '*' FUNCTION TRIM(
                           WS-RARC-CODE(WS-SUB-IDX))
                       DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
                   END-STRING
               END-PERFORM
               STRING WS-OUTPUT-BUFFER DELIMITED BY '  '
                   '~' DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
               END-STRING
               WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
               ADD 1 TO WS-SEGMENT-COUNT
           END-IF.

       6000-BUILD-PLB.
           IF WS-PLB-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'PLB*' FUNCTION TRIM(WS-CURR-TAX-ID)
               '*' WS-CHECK-DATE
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           PERFORM VARYING WS-SUB-IDX FROM 1 BY 1
               UNTIL WS-SUB-IDX > WS-PLB-COUNT
               MOVE WS-PLB-AMT(WS-SUB-IDX) TO WS-WORK-AMT
               STRING WS-OUTPUT-BUFFER DELIMITED BY '  '
                   '*' FUNCTION TRIM(
                       WS-PLB-REASON(WS-SUB-IDX))
                   ':' FUNCTION TRIM(
                       WS-PLB-REF(WS-SUB-IDX))
                   '*' FUNCTION TRIM(WS-WORK-AMT)
                   DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
               END-STRING
           END-PERFORM
           STRING WS-OUTPUT-BUFFER DELIMITED BY '  '
               '~' DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER
           ADD 1 TO WS-SEGMENT-COUNT.

       6500-BUILD-SE-TRAILER.
           ADD 1 TO WS-SEGMENT-COUNT
           ADD 1 TO WS-TRANS-SET-COUNT
           MOVE WS-SEGMENT-COUNT TO WS-WORK-NUM6
           MOVE WS-ST-CONTROL-NUM TO WS-WORK-NUM9
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'SE*' FUNCTION TRIM(WS-WORK-NUM6)
               '*' WS-WORK-NUM9 '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER.

       7000-BUILD-INTERCHANGE-TRAILER.
           MOVE WS-TRANS-SET-COUNT TO WS-WORK-NUM6
           MOVE WS-GS-CONTROL-NUM TO WS-WORK-NUM9
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'GE*' FUNCTION TRIM(WS-WORK-NUM6)
               '*' WS-WORK-NUM9 '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER

           MOVE WS-ISA-CONTROL-NUM TO WS-WORK-NUM9
           INITIALIZE WS-OUTPUT-BUFFER
           STRING
               'IEA*1*' WS-WORK-NUM9 '~'
               DELIMITED BY SIZE INTO WS-OUTPUT-BUFFER
           END-STRING
           WRITE EDI-OUTPUT-RECORD FROM WS-OUTPUT-BUFFER.

       9000-FINALIZE.
           MOVE WS-ISA-CONTROL-NUM TO CTL-LAST-ISA-NUM
           MOVE WS-GS-CONTROL-NUM TO CTL-LAST-GS-NUM
           MOVE WS-ST-CONTROL-NUM TO CTL-LAST-ST-NUM
           REWRITE CONTROL-RECORD
           CLOSE PAYMENT-INPUT-FILE
                 EDI-OUTPUT-FILE
                 CONTROL-FILE
           DISPLAY 'WCEDI835: COMPLETED'
           DISPLAY 'WCEDI835: PROVIDERS PROCESSED = '
                   WS-PROVIDERS-PROC
           DISPLAY 'WCEDI835: CLAIMS PROCESSED    = '
                   WS-CLAIMS-PROC
           DISPLAY 'WCEDI835: SERVICES PROCESSED  = '
                   WS-SERVICES-PROC
           DISPLAY 'WCEDI835: TOTAL PAID AMOUNT   = '
                   WS-TOTAL-PAID
           DISPLAY 'WCEDI835: RECORDS READ        = '
                   WS-RECORDS-READ.
