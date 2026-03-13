       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCNCM.
      *================================================================
      * WCNCM - Workers' Compensation Nurse Case Management
      *
      * Manages the NCM lifecycle for workers' compensation claims:
      * referral intake, treatment plan tracking, provider
      * communication, return-to-work facilitation, medical record
      * requests, peer review/IME coordination, activity diary
      * with time tracking, and outcome measurement.
      *
      * File: NCMFILF (NCM case records, key-sequenced)
      * File: NCMACTF (NCM activity diary, entry-sequenced)
      * File: NCMRPTF (NCM monthly report output)
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NCM-CASE-FILE
               ASSIGN TO "NCMFILF"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NCM-CASE-KEY
               FILE STATUS IS WS-FILE-STATUS.

           SELECT NCM-ACTIVITY-FILE
               ASSIGN TO "NCMACTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT NCM-REPORT-FILE
               ASSIGN TO "NCMRPTF"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  NCM-CASE-FILE.
       01  NCM-CASE-RECORD.
           05 NCM-CASE-KEY.
              10 NCM-CLAIM-NUMBER         PIC X(12).
              10 NCM-REFERRAL-SEQ         PIC 9(3).
           05 NCM-NURSE-ID               PIC X(8).
           05 NCM-NURSE-NAME             PIC X(30).
           05 NCM-STATUS                 PIC X(2).
              88 NCM-STAT-REFERRED       VALUE "RF".
              88 NCM-STAT-ASSIGNED       VALUE "AS".
              88 NCM-STAT-ACTIVE         VALUE "AC".
              88 NCM-STAT-RTW-PLAN       VALUE "RP".
              88 NCM-STAT-MONITORING     VALUE "MN".
              88 NCM-STAT-CLOSED         VALUE "CL".
           05 NCM-REFERRAL-DATE          PIC 9(8).
           05 NCM-ASSIGNED-DATE          PIC 9(8).
           05 NCM-CLOSE-DATE             PIC 9(8).
           05 NCM-CLOSE-REASON           PIC X(2).
              88 NCM-CLOSE-RTW           VALUE "RW".
              88 NCM-CLOSE-MMI           VALUE "MM".
              88 NCM-CLOSE-SETTLE        VALUE "ST".
              88 NCM-CLOSE-NONCOMPLIANT  VALUE "NC".
           05 NCM-CLAIMANT-INFO.
              10 NCM-CLAIMANT-NAME       PIC X(30).
              10 NCM-CLAIMANT-DOB        PIC 9(8).
              10 NCM-INJURY-DATE         PIC 9(8).
              10 NCM-INJURY-DESC         PIC X(60).
              10 NCM-BODY-PART           PIC X(4).
              10 NCM-NATURE-INJURY       PIC X(4).
           05 NCM-TREATMENT-PLAN.
              10 NCM-TREATING-PHYS       PIC X(30).
              10 NCM-PHYS-PHONE          PIC X(10).
              10 NCM-PHYS-SPECIALTY      PIC X(20).
              10 NCM-DIAGNOSIS-1         PIC X(8).
              10 NCM-DIAGNOSIS-2         PIC X(8).
              10 NCM-DIAGNOSIS-3         PIC X(8).
              10 NCM-TREATMENT-TYPE      PIC X(2).
                 88 NCM-TX-SURGERY       VALUE "SU".
                 88 NCM-TX-PT            VALUE "PT".
                 88 NCM-TX-PAIN-MGMT     VALUE "PM".
                 88 NCM-TX-CONSULT       VALUE "CO".
              10 NCM-TX-START-DATE       PIC 9(8).
              10 NCM-TX-TARGET-END       PIC 9(8).
              10 NCM-TX-ACTUAL-END       PIC 9(8).
              10 NCM-TX-STATUS           PIC X(1).
                 88 NCM-TX-PLANNED       VALUE "P".
                 88 NCM-TX-IN-PROGRESS   VALUE "I".
                 88 NCM-TX-COMPLETED     VALUE "C".
                 88 NCM-TX-DISCONTINUED  VALUE "D".
              10 NCM-TX-NOTES            PIC X(200).
           05 NCM-RTW-INFO.
              10 NCM-RTW-STATUS          PIC X(2).
                 88 NCM-RTW-FULL         VALUE "FD".
                 88 NCM-RTW-MODIFIED     VALUE "MD".
                 88 NCM-RTW-PART-TIME    VALUE "PT".
                 88 NCM-RTW-NOT-READY    VALUE "NR".
                 88 NCM-RTW-PERMANENT    VALUE "PD".
              10 NCM-RTW-TARGET-DATE     PIC 9(8).
              10 NCM-RTW-ACTUAL-DATE     PIC 9(8).
              10 NCM-RTW-RESTRICTIONS    PIC X(100).
              10 NCM-EMPLOYER-NAME       PIC X(30).
              10 NCM-EMPLOYER-CONTACT    PIC X(30).
              10 NCM-EMPLOYER-PHONE      PIC X(10).
              10 NCM-JOB-AVAILABLE       PIC X(1).
                 88 NCM-JOB-YES          VALUE "Y".
                 88 NCM-JOB-NO           VALUE "N".
                 88 NCM-JOB-MODIFIED     VALUE "M".
           05 NCM-MED-RECORDS.
              10 NCM-RECORDS-REQUESTED   PIC 9(3).
              10 NCM-RECORDS-RECEIVED    PIC 9(3).
              10 NCM-LAST-REQUEST-DATE   PIC 9(8).
              10 NCM-PEER-REVIEW-REQ     PIC X(1).
                 88 NCM-PEER-YES         VALUE "Y".
              10 NCM-PEER-REVIEW-DATE    PIC 9(8).
              10 NCM-PEER-REVIEW-RESULT  PIC X(2).
              10 NCM-IME-REQUESTED       PIC X(1).
                 88 NCM-IME-YES          VALUE "Y".
              10 NCM-IME-DATE            PIC 9(8).
              10 NCM-IME-PHYSICIAN       PIC X(30).
              10 NCM-IME-RESULT          PIC X(2).
                 88 NCM-IME-AGREE        VALUE "AG".
                 88 NCM-IME-DISAGREE     VALUE "DG".
           05 NCM-OUTCOME-DATA.
              10 NCM-DAYS-TO-MMI         PIC 9(5).
              10 NCM-TOTAL-HOURS         PIC 9(5)V99.
              10 NCM-TOTAL-COST          PIC 9(7)V99.
              10 NCM-SAVINGS-EST         PIC 9(7)V99.

       FD  NCM-ACTIVITY-FILE.
       01  NCM-ACTIVITY-RECORD.
           05 NCM-ACT-CLAIM-NUMBER       PIC X(12).
           05 NCM-ACT-REFERRAL-SEQ       PIC 9(3).
           05 NCM-ACT-DATE               PIC 9(8).
           05 NCM-ACT-TIME               PIC 9(6).
           05 NCM-ACT-NURSE-ID           PIC X(8).
           05 NCM-ACT-TYPE               PIC X(2).
              88 NCM-ACT-PHONE-CLMT      VALUE "PC".
              88 NCM-ACT-PHONE-PHYS      VALUE "PP".
              88 NCM-ACT-PHONE-EMPL      VALUE "PE".
              88 NCM-ACT-PHONE-ATTY      VALUE "PA".
              88 NCM-ACT-OFFICE-VISIT    VALUE "OV".
              88 NCM-ACT-MED-REVIEW      VALUE "MR".
              88 NCM-ACT-RTW-COORD       VALUE "RW".
              88 NCM-ACT-REPORT          VALUE "RP".
              88 NCM-ACT-CORR-IN         VALUE "CI".
              88 NCM-ACT-CORR-OUT        VALUE "CO".
           05 NCM-ACT-MINUTES            PIC 9(4).
           05 NCM-ACT-MILEAGE            PIC 9(4)V9.
           05 NCM-ACT-NARRATIVE          PIC X(500).
           05 NCM-ACT-FOLLOW-UP-DATE     PIC 9(8).
           05 NCM-ACT-FOLLOW-UP-ACTION   PIC X(100).

       FD  NCM-REPORT-FILE.
       01  NCM-REPORT-LINE               PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS                PIC X(2).
       01  WS-CURRENT-DATE               PIC 9(8).
       01  WS-CURRENT-TIME               PIC 9(6).
       01  WS-EOF-FLAG                   PIC X(1) VALUE "N".
           88 WS-EOF                     VALUE "Y".
       01  WS-FOUND-FLAG                 PIC X(1) VALUE "N".
           88 WS-FOUND                   VALUE "Y".

       01  WS-REQUEST-AREA.
           05 WS-REQ-FUNCTION            PIC X(2).
              88 WS-REQ-ADD-REFERRAL     VALUE "AR".
              88 WS-REQ-ASSIGN-NURSE     VALUE "AN".
              88 WS-REQ-UPDATE-PLAN      VALUE "UP".
              88 WS-REQ-LOG-ACTIVITY     VALUE "LA".
              88 WS-REQ-UPDATE-RTW       VALUE "UR".
              88 WS-REQ-REQUEST-RECORDS  VALUE "RR".
              88 WS-REQ-SCHEDULE-IME     VALUE "SI".
              88 WS-REQ-CLOSE-CASE       VALUE "CC".
              88 WS-REQ-GEN-REPORT       VALUE "GR".
           05 WS-REQ-CLAIM-NUMBER        PIC X(12).
           05 WS-REQ-DATA                PIC X(500).

       01  WS-REPORT-COUNTERS.
           05 WS-RPT-TOTAL-CASES         PIC 9(5) VALUE 0.
           05 WS-RPT-ACTIVE-CASES        PIC 9(5) VALUE 0.
           05 WS-RPT-CLOSED-CASES        PIC 9(5) VALUE 0.
           05 WS-RPT-RTW-SUCCESS         PIC 9(5) VALUE 0.
           05 WS-RPT-TOTAL-HOURS         PIC 9(7)V99 VALUE 0.
           05 WS-RPT-TOTAL-COST          PIC 9(9)V99 VALUE 0.
           05 WS-RPT-TOTAL-SAVINGS       PIC 9(9)V99 VALUE 0.
           05 WS-RPT-AVG-DAYS-MMI        PIC 9(5)V99 VALUE 0.
           05 WS-RPT-RTW-RATE            PIC 9(3)V99 VALUE 0.
           05 WS-RPT-MMI-ACCUM           PIC 9(9) VALUE 0.
           05 WS-RPT-MMI-COUNT           PIC 9(5) VALUE 0.

       01  WS-HOURLY-RATE                PIC 9(3)V99 VALUE 125.00.
       01  WS-MILEAGE-RATE               PIC 9(1)V999 VALUE 0.670.

       01  WS-DATE-WORK.
           05 WS-DATE-1                  PIC 9(8).
           05 WS-DATE-2                  PIC 9(8).
           05 WS-DATE-DIFF-DAYS          PIC S9(5).
           05 WS-DATE-YYYY               PIC 9(4).
           05 WS-DATE-MM                 PIC 9(2).
           05 WS-DATE-DD                 PIC 9(2).

       01  WS-RPT-HDR-1.
           05 FILLER          PIC X(35)
              VALUE "NURSE CASE MANAGEMENT MONTHLY REPORT".
           05 FILLER          PIC X(20) VALUE SPACES.
           05 WS-RPT-DATE    PIC X(10).
           05 FILLER          PIC X(67) VALUE SPACES.

       01  WS-RPT-DETAIL.
           05 WS-RPT-CLAIM   PIC X(12).
           05 FILLER          PIC X(2) VALUE SPACES.
           05 WS-RPT-NAME    PIC X(25).
           05 FILLER          PIC X(2) VALUE SPACES.
           05 WS-RPT-STATUS  PIC X(10).
           05 FILLER          PIC X(2) VALUE SPACES.
           05 WS-RPT-NURSE   PIC X(20).
           05 FILLER          PIC X(2) VALUE SPACES.
           05 WS-RPT-HOURS   PIC ZZ,ZZ9.99.
           05 FILLER          PIC X(2) VALUE SPACES.
           05 WS-RPT-COST    PIC $$$,$$$,$$9.99.
           05 FILLER          PIC X(2) VALUE SPACES.
           05 WS-RPT-RTW-DT  PIC X(10).
           05 FILLER          PIC X(17) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUEST
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O NCM-CASE-FILE
           OPEN I-O NCM-ACTIVITY-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME.

       2000-PROCESS-REQUEST.
           EVALUATE TRUE
               WHEN WS-REQ-ADD-REFERRAL
                   PERFORM 3000-ADD-REFERRAL
               WHEN WS-REQ-ASSIGN-NURSE
                   PERFORM 3100-ASSIGN-NURSE
               WHEN WS-REQ-UPDATE-PLAN
                   PERFORM 3200-UPDATE-TREATMENT-PLAN
               WHEN WS-REQ-LOG-ACTIVITY
                   PERFORM 3300-LOG-ACTIVITY
               WHEN WS-REQ-UPDATE-RTW
                   PERFORM 3400-UPDATE-RTW-STATUS
               WHEN WS-REQ-REQUEST-RECORDS
                   PERFORM 3500-REQUEST-MEDICAL-RECORDS
               WHEN WS-REQ-SCHEDULE-IME
                   PERFORM 3600-SCHEDULE-IME
               WHEN WS-REQ-CLOSE-CASE
                   PERFORM 3700-CLOSE-CASE
               WHEN WS-REQ-GEN-REPORT
                   PERFORM 4000-GENERATE-MONTHLY-REPORT
           END-EVALUATE.

       3000-ADD-REFERRAL.
      *    Create new NCM referral for a claim
           INITIALIZE NCM-CASE-RECORD
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           SET NCM-STAT-REFERRED TO TRUE
           MOVE WS-CURRENT-DATE TO NCM-REFERRAL-DATE
           MOVE ZEROS TO NCM-ASSIGNED-DATE
           MOVE ZEROS TO NCM-CLOSE-DATE

      *    Populate claimant info from request data
           MOVE WS-REQ-DATA(1:30) TO NCM-CLAIMANT-NAME
           MOVE WS-REQ-DATA(31:8) TO NCM-CLAIMANT-DOB
           MOVE WS-REQ-DATA(39:8) TO NCM-INJURY-DATE
           MOVE WS-REQ-DATA(47:60) TO NCM-INJURY-DESC
           MOVE WS-REQ-DATA(107:4) TO NCM-BODY-PART
           MOVE WS-REQ-DATA(111:4) TO NCM-NATURE-INJURY

           INITIALIZE NCM-OUTCOME-DATA
           WRITE NCM-CASE-RECORD
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "ERROR WRITING NCM REFERRAL: " WS-FILE-STATUS
           END-IF.

       3100-ASSIGN-NURSE.
      *    Assign a nurse case manager to the referral
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           READ NCM-CASE-FILE
               INVALID KEY
                   DISPLAY "NCM CASE NOT FOUND: " NCM-CASE-KEY
                   GO TO 3100-EXIT
           END-READ

           MOVE WS-REQ-DATA(1:8) TO NCM-NURSE-ID
           MOVE WS-REQ-DATA(9:30) TO NCM-NURSE-NAME
           MOVE WS-CURRENT-DATE TO NCM-ASSIGNED-DATE
           SET NCM-STAT-ASSIGNED TO TRUE

           REWRITE NCM-CASE-RECORD
           PERFORM 3300-LOG-INITIAL-CONTACT.
       3100-EXIT.
           EXIT.

       3200-UPDATE-TREATMENT-PLAN.
      *    Update treatment plan from physician communication
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           READ NCM-CASE-FILE
               INVALID KEY
                   DISPLAY "NCM CASE NOT FOUND"
                   GO TO 3200-EXIT
           END-READ

           MOVE WS-REQ-DATA(1:30)  TO NCM-TREATING-PHYS
           MOVE WS-REQ-DATA(31:10) TO NCM-PHYS-PHONE
           MOVE WS-REQ-DATA(41:20) TO NCM-PHYS-SPECIALTY
           MOVE WS-REQ-DATA(61:8)  TO NCM-DIAGNOSIS-1
           MOVE WS-REQ-DATA(69:8)  TO NCM-DIAGNOSIS-2
           MOVE WS-REQ-DATA(77:8)  TO NCM-DIAGNOSIS-3
           MOVE WS-REQ-DATA(85:2)  TO NCM-TREATMENT-TYPE
           MOVE WS-REQ-DATA(87:8)  TO NCM-TX-START-DATE
           MOVE WS-REQ-DATA(95:8)  TO NCM-TX-TARGET-END
           SET NCM-TX-IN-PROGRESS TO TRUE
           SET NCM-STAT-ACTIVE TO TRUE
           MOVE WS-REQ-DATA(103:200) TO NCM-TX-NOTES

           REWRITE NCM-CASE-RECORD.
       3200-EXIT.
           EXIT.

       3300-LOG-ACTIVITY.
      *    Record NCM activity diary entry with time tracking
           INITIALIZE NCM-ACTIVITY-RECORD
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-ACT-CLAIM-NUMBER
           MOVE 1 TO NCM-ACT-REFERRAL-SEQ
           MOVE WS-CURRENT-DATE TO NCM-ACT-DATE
           MOVE WS-CURRENT-TIME TO NCM-ACT-TIME

      *    Parse activity data from request
           MOVE WS-REQ-DATA(1:8)   TO NCM-ACT-NURSE-ID
           MOVE WS-REQ-DATA(9:2)   TO NCM-ACT-TYPE
           MOVE WS-REQ-DATA(11:4)  TO NCM-ACT-MINUTES
           MOVE WS-REQ-DATA(15:5)  TO NCM-ACT-MILEAGE
           MOVE WS-REQ-DATA(20:500) TO NCM-ACT-NARRATIVE

           WRITE NCM-ACTIVITY-RECORD

      *    Update case totals
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           READ NCM-CASE-FILE
               INVALID KEY
                   GO TO 3300-EXIT
           END-READ

      *    Accumulate hours (convert minutes to hours)
           COMPUTE NCM-TOTAL-HOURS =
               NCM-TOTAL-HOURS +
               (NCM-ACT-MINUTES / 60)

      *    Accumulate cost (hourly rate + mileage)
           COMPUTE NCM-TOTAL-COST =
               NCM-TOTAL-COST +
               ((NCM-ACT-MINUTES / 60) * WS-HOURLY-RATE) +
               (NCM-ACT-MILEAGE * WS-MILEAGE-RATE)

           REWRITE NCM-CASE-RECORD.
       3300-EXIT.
           EXIT.

       3300-LOG-INITIAL-CONTACT.
      *    Auto-log initial contact activity on nurse assignment
           INITIALIZE NCM-ACTIVITY-RECORD
           MOVE NCM-CLAIM-NUMBER TO NCM-ACT-CLAIM-NUMBER
           MOVE NCM-REFERRAL-SEQ TO NCM-ACT-REFERRAL-SEQ
           MOVE WS-CURRENT-DATE TO NCM-ACT-DATE
           MOVE WS-CURRENT-TIME TO NCM-ACT-TIME
           MOVE NCM-NURSE-ID TO NCM-ACT-NURSE-ID
           SET NCM-ACT-PHONE-CLMT TO TRUE
           MOVE 15 TO NCM-ACT-MINUTES
           MOVE ZEROS TO NCM-ACT-MILEAGE
           STRING "INITIAL CONTACT - NURSE " DELIMITED BY SIZE
                  NCM-NURSE-NAME DELIMITED BY "  "
                  " ASSIGNED TO CASE. INTRODUCTORY CALL TO CLAIMANT."
                  DELIMITED BY SIZE
                  INTO NCM-ACT-NARRATIVE
           WRITE NCM-ACTIVITY-RECORD.

       3400-UPDATE-RTW-STATUS.
      *    Update return-to-work status and coordination notes
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           READ NCM-CASE-FILE
               INVALID KEY
                   DISPLAY "NCM CASE NOT FOUND"
                   GO TO 3400-EXIT
           END-READ

           MOVE WS-REQ-DATA(1:2)   TO NCM-RTW-STATUS
           MOVE WS-REQ-DATA(3:8)   TO NCM-RTW-TARGET-DATE
           MOVE WS-REQ-DATA(11:100) TO NCM-RTW-RESTRICTIONS
           MOVE WS-REQ-DATA(111:30) TO NCM-EMPLOYER-NAME
           MOVE WS-REQ-DATA(141:30) TO NCM-EMPLOYER-CONTACT
           MOVE WS-REQ-DATA(171:10) TO NCM-EMPLOYER-PHONE
           MOVE WS-REQ-DATA(181:1)  TO NCM-JOB-AVAILABLE

      *    If actual RTW occurred, record the date
           IF NCM-RTW-FULL OR NCM-RTW-MODIFIED
               MOVE WS-CURRENT-DATE TO NCM-RTW-ACTUAL-DATE
               SET NCM-STAT-RTW-PLAN TO TRUE

      *        Calculate savings estimate (WC indemnity avoided)
      *        Assumes $600/week avg indemnity rate
               PERFORM 8000-CALC-DATE-DIFF
               COMPUTE NCM-SAVINGS-EST =
                   (WS-DATE-DIFF-DAYS / 7) * 600
           END-IF

           REWRITE NCM-CASE-RECORD.
       3400-EXIT.
           EXIT.

       3500-REQUEST-MEDICAL-RECORDS.
      *    Track medical record requests to providers
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           READ NCM-CASE-FILE
               INVALID KEY
                   DISPLAY "NCM CASE NOT FOUND"
                   GO TO 3500-EXIT
           END-READ

           ADD 1 TO NCM-RECORDS-REQUESTED
           MOVE WS-CURRENT-DATE TO NCM-LAST-REQUEST-DATE

      *    Check if peer review is warranted (>90 days, no improvement)
           MOVE NCM-INJURY-DATE TO WS-DATE-1
           MOVE WS-CURRENT-DATE TO WS-DATE-2
           PERFORM 8000-CALC-DATE-DIFF
           IF WS-DATE-DIFF-DAYS > 90
               AND NCM-TX-IN-PROGRESS
               AND NOT NCM-RTW-FULL
               AND NOT NCM-RTW-MODIFIED
               SET NCM-PEER-YES TO TRUE
               MOVE WS-CURRENT-DATE TO NCM-PEER-REVIEW-DATE
           END-IF

           REWRITE NCM-CASE-RECORD.
       3500-EXIT.
           EXIT.

       3600-SCHEDULE-IME.
      *    Schedule Independent Medical Examination
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           READ NCM-CASE-FILE
               INVALID KEY
                   DISPLAY "NCM CASE NOT FOUND"
                   GO TO 3600-EXIT
           END-READ

           SET NCM-IME-YES TO TRUE
           MOVE WS-REQ-DATA(1:8) TO NCM-IME-DATE
           MOVE WS-REQ-DATA(9:30) TO NCM-IME-PHYSICIAN

      *    Log the IME scheduling activity
           INITIALIZE NCM-ACTIVITY-RECORD
           MOVE NCM-CLAIM-NUMBER TO NCM-ACT-CLAIM-NUMBER
           MOVE NCM-REFERRAL-SEQ TO NCM-ACT-REFERRAL-SEQ
           MOVE WS-CURRENT-DATE TO NCM-ACT-DATE
           MOVE WS-CURRENT-TIME TO NCM-ACT-TIME
           MOVE NCM-NURSE-ID TO NCM-ACT-NURSE-ID
           SET NCM-ACT-MED-REVIEW TO TRUE
           MOVE 30 TO NCM-ACT-MINUTES
           STRING "IME SCHEDULED WITH DR. " DELIMITED BY SIZE
                  NCM-IME-PHYSICIAN DELIMITED BY "  "
                  " FOR " DELIMITED BY SIZE
                  NCM-IME-DATE DELIMITED BY SIZE
                  INTO NCM-ACT-NARRATIVE
           WRITE NCM-ACTIVITY-RECORD

           REWRITE NCM-CASE-RECORD.
       3600-EXIT.
           EXIT.

       3700-CLOSE-CASE.
      *    Close NCM case with outcome data
           MOVE WS-REQ-CLAIM-NUMBER TO NCM-CLAIM-NUMBER
           MOVE 1 TO NCM-REFERRAL-SEQ
           READ NCM-CASE-FILE
               INVALID KEY
                   DISPLAY "NCM CASE NOT FOUND"
                   GO TO 3700-EXIT
           END-READ

           SET NCM-STAT-CLOSED TO TRUE
           MOVE WS-CURRENT-DATE TO NCM-CLOSE-DATE
           MOVE WS-REQ-DATA(1:2) TO NCM-CLOSE-REASON

      *    Calculate days from injury to MMI/closure
           MOVE NCM-INJURY-DATE TO WS-DATE-1
           MOVE WS-CURRENT-DATE TO WS-DATE-2
           PERFORM 8000-CALC-DATE-DIFF
           MOVE WS-DATE-DIFF-DAYS TO NCM-DAYS-TO-MMI

      *    Log closure activity
           INITIALIZE NCM-ACTIVITY-RECORD
           MOVE NCM-CLAIM-NUMBER TO NCM-ACT-CLAIM-NUMBER
           MOVE NCM-REFERRAL-SEQ TO NCM-ACT-REFERRAL-SEQ
           MOVE WS-CURRENT-DATE TO NCM-ACT-DATE
           MOVE WS-CURRENT-TIME TO NCM-ACT-TIME
           MOVE NCM-NURSE-ID TO NCM-ACT-NURSE-ID
           SET NCM-ACT-REPORT TO TRUE
           MOVE 60 TO NCM-ACT-MINUTES
           STRING "CASE CLOSED. REASON: " DELIMITED BY SIZE
                  NCM-CLOSE-REASON DELIMITED BY SIZE
                  ". TOTAL HOURS: " DELIMITED BY SIZE
                  NCM-TOTAL-HOURS DELIMITED BY SIZE
                  ". DAYS TO CLOSURE: " DELIMITED BY SIZE
                  NCM-DAYS-TO-MMI DELIMITED BY SIZE
                  INTO NCM-ACT-NARRATIVE
           WRITE NCM-ACTIVITY-RECORD

           REWRITE NCM-CASE-RECORD.
       3700-EXIT.
           EXIT.

       4000-GENERATE-MONTHLY-REPORT.
      *    Generate monthly NCM report with outcome metrics
           OPEN OUTPUT NCM-REPORT-FILE
           INITIALIZE WS-REPORT-COUNTERS

      *    Write report header
           MOVE WS-CURRENT-DATE TO WS-RPT-DATE
           WRITE NCM-REPORT-LINE FROM WS-RPT-HDR-1
           MOVE SPACES TO NCM-REPORT-LINE
           WRITE NCM-REPORT-LINE
           MOVE "CLAIM        CLAIMANT NAME             STATUS"
               & "      NURSE                HOURS      "
               & "COST             RTW DATE"
               TO NCM-REPORT-LINE
           WRITE NCM-REPORT-LINE
           MOVE ALL "-" TO NCM-REPORT-LINE
           WRITE NCM-REPORT-LINE

      *    Read all NCM cases sequentially
           MOVE LOW-VALUES TO NCM-CASE-KEY
           START NCM-CASE-FILE KEY >= NCM-CASE-KEY
               INVALID KEY
                   GO TO 4000-WRITE-TOTALS
           END-START

           MOVE "N" TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ NCM-CASE-FILE NEXT
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-RPT-TOTAL-CASES
                       IF NCM-STAT-ACTIVE OR NCM-STAT-RTW-PLAN
                           OR NCM-STAT-MONITORING
                           ADD 1 TO WS-RPT-ACTIVE-CASES
                       END-IF
                       IF NCM-STAT-CLOSED
                           ADD 1 TO WS-RPT-CLOSED-CASES
                           IF NCM-CLOSE-RTW
                               ADD 1 TO WS-RPT-RTW-SUCCESS
                           END-IF
                           IF NCM-DAYS-TO-MMI > 0
                               ADD NCM-DAYS-TO-MMI
                                   TO WS-RPT-MMI-ACCUM
                               ADD 1 TO WS-RPT-MMI-COUNT
                           END-IF
                       END-IF
                       ADD NCM-TOTAL-HOURS TO WS-RPT-TOTAL-HOURS
                       ADD NCM-TOTAL-COST TO WS-RPT-TOTAL-COST
                       ADD NCM-SAVINGS-EST TO WS-RPT-TOTAL-SAVINGS

      *                Format detail line
                       MOVE NCM-CLAIM-NUMBER TO WS-RPT-CLAIM
                       MOVE NCM-CLAIMANT-NAME(1:25)
                           TO WS-RPT-NAME
                       EVALUATE TRUE
                           WHEN NCM-STAT-REFERRED
                               MOVE "REFERRED" TO WS-RPT-STATUS
                           WHEN NCM-STAT-ASSIGNED
                               MOVE "ASSIGNED" TO WS-RPT-STATUS
                           WHEN NCM-STAT-ACTIVE
                               MOVE "ACTIVE" TO WS-RPT-STATUS
                           WHEN NCM-STAT-RTW-PLAN
                               MOVE "RTW PLAN" TO WS-RPT-STATUS
                           WHEN NCM-STAT-MONITORING
                               MOVE "MONITOR" TO WS-RPT-STATUS
                           WHEN NCM-STAT-CLOSED
                               MOVE "CLOSED" TO WS-RPT-STATUS
                       END-EVALUATE
                       MOVE NCM-NURSE-NAME(1:20) TO WS-RPT-NURSE
                       MOVE NCM-TOTAL-HOURS TO WS-RPT-HOURS
                       MOVE NCM-TOTAL-COST TO WS-RPT-COST
                       IF NCM-RTW-ACTUAL-DATE > 0
                           MOVE NCM-RTW-ACTUAL-DATE
                               TO WS-RPT-RTW-DT
                       ELSE
                           MOVE "N/A" TO WS-RPT-RTW-DT
                       END-IF
                       WRITE NCM-REPORT-LINE FROM WS-RPT-DETAIL
               END-READ
           END-PERFORM.

       4000-WRITE-TOTALS.
      *    Calculate outcome metrics
           IF WS-RPT-MMI-COUNT > 0
               COMPUTE WS-RPT-AVG-DAYS-MMI =
                   WS-RPT-MMI-ACCUM / WS-RPT-MMI-COUNT
           END-IF
           IF WS-RPT-CLOSED-CASES > 0
               COMPUTE WS-RPT-RTW-RATE =
                   (WS-RPT-RTW-SUCCESS / WS-RPT-CLOSED-CASES) * 100
           END-IF

           MOVE SPACES TO NCM-REPORT-LINE
           WRITE NCM-REPORT-LINE
           MOVE ALL "=" TO NCM-REPORT-LINE
           WRITE NCM-REPORT-LINE
           STRING "TOTAL CASES: " DELIMITED BY SIZE
                  WS-RPT-TOTAL-CASES DELIMITED BY SIZE
                  "  ACTIVE: " DELIMITED BY SIZE
                  WS-RPT-ACTIVE-CASES DELIMITED BY SIZE
                  "  CLOSED: " DELIMITED BY SIZE
                  WS-RPT-CLOSED-CASES DELIMITED BY SIZE
                  INTO NCM-REPORT-LINE
           WRITE NCM-REPORT-LINE
           STRING "RTW SUCCESS RATE: " DELIMITED BY SIZE
                  WS-RPT-RTW-RATE DELIMITED BY SIZE
                  "%  AVG DAYS TO MMI: " DELIMITED BY SIZE
                  WS-RPT-AVG-DAYS-MMI DELIMITED BY SIZE
                  INTO NCM-REPORT-LINE
           WRITE NCM-REPORT-LINE
           STRING "TOTAL NCM HOURS: " DELIMITED BY SIZE
                  WS-RPT-TOTAL-HOURS DELIMITED BY SIZE
                  "  TOTAL COST: $" DELIMITED BY SIZE
                  WS-RPT-TOTAL-COST DELIMITED BY SIZE
                  "  EST SAVINGS: $" DELIMITED BY SIZE
                  WS-RPT-TOTAL-SAVINGS DELIMITED BY SIZE
                  INTO NCM-REPORT-LINE
           WRITE NCM-REPORT-LINE

           CLOSE NCM-REPORT-FILE.

       8000-CALC-DATE-DIFF.
      *    Calculate difference in days between WS-DATE-1 and WS-DATE-2
      *    Result in WS-DATE-DIFF-DAYS (simplified Julian approach)
           COMPUTE WS-DATE-DIFF-DAYS =
               FUNCTION INTEGER-OF-DATE(WS-DATE-2) -
               FUNCTION INTEGER-OF-DATE(WS-DATE-1).

       9000-TERMINATE.
           CLOSE NCM-CASE-FILE
           CLOSE NCM-ACTIVITY-FILE
           STOP RUN.
