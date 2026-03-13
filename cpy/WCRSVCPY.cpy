      ******************************************************************
      * COPYBOOK:  WCRSVCPY
      * PURPOSE:   Workers' Compensation Reserve Record Layouts
      * SYSTEM:    Claims Management System (CMS)
      * PLATFORM:  HPE NonStop / Enscribe Key-Sequenced File
      * FILE:      $DATA1.CLMDB.CLMRSV (master), $DATA1.CLMDB.RSVTRAN
      * KEY:       WC-RSV-CLAIM-NUMBER + WC-RSV-CATEGORY (compound)
      * AUTHOR:    TPA Systems Development
      * CREATED:   2023-11-15
      * MODIFIED:  2025-01-22 - Added S&S category, authority matrix
      *
      * Contains three record layouts:
      *   1. Reserve Master Record (per claim/category)
      *   2. Reserve Transaction Record (change audit trail)
      *   3. Reserve Review Schedule Record (compliance tracking)
      ******************************************************************
      *
      *================================================================
      * RESERVE MASTER RECORD
      * One record per claim per reserve category.
      * File: $DATA1.CLMDB.CLMRSV
      * Record length: 256 bytes (fixed)
      *================================================================
      *
       01  WC-RESERVE-MASTER-RECORD.
      *
      *--- Key Fields
      *
           05  WC-RSV-CLAIM-NUMBER        PIC X(12).
           05  WC-RSV-CATEGORY            PIC X(2).
               88  WC-RSV-CAT-MEDICAL         VALUE "MD".
               88  WC-RSV-CAT-TTD             VALUE "TD".
               88  WC-RSV-CAT-TPD             VALUE "TP".
               88  WC-RSV-CAT-PPD             VALUE "PD".
               88  WC-RSV-CAT-PTD             VALUE "PT".
               88  WC-RSV-CAT-EXPENSE         VALUE "EX".
               88  WC-RSV-CAT-LEGAL           VALUE "LG".
               88  WC-RSV-CAT-SUBROG          VALUE "SS".
               88  WC-RSV-CAT-INDEMNITY       VALUES "TD" "TP"
                                                      "PD" "PT".
      *
      *--- Reserve Amounts (all S9(9)V99 COMP for TMF integrity)
      *
           05  WC-RSV-AMOUNTS.
               10  WC-RSV-OUTSTANDING        PIC S9(9)V99 COMP.
               10  WC-RSV-PAID-TO-DATE       PIC S9(9)V99 COMP.
               10  WC-RSV-INCURRED           PIC S9(9)V99 COMP.
               10  WC-RSV-INITIAL-RESERVE    PIC S9(9)V99 COMP.
               10  WC-RSV-RECOVERY-AMOUNT    PIC S9(9)V99 COMP.
               10  WC-RSV-NET-INCURRED       PIC S9(9)V99 COMP.
      *
      *--- Reserve Status
      *
           05  WC-RSV-STATUS              PIC X(2).
               88  WC-RSV-STAT-ACTIVE         VALUE "AC".
               88  WC-RSV-STAT-CLOSED         VALUE "CL".
               88  WC-RSV-STAT-PENDING-APPR   VALUE "PA".
               88  WC-RSV-STAT-FROZEN         VALUE "FZ".
      *
      *--- Authority Information
      *
           05  WC-RSV-AUTHORITY-INFO.
               10  WC-RSV-AUTH-LEVEL      PIC X(2).
                   88  WC-RSV-AUTH-ADJUSTER   VALUE "01".
                   88  WC-RSV-AUTH-SENIOR     VALUE "02".
                   88  WC-RSV-AUTH-SUPVSR     VALUE "03".
                   88  WC-RSV-AUTH-MANAGER    VALUE "04".
                   88  WC-RSV-AUTH-DIRECTOR   VALUE "05".
                   88  WC-RSV-AUTH-VP         VALUE "06".
               10  WC-RSV-AUTH-LIMIT      PIC S9(9)V99 COMP.
               10  WC-RSV-CURR-AUTHORITY  PIC X(8).
               10  WC-RSV-APPR-AUTHORITY  PIC X(8).
               10  WC-RSV-APPR-DATE       PIC X(8).
      *
      *--- Tracking Dates
      *
           05  WC-RSV-DATES.
               10  WC-RSV-DATE-OPENED     PIC X(8).
               10  WC-RSV-DATE-CLOSED     PIC X(8).
               10  WC-RSV-LAST-CHANGED    PIC X(8).
               10  WC-RSV-LAST-REVIEWED   PIC X(8).
               10  WC-RSV-NEXT-REVIEW     PIC X(8).
      *
      *--- Change Counters
      *
           05  WC-RSV-CHANGE-COUNT        PIC 9(4).
           05  WC-RSV-LAST-CHANGED-BY     PIC X(8).
      *
      *--- Audit
      *
           05  WC-RSV-CREATED-BY          PIC X(8).
           05  WC-RSV-CREATED-TIMESTAMP   PIC X(26).
           05  WC-RSV-MODIFIED-BY         PIC X(8).
           05  WC-RSV-MODIFIED-TIMESTAMP  PIC X(26).
           05  WC-RSV-RECORD-VERSION      PIC 9(6).
      *
           05  WC-RSV-FILLER             PIC X(24).
      *
      *================================================================
      * RESERVE TRANSACTION RECORD
      * Immutable audit trail of every reserve change. One record
      * per change per category. Used for reserve history display,
      * actuarial triangle reporting, and regulatory audits.
      * File: $DATA1.CLMDB.RSVTRAN
      * Record length: 512 bytes (fixed)
      *================================================================
      *
       01  WC-RESERVE-TRANSACTION-RECORD.
      *
      *--- Key Fields
      *
           05  WC-RSVT-CLAIM-NUMBER       PIC X(12).
           05  WC-RSVT-CATEGORY           PIC X(2).
           05  WC-RSVT-SEQUENCE-NBR       PIC 9(6).
      *
      *--- Before/After Snapshot
      *
           05  WC-RSVT-BEFORE-AMOUNTS.
               10  WC-RSVT-BEF-OUTSTANDING   PIC S9(9)V99 COMP.
               10  WC-RSVT-BEF-PAID          PIC S9(9)V99 COMP.
               10  WC-RSVT-BEF-INCURRED      PIC S9(9)V99 COMP.
      *
           05  WC-RSVT-AFTER-AMOUNTS.
               10  WC-RSVT-AFT-OUTSTANDING   PIC S9(9)V99 COMP.
               10  WC-RSVT-AFT-PAID          PIC S9(9)V99 COMP.
               10  WC-RSVT-AFT-INCURRED      PIC S9(9)V99 COMP.
      *
           05  WC-RSVT-CHANGE-AMOUNT      PIC S9(9)V99 COMP.
      *
      *--- Reason Information
      *
           05  WC-RSVT-REASON-CODE        PIC X(3).
               88  WC-RSVT-RSN-INITIAL        VALUE "INI".
               88  WC-RSVT-RSN-MEDICAL-UPD    VALUE "MED".
               88  WC-RSVT-RSN-NEW-INFO       VALUE "NEW".
               88  WC-RSVT-RSN-LITIGATION     VALUE "LIT".
               88  WC-RSVT-RSN-SETTLEMENT     VALUE "SET".
               88  WC-RSVT-RSN-PAYMENT-ADJ    VALUE "PAY".
               88  WC-RSVT-RSN-MMI-REACHED    VALUE "MMI".
               88  WC-RSVT-RSN-RTW            VALUE "RTW".
               88  WC-RSVT-RSN-REOPEN         VALUE "REO".
               88  WC-RSVT-RSN-CLOSURE        VALUE "CLO".
               88  WC-RSVT-RSN-SUBROG-RECOV   VALUE "SUB".
               88  WC-RSVT-RSN-JURISDICTION    VALUE "JUR".
               88  WC-RSVT-RSN-ACTUARY-ADJ    VALUE "ACT".
               88  WC-RSVT-RSN-MGMT-OVERRIDE  VALUE "MGT".
      *
           05  WC-RSVT-NARRATIVE          PIC X(180).
      *
      *--- Change Source
      *
           05  WC-RSVT-SOURCE-CODE        PIC X(2).
               88  WC-RSVT-SRC-MANUAL         VALUE "MN".
               88  WC-RSVT-SRC-SYSTEM         VALUE "SY".
               88  WC-RSVT-SRC-BATCH          VALUE "BT".
               88  WC-RSVT-SRC-IMPORT         VALUE "IM".
      *
      *--- User and Approval
      *
           05  WC-RSVT-CHANGED-BY         PIC X(8).
           05  WC-RSVT-CHANGED-TIMESTAMP  PIC X(26).
           05  WC-RSVT-APPROVAL-REQUIRED  PIC X(1).
               88  WC-RSVT-NEEDS-APPROVAL     VALUE "Y".
               88  WC-RSVT-NO-APPROVAL        VALUE "N".
           05  WC-RSVT-APPROVED-BY        PIC X(8).
           05  WC-RSVT-APPROVED-TIMESTAMP PIC X(26).
           05  WC-RSVT-APPROVAL-STATUS    PIC X(2).
               88  WC-RSVT-APPR-PENDING       VALUE "PD".
               88  WC-RSVT-APPR-APPROVED      VALUE "AP".
               88  WC-RSVT-APPR-REJECTED      VALUE "RJ".
               88  WC-RSVT-APPR-NOT-REQD      VALUE "NR".
      *
      *--- Authority at Time of Change
      *
           05  WC-RSVT-AUTH-LEVEL-REQD    PIC X(2).
           05  WC-RSVT-AUTH-LEVEL-USED    PIC X(2).
      *
           05  WC-RSVT-FILLER            PIC X(40).
      *
      *================================================================
      * RESERVE AUTHORITY LEVEL MATRIX
      * Defines authority limits by user level and reserve category.
      * Used by the reserve change validation logic.
      *================================================================
      *
       01  WC-AUTHORITY-LEVEL-MATRIX.
      *
           05  WC-AUTH-LEVELS OCCURS 6 TIMES.
               10  WC-AUTH-LVL-CODE       PIC X(2).
               10  WC-AUTH-LVL-DESC       PIC X(15).
               10  WC-AUTH-LVL-LIMITS.
                   15  WC-AUTH-LMT-MEDICAL    PIC S9(9)V99 COMP.
                   15  WC-AUTH-LMT-INDEMNITY  PIC S9(9)V99 COMP.
                   15  WC-AUTH-LMT-EXPENSE    PIC S9(9)V99 COMP.
                   15  WC-AUTH-LMT-LEGAL      PIC S9(9)V99 COMP.
                   15  WC-AUTH-LMT-TOTAL      PIC S9(9)V99 COMP.
      *
      *================================================================
      * RESERVE REVIEW SCHEDULE RECORD
      * Tracks compliance with reserve adequacy review requirements.
      * Most TPAs require reviews at 30, 60, 90 days and quarterly.
      * File: $DATA1.CLMDB.RSVREVW
      * Record length: 128 bytes (fixed)
      *================================================================
      *
       01  WC-RESERVE-REVIEW-RECORD.
      *
           05  WC-REVW-CLAIM-NUMBER       PIC X(12).
           05  WC-REVW-SEQUENCE-NBR       PIC 9(4).
           05  WC-REVW-SCHEDULED-DATE     PIC X(8).
           05  WC-REVW-ACTUAL-DATE        PIC X(8).
           05  WC-REVW-REVIEWER-ID        PIC X(8).
           05  WC-REVW-REVIEW-TYPE        PIC X(2).
               88  WC-REVW-TYPE-30-DAY        VALUE "30".
               88  WC-REVW-TYPE-60-DAY        VALUE "60".
               88  WC-REVW-TYPE-90-DAY        VALUE "90".
               88  WC-REVW-TYPE-QUARTERLY     VALUE "QT".
               88  WC-REVW-TYPE-ANNUAL        VALUE "AN".
               88  WC-REVW-TYPE-ADHOC         VALUE "AH".
               88  WC-REVW-TYPE-SUPERVISOR    VALUE "SV".
               88  WC-REVW-TYPE-AUDIT         VALUE "AU".
           05  WC-REVW-STATUS             PIC X(2).
               88  WC-REVW-SCHEDULED          VALUE "SC".
               88  WC-REVW-COMPLETED          VALUE "CM".
               88  WC-REVW-OVERDUE            VALUE "OD".
               88  WC-REVW-SKIPPED            VALUE "SK".
           05  WC-REVW-OUTCOME            PIC X(2).
               88  WC-REVW-ADEQUATE           VALUE "AD".
               88  WC-REVW-INCREASED          VALUE "IN".
               88  WC-REVW-DECREASED          VALUE "DE".
               88  WC-REVW-UNCHANGED          VALUE "UC".
           05  WC-REVW-TOTAL-INCURRED     PIC S9(9)V99 COMP.
           05  WC-REVW-NOTES              PIC X(60).
           05  WC-REVW-FILLER             PIC X(10).
