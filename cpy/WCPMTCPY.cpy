      ******************************************************************
      * COPYBOOK:  WCPMTCPY
      * PURPOSE:   Workers' Compensation Payment Record Layouts
      * SYSTEM:    Claims Management System (CMS)
      * PLATFORM:  HPE NonStop / Enscribe Key-Sequenced File
      * FILE:      $DATA1.CLMDB.CLMPMT (master), $DATA1.CLMDB.PMTLIEN
      * KEY:       WC-PMT-PAYMENT-NUMBER (primary)
      *            Alternate key: WC-PMT-CLAIM-NUMBER
      * AUTHOR:    TPA Systems Development
      * CREATED:   2023-11-20
      * MODIFIED:  2025-03-05 - Added EFT/ACH fields, 1099 tracking
      *
      * Contains two record layouts:
      *   1. Payment Master Record (per disbursement)
      *   2. Lien/Garnishment Deduction Record
      ******************************************************************
      *
      *================================================================
      * PAYMENT MASTER RECORD
      * One record per payment issued. Payments may be voided but
      * never deleted (void creates a reversal record).
      * Record length: 768 bytes (fixed)
      *================================================================
      *
       01  WC-PAYMENT-MASTER-RECORD.
      *
      *--- Payment Identification
      *
           05  WC-PMT-PAYMENT-NUMBER      PIC X(15).
           05  WC-PMT-CLAIM-NUMBER        PIC X(12).
           05  WC-PMT-CLAIM-SUFFIX        PIC X(2).
           05  WC-PMT-VOUCHER-NUMBER      PIC X(10).
           05  WC-PMT-BATCH-NUMBER        PIC X(8).
      *
      *--- Payment Type
      *
           05  WC-PMT-TYPE-CODE           PIC X(3).
               88  WC-PMT-TYPE-MEDICAL        VALUE "MED".
               88  WC-PMT-TYPE-IND-TTD        VALUE "TTD".
               88  WC-PMT-TYPE-IND-TPD        VALUE "TPD".
               88  WC-PMT-TYPE-IND-PPD        VALUE "PPD".
               88  WC-PMT-TYPE-IND-PTD        VALUE "PTD".
               88  WC-PMT-TYPE-IND-DEATH      VALUE "DTH".
               88  WC-PMT-TYPE-EXPENSE        VALUE "EXP".
               88  WC-PMT-TYPE-LEGAL          VALUE "LGL".
               88  WC-PMT-TYPE-REHAB          VALUE "RHB".
               88  WC-PMT-TYPE-SETTLEMENT     VALUE "SET".
               88  WC-PMT-TYPE-INDEMNITY      VALUES "TTD" "TPD"
                                                      "PPD" "PTD"
                                                      "DTH".
      *
      *--- Payment Method
      *
           05  WC-PMT-METHOD-CODE         PIC X(3).
               88  WC-PMT-METHOD-CHECK        VALUE "CHK".
               88  WC-PMT-METHOD-EFT          VALUE "EFT".
               88  WC-PMT-METHOD-WIRE         VALUE "WIR".
               88  WC-PMT-METHOD-DRAFT        VALUE "DFT".
      *
      *--- Payment Amounts
      *
           05  WC-PMT-AMOUNTS.
               10  WC-PMT-GROSS-AMOUNT    PIC S9(9)V99 COMP.
               10  WC-PMT-DEDUCTION-TOTAL PIC S9(9)V99 COMP.
               10  WC-PMT-NET-AMOUNT      PIC S9(9)V99 COMP.
               10  WC-PMT-TAX-WITHHOLD    PIC S9(7)V99 COMP.
               10  WC-PMT-LIEN-DEDUCTION  PIC S9(7)V99 COMP.
               10  WC-PMT-CHILD-SUPPORT   PIC S9(7)V99 COMP.
               10  WC-PMT-ATTORNEY-FEE    PIC S9(7)V99 COMP.
      *
      *--- Payment Status
      *
           05  WC-PMT-STATUS-CODE         PIC X(2).
               88  WC-PMT-STAT-PENDING        VALUE "PD".
               88  WC-PMT-STAT-APPROVED       VALUE "AP".
               88  WC-PMT-STAT-ISSUED         VALUE "IS".
               88  WC-PMT-STAT-CLEARED        VALUE "CL".
               88  WC-PMT-STAT-VOIDED         VALUE "VD".
               88  WC-PMT-STAT-STOPPED        VALUE "ST".
               88  WC-PMT-STAT-RETURNED       VALUE "RT".
               88  WC-PMT-STAT-REISSUED       VALUE "RI".
               88  WC-PMT-STAT-ESCHEAT        VALUE "ES".
      *
      *--- Payment Dates
      *
           05  WC-PMT-DATES.
               10  WC-PMT-CHECK-DATE      PIC X(8).
               10  WC-PMT-ISSUE-DATE      PIC X(8).
               10  WC-PMT-CLEAR-DATE      PIC X(8).
               10  WC-PMT-VOID-DATE       PIC X(8).
               10  WC-PMT-STALE-DATE      PIC X(8).
      *
      *--- Indemnity Period (for TTD/TPD/PPD/PTD)
      *
           05  WC-PMT-INDEM-PERIOD.
               10  WC-PMT-PERIOD-FROM     PIC X(8).
               10  WC-PMT-PERIOD-TO       PIC X(8).
               10  WC-PMT-WEEKS-PAID      PIC 9(3)V99 COMP-3.
               10  WC-PMT-DAYS-PAID       PIC 9(4) COMP.
               10  WC-PMT-WEEKLY-RATE     PIC S9(7)V99 COMP.
               10  WC-PMT-WAITING-PD-SAT  PIC X(1).
                   88  WC-PMT-WAIT-SATISFIED  VALUE "Y".
                   88  WC-PMT-WAIT-NOT-SAT    VALUE "N".
      *
      *--- Payee Information
      *
           05  WC-PMT-PAYEE-INFO.
               10  WC-PMT-PAYEE-TYPE      PIC X(2).
                   88  WC-PMT-PAYEE-CLAIMANT  VALUE "CL".
                   88  WC-PMT-PAYEE-PROVIDER  VALUE "PR".
                   88  WC-PMT-PAYEE-ATTORNEY  VALUE "AT".
                   88  WC-PMT-PAYEE-VENDOR    VALUE "VN".
                   88  WC-PMT-PAYEE-EMPLOYER  VALUE "EM".
               10  WC-PMT-PAYEE-ID        PIC X(12).
               10  WC-PMT-PAYEE-NAME      PIC X(35).
               10  WC-PMT-PAYEE-TIN       PIC X(9).
               10  WC-PMT-PAYEE-TIN-TYPE  PIC X(1).
                   88  WC-PMT-TIN-SSN         VALUE "S".
                   88  WC-PMT-TIN-FEIN        VALUE "F".
               10  WC-PMT-PAYEE-ADDR.
                   15  WC-PMT-ADDR-LINE1  PIC X(30).
                   15  WC-PMT-ADDR-LINE2  PIC X(30).
                   15  WC-PMT-ADDR-CITY   PIC X(20).
                   15  WC-PMT-ADDR-STATE  PIC X(2).
                   15  WC-PMT-ADDR-ZIP    PIC X(10).
      *
      *--- Check/EFT Details
      *
           05  WC-PMT-CHECK-INFO.
               10  WC-PMT-CHECK-NUMBER    PIC X(10).
               10  WC-PMT-BANK-ACCOUNT    PIC X(4).
      *
           05  WC-PMT-EFT-INFO.
               10  WC-PMT-EFT-ROUTING     PIC X(9).
               10  WC-PMT-EFT-ACCOUNT-NBR PIC X(17).
               10  WC-PMT-EFT-ACCT-TYPE   PIC X(1).
                   88  WC-PMT-EFT-CHECKING    VALUE "C".
                   88  WC-PMT-EFT-SAVINGS     VALUE "S".
               10  WC-PMT-EFT-TRACE-NBR   PIC X(15).
               10  WC-PMT-EFT-STATUS      PIC X(2).
                   88  WC-PMT-EFT-SUBMITTED   VALUE "SB".
                   88  WC-PMT-EFT-ACCEPTED    VALUE "AC".
                   88  WC-PMT-EFT-REJECTED    VALUE "RJ".
                   88  WC-PMT-EFT-RETURNED    VALUE "RT".
      *
      *--- Medical Payment Details
      *
           05  WC-PMT-MEDICAL-INFO.
               10  WC-PMT-PROVIDER-NPI    PIC X(10).
               10  WC-PMT-BILL-NUMBER     PIC X(15).
               10  WC-PMT-AUTH-NUMBER     PIC X(15).
               10  WC-PMT-TOTAL-BILLED    PIC S9(7)V99 COMP.
               10  WC-PMT-TOTAL-ALLOWED   PIC S9(7)V99 COMP.
               10  WC-PMT-FEE-SCHED-CODE  PIC X(4).
               10  WC-PMT-REVIEW-TYPE     PIC X(2).
                   88  WC-PMT-RVW-AUTO        VALUE "AU".
                   88  WC-PMT-RVW-MANUAL      VALUE "MN".
                   88  WC-PMT-RVW-NETWORK     VALUE "NW".
      *
      *--- 1099 Reporting
      *
           05  WC-PMT-1099-INFO.
               10  WC-PMT-1099-REPORTABLE PIC X(1).
                   88  WC-PMT-1099-YES        VALUE "Y".
                   88  WC-PMT-1099-NO         VALUE "N".
               10  WC-PMT-1099-TYPE       PIC X(4).
                   88  WC-PMT-1099-MISC       VALUE "MISC".
                   88  WC-PMT-1099-NEC        VALUE "NEC ".
               10  WC-PMT-1099-BOX        PIC X(2).
               10  WC-PMT-1099-YEAR       PIC X(4).
               10  WC-PMT-1099-AMOUNT-YTD PIC S9(9)V99 COMP.
               10  WC-PMT-1099-FILED      PIC X(1).
                   88  WC-PMT-1099-FILED-YES  VALUE "Y".
                   88  WC-PMT-1099-FILED-NO   VALUE "N".
               10  WC-PMT-W9-ON-FILE      PIC X(1).
                   88  WC-PMT-W9-YES          VALUE "Y".
                   88  WC-PMT-W9-NO           VALUE "N".
               10  WC-PMT-BACKUP-WITHHOLD PIC X(1).
                   88  WC-PMT-BKUP-YES        VALUE "Y".
                   88  WC-PMT-BKUP-NO         VALUE "N".
      *
      *--- Reserve Impact
      *
           05  WC-PMT-RESERVE-INFO.
               10  WC-PMT-RSV-CATEGORY    PIC X(2).
               10  WC-PMT-RSV-BEFORE      PIC S9(9)V99 COMP.
               10  WC-PMT-RSV-AFTER       PIC S9(9)V99 COMP.
               10  WC-PMT-RSV-ADEQUATE    PIC X(1).
                   88  WC-PMT-RSV-WAS-ADEQ   VALUE "Y".
                   88  WC-PMT-RSV-EXCEEDED    VALUE "N".
      *
      *--- Duplicate Detection
      *
           05  WC-PMT-DUP-CHECK-INFO.
               10  WC-PMT-DUP-FLAG        PIC X(1).
                   88  WC-PMT-IS-DUPLICATE    VALUE "Y".
                   88  WC-PMT-NOT-DUPLICATE   VALUE "N".
               10  WC-PMT-DUP-OVERRIDE-BY PIC X(8).
               10  WC-PMT-DUP-REASON      PIC X(30).
      *
      *--- Audit Trail
      *
           05  WC-PMT-AUDIT-INFO.
               10  WC-PMT-CREATED-BY      PIC X(8).
               10  WC-PMT-CREATED-TS      PIC X(26).
               10  WC-PMT-APPROVED-BY     PIC X(8).
               10  WC-PMT-APPROVED-TS     PIC X(26).
               10  WC-PMT-MODIFIED-BY     PIC X(8).
               10  WC-PMT-MODIFIED-TS     PIC X(26).
               10  WC-PMT-RECORD-VERSION  PIC 9(6).
      *
           05  WC-PMT-FILLER             PIC X(20).
      *
      *================================================================
      * LIEN/GARNISHMENT DEDUCTION RECORD
      * Tracks court-ordered deductions from indemnity payments.
      * Multiple liens may exist per claim (child support, tax levy,
      * medical liens). Priority order per state law.
      * File: $DATA1.CLMDB.PMTLIEN
      * Record length: 256 bytes (fixed)
      *================================================================
      *
       01  WC-LIEN-DEDUCTION-RECORD.
      *
           05  WC-LIEN-CLAIM-NUMBER       PIC X(12).
           05  WC-LIEN-SEQUENCE-NBR       PIC 9(4).
      *
           05  WC-LIEN-TYPE-CODE          PIC X(2).
               88  WC-LIEN-CHILD-SUPPORT      VALUE "CS".
               88  WC-LIEN-TAX-LEVY-FED       VALUE "TF".
               88  WC-LIEN-TAX-LEVY-STATE     VALUE "TS".
               88  WC-LIEN-MEDICAL-LIEN       VALUE "ML".
               88  WC-LIEN-ATTORNEY-LIEN      VALUE "AL".
               88  WC-LIEN-HOSPITAL-LIEN      VALUE "HL".
               88  WC-LIEN-SUBROGATION        VALUE "SB".
      *
           05  WC-LIEN-PRIORITY           PIC 9(2).
      *
           05  WC-LIEN-STATUS             PIC X(2).
               88  WC-LIEN-ACTIVE             VALUE "AC".
               88  WC-LIEN-SATISFIED          VALUE "SA".
               88  WC-LIEN-DISPUTED           VALUE "DS".
               88  WC-LIEN-RELEASED           VALUE "RL".
      *
           05  WC-LIEN-AMOUNTS.
               10  WC-LIEN-TOTAL-AMOUNT   PIC S9(9)V99 COMP.
               10  WC-LIEN-DEDUCTED-YTD   PIC S9(9)V99 COMP.
               10  WC-LIEN-REMAINING      PIC S9(9)V99 COMP.
               10  WC-LIEN-DEDUCT-PERCENT PIC 9V9999 COMP-3.
               10  WC-LIEN-DEDUCT-FIXED   PIC S9(7)V99 COMP.
               10  WC-LIEN-MAX-PER-PERIOD PIC S9(7)V99 COMP.
      *
           05  WC-LIEN-HOLDER-INFO.
               10  WC-LIEN-HOLDER-NAME    PIC X(35).
               10  WC-LIEN-HOLDER-ID      PIC X(12).
               10  WC-LIEN-COURT-ORDER    PIC X(20).
               10  WC-LIEN-EFFECTIVE-DATE PIC X(8).
               10  WC-LIEN-EXPIRY-DATE    PIC X(8).
      *
           05  WC-LIEN-AUDIT.
               10  WC-LIEN-CREATED-BY     PIC X(8).
               10  WC-LIEN-CREATED-TS     PIC X(26).
               10  WC-LIEN-MODIFIED-BY    PIC X(8).
               10  WC-LIEN-MODIFIED-TS    PIC X(26).
      *
           05  WC-LIEN-FILLER            PIC X(16).
