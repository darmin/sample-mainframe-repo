      ******************************************************************
      * COPYBOOK:  WCPRVCPY
      * PURPOSE:   Workers' Compensation Provider Record Layouts
      * SYSTEM:    Claims Management System (CMS)
      * PLATFORM:  HPE NonStop / Enscribe Key-Sequenced File
      * FILE:      $DATA1.CLMDB.PRVMSTR (provider master)
      * KEY:       WC-PRV-NPI (primary), alternate key on TIN
      * AUTHOR:    TPA Systems Development
      * CREATED:   2023-12-10
      * MODIFIED:  2025-02-20 - Added network tiers, W-9 tracking
      *
      * Contains the provider master record used for medical bill
      * processing, payment issuance, 1099 reporting, and network
      * management. Providers are identified by NPI (National
      * Provider Identifier) as mandated by HIPAA.
      *
      * Provider types follow CMS taxonomy codes. Network status
      * determines fee schedule application and payment rates.
      ******************************************************************
      *
      *================================================================
      * PROVIDER MASTER RECORD
      * One record per provider NPI. A provider may have multiple
      * practice locations (stored in separate location records).
      * Record length: 768 bytes (fixed)
      *================================================================
      *
       01  WC-PROVIDER-MASTER-RECORD.
      *
      *--- Provider Identification
      *
           05  WC-PRV-NPI                 PIC X(10).
           05  WC-PRV-TIN                 PIC X(9).
           05  WC-PRV-TIN-TYPE            PIC X(1).
               88  WC-PRV-TIN-IS-SSN         VALUE "S".
               88  WC-PRV-TIN-IS-FEIN        VALUE "F".
           05  WC-PRV-LICENSE-NBR          PIC X(15).
           05  WC-PRV-LICENSE-STATE        PIC X(2).
           05  WC-PRV-DEA-NUMBER           PIC X(9).
           05  WC-PRV-MEDICARE-ID          PIC X(12).
      *
      *--- Provider Name
      *
           05  WC-PRV-NAME-INFO.
               10  WC-PRV-LAST-NAME       PIC X(25).
               10  WC-PRV-FIRST-NAME      PIC X(15).
               10  WC-PRV-MIDDLE-INIT     PIC X(1).
               10  WC-PRV-CREDENTIALS     PIC X(8).
               10  WC-PRV-ENTITY-NAME     PIC X(40).
               10  WC-PRV-ENTITY-TYPE     PIC X(1).
                   88  WC-PRV-IS-INDIVIDUAL   VALUE "I".
                   88  WC-PRV-IS-ORGANIZATION VALUE "O".
      *
      *--- Provider Type (CMS Taxonomy-based)
      *
           05  WC-PRV-TYPE-CODE           PIC X(3).
               88  WC-PRV-TYPE-PHYSICIAN      VALUE "PHY".
               88  WC-PRV-TYPE-HOSPITAL       VALUE "HOS".
               88  WC-PRV-TYPE-SURGEON        VALUE "SRG".
               88  WC-PRV-TYPE-ORTHO          VALUE "ORT".
               88  WC-PRV-TYPE-NEURO          VALUE "NRO".
               88  WC-PRV-TYPE-CHIROPRACTOR   VALUE "CHI".
               88  WC-PRV-TYPE-PHYS-THER      VALUE "PTH".
               88  WC-PRV-TYPE-OCC-THER       VALUE "OTH".
               88  WC-PRV-TYPE-PHARMACY       VALUE "PHR".
               88  WC-PRV-TYPE-DME            VALUE "DME".
               88  WC-PRV-TYPE-AMBULANCE      VALUE "AMB".
               88  WC-PRV-TYPE-IMAGING        VALUE "IMG".
               88  WC-PRV-TYPE-LAB            VALUE "LAB".
               88  WC-PRV-TYPE-PSYCHOL        VALUE "PSY".
               88  WC-PRV-TYPE-PAIN-MGMT      VALUE "PMG".
               88  WC-PRV-TYPE-URGENT-CARE    VALUE "UGC".
               88  WC-PRV-TYPE-ER             VALUE "ER ".
               88  WC-PRV-TYPE-ASC            VALUE "ASC".
               88  WC-PRV-TYPE-IME            VALUE "IME".
               88  WC-PRV-TYPE-FCE            VALUE "FCE".
      *
      *--- Specialty (for physicians)
      *
           05  WC-PRV-SPECIALTY-INFO.
               10  WC-PRV-PRIMARY-SPEC    PIC X(3).
               10  WC-PRV-PRIMARY-DESC    PIC X(30).
               10  WC-PRV-SECONDARY-SPEC  PIC X(3).
               10  WC-PRV-BOARD-CERTIFIED PIC X(1).
                   88  WC-PRV-IS-BOARD-CERT   VALUE "Y".
                   88  WC-PRV-NOT-BOARD-CERT  VALUE "N".
      *
      *--- Primary Address
      *
           05  WC-PRV-ADDRESS.
               10  WC-PRV-ADDR-LINE1      PIC X(35).
               10  WC-PRV-ADDR-LINE2      PIC X(35).
               10  WC-PRV-CITY            PIC X(25).
               10  WC-PRV-STATE           PIC X(2).
               10  WC-PRV-ZIP             PIC X(10).
               10  WC-PRV-COUNTY          PIC X(20).
               10  WC-PRV-PHONE           PIC X(10).
               10  WC-PRV-FAX             PIC X(10).
               10  WC-PRV-EMAIL           PIC X(40).
      *
      *--- Network Status
      *
           05  WC-PRV-NETWORK-INFO.
               10  WC-PRV-NETWORK-STATUS  PIC X(2).
                   88  WC-PRV-NET-IN-NETWORK  VALUE "IN".
                   88  WC-PRV-NET-OUT-NETWORK VALUE "OT".
                   88  WC-PRV-NET-COE         VALUE "CE".
                   88  WC-PRV-NET-PREFERRED   VALUE "PF".
                   88  WC-PRV-NET-DEBARRED    VALUE "DB".
               10  WC-PRV-NETWORK-TIER    PIC X(1).
                   88  WC-PRV-TIER-1          VALUE "1".
                   88  WC-PRV-TIER-2          VALUE "2".
                   88  WC-PRV-TIER-3          VALUE "3".
               10  WC-PRV-NETWORK-EFF-DT  PIC X(8).
               10  WC-PRV-NETWORK-EXP-DT  PIC X(8).
               10  WC-PRV-NETWORK-CODE    PIC X(6).
               10  WC-PRV-NETWORK-NAME    PIC X(25).
               10  WC-PRV-CONTRACT-ID     PIC X(12).
      *
      *--- Fee Schedule Assignment
      *
           05  WC-PRV-FEE-SCHEDULE-INFO.
               10  WC-PRV-FEE-SCHED-CODE PIC X(4).
               10  WC-PRV-FEE-SCHED-PCT  PIC 9V9999 COMP-3.
               10  WC-PRV-FEE-SCHED-EFF  PIC X(8).
               10  WC-PRV-FEE-SCHED-TYPE PIC X(2).
                   88  WC-PRV-FEE-STATE       VALUE "ST".
                   88  WC-PRV-FEE-MEDICARE    VALUE "MC".
                   88  WC-PRV-FEE-NETWORK     VALUE "NW".
                   88  WC-PRV-FEE-NEGOTIATED  VALUE "NG".
                   88  WC-PRV-FEE-UCR         VALUE "UC".
               10  WC-PRV-CONVERSION-FACTOR PIC 9(3)V9999 COMP-3.
      *
      *--- Provider Status
      *
           05  WC-PRV-STATUS              PIC X(2).
               88  WC-PRV-STAT-ACTIVE         VALUE "AC".
               88  WC-PRV-STAT-INACTIVE       VALUE "IN".
               88  WC-PRV-STAT-SUSPENDED      VALUE "SU".
               88  WC-PRV-STAT-DEBARRED       VALUE "DB".
               88  WC-PRV-STAT-DECEASED       VALUE "DC".
               88  WC-PRV-STAT-REVIEW         VALUE "RV".
      *
      *--- 1099 Accumulation
      *
           05  WC-PRV-1099-INFO.
               10  WC-PRV-1099-REQUIRED   PIC X(1).
                   88  WC-PRV-1099-YES        VALUE "Y".
                   88  WC-PRV-1099-NO         VALUE "N".
               10  WC-PRV-1099-TYPE       PIC X(4).
                   88  WC-PRV-1099-MISC       VALUE "MISC".
                   88  WC-PRV-1099-NEC        VALUE "NEC ".
               10  WC-PRV-1099-ACCUM OCCURS 4 TIMES.
                   15  WC-PRV-1099-YEAR   PIC X(4).
                   15  WC-PRV-1099-AMOUNT PIC S9(9)V99 COMP.
                   15  WC-PRV-1099-FILED  PIC X(1).
                       88  WC-PRV-1099-FLD-Y  VALUE "Y".
                       88  WC-PRV-1099-FLD-N  VALUE "N".
                   15  WC-PRV-1099-CORR   PIC X(1).
                       88  WC-PRV-1099-CORRECTED VALUE "Y".
                       88  WC-PRV-1099-ORIGINAL  VALUE "N".
      *
      *--- W-9 Status Tracking
      *
           05  WC-PRV-W9-INFO.
               10  WC-PRV-W9-ON-FILE      PIC X(1).
                   88  WC-PRV-W9-RECEIVED     VALUE "Y".
                   88  WC-PRV-W9-NOT-RECD     VALUE "N".
                   88  WC-PRV-W9-EXPIRED      VALUE "E".
               10  WC-PRV-W9-RECEIVED-DT  PIC X(8).
               10  WC-PRV-W9-CERT-NAME    PIC X(40).
               10  WC-PRV-W9-CERT-TIN     PIC X(9).
               10  WC-PRV-W9-TIN-MATCH    PIC X(1).
                   88  WC-PRV-W9-TIN-MATCHED  VALUE "M".
                   88  WC-PRV-W9-TIN-MISMATCH VALUE "X".
                   88  WC-PRV-W9-TIN-PENDING  VALUE "P".
               10  WC-PRV-BACKUP-WITHHOLD PIC X(1).
                   88  WC-PRV-BW-REQUIRED     VALUE "Y".
                   88  WC-PRV-BW-NOT-REQD     VALUE "N".
               10  WC-PRV-BW-START-DATE   PIC X(8).
               10  WC-PRV-BW-RATE         PIC 9V99 COMP-3.
      *
      *--- Payment Statistics
      *
           05  WC-PRV-PAYMENT-STATS.
               10  WC-PRV-TOTAL-CLAIMS    PIC 9(6) COMP.
               10  WC-PRV-TOTAL-BILLS     PIC 9(6) COMP.
               10  WC-PRV-TOTAL-BILLED    PIC S9(11)V99 COMP.
               10  WC-PRV-TOTAL-PAID      PIC S9(11)V99 COMP.
               10  WC-PRV-TOTAL-SAVINGS   PIC S9(11)V99 COMP.
               10  WC-PRV-AVG-DAYS-TO-PAY PIC 9(3) COMP.
               10  WC-PRV-LAST-PMT-DATE   PIC X(8).
      *
      *--- Quality Indicators
      *
           05  WC-PRV-QUALITY-INFO.
               10  WC-PRV-QUALITY-SCORE   PIC 9V99 COMP-3.
               10  WC-PRV-COMPLAINT-COUNT PIC 9(3) COMP.
               10  WC-PRV-OUTCOME-RATING  PIC X(1).
                   88  WC-PRV-OUTCOME-EXCEL   VALUE "A".
                   88  WC-PRV-OUTCOME-GOOD    VALUE "B".
                   88  WC-PRV-OUTCOME-AVG     VALUE "C".
                   88  WC-PRV-OUTCOME-BELOW   VALUE "D".
                   88  WC-PRV-OUTCOME-POOR    VALUE "F".
               10  WC-PRV-LAST-REVIEW-DT  PIC X(8).
      *
      *--- Audit Trail
      *
           05  WC-PRV-AUDIT-INFO.
               10  WC-PRV-CREATED-BY      PIC X(8).
               10  WC-PRV-CREATED-TS      PIC X(26).
               10  WC-PRV-MODIFIED-BY     PIC X(8).
               10  WC-PRV-MODIFIED-TS     PIC X(26).
               10  WC-PRV-RECORD-VERSION  PIC 9(6).
      *
           05  WC-PRV-FILLER             PIC X(20).
