class ZCL_SIMPLE_ACC_POST definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF TY_ACCPOST_DOC,
            ICON_STS  TYPE ICON_D,       " Status ICON
            ID_ACCDOC TYPE C LENGTH 255, " Financial document ID
            LOG_NO    TYPE N LENGTH 10,  " Internal LOG number. We will use it to link document header with results table.
            STATUS TYPE STRING,          " Last status of ID_ACCDOC
            AWTYP  TYPE AWTYP,           " Reference Transaction
            AWKEY  TYPE AWKEY,           " Reference Key
            AWSYS  TYPE AWSYS,           " Logical system of source document
            BUKRS  TYPE BUKRS,           " Company Code
            BELNR  TYPE BELNR_D,         " Accounting document number
            GJAHR  TYPE GJAHR,           " Fiscal year
          END OF TY_ACCPOST_DOC .
  types:
    TT_ACCPOST type STANDARD TABLE OF ZFIS_SIMPLE_ACCPOST .
  types:
    TT_ACCPOST_DOC type STANDARD TABLE OF TY_ACCPOST_DOC .
  types TT_ACCPOST_MSG type TT_BAPIRET2 .

  data R_MSG_LOG type ref to IF_RECA_MESSAGE_LIST .
  data T_ACCPOST type TT_ACCPOST .
  data T_ACCPOST_DOC type TT_ACCPOST_DOC .
  data T_ACCPOST_MSG type TT_ACCPOST_MSG .
  constants C_STATUS_NEW type STRING value 'NEW'. "#EC NOTEXT
  constants C_STATUS_OK type STRING value 'OK'. "#EC NOTEXT
  constants C_STATUS_WARNING type STRING value 'WARNING'. "#EC NOTEXT
  constants C_STATUS_ERROR type STRING value 'ERROR'. "#EC NOTEXT

  methods DISPLAY_RESULT .
  methods DISPLAY_LOG .
  type-pools ABAP .
  methods CONSTRUCTOR
    importing
      !IR_MSG_LOG type ref to IF_RECA_MESSAGE_LIST optional
      !IV_CHECK type ABAP_BOOL default ABAP_FALSE .
  methods POST
    importing
      !IV_CHECK type ABAP_BOOL default ABAP_FALSE
      !IV_CLIPBOARD_DATA type ABAP_BOOL default ABAP_FALSE .
protected section.

  data:
    T_ACCOUNTGL type STANDARD TABLE OF BAPIACGL09 .
  data:
    T_ACCOUNTRECEIVABLE type STANDARD TABLE OF BAPIACAR09 .
  data:
    T_ACCOUNTPAYABLE type STANDARD TABLE OF BAPIACAP09 .
  data:
    T_ACCOUNTTAX type STANDARD TABLE OF BAPIACTX09 .
  data:
    T_CURRENCYAMOUNT type STANDARD TABLE OF BAPIACCR09 .
  data:
    T_CRITERIA type STANDARD TABLE OF BAPIACKEC9 .
  data:
    T_VALUEFIELD type STANDARD TABLE OF BAPIACKEV9 .
  data:
    T_EXTENSION1 type STANDARD TABLE OF BAPIACEXTC .
  data:
    T_PAYMENTCARD type STANDARD TABLE OF BAPIACPC09 .
  data:
    T_CONTRACTITEM type STANDARD TABLE OF BAPIACCAIT .
  data:
    T_EXTENSION2 type STANDARD TABLE OF BAPIPAREX .
  data:
    T_REALESTATE type STANDARD TABLE OF BAPIACRE09 .
  data:
    T_ACCOUNTWT type STANDARD TABLE OF BAPIACWT09 .
  data S_DOCUMENTHEADER type BAPIACHE09 .
  data S_CUSTOMERCPD type BAPIACPA09 .
  data N_ITEMNO_ACC type POSNR_ACC .
  data N_ACCPOST_TABIX type SY-TABIX .
  data T_RETURN type BAPIRET2_T .

  methods FILL_CUSTOMERCPD
    importing
      !IS_ACCPOST type ZFIS_SIMPLE_ACCPOST .
  methods CALL_BAPI
    importing
      !IV_CHECK type ABAP_BOOL default ABAP_FALSE
    changing
      !CS_ACCPOST_DOC type TY_ACCPOST_DOC .
  methods FILL_ACCOUNTGL
    importing
      !IS_ACCPOST type ZFIS_SIMPLE_ACCPOST .
  methods FILL_ACCOUNTPAYABLE
    importing
      !IS_ACCPOST type ZFIS_SIMPLE_ACCPOST .
  methods FILL_ACCOUNTRECEIVABLE
    importing
      !IS_ACCPOST type ZFIS_SIMPLE_ACCPOST .
  methods FILL_ACCOUNTTAX
    importing
      !IS_ACCPOST type ZFIS_SIMPLE_ACCPOST .
  methods FILL_DOCUMENTHEADER
    changing
      !CS_ACCPOST_DOC type TY_ACCPOST_DOC .
  methods FILL_BAPI
    changing
      !CS_ACCPOST_DOC type TY_ACCPOST_DOC .
  methods FILL_T_ACCPOST_WITH_CLIPDATA .
  methods READ_CLIPBOARD
    returning
      value(RV_CLIPDATA) type STRING .
private section.

  constants C_SLG0_OBJECT type STRING value 'RECA'. "#EC NOTEXT
  constants C_SLG0_SUBOBJECT type STRING value 'MISC'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_SIMPLE_ACC_POST IMPLEMENTATION.


METHOD CALL_BAPI.
  DATA: LS_RETURN   TYPE BAPIRET2,
        LS_ACCPOST  TYPE ZFIS_SIMPLE_ACCPOST,
        LV_OBJ_TYPE TYPE BAPIACHE02-OBJ_TYPE,
        LV_OBJ_KEY  TYPE BAPIACHE02-OBJ_KEY,
        LV_OBJ_SYS  TYPE BAPIACHE02-OBJ_SYS.

  " Initialize
  CLEAR ME->T_RETURN.

  " Check or Post
  IF IV_CHECK = ABAP_TRUE.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        DOCUMENTHEADER    = ME->S_DOCUMENTHEADER
        CUSTOMERCPD       = ME->S_CUSTOMERCPD
*       CONTRACTHEADER    =
      TABLES
        ACCOUNTGL         = ME->T_ACCOUNTGL
        ACCOUNTRECEIVABLE = ME->T_ACCOUNTRECEIVABLE
        ACCOUNTPAYABLE    = ME->T_ACCOUNTPAYABLE
        ACCOUNTTAX        = ME->T_ACCOUNTTAX
        CURRENCYAMOUNT    = ME->T_CURRENCYAMOUNT
*       CRITERIA          =
*       VALUEFIELD        =
        EXTENSION1        = ME->T_EXTENSION1
        RETURN            = ME->T_RETURN
*       PAYMENTCARD       =
*       CONTRACTITEM      =
*       EXTENSION2        =
*       REALESTATE        =
*       ACCOUNTWT         =
      .

  ELSE.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        DOCUMENTHEADER    = ME->S_DOCUMENTHEADER
        CUSTOMERCPD       = ME->S_CUSTOMERCPD
*       CONTRACTHEADER    =
      IMPORTING
        OBJ_TYPE          = LV_OBJ_TYPE
        OBJ_KEY           = LV_OBJ_KEY
        OBJ_SYS           = LV_OBJ_SYS
      TABLES
        ACCOUNTGL         = ME->T_ACCOUNTGL
        ACCOUNTRECEIVABLE = ME->T_ACCOUNTRECEIVABLE
        ACCOUNTPAYABLE    = ME->T_ACCOUNTPAYABLE
        ACCOUNTTAX        = ME->T_ACCOUNTTAX
        CURRENCYAMOUNT    = ME->T_CURRENCYAMOUNT
*       CRITERIA          =
*       VALUEFIELD        =
        EXTENSION1        = ME->T_EXTENSION1
        RETURN            = ME->T_RETURN
*       PAYMENTCARD       =
*       CONTRACTITEM      =
*       EXTENSION2        =
*       REALESTATE        =
*       ACCOUNTWT         =
      .

  ENDIF.

  " Update status of accounting document
  CS_ACCPOST_DOC-STATUS = ZCL_SIMPLE_ACC_POST=>C_STATUS_OK.
  LOOP AT ME->T_RETURN INTO LS_RETURN.
    CASE LS_RETURN-TYPE.
      WHEN 'W'.
        CS_ACCPOST_DOC-STATUS = ZCL_SIMPLE_ACC_POST=>C_STATUS_WARNING.
      WHEN 'E'.
        CS_ACCPOST_DOC-STATUS = ZCL_SIMPLE_ACC_POST=>C_STATUS_ERROR.
        EXIT.
      WHEN 'A'.
        CS_ACCPOST_DOC-STATUS = ZCL_SIMPLE_ACC_POST=>C_STATUS_ERROR.
        EXIT.
    ENDCASE.
  ENDLOOP.

  " Fill document reference and commit data
  IF IV_CHECK = ABAP_FALSE AND CS_ACCPOST_DOC-STATUS <> ZCL_SIMPLE_ACC_POST=>C_STATUS_ERROR.

    " Document created
    CS_ACCPOST_DOC-BUKRS = LV_OBJ_KEY+10(4).
    CS_ACCPOST_DOC-BELNR = LV_OBJ_KEY(10).
    CS_ACCPOST_DOC-GJAHR = LV_OBJ_KEY+14(4).
    CS_ACCPOST_DOC-AWTYP = LV_OBJ_TYPE.
    CS_ACCPOST_DOC-AWKEY = LV_OBJ_KEY.
    CS_ACCPOST_DOC-AWSYS = LV_OBJ_SYS.

    " Update main table
    LOOP AT ME->T_ACCPOST INTO LS_ACCPOST WHERE ID_ACCDOC = CS_ACCPOST_DOC-ID_ACCDOC.
      LS_ACCPOST-AWTYP = CS_ACCPOST_DOC-AWTYP.
      LS_ACCPOST-AWKEY = CS_ACCPOST_DOC-AWKEY.
      LS_ACCPOST-AWSYS = CS_ACCPOST_DOC-AWSYS.
      MODIFY ME->T_ACCPOST FROM LS_ACCPOST.
    ENDLOOP.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  ENDIF.

  " Link results with document header.
  LOOP AT ME->T_RETURN INTO LS_RETURN.
    LS_RETURN-LOG_NO = CS_ACCPOST_DOC-LOG_NO.
    MODIFY ME->T_RETURN FROM LS_RETURN.
  ENDLOOP.

  " Append results to message collector
  ME->R_MSG_LOG->ADD_FROM_BAPI( IT_BAPIRET = ME->T_RETURN ).

  " Append results to auxiliary results table.
  INSERT LINES OF ME->T_RETURN INTO TABLE ME->T_ACCPOST_MSG.

ENDMETHOD.


METHOD CONSTRUCTOR.

  IF IR_MSG_LOG IS INITIAL.

    " Create a new message collector
    ME->R_MSG_LOG = CF_RECA_MESSAGE_LIST=>CREATE(
        ID_OBJECT       = |{ ZCL_SIMPLE_ACC_POST=>C_SLG0_OBJECT }|
        ID_SUBOBJECT    = |{ ZCL_SIMPLE_ACC_POST=>C_SLG0_SUBOBJECT }|
*        ID_EXTNUMBER    =
*        ID_DELDATE      = RECA0_DATE-MIN
*        IF_DELNOTBEFORE = ABAP_FALSE
    ).

  ELSE.
    ME->R_MSG_LOG = IR_MSG_LOG.
  ENDIF.

ENDMETHOD.


METHOD DISPLAY_LOG.
  DATA: LV_BALLOGHNDL TYPE BALLOGHNDL,
        LT_BAL_T_LOGH TYPE BAL_T_LOGH.


  " Check if message collector is empty
  IF ME->R_MSG_LOG->IS_EMPTY( ) = ABAP_TRUE.
    RETURN.
  ENDIF.

  " Add handle of messages collected to a table
  LV_BALLOGHNDL = ME->R_MSG_LOG->GET_HANDLE( ).
  APPEND LV_BALLOGHNDL TO LT_BAL_T_LOGH.

  " Show messages
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      I_T_LOG_HANDLE       = LT_BAL_T_LOGH    " Restrict display by log handle
    EXCEPTIONS
      PROFILE_INCONSISTENT = 1
      INTERNAL_ERROR       = 2
      NO_DATA_AVAILABLE    = 3
      NO_AUTHORITY         = 4
      OTHERS               = 5.

  IF SY-SUBRC <> 0.
    RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
      EXPORTING
        TEXTID = ZCX_SIMPLE_ACC_POST=>ERROR
        ATTR1  = 'Error displaying message collector. '
        ATTR2  = |{ SY-SUBRC }|.
  ENDIF.

ENDMETHOD.


METHOD DISPLAY_RESULT.
  DATA: LO_RESULT TYPE REF TO LCL_RESULT.

  CREATE OBJECT LO_RESULT
    EXPORTING
      IT_ACCPOST_DOC = ME->T_ACCPOST_DOC
      IT_ACCPOST     = ME->T_ACCPOST
      IT_ACCPOST_MSG = ME->T_ACCPOST_MSG.

  LO_RESULT->DISPLAY( ).
ENDMETHOD.


METHOD FILL_ACCOUNTGL.
  DATA: LS_ACCOUNTGL TYPE BAPIACGL09,
        LS_CURRENCYAMOUNT TYPE BAPIACCR09.


  CLEAR LS_ACCOUNTGL.

  LS_ACCOUNTGL-ITEMNO_ACC       = ME->N_ITEMNO_ACC.    " Accounting Document Line Item Number
  LS_ACCOUNTGL-GL_ACCOUNT       = IS_ACCPOST-HKONT.    " General Ledger Account
  LS_ACCOUNTGL-ITEM_TEXT        = IS_ACCPOST-SGTXT.    " Item Text
* LS_ACCOUNTGL-STAT_CON         =                      " Indicator for statistical line items
* LS_ACCOUNTGL-LOG_PROC         =                      " Logical Transaction
* LS_ACCOUNTGL-AC_DOC_NO        =                      " Accounting Document Number
  LS_ACCOUNTGL-REF_KEY_1        = IS_ACCPOST-XREF1.    " Business Partner Reference Key
  LS_ACCOUNTGL-REF_KEY_2        = IS_ACCPOST-XREF2.    " Business Partner Reference Key
  LS_ACCOUNTGL-REF_KEY_3        = IS_ACCPOST-XREF3.    " Reference Key for Line Item
* LS_ACCOUNTGL-ACCT_KEY         =                      " Transaction Key
* LS_ACCOUNTGL-ACCT_TYPE        =                      " Account Type
  LS_ACCOUNTGL-DOC_TYPE         = IS_ACCPOST-BLART.    " Document Type
* LS_ACCOUNTGL-COMP_CODE        =                      " Company Code
* LS_ACCOUNTGL-BUS_AREA         =                      " Business Area
* LS_ACCOUNTGL-FUNC_AREA        =                      " Functional Area
* LS_ACCOUNTGL-PLANT            =                      " Plant
* LS_ACCOUNTGL-FIS_PERIOD       =                      " Fiscal Period
* LS_ACCOUNTGL-FISC_YEAR        =                      " Fiscal Year
  LS_ACCOUNTGL-PSTNG_DATE       = IS_ACCPOST-BUDAT.    " Posting Date in the Document
  LS_ACCOUNTGL-VALUE_DATE       = IS_ACCPOST-VALUT.    " Value Date
* LS_ACCOUNTGL-FM_AREA          =                      " Financial Management Area
* LS_ACCOUNTGL-CUSTOMER         =                      " Customer Number
* LS_ACCOUNTGL-CSHDIS_IND       =                      " Indicator: Line Item Not Liable to Cash Discount?
* LS_ACCOUNTGL-VENDOR_NO        =                      " Account Number of Vendor or Creditor
  LS_ACCOUNTGL-ALLOC_NMBR       = IS_ACCPOST-ZUONR.    " Assignment Number
  LS_ACCOUNTGL-TAX_CODE         = IS_ACCPOST-MWSKZ.    " Tax on sales/purchases code
* LS_ACCOUNTGL-TAXJURCODE       =                      " Tax Jurisdiction
* LS_ACCOUNTGL-EXT_OBJECT_ID    =                      " Technical Key of External Object
* LS_ACCOUNTGL-BUS_SCENARIO     =                      " Business Scenario in Controlling for Logistical Objects
* LS_ACCOUNTGL-COSTOBJECT       =                      " Cost Object
  LS_ACCOUNTGL-COSTCENTER       = IS_ACCPOST-KOSTL.    " Cost Center
* LS_ACCOUNTGL-ACTTYPE          =                      " Activity Type
* LS_ACCOUNTGL-PROFIT_CTR       =                      " Profit Center
* LS_ACCOUNTGL-PART_PRCTR       =                      " Partner Profit Center
* LS_ACCOUNTGL-NETWORK          =                      " Network Number for Account Assignment
* LS_ACCOUNTGL-WBS_ELEMENT      =                      " Work Breakdown Structure Element (WBS Element)
* LS_ACCOUNTGL-ORDERID          =                      " Order Number
* LS_ACCOUNTGL-ORDER_ITNO       =                      " Order Item Number
* LS_ACCOUNTGL-ROUTING_NO       =                      " Routing number of operations in the order
* LS_ACCOUNTGL-ACTIVITY         =                      " Operation/Activity Number
* LS_ACCOUNTGL-COND_TYPE        =                      " Condition type
* LS_ACCOUNTGL-COND_COUNT       =                      " Condition Counter
* LS_ACCOUNTGL-COND_ST_NO       =                      " Level Number
* LS_ACCOUNTGL-FUND             =                      " Fund
* LS_ACCOUNTGL-FUNDS_CTR        =                      " Funds Center
* LS_ACCOUNTGL-CMMT_ITEM        =                      " Commitment Item
* LS_ACCOUNTGL-CO_BUSPROC       =                      " Business Process
* LS_ACCOUNTGL-ASSET_NO         =                      " Main Asset Number
* LS_ACCOUNTGL-SUB_NUMBER       =                      " Asset Subnumber
* LS_ACCOUNTGL-BILL_TYPE        =                      " Billing Type
* LS_ACCOUNTGL-SALES_ORD        =                      " Sales Order Number
* LS_ACCOUNTGL-S_ORD_ITEM       =                      " Item Number in Sales Order
* LS_ACCOUNTGL-DISTR_CHAN       =                      " Distribution Channel
* LS_ACCOUNTGL-DIVISION         =                      " Division
* LS_ACCOUNTGL-SALESORG         =                      " Sales Organization
* LS_ACCOUNTGL-SALES_GRP        =                      " Sales Group
* LS_ACCOUNTGL-SALES_OFF        =                      " Sales Office
* LS_ACCOUNTGL-SOLD_TO          =                      " Sold-to party
* LS_ACCOUNTGL-DE_CRE_IND       =                      " Indicator: subsequent debit/credit
* LS_ACCOUNTGL-P_EL_PRCTR       =                      " Partner profit center for elimination of internal business
* LS_ACCOUNTGL-XMFRW            =                      " Indicator: Update quantity in RW
* LS_ACCOUNTGL-QUANTITY         =                      " Quantity
* LS_ACCOUNTGL-BASE_UOM         =                      " Base Unit of Measure
* LS_ACCOUNTGL-BASE_UOM_ISO     =                      " Base unit of measure in ISO code
* LS_ACCOUNTGL-INV_QTY          =                      " Actual Invoiced Quantity
* LS_ACCOUNTGL-INV_QTY_SU       =                      " Billing quantity in stockkeeping unit
* LS_ACCOUNTGL-SALES_UNIT       =                      " Sales unit
* LS_ACCOUNTGL-SALES_UNIT_ISO   =                      " Sales unit in ISO code
* LS_ACCOUNTGL-PO_PR_QNT        =                      " Quantity in order price quantity unit
* LS_ACCOUNTGL-PO_PR_UOM        =                      " Order price unit (purchasing)
* LS_ACCOUNTGL-PO_PR_UOM_ISO    =                      " Purchase order price unit in ISO code
* LS_ACCOUNTGL-ENTRY_QNT        =                      " Quantity in Unit of Entry
* LS_ACCOUNTGL-ENTRY_UOM        =                      " Unit of Entry
* LS_ACCOUNTGL-ENTRY_UOM_ISO    =                      " Unit of entry in ISO code
* LS_ACCOUNTGL-VOLUME           =                      " Volume
* LS_ACCOUNTGL-VOLUMEUNIT       =                      " Volume unit
* LS_ACCOUNTGL-VOLUMEUNIT_ISO   =                      " Volume unit in ISO code
* LS_ACCOUNTGL-GROSS_WT         =                      " Gross Weight
* LS_ACCOUNTGL-NET_WEIGHT       =                      " Net weight
* LS_ACCOUNTGL-UNIT_OF_WT       =                      " Weight unit
* LS_ACCOUNTGL-UNIT_OF_WT_ISO   =                      " Unit of weight in ISO code
* LS_ACCOUNTGL-ITEM_CAT         =                      " Item category in purchasing document
* LS_ACCOUNTGL-MATERIAL         =                      " Material Number
* LS_ACCOUNTGL-MATL_TYPE        =                      " Material Type
* LS_ACCOUNTGL-MVT_IND          =                      " Movement Indicator
* LS_ACCOUNTGL-REVAL_IND        =                      " Revaluation
* LS_ACCOUNTGL-ORIG_GROUP       =                      " Origin Group as Subdivision of Cost Element
* LS_ACCOUNTGL-ORIG_MAT         =                      " Material-related origin
* LS_ACCOUNTGL-SERIAL_NO        =                      " Sequential number of account assignment
* LS_ACCOUNTGL-PART_ACCT        =                      " Partner Account Number
* LS_ACCOUNTGL-TR_PART_BA       =                      " Trading Partner's Business Area
* LS_ACCOUNTGL-TRADE_ID         =                      " Company ID of Trading Partner
* LS_ACCOUNTGL-VAL_AREA         =                      " Valuation Area
* LS_ACCOUNTGL-VAL_TYPE         =                      " Valuation Type
* LS_ACCOUNTGL-ASVAL_DATE       =                      " Reference Date
* LS_ACCOUNTGL-PO_NUMBER        =                      " Purchasing Document Number
* LS_ACCOUNTGL-PO_ITEM          =                      " Item Number of Purchasing Document
* LS_ACCOUNTGL-ITM_NUMBER       =                      " Item number of the SD document
* LS_ACCOUNTGL-COND_CATEGORY    =                      " Condition Category (Examples: Tax, Freight, Price, Cost)
* LS_ACCOUNTGL-FUNC_AREA_LONG   =                      " Functional Area
* LS_ACCOUNTGL-CMMT_ITEM_LONG   =                      " Commitment Item
* LS_ACCOUNTGL-GRANT_NBR        =                      " Grant
* LS_ACCOUNTGL-CS_TRANS_T       =                      " Transaction Type
* LS_ACCOUNTGL-MEASURE          =                      " Funded Program
* LS_ACCOUNTGL-SEGMENT          =                      " Segment for Segmental Reporting
* LS_ACCOUNTGL-PARTNER_SEGMENT  =                      " Partner Segment for Segmental Reporting
* LS_ACCOUNTGL-RES_DOC          =                      " Document Number for Earmarked Funds
* LS_ACCOUNTGL-RES_ITEM         =                      " Earmarked Funds: Document Item
* LS_ACCOUNTGL-BILLING_PERIOD_START_DATE =             " Billing Period of Performance Start Date
* LS_ACCOUNTGL-BILLING_PERIOD_END_DATE   =             " Billing Period of Performance End Date
* LS_ACCOUNTGL-PPA_EX_IND                =             " PPA Exclude Indicator
* LS_ACCOUNTGL-FASTPAY                   =             " PPA Fast Pay Indicator
* LS_ACCOUNTGL-PARTNER_GRANT_NBR         =             " Partner Grant
* LS_ACCOUNTGL-BUDGET_PERIOD             =             " FM: Budget Period
* LS_ACCOUNTGL-PARTNER_BUDGET_PERIOD     =             " FM: Partner Budget Period
* LS_ACCOUNTGL-PARTNER_FUND              =             " Partner Fund
* LS_ACCOUNTGL-ITEMNO_TAX                =             " Document item number refering to tax document.
* LS_ACCOUNTGL-PAYMENT_TYPE              =             " Payment Type for Grantor
* LS_ACCOUNTGL-EXPENSE_TYPE              =             " Expense Type for Grantor
* LS_ACCOUNTGL-PROGRAM_PROFILE           =             " Grantor Program Profile

  APPEND LS_ACCOUNTGL TO ME->T_ACCOUNTGL.

  " If item has a tax, we have to distribute gross amount between this position and corresponding tax position.
  IF IS_ACCPOST-MWSKZ IS NOT INITIAL.
    ME->FILL_ACCOUNTTAX( IS_ACCPOST = IS_ACCPOST ).
  ELSE.

    " (BAPIACCR09) Currency Items
    CLEAR LS_CURRENCYAMOUNT.

    LS_CURRENCYAMOUNT-ITEMNO_ACC   = ME->N_ITEMNO_ACC.   " Accounting Document Line Item Number
    LS_CURRENCYAMOUNT-CURR_TYPE    = '00'.               " Currency Type and Valuation View
    LS_CURRENCYAMOUNT-CURRENCY     = IS_ACCPOST-WAERS.   " Currency Key
*   LS_CURRENCYAMOUNT-CURRENCY_ISO =                     " ISO code currency
    LS_CURRENCYAMOUNT-AMT_DOCCUR   = IS_ACCPOST-WRBTR.   " Amount in Document Currency
*   LS_CURRENCYAMOUNT-EXCH_RATE    =                     " Exchange rate
*   LS_CURRENCYAMOUNT-EXCH_RATE_V  =                     " Indirect quoted exchange rate
*   LS_CURRENCYAMOUNT-AMT_BASE     =                     " Tax Base Amount in Document Currency
*   LS_CURRENCYAMOUNT-DISC_BASE    =                     " Amount eligible for cash discount in document currency
*   LS_CURRENCYAMOUNT-DISC_AMT     =                     " Cash discount amount in the currency of the currency types
*   LS_CURRENCYAMOUNT-TAX_AMT      =                     " Amount in Document Currency

    APPEND LS_CURRENCYAMOUNT TO ME->T_CURRENCYAMOUNT.

  ENDIF.

  ADD 1 TO ME->N_ITEMNO_ACC.

ENDMETHOD.


METHOD FILL_ACCOUNTPAYABLE.
  DATA: LS_ACCOUNTPAYABLE TYPE BAPIACAP09,
        LS_CURRENCYAMOUNT TYPE BAPIACCR09.


  CLEAR LS_ACCOUNTPAYABLE.

  LS_ACCOUNTPAYABLE-ITEMNO_ACC      = ME->N_ITEMNO_ACC.    " Accounting Document Line Item Number
  LS_ACCOUNTPAYABLE-VENDOR_NO       = IS_ACCPOST-LIFNR.    " Account Number of Vendor or Creditor
* LS_ACCOUNTPAYABLE-GL_ACCOUNT      =                      " General Ledger Account
  LS_ACCOUNTPAYABLE-REF_KEY_1       = IS_ACCPOST-XREF1.    " Business Partner Reference Key
  LS_ACCOUNTPAYABLE-REF_KEY_2       = IS_ACCPOST-XREF2.    " Business Partner Reference Key
  LS_ACCOUNTPAYABLE-REF_KEY_3       = IS_ACCPOST-XREF3.    " Reference Key for Line Item
  LS_ACCOUNTPAYABLE-COMP_CODE       = IS_ACCPOST-BUKRS.    " Company Code
* LS_ACCOUNTPAYABLE-BUS_AREA        =                      " Business Area
  LS_ACCOUNTPAYABLE-PMNTTRMS        = IS_ACCPOST-ZTERM.    " Terms of Payment Key
  LS_ACCOUNTPAYABLE-BLINE_DATE      = IS_ACCPOST-ZFBDT.    " Baseline Date For Due Date Calculation
  LS_ACCOUNTPAYABLE-DSCT_DAYS1      = IS_ACCPOST-ZBD1T.    " Days for first cash discount
* LS_ACCOUNTPAYABLE-DSCT_DAYS2      =                      " Days for second cash discount
* LS_ACCOUNTPAYABLE-NETTERMS        =                      " Deadline for net conditions
* LS_ACCOUNTPAYABLE-DSCT_PCT1       =                      " Percentage for First Cash Discount
* LS_ACCOUNTPAYABLE-DSCT_PCT2       =                      " Percentage for Second Cash Discount
  LS_ACCOUNTPAYABLE-PYMT_METH       = IS_ACCPOST-ZLSCH.    " Payment method
* LS_ACCOUNTPAYABLE-PMTMTHSUPL      =                      " Payment Method Supplement
  LS_ACCOUNTPAYABLE-PMNT_BLOCK      = IS_ACCPOST-ZLSPR.    " Payment block key
* LS_ACCOUNTPAYABLE-SCBANK_IND      =                      " State Central Bank Indicator
* LS_ACCOUNTPAYABLE-SUPCOUNTRY      =                      " Supplying Country
* LS_ACCOUNTPAYABLE-SUPCOUNTRY_ISO  =                      " Supplier country ISO code
* LS_ACCOUNTPAYABLE-BLLSRV_IND      =                      " Service Indicator (Foreign Payment)
  LS_ACCOUNTPAYABLE-ALLOC_NMBR      = IS_ACCPOST-ZUONR.    " Assignment Number
  LS_ACCOUNTPAYABLE-ITEM_TEXT       = IS_ACCPOST-SGTXT.    " Item Text
* LS_ACCOUNTPAYABLE-PO_SUB_NO       =                      " ISR Subscriber Number
* LS_ACCOUNTPAYABLE-PO_CHECKDG      =                      " ISR Check Digit
* LS_ACCOUNTPAYABLE-PO_REF_NO       =                      " ISR Reference Number
* LS_ACCOUNTPAYABLE-W_TAX_CODE      =                      " Withholding tax code
* LS_ACCOUNTPAYABLE-BUSINESSPLACE   =                      " Stores
* LS_ACCOUNTPAYABLE-SECTIONCODE     =                      " Section Code
* LS_ACCOUNTPAYABLE-INSTR1          =                      " Instruction Key 1
* LS_ACCOUNTPAYABLE-INSTR2          =                      " Instruction Key 2
* LS_ACCOUNTPAYABLE-INSTR3          =                      " Instruction Key 3
* LS_ACCOUNTPAYABLE-INSTR4          =                      " Instruction Key 4
* LS_ACCOUNTPAYABLE-BRANCH          =                      " Account number of the branch
* LS_ACCOUNTPAYABLE-PYMT_CUR        =                      " Currency for automatic payment
* LS_ACCOUNTPAYABLE-PYMT_AMT        =                      " Amount in Payment Currency
* LS_ACCOUNTPAYABLE-PYMT_CUR_ISO    =                      " ISO code currency
  LS_ACCOUNTPAYABLE-SP_GL_IND       = IS_ACCPOST-UMSKS.    " Special G/L Indicator
* LS_ACCOUNTPAYABLE-TAX_CODE        =                      " Tax on sales/purchases code
* LS_ACCOUNTPAYABLE-TAX_DATE        =                      " Date Relevant for Determining the Tax Rate
* LS_ACCOUNTPAYABLE-TAXJURCODE      =                      " Tax Jurisdiction
* LS_ACCOUNTPAYABLE-ALT_PAYEE       =                      " Alternative payee
* LS_ACCOUNTPAYABLE-ALT_PAYEE_BANK  =                      " Bank type of alternative payer
* LS_ACCOUNTPAYABLE-PARTNER_BK      =                      " Partner Bank Type
  LS_ACCOUNTPAYABLE-BANK_ID         = IS_ACCPOST-HBKID.    " Short Key for a House Bank
* LS_ACCOUNTPAYABLE-PARTNER_GUID    =                      " Com. Interface: Business Partner GUID
* LS_ACCOUNTPAYABLE-PROFIT_CTR      =                      " Profit Center
* LS_ACCOUNTPAYABLE-FUND            =                      " Fund
* LS_ACCOUNTPAYABLE-GRANT_NBR       =                      " Grant
* LS_ACCOUNTPAYABLE-MEASURE         =                      " Funded Program
  LS_ACCOUNTPAYABLE-HOUSEBANKACCTID = IS_ACCPOST-HKTID.    " ID for Account Details
* LS_ACCOUNTPAYABLE-BUDGET_PERIOD   =                      " FM: Budget Period
* LS_ACCOUNTPAYABLE-PPA_EX_IND      =                      " PPA Exclude Indicator

  APPEND LS_ACCOUNTPAYABLE TO ME->T_ACCOUNTPAYABLE.


  IF IS_ACCPOST-MWSKZ IS NOT INITIAL.
    ME->FILL_ACCOUNTTAX( IS_ACCPOST = IS_ACCPOST ).
  ELSE.

    " (BAPIACCR09) Currency Items
    CLEAR LS_CURRENCYAMOUNT.

    LS_CURRENCYAMOUNT-ITEMNO_ACC   = ME->N_ITEMNO_ACC.   " Accounting Document Line Item Number
    LS_CURRENCYAMOUNT-CURR_TYPE    = '00'.               " Currency Type and Valuation View
    LS_CURRENCYAMOUNT-CURRENCY     = IS_ACCPOST-WAERS.   " Currency Key
*   LS_CURRENCYAMOUNT-CURRENCY_ISO =                     " ISO code currency
    LS_CURRENCYAMOUNT-AMT_DOCCUR   = IS_ACCPOST-WRBTR.   " Amount in Document Currency
*   LS_CURRENCYAMOUNT-EXCH_RATE    =                     " Exchange rate
*   LS_CURRENCYAMOUNT-EXCH_RATE_V  =                     " Indirect quoted exchange rate
*   LS_CURRENCYAMOUNT-AMT_BASE     =                     " Tax Base Amount in Document Currency
*   LS_CURRENCYAMOUNT-DISC_BASE    =                     " Amount eligible for cash discount in document currency
*   LS_CURRENCYAMOUNT-DISC_AMT     =                     " Cash discount amount in the currency of the currency types
*   LS_CURRENCYAMOUNT-TAX_AMT      =                     " Amount in Document Currency

    APPEND LS_CURRENCYAMOUNT TO ME->T_CURRENCYAMOUNT.

  ENDIF.

  ADD 1 TO ME->N_ITEMNO_ACC.

ENDMETHOD.


METHOD FILL_ACCOUNTRECEIVABLE.
  DATA: LS_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
        LS_CURRENCYAMOUNT TYPE BAPIACCR09.


  CLEAR LS_ACCOUNTRECEIVABLE.

  LS_ACCOUNTRECEIVABLE-ITEMNO_ACC      = ME->N_ITEMNO_ACC.      " Accounting Document Line Item Number
  LS_ACCOUNTRECEIVABLE-CUSTOMER        = IS_ACCPOST-KUNNR.      " Customer Number
* LS_ACCOUNTRECEIVABLE-GL_ACCOUNT      =                        " General Ledger Account
  LS_ACCOUNTRECEIVABLE-REF_KEY_1       = IS_ACCPOST-XREF1.      " Business Partner Reference Key
  LS_ACCOUNTRECEIVABLE-REF_KEY_2       = IS_ACCPOST-XREF2.      " Business Partner Reference Key
  LS_ACCOUNTRECEIVABLE-REF_KEY_3       = IS_ACCPOST-XREF3.      " Reference Key for Line Item
* LS_ACCOUNTRECEIVABLE-COMP_CODE       =                        " Company Code
* LS_ACCOUNTRECEIVABLE-BUS_AREA        =                        " Business Area
  LS_ACCOUNTRECEIVABLE-PMNTTRMS        = IS_ACCPOST-ZUONR.      " Terms of Payment Key
  LS_ACCOUNTRECEIVABLE-BLINE_DATE      = IS_ACCPOST-ZFBDT.      " Baseline Date For Due Date Calculation
  LS_ACCOUNTRECEIVABLE-DSCT_DAYS1      = IS_ACCPOST-ZBD1T.      " Days for first cash discount
* LS_ACCOUNTRECEIVABLE-DSCT_DAYS2      =                        " Days for second cash discount
* LS_ACCOUNTRECEIVABLE-NETTERMS        =                        " Deadline for net conditions
* LS_ACCOUNTRECEIVABLE-DSCT_PCT1       =                        " Percentage for First Cash Discount
* LS_ACCOUNTRECEIVABLE-DSCT_PCT2       =                        " Percentage for Second Cash Discount
  LS_ACCOUNTRECEIVABLE-PYMT_METH       = IS_ACCPOST-ZLSCH.      " Payment method
* LS_ACCOUNTRECEIVABLE-PMTMTHSUPL      =                        " Payment Method Supplement
* LS_ACCOUNTRECEIVABLE-PAYMT_REF       =                        " Payment Reference
* LS_ACCOUNTRECEIVABLE-DUNN_KEY        =                        " Dunning keys
* LS_ACCOUNTRECEIVABLE-DUNN_BLOCK      =                        " Dunning block
  LS_ACCOUNTRECEIVABLE-PMNT_BLOCK      = IS_ACCPOST-ZLSPR.      " Payment block key
* LS_ACCOUNTRECEIVABLE-VAT_REG_NO      =                        " VAT Registration Number
  LS_ACCOUNTRECEIVABLE-ALLOC_NMBR      = IS_ACCPOST-ZUONR.      " Assignment Number
  LS_ACCOUNTRECEIVABLE-ITEM_TEXT       = IS_ACCPOST-SGTXT.      " Item Text
* LS_ACCOUNTRECEIVABLE-PARTNER_BK      =                        " Partner Bank Type
* LS_ACCOUNTRECEIVABLE-SCBANK_IND      =                        " State Central Bank Indicator
* LS_ACCOUNTRECEIVABLE-BUSINESSPLACE   =                        " Stores
* LS_ACCOUNTRECEIVABLE-SECTIONCODE     =                        " Section Code
* LS_ACCOUNTRECEIVABLE-BRANCH          =                        " Account number of the branch
* LS_ACCOUNTRECEIVABLE-PYMT_CUR        =                        " Currency for automatic payment
* LS_ACCOUNTRECEIVABLE-PYMT_CUR_ISO    =                        " ISO code currency
* LS_ACCOUNTRECEIVABLE-PYMT_AMT        =                        " Amount in Payment Currency
* LS_ACCOUNTRECEIVABLE-C_CTR_AREA      =                        " Credit control area
  LS_ACCOUNTRECEIVABLE-BANK_ID         = IS_ACCPOST-HBKID.      " Short Key for a House Bank
* LS_ACCOUNTRECEIVABLE-SUPCOUNTRY      =                        " Supplying Country
* LS_ACCOUNTRECEIVABLE-SUPCOUNTRY_ISO  =                        " Supplier country ISO code
* LS_ACCOUNTRECEIVABLE-TAX_CODE        =                        " Tax on sales/purchases code
* LS_ACCOUNTRECEIVABLE-TAXJURCODE      =                        " Tax Jurisdiction
* LS_ACCOUNTRECEIVABLE-TAX_DATE        =                        " Date Relevant for Determining the Tax Rate
  LS_ACCOUNTRECEIVABLE-SP_GL_IND       = IS_ACCPOST-UMSKS.      " Special G/L Indicator
* LS_ACCOUNTRECEIVABLE-PARTNER_GUID    =                        " Com. Interface: Business Partner GUID
* LS_ACCOUNTRECEIVABLE-ALT_PAYEE       =                        " Alternative payee
* LS_ACCOUNTRECEIVABLE-ALT_PAYEE_BANK  =                        " Bank type of alternative payer
* LS_ACCOUNTRECEIVABLE-DUNN_AREA       =                        " Dunning Area
* LS_ACCOUNTRECEIVABLE-CASE_GUID       =                        " Technical Case Key (Case GUID)
* LS_ACCOUNTRECEIVABLE-PROFIT_CTR      =                        " Profit Center
* LS_ACCOUNTRECEIVABLE-FUND            =                        " Fund
* LS_ACCOUNTRECEIVABLE-GRANT_NBR       =                        " Grant
* LS_ACCOUNTRECEIVABLE-MEASURE         =                        " Funded Program
  LS_ACCOUNTRECEIVABLE-HOUSEBANKACCTID = IS_ACCPOST-HKTID.      " ID for Account Details
* LS_ACCOUNTRECEIVABLE-RES_DOC         =                        " Document Number for Earmarked Funds
* LS_ACCOUNTRECEIVABLE-RES_ITEM        =                        " Earmarked Funds: Document Item
* LS_ACCOUNTRECEIVABLE-FUND_LONG       =                        " Long Fund (Obsolete)
* LS_ACCOUNTRECEIVABLE-DISPUTE_IF_TYPE =                        " Dispute Management: Dispute Interface Category
* LS_ACCOUNTRECEIVABLE-BUDGET_PERIOD   =                        " FM: Budget Period
* LS_ACCOUNTRECEIVABLE-PAYS_PROV       =                        " Payment Service Provider
* LS_ACCOUNTRECEIVABLE-PAYS_TRAN       =                        " Payment Reference of Payment Service Provider
* LS_ACCOUNTRECEIVABLE-SEPA_MANDATE_ID =                        " Unique Reference to Mandate for each Payee

  APPEND LS_ACCOUNTRECEIVABLE TO ME->T_ACCOUNTRECEIVABLE.


  " Custormer CPD
  IF IS_ACCPOST-NAME IS NOT INITIAL.
    ME->FILL_CUSTOMERCPD( IS_ACCPOST = IS_ACCPOST ).
  ENDIF.


  " Currency and tax items
  IF IS_ACCPOST-MWSKZ IS NOT INITIAL.
    ME->FILL_ACCOUNTTAX( IS_ACCPOST = IS_ACCPOST ).
  ELSE.

    " (BAPIACCR09) Currency Items
    CLEAR LS_CURRENCYAMOUNT.

    LS_CURRENCYAMOUNT-ITEMNO_ACC   = ME->N_ITEMNO_ACC.   " Accounting Document Line Item Number
    LS_CURRENCYAMOUNT-CURR_TYPE    = '00'.               " Currency Type and Valuation View
    LS_CURRENCYAMOUNT-CURRENCY     = IS_ACCPOST-WAERS.   " Currency Key
*   LS_CURRENCYAMOUNT-CURRENCY_ISO =                     " ISO code currency
    LS_CURRENCYAMOUNT-AMT_DOCCUR   = IS_ACCPOST-WRBTR.   " Amount in Document Currency
*   LS_CURRENCYAMOUNT-EXCH_RATE    =                     " Exchange rate
*   LS_CURRENCYAMOUNT-EXCH_RATE_V  =                     " Indirect quoted exchange rate
*   LS_CURRENCYAMOUNT-AMT_BASE     =                     " Tax Base Amount in Document Currency
*   LS_CURRENCYAMOUNT-DISC_BASE    =                     " Amount eligible for cash discount in document currency
*   LS_CURRENCYAMOUNT-DISC_AMT     =                     " Cash discount amount in the currency of the currency types
*   LS_CURRENCYAMOUNT-TAX_AMT      =                     " Amount in Document Currency

    APPEND LS_CURRENCYAMOUNT TO ME->T_CURRENCYAMOUNT.

  ENDIF.

  ADD 1 TO ME->N_ITEMNO_ACC.

ENDMETHOD.


METHOD FILL_ACCOUNTTAX.

  TYPES: TT_MWDAT TYPE TABLE OF RTAX1U15.
  DATA: LT_MWDAT          TYPE TT_MWDAT,
        LS_MWDAT          TYPE LINE OF TT_MWDAT,
        LS_ACCOUNTTAX     TYPE BAPIACTX09,
        LS_CURRENCYAMOUNT TYPE BAPIACCR09,
        LV_WRBTR          TYPE BSEG-WRBTR.


  " Initialization
  CLEAR LT_MWDAT.

  " Cast value to expected data type
  LV_WRBTR = IS_ACCPOST-WRBTR.

  " Get net amount and tax amount
  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      I_BUKRS                 = IS_ACCPOST-BUKRS
      I_MWSKZ                 = IS_ACCPOST-MWSKZ
      I_WAERS                 = IS_ACCPOST-WAERS
      I_WRBTR                 = LV_WRBTR
    TABLES
      T_MWDAT                 = lT_MWDAT
    EXCEPTIONS
      BUKRS_NOT_FOUND         = 1
      COUNTRY_NOT_FOUND       = 2
      MWSKZ_NOT_DEFINED       = 3
      MWSKZ_NOT_VALID         = 4
      ACCOUNT_NOT_FOUND       = 5
      DIFFERENT_DISCOUNT_BASE = 6
      DIFFERENT_TAX_BASE      = 7
      TXJCD_NOT_VALID         = 8
      NOT_FOUND               = 9
      KTOSL_NOT_FOUND         = 10
      KALSM_NOT_FOUND         = 11
      PARAMETER_ERROR         = 12
      KNUMH_NOT_FOUND         = 13
      KSCHL_NOT_FOUND         = 14
      UNKNOWN_ERROR           = 15
      OTHERS                  = 16.

  IF SY-SUBRC <> 0.
    RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
      EXPORTING
        TEXTID   = ZCX_SIMPLE_ACC_POST=>TAX_ERROR
        ATTR1    = |{ SY-SUBRC }|
        ATTR2    = |{ IS_ACCPOST-BUKRS }-{ IS_ACCPOST-MWSKZ }-{ IS_ACCPOST-WAERS }|
        ATTR3    = |{ LV_WRBTR }|
        ATTR4    = |{ ME->N_ACCPOST_TABIX }|.
    .
  ENDIF.

  " For each tax
  LOOP AT lT_MWDAT INTO lS_MWDAT.

    " Condition base value
    CLEAR LS_CURRENCYAMOUNT.

    LS_CURRENCYAMOUNT-ITEMNO_ACC   = ME->N_ITEMNO_ACC.   " Accounting Document Line Item Number
    LS_CURRENCYAMOUNT-CURR_TYPE    = '00'.               " Currency Type and Valuation View
    LS_CURRENCYAMOUNT-CURRENCY     = IS_ACCPOST-WAERS.   " Currency Key
*   LS_CURRENCYAMOUNT-CURRENCY_ISO =                     " ISO code currency
    LS_CURRENCYAMOUNT-AMT_DOCCUR   = LS_MWDAT-KAWRT.     " Amount in Document Currency
*   LS_CURRENCYAMOUNT-EXCH_RATE    =                     " Exchange rate
*   LS_CURRENCYAMOUNT-EXCH_RATE_V  =                     " Indirect quoted exchange rate
*   LS_CURRENCYAMOUNT-AMT_BASE     =                     " Tax Base Amount in Document Currency
*   LS_CURRENCYAMOUNT-DISC_BASE    =                     " Amount eligible for cash discount in document currency
*   LS_CURRENCYAMOUNT-DISC_AMT     =                     " Cash discount amount in the currency of the currency types
*   LS_CURRENCYAMOUNT-TAX_AMT      =                     " Amount in Document Currency

    APPEND LS_CURRENCYAMOUNT TO ME->T_CURRENCYAMOUNT.
    ADD 1 TO ME->N_ITEMNO_ACC.


    " New position with calculated tax
    CLEAR LS_ACCOUNTTAX.

    LS_ACCOUNTTAX-ITEMNO_ACC        = ME->N_ITEMNO_ACC. " Accounting Document Line Item Number
    LS_ACCOUNTTAX-GL_ACCOUNT        = LS_MWDAT-HKONT.   " General Ledger Account
    LS_ACCOUNTTAX-COND_KEY          = LS_MWDAT-KSCHL.   " Condition Type
    LS_ACCOUNTTAX-ACCT_KEY          = LS_MWDAT-KTOSL.   " Transaction Key
    LS_ACCOUNTTAX-TAX_CODE          = IS_ACCPOST-MWSKZ. " Tax on sales/purchases code
    LS_ACCOUNTTAX-TAX_RATE          = LS_MWDAT-MSATZ.   " Tax rate
*    LS_ACCOUNTTAX-TAX_DATE         =                   " Date Relevant for Determining the Tax Rate
*    LS_ACCOUNTTAX-TAXJURCODE       =                   " Tax Jurisdiction
*    LS_ACCOUNTTAX-TAXJURCODE_DEEP  =                   " Tax jurisdiction code - jurisdiction for lowest level tax
*    LS_ACCOUNTTAX-TAXJURCODE_LEVEL =                   " Tax Jurisdiction Code Level
*    LS_ACCOUNTTAX-ITEMNO_TAX       =                   " Document item number refering to tax document.
*    LS_ACCOUNTTAX-DIRECT_TAX       =                   " Indicator: Direct Tax Posting

    APPEND LS_ACCOUNTTAX TO ME->T_ACCOUNTTAX.


    " Tax Amount in Document Currency
    CLEAR LS_CURRENCYAMOUNT.

    LS_CURRENCYAMOUNT-ITEMNO_ACC   = ME->N_ITEMNO_ACC.    " Accounting Document Line Item Number
    LS_CURRENCYAMOUNT-CURR_TYPE    = '00'.                " Currency Type and Valuation View
    LS_CURRENCYAMOUNT-CURRENCY     = IS_ACCPOST-WAERS.    " Currency Key
*   LS_CURRENCYAMOUNT-CURRENCY_ISO =                      " ISO code currency
    LS_CURRENCYAMOUNT-AMT_DOCCUR   = LS_MWDAT-WMWST.      " Amount in Document Currency
*   LS_CURRENCYAMOUNT-EXCH_RATE    =                      " Exchange rate
*   LS_CURRENCYAMOUNT-EXCH_RATE_V  =                      " Indirect quoted exchange rate
    LS_CURRENCYAMOUNT-AMT_BASE     = LS_MWDAT-KAWRT * -1. " Tax Base Amount in Document Currency
*   LS_CURRENCYAMOUNT-DISC_BASE    =                      " Amount eligible for cash discount in document currency
*   LS_CURRENCYAMOUNT-DISC_AMT     =                      " Cash discount amount in the currency of the currency types
*   LS_CURRENCYAMOUNT-TAX_AMT      =                      " Amount in Document Currency

    APPEND LS_CURRENCYAMOUNT TO ME->T_CURRENCYAMOUNT.
    ADD 1 TO ME->N_ITEMNO_ACC.

  ENDLOOP.


ENDMETHOD.


METHOD FILL_BAPI.
  DATA: LR_CX_ROOT TYPE REF TO CX_ROOT,
        LR_ZCX_SIMPLE_ACC_POST TYPE REF TO ZCX_SIMPLE_ACC_POST,
        LS_ACCPOST TYPE ZFIS_SIMPLE_ACCPOST.


  " Add LOG to message collector
  ME->R_MSG_LOG->ADD(
    EXPORTING
      ID_MSGTY = 'S' ID_MSGID = 'ZFSAP' ID_MSGNO = '000'
      ID_MSGV1 = 'Accounting document ID: '
      ID_MSGV2 = |{ CS_ACCPOST_DOC-ID_ACCDOC }|
  ).


  " Initialize data
  CLEAR: S_DOCUMENTHEADER, S_CUSTOMERCPD, T_ACCOUNTGL, T_ACCOUNTPAYABLE, T_ACCOUNTRECEIVABLE,
         T_ACCOUNTTAX, T_ACCOUNTWT, T_CONTRACTITEM, T_CRITERIA, T_CURRENCYAMOUNT, T_EXTENSION1, T_EXTENSION2,
         T_PAYMENTCARD, T_REALESTATE, T_RETURN, T_VALUEFIELD.


  " Fill BAPI data
  TRY.

      " Document Header
      ME->FILL_DOCUMENTHEADER(
        CHANGING
          CS_ACCPOST_DOC = CS_ACCPOST_DOC    " Header information
      ).


      " Items
      ME->N_ITEMNO_ACC = 1.
      LOOP AT ME->T_ACCPOST INTO LS_ACCPOST WHERE ID_ACCDOC = CS_ACCPOST_DOC-ID_ACCDOC.

        " G/L items
        IF LS_ACCPOST-HKONT IS NOT INITIAL.
          ME->FILL_ACCOUNTGL( IS_ACCPOST = LS_ACCPOST ).
        ENDIF.

        " Vendor items
        IF LS_ACCPOST-LIFNR IS NOT INITIAL.
          ME->FILL_ACCOUNTPAYABLE( IS_ACCPOST = LS_ACCPOST ).
        ENDIF.

        " Debtor items (Customer CPD as well)
        IF LS_ACCPOST-KUNNR IS NOT INITIAL.
          ME->FILL_ACCOUNTRECEIVABLE( IS_ACCPOST = LS_ACCPOST ).
        ENDIF.

      ENDLOOP.

      " Extension
*     CLEAR ME->T_EXTENSION1.
*     DATA: LS_extension1 TYPE BAPIACEXTC.
*     LS_EXTENSION1-FIELD1 = 'BAPI CALL'.
*     APPEND LS_EXTENSION1 TO ME->T_EXTENSION1.


    CATCH ZCX_SIMPLE_ACC_POST INTO LR_ZCX_SIMPLE_ACC_POST.

      " Add exception to message collector
      ME->R_MSG_LOG->ADD_FROM_EXCEPTION(
        EXPORTING
          IO_EXCEPTION = LR_ZCX_SIMPLE_ACC_POST
          ID_TABNAME   = 'CS_ACCPOST_DOC'
          ID_FIELDNAME = 'ID_ACCDOC'
          ID_VALUE     = |{ CS_ACCPOST_DOC-ID_ACCDOC }|
      ).

      " Add exception to auxiliary messages table
      DATA: LS_BAPIRET2 TYPE BAPIRET2.
      LR_ZCX_SIMPLE_ACC_POST->GET_BAPIRET2(
        EXPORTING
          IV_MSGTY = 'E'
        IMPORTING
          ES_BAPIRET2 = LS_BAPIRET2
      ).
      INSERT LS_BAPIRET2 INTO TABLE ME->T_ACCPOST_MSG.

      " Update document status
      CS_ACCPOST_DOC-STATUS = ZCL_SIMPLE_ACC_POST=>C_STATUS_ERROR.

  ENDTRY.


ENDMETHOD.


method FILL_CUSTOMERCPD.
  DATA LS_CUSTOMERCPD TYPE BAPIACPA09.

  CLEAR LS_CUSTOMERCPD.

  LS_CUSTOMERCPD-NAME          = IS_ACCPOST-NAME.         " Name 1
* LS_CUSTOMERCPD-NAME_2        =                          " Name 2
* LS_CUSTOMERCPD-NAME_3        =                          " Name 3
* LS_CUSTOMERCPD-NAME_4        =                          " Name 4
  LS_CUSTOMERCPD-POSTL_CODE    = IS_ACCPOST-POSTL_CODE.   " Postal Code
  LS_CUSTOMERCPD-CITY          = IS_ACCPOST-CITY.         " City
* LS_CUSTOMERCPD-COUNTRY       =                          " Country Key
  LS_CUSTOMERCPD-COUNTRY_ISO   = IS_ACCPOST-COUNTRY_ISO.  " Country key in ISO code
  LS_CUSTOMERCPD-STREET        = IS_ACCPOST-STREET.       " House number and street
* LS_CUSTOMERCPD-PO_BOX        =                          " PO Box
* LS_CUSTOMERCPD-POBX_PCD      =                          " P.O. Box Postal Code
* LS_CUSTOMERCPD-POBK_CURAC    =                          " Account Number of Bank Account At Post Office
* LS_CUSTOMERCPD-BANK_ACCT     =                          " Bank account number
* LS_CUSTOMERCPD-BANK_NO       =                          " Bank number
* LS_CUSTOMERCPD-BANK_CTRY     =                          " Bank country key
* LS_CUSTOMERCPD-BANK_CTRY_ISO =                          " Bank country key in ISO code
  LS_CUSTOMERCPD-TAX_NO_1      = IS_ACCPOST-TAX_NO_1.     " Tax Number 1
* LS_CUSTOMERCPD-TAX_NO_2      =                          " Tax Number 2
* LS_CUSTOMERCPD-TAX           =                          " Liable for VAT
* LS_CUSTOMERCPD-EQUAL_TAX     =                          " Indicator: Business partner subject to equalization tax ?
* LS_CUSTOMERCPD-REGION        =                          " Region (State, Province, County)
* LS_CUSTOMERCPD-CTRL_KEY      =                          " Bank Control Key
* LS_CUSTOMERCPD-INSTR_KEY     =                          " Instruction key for data medium exchange
* LS_CUSTOMERCPD-DME_IND       =                          " Indicator for Data Medium Exchange
* LS_CUSTOMERCPD-LANGU_ISO     =                          " 2-Character SAP Language Code
* LS_CUSTOMERCPD-IBAN          =                          " IBAN (International Bank Account Number)
* LS_CUSTOMERCPD-SWIFT_CODE    =                          " SWIFT/BIC for International Payments
* LS_CUSTOMERCPD-TAX_NO_3      =                          " Tax Number 3
* LS_CUSTOMERCPD-TAX_NO_4      =                          " Tax Number 4

  ME->S_CUSTOMERCPD = LS_CUSTOMERCPD.

endmethod.


METHOD FILL_DOCUMENTHEADER.
  DATA: LS_ACCPOST TYPE LINE OF ME->TT_ACCPOST.

  " Read data from one item of this group
  READ TABLE ME->T_ACCPOST INTO LS_ACCPOST WITH KEY ID_ACCDOC = CS_ACCPOST_DOC-ID_ACCDOC.

  " Fill header data
  CLEAR ME->S_DOCUMENTHEADER.

* ME->S_DOCUMENTHEADER-OBJ_TYPE         =                    " Reference Transaction
* ME->S_DOCUMENTHEADER-OBJ_KEY          =                    " Reference Key
* ME->S_DOCUMENTHEADER-OBJ_SYS          =                    " Logical system of source document
* ME->S_DOCUMENTHEADER-BUS_ACT          =                    " Business Transaction
  ME->S_DOCUMENTHEADER-USERNAME         = SY-UNAME.          " User name
  ME->S_DOCUMENTHEADER-HEADER_TXT       = LS_ACCPOST-BKTXT.  " Document Header Text
  ME->S_DOCUMENTHEADER-COMP_CODE        = LS_ACCPOST-BUKRS.  " Company Code
  ME->S_DOCUMENTHEADER-DOC_DATE         = LS_ACCPOST-BLDAT.  " Document Date in Document
  ME->S_DOCUMENTHEADER-PSTNG_DATE       = LS_ACCPOST-BUDAT.  " Posting Date in the Document
* ME->S_DOCUMENTHEADER-TRANS_DATE       =                    " Translation Date
* ME->S_DOCUMENTHEADER-FISC_YEAR        =                    " Fiscal Year
* ME->S_DOCUMENTHEADER-FIS_PERIOD       =                    " Fiscal Period
  ME->S_DOCUMENTHEADER-DOC_TYPE         = LS_ACCPOST-BLART.  " Document Type
  ME->S_DOCUMENTHEADER-REF_DOC_NO       = LS_ACCPOST-XBLNR.  " Reference Document Number
* ME->S_DOCUMENTHEADER-AC_DOC_NO        =                    " Accounting Document Number
* ME->S_DOCUMENTHEADER-OBJ_KEY_R        =                    " Cancel: object key (AWREF_REV and AWORG_REV)
* ME->S_DOCUMENTHEADER-REASON_REV       =                    " Reason for reversal
* ME->S_DOCUMENTHEADER-COMPO_ACC        =                    " Component in ACC Interface
* ME->S_DOCUMENTHEADER-REF_DOC_NO_LONG  =                    " Reference Document Number (for Dependencies see Long Text)
* ME->S_DOCUMENTHEADER-ACC_PRINCIPLE    =                    " Accounting Principle
* ME->S_DOCUMENTHEADER-NEG_POSTNG       =                    " Indicator: Negative posting
* ME->S_DOCUMENTHEADER-OBJ_KEY_INV      =                    " Invoice Ref.: Object Key (AWREF_REB and AWORG_REB)
* ME->S_DOCUMENTHEADER-BILL_CATEGORY    =                    " Billing category
* ME->S_DOCUMENTHEADER-VATDATE          =                    " Tax Reporting Date
* ME->S_DOCUMENTHEADER-INVOICE_REC_DATE =                    " Invoice Receipt Date
* ME->S_DOCUMENTHEADER-ECS_ENV          =                    " ECS Environment
* ME->S_DOCUMENTHEADER-PARTIAL_REV      =                    " Indicator: Partial Reversal


ENDMETHOD.


method FILL_T_ACCPOST_WITH_CLIPDATA.
  DATA: LT_ACCPOST TYPE STANDARD TABLE OF ZFIS_SIMPLE_ACCPOST,
        LR_DATA_PARSER_ERROR TYPE REF TO LCX_DATA_PARSER_ERROR,
        LV_CLIPDATA TYPE STRING.

  " Read clipboard. First line must be field names of required data for posting (See structure ZFIS_SIMPLE_ACCPOST).
  LV_CLIPDATA = ME->READ_CLIPBOARD( ).

  " Parse clipboard data to LT_ACCPOST
  TRY .

      LCL_DATA_PARSER=>CREATE( LT_ACCPOST )->PARSE(
         EXPORTING I_DATA      = LV_CLIPDATA
                   I_STRICT    = ABAP_FALSE
                   I_HAS_HEAD  = ABAP_TRUE
         IMPORTING E_CONTAINER = LT_ACCPOST
      ).

    CATCH LCX_DATA_PARSER_ERROR INTO LR_DATA_PARSER_ERROR.
      ME->R_MSG_LOG->ADD_FROM_EXCEPTION( EXPORTING IO_EXCEPTION = LR_DATA_PARSER_ERROR ).
      RETURN.
  ENDTRY.

  " Insert parsed data to main table
  INSERT LINES OF LT_ACCPOST INTO TABLE ME->T_ACCPOST.


endmethod.


METHOD POST.
  DATA: LS_ACCPOST TYPE LINE OF ME->TT_ACCPOST,
        LT_ACCPOST_AUX TYPE ME->TT_ACCPOST,
        LS_ACCPOST_DOC TYPE LINE OF ME->TT_ACCPOST_DOC.


  " Populate data from clipboard into LT_ACCPOST
  IF IV_CLIPBOARD_DATA = ABAP_TRUE.
    CLEAR ME->T_ACCPOST.
    ME->FILL_T_ACCPOST_WITH_CLIPDATA( ).
  ENDIF.

  " Check number of items to post
  IF LINES( ME->T_ACCPOST ) = 0.
    ME->R_MSG_LOG->ADD(
      EXPORTING
        ID_MSGTY = 'E' ID_MSGID = '00' ID_MSGNO = '398'
        ID_MSGV1 = 'Nothing to post'
    ).
    RETURN.
  ENDIF.

  " Group all items without ID into 'Accounting_Document'. Let ID_ACCDOS be a non-required field
  CLEAR LS_ACCPOST.
  LS_ACCPOST-ID_ACCDOC = 'Accounting_Document'.
  MODIFY ME->T_ACCPOST FROM LS_ACCPOST TRANSPORTING ID_ACCDOC WHERE ID_ACCDOC IS INITIAL.

  " Initialize returning tables and message collector
  CLEAR ME->T_ACCPOST_DOC.
  CLEAR ME->T_ACCPOST_MSG.
  ME->R_MSG_LOG->CLEAR( ).

  " Select distint values of ID_ACCDOC into LT_ACCPOST_AUX
  LT_ACCPOST_AUX[] = ME->T_ACCPOST[].
  SORT LT_ACCPOST_AUX BY ID_ACCDOC.
  DELETE ADJACENT DUPLICATES FROM LT_ACCPOST_AUX COMPARING ID_ACCDOC.

  " Call BAPI for each distinct ID_ACCDOC
  LOOP AT LT_ACCPOST_AUX INTO LS_ACCPOST.

    " Internal ID of document
    ME->N_ACCPOST_TABIX = SY-TABIX.

    CLEAR LS_ACCPOST_DOC.

    LS_ACCPOST_DOC-ID_ACCDOC = LS_ACCPOST-ID_ACCDOC.
    LS_ACCPOST_DOC-LOG_NO = ME->N_ACCPOST_TABIX.
    LS_ACCPOST_DOC-STATUS = ZCL_SIMPLE_ACC_POST=>C_STATUS_NEW.

    ME->FILL_BAPI( CHANGING CS_ACCPOST_DOC = LS_ACCPOST_DOC ).
    ME->CALL_BAPI( EXPORTING IV_CHECK = IV_CHECK CHANGING CS_ACCPOST_DOC = LS_ACCPOST_DOC ).

    APPEND LS_ACCPOST_DOC TO ME->T_ACCPOST_DOC.

  ENDLOOP.

  " Store logs in database
  ME->R_MSG_LOG->STORE(
    EXCEPTIONS
      ERROR             = 1
      OTHERS            = 2
  ).
  IF SY-SUBRC <> 0.
  ENDIF.



ENDMETHOD.


METHOD READ_CLIPBOARD.

  CONSTANTS:
    C_TAB  LIKE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,
    C_CRLF LIKE CL_ABAP_CHAR_UTILITIES=>CR_LF          VALUE CL_ABAP_CHAR_UTILITIES=>CR_LF.
  TYPES:
    BEGIN OF TY_CLIPDATA,
        DATA TYPE C LENGTH 500,
       END OF TY_CLIPDATA.
  DATA: LT_CLIPDATA TYPE STANDARD TABLE OF TY_CLIPDATA,
        LW_CLIPDATA LIKE LINE OF LT_CLIPDATA,
        LN_CLIP_LEN TYPE I.

  " Read clipboard
  CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_IMPORT(
    IMPORTING
       DATA                 = LT_CLIPDATA
       LENGTH               = LN_CLIP_LEN
     EXCEPTIONS
       CNTL_ERROR           = 1
       ERROR_NO_GUI         = 2
       NOT_SUPPORTED_BY_GUI = 3
       OTHERS               = 4 ).

  IF SY-SUBRC NE 0.
    ME->R_MSG_LOG->ADD_SYMSG( ).
    RETURN.
  ENDIF.

  " Concatenate lines into RS_CLIPDATA separated by carriage return
  LOOP AT LT_CLIPDATA INTO LW_CLIPDATA.
    CONCATENATE RV_CLIPDATA LW_CLIPDATA C_CRLF INTO RV_CLIPDATA.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.