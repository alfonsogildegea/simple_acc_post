*&---------------------------------------------------------------------*
*& Report  ZTEST_SIMPLE_ACC_POST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZTEST_SIMPLE_ACC_POST.

*&---------------------------------------------------------------------*
*&       Class LCL_MAIN
*&---------------------------------------------------------------------*
*        Main class
*----------------------------------------------------------------------*
CLASS LCL_MAIN DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      BTN_DOWNLOAD_TEMPLATE,  " Download excel template from server
      BTN_VIEW_TEMPLATE,      " View excel template from server
      BTN_CB_C_D,             " Check+Display results
      BTN_CB_P_D,             " Post+Display results
      BTN_CB_P_D_S,           " Post+Display+Store DB
      BTN_CB_C_P_L,           " Check+Post+Log
      BTN_CB_C_P_L_S,         " Check+Post+Log+Store DB
      BTN_CB_BDC_BI.          " Create Batch Input with BDC

  PRIVATE SECTION.
    CLASS-METHODS:
      FILL_ZSIMPLE_ACC_POST IMPORTING IR_SIMPLE_ACC_POST TYPE REF TO ZCL_SIMPLE_ACC_POST.  " Fill and display Z-table

ENDCLASS.               "LCL_MAIN


*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_MAIN
*&---------------------------------------------------------------------*
*        Main class
*----------------------------------------------------------------------*
CLASS LCL_MAIN IMPLEMENTATION.


  METHOD BTN_DOWNLOAD_TEMPLATE.
    DATA: LS_WWWDATA_TAB TYPE WWWDATATAB.

    " Make sure template exists in SMW0 as binary object
    SELECT SINGLE *
      FROM WWWDATA INNER JOIN TADIR ON WWWDATA~OBJID = TADIR~OBJ_NAME
      INTO CORRESPONDING FIELDS OF LS_WWWDATA_TAB
     WHERE WWWDATA~SRTF2  = 0
       AND WWWDATA~RELID  = 'MI'
       AND TADIR~PGMID    = 'R3TR'
       AND TADIR~OBJECT   = 'W3MI'
       AND TADIR~OBJ_NAME = 'ZSIMPLE_ACC_POST_TEMPLATE'.

    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
        EXPORTING
          TEXTID = ZCX_SIMPLE_ACC_POST=>ERROR
          ATTR1  = 'Template not found in SMW0'
          ATTR2  = 'as binary object'
          ATTR3  = 'ZSIMPLE_ACC_POST_TEMPLATE'.
    ENDIF.

    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        KEY = LS_WWWDATA_TAB.

  ENDMETHOD.                    "BTN_DOWNLOAD_TEMPLATE


  METHOD BTN_VIEW_TEMPLATE.
    DATA: LS_WWWDATA_TAB TYPE WWWDATATAB.

    " Make sure template exists in SMW0 as binary object
    SELECT SINGLE *
      FROM WWWDATA INNER JOIN TADIR ON WWWDATA~OBJID = TADIR~OBJ_NAME
      INTO CORRESPONDING FIELDS OF LS_WWWDATA_TAB
     WHERE WWWDATA~SRTF2  = 0
       AND WWWDATA~RELID  = 'MI'
       AND TADIR~PGMID    = 'R3TR'
       AND TADIR~OBJECT   = 'W3MI'
       AND TADIR~OBJ_NAME = 'ZSIMPLE_ACC_POST_TEMPLATE'.

    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
        EXPORTING
          TEXTID = ZCX_SIMPLE_ACC_POST=>ERROR
          ATTR1  = 'Template not found in SMW0'
          ATTR2  = 'as binary object'
          ATTR3  = 'ZSIMPLE_ACC_POST_TEMPLATE'.
    ENDIF.

    CALL FUNCTION 'SHOW_WEB_OBJECT'
      EXPORTING
        KEY               = LS_WWWDATA_TAB
      EXCEPTIONS
        CANCELED_BY_USER  = 2
        PROGRAM_NOT_FOUND = 3.

    IF SY-SUBRC <> 0.
    ENDIF.

  ENDMETHOD.                    "BTN_VIEW_TEMPLATE


  METHOD BTN_CB_P_D.
    DATA: LR_SIMPLE_ACC_POST TYPE REF TO ZCL_SIMPLE_ACC_POST.

    CREATE OBJECT LR_SIMPLE_ACC_POST.

    LR_SIMPLE_ACC_POST->POST(
      EXPORTING
        IV_CHECK          = ABAP_FALSE    " Only check posting
        IV_CLIPBOARD_DATA = ABAP_TRUE     " Post clipboard data
    ).

    LR_SIMPLE_ACC_POST->DISPLAY_RESULT( ).

  ENDMETHOD.                    "BTN_CB_P_D


  METHOD BTN_CB_P_D_S.
    DATA: LC_ANSWER TYPE C LENGTH 1,
          LR_SIMPLE_ACC_POST TYPE REF TO ZCL_SIMPLE_ACC_POST.

    CREATE OBJECT LR_SIMPLE_ACC_POST.

    LR_SIMPLE_ACC_POST->POST(
      EXPORTING
        IV_CHECK          = ABAP_FALSE    " Only check posting
        IV_CLIPBOARD_DATA = ABAP_TRUE     " Post clipboard data
    ).

    LR_SIMPLE_ACC_POST->DISPLAY_RESULT( ).

    " Store items into ZSIMPLE_ACC_POST
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TEXT_QUESTION = 'Do you like store items into ZSIMPLE_ACC_POST?'
      IMPORTING
        ANSWER        = LC_ANSWER.

    IF LC_ANSWER = '1'. " Yes
      LCL_MAIN=>FILL_ZSIMPLE_ACC_POST( IR_SIMPLE_ACC_POST = LR_SIMPLE_ACC_POST ).
    ENDIF.

  ENDMETHOD.                    "BTN_CB_P_D_S


  METHOD BTN_CB_C_D.
    DATA: LR_SIMPLE_ACC_POST TYPE REF TO ZCL_SIMPLE_ACC_POST.

    CREATE OBJECT LR_SIMPLE_ACC_POST.

    LR_SIMPLE_ACC_POST->POST(
      EXPORTING
        IV_CHECK          = ABAP_TRUE    " Only check posting
        IV_CLIPBOARD_DATA = ABAP_TRUE    " Post clipboard data
    ).

    " if exists error display log
    IF LR_SIMPLE_ACC_POST->R_MSG_LOG->HAS_MESSAGES_OF_MSGTY( ID_MSGTY = 'E' IF_OR_HIGHER = ABAP_TRUE ) = ABAP_TRUE.
      LR_SIMPLE_ACC_POST->DISPLAY_LOG( ).
    ELSE.
      LR_SIMPLE_ACC_POST->DISPLAY_RESULT( ).
    ENDIF.

  ENDMETHOD.                    "BTN_CB_C_D


  METHOD BTN_CB_C_P_L.
    DATA: LC_ANSWER TYPE C LENGTH 1,
          LR_SIMPLE_ACC_POST TYPE REF TO ZCL_SIMPLE_ACC_POST.

    " Create main object
    CREATE OBJECT LR_SIMPLE_ACC_POST.

    " Check clipboard data. First line must be field names. (See ZFIS_SIMPLE_ACCPOST)
    LR_SIMPLE_ACC_POST->POST(
      EXPORTING
        IV_CHECK          = ABAP_TRUE    " Only check posting
        IV_CLIPBOARD_DATA = ABAP_TRUE    " Post clipboard data
    ).
    LR_SIMPLE_ACC_POST->DISPLAY_LOG( ).

    " If no errors messages, make real post.
    IF LR_SIMPLE_ACC_POST->R_MSG_LOG->HAS_MESSAGES_OF_MSGTY( ID_MSGTY = 'E' IF_OR_HIGHER = ABAP_TRUE ) = ABAP_FALSE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION = 'Post documents?'
        IMPORTING
          ANSWER        = LC_ANSWER.

      IF LC_ANSWER = '1'. " Yes

        " Post clipboard data. First line must be field names. (See ZFIS_SIMPLE_ACCPOST)
        LR_SIMPLE_ACC_POST->POST( EXPORTING IV_CLIPBOARD_DATA = ABAP_TRUE ).
        LR_SIMPLE_ACC_POST->DISPLAY_LOG( ).

*       IF SY-UNAME = 'SIT9935'.
*         ZCX_FI=>POPUP_TABLA( EXPORTING I_HEADER = 'ME->T_ACCPOST'     CHANGING CT_TABLA = GR_MAIN->R_SIMPLE_ACC_POST->T_ACCPOST ).
*         ZCX_FI=>POPUP_TABLA( EXPORTING I_HEADER = 'ME->T_ACCPOST_DOC' CHANGING CT_TABLA = GR_MAIN->R_SIMPLE_ACC_POST->T_ACCPOST_DOC ).
*         ZCX_FI=>POPUP_TABLA( EXPORTING I_HEADER = 'ME->T_ACCPOST_MSG' CHANGING CT_TABLA = GR_MAIN->R_SIMPLE_ACC_POST->T_ACCPOST_MSG ).
*       ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "BTN_CB_C_P_L


  METHOD BTN_CB_C_P_L_S.
    DATA: LR_SIMPLE_ACC_POST TYPE REF TO ZCL_SIMPLE_ACC_POST,
          LC_ANSWER TYPE C LENGTH 1,
          LS_ACCPOST TYPE LINE OF ZCL_SIMPLE_ACC_POST=>TT_ACCPOST.


    " Create main object
    CREATE OBJECT LR_SIMPLE_ACC_POST.

    " Check clipboard data. First line must be field names. (See ZFIS_SIMPLE_ACCPOST)
    LR_SIMPLE_ACC_POST->POST(
      EXPORTING
        IV_CHECK          = ABAP_TRUE    " Only check posting
        IV_CLIPBOARD_DATA = ABAP_TRUE    " Post clipboard data
    ).
    LR_SIMPLE_ACC_POST->DISPLAY_LOG( ).

    " If no errors messages, make real post.
    IF LR_SIMPLE_ACC_POST->R_MSG_LOG->HAS_MESSAGES_OF_MSGTY( ID_MSGTY = 'E' IF_OR_HIGHER = ABAP_TRUE ) = ABAP_FALSE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION = 'Post documents?'
        IMPORTING
          ANSWER        = LC_ANSWER.

      IF LC_ANSWER = '1'. " Yes

        " Post clipboard data. First line must be field names. (See ZFIS_SIMPLE_ACCPOST)
        LR_SIMPLE_ACC_POST->POST( EXPORTING IV_CLIPBOARD_DATA = ABAP_TRUE ).
        LR_SIMPLE_ACC_POST->DISPLAY_LOG( ).

*       IF SY-UNAME = 'SIT9935'.
*         ZCX_FI=>POPUP_TABLA( EXPORTING I_HEADER = 'ME->T_ACCPOST'     CHANGING CT_TABLA = GR_MAIN->R_SIMPLE_ACC_POST->T_ACCPOST ).
*         ZCX_FI=>POPUP_TABLA( EXPORTING I_HEADER = 'ME->T_ACCPOST_DOC' CHANGING CT_TABLA = GR_MAIN->R_SIMPLE_ACC_POST->T_ACCPOST_DOC ).
*         ZCX_FI=>POPUP_TABLA( EXPORTING I_HEADER = 'ME->T_ACCPOST_MSG' CHANGING CT_TABLA = GR_MAIN->R_SIMPLE_ACC_POST->T_ACCPOST_MSG ).
*       ENDIF.
      ENDIF.

    ENDIF.

    " Store items into ZSIMPLE_ACC_POST
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TEXT_QUESTION = 'Do you like store items into ZSIMPLE_ACC_POST?'
      IMPORTING
        ANSWER        = LC_ANSWER.

    IF LC_ANSWER = '1'. " Yes
      LCL_MAIN=>FILL_ZSIMPLE_ACC_POST( IR_SIMPLE_ACC_POST = LR_SIMPLE_ACC_POST ).
    ENDIF.

  ENDMETHOD.                    "BTN_CB_C_P_L_S


  METHOD FILL_ZSIMPLE_ACC_POST.
    DATA: LS_ZSIMPLE_ACC_POST TYPE ZSIMPLE_ACC_POST,
          LV_ID TYPE N LENGTH 10,
          LS_ACCPOST TYPE LINE OF ZCL_SIMPLE_ACC_POST=>TT_ACCPOST.


    " Get next ID. (Max+1 for test purposes only. This should be a object of SNUM on production environment)
    SELECT MAX( ID ) FROM ZSIMPLE_ACC_POST INTO LV_ID.
    IF LV_ID IS INITIAL.
      LV_ID = '0000000001'.
    ELSE.
      ADD 1 TO LV_ID.
    ENDIF.

    " Loop items
    LOOP AT IR_SIMPLE_ACC_POST->T_ACCPOST INTO LS_ACCPOST.

      CLEAR LS_ZSIMPLE_ACC_POST.

      LS_ZSIMPLE_ACC_POST-ID = LV_ID.
      LS_ZSIMPLE_ACC_POST-NO_ITEM = SY-TABIX.
      MOVE-CORRESPONDING LS_ACCPOST TO LS_ZSIMPLE_ACC_POST.

      INSERT INTO ZSIMPLE_ACC_POST VALUES LS_ZSIMPLE_ACC_POST.

      IF SY-SUBRC <> 0.
        RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
          EXPORTING
            TEXTID = ZCX_SIMPLE_ACC_POST=>ERROR
            ATTR1  = 'Error inserting on ZSIMPLE_ACC_POST'
            ATTR2  = |{ SY-SUBRC }|
            ATTR3  = |{ LV_ID }|
            ATTR4  = |{ SY-TABIX }|.
      ENDIF.

    ENDLOOP.

    " Show items stored
    DATA: LT_SELFIELDS TYPE STANDARD TABLE OF SE16N_SELTAB,
          LS_SELFIELDS TYPE SE16N_SELTAB.

    CLEAR LS_SELFIELDS.

    LS_SELFIELDS-FIELD = 'ID'.
    LS_SELFIELDS-SIGN = 'I'.
    LS_SELFIELDS-OPTION = 'EQ'.
    LS_SELFIELDS-LOW = LV_ID.

    APPEND LS_SELFIELDS TO LT_SELFIELDS.

    CALL FUNCTION 'SE16N_INTERFACE'
      EXPORTING
        I_TAB        = 'ZSIMPLE_ACC_POST'
      TABLES
        IT_SELFIELDS = LT_SELFIELDS
      EXCEPTIONS
        NO_VALUES    = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.                    "FILL_ZSIMPLE_ACC_POST


  METHOD BTN_CB_BDC_BI.
    DATA: LR_SIMPLE_ACC_POST TYPE REF TO ZCL_SIMPLE_ACC_POST.

    CREATE OBJECT LR_SIMPLE_ACC_POST.

    LR_SIMPLE_ACC_POST->BDC_POST(
      EXPORTING
        IV_CLIPBOARD_DATA = ABAP_TRUE     " Post clipboard data
    ).

    LR_SIMPLE_ACC_POST->DISPLAY_LOG( ).

  ENDMETHOD.                    "BTN_CB_BDC_BI


ENDCLASS.               "LCL_MAIN


" -------------------------------------------------------------------------------------------------------------------------------------------------------------
" Selections screen -------------------------------------------------------------------------------------------------------------------------------------------
" -------------------------------------------------------------------------------------------------------------------------------------------------------------
TABLES: SSCRFIELDS.
INCLUDE: <ICON>.
DATA: GS_SEL_BUTTON TYPE SMP_DYNTXT.

SELECTION-SCREEN: BEGIN OF SCREEN 100.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE B1_TITLE.
SELECTION-SCREEN PUSHBUTTON /2(40) B_CBCD   USER-COMMAND BTN_CB_C_D.
SELECTION-SCREEN PUSHBUTTON /2(40) B_CBPD   USER-COMMAND BTN_CB_P_D.
SELECTION-SCREEN PUSHBUTTON /2(40) B_CBPDS  USER-COMMAND BTN_CB_P_D_S.
SELECTION-SCREEN PUSHBUTTON /2(40) B_CBCPL  USER-COMMAND BTN_CB_C_P_L.
SELECTION-SCREEN PUSHBUTTON /2(40) B_CBCPLS USER-COMMAND BTN_CB_C_P_L_S.
SELECTION-SCREEN PUSHBUTTON /2(40) B_CBBDC1 USER-COMMAND BTN_CB_BDC_BI.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN: END OF SCREEN 100.

AT SELECTION-SCREEN OUTPUT.

  GS_SEL_BUTTON-ICON_ID = ICON_EXPORT.
  GS_SEL_BUTTON-ICON_TEXT = 'Download template'.
  SSCRFIELDS-FUNCTXT_01 = GS_SEL_BUTTON.

  GS_SEL_BUTTON-ICON_ID = ICON_DISPLAY.
  GS_SEL_BUTTON-ICON_TEXT = 'View template'.
  SSCRFIELDS-FUNCTXT_02 = GS_SEL_BUTTON.

  B1_TITLE = 'Clipboard'.
  B_CBCD   = 'Check + Dialog'.
  B_CBPD   = 'Post + Dialog'.
  B_CBPDS  = 'Post + Dialog + Store DB'.
  B_CBCPL  = 'Check + Post + Log'.
  B_CBCPLS = 'Check + Post + Log + Store DB'.
  B_CBBDC1 = 'BDC + BI'.

AT SELECTION-SCREEN.
  IF SY-DYNNR = '0100'.
    CASE SSCRFIELDS-UCOMM.
      WHEN 'FC01'.
        LCL_MAIN=>BTN_DOWNLOAD_TEMPLATE( ).
      WHEN 'FC02'.
        LCL_MAIN=>BTN_VIEW_TEMPLATE( ).
      WHEN 'BTN_CB_C_D'.
        LCL_MAIN=>BTN_CB_C_D( ).
      WHEN 'BTN_CB_P_D'.
        LCL_MAIN=>BTN_CB_P_D( ).
      WHEN 'BTN_CB_P_D_S'.
        LCL_MAIN=>BTN_CB_P_D_S( ).
      WHEN 'BTN_CB_C_P_L'.
        LCL_MAIN=>BTN_CB_C_P_L( ).
      WHEN 'BTN_CB_C_P_L_S'.
        LCL_MAIN=>BTN_CB_C_P_L_S( ).
      WHEN 'BTN_CB_BDC_BI'.
        LCL_MAIN=>BTN_CB_BDC_BI( ).
      WHEN OTHERS.
    ENDCASE.
  ENDIF.


  " -------------------------------------------------------------------------------------------------------------------------------------------------------------
  " Main --------------------------------------------------------------------------------------------------------------------------------------------------------
  " -------------------------------------------------------------------------------------------------------------------------------------------------------------

START-OF-SELECTION.

  DATA: GR_CX_ROOT TYPE REF TO CX_ROOT.

  TRY.

      " Only show buttons
      CALL SELECTION-SCREEN 100.

    CATCH CX_ROOT INTO GR_CX_ROOT.
      DATA: LC_MESSAGE TYPE STRING.
      LC_MESSAGE = GR_CX_ROOT->GET_LONGTEXT( ).
*      MESSAGE LC_MESSAGE TYPE 'E'.
      BREAK-POINT.
      LEAVE PROGRAM.
  ENDTRY.
