*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

" --------------------------------------------------------------------------------------------------------------------------------------------
" https://github.com/sbcgua/abap_data_parser
" TAB-delimited text parser for ABAP
" Abap data parser is an utility to parse TAB-delimited text into an internal table of an arbitrary flat structure.
" It support "unstrict" mode which allows to skip fields in the source data (for the case when only certain fields are being loaded).
" It supports "header" specification as the first line in the text - in this case field order in the text may differ from the
" internal abap structure field order. It also supports loading into a structure (the first data line of the text is parsed).

INCLUDE ZDATA_PARSER_CLASS.
" --------------------------------------------------------------------------------------------------------------------------------------------


" --------------------------------------------------------------------------------------------------------------------------------------------
TYPE-POOLS: ICON. "Iconos
CLASS LCL_RESULT_EVENTS DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*&       Class LCL_RESULT DEFINITION
*&---------------------------------------------------------------------*
*        Utility class to show posting details in a new dialog box.
*
*        This window has 3 tables:
*        - At top-left: A table with status of all documents (distinct ID_ACCDOC)
*        - At top-right: A table with parsed clipboard data
*        - At botton: A table with all messages returned by BAPI_ACC_DOCUMENT_POST
*----------------------------------------------------------------------*
CLASS LCL_RESULT DEFINITION.
  PUBLIC SECTION.
    TYPES:
      TT_ACCPOST TYPE STANDARD TABLE OF ZFIS_SIMPLE_ACCPOST.

    DATA:
      T_ACCPOST_DOC     TYPE ZCL_SIMPLE_ACC_POST=>TT_ACCPOST_DOC,
      T_ACCPOST         TYPE TT_ACCPOST,
      T_ACCPOST_MSG     TYPE BAPIRET2_T,
      R_ALV_ACCPOST_DOC TYPE REF TO CL_SALV_TABLE,
      R_ALV_ACCPOST     TYPE REF TO CL_SALV_TABLE,
      R_ALV_ACCPOST_MSG TYPE REF TO CL_SALV_TABLE.

    CONSTANTS:
      C_DIALOG_WIDTH  TYPE I VALUE '1500',    " Width of dialog box
      C_DIALOG_HEIGHT TYPE I VALUE '400'.     " Height of dialog box

    METHODS:
      CONSTRUCTOR
        IMPORTING IT_ACCPOST_DOC TYPE ZCL_SIMPLE_ACC_POST=>TT_ACCPOST_DOC
                  IT_ACCPOST     TYPE TT_ACCPOST
                  IT_ACCPOST_MSG TYPE BAPIRET2_T,
      DISPLAY.

  PRIVATE SECTION.
    DATA:
      R_ALV_EVENTS      TYPE REF TO LCL_RESULT_EVENTS,
      R_DIALOG          TYPE REF TO CL_GUI_DIALOGBOX_CONTAINER,   " Ventana principal
      R_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,    " Splitter top-bottom
      R_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,    " Splitter left-right.
      R_CELL_TOP        TYPE REF TO CL_GUI_CONTAINER,             " Top cell of splitter 1
      R_CELL_TOP_LEFT   TYPE REF TO CL_GUI_CONTAINER,             " Left cell of splitter 2 (R_ALV_ACCPOST_DOC)
      R_CELL_TOP_RIGHT  TYPE REF TO CL_GUI_CONTAINER,             " Right cell of splitter 2 (R_ALV_ACCPOST)
      R_CELL_BOTTOM     TYPE REF TO CL_GUI_CONTAINER.             " Bottom cell of splitter 1 (R_ALV_ACCPOST_MSG)

    METHODS:
      ADD_ALV_ACCPOST_DOC IMPORTING IR_CONTAINER TYPE REF TO CL_GUI_CONTAINER,
      ADD_ALV_ACCPOST     IMPORTING IR_CONTAINER TYPE REF TO CL_GUI_CONTAINER,
      ADD_ALV_ACCPOST_MSG IMPORTING IR_CONTAINER TYPE REF TO CL_GUI_CONTAINER.

ENDCLASS.               "LCL_RESULT


*&---------------------------------------------------------------------*
*&       Class LCL_RESULT_EVENTS DEFINITION
*&---------------------------------------------------------------------*
*        Manage events of ALVs
*----------------------------------------------------------------------*
CLASS LCL_RESULT_EVENTS DEFINITION.
  PUBLIC SECTION.

    DATA:
      R_RESULT TYPE REF TO LCL_RESULT.

    METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN,
      ON_LINK_CLICK FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN.
*      ON_LINK_CLICK FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TREE
*        IMPORTING NODE_KEY COLUMNNAME SENDER,
*      ON_KEYPRESS FOR EVENT KEYPRESS OF CL_SALV_EVENTS_TREE
*        IMPORTING NODE_KEY COLUMNNAME KEY,
*      ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
*        IMPORTING E_SALV_FUNCTION.
*      ON_BEFORE_USER_COMMAND FOR EVENT BEFORE_SALV_FUNCTION OF CL_SALV_EVENTS
*        IMPORTING E_SALV_FUNCTION,
*      ON_AFTER_USER_COMMAND FOR EVENT AFTER_SALV_FUNCTION OF CL_SALV_EVENTS
*        IMPORTING E_SALV_FUNCTION.

ENDCLASS.               "LCL_MAIN_EVENTS


*---------------------------------------------------------------------*
*       CLASS LCL_RESULT_EVENTS IMPLEMENTATION
*---------------------------------------------------------------------*
*       Manage events of ALVs
*---------------------------------------------------------------------*
CLASS LCL_RESULT_EVENTS IMPLEMENTATION.

  METHOD ON_DOUBLE_CLICK.
    DATA: LS_ACCPOST_DOC TYPE ZCL_SIMPLE_ACC_POST=>TY_ACCPOST_DOC,
          LS_LOW_ACCPOST TYPE SALV_DE_SELOPT_LOW,
          LS_LOW_BAPIRET TYPE SALV_DE_SELOPT_LOW.

    " Retrieve data row
    READ TABLE ME->R_RESULT->T_ACCPOST_DOC INTO LS_ACCPOST_DOC INDEX ROW.
    LS_LOW_BAPIRET = LS_ACCPOST_DOC-LOG_NO.
    LS_LOW_ACCPOST = LS_ACCPOST_DOC-ID_ACCDOC.

    IF LS_ACCPOST_DOC IS INITIAL.
      RETURN.
    ENDIF.

    " Clear filters of messages
    DATA: LO_FILTERS TYPE REF TO CL_SALV_FILTERS.
    LO_FILTERS = ME->R_RESULT->R_ALV_ACCPOST_MSG->GET_FILTERS( ).
    LO_FILTERS->CLEAR( ).

    " Add filter with related messages
    TRY .

        LO_FILTERS->ADD_FILTER(
          EXPORTING
            COLUMNNAME = 'LOG_NO'          " ALV Control: Field Name of Internal Table Field
            SIGN       = 'I'               " Selection Condition Sign
            OPTION     = 'EQ'              " Selection Condition Option
            LOW        = LS_LOW_BAPIRET    " Lower Value of Selection Condition
        ).

      CATCH CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR CX_SALV_EXISTING.
    ENDTRY.

    " Apply filter
    ME->R_RESULT->R_ALV_ACCPOST_MSG->REFRESH(
      EXPORTING
        REFRESH_MODE = IF_SALV_C_REFRESH=>FULL
    ).


    " Clear filters of items
    DATA: LO_FILTERS2 TYPE REF TO CL_SALV_FILTERS.
    LO_FILTERS2 = ME->R_RESULT->R_ALV_ACCPOST->GET_FILTERS( ).
    LO_FILTERS2->CLEAR( ).

    TRY .

        LO_FILTERS2->ADD_FILTER(
          EXPORTING
            COLUMNNAME = 'ID_ACCDOC'       " ALV Control: Field Name of Internal Table Field
            SIGN       = 'I'               " Selection Condition Sign
            OPTION     = 'EQ'              " Selection Condition Option
            LOW        = LS_LOW_ACCPOST    " Lower Value of Selection Condition
        ).

      CATCH CX_SALV_NOT_FOUND CX_SALV_DATA_ERROR CX_SALV_EXISTING.
    ENDTRY.

    " Apply filter
    ME->R_RESULT->R_ALV_ACCPOST->REFRESH(
      EXPORTING
        REFRESH_MODE = IF_SALV_C_REFRESH=>FULL
    ).


  ENDMETHOD.                    "ON_DOUBLE_CLICK


  METHOD ON_LINK_CLICK.
    DATA LS_ACCPOST_DOC TYPE ZCL_SIMPLE_ACC_POST=>TY_ACCPOST_DOC.

    " Retrieve row data
    READ TABLE ME->R_RESULT->T_ACCPOST_DOC INTO LS_ACCPOST_DOC INDEX ROW.

    CASE COLUMN.
      WHEN 'ID_ACCDOC'.
        IF LS_ACCPOST_DOC-STATUS = ZCL_SIMPLE_ACC_POST=>C_STATUS_OK OR LS_ACCPOST_DOC-STATUS = ZCL_SIMPLE_ACC_POST=>C_STATUS_WARNING.
          CALL FUNCTION 'BAPI_ACC_DOCUMENT_DISPLAY'
            EXPORTING
              OBJ_TYPE = LS_ACCPOST_DOC-AWTYP
              OBJ_KEY  = LS_ACCPOST_DOC-AWKEY
              OBJ_SYS  = LS_ACCPOST_DOC-AWSYS.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "ON_LINK_CLICK


ENDCLASS.                    "LCL_RESULT_EVENTS IMPLEMENTATION


*---------------------------------------------------------------------*
*       CLASS LCL_RESULT IMPLEMENTATION
*---------------------------------------------------------------------*
*       Muestra el RESULT de ZCL_CONTABILIZA
*---------------------------------------------------------------------*
CLASS LCL_RESULT IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ME->T_ACCPOST_DOC   = IT_ACCPOST_DOC.
    ME->T_ACCPOST       = IT_ACCPOST.
    ME->T_ACCPOST_MSG   = IT_ACCPOST_MSG.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD DISPLAY.

    " Create Dialog box
    CREATE OBJECT R_DIALOG
      EXPORTING
        PARENT                      = CL_GUI_CONTAINER=>SCREEN0    " Parent container
*       PARENT                      = CL_GUI_CONTAINER=>DESKTOP    " Parent container
        WIDTH                       = LCL_RESULT=>C_DIALOG_WIDTH    " Width of This Container
        HEIGHT                      = LCL_RESULT=>C_DIALOG_HEIGHT     " Height of This Container
*       STYLE                       = cl_gui_control=>WS_MAXIMIZEBOX    " Windows Style Attributes Applied to this Container
*       REPID                       = SY-REPI2    " Report to Which This Control is Linked
*       DYNNR                       = sy-DYNNR    " Screen to Which the Control is Linked
*       LIFETIME                    = CL_GUI_CONTAINER=>LIFETIME_DEFAULT    " Lifetime
        TOP                         = 25    " Top Position of Dialog Box
        LEFT                        = 50    " Left Position of Dialog Box
        CAPTION                     = 'Posting details'    " Dialog Box Caption
*       NO_AUTODEF_PROGID_DYNNR     =     " Don't Autodefined Progid and Dynnr?
*       METRIC                      = 0    " Metric
*       NAME                        =     " Name
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        EVENT_ALREADY_REGISTERED    = 6
        ERROR_REGIST_EVENT          = 7
        OTHERS                      = 8.
    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
        EXPORTING
          TEXTID = ZCX_SIMPLE_ACC_POST=>ERROR
          ATTR1  = 'Error creating R_DIALOG'.
    ENDIF.

    " Activate "X" by default to close dialog box
    R_DIALOG->REG_EVENT_CLOSE( REGISTER  = 0 ).   " True = Register, False = Deregister

    " 2 rows splitter
    CREATE OBJECT R_SPLITTER_1
      EXPORTING
        PARENT                  = R_DIALOG
        ROWS                    = 2
        COLUMNS                 = 1
*       NO_AUTODEF_PROGID_DYNNR =
*       NAME                    =
      EXCEPTIONS
        CNTL_ERROR              = 1
        CNTL_SYSTEM_ERROR       = 2
        OTHERS                  = 3.

    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
        EXPORTING
          TEXTID = ZCX_SIMPLE_ACC_POST=>ERROR
          ATTR1  = 'Error creating R_SPLITTER_1'.
    ENDIF.

    " Reference to TOP cell (Documents and items)
    CALL METHOD R_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = R_CELL_TOP.

    " 2 columns splitter (Documents at left and items at right)
    CREATE OBJECT R_SPLITTER_2
      EXPORTING
        PARENT                  = R_CELL_TOP
        ROWS                    = 1
        COLUMNS                 = 2
*       NO_AUTODEF_PROGID_DYNNR =
*       NAME                    =
      EXCEPTIONS
        CNTL_ERROR              = 1
        CNTL_SYSTEM_ERROR       = 2
        OTHERS                  = 3.

    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
        EXPORTING
          TEXTID = ZCX_SIMPLE_ACC_POST=>ERROR
          ATTR1  = 'Error creating R_SPLITTER_2'.
    ENDIF.

    " Add ALV of documents to top-left cell
    IF LINES( ME->T_ACCPOST_DOC ) > 0.

      CALL METHOD R_SPLITTER_2->GET_CONTAINER
        EXPORTING
          ROW       = 1
          COLUMN    = 1
        RECEIVING
          CONTAINER = R_CELL_TOP_LEFT.

      ME->ADD_ALV_ACCPOST_DOC( IR_CONTAINER = R_CELL_TOP_LEFT ).

    ENDIF.

    " Add ALV of items to top-right cell
    IF LINES( ME->T_ACCPOST ) > 0.

      CALL METHOD R_SPLITTER_2->GET_CONTAINER
        EXPORTING
          ROW       = 1
          COLUMN    = 2
        RECEIVING
          CONTAINER = R_CELL_TOP_RIGHT.

      ME->ADD_ALV_ACCPOST( IR_CONTAINER = R_CELL_TOP_RIGHT ).

    ELSE.
      R_SPLITTER_2->SET_COLUMN_WIDTH( ID = 1 WIDTH = 100 ).
    ENDIF.

    " Add ALV of BAPI messages to buttom cell
    IF LINES( ME->T_ACCPOST_MSG ) > 0.

      CALL METHOD R_SPLITTER_1->GET_CONTAINER
        EXPORTING
          ROW       = 2
          COLUMN    = 1
        RECEIVING
          CONTAINER = R_CELL_BOTTOM. " Tabla de log BAPIRET2

      ME->ADD_ALV_ACCPOST_MSG( IR_CONTAINER = R_CELL_BOTTOM ).

      R_SPLITTER_1->SET_ROW_HEIGHT( ID = 1 HEIGHT = 80 ).
    ELSE.
      R_SPLITTER_1->SET_ROW_HEIGHT( ID = 1 HEIGHT = 100 ).
    ENDIF.


    " Refresh window
    CALL METHOD CL_GUI_CFW=>FLUSH
      EXCEPTIONS
        CNTL_SYSTEM_ERROR = 1
        CNTL_ERROR        = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
        EXPORTING
          TEXTID = ZCX_SIMPLE_ACC_POST=>ERROR
          ATTR1  = 'Error creating R_SPLITTER_2'.
    ENDIF.

  ENDMETHOD.                    "DISPLAY


  METHOD ADD_ALV_ACCPOST.
    DATA: LR_CX_SALV_ERROR TYPE REF TO CX_SALV_ERROR.


    TRY .

        " Create ALV on container
        CL_SALV_TABLE=>FACTORY(
            EXPORTING
              R_CONTAINER  = IR_CONTAINER
            IMPORTING
              R_SALV_TABLE = ME->R_ALV_ACCPOST
            CHANGING
              T_TABLE      = ME->T_ACCPOST
          ).

        " Columns
        DATA LR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.
        LR_COLUMNS = ME->R_ALV_ACCPOST->GET_COLUMNS( ).
        LR_COLUMNS->SET_OPTIMIZE( ).

        " Functions
        DATA LR_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST.
        LR_FUNCTIONS = ME->R_ALV_ACCPOST->GET_FUNCTIONS( ).
        LR_FUNCTIONS->SET_ALL( ).

        " Display
        ME->R_ALV_ACCPOST->DISPLAY( ).

      CATCH CX_SALV_MSG CX_SALV_EXISTING CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND INTO LR_CX_SALV_ERROR .
        RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
          EXPORTING
            TEXTID   = ZCX_SIMPLE_ACC_POST=>ERROR
            PREVIOUS = LR_CX_SALV_ERROR
            ATTR1    = 'Error creating R_ALV_ACCPOST'.
    ENDTRY.


  ENDMETHOD.                    "ALV_ACCPOST


  METHOD ADD_ALV_ACCPOST_DOC.
    DATA: LR_CX_SALV_ERROR TYPE REF TO CX_SALV_ERROR,
          LS_ACCPOST_DOC TYPE ZCL_SIMPLE_ACC_POST=>TY_ACCPOST_DOC.


    TRY .

        " Set icon status
        LOOP AT ME->T_ACCPOST_DOC INTO LS_ACCPOST_DOC.

          CASE LS_ACCPOST_DOC-STATUS.
            WHEN ZCL_SIMPLE_ACC_POST=>C_STATUS_NEW.
              LS_ACCPOST_DOC-ICON_STS = ICON_LIGHT_OUT.
            WHEN ZCL_SIMPLE_ACC_POST=>C_STATUS_OK.
              LS_ACCPOST_DOC-ICON_STS = ICON_GREEN_LIGHT.
            WHEN ZCL_SIMPLE_ACC_POST=>C_STATUS_WARNING.
              LS_ACCPOST_DOC-ICON_STS = ICON_YELLOW_LIGHT.
            WHEN ZCL_SIMPLE_ACC_POST=>C_STATUS_ERROR.
              LS_ACCPOST_DOC-ICON_STS = ICON_RED_LIGHT.
          ENDCASE.

          MODIFY ME->T_ACCPOST_DOC FROM LS_ACCPOST_DOC.

        ENDLOOP.


        " Create ALV on container
        CL_SALV_TABLE=>FACTORY(
            EXPORTING
              R_CONTAINER  = IR_CONTAINER
            IMPORTING
              R_SALV_TABLE = ME->R_ALV_ACCPOST_DOC
            CHANGING
              T_TABLE      = ME->T_ACCPOST_DOC
          ).

        " Columns
        DATA:
          LO_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE,
          LO_COLUMN TYPE REF TO CL_SALV_COLUMN_TABLE.

        LO_COLUMNS = ME->R_ALV_ACCPOST_DOC->GET_COLUMNS( ).
        LO_COLUMNS->SET_OPTIMIZE( ).

        TRY.

            LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'ICON_STS' ).
            LO_COLUMN->SET_ICON( IF_SALV_C_BOOL_SAP=>TRUE ).
            LO_COLUMN->SET_LONG_TEXT( 'Status' ).
            LO_COLUMN->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
            LO_COLUMN->SET_OUTPUT_LENGTH( 25 ).

            LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'STATUS' ).
            LO_COLUMN->SET_VISIBLE( ABAP_FALSE ).

            LO_COLUMN ?= LO_COLUMNS->GET_COLUMN( 'ID_ACCDOC' ).
            LO_COLUMN->SET_LONG_TEXT( 'ID_ACCDOC' ).
            LO_COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).


          CATCH CX_SALV_NOT_FOUND.                      "#EC NO_HANDLER
        ENDTRY.

        " Tooltips
        DATA: LO_FUNCTIONAL_SETTINGS  TYPE REF TO CL_SALV_FUNCTIONAL_SETTINGS.
        DATA: LO_TOOLTIPS             TYPE REF TO CL_SALV_TOOLTIPS,
              LV_VALUE                TYPE LVC_VALUE.

        LO_FUNCTIONAL_SETTINGS =  ME->R_ALV_ACCPOST_DOC->GET_FUNCTIONAL_SETTINGS( ).
        LO_TOOLTIPS = LO_FUNCTIONAL_SETTINGS->GET_TOOLTIPS( ).
        TRY.
            LV_VALUE = ICON_GREEN_LIGHT.
            LO_TOOLTIPS->ADD_TOOLTIP(
              TYPE    = CL_SALV_TOOLTIP=>C_TYPE_ICON
              VALUE   = LV_VALUE
              TOOLTIP = 'OK' ).                             "#EC NOTEXT
          CATCH CX_SALV_EXISTING.                       "#EC NO_HANDLER
        ENDTRY.
        TRY.
            LV_VALUE = ICON_YELLOW_LIGHT.
            LO_TOOLTIPS->ADD_TOOLTIP(
              TYPE    = CL_SALV_TOOLTIP=>C_TYPE_ICON
              VALUE   = LV_VALUE
              TOOLTIP = 'Found warnigs' ).                  "#EC NOTEXT
          CATCH CX_SALV_EXISTING.                       "#EC NO_HANDLER
        ENDTRY.
        TRY.
            LV_VALUE = ICON_RED_LIGHT.
            LO_TOOLTIPS->ADD_TOOLTIP(
              TYPE    = CL_SALV_TOOLTIP=>C_TYPE_ICON
              VALUE   = LV_VALUE
              TOOLTIP = 'Found errors' ).                   "#EC NOTEXT
          CATCH CX_SALV_EXISTING.                       "#EC NO_HANDLER
        ENDTRY.


        " Multiselection
        DATA: LO_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS.
        LO_SELECTIONS = ME->R_ALV_ACCPOST_DOC->GET_SELECTIONS( ).
        LO_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>SINGLE ).


        " Functions
        DATA LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST.
        LO_FUNCTIONS = ME->R_ALV_ACCPOST_DOC->GET_FUNCTIONS( ).
        LO_FUNCTIONS->SET_ALL( ).

        " Events
        DATA: LO_EVENTS TYPE REF TO CL_SALV_EVENTS_TABLE.

        LO_EVENTS = ME->R_ALV_ACCPOST_DOC->GET_EVENT( ).
        CREATE OBJECT ME->R_ALV_EVENTS.
        ME->R_ALV_EVENTS->R_RESULT = ME.
        SET HANDLER ME->R_ALV_EVENTS->ON_DOUBLE_CLICK FOR LO_EVENTS.
        SET HANDLER ME->R_ALV_EVENTS->ON_LINK_CLICK FOR LO_EVENTS.


        " Display
        ME->R_ALV_ACCPOST_DOC->DISPLAY( ).

      CATCH CX_SALV_MSG CX_SALV_EXISTING CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND INTO LR_CX_SALV_ERROR .
        RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
          EXPORTING
            TEXTID   = ZCX_SIMPLE_ACC_POST=>ERROR
            PREVIOUS = LR_CX_SALV_ERROR
            ATTR1    = 'Error creating R_ALV_ACCPOST_DOC'.
    ENDTRY.


  ENDMETHOD.                    "ALV_ACCPOST_DOC


  METHOD ADD_ALV_ACCPOST_MSG.
    DATA: LR_CX_SALV_ERROR TYPE REF TO CX_SALV_ERROR.


    TRY .

        " Create ALV on container
        CL_SALV_TABLE=>FACTORY(
            EXPORTING
              R_CONTAINER  = IR_CONTAINER
            IMPORTING
              R_SALV_TABLE = ME->R_ALV_ACCPOST_MSG
            CHANGING
              T_TABLE      = ME->T_ACCPOST_MSG
          ).

        " Columns
        DATA: LO_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.
        LO_COLUMNS = ME->R_ALV_ACCPOST_MSG->GET_COLUMNS( ).
        LO_COLUMNS->SET_OPTIMIZE( ).

        " Funtions
        DATA LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST.
        LO_FUNCTIONS = ME->R_ALV_ACCPOST_MSG->GET_FUNCTIONS( ).
        LO_FUNCTIONS->SET_ALL( ).

        " Display
        ME->R_ALV_ACCPOST_MSG->DISPLAY( ).

      CATCH CX_SALV_MSG CX_SALV_EXISTING CX_SALV_DATA_ERROR CX_SALV_NOT_FOUND INTO LR_CX_SALV_ERROR .
        RAISE EXCEPTION TYPE ZCX_SIMPLE_ACC_POST
          EXPORTING
            TEXTID   = ZCX_SIMPLE_ACC_POST=>ERROR
            PREVIOUS = LR_CX_SALV_ERROR
            ATTR1    = 'Error creating R_ALV_ACCPOST_MSG'.
    ENDTRY.


  ENDMETHOD.                    "ALV_RETURN


ENDCLASS.                    "LCL_RESULT IMPLEMENTATION
" --------------------------------------------------------------------------------------------------------------------------------------------
