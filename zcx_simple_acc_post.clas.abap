class ZCX_SIMPLE_ACC_POST definition
  public
  inheriting from CX_NO_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ERROR,
      msgid type symsgid value 'ZFSAP',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'ATTR1',
      attr2 type scx_attrname value 'ATTR2',
      attr3 type scx_attrname value 'ATTR3',
      attr4 type scx_attrname value 'ATTR4',
    end of ERROR .
  constants:
    begin of INFO,
      msgid type symsgid value 'ZFSAP',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'ATTR1',
      attr2 type scx_attrname value 'ATTR2',
      attr3 type scx_attrname value 'ATTR3',
      attr4 type scx_attrname value 'ATTR4',
    end of INFO .
  constants:
    begin of TAX_ERROR,
      msgid type symsgid value 'ZFSAP',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'ATTR1',
      attr2 type scx_attrname value 'ATTR2',
      attr3 type scx_attrname value 'ATTR3',
      attr4 type scx_attrname value 'ATTR4',
    end of TAX_ERROR .
  data ATTR1 type SCX_ATTRNAME .
  data ATTR2 type SCX_ATTRNAME .
  data ATTR3 type SCX_ATTRNAME .
  data ATTR4 type SCX_ATTRNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !ATTR1 type SCX_ATTRNAME optional
      !ATTR2 type SCX_ATTRNAME optional
      !ATTR3 type SCX_ATTRNAME optional
      !ATTR4 type SCX_ATTRNAME optional .
  methods GET_BAPIRET2
    importing
      !IV_MSGTY type SY-MSGTY
      !IV_LOG_NO type BALOGNR optional
    exporting
      value(ES_BAPIRET2) type BAPIRET2 .
protected section.
private section.
ENDCLASS.



CLASS ZCX_SIMPLE_ACC_POST IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->ATTR1 = ATTR1 .
me->ATTR2 = ATTR2 .
me->ATTR3 = ATTR3 .
me->ATTR4 = ATTR4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


METHOD GET_BAPIRET2.

  CLEAR ES_BAPIRET2.

  ES_BAPIRET2-TYPE       = IV_MSGTY.
  ES_BAPIRET2-ID         = ME->IF_T100_MESSAGE~T100KEY-MSGID.
  ES_BAPIRET2-NUMBER     = ME->IF_T100_MESSAGE~T100KEY-MSGNO.
  ES_BAPIRET2-MESSAGE    = ME->IF_MESSAGE~GET_LONGTEXT( ).
  ES_BAPIRET2-LOG_NO     = IV_LOG_NO.
  ES_BAPIRET2-LOG_MSG_NO = ''.
  ES_BAPIRET2-MESSAGE_V1 = ME->ATTR1.
  ES_BAPIRET2-MESSAGE_V2 = ME->ATTR2.
  ES_BAPIRET2-MESSAGE_V3 = ME->ATTR3.
  ES_BAPIRET2-MESSAGE_V4 = ME->ATTR4.
  ES_BAPIRET2-PARAMETER  = ''.
  ES_BAPIRET2-ROW        = ''.
  ES_BAPIRET2-FIELD      = ''.
  ES_BAPIRET2-SYSTEM     = ''.

ENDMETHOD.
ENDCLASS.