# Multiple SAP postings made using excel's clipboard

This is a simple way to make business transaction with account posting, from excel with copy / paste method. 
Copy multiple documents to clipboard from one single sheet and post them into SAP … with one single button !.

You can post debitor, creditor, GL, tax, withholdings tax, CPD customer... like FB50, FB60 or FB70

## Show in action
Samples of postings made with BAPI_ACC_DOCUMENT_POST from [SAP note 2083799](https://launchpad.support.sap.com/#/notes/2083799)

#### Single Document
![Single document](/images/min_doc.gif)

#### Multiple Debitor documents with tax
![Multiple documents](/images/tax_doc.gif)

## How it works
![Diagram](/images/Diagram.jpg)

```abap
METHOD BTN_CB_P_D.
  DATA: LR_SIMPLE_ACC_POST TYPE REF TO ZCL_SIMPLE_ACC_POST.

  CREATE OBJECT LR_SIMPLE_ACC_POST.
  LR_SIMPLE_ACC_POST->POST(
    EXPORTING
      IV_CHECK          = ABAP_FALSE    
      IV_CLIPBOARD_DATA = ABAP_TRUE     " Post clipboard data
  ).
  LR_SIMPLE_ACC_POST->DISPLAY_RESULT( ).
  
ENDMETHOD.                    "BTN_CB_P_D  
```

## Fields available
![Fields available](/images/fields.jpg)

## Install

### Prerequisites
Only if you need bill of exchange postings. They are not supported by BAPI_ACC_DOCUMENT_POST (see note [2076117](https://launchpad.support.sap.com/#/notes/2076117)), and for me, the simplest way to do it's using a BDC with [ZCL_BC_BCD](https://github.com/EsperancaB/sap_project_object/tree/master/UTILITIES/ZCL_BC_BDC). 
If you don't want install it, only have to do is remove (or comment) method ZCL_SIMPLE_ACC_POST->BDC_POST.

### SAPLinks
[SAPlink User Documentation](https://wiki.scn.sap.com/wiki/display/ABAP/SAPlink+User+Documentation)

Import nugget file: [NUGG_Z_SIMPLE_ACC_POST.nugg](https://github.com/alfonsogildegea/simple_acc_post/blob/master/NUGG_Z_SIMPLE_ACC_POST.nugg)

### ABAPGIT
[http://www.abapgit.org](http://www.abapgit.org)

Import package: [ABAPGIT_20180122.zip](https://github.com/alfonsogildegea/simple_acc_post/blob/master/ABAPGIT_20180122.zip)


