# Multiple SAP postings made with excel's clipboard

This is a simple way to make business transaction with account posting, from excel with copy / paste method. 
Copy multiple documents to clipboard from one single sheet and post them into SAP … with one single button !.

You can post debitor, creditor, GL, tax, withholdings tax, CPD customer... like FB50, FB60 or FB70

- [See it in action](#see-it-in-action)
- [How it works](#how-it-works)
- [Fields available](#fields-available)
- [Install](#install)
	- [Prerequisites](#prerequisites)
	- [SAPLinks](#saplinks-recommended)
	- [ABAPGIT](#abapgit-alternative)

![Steps](/images/steps.jpg)


## See it in action
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

[Excel Template](/Template_ZCL_SIMPLE_ACC_POST.xlsx)

**Plain template**

ID_ACCDOC|AWTYP|AWKEY|AWSYS|BUKRS|BLART|BLDAT|BUDAT|MONAT|VATDATE|WAERS|XBLNR|BKTXT|KUNNR|LIFNR|UMSKS|HKONT|WRBTR|VALUT|SGTXT|ZUONR|MWSKZ|KOSTL|POSID|XREF1|XREF2|XREF3|ZTERM|ZFBDT|ZBD1T|ZLSCH|ZLSPR|HBKID|HKTID|WITHT|WT_WITHCD|WT_QSSHB|WT_QBSHB|VBUND|NAME|POSTL_CODE|COUNTRY_ISO|CITY|STREET|TAX_NO_1|TAX_NO_2|TAX_NO_3|TAX_NO_4
---------|-----|-----|-----|-----|-----|-----|-----|-----|-------|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|---------|--------|--------|-----|----|----------|-----------|----|------|--------|--------|--------|--------
Financial Document ID. Group items into an Acc. Document|Reference Transaction|Reference Key|Logical system of source document|Company Code|Document Type|Document Date in Document|Posting Date in the Document|Fiscal Period|Tax Reporting Date|Currency Key|Reference Document Number|Document Header Text|Customer Number|Account Number of Vendor or Creditor|Special G/L Transaction Type|General Ledger Account|Amount in Document Currency|Value Date|Item Text|Assignment Number|Tax on sales/purchases code|Cost Center|Work Breakdown Structure Element (WBS Element)|Business Partner Reference Key|Business Partner Reference Key|Reference Key for Line Item|Terms of Payment Key|Baseline Date For Due Date Calculation|Days for first cash discount|Payment method|Payment block key|Short Key for a House Bank|ID for Account Details|Indicator for withholding tax type|Withholding tax code|Withholding tax base amount in document currency|Withholding tax amount in document currency|Company ID of Trading Partner|Name 1|Postal Code|Country key in ISO code|City|House number and street|Tax Number 1|Tax Number 2|Tax Number 3|Tax Number 4
CHAR 255|CHAR 5|CHAR 20|CHAR 10|CHAR 4|CHAR 2|DATS 8|DATS 8|CHAR 2|DATS 8|CUKY 5|CHAR 16|CHAR 25|CHAR 10|CHAR 10|CHAR 1|CHAR 10|DEC  23  4|DATS 8|CHAR 50|CHAR 18|CHAR 2|CHAR 10|CHAR 24|CHAR 12|CHAR 12|CHAR 20|CHAR 4|DATS 8|DEC  3|CHAR 1|CHAR 1|CHAR 5|CHAR 5|CHAR 2|CHAR 2|CURR 15  2|CURR 15  2|CHAR 6|CHAR 35|CHAR 10|CHAR 2|CHAR 35|CHAR 35|CHAR 16|CHAR 11|CHAR 18|CHAR 18


## Install

### Prerequisites
Only if you need bill of exchange postings. They are not supported by BAPI_ACC_DOCUMENT_POST (see note [2076117](https://launchpad.support.sap.com/#/notes/2076117)), and for me, the simplest way to do it's using a BDC with [ZCL_BC_BCD](https://github.com/EsperancaB/sap_project_object/tree/master/UTILITIES/ZCL_BC_BDC). 
If you don't want install it, only have to do is remove (or comment) method ZCL_SIMPLE_ACC_POST->BDC_POST.

### SAPLinks (Recommended)
[SAPlink User Documentation](https://wiki.scn.sap.com/wiki/display/ABAP/SAPlink+User+Documentation)

Import nugget file: [NUGG_Z_SIMPLE_ACC_POST.nugg](https://github.com/alfonsogildegea/simple_acc_post/blob/master/NUGG_Z_SIMPLE_ACC_POST.nugg)

### ABAPGIT (Alternative)
[http://www.abapgit.org](http://www.abapgit.org)

Import package: [ABAPGIT_20180122.zip](https://github.com/alfonsogildegea/simple_acc_post/blob/master/ABAPGIT_20180122.zip)


