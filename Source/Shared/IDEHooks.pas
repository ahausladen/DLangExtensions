{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit IDEHooks;

{$I jedi\jedi.inc}

interface

const
{$IFDEF COMPILER5}
  bcbide_bpl = 'bcbide50.bpl';
  delphide_bpl = 'dphide50.bpl';
  coreide_bpl = 'coride50.bpl';
  rtl_bpl = 'vcl50.bpl';
  vcl_bpl = 'vcl50.bpl';
  delphicoreide_bpl = coreide_bpl;
  designide_bpl = 'dsgnide50.bpl';
  vclide_bpl = 'vclide50.bpl';
  DelphiVersion = '5';
{$ENDIF COMPIELR5}
{$IFDEF COMPILER6}
  bcbide_bpl = 'bcbide60.bpl';
  delphide_bpl = 'delphide60.bpl';
  coreide_bpl = 'coreide60.bpl';
  rtl_bpl = 'rtl60.bpl';
  vcl_bpl = 'vcl60.bpl';
  delphicoreide_bpl = coreide_bpl;
  vclide_bpl = 'vclide60.bpl';
  designide_bpl = 'designide60.bpl';
  DelphiVersion = '6';
{$ENDIF COMPIELR6}
{$IFDEF COMPILER7}
  bcbide_bpl = 'bcbide70.bpl';
  delphide_bpl = 'delphide70.bpl';
  coreide_bpl = 'coreide70.bpl';
  rtl_bpl = 'rtl70.bpl';
  vcl_bpl = 'vcl70.bpl';
  delphicoreide_bpl = coreide_bpl;
  vclide_bpl = 'vclide70.bpl';
  designide_bpl = 'designide70.bpl';
  DelphiVersion = '7';
{$ENDIF COMPIELR7}
{$IFDEF COMPILER9}
  bcbide_bpl = 'bcbide90.bpl';
  delphide_bpl = 'delphide90.bpl';
  coreide_bpl = 'coreide90.bpl';
  rtl_bpl = 'rtl90.bpl';
  vcl_bpl = 'vcl90.bpl';
  delphicoreide_bpl = 'delphicoreide90.bpl';
  vclide_bpl = 'vclide90.bpl';
  designide_bpl = 'designide90.bpl';
  idectrls_bpl = 'idectrls90.bpl';
  DelphiVersion = '9';
{$ENDIF COMPILER9}
{$IFDEF COMPILER10}
  bcbide_bpl = 'bcbide100.bpl';
  delphide_bpl = 'delphide100.bpl';
  coreide_bpl = 'coreide100.bpl';
  rtl_bpl = 'rtl100.bpl';
  vcl_bpl = 'vcl100.bpl';
  delphicoreide_bpl = 'delphicoreide100.bpl';
  vclide_bpl = 'vclide100.bpl';
  designide_bpl = 'designide100.bpl';
  dbkdebugide_bpl = 'dbkdebugide100.bpl';
  idectrls_bpl = 'idectrls100.bpl';
  DelphiVersion = '10';
{$ENDIF COMPILER10}
{$IFDEF COMPILER11}
  bcbide_bpl = 'bcbide110.bpl';
  delphide_bpl = 'delphide110.bpl';
  coreide_bpl = 'coreide110.bpl';
  rtl_bpl = 'rtl110.bpl';
  vcl_bpl = 'vcl110.bpl';
  delphicoreide_bpl = 'delphicoreide110.bpl';
  vclide_bpl = 'vclide110.bpl';
  designide_bpl = 'designide110.bpl';
  dbkdebugide_bpl = 'dbkdebugide110.bpl';
  idectrls_bpl = 'idectrls110.bpl';
  DelphiVersion = '11';
{$ENDIF COMPIELR11}
{$IFDEF COMPILER12}
  bcbide_bpl = 'bcbide120.bpl';
  delphide_bpl = 'delphide120.bpl';
  coreide_bpl = 'coreide120.bpl';
  rtl_bpl = 'rtl120.bpl';
  vcl_bpl = 'vcl120.bpl';
  delphicoreide_bpl = 'delphicoreide120.bpl';
  vclide_bpl = 'vclide120.bpl';
  designide_bpl = 'designide120.bpl';
  dbkdebugide_bpl = 'dbkdebugide120.bpl';
  idectrls_bpl = 'idectrls120.bpl';
  DelphiVersion = '12';
{$ENDIF COMPIELR12}

implementation

end.
