{ ------------------------------------------------------------------------ }
{                                                                          }
{ DLangExtensions plugin interface 2.0                                     }
{                                                                          }
{ (C) 2007-2008 Andreas Hausladen                                          }
{                                                                          }
{ ------------------------------------------------------------------------ }

unit LangExtIntf;

{$A8,B-,C+,D-,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

interface

uses
  Classes;

type
  PUtf8Char = PAnsiChar;

  { IVirtualStream represents a read only stream }
  IVirtualStream = interface
    ['{6BBD7B93-9402-4534-ADD3-A3D287FD70E9}']
    { Seek() sets the file pointer to the Offset relative to Origin.
      Origin: 0 = begin of file
              1 = current position
              2 = file end }
    function Seek(Offset: Integer; Origin: Integer): Integer; stdcall;
    { Read reads @Size bytes from the stream and copies them into @Buffer.
      It returns the actual number of read bytes. 0 indicates the file end. }
    function Read(var Buffer; Size: Integer): Integer; stdcall;
    { FileStatus() returns the file size. (FileDate is deprecated) }
    procedure FileStatus(out FileDate: Integer; out FileSize: Integer); stdcall;
  end;

  { IString represents a string }
  IString = interface
    ['{8A9A7E93-F820-42AC-AA87-D577FA865DC0}']
    { SetString() copies @Len bytes of @Buffer into the internal string. }
    procedure SetString(Buffer: PUtf8Char; Len: Cardinal); stdcall;
    { GetString() returns the internal string that must not be modified
      thought the returned pointer. It returns the length of the internal
      string in @Len. }
    function GetString(out Len: Cardinal): PAnsiChar; stdcall;
  end;

  { ILanguageExtensionServices is used to communicate with DLangExtensions }
  ILanguageExtensionServices = interface
    ['{C8272DA5-9BCA-49A3-BA69-35DAC963C414}']
    { RegisterVirtualDiskFile() forces the compiler to use the file on disk instead of
      the content from the editor buffer. }
    procedure RegisterVirtualDiskFile(Filename: PWideChar); stdcall;
    { UnregisterVirtualDiskFile() removes the editor buffer "lock". }
    procedure UnregisterVirtualDiskFile(Filename: PWideChar); stdcall;

    { GetFilename() returns the filename of the file that is preprocessed. }
    function GetFilename: PWideChar; stdcall;

    { GetIsCodeInsight() returns True if the Code Insight compiler has started the
      preprocessing. }
    function GetIsCodeInsight: Boolean; stdcall;

    property Filename: PWideChar read GetFilename;
    property IsCodeInsight: Boolean read GetIsCodeInsight;
  end;

  { The language extension must implement this interface }
  ILanguageExtension = interface
    ['{52B0C6FF-E144-40B7-9BAB-9B26A70DCBEF}']

    { CompileProject() is invoked if the compilation starts. ProjectFilename is the
      file name of the project and is "nil" if the command line compiler is started
      with a single unit. }
    procedure CompileProject(ProjectFilename: PWideChar; IsCodeInsight: Boolean); stdcall;

    { AlterContent() is invoked if a file must be preprocessed. The filename can be
      retrieved by using the Services.Filename property. The return value must be TRUE
      if the function has changed the file content. If it hasn't changed the content it
      must return FALSE. The content is always UTF8 encoded. }
    function AlterContent(Services: ILanguageExtensionServices; Content: IString): LongBool; stdcall;
  end;


{ Set RegisterLanguageExtension to a register procedure in the DllMain function.
  It will be called by DLangExtensions after the DLL is loaded. In this register
  procedure you can call DLang_Ext_RegisterExtension multiple times. }
var
  RegisterLanguageExtensionProc: procedure;

function DLangExt_RegisterExtension(Extension: ILanguageExtension): THandle; stdcall;
  external 'DLangExtensions.dll' name 'DLangExt_RegisterExtension';
procedure DLangExt_UnregisterExtension(Handle: THandle); stdcall;
  external 'DLangExtensions.dll' name 'DLangExt_UnregisterExtension';

type
  { Helper for IVirtualStream }
  TVirtualStream = class(TInterfacedObject, IVirtualStream)
  private
    FStream: TStream;
    FAutoDelete: Boolean;
    FFileDate: Integer;
  public
    constructor Create(AStream: TStream; AFileDate: Integer = 0; AAutoDelete: Boolean = True);
    destructor Destroy; override;

    function Seek(Offset: Integer; Origin: Integer): Integer; stdcall;
    function Read(var Buffer; Size: Integer): Integer; stdcall;
    procedure FileStatus(out FileDate: Integer; out FileSize: Integer); stdcall;
  end;

implementation

{$IFNDEF DLANGEXTENSIONS}
procedure RegisterLanguageExtension; stdcall;
begin
  if Assigned(RegisterLanguageExtensionProc) then
    RegisterLanguageExtensionProc;
end;

exports
  RegisterLanguageExtension;
{$ENDIF ~DLANGEXTENSIONS}

{ TVirtualStreamAdapter }

constructor TVirtualStream.Create(AStream: TStream; AFileDate: Integer; AAutoDelete: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FAutoDelete := AAutoDelete;
  FFileDate := AFileDate;
end;

destructor TVirtualStream.Destroy;
begin
  if FAutoDelete then
    FStream.Free;
  inherited Destroy;
end;

procedure TVirtualStream.FileStatus(out FileDate, FileSize: Integer);
begin
  FileSize := FStream.Size;
  FileDate := FFileDate;
end;

function TVirtualStream.Read(var Buffer; Size: Integer): Integer;
begin
  Result := FStream.Read(Buffer, Size);
end;

function TVirtualStream.Seek(Offset, Origin: Integer): Integer;
begin
  Result := FStream.Seek(Offset, Origin);
end;

end.
