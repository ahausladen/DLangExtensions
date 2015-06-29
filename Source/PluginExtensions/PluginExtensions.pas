unit PluginExtensions;

interface

uses
  Windows, SysUtils, Classes, Contnrs, LanguageExtension, LangExtIntf, Utils;

type
  TPluginExtensions = class(TLanguageExtension, ILanguageExtensionServices)
  protected
    class procedure CompileProject(const AFilename: string; AIsCodeInsight: Boolean); override;
    function AlterContent(var Content: UTF8String): Boolean; override;
  public
    function CanPreprocessFile: Boolean;
    function GetFilename: PWideChar; stdcall;
    procedure RegisterVirtualDiskFile(Filename: PWideChar); stdcall;
    procedure UnregisterVirtualDiskFile(Filename: PWideChar); stdcall;
    function GetIsCodeInsight: Boolean; stdcall;
  end;

  TStringValue = class(TInterfaceEnabledObject, IString)
  private
    FValue: UTF8String;
  public
    constructor Create(const AValue: UTF8String);
    function GetString(out Len: Cardinal): PUtf8Char; stdcall;
    procedure SetString(Buffer: PUtf8Char; Len: Cardinal); stdcall;

    procedure SetLength(NewLen: Cardinal);

    property Value: UTF8String read FValue write FValue;
  end;

function DLangExt_RegisterExtension(Extension: ILanguageExtension): THandle; stdcall;
procedure DLangExt_UnregisterExtension(Handle: THandle); stdcall;

implementation

var
  Plugins: TList;
  PluginsCS: TRTLCriticalSection;

exports
  DLangExt_RegisterExtension,
  DLangExt_UnregisterExtension;

function DLangExt_RegisterExtension(Extension: ILanguageExtension): THandle; stdcall;
begin
  EnterCriticalSection(PluginsCS);
  try
    if Plugins = nil then
      Plugins := TList.Create;
    if Plugins.IndexOf(Pointer(Extension)) = -1 then
    begin
      Plugins.Add(Pointer(Extension));
      Extension._AddRef;
    end;
    Result := THandle(Extension);
  finally
    LeaveCriticalSection(PluginsCS);
  end;
end;

procedure DLangExt_UnregisterExtension(Handle: THandle); stdcall;
var
  Index: Integer;
begin
  EnterCriticalSection(PluginsCS);
  try
    if Plugins <> nil then
    begin
      Index := Plugins.IndexOf(Pointer(Handle));
      if Index <> -1 then
      begin
        Plugins.Delete(Index);
        if not IsBadReadPtr(Pointer(Handle), 4) then
          ILanguageExtension(Pointer(Handle))._Release;
      end;
      if Plugins.Count = 0 then
        FreeAndNil(Plugins);
    end;
  finally
    LeaveCriticalSection(PluginsCS);
  end;
end;

{--------------------------------------------------------------------------------------------------}

{ TStringValue }

constructor TStringValue.Create(const AValue: UTF8String);
begin
  inherited Create;
  FValue := AValue;
end;

function TStringValue.GetString(out Len: Cardinal): PUtf8Char;
begin
  Result := PAnsiChar(FValue);
  Len := Length(Value);
end;

procedure TStringValue.SetLength(NewLen: Cardinal);
begin
  System.SetLength(FValue, NewLen);
end;

procedure TStringValue.SetString(Buffer: PUtf8Char; Len: Cardinal);
begin
  System.SetString(FValue, Buffer, Len);
end;

{ TPluginExtensions }

function TPluginExtensions.CanPreprocessFile: Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(Filename));
  Result := (Ext = '.dpr') or (Ext = '.dpk') or (Ext = '.pas') or (Ext = '.inc');
end;

function TPluginExtensions.AlterContent(var Content: UTF8String): Boolean;
var
  I: Integer;
  ContentStr: TStringValue;
  Intf: ILanguageExtension;
begin
  Result := False;
  if (Plugins <> nil) and CanPreprocessFile then
  begin
    ContentStr := TStringValue.Create(Content);
    EnterCriticalSection(PluginsCS);
    try
      try
        for I := 0 to Plugins.Count - 1 do
        begin
          Intf := ILanguageExtension(Plugins[I]);
          if Intf.AlterContent(Self, ContentStr) then
            Result := True;
        end;
      finally
        Intf := nil;
      end;
      if Result then
        Content := ContentStr.Value;
    finally
      Intf := nil;
      LeaveCriticalSection(PluginsCS);
      ContentStr.Free;
    end;
  end;
end;

class procedure TPluginExtensions.CompileProject(const AFilename: string; AIsCodeInsight: Boolean);
var
  I: Integer;
  Intf: ILanguageExtension;
begin
  inherited CompileProject(AFilename, AIsCodeInsight);
  if Plugins <> nil then
  begin
    EnterCriticalSection(PluginsCS);
    try
      try
        for I := 0 to Plugins.Count - 1 do
        begin
          Intf := ILanguageExtension(Plugins[I]);
          Intf.CompileProject(PChar(AFilename), AIsCodeInsight);
        end;
      finally
        Intf := nil;
      end;
   finally
      LeaveCriticalSection(PluginsCS);
    end;
  end;
end;

function TPluginExtensions.GetFilename: PWideChar;
begin
  Result := PWideChar(Filename);
end;

function TPluginExtensions.GetIsCodeInsight: Boolean;
begin
  Result := Manager.IsCodeInsight;
end;

procedure TPluginExtensions.RegisterVirtualDiskFile(Filename: PWideChar);
begin
  inherited RegisterVirtualDiskFile(Filename);
end;

procedure TPluginExtensions.UnregisterVirtualDiskFile(Filename: PWideChar);
begin
  inherited UnregisterVirtualDiskFile(Filename);
end;

{--------------------------------------------------------------------------------------------------}

var
  PluginLibs: TList;

procedure LoadPlugins;
var
  sr: TSearchRec;
  Path: string;
  h: THandle;
  RegisterProc: procedure; stdcall;
begin
  Path := ExtractFilePath(GetModuleName(HInstance)) + 'DLangExtPlugins' + PathDelim;
  if FindFirst(Path + '*.dll', faAnyFile and not faDirectory, sr) = 0 then
  repeat
    if sr.Attr and faDirectory = 0 then // should not happen at all
    begin
      h := LoadLibrary(PChar(Path + sr.Name));
      if h <> 0 then
      begin
        @RegisterProc := GetProcAddress(h, 'RegisterLanguageExtension');
        if Assigned(RegisterProc) then
        begin
          try
            RegisterProc;
          except
            OutputDebugString(PChar('Failed to register DLangExtensions plugin ' + sr.Name));
          end;
          if PluginLibs = nil then
            PluginLibs := TList.Create;
          PluginLibs.Add(Pointer(h));
        end
        else
          FreeLibrary(h);
      end;
    end;
  until FindNext(sr) <> 0;
  FindClose(sr);
end;

procedure UnloadPlugins;
var
  I: Integer;
begin
  try
    if Plugins <> nil then
    begin
      for I := 0 to Plugins.Count - 1 do
        if not IsBadReadPtr(Plugins[I], 4) then
          ILanguageExtension(Plugins[I])._Release;
      FreeAndNil(Plugins);
    end;

    if PluginLibs <> nil then
      for I := 0 to PluginLibs.Count - 1 do
        FreeLibrary(HMODULE(PluginLibs[I]));
  except
    OutputDebugString('Failed to unload DLangExtensions plugins');
  end;
  PluginLibs.Free;
end;

initialization
  InitializeCriticalSection(PluginsCS);
  //AddCleanupProc(UnloadPlugins);
  //LoadPlugins;

finalization
  //UnloadPlugins;
  DeleteCriticalSection(PluginsCS);

end.
