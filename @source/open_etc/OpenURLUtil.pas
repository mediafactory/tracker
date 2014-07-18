unit OpenURLUtil;

interface 

{$mode objfpc}{$h+}

// Using Win32Proc binds the unit to LCL
// to break the binding, check for Unicode Windows must be done manually

// CarbonProc CreateCFString should also be replaced with MacOSAll code

{$ifdef Windows}
uses ShellAPI, Win32Proc;
{$endif}
{$ifdef LINUX}
uses SysUtils;
{$endif}
{$ifdef Darwin} // LCLCarbon?
uses MacOSAll, CarbonProc;
{$endif}
  
function OpenURLWide(const URLWide: WideString): Boolean;
function OpenURLAnsi(const URLAnsi: AnsiString): Boolean;
function OpenURL(const URLUtf8: String): Boolean;
  
implementation

function OpenURLWide(const URLWide: WideString): Boolean;
begin 
  Result := OpenURL(UTF8Encode(URLWide));
end;

function OpenURLAnsi(const URLAnsi: AnsiString): Boolean;
begin
  Result := OpenURL(AnsiToUtf8(URLAnsi));
end;

{$IFDEF WINDOWS}
function OpenURL(const URLUtf8: String): Boolean;
var
  ws    : WideString;
  ans   : AnsiString;
begin
  Result := false;
  if URLUtf8 = '' then Exit;

  if UnicodeEnabledOS then begin
    ws := UTF8Decode(URLUtf8);
    Result := ShellExecuteW(0, 'open', PWideChar(ws), nil, nil, 0) > 32;
  end else begin
    ans := Utf8ToAnsi(URLUtf8); // utf8 must be converted to Windows Ansi-codepage
    Result := ShellExecute(0, 'open', PAnsiChar(ans), nil, nil, 0) > 32;
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
function OpenURL(const URLUtf8: string): Boolean;
var
  Helper: string;
begin
  Result := True;
  try 
    Helper := '';
    if fpsystem('which xdg-open') = 0 then
      Helper := 'xdg-open'
    else if FileExists('/etc/alternatives/x-www-browser') then
      Helper := '/etc/alternatives/x-www-browser'
    else if fpsystem('which firefox') = 0 then
      Helper := 'firefox'
    else if fpsystem('which konqueror') = 0 then
      Helper := 'konqueror'
    else if fpsystem('which opera') = 0 then
      Helper := 'opera'
    else if fpsystem('which mozilla') = 0 then
       Helper := 'mozilla';
  
    if Helper <> '' then
      fpSystem(Helper + ' ' + URLUtf8 + '&');
    else
      Result := False;
  except
  end;
end;
{$ENDIF}

{$IFDEF DARWIN}
function OpenURL(const URLUtf8: string): Boolean;
var
  cf  : CFStringRef;
  url : CFURLRef;
begin
  if URLUtf8 = '' then begin
    Result := false;
    Exit;
  end;
  CreateCFString(URLUtf8, cf);
  url := CFURLCreateWithString(nil, cf, nil);
  Result := LSOpenCFURLRef(url, nil) = 0;
  CFRelease(url);
  CFRelease(cf);
end;
{$ENDIF}

end.
