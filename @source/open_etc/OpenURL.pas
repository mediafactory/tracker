function OpenURL(const aURL: string): Boolean;
var
  Helper: string;
begin
  Result := True;
  
  {$IFDEF WINDOWS}
  try
    ShellExecute(0, 'open', PChar(aURL), nil, nil, 0) ;
  except
    Result := False;
  end;
  {$ENDIF}
  
  {$IFDEF LINUX}
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
    fpSystem(Helper + ' ' + aURL + '&');
  else
    Result := False;
  {$ENDIF}
end;