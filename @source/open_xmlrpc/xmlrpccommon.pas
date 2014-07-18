unit xmlrpccommon;

{$mode delphi}

interface

uses
  {$IFDEF WIN32}
  windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  Classes,
  sysutils;

type
   T4x4LongWordRecord = array[0..3] of LongWord;


function GetTempDir: String;
function DateTimeToISO(const ConvertDate: TDateTime): string;
function ISOToDateTime(const ISOStringDate: string): TDateTime;

function FileIsExpired(sFileName: string; elapsed: integer): boolean;
function FixEmptyString(value: String):String;

function StreamToString(const Stream: TStream): String;
procedure StringToStream(const Text: String; const Stream: TStream);
function Hash128AsHex(const AHash128Value: T4x4LongWordRecord):String;

implementation

{------------------------------------------------------------------------------}
{ convert stream to a string                                                   }
{------------------------------------------------------------------------------}
function StreamToString(const Stream: TStream): String;
begin
  Result := '';
  Stream.Seek(0, soFromBeginning);
  SetLength(Result, Stream.Size);
  Stream.Read(Result[1], Stream.Size);
end;

{------------------------------------------------------------------------------}
{  Converts a string to a stream                                               }
{------------------------------------------------------------------------------}
procedure StringToStream(const Text: String;
                         const Stream: TStream);
begin
   Stream.WriteBuffer(Pointer(Text)^, Length(Text));
   Stream.Position := 0;
end;

{------------------------------------------------------------------------------}
{  Converts a date time to iso 8601 format                                     }
{------------------------------------------------------------------------------}
function DateTimeToISO(const ConvertDate: TDateTime): string;
begin
  result := FormatDateTime('yyyymmdd"T"hh:mm:ss',ConvertDate);
end;

{------------------------------------------------------------------------------}
{  Converts a ISO 8601 data to TDateTime                                       }
{------------------------------------------------------------------------------}
function ISOToDateTime(const ISOStringDate: string): TDateTime;
begin
  Result := EncodeDate(StrToInt(ISOStringDate[1] +
                                ISOStringDate[2] +
                                ISOStringDate[3] +
                                ISOStringDate[4]),

                                StrToInt(ISOStringDate[5] +
                                         ISOStringDate[6]),

                                StrToInt(ISOStringDate[7] +
                                         ISOStringDate[8])) +
                                EncodeTime(StrToInt(ISOStringDate[10] +
                                                    ISOStringDate[11]),

                                StrToInt(ISOStringDate[13] +
                                         ISOStringDate[14]),

                                StrToInt(ISOStringDate[16] +
                                            ISOStringDate[17]),0);

end;

{------------------------------------------------------------------------------}
//check a file to see if the elapsed time is expired
function FileIsExpired(sFileName: string; elapsed: integer): boolean;
var                                                                             
  FHandle: integer;
  FDate: TDateTime;
  ts: TTimeStamp;
  ct: TTimeStamp;
  mn: integer;
begin
  if elapsed <=0 then
  begin
     Result := true;
     Exit;
  end;

  FHandle := FileOpen(sFileName, 0);
  try
    FDate := FileDateToDateTime(FileGetDate(FHandle));
    ts := DateTimeToTimeStamp(FDate);
    ct := DateTimeToTimeStamp(Now);
    mn := round((ct.Time - ts.Time));
    if (mn > elapsed) then
      result := true
    else
      result := false;
  finally
    FileClose(FHandle);
  end;
end;

{------------------------------------------------------------------------------}
function GetTempDir: String;
{$IFDEF WIN32}
var
 buf: array[0..MAX_PATH] of Char;
{$ENDIF}
begin
  {$IFDEF WIN32}
  GetTempPath( Sizeof(buf), buf );
  Result := buf;
  If Result[Length(Result)] <> '\' Then
    Result := Result + '\';
  {$ENDIF}
  {$IFDEF LINUX}
   Result := '/var/tmp/'
  {$ENDIF}

end;

{------------------------------------------------------------------------------}
function Hash128AsHex(const AHash128Value: T4x4LongWordRecord):string;
begin
  Result := IntToHex(AHash128Value[0],4) +
            IntToHex(AHash128Value[1],4) +
            IntToHex(AHash128Value[2],4) +
            IntToHex(AHash128Value[3],4);

end;
{------------------------------------------------------------------------------}
function FixEmptyString(value: String):String;
begin
  result := StringReplace(value,'<string></string>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<string></nil></string>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<string></null></string>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<string> </string>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);

  result := StringReplace(result,'<value></value>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<value></nil></value>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<value></null></value>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<value> </value>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
end;

end.