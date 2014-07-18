unit xmlrpc;

{$mode delphi}

(*
   <c> media factory gmbh, luebeck germany
   Henrik Genssen hg@mediafactory.de
   license: GPL
*)

interface

uses
dialogs,
   Classes,
   SysUtils,
   XMLRead,
   DOM,
   // lNet
   lNet,
   lhttp,
   lHTTPUtil,
   base64,
   // xmlRPC
   xmlrpctypes,
   xmlrpccommon;

type
   TxmlRpcMulticallSupport = (mcYes, mcNo, mcDontKnow);
   TxmlRpcParserType = (xmlRpcServer, xmlRpcClient);

   { TFPCxmlRpcParser }

   TFPCxmlRpcParser = class
      private
         FDOM: TXMLDocument;
         FxmlRpcResult: TxmlRpcResult;
         FMethodName: String;
         FObjectList: TxmlRpcParamList;
         FxmlRpcParserType: TxmlRpcParserType;
         FDebug: boolean;

         procedure DataTag(Node: TDOMNode; aValue: TxmlRpcDataElement);

      public
         constructor Create(xmlRpcParserType: TxmlRpcParserType); reintroduce;
         destructor Destroy; override;
         procedure Parse(sdata: string);
         function GetResult : TxmlRpcResult;
         property  RequestName: string read FMethodName;
         property Parameter: TxmlRpcParamList read FObjectList;
         property Debug: boolean read FDebug write FDebug;
   end;

   { TxmlRpcCaller }

   TxmlRpcCaller = class(TFPCxmlRpcParser)
    private
      FHostName: string;
      FHostPort: integer;
      FProxyName: string;
      FProxyPort: integer;
      FProxyUserName: string;
      FProxyPassword: string;
      FUseSSL: boolean;
      FSSLRootCertFile: string;
      FSSLCertFile: string;
      FSSLKeyFile: string;
      FEndPoint: string;
      FProxyBasicAuth: boolean;
      FFixEmptyString: boolean;

      FUserName: string;
      FPassword: string;
      FBasicAuth: boolean;
      FUserAgent: String;
      FCachePath: String;

      FDone: boolean;
      FValidResponse: boolean;
      FResponse: String;
      FXMLRequest: TStringStream;
      FXMLResponse: TStringStream;

      function Post(Data: TxmlRpcFunction): string;
      procedure ClientDisconnect(ASocket: TLSocket);
      procedure ClientDoneInput(ASocket: TLHTTPClientSocket);
      procedure ClientError(const Msg: string; aSocket: TLSocket);
      function ClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: Integer): Integer;
      procedure ClientProcessHeaders(ASocket: TLHTTPClientSocket);
      procedure ClientCanWrite(ASocket: TLHTTPClientSocket; var OutputEof: TWriteBlockStatus);

    public
      constructor Create;
      destructor Destroy; override;
      property FixEmptyStrings: boolean read FFixEmptyString write FFixEmptyString;
      property EndPoint: string read FEndPoint write FEndPoint;
      property HostName: string read FHostName write FHostName;
      property HostPort: integer read FHostPort write FHostPort;
      property ProxyName: string read FProxyName write FProxyName;
      property ProxyPort: integer read FProxyPort write FProxyPort;
      property ProxyUserName: string read FProxyUserName write FProxyUserName;
      property ProxyPassword: string read FProxyPassword write FProxyPassword;
      property ProxyBasicAuth: boolean read FProxyBasicAuth write FProxyBasicAuth;
      property UseSSL: boolean read FUseSSL write FUseSSL;
      property SSLRootCertFile: string read FSSLRootCertFile write FSSLRootCertFile;
      property SSLCertFile: string read FSSLCertFile write FSSLCertFile;
      property SSLKeyFile: string read FSSLKeyFile write FSSLKeyFile;

      property Username: string read FUsername write FUsername;
      property Password: string read FPassword write FPassword;
      property BasicAuth: boolean read FBasicAuth write FBasicAuth;
      property UserAgent: string read FUserAgent write FUserAgent;
      property CachePath: String read FCachePath write FCachePath;

      function Execute(Data: TxmlRpcFunction; ttl: integer): TxmlRpcResult; overload;
      function Execute(Data: TxmlRpcFunction): TxmlRpcResult; overload;

      procedure DeleteOldCache(ttl: integer);

    end;

    TxmlRpcMultiCaller = class(TxmlRpcCaller)
       private
          FMulti: TxmlRpcFunction;
          FFunctionArray: TxmlRpcArray;
          FResult: TxmlRpcResult;
          FMultiCallSupport: TxmlRpcMulticallSupport;

          function getResult(index: integer) : TxmlRpcDataElement;

       public
          constructor Create(ServerSupportsMulticall: TxmlRpcMulticallSupport=mcDontKnow);
          destructor Destroy; override;
          procedure Execute; reintroduce;
          function CallCount : Integer;
          function ResultCount : Integer;
          function AddFunctionCall(Data: TxmlRpcFunction) : TxmlRpcFunction; overload;
          procedure AddFunctionCall(ObjectMethod: String; Params: array of const); overload;
          property Result[index: integer] : TxmlRpcDataElement read getResult;
          property ServerSupportsMulticall: TxmlRpcMulticallSupport read FMultiCallSupport;
    end;

    function XMLRpcFunctionCall(const UseSSL: boolean; const Host: String; const Port: Integer; const URL, User, Passwd, RPCFunction: string; Params: array of const; {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr: TIdClientTCPConnectionMgr; {$ENDIF}{$ENDIF}RaiseException: boolean; Debug: boolean=false) : TxmlRpcDataElement; overload;
    function XMLRpcFunctionCall(const UseSSL: boolean; const Host: String; const Port: Integer; const URL, User, Passwd, RPCFunction: string; Params: TxmlRpcArray; {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr: TIdClientTCPConnectionMgr; {$ENDIF}{$ENDIF}RaiseException: boolean; Debug: boolean=false) : TxmlRpcDataElement; overload;
    function XMLRpcSupportMulticall(const UseSSL: boolean; const Host: String; const Port: Integer; const URL, User, Passwd: string; {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr: TIdClientTCPConnectionMgr; {$ENDIF}{$ENDIF}Debug: boolean=false) : TxmlRpcMulticallSupport;

const
  ERROR_EMPTY_RESULT = 600;
  ERROR_EMPTY_RESULT_MESSAGE = 'The xml-rpc server returned a empty response';
  ERROR_INVALID_RESPONSE = 601;
  ERROR_INVALID_RESPONSE_MESSAGE = 'Invalid payload received from xml-rpc server';


var
   debugXMLRPCDataRequest: String;
   debugXMLRPCDataResponse: String;

implementation

uses Variants;

{ TFPCxmlRpcParser }

procedure TFPCxmlRpcParser.DataTag(Node: TDOMNode; aValue: TxmlRpcDataElement);
var
   nL: TDOMNodeList;
   I: Integer;
   Ar: TxmlRpcArray;
   Struct: TxmlRpcStruct;
   iE1, iE2: TDOMNode;

begin
   // String
   if UpperCase(Node.childNodes[0].nodeName) = 'STRING' then
   begin
      if Assigned(Node.childNodes[0].attributes.getNamedItem('cid')) then
         aValue.CID := Node.childNodes[0].Attributes.GetNamedItem('cid').TextContent
      else
      begin
         if Node.childNodes[0].TextContent = '[NULL]' then
            aValue.SetItem('')
         else
            aValue.SetItem(UTF8Encode(Widestring(Node.childNodes[0].TextContent)));
      end;
   end

   // Integer
   else if UpperCase(Node.childNodes[0].nodeName) = 'INT' then
      aValue.SetItem(StrToInt(Node.childNodes[0].TextContent))

   // Integer 4
   else if (UpperCase(Node.childNodes[0].nodeName) = 'INT4') or (UpperCase(Node.childNodes[0].nodeName) = 'I4') then
      aValue.SetItem(StrToInt(Node.childNodes[0].TextContent))

   // Double
   else if UpperCase(Node.childNodes[0].nodeName) = 'DOUBLE' then
      aValue.SetItem(StrToFloat(Node.childNodes[0].TextContent))

   // DateTime
   else if UpperCase(Node.childNodes[0].nodeName) = 'DATETIME.ISO8601' then
      aValue.SetItem(ISOToDateTime(Node.childNodes[0].TextContent))

   // BASE64
   else if UpperCase(Node.childNodes[0].nodeName) = 'BASE64' then
   begin
      if Assigned(Node.childNodes[0].attributes.getNamedItem('cid')) then
         aValue.CID := Node.childNodes[0].attributes.getNamedItem('cid').TextContent
      else
        aValue.SetItemBase64Data(Node.childNodes[0].TextContent);
   end

   // Boolean
   else if UpperCase(Node.childNodes[0].nodeName) = 'BOOLEAN' then
   begin
      if Node.childNodes[0].TextContent = '0' then
         aValue.SetItem(false)
      else
         aValue.SetItem(true);
   end

   // Nil
   else if UpperCase(Node.childNodes[0].nodeName) = 'NIL' then
   begin
      aValue.SetNil;
   end

   // Array
   else if UpperCase(Node.childNodes[0].nodeName) = 'ARRAY' then
   begin
      Ar := TxmlRpcArray.Create;
      aValue.SetItem(Ar);
      nL := Node.childNodes[0].childnodes[0].ChildNodes;
      if Assigned(nL) then
      begin
         for I := 0 to pred(nL.Count) do
            DataTag(nL.item[I], Ar.AddItem);
      end;
   end

   // Struct
   else if UpperCase(Node.childNodes[0].nodeName) = 'STRUCT' then
   begin
      Struct := TxmlRpcStruct.Create;

      nL := Node.childNodes[0].ChildNodes;
      if Assigned(nL) then
      begin
         for I := 0 to pred(nL.Count) do
         begin
            if nL.item[i].ChildNodes.Item[0].NodeName = 'name' then
            begin
               iE1 := nL.item[i].ChildNodes.Item[0];
               iE2 := nL.item[i].ChildNodes.Item[1];
            end
            else
            begin
               iE1 := nL.item[i].ChildNodes.Item[1];
               iE2 := nL.item[i].ChildNodes.Item[0];
            end;

            if Assigned(iE1) and Assigned(iE2) then
               DataTag(iE2, Struct.AddItem(iE1.TextContent))
            else
               raise Exception.CreateFmt('error in RPC-XML data: STRUCT element corrupt %s', [nL.item[i].TextContent]); // todo: fixme xml in error
         end;
      end;

      // check for error struct
      {
         <struct>
           <member>
             <name>faultCode</name>
             <value><int>999</int></value>
           </member>
           <member>
             <name>faultString</name>
             <value><string><![CDATA[Requested method was not registered on the server]]></string></value>
           </member>
         </struct>
      }
      if Struct.HasKey('faultCode') and Struct.HasKey('faultString') and (Struct.Count = 2) then
      begin
         aValue.SetErrorCode(Struct.Keys['faultCode'].GetInteger);
         aValue.SetErrorString(Struct.Keys['faultString'].GetString);
         FreeAndNil(Struct);
      end
      else
         aValue.SetItem(Struct);
   end

   // unknown var type
   else
      raise Exception.CreateFmt('unknown XML-RPC value-type %s', [Node.nodeName]);
end;

constructor TFPCxmlRpcParser.Create(xmlRpcParserType: TxmlRpcParserType);
begin
   FxmlRpcParserType := xmlRpcParserType;

   if xmlRpcParserType = xmlRpcServer then
      FObjectList := TxmlRpcParamList.Create
   else
      FxmlRpcResult := TxmlRpcResult.Create;
end;

destructor TFPCxmlRpcParser.Destroy;
var
   i: Integer;
   Param: TxmlRpcDataElement;

begin
   if Assigned(FObjectList) then
   begin
      for i := pred(FObjectList.Count) downto 0 do
      begin
         Param := FObjectList.Items[i];
         FreeAndNil(Param);
         FObjectList.Delete(i);
      end;
      FreeAndNil(FObjectList);
   end;
   if Assigned(FxmlRpcResult) then
      FreeAndNil(FxmlRpcResult);

   inherited;
end;

procedure TFPCxmlRpcParser.Parse(sdata: string);
var
  S: TStringStream;
  iE: TDOMNode;
  i: Integer;
  sTag: String;
  nL: TDOMNodeList;

begin
  if FDebug then
     debugXMLRPCDataResponse := debugXMLRPCDataResponse + #13#10 + sdata;

  S := TStringStream.Create(sdata);
  S.Position := 0;
  FDOM := nil;
  ReadXMLFile(FDOM, TStream(S));

{  <?xml version="1.0"?>
   <methodCall>
      <methodName>emSys.getTasks</methodName>
      <params>
         <param>
            <value>
               <int>0</int>
            </value>
         </param>
      </params>
   </methodCall>

   <?xml version="1.0"?>
   <methodResponse>
      <params>
         <param>
            <value>
               <array>
                  <data>
                     <value>
                        <int>2</int>
                     </value>
                     <value>
                        <boolean>0</boolean>
                     </value>
                     <value>
                        <dateTime.iso8601>20041231T12:19:09</dateTime.iso8601>
                     </value>
                  </data>
               </array>
            </value>
         </param>
      </params>
   </methodResponse>
}
  // aufgerufene Methode
  iE := FDOM.DocumentElement.FindNode('methodName');
  if Assigned(iE) then
     FMethodName := iE.NodeValue;

  iE := FDOM.DocumentElement.FindNode('fault');
  if Assigned(iE) then
  begin
{
   <?xml version="1.0"?>
   <methodResponse>
     <fault>
       <value>
         <struct>
           <member>
             <name>faultCode</name>
             <value><int>999</int></value>
           </member>
           <member>
             <name>faultString</name>
             <value><string><![CDATA[Requested method was not registered on the server]]></string></value>
           </member>
         </struct>
         </value>
     </fault>
   </methodResponse>
}

     DataTag(iE.ChildNodes.Item[0], FxmlRpcResult.Data);
     exit;
  end;

  // parameter lesen
  nL := FDom.ChildNodes.Item[0].ChildNodes.Item[0].ChildNodes;
  if Assigned(nL) then
  begin
     for i := 0 to pred(nL.Count) do
     begin
        sTag := UpperCase(nL.item[i].nodeName);
        // parameter
        if(sTag = 'PARAM') then
        begin
           // act as server
           if FxmlRpcParserType = xmlRpcServer then
           begin
              FObjectList.Add(TxmlRpcDataElement.Create);
              if UpperCase(nL.item[i].childNodes.item[0].nodeName) = 'VALUE' then
                 DataTag(nL.item[i].childNodes.item[0], FObjectList.Items[pred(FObjectList.Count)]);
           end
           // act as cliesnt
           else
           begin
              if nL.Count > 1 then
                 raise Exception.CreateFmt('error in RPC-XML data: only one PARAM expected, but %d found', [nL.Count]);
              DataTag(nL.item[i].childNodes.item[0], FxmlRpcResult.Data);
           end;
        end
        else
           raise Exception.CreateFmt('error in RPC-XML data: PARAM expected but %s found', [sTag]);
     end;
  end;
end;

function TFPCxmlRpcParser.GetResult: TxmlRpcResult;
begin
   Result := FxmlRpcResult;
end;

{ TxmlRpcCaller }

constructor TxmlRpcCaller.Create;
begin
   inherited Create(xmlRpcClient);
   FHostPort := 80;
   FUseSSL := false;
   FProxyBasicAuth := false;
   FCachePath := GetTempDir;
   FDebug := false;
end;

destructor TxmlRpcCaller.Destroy;
begin
   inherited;
end;

procedure TxmlRpcCaller.DeleteOldCache(ttl: integer);
var
   SR : tSearchRec;

begin
   if FindFirst(IncludeTrailingPathDelimiter(FCachePath) + '*.csh', faAnyFile, SR) = 0 then
   repeat
       if (SR.attr and faDirectory = 0) then
       begin
          if ttl <= 0 then
             DeleteFile(IncludeTrailingPathDelimiter(FCachePath) + SR.Name)
          else
             if FileIsExpired(IncludeTrailingPathDelimiter(FCachePath) + SR.Name,ttl) then
                DeleteFile(IncludeTrailingPathDelimiter(FCachePath) + SR.Name);
       end;
   until FindNext(SR) <> 0;
   FindClose(SR);
end;

// CACHED WEB CALL with ttl in Milliseconds
function TxmlRpcCaller.Execute(Data: TxmlRpcFunction; ttl: integer): TxmlRpcResult;
var
   sl: TStrings;
   rs: string;
   rq: string;
   fn: string;
   //md : TIdHashMessageDigest5;

begin
(*
   rq := Data.GetRequestXML;
   if fDebug then
      debugXMLRPCDataRequest := debugXMLRPCDataRequest + #13#10 + Data.GetRequestXML;

   md := TIdHashMessageDigest5.Create;
   try
      { determine the md5 digest hash of the request }
      //fn := Hash128AsHex(md.HashValue(rq));
      fn := md.HashBytesAsHex(md.HashString(rq));
   finally
      FreeAndNil(md);
   end;

   sl := TStringList.Create;
   try
      { if we have a cached file from a previous request
        that has not expired then load it }
      if FileExists(IncludeTrailingPathDelimiter(FCachePath) + fn + '.csh') then
      begin
         if not FileIsExpired(IncludeTrailingPathDelimiter(FCachePath) + fn + '.csh',ttl) then
         begin
            sl.LoadFromFile(IncludeTrailingPathDelimiter(FCachePath) + fn + '.csh');

            if(FFixEmptyString) then
              Parse(FixEmptyString(sl.Text))
            else
              Parse(sl.Text);

            Data.Clear;
            Result := GetResult;
            exit;
         end;
      end;

      { ok we got here so we where expired or did not exist
        make the call and cache the result this time }
      rs := Post(Data);

      // empty string fix
      if (FFixEmptyString) then
         rs := FixEmptyString(rs);

      // simple error check
      if not (pos('xml', rs)  > 0) then
      begin
        Result := TxmlRpcResult.Create;
        Result.Data.SetErrorCode(ERROR_INVALID_RESPONSE);
        Result.Data.SetErrorString(ERROR_INVALID_RESPONSE_MESSAGE + rs);
        EXIT;
      end;

      // empty response
      if (trim(rs) = '') then
      begin
        Result := TxmlRpcResult.Create;
        Result.Data.SetErrorCode(ERROR_EMPTY_RESULT);
        Result.Data.SetErrorString(ERROR_EMPTY_RESULT_MESSAGE);
        EXIT;
      end;

      Parse(rs);

      sl.Text := rs;
      sl.SaveToFile(IncludeTrailingPathDelimiter(FCachePath) + fn + '.csh');

   finally
      sl.Free;
   end;

   Data.Clear;
   Result := GetResult;
*)
end;

// NON - CACHED WEB CALL
function TxmlRpcCaller.Execute(Data: TxmlRpcFunction): TxmlRpcResult;
var
   rs: string;

begin
   rs := Post(Data);
   if fDebug then
      debugXMLRPCDataRequest := debugXMLRPCDataRequest + #13#10 + Data.GetRequestXML;

   // empty string fix
   if (FFixEmptyString) then
      rs := FixEmptyString(rs);

   // simple error check
   if not (pos('xml', rs)  > 0) then
   begin
     Result := TxmlRpcResult.Create;
     Result.Data.SetErrorCode(ERROR_INVALID_RESPONSE);
     Result.Data.SetErrorString(ERROR_INVALID_RESPONSE_MESSAGE + rs);
     EXIT;
   end;

   // empty response
   if (trim(rs) = '') then
   begin
     Result := TxmlRpcResult.Create;
     Result.Data.SetErrorCode(ERROR_EMPTY_RESULT);
     Result.Data.SetErrorString(ERROR_EMPTY_RESULT_MESSAGE);
     EXIT;
   end;

   Parse(rs);
   Data.Clear;
   Result := GetResult;
end;

function GenerateCID : String;
var
   guid: TGuid;

begin
   CreateGUID(guid);
   Result := StringReplace(StringReplace(GUIDToString(guid), '{', '', []), '}', '', []);  // todo: fixme => war + @indyhostname
end;

function TxmlRpcCaller.Post(Data: TxmlRpcFunction): string;

   function encodeBase64(encodedata: String) : String;
   var
     DecodedStream: TStringStream;
     EncodedStream: TStringStream;
     Encoder: TBase64EncodingStream;

   begin
     DecodedStream := TStringStream.Create(encodedata);
     EncodedStream := TStringStream.Create('');
     Encoder       := TBase64EncodingStream.Create(EncodedStream);
     Encoder.CopyFrom(DecodedStream, DecodedStream.Size);
     Encoder.Free;

     Result := EncodedStream.DataString;

     DecodedStream.Free;
     EncodedStream.Free;
   end;

   var
      HttpClient: TLHTTPClient;

begin
   // HasFilesAssigned := Data.HasFilesAssigned;
   FValidResponse := false;

   FXMLRequest := TStringStream.Create(data.GetRequestXML);
   FXMLResponse := TStringStream.Create('');
   try
      HttpClient := TLHTTPClient.Create(nil);
      HttpClient.Host := FHostName;
      HttpClient.Method := hmPost;
      HttpClient.Port := FHostPort;
      HttpClient.URI := FEndPoint;
      HttpClient.Timeout := -1;
      HttpClient.OnDisconnect := ClientDisconnect;
      HttpClient.OnDoneInput := ClientDoneInput;
      HttpClient.OnError := ClientError;
      HttpClient.OnInput := ClientInput;
      HttpClient.OnCanWrite := ClientCanWrite;
      HttpClient.OnProcessHeaders := ClientProcessHeaders;
      HttpClient.AddExtraHeader('Content-Type: text/xml');
      HttpClient.AddExtraHeader('Content-Length: ' + IntToStr(FXMLRequest.Size));
      if FUserName <> '' then
         HttpClient.AddExtraHeader('Authorization: Basic ' + encodeBase64(FUserName + ':' + FPassword));
      FXMLRequest.Position := 0;
      HttpClient.SendRequest;
      FDone := false;
      while not FDone do
         HttpClient.CallAction;

      if not FValidResponse then
         raise Exception.Create(FResponse);

      Result := FXMLResponse.DataString;
   finally
      HttpClient.Free;
      FXMLRequest.Free;
      FXMLResponse.Free;
   end;
end;

procedure TxmlRpcCaller.ClientDisconnect(ASocket: TLSocket);
begin
   FDone := true;
end;

procedure TxmlRpcCaller.ClientDoneInput(ASocket: TLHTTPClientSocket);
begin
   if ASocket.Connected then;
      ASocket.Disconnect;
end;

procedure TxmlRpcCaller.ClientError(const Msg: string; aSocket: TLSocket);
begin
   //ShowMessage('ERROR: ' + Msg);
end;

function TxmlRpcCaller.ClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: Integer): Integer;
begin
   FXMLResponse.Write(ABuffer^, ASize);
   Result := ASize;
end;

procedure TxmlRpcCaller.ClientProcessHeaders(ASocket: TLHTTPClientSocket);
begin
   if HTTPStatusCodes[ASocket.ResponseStatus] = 200 then
   begin
      FValidResponse := true;
   end
   else
      FResponse := IntToStr(HTTPStatusCodes[ASocket.ResponseStatus]) + ' ' + ASocket.ResponseReason;
end;

procedure TxmlRpcCaller.ClientCanWrite(ASocket: TLHTTPClientSocket; var OutputEof: TWriteBlockStatus);
var
   data: Pointer;
   read, wrote: Integer;

begin
   if FXMLRequest.Position < FXMLRequest.Size then
   begin
      try
         data := GetMem(1024);
         read := FXMLRequest.Read(data^, 1024);

         wrote := ASocket.Send(data^, read);
         if read <> wrote then
            FXMLRequest.Position := FXMLRequest.Position - (read - wrote)
         else
         begin
            if FXMLRequest.Position = FXMLRequest.Size then
               OutputEof := wsDone
            else
               OutputEof := wsPendingData;
         end;
      finally
         FreeMem(data);
      end;
   end
   else
      OutputEof := wsDone;
end;

function XMLRpcFunctionCall(const UseSSL: boolean; const Host: String; const Port: Integer; const URL, User, Passwd, RPCFunction: string; Params: array of const; {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr: TIdClientTCPConnectionMgr;{$ENDIF}{$ENDIF} RaiseException: boolean; Debug: boolean=false) : TxmlRpcDataElement;
var
   Arr: TxmlRpcArray;

begin
   Arr := TxmlRpcArray.Create;
   try
      ArrayToXMLRpcArray(Arr, Params);
      Result := XMLRpcFunctionCall(UseSSL, Host, Port, URL, User, Passwd, RPCFunction, Arr, {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr,{$ENDIF}{$ENDIF} RaiseException, Debug);

   finally
      FreeAndNil(Arr);
   end;
end;

function XMLRpcFunctionCall(const UseSSL: boolean; const Host: String; const Port: Integer; const URL, User, Passwd, RPCFunction: string; Params: TxmlRpcArray; {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr: TIdClientTCPConnectionMgr;{$ENDIF}{$ENDIF} RaiseException: boolean; Debug: boolean=false) : TxmlRpcDataElement;
var
   Caller: TxmlRpcCaller;
   Func: TxmlRpcFunction;
   rst: TxmlRpcResult;
   I: Integer;
   
begin
   Result := TxmlRpcDataElement.Create;
   try
      Caller := TxmlRpcCaller.Create;
      try
         if Host = '' then
            raise Exception.Create('no host supplied');
         Caller.HostName := Host;
         Caller.HostPort := Port;
         Caller.EndPoint := URL;
         Caller.Debug := Debug;
         if Port < 0 then
            raise Exception.Create('port lower 0 not supported');

         if User <> '' then
         begin
            Caller.Username := User;
            Caller.Password := Passwd;
            Caller.BasicAuth := true;
         end;

         Func := TxmlRpcFunction.Create;
         try
            Func.ObjectMethod := RPCFunction;

            // pass all vars to the function
            if assigned(Params) then
               for I := 0 to pred(Params.Count) do
                  Func.AddParam.Assign(Params.Item[I]);

            // call the function
            rst := Caller.Execute(Func);
            Result.Assign(rst.Data);

         finally
            //rst := nil;
            FreeAndNil(Func);
         end;

      finally
        FreeAndNil(Caller);
      end;

   except
      on E: Exception do
      begin
         Result.SetErrorCode(-1);
         Result.SetErrorString(e.Message);
      end;
   end;

   if RaiseException and Result.IsError then
      raise Exception.Create(Result.GetErrorString);
end;

function XMLRpcSupportMulticall(const UseSSL: boolean; const Host: String; const Port: Integer; const URL, User, Passwd: string; {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr: TIdClientTCPConnectionMgr;{$ENDIF}{$ENDIF} Debug: boolean=false) : TxmlRpcMulticallSupport;
var
   FuncResult: TxmlRpcDataElement;
   Ar: TxmlRpcArray;
   i: Integer;
   Data: Variant;
   
begin
   Result := mcNo;
   try
      Data := VarArrayCreate([-1, -1], varVariant);
      FuncResult := XMLRpcFunctionCall(UseSSL, Host, Port, URL, User, Passwd, 'system.multicall', [Data], {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr,{$ENDIF}{$ENDIF} false, Debug);
      if not FuncResult.IsError then
         Result := mcYes;
      FreeAndNil(FuncResult);

      if Result = mcNo then
      begin
         Funcresult := XMLRpcFunctionCall(UseSSL, Host, Port, URL, User, Passwd, 'system.listMethods', [], {$IFNDEF USE_IE}{$IFNDEF FPC}ConnectionMgr,{$ENDIF}{$ENDIF} false, Debug);
         if FuncResult.IsArray then
         begin
            Ar := FuncResult.GetArray;
            for I := 0 to pred(Ar.Count) do
            begin
               if LowerCase(Ar.Item[I].GetString) = 'system.multicall' then
               begin
                  Result := mcYes;
                  Break;
               end;
            end;
         end;
         FreeAndNil(FuncResult);
      end;
   except
   end;
end;


{ TxmlRpcMultiCaller }

function TxmlRpcMultiCaller.AddFunctionCall(Data: TxmlRpcFunction) : TxmlRpcFunction;
var
   Struct: TxmlRpcStruct;
   Ar: TxmlRpcArray;
   I: Integer;

begin
   Struct := TxmlRpcStruct.Create;
   Struct.AddItem('methodName').SetItem(Data.ObjectMethod);
   Ar := TxmlRpcArray.Create;
   Struct.AddItem('params').SetItem(Ar);

   for I := 0 to pred(Data.ParamCount) do
      Ar.AddItem(Data.Item[i]);

   FFunctionArray.AddItem.SetItem(Struct);

   Result := Data;
end;

procedure TxmlRpcMultiCaller.AddFunctionCall(ObjectMethod: String; Params: array of const);
var
   Struct: TxmlRpcStruct;
   Ar: TxmlRpcArray;

begin
   Struct := TxmlRpcStruct.Create;
   Struct.AddItem('methodName').SetItem(ObjectMethod);
   Ar := TxmlRpcArray.Create;
   Struct.AddItem('params').SetItem(Ar);

   ArrayToXMLRpcArray(Ar, Params);

   FFunctionArray.AddItem.SetItem(Struct);
end;

function TxmlRpcMultiCaller.CallCount: Integer;
begin
   Result := FFunctionArray.Count;
end;

constructor TxmlRpcMultiCaller.Create(ServerSupportsMulticall: TxmlRpcMulticallSupport=mcDontKnow);
begin
   inherited Create;
   FMulti := TxmlRpcFunction.Create;
   FMulti.ObjectMethod := 'system.multicall';
   FFunctionArray := TxmlRpcArray.Create;
   FMulti.AddParam.SetItem(FFunctionArray);
   FMultiCallSupport := ServerSupportsMulticall;
end;

destructor TxmlRpcMultiCaller.Destroy;
begin
   inherited;

   if FMultiCallSupport = mcNo then
      FreeAndNil(FResult);
   FreeAndNil(FMulti);
end;

procedure TxmlRpcMultiCaller.Execute;
var
   Arr, Con: TxmlRpcArray;
   i: integer;
   Data: TxmlRpcDataElement;
   
begin
   if FFunctionArray.Count = 0 then
      raise Exception.Create('no function to call');

   if FMultiCallSupport = mcDontKnow then
      FMultiCallSupport := XMLRpcSupportMulticall(self.UseSSL, HostName, self.HostPort, self.EndPoint, self.Username, self.Password{$IFNDEF USE_IE}{$IFNDEF FPC}, self.ConnectionMgr{$ENDIF}{$ENDIF});

   // server supports multicall, so do boxcarrying
   if FMultiCallSupport = mcYes then
   begin
      FResult := inherited Execute(FMulti);
      if FResult.Data.IsError then
         raise Exception.CreateFmt('an error occured on a multicall: #%d %s ', [FResult.Data.GetErrorCode, FResult.Data.GetErrorString]);

      if not FResult.Data.IsArray then
         raise Exception.Create('an error occured on a multicall: result is not an array as expected');
   end
   // server does not support multicall
   else
   begin
      FResult := TxmlRpcResult.Create;
      Arr := TxmlRpcArray.Create;
      FResult.Data.SetItem(Arr);

      for i := 0 to pred(FFunctionArray.Count) do
      begin
         Data := XMLRpcFunctionCall(self.UseSSL, HostName, self.HostPort, self.EndPoint, self.Username, self.Password, FFunctionArray.Item[i].GetStruct.Keys['methodName'].GetString, FFunctionArray.Item[i].GetStruct.Keys['params'].getArray, {$IFNDEF USE_IE}{$IFNDEF FPC}self.ConnectionMgr,{$ENDIF}{$ENDIF} self.Debug);
         if Data.IsError then
            Arr.AddItem(Data)
         else
         begin
            Con := TxmlRpcArray.Create;
            Arr.AddItem.SetItem(Con);
            Con.AddItem(Data);
         end;
         FreeAndNil(Data);
      end;
   end;
end;

function TxmlRpcMultiCaller.getResult(index: integer): TxmlRpcDataElement;
begin
   if (index < 0) or (index > pred(FResult.Data.GetArray.Count)) then
      raise Exception.Create('index out of bounds on result array');

   Result := FResult.Data.GetArray.Item[index];
end;

function TxmlRpcMultiCaller.ResultCount: Integer;
begin
   Result := FResult.Data.GetArray.Count;
end;
end.
