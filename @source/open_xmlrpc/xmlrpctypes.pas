unit xmlrpctypes;

{$mode delphi}

{
   <c> media factory gmbh, luebeck germany
   Henrik Genssen hg@mediafactory.de
   license: GPL

   ToDo:
   XML on each Element itself
   implement VarArrayToXMLRpcArray
   option when to uses the multipart version and when to use RFC handling
}

interface
uses
  classes,
  Contnrs,
  sysutils,
  xmlrpccommon;

type
  TxmlRpcArray = class;
  TxmlRpcStruct = class;

  TxmlRpcDataType = (dtDouble, dtInteger, dtString, dtStringFile, dtBoolean, dtDate, dtBase64, dtBase64File, dtStruct, dtArray, dtError, dtNone, dtName, dtSingle, dtValue);

  { TxmlRpcDataElement }

  TxmlRpcDataElement = class
     private
        FType: TxmlRpcDataType;
        FString: string;
        FStruct: TxmlRpcStruct;
        FArray: TxmlRpcArray;
        FInteger: integer;
        FDouble: Extended;
        FBoolean: boolean;
        FDate: TDateTime;
        FBase64: string;
        FFileName: String;
        FDeleteAfterRead: Boolean;
        FErrorString: String;
        FErrorCode: integer;
        FCid: String;
        FNil: boolean;

     public
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TxmlRpcDataElement); virtual;
        procedure SetItem(value: string); overload;
        procedure SetItem(value: Extended); overload;
        procedure SetItem(value: integer); overload;
        procedure SetItem(value: boolean); overload;
        procedure SetItem(value: TxmlRpcStruct); overload;
        procedure SetItem(value: TxmlRpcArray); overload;
        procedure SetItem(value: TDateTime); overload;
        procedure SetItemBase64Data(value: string);
        procedure SetItemBase64ByFile(filename: string; DeleteAfterRead: Boolean=false);
        procedure SetItemStringByFile(filename: string; DeleteAfterRead: Boolean=false);
        procedure SetErrorCode(value: integer);
        procedure SetErrorString(value: string);
        procedure SetNil();
        function GetBase64String: string;
        function GetBase64Raw: string;
        function GetBase64Stream: TMemoryStream;
        function GetString: string;
        function GetInteger: integer;
        function GetDouble: double;
        function GetDate: TDateTime;
        function GetBoolean: boolean;
        function GetStruct: TxmlRpcStruct;
        function GetArray: TxmlRpcArray;
        function GetErrorCode: integer;
        function GetErrorString: string;
        function GetFileName: String;
        procedure SetError(Number: integer; Reason: String); 
        procedure PutBase64ToFile(filename: string);
        procedure PutStringToFile(filename: string);
        function IsError: boolean;
        function IsInteger: boolean;
        function IsString: boolean;
        function IsDouble: boolean;
        function IsDate: boolean;
        function IsBoolean: boolean;
        function IsStruct: boolean;
        function IsArray: boolean;
        function IsBase64: boolean;
        function IsFile: boolean;
        function IsNil: boolean;

        property xmlRpcDataType: TxmlRpcDataType read FType write FType;
        property CID: String read FCID write FCID;
  end;

  TxmlRpcArrayItem = TxmlRpcDataElement;

  TxmlRpcArray = class
     private
        FList: TObjectList;
        function getItem(index: integer) : TxmlRpcArrayItem;
        procedure setItem(index: integer; Value: TxmlRpcArrayItem);

     public
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TxmlRpcArray);
        function AddItem: TxmlRpcArrayItem; overload;
        function AddItem(Data: TxmlRpcDataElement) : TxmlRpcArrayItem; overload;
        function Count: integer;
        function GetAsXML : string;
        procedure LoadRawData(dt: TxmlRpcDataType; value: string);
        procedure Delete(index: integer);
        procedure Clear;

        property Item[index: integer] : TxmlRpcArrayItem read getItem write setItem;
  end;

  TxmlRpcStructItem = class(TxmlRpcDataElement)
     private
        FName: string;

     public
        procedure Assign(Source: TxmlRpcDataElement); override;
  end;

  TxmlRpcStruct = class
     private
        FItemList: TStringList;

        function getItem(index: integer) : TxmlRpcStructItem;
        procedure setItem(index: integer; Value: TxmlRpcStructItem);
        function getItemByKey(index: string) : TxmlRpcStructItem;
        procedure setItemByKey(index: string; Value: TxmlRpcStructItem);

     public
       constructor Create;
       destructor Destroy;override;
       procedure Assign(Source: TxmlRpcStruct);
       function AddItem(key: string) : TxmlRpcStructItem;
       procedure AddStrings(AList: TStrings);
       function GetKeyList: TStringList;
       function HasKey(key: string): boolean;
       procedure Delete(key: string);
       procedure Clear;
       procedure LoadRawData(dt: TxmlRpcDataType; key: string; value: string);
       function Count: integer;
       function GetAsXML: string;

       property Item[index: integer] : TxmlRpcStructItem read getItem write setItem;
       property Keys[index: string] : TxmlRpcStructItem read getItemByKey write setItemByKey;
  end;

  TxmlRpcFunctionItem = TxmlRpcDataElement;

  TxmlRpcFunction = class
     private
        FList: TObjectList;
        FMethod: string;
        FErrorCode: integer;
        FErrorMessage: string;
        function getItem(index: integer) : TxmlRpcFunctionItem;
        procedure setItem(index: integer; Value: TxmlRpcFunctionItem);

     public
       constructor Create; overload;
       constructor Create(ObjectMethod: String; Params: array of const); overload;
       destructor Destroy; override;
       procedure Clear;
       property ObjectMethod: string read FMethod write FMethod;
       function AddParam : TxmlRpcFunctionItem; overload;
       function AddParam(Item: TxmlRpcFunctionItem) : TxmlRpcFunctionItem; overload;
       procedure AddError(code: integer; msg: string);
       function GetRequestXML: string;
       function GetResponseXML:string;
       function GetErrorXML: string;
       function ParamCount: Integer;
       function HasFilesAssigned: boolean;

       property Item[index: integer] : TxmlRpcFunctionItem read getItem write setItem;
    end;

  TxmlRpcResult = class
     private
        FData: TxmlRpcDataElement;

     public
        constructor Create;
        destructor Destroy; override;
        procedure Clear;
        procedure Assign(Source: TxmlRpcResult); 

        property Data: TxmlRpcDataElement read FData write FData;
    end;

   TxmlRpcParameter = TxmlRpcResult;
   TxmlRpcReturn = TxmlRpcFunction;

    TxmlRpcParamList = class(TList)
       private
          function Get(Index: Integer): TxmlRpcDataElement;
          procedure Put(Index: Integer; const Value: TxmlRpcDataElement);

       public
          function Add(Item: TxmlRpcDataElement): Integer;
          property Items[Index: Integer]: TxmlRpcDataElement read Get write Put; default;
    end;

    procedure ArrayToXMLRpcParamList(Func: TxmlRpcFunction; Params: array of const);
    procedure ArrayToXMLRpcArray(Arr: TxmlRpcArray; Params: array of const);
    procedure VarArrayToXMLRpcArray(Arr: TxmlRpcArray; Params: Variant);
    procedure OLEVariantToXMLRpcItem(Value: OLEVariant; Item: TxmlRpcDataElement);
    procedure XMLRpcParamListToArray(plist: TxmlRpcParamList; var Params: OLEVariant);

implementation

uses Variants;

procedure ArrayToXMLRpcParamList(Func: TxmlRpcFunction; Params: array of const);
var I: Integer;
begin
   // pass all vars to the function
   for I := 0 to High(Params) do
   with Params[I] do
     case VType of
       vtInteger:    Func.AddParam.SetItem(VInteger);
       vtBoolean:    Func.AddParam.SetItem(VBoolean);
       vtChar:       Func.AddParam.SetItem(VChar);
       vtExtended:   Func.AddParam.SetItem(VExtended^);

       vtString:     Func.AddParam.SetItem(VString^);
       vtPChar:      Func.AddParam.SetItem(VPChar);
       vtObject:     raise Exception.Create('object not supported as param for XML-RPC');
       vtClass:      raise Exception.Create('class not supported as param for XML-RPC');
       vtAnsiString: Func.AddParam.SetItem(string(VAnsiString));
       //vtCurrency:   Func.AddParam.SetItem(VCurrency^);      // todo: fixme!!
       vtVariant:    raise Exception.Create('variant not supported as param for XML-RPC');
       vtInt64:      Func.AddParam.SetItem(VInt64^);

   end;
end;

procedure ArrayToXMLRpcArray(Arr: TxmlRpcArray; Params: array of const);
var
   I: Integer;
   varArr: TxmlRpcArray;

begin
   if not Assigned(Arr) then
      Arr := TxmlRpcArray.Create;

   // pass all vars to the function                  
   for I := 0 to High(Params) do
   with Params[I] do
     case VType of
       vtInteger:    Arr.AddItem.SetItem(VInteger);
       vtBoolean:    Arr.AddItem.SetItem(VBoolean);
       vtChar:       Arr.AddItem.SetItem(VChar);
       vtExtended:   Arr.AddItem.SetItem(VExtended^);
       vtWideString: Arr.AddItem.SetItem(VString^);
       vtString:     Arr.AddItem.SetItem(VString^);
       vtPChar:      Arr.AddItem.SetItem(VPChar);
       vtObject:     raise Exception.Create('object not supported as param for XML-RPC');
       vtClass:      raise Exception.Create('class not supported as param for XML-RPC');
       vtAnsiString: Arr.AddItem.SetItem(string(VAnsiString));
       //vtCurrency:   Arr.AddItem.SetItem(VCurrency^); // todo: fixme
       vtVariant:
       begin
          if VarIsArray(VVariant^) then
          begin
             varArr := TxmlRpcArray.Create;
             VarArrayToXMLRpcArray(varArr, VVariant^);
             Arr.AddItem.SetItem(varArr);
          end
          else
             raise Exception.Create('variant not supported as param for XML-RPC');
       end;
       vtInt64:      Arr.AddItem.SetItem(VInt64^);

   end;
end;

procedure VarArrayToXMLRpcArray(Arr: TxmlRpcArray; Params: Variant);
var v: Integer;
begin
   if not Assigned(Arr) then
      Arr := TxmlRpcArray.Create;

   if VarIsArray(Params) then
   begin
      for v := 0 to VarArrayHighBound(Params, 1) do
         OLEVariantToXMLRpcItem(Params[v], Arr.AddItem);
   end
   else
      raise Exception.Create('variant is not an array');

end;

procedure OLEVariantToXMLRpcItem(Value: OLEVariant; Item: TxmlRpcDataElement);
var
  Arr: TxmlRpcArray;
  
begin
   case VarType(Value) of
      varEmpty: ;
      varNull: ;
      varSmallInt: Item.SetItem(StrToInt(varToStr(Value)));
      varInteger:  Item.SetItem(StrToInt(varToStr(Value)));
      varSingle:   ;
      varDouble:   ;
      varCurrency: ;
      varDate:     Item.SetItem(VarToDateTime(Value));
      varOleStr:   Item.SetItem(varToStr(Value));
      varDispatch: ;
      varError:    ;
      varBoolean:  if Value then Item.SetItem(true) else Item.SetItem(false);
      varUnknown:  ;
      varShortInt: Item.SetItem(StrToInt(varToStr(Value)));
      varByte:     ;
      varWord:     ;
      varLongWord: ;
      varInt64:    Item.SetItem(StrToInt(varToStr(Value)));
      varString:   Item.SetItem(varToStr(Value));
      varAny:      ;
      varByRef:    ;
   end;

   if VarType(Value) >= varArray then
   begin
      Arr := TxmlRpcArray.Create;
      VarArrayToXMLRpcArray(Arr, Value);
      Item.SetItem(Arr);
   end;
end;

procedure XMLRpcParamListToArray(plist: TxmlRpcParamList; var Params: OLEVariant);
var
   i: integer;
   
begin
   Params := VarArrayCreate([0, plist.Count - 1], varvariant);
   for i := 0 to pred(plist.Count) do
   begin
      case plist.Items[i].xmlRpcDataType of
         dtDouble:     Params[i] := plist.Items[i].GetDouble;
         dtInteger:    Params[i] := plist.Items[i].GetInteger;
         dtString:     Params[i] := plist.Items[i].GetString;
         dtStringFile: ;
         dtBoolean:    Params[i] := plist.Items[i].GetBoolean;
         dtDate:       Params[i] := plist.Items[i].GetDate;
         dtBase64:     ;
         dtBase64File: ;
         dtStruct:     ;
         dtArray:      ;
         dtError:      ;
         dtNone:       ;
         dtName:       ;
         dtSingle:     Params[i] := plist.Items[i].GetDouble;
         dtValue:;
      end;
   end;
end;

{ TxmlRpcDataElement }

procedure TxmlRpcDataElement.Assign(Source: TxmlRpcDataElement);
begin
    FType := Source.FType;
    FString := Source.FString ;
    if Assigned(Source.FStruct) then
    begin
       if Assigned(FStruct) then
          FreeAndNil(FStruct);
       FStruct := TxmlRpcStruct.Create;
       FStruct.Assign(Source.FStruct);
    end;
    if Assigned(Source.FArray) then
    begin
       if Assigned(FArray) then
          FreeAndNil(FArray);
       FArray := TxmlRpcArray.Create;
       FArray.Assign(Source.FArray);
    end;
    FInteger := Source.FInteger;
    FDouble := Source.FDouble;
    FBoolean := Source.FBoolean;
    FDate := Source.FDate;
    FBase64 := Source.FBase64;
    FErrorString := Source.FErrorString ;
    FErrorCode := Source.FErrorCode;
    FFileName := Source.FFileName;
    FDeleteAfterRead := Source.FDeleteAfterRead;
    FCid := Source.FCid;
end;

constructor TxmlRpcDataElement.Create;
begin
   FNil := false;
end;

destructor TxmlRpcDataElement.Destroy;
begin
  if FDeleteAfterRead and FileExists(FFileName) then
     DeleteFile(FFileName);
  FreeAndNil(FStruct);
  FreeAndNil(FArray);
  inherited;
end;

function TxmlRpcDataElement.GetArray: TxmlRpcArray;
begin
   Result := FArray;
end;

function TxmlRpcDataElement.GetBase64Raw: string;
begin
   if (FType = dtBase64) then
      result := FBase64
   else
      raise Exception.Create('TResult.GetBase64String - Item is not a base64 type')
end;

function TxmlRpcDataElement.GetBase64Stream: TMemoryStream;
begin
   if (FType = dtBase64) then
   begin
      result := TMemoryStream.Create;
      //StringToStream(TIdDecoderMIME.DecodeString(FBase64), result);
      //StringToStream(DecodeString(TIdDecoderMIME, FBase64), result);
   end
   else
      raise Exception.Create('TResult.GetBaseStream - Item is not a base64 type');
end;

function TxmlRpcDataElement.GetBase64String: string;
begin
   if (FType = dtBase64) then
      //result := TIdDecoderMIME.DecodeString(FBase64)
      //result := DecodeString(TIdDecoderMIME, FBase64)
   else
      raise Exception.Create('TResult.GetBase64String - Item is not a base64 type')
end;

function TxmlRpcDataElement.GetBoolean: boolean;
begin
   Result := FBoolean;
end;

function TxmlRpcDataElement.GetDate: TDateTime;
begin
   Result := FDate;
end;

function TxmlRpcDataElement.GetDouble: double;
begin
   Result := FDouble;
end;

function TxmlRpcDataElement.GetErrorCode: integer;
begin
   Result := FErrorCode;
end;

function TxmlRpcDataElement.GetErrorString: string;
begin
   Result := FErrorString;
end;

function TxmlRpcDataElement.GetInteger: integer;
begin
   Result := FInteger;
end;

function TxmlRpcDataElement.GetString: string;
begin
   Result := FString;
end;

function TxmlRpcDataElement.GetStruct: TxmlRpcStruct;
begin
   Result := FStruct;
end;

function TxmlRpcDataElement.IsArray: boolean;
begin
   Result := (FType = dtArray);
end;

function TxmlRpcDataElement.IsBase64: boolean;
begin
   Result := (FType = dtBase64);
end;

function TxmlRpcDataElement.IsBoolean: boolean;
begin
   Result := (FType = dtBoolean);
end;

function TxmlRpcDataElement.IsDate: boolean;
begin
   Result := (FType = dtDate);
end;

function TxmlRpcDataElement.IsDouble: boolean;
begin
   Result := (FType = dtDouble);
end;

function TxmlRpcDataElement.IsError: boolean;
begin
   Result := (FType = dtError);
end;

function TxmlRpcDataElement.IsInteger: boolean;
begin
   Result := (FType = dtInteger);
end;

function TxmlRpcDataElement.IsString: boolean;
begin
   Result := (FType = dtString);
end;

function TxmlRpcDataElement.IsStruct: boolean;
begin
   Result := (FType = dtStruct);
end;

procedure TxmlRpcDataElement.PutBase64ToFile(filename: string);
var
  mstream: TMemoryStream;

begin
  if (FType = dtBase64) then
  begin
     mstream := TMemoryStream.Create;
     //StringToStream(TIdDecoderMIME.DecodeString(FBase64), mstream);
     mstream.SaveToFile(filename);
     mstream.Free;
  end
  else
    raise Exception.Create('PutBase64ToFile - Item is not a base64 type');
end;

procedure TxmlRpcDataElement.PutStringToFile(filename: string);
var
  mstream: TMemoryStream;

  begin
  if (FType = dtString) then
  begin
     mstream := TMemoryStream.Create;
     StringToStream(FString, mstream);
     mstream.SaveToFile(filename);
     mstream.Free;
  end
  else
    raise Exception.Create('PutStringToFile - Item is not a string type');
end;

procedure TxmlRpcDataElement.SetError(Number: integer; Reason: String);
begin
   SetErrorCode(number);
   SetErrorString(Reason);
end;

procedure TxmlRpcDataElement.SetErrorCode(value: integer);
begin
   FType := dtError;
   FErrorCode := Value;
end;

procedure TxmlRpcDataElement.SetErrorString(value: string);
begin
   FType := dtError;
   FErrorString := Value;
end;

procedure TxmlRpcDataElement.SetNil();
begin
   FNil := true;
end;

procedure TxmlRpcDataElement.SetItem(value: string);
begin
   FType := dtString;
   FString := Value;
end;

procedure TxmlRpcDataElement.SetItem(value: TxmlRpcStruct);
begin
   FType := dtStruct;
   FStruct := Value;

   if FStruct.HasKey('faultCode') then
   begin
      FType := dtError;
      FErrorCode := FStruct.Keys['faultCode'].GetInteger;
      FErrorString := FStruct.Keys['faultString'].GetString;
   end;
end;

procedure TxmlRpcDataElement.SetItem(value: TxmlRpcArray);
begin
   FType := dtArray;
   FArray := Value;
end;

procedure TxmlRpcDataElement.SetItem(value: TDateTime);
begin
   FType := dtDate;
   FDate := Value;
end;

procedure TxmlRpcDataElement.SetItem(value: integer);
begin
   FType := dtInteger;
   FInteger := Value;
end;

procedure TxmlRpcDataElement.SetItem(value: Extended);
begin
   FType := dtDouble;
   FDouble := Value;
end;

procedure TxmlRpcDataElement.SetItem(value: boolean);
begin
   FType := dtBoolean;
   FBoolean := Value;
end;

procedure TxmlRpcDataElement.SetItemBase64Data(value: string);
begin
   FType := dtBase64;
   FBase64 := Value;
end;

procedure TxmlRpcDataElement.SetItemBase64ByFile(filename: string; DeleteAfterRead: Boolean=false);
begin
   FType := dtBase64File;
   FFileName := filename;
   FDeleteAfterRead := DeleteAfterRead;
end;

procedure TxmlRpcDataElement.SetItemStringByFile(filename: string; DeleteAfterRead: Boolean);
begin
   FType := dtStringFile;
   FFileName := filename;
   FDeleteAfterRead := DeleteAfterRead;
end;

function TxmlRpcDataElement.IsFile: boolean;
begin
   Result := (FType in [dtStringFile, dtBase64File]);
end;

function TxmlRpcDataElement.IsNil: boolean;
begin
   Result := FNil;
end;

function TxmlRpcDataElement.GetFileName: String;
begin
   Result := FFileName;
end;

{ TxmlRpcArray }

constructor TxmlRpcArray.Create;
begin
   FList := TObjectList.Create(true);
end;

destructor TxmlRpcArray.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TxmlRpcArray.AddItem: TxmlRpcArrayItem;
begin
   Result := TxmlRpcArrayItem.Create;
   FList.Add(Result);
end;

function TxmlRpcArray.AddItem(Data: TxmlRpcDataElement): TxmlRpcArrayItem;
begin
   Result := TxmlRpcArrayItem.Create;
   FList.Add(Result);
   Result.Assign(Data);
end;

procedure TxmlRpcArray.Clear;
begin
   FList.Clear;
   FList.Pack;
end;

function TxmlRpcArray.Count: integer;
begin
   Result := FList.Count;
end;

procedure TxmlRpcArray.Delete(index: integer);
begin
   FList.Delete(index);
end;

function TxmlRpcArray.GetAsXML: string;
var
   index: integer;
   st: TStringList;

begin
   st := TStringList.Create;
   try
      st.Add('<value>');
      st.Add('  <array>');
      st.Add('    <data>');
      for index := 0 to FList.Count - 1 do
      begin
         case TxmlRpcArrayItem(FList[index]).FType of
            dtString: st.Add('      <value><string><![CDATA[' +
                             TxmlRpcArrayItem(FList[index]).FString +
                             ']]></string></value>');
            dtInteger: st.Add('      <value><int>' +
                             IntToStr(TxmlRpcArrayItem(FList[index]).FInteger) +
                             '</int></value>');
            dtDouble: st.Add('      <value><double>' +
                             FloatToStr(TxmlRpcArrayItem(FList[index]).FDouble) +
                             '</double></value>');
            dtBase64: st.Add('      <value><base64>' +
                             TxmlRpcArrayItem(FList[index]).FBase64 +
                             '      </base64></value>');
            dtDate: st.Add('      <value><dateTime.iso8601>' +
                             DateTimeToISO(TxmlRpcArrayItem(FList[index]).FDate) +
                             '</dateTime.iso8601></value>');
            dtBoolean:
              if TxmlRpcArrayItem(FList[index]).FBoolean then
                st.Add('      <value><boolean>1</boolean></value>')
              else
                st.Add('      <value><boolean>0</boolean></value>');

            dtStruct: st.Add(TxmlRpcArrayItem(FList[index]).FStruct.GetAsXML);
            dtArray: st.Add(TxmlRpcArrayItem(FList[index]).FArray.GetAsXML);
         end;
      end;
      st.Add('  </data>');
      st.Add('</array>');
      st.Add('</value>');
      Result := st.Text;

   finally
      FreeAndNil(st);
   end;
end;

function TxmlRpcArray.getItem(index: integer): TxmlRpcArrayItem;
begin
   Result := TxmlRpcArrayItem(FList.Items[index]);
end;

procedure TxmlRpcArray.setItem(index: integer; Value: TxmlRpcArrayItem);
var
   FArray: TxmlRpcArrayItem;

begin
   FArray := getItem(index);
   if Assigned(FArray) then
      FreeAndNil(FArray);
   FList.Items[index] := Value;
end;

procedure TxmlRpcArray.LoadRawData(dt: TxmlRpcDataType; value: string);
var
   ti : TxmlRpcArrayItem;

begin
   ti := TxmlRpcArrayItem.Create;
   case dt of
      dtString:
      begin
         ti.FType := dtString;
         ti.FString := value;
      end;
      dtInteger:
      begin
         ti.FType := dtInteger;
         ti.FInteger := StrToInt(value);
      end;
      dtDouble:
      begin
         ti.FType := dtDouble;
         ti.FDouble := StrToFloat(value);
      end;
      dtBoolean:
      begin
         ti.FType := dtBoolean;
         if (value = '0') then
            ti.FBoolean := false
         else
            ti.FBoolean := true;
      end;
      dtDate:
      begin
         ti.FType := dtDate;
         ti.FDate := ISOToDateTime(value);
      end;
      dtBase64:
      begin
         ti.FType := dtBase64;
         ti.FBase64 := value;
      end;
   end;
   FList.Add(ti);
end;

procedure TxmlRpcArray.Assign(Source: TxmlRpcArray);
var
   I: Integer;
   Item: TxmlRpcArrayItem;

begin
   for i := 0 to pred(Source.FList.Count) do
   begin
       Item := TxmlRpcArrayItem.Create;
       Item.Assign(TxmlRpcArrayItem(Source.FList.Items[i]));
       FList.Add(Item);
   end;
end;

{ TxmlRpcStruct }

constructor TxmlRpcStruct.Create;
begin
   FItemList := TStringList.Create;
   //FItemList.Sorted := true;
   //FItemList.Duplicates := dupError;
end;

destructor TxmlRpcStruct.Destroy;
begin
   Clear;
   FreeAndNil(FItemList);
   inherited;
end;

function TxmlRpcStruct.AddItem(key: string) : TxmlRpcStructItem;
begin
   Result := TxmlRpcStructItem.Create;
   Result.FName := key;
   FItemList.AddObject(key, Result);
end;

procedure TxmlRpcStruct.Clear;
var
   I: Integer;
   P: Pointer;

begin
   for I := pred(FItemList.Count) downto 0 do
   begin
      P := FItemList.Objects[i];
      FreeAndNil(P);
      FItemList.Delete(I);
   end;
end;

function TxmlRpcStruct.Count: integer;
begin
   Result := FItemList.Count;
end;

procedure TxmlRpcStruct.Delete(key: string);
var
   P: Pointer;
   Index: Integer;

begin
   Index := FItemList.IndexOf(key);
   P := FItemList.Objects[Index];
   FreeAndNil(P);
   FItemList.Delete(Index);
end;

function TxmlRpcStruct.GetAsXML: string;
var
   index: integer;
   st: TStrings;

begin
   st := TStringList.Create;
   try
      st.Add('<value>');
      st.Add('  <struct>');
      for index := 0 to FItemList.Count - 1 do
      begin
         st.Add('    <member>');
         case TxmlRpcStructItem(FItemList.Objects[index]).FType of
           dtString:
           begin
              st.Add('      <name>' +
                      FItemList[index]  +
                      '</name>');
              st.Add('      <value><string><![CDATA[' +
                     TxmlRpcStructItem(FItemList.Objects[index]).FString +
                     ']]></string></value>');
            end;
            dtInteger:
            begin
               st.Add('      <name>' +
                      FItemList[index]  +
                      '</name>');
               st.Add('      <value><int>' +
                      IntToStr(TxmlRpcStructItem(FItemList.Objects[index]).FInteger) +
                      '</int></value>');
            end;
            dtDouble:
            begin
               st.Add('      <name>' +
                      FItemList[index]  +
                      '</name>');
               st.Add('      <value><double>' +
                      FloatToStr(TxmlRpcStructItem(FItemList.Objects[index]).FDouble) +
                      '</double></value>');
            end;
            dtBase64:
            begin
               st.Add('      <name>' +
                       FItemList[index]  +
                      '</name>');
               st.Add('      <value><base64>' +
                       TxmlRpcStructItem(FItemList.Objects[index]).FBase64 +
                       '</base64></value>');
            end;
            dtDate:
            begin
               st.Add('      <name>' +
                      FItemList[index]  +
                      '</name>');
               st.Add('      <value><dateTime.iso8601>' +
                            DateTimeToISO(TxmlRpcStructItem(FItemList.Objects[index]).FDate) +
                            '</dateTime.iso8601></value>');
            end;
            dtBoolean:
            begin
               st.Add('      <name>' +
                      FItemList[index]  +
                      '</name>');
               if TxmlRpcStructItem(FItemList.Objects[index]).FBoolean then
                 st.Add('      <value><boolean>1</boolean></value>')
               else
                 st.Add('      <value><boolean>0</boolean></value>');
            end;
            dtStruct:
            begin
               st.Add('      <name>' +
                      FItemList[index]  +
                      '      </name>');
               st.Add(TxmlRpcStructItem(FItemList.Objects[index]).FStruct.GetAsXML);
            end;
            dtArray:
            begin
               st.Add('      <name>' +
                      FItemList[index]  +
                      '</name>');
                st.Add(TxmlRpcStructItem(FItemList.Objects[index]).FArray.GetAsXML);
            end;
         end;
         st.Add('    </member>');
      end;
      st.Add('  </struct>');
      st.Add('</value>');
      result := st.Text;

   finally
      st.Free;
   end;
end;

function TxmlRpcStruct.getItem(index: integer): TxmlRpcStructItem;
begin
   Result := TxmlRpcStructItem(FItemList.Objects[index]);
end;

function TxmlRpcStruct.GetKeyList: TStringList;
begin
   Result := FItemList;
end;

function TxmlRpcStruct.HasKey(key: string): boolean;
begin
   if FItemList.IndexOf(key) <> -1 then
      result := true
   else
      result := false;
end;

procedure TxmlRpcStruct.setItem(index: integer; Value: TxmlRpcStructItem);
begin
   FItemList.Objects[index] := Value;
end;

function TxmlRpcStruct.getItemByKey(index: string): TxmlRpcStructItem;
begin
   Result := getItem(FItemList.IndexOf(index));
end;

procedure TxmlRpcStruct.setItemByKey(index: string; Value: TxmlRpcStructItem);
begin
   setItem(FItemList.IndexOf(index), Value);
end;

procedure TxmlRpcStruct.LoadRawData(dt: TxmlRpcDataType; key, value: string);
var
   titem: TxmlRpcStructItem;

begin
  titem := TxmlRpcStructItem.Create;
  case dt of
     dtString:
     begin
        titem.FType := dtString;
        titem.FString := value;
     end;
     dtInteger:
     begin
        titem.FType := dtInteger;
        titem.FInteger := StrToInt(value);
     end;
     dtBoolean:
     begin
        titem.FType := dtBoolean;
        if value = '0' then
           titem.FBoolean := false
        else
           titem.FBoolean := true;
        FItemList.AddObject(key,titem);
     end;
     dtDouble:
     begin
        titem.FType := dtDouble;
        titem.FDouble := StrToFloat(value);
     end;
     dtDate:
     begin
        titem.FType := dtDate;
        titem.FDate := ISOToDateTime(value);
     end;
     dtBase64:
     begin
        titem.FType := dtBase64;
        titem.FBase64 := value;
     end;
  end;

  FItemList.AddObject(key, titem);
end;

procedure TxmlRpcStruct.Assign(Source: TxmlRpcStruct);
var
   I: Integer;
   Item: TxmlRpcStructItem;

begin
   for i := 0 to pred(Source.FItemList.Count) do
   begin
       Item := TxmlRpcStructItem.Create;
       Item.Assign(TxmlRpcStructItem(Source.FItemList.Objects[i]));
       FItemList.AddObject(TxmlRpcStructItem(Source.FItemList.Objects[i]).FName, Item);
   end;
end;

procedure TxmlRpcStruct.AddStrings(AList: TStrings);
var I: Integer;
begin
  if Assigned(AList) then
  begin
     for I := 0 to pred(Alist.Count) do
        AddItem(AList.Names[I]).SetItem(AList.Values[AList.Names[I]]);
  end;
end;

{ TxmlRpcFunction }

constructor TxmlRpcFunction.Create;
begin
   FList := TObjectList.Create(true);
end;


constructor TxmlRpcFunction.Create(ObjectMethod: String; Params: array of const);
begin
   FList := TObjectList.Create(true);
   FMethod := ObjectMethod;
   ArrayToXMLRpcParamList(self, Params);
end;

destructor TxmlRpcFunction.Destroy;
begin
   FreeAndNil(FList);
   inherited;
end;

procedure TxmlRpcFunction.AddError(code: integer; msg: string);
begin
  FErrorCode := code;
  FErrorMessage := msg;
end;

function TxmlRpcFunction.AddParam: TxmlRpcFunctionItem;
begin
   Result := TxmlRpcFunctionItem.Create;
   FList.Add(Result);
end;

function TxmlRpcFunction.AddParam(Item: TxmlRpcFunctionItem): TxmlRpcFunctionItem;
begin
   FList.Add(Item);
   if Item.IsError then
   begin
      FErrorCode := Item.GetErrorCode;
      FErrorMessage := Item.GetErrorString;
   end;
   Result := Item;
end;

procedure TxmlRpcFunction.Clear;
begin
   FList.Clear;
   FMethod := '';
end;

function TxmlRpcFunction.GetErrorXML: string;
begin
  Result := '<?xml version="1.0"?>' + #13#10;
  Result := Result + '<methodResponse>' + #13#10;
  Result := Result + '   <fault>' + #13#10;
  Result := Result + '      <value>' + #13#10;
  Result := Result + '        <struct>' + #13#10;
  Result := Result + '            <member>' + #13#10;
  Result := Result + '               <name>faultCode</name>' + #13#10;
  Result := Result + '               <value><int>' + IntToStr(FErrorCode) + '</int></value>' + #13#10;
  Result := Result + '               </member>' + #13#10;
  Result := Result + '            <member>' + #13#10;
  Result := Result + '               <name>faultString</name>' + #13#10;
  Result := Result + '               <value><string><![CDATA[' + FErrorMessage + ']]></string></value>' + #13#10;
  Result := Result + '               </member>' + #13#10;
  Result := Result + '            </struct>' + #13#10;
  Result := Result + '         </value>' + #13#10;
  Result := Result + '      </fault>' + #13#10;
  Result := Result + '   </methodResponse>' + #13#10;
end;

function TxmlRpcFunction.getItem(index: integer): TxmlRpcFunctionItem;
begin
   Result := TxmlRpcFunctionItem(FList.Items[index]);
end;

function TxmlRpcFunction.GetRequestXML: string;
var
   index: integer;
   srtn: string;

begin
   srtn := '<?xml version="1.0"?>' + #13#10;
   srtn := srtn + '<methodCall>' + #13#10;
   srtn := srtn + '   <methodName>' +  FMethod  + '</methodName>' +#13#10;
   srtn := srtn + '   <params>' + #13#10;
   for index := 0 to FList.Count -1 do
   begin
      srtn := srtn + '   <param>' + #13#10;
      case TxmlRpcFunctionItem(FList[index]).FType of
        dtInteger:
            srtn := srtn + '<value><int>' +
                    IntToStr(TxmlRpcFunctionItem(FList[index]).FInteger) +
                    '</int></value>' + #13#10;
        dtString:
            srtn := srtn + '<value><string><![CDATA[' +
                    TxmlRpcFunctionItem(FList[index]).FString +
                    ']]></string></value>' + #13#10;

        dtStringFile:
            srtn := srtn + '<value><string cid="' + TxmlRpcFunctionItem(FList[index]).CID + '" /></value>' + #13#10;

        dtDouble:
            srtn := srtn + '<value><double>' +
                    FloatToStr(TxmlRpcFunctionItem(FList[index]).FDouble) +
                    '</double></value>' + #13#10;

        dtBoolean:
            if TxmlRpcFunctionItem(FList[index]).FBoolean then
              srtn := srtn + '<value><boolean>1</boolean></value>' + #13#10
            else
              srtn := srtn + '<value><boolean>0</boolean></value>' + #13#10;

        dtDate:
            srtn := srtn + '<value><dateTime.iso8601>' +
                    DateTimeToISO(TxmlRpcFunctionItem(FList[index]).FDate) +
                    '</dateTime.iso8601></value>' + #13#10;
        dtArray:
            srtn := srtn + TxmlRpcFunctionItem(FList[index]).FArray.GetAsXML + #13#10;

        dtStruct:
            srtn := srtn + TxmlRpcFunctionItem(FList[index]).FStruct.GetAsXML + #13#10;

        dtBase64:
           srtn := srtn + '<value><base64>' +
                           TxmlRpcFunctionItem(FList[index]).FBase64 +
                           '</base64></value>' + #13#10;
        dtBase64File:
            srtn := srtn + '<value><base64 cid="' + TxmlRpcFunctionItem(FList[index]).CID + '" /></value>' + #13#10;

      end;
      srtn := srtn + '   </param>' + #13#10;
   end;
   srtn := srtn + '   </params>' + #13#10;
   result := srtn + '</methodCall>' + #13#10;
end;

function TxmlRpcFunction.GetResponseXML: string;
var
   index: integer;
   srtn, data: string;

begin
   {if we have a error condition return the error instead}
   if (FErrorCode > 0) then
   begin
      Result := GetErrorXML;
      exit;
   end;

   srtn := '<?xml version="1.0"?>' + #13#10;
   srtn := srtn + '<methodResponse>' + #13#10;
   srtn := srtn + '   <params>' + #13#10;
   for index := 0 to FList.Count -1 do
   begin
      srtn := srtn + '   <param>' + #13#10;
      case TxmlRpcFunctionItem(FList[index]).FType of
        dtInteger:
            srtn := srtn + '<value><int>' +
                    IntToStr(TxmlRpcFunctionItem(FList[index]).FInteger) +
                    '</int></value>' + #13#10;
        dtString:
            srtn := srtn + '<value><string><![CDATA[' +
                    TxmlRpcFunctionItem(FList[index]).FString +
                    ']]></string></value>' + #13#10;

        dtStringFile:
           begin
              try
                 with TFileStream.Create(TxmlRpcFunctionItem(FList[index]).FFileName, fmOpenRead) do
                 begin
                    try
                       SetLength(data, Size);
                       ReadBuffer(Pointer(data)^, Size);
                       srtn := srtn + '<value><string>' +
                                    data +
                                    '</string></value>' + #13#10;
                       SetLength(data, 0);
                     except
                     end;
                     Free;
                 end;
              except
              end;
           end;

        dtDouble:
            srtn := srtn + '<value><double>' +
                    FloatToStr(TxmlRpcFunctionItem(FList[index]).FDouble) +
                    '</double></value>' + #13#10;

        dtBoolean:
            if TxmlRpcFunctionItem(FList[index]).FBoolean then
              srtn := srtn + '<value><boolean>1</boolean></value>' + #13#10
            else
              srtn := srtn + '<value><boolean>0</boolean></value>' + #13#10;

        dtDate:
            srtn := srtn + '<value><dateTime.iso8601>' +
                    DateTimeToISO(TxmlRpcFunctionItem(FList[index]).FDate) +
                    '</dateTime.iso8601></value>' + #13#10;
        dtArray:
            srtn := srtn + TxmlRpcFunctionItem(FList[index]).FArray.GetAsXML + #13#10;

        dtStruct:
            srtn := srtn + TxmlRpcFunctionItem(FList[index]).FStruct.GetAsXML + #13#10;

        dtBase64:
           srtn := srtn + '<value><base64>' +
                           TxmlRpcFunctionItem(FList[index]).FBase64 +
                           '</base64></value>' + #13#10;
        dtBase64File:
           begin
              try
                 with TFileStream.Create(TxmlRpcFunctionItem(FList[index]).FFileName, fmOpenRead) do
                 begin
                    try
                       SetLength(data, Size);
                       ReadBuffer(Pointer(data)^, Size);
                       srtn := srtn + '<value><base64>' +
                                    //EncodeString(TIdEncoderMIME, data) +
                                    //TIdEncoderMIME.EncodeString(data) +
                                    '</base64></value>' + #13#10;
                       SetLength(data, 0);
                     except
                     end;
                     Free;
                 end;
              except
              end;
           end;
      end;
      srtn := srtn + '   </param>' + #13#10;
   end;
   srtn := srtn + '   </params>' + #13#10;
   Result := srtn + '</methodResponse>' + #13#10;
end;

procedure TxmlRpcFunction.setItem(index: integer; Value: TxmlRpcFunctionItem);
begin
   FList.Items[index] := Value;
end;

function TxmlRpcFunction.ParamCount: Integer;
begin
   Result := FList.Count;
end;

function TxmlRpcFunction.HasFilesAssigned: boolean;
var i: Integer;
begin
   Result := false;
   for i := 0 to pred(FList.Count) do
   begin
      Result := (TxmlRpcFunctionItem(FList.Items[i]).FType in [dtStringFile, dtBase64File]);
      if Result then
         Break;
   end;
end;

{ TxmlRpcResult }

constructor TxmlRpcResult.Create;
begin
   FData := TxmlRpcDataElement.Create;
end;

destructor TxmlRpcResult.Destroy;
begin
   FreeAndNil(FData);
   inherited;
end;

procedure TxmlRpcResult.Clear;
begin
   FreeAndNil(FData);
   FData := TxmlRpcDataElement.Create;
end;

procedure TxmlRpcResult.Assign(Source: TxmlRpcResult);
begin
   FData.Assign(Source.Data);
end;

{ TxmlRpcStructItem }

procedure TxmlRpcStructItem.Assign(Source: TxmlRpcDataElement);
begin
  FName := TxmlRpcStructItem(Source).FName;
  inherited;
end;

{ TxmlRpcParamList }

function TxmlRpcParamList.Add(Item: TxmlRpcDataElement): Integer;
begin
   Result := inherited Add(Item);
end;

function TxmlRpcParamList.Get(Index: Integer): TxmlRpcDataElement;
begin
  Result := TxmlRpcDataElement(inherited get(Index));
end;

procedure TxmlRpcParamList.Put(Index: Integer; const Value: TxmlRpcDataElement);
begin
   inherited Put(Index, Value);
end;

end.
