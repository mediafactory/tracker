unit trac_main;

{
    GPL licensed
    <c> Henrik Genssen hg@mediafactory.de
}

{$mode objfpc}{$H+}

{
   XMLRPC docs (or introspect :-)):
   https://fedorahosted.org/rel-eng/xmlrpc

   todo:
   - do multicall on changes
   - check for API version and TRAC version
   - save server settings history + clear selected fields or bound them to URLs
   - execute "actions" on tickets
   - table filter locally
   - table "filter" as add to searchform
   - view ticket locally viewMnu and IPro
   - pool requests
   - cache XMLRPC requests
   - format hint align left and go away on mouse move
   - limit in API 0.2

   bugs:
   - TCheckGroup wrong height bug
   - table sort images not rendered
   - sort on datetime fields with german datetime
}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  xmlrpc, xmlrpctypes, Grids, ComCtrls, Menus, ExtCtrls, SemaphorGrids,
  // etc
  OpenURLUtil,
  // trac
  trac_server,
  trac_columns,
  trac_search,
  trac_change_select,
  trac_change_text;

type

  { Tmainform }

  Tmainform = class(TForm)
    sortImgList: TImageList;
     MainMenu: TMainMenu;
     MenuItem1: TMenuItem;
     changeMnu: TMenuItem;
     searchMnu: TMenuItem;
     Grid: TSemaphorGrid;
     settingsMnu: TMenuItem;

     hostMnu: TMenuItem;
     MenuItem3: TMenuItem;
     actionMnu: TMenuItem;
     columnsMnu: TMenuItem;
     refreshMnu: TMenuItem;
     TicketMenu: TPopupMenu;
     StatusBar: TStatusBar;
     hintTimer: TTimer;
     procedure FormCreate(Sender: TObject);
     procedure FormDestroy(Sender: TObject);
     procedure GridDblClick(Sender: TObject);
     procedure GridHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
     procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
     procedure GridShowHint(Sender: TObject; HintInfo: PHintInfo) ;
     procedure hostMnuClick(Sender: TObject);
     procedure columnsMnuClick(Sender: TObject);
     procedure MenuItem3Click(Sender: TObject);
     procedure refreshMnuClick(Sender: TObject);
     procedure searchMnuClick(Sender: TObject);
     procedure TicketMenuPopup(Sender: TObject);
     procedure addSelectMenu(Structure: TxmlRpcStruct);
     procedure addTextMenu(Structure: TxmlRpcStruct);

  private
     FServerVersion: Integer;
     FApiMajorVersion: Integer;
     FInit: boolean;
     FFieldsAvailable: TStringList;
     FFieldsSelected: TStringList;
     FLastHintedRow: Integer;

     FUseSSL: boolean;
     FServer: String;
     FPort: Integer;
     FURL: String;
     FUser: String;
     FPassword: String;

     procedure GetTicketFields;
     function GetTicketActions(id: Integer) : TxmlRpcArray;
     function GetTicketIDByRow(row: Integer) : Integer;
     procedure ChangeTicket(id: Integer; Comment, Field, Value: String; notify: boolean);
     procedure SelectMnuClick(Sender: TObject);
     procedure TextMnuClick(Sender: TObject);
     function WikiToPlainText(aWiki: String) : String;
     procedure OpenTicketInBrowser(id: Integer);
     procedure GetTracVersion;

  public
     procedure LoadList;
     procedure Init;

     property UseSSL: boolean read FUseSSL write FUseSSL;
     property Server: String read FServer write FServer;
     property Port: Integer read FPort write FPort;
     property URL: String read FURL write FURL;
     property User: String read FUser write FUser;
     property Password: String read FPassword write FPassword;
     property FieldsAvailable: TStringList read FFieldsAvailable;
     property FieldsSelected: TStringList read FFieldsSelected;
  end; 

var
  mainform: Tmainform;

implementation

{ Tmainform }

procedure Tmainform.LoadList;
var
   Result: TxmlRpcDataElement;
   ar, Tic : TxmlRpcArray;
   Ticket: TxmlRpcStruct;
   i, q: Integer;
   caller: TxmlRpcMultiCaller;

begin
   StatusBar.Panels[0].Text := 'searching for tickets';

   Grid.ShowCol(0);
   Grid.ShowCol(1);
   Grid.Clear(true);
   Grid.Columns.Clear;

   Application.ProcessMessages;

   if searchform.SearchVars <> '' then
      Result := XMLRpcFunctionCall(FUseSSL, FServer, FPort, FURL, FUser, FPassword, 'ticket.query', [searchform.SearchVars], true)
   else
      Result := XMLRpcFunctionCall(FUseSSL, FServer, FPort, FURL, FUser, FPassword, 'ticket.query', [], true);

   if Result.isArray then
   begin
      ar := Result.getArray;

      if Ar.Count = 0 then
      begin
         StatusBar.Panels[0].Text := '0';
         Exit;
      end;

      caller := TxmlRpcMultiCaller.Create(mcYes);
      try
         caller.HostName := FServer;
         caller.HostPort := FPort;
         caller.EndPoint := FURL;
         caller.Debug :=  false;

         caller.Username := FUser;
         caller.Password := FPassword;

         for i := 0 to pred(Ar.Count) do
            caller.AddFunctionCall('ticket.get', [ar.Item[i].getInteger]);

         caller.Execute;

         Statusbar.Panels[0].Text := IntToStr(caller.ResultCount);
         Grid.RowCount := caller.ResultCount + 1;
//         Grid.FixedCols := 1;
         Grid.FixedRows := 1;

         // 2 extra columns for hidden fixed content
         Grid.Columns.Add;
         Grid.Columns.Add;

         // table head
         for q := 0 to pred(FFieldsSelected.Count) do
         begin
            Grid.Columns.Add.Title.Caption := FFieldsSelected.Strings[q];
            Grid.Cells[q + 2, 0] := FFieldsSelected.Strings[q];
         end;

         // table content
         for i := 0 to pred(caller.ResultCount) do
         begin
            if caller.Result[i].IsArray then
            begin
               Tic := Caller.Result[i].GetArray;
               Ticket := Tic.Item[3].GetStruct;

               // fixed content at the end of grid (not shown)
               Grid.Cells[0 + Grid.FixedCols, i + 1] := IntToStr(Tic.Item[0].GetInteger);
               Grid.Cells[1 + Grid.FixedCols, i + 1] := WikiToPlainText(Ticket.Keys['description'].getString);

               // content
               for q := 0 to pred(FFieldsSelected.Count) do
               begin
                  if FFieldsSelected.Strings[q] = 'id' then
                     Grid.Cells[q + Grid.FixedCols + 2, i + 1] := IntToStr(Tic.Item[0].GetInteger)
                  else if FFieldsSelected.Strings[q] = 'time created' then
                     Grid.Cells[q + Grid.FixedCols + 2, i + 1] := DateTimeToStr(Tic.Item[1].GetDate)
                  else if FFieldsSelected.Strings[q] = 'time changed' then
                     Grid.Cells[q + Grid.FixedCols + 2, i + 1] := DateTimeToStr(Tic.Item[2].GetDate)
                  else
                     if Ticket.HasKey(FFieldsSelected.Strings[q]) then
                        Grid.Cells[q + Grid.FixedCols + 2, i + 1] := Ticket.Keys[FFieldsSelected.Strings[q]].GetString
                     else
                        Grid.Cells[q + Grid.FixedCols + 2, i + 1] := '';
               end;
            end;
         end;

      except
         on e: Exception do ShowMessage(e.Message);
      end;
      Grid.AutoSizeColumns;
      Grid.HideCol(0);
      Grid.HideCol(1);
      FreeAndNil(caller);
   end;
end;

procedure Tmainform.GetTicketFields;
var
   Result: TxmlRpcDataElement;
   ar: TxmlRpcArray;
   str: TxmlRpcStruct;
   i: Integer;

begin
   FFieldsAvailable.Clear;

   Result := XMLRpcFunctionCall(FUseSSL, FServer, FPort, FURL, FUser, FPassword, 'ticket.getTicketFields', [], true);

   // type, name, label, optional, options

   if Result.isArray then
   begin
      ar := Result.getArray;
      searchform.TracFields.Assign(ar);

      for i := 0 to pred(ar.Count) do
      begin
         Str := ar.Item[i].GetStruct;

         // add to available field list
         if Str.HasKey('label') then
            FFieldsAvailable.Add(Str.Keys['label'].getString)
         else if Str.HasKey('name') then
            FFieldsAvailable.Add(Str.Keys['name'].getString);

         // add to search form and to menu structure
         if Str.HasKey('type') then
         begin
            if (Str.Keys['type'].getString = 'text') or (Str.Keys['type'].getString = 'textarea') then
            begin
               searchform.AddTextElement(Str);
               addTextMenu(Str);
            end
            else if Str.Keys['type'].getString = 'select' then
            begin
               searchform.AddSelectElement(Str);
               addSelectMenu(str);
            end
            else if Str.Keys['type'].getString = 'radio' then
               searchform.AddRadioElement(Str)
            else if Str.Keys['type'].getString = 'checkbox' then
               searchform.AddCheckboxElement(Str);
         end;
      end;
   end;

   searchform.FinishSearchForm;
   searchform.InitSearchFields;

   FreeAndNil(Result);
end;

function Tmainform.GetTicketActions(id: Integer): TxmlRpcArray;
var
   Res: TxmlRpcDataElement;

begin
   Result := TxmlRpcArray.Create;

   Res := XMLRpcFunctionCall(FUseSSL, FServer, FPort, FURL, FUser, FPassword, 'ticket.getAvailableActions', [id], true);

   if Res.isArray then
      Result.Assign(Res.GetArray);
end;

function Tmainform.GetTicketIDByRow(row: Integer): Integer;
begin
   if row < Grid.RowCount then
      Result := StrToInt(Grid.Cells[0, row])
   else
      raise Exception.Create('row out of range' + IntToStr(row));
end;

procedure Tmainform.ChangeTicket(id: Integer; Comment, Field, Value: String; notify: boolean);
var
   Params: TxmlRpcArray;
   Struct: TxmlRpcStruct;

begin
   Params := TxmlRpcArray.Create;
   try
      Params.AddItem().SetItem(id);
      Params.AddItem().SetItem(Comment);
      Struct := TxmlRpcStruct.Create;
      try
         Struct.AddItem(Field).SetItem(Value);
         Params.AddItem().SetItem(Struct);
         Params.AddItem().SetItem(true);
         XMLRpcFunctionCall(FUseSSL, FServer, FPort, FURL, FUser, FPassword, 'ticket.update', Params, notify);

      finally
         // FreeAndNil(Struct); implicit gefreet
      end;
   finally
      FreeAndNil(Params);
   end;
end;

procedure Tmainform.SelectMnuClick(Sender: TObject);
var
   i: Integer;
   ColumnIndex: Integer;

begin
   // check if column displayed
   ColumnIndex := -1;
   for i := 0 to pred(FFieldsSelected.Count) do
      if LowerCase(FFieldsSelected.Strings[i]) = LowerCase(TMenuItem(Sender).Parent.Caption) then
         ColumnIndex := i;

   // request comment
   if not Assigned(changeselectform) then
      Application.CreateForm(Tchangeselectform, changeselectform);
   changeselectform.Caption := 'TRAC - ticket change';
   changeselectform.commentLabel.Caption := 'you are about to change ' + IntToStr(Grid.Selection.Bottom - Grid.Selection.top + 1) + ' tickets' + #13#10 +
                                            TMenuItem(Sender).Parent.Caption + ' => ' + TMenuItem(Sender).Caption + #13#10;
   if changeselectform.ShowModal = mrOK then
   begin
      for i := Grid.Selection.top to Grid.Selection.Bottom do
      begin
         ChangeTicket(GetTicketIDByRow(i), changeselectform.commentMemo.Text, TMenuItem(Sender).Parent.Caption, TMenuItem(Sender).Caption, changeselectform.notifyChkBox.Checked);
         if ColumnIndex > -1 then
            Grid.Cells[ColumnIndex + 2, i] := TMenuItem(Sender).Caption;
      end;
   end;
end;

procedure Tmainform.TextMnuClick(Sender: TObject);
var
   i: Integer;
   ColumnIndex: Integer;
   Result: TxmlRpcDataElement;
   Tic : TxmlRpcArray;
   Ticket: TxmlRpcStruct;

begin
   // if only one ticket selected fetch old value
   if Grid.Selection.Bottom - Grid.Selection.Top = 0 then
   begin
      Result := XMLRpcFunctionCall(FUseSSL, FServer, FPort, FURL, FUser, FPassword, 'ticket.get', [GetTicketIDByRow(Grid.Selection.Top)], true);
      if Result.isArray then
      begin
         Tic := Result.getArray;
         Ticket := Tic.Item[3].GetStruct;
         if Ticket.HasKey(TMenuItem(Sender).Caption) then
            changetextform.textEdit.Text := Ticket.Keys[TMenuItem(Sender).Caption].getString;
      end;
   end;

   // check if column displayed
   ColumnIndex := -1;
   for i := 0 to pred(FFieldsSelected.Count) do
      if LowerCase(FFieldsSelected.Strings[i]) = LowerCase(TMenuItem(Sender).Caption) then
         ColumnIndex := i;

   // request comment
   if not Assigned(changetextform) then
      Application.CreateForm(Tchangetextform, changetextform);
   changetextform.Caption := 'TRAC - ticket change';
   changetextform.commentLabel.Caption := 'you are about to change ' + IntToStr(Grid.Selection.Bottom - Grid.Selection.top + 1) + ' tickets' + #13#10 +
                                          #13#10 +
                                          TMenuItem(Sender).Caption + ':';
   if changetextform.ShowModal = mrOK then
   begin
      for i := Grid.Selection.top to Grid.Selection.Bottom do
      begin
         ChangeTicket(GetTicketIDByRow(i), changetextform.commentMemo.Text, TMenuItem(Sender).Caption, changetextform.textEdit.Text, changetextform.notifyChkBox.Checked);
         if ColumnIndex > -1 then
            Grid.Cells[ColumnIndex + 2, i] := changetextform.textEdit.Text;
      end;
   end;
end;

function Tmainform.WikiToPlainText( aWiki: String) : String;
begin
   Result := StringReplace(aWiki, '[[BR]]', '', [rfReplaceAll]);
end;

procedure Tmainform.OpenTicketInBrowser(id: Integer) ;
begin
   OpenURL(serverform.URLBase + 'ticket/' + IntToStr(id));
end;

procedure Tmainform.GetTracVersion;
var
   Res: TxmlRpcDataElement;
   Arr: TxmlRpcArray;

begin
   Res := XMLRpcFunctionCall(FUseSSL, FServer, FPort, FURL, FUser, FPassword, 'system.getAPIVersion', [], true);

   if Res.isArray then
   begin
      if Res.IsArray then
      begin
         Arr := Res.GetArray;
         if Arr.Count = 3 then
         begin
            FServerVersion := Arr.Item[0].GetInteger;
            if FServerVersion = 0 then
               StatusBar.Panels[1].Text := 'TRAC 0.10'
            else if FServerVersion = 1 then
               StatusBar.Panels[1].Text := 'TRAC 0.11'
            else
               StatusBar.Panels[1].Text := 'TRAC ??';

            StatusBar.Panels[1].Text := StatusBar.Panels[1].Text + ' API ' + IntToStr(Arr.Item[1].GetInteger) + '.' + IntToStr(Arr.Item[2].GetInteger);
         end
         else
            StatusBar.Panels[1].Text := 'API ' + IntToStr(Arr.Item[0].GetInteger) + '.' + IntToStr(Arr.Item[1].GetInteger);
      end;
   end;
end;

procedure Tmainform.Init;
begin
   if not FInit then
   begin
      GetTracVersion;
      GetTicketFields;
      if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'selectedfields.txt') then
         FFieldsSelected.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'selectedfields.txt')
      else
      begin
         FFieldsSelected.Add('id');
         FFieldsSelected.Add('time created');
         FFieldsSelected.Add('time changed');
         FFieldsSelected.Add('status');
         FFieldsSelected.Add('summary');
         FFieldsSelected.Add('owner');
         FFieldsSelected.Add('reporter');
      end;

      FInit := true;
   end;
   searchform.InitSearchFields;
   searchform.BuildSearchVars;
   LoadList;
end;

procedure Tmainform.hostMnuClick(Sender: TObject);
begin
   serverform.Show;
end;

procedure Tmainform.columnsMnuClick(Sender: TObject);
begin
   columnform.Init;
   columnform.Show;
end;

procedure Tmainform.MenuItem3Click(Sender: TObject);
var
   I: Integer;

begin
   for I := Grid.Selection.top to Grid.Selection.Bottom do
   begin
      ShowMessage(IntToStr(GetTicketIDByRow(i)));
      ShowMessage(Grid.Cells[0, I]);
      ShowMessage(Grid.Cells[1, I]);
   end;
end;

procedure Tmainform.refreshMnuClick(Sender: TObject);
begin
   LoadList;
end;

procedure Tmainform.searchMnuClick(Sender: TObject);
begin
   searchform.Show;
end;

procedure Tmainform.TicketMenuPopup(Sender: TObject);
var
   i, q: Integer;
   Arr: TxmlRpcArray;
   ActionList: TSTringList;
   Mnu: TMenuItem;

begin
   actionMnu.Clear;
Exit;
   ActionList := TStringList.Create;
   try
      ActionList.Sorted := true;
      ActionList.Duplicates := dupIgnore;

      for i := Grid.Selection.Top to Grid.Selection.Bottom do
      begin
         Arr := GetTicketActions(GetTicketIDByRow(i));
         // add all actions from the first ticket to actionlist
         if i = Grid.Selection.Top then
         begin
            for q := 0 to pred(Arr.Count) do
               ActionList.Add(Arr.Item[q].getString);
         end
         // remove all actions from teh list, no in array
         else
         begin

         end;
      end;

      for i := 0 to pred(ActionList.Count) do
      begin
         Mnu := TMenuItem.Create(actionMnu);
         Mnu.Caption := ActionList.Strings[i];
         actionMnu.Add(Mnu);
      end;
   finally
      ActionList.Free;
   end;
end;

procedure Tmainform.addSelectMenu(Structure: TxmlRpcStruct);
var
   Mnu, SubMnu: TmenuItem;
   op: TxmlRpcArray;
   q: Integer;
begin
   if Structure.HasKey('name') then
   begin
      Mnu := TMenuItem.Create(changeMnu);
      Mnu.Caption := Structure.Keys['name'].getString;
      changeMnu.Add(Mnu);
      if Structure.HasKey('options') then
      begin
         op := Structure.Keys['options'].GetArray;
         for q := 0 to pred(op.Count) do
         begin
            SubMnu := TMenuItem.Create(Mnu);
            SubMnu.Caption := op.Item[q].GetString;
            SubMnu.OnClick := @SelectMnuClick;
            Mnu.Add(SubMnu);
         end;
      end;
   end;
end;

procedure Tmainform.addTextMenu(Structure: TxmlRpcStruct);
var
   Mnu: TmenuItem;

begin
   if Structure.HasKey('name') then
   begin
      Mnu := TMenuItem.Create(changeMnu);
      Mnu.Caption := Structure.Keys['name'].getString;
      Mnu.OnClick := @TextMnuClick;
      changeMnu.Add(Mnu);
   end;
end;

procedure Tmainform.FormCreate(Sender: TObject);
begin
   FInit := false;
   FFieldsAvailable := TStringList.Create;
   FFieldsSelected := TStringList.Create;
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
   FreeAndNil(FFieldsAvailable);
   FreeAndNil(FFieldsSelected);
end;

procedure Tmainform.GridDblClick( Sender: TObject) ;
begin
   if Grid.Selection.Bottom - Grid.Selection.Top = 0 then
      OpenTicketInBrowser(GetTicketIDByRow(Grid.Selection.Top));
end;

procedure Tmainform.GridHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
var
   i: Integer;
   TD: TDirection;
   ST: TTypeSort;

begin
   if not IsColumn then
      exit;

   for i := 0 to pred(Grid.Columns.Count) do
      if i <> Index then
         Grid.Columns[i].Title.ImageIndex := 0;
  with Grid.Columns[Index].Title do
  begin
     case ImageIndex of
        0: ImageIndex := 1;
        1: ImageIndex := 2;
        2: ImageIndex := 1;
     end;
     ST := tsAutomatic;
     if Caption = 'id' then
        ST := tsNumeric;
//     if (Caption = 'time created') or (Caption = 'time changed') then
//        ST := tsDate;
     Grid.SortFromColumn(Index, ST, TDirection(ImageIndex < 2), false);
  end
end;

procedure Tmainform.GridMouseMove( Sender: TObject; Shift: TShiftState; X, Y: Integer) ;
var
   Col, Row: Integer;
begin
   Grid.MouseToCell(X, Y, Col, Row);
   if FLastHintedRow = Row then
     Exit;
   FLastHintedRow := Row;

   if (Row > -1) and (Row < Grid.RowCount) then
      Grid.Hint := Grid.Cells[1, Row]
   else
      Grid.Hint := '';
end;

procedure Tmainform.GridShowHint( Sender: TObject; HintInfo: PHintInfo) ;
begin
   //   HintInfo^.HideTimeout := 0;
end;

initialization
  {$I trac_main.lrs}

end.

