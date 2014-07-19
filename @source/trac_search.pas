unit trac_search;

{
    GPL licensed
    <c> Henrik Genssen hg@mediafactory.de
}

{$mode objfpc}{$H+}

interface

// http://delight.opendfki.de/wiki/TracQuery#QueryLanguage

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, MaskEdit, xmlrpctypes;

type

  { Tsearchform }

  Tsearchform = class(TForm)
    resetBtn: TButton;
    flyLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    id: TMaskEdit;
    max: TMaskEdit;
    searchBtn: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow( Sender: TObject) ;
    procedure resetBtnClick( Sender: TObject) ;
    procedure searchBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
     FTracFields: TxmlRpcArray;
     FSpacer: Integer;
     FLeft: Integer;
     FElementLeft: Integer;
     Y: Integer;
     FSearch: String;
     FJoinOptions: TStringList;

     function ControlByName(CtrlName: String) : TControl;

  public
     procedure AddTextElement(Structure: TxmlRpcStruct);
     procedure AddSelectElement(Structure: TxmlRpcStruct);
     procedure AddRadioElement(Structure: TxmlRpcStruct);
     procedure AddCheckboxElement(Structure: TxmlRpcStruct);
     procedure BuildSearchVars;
     procedure InitSearchFields;
     procedure FinishSearchForm;

     property TracFields: TxmlRpcArray read FTracFields write FTracFields;
     property SearchVars: String read FSearch;
  end; 

var
  searchform: Tsearchform;

implementation

uses
   trac_main,
   trac_server;

{ Tsearchform }

procedure Tsearchform.FormCreate(Sender: TObject);
begin
   Y := 89;
   FLeft := 20;
   FSpacer := 10;
   FElementLeft := 150;

   FTracFields := TxmlRpcArray.Create;
   FJoinOptions := TStringList.Create;
   FJoinOptions.Add('is==');
   FJoinOptions.Add('is not=!=');
   FJoinOptions.Add('contains=~=');
   FJoinOptions.Add('contains not=!~=');
   FJoinOptions.Add('starts with=^=');
   FJoinOptions.Add('starts not with=!^=');
   FJoinOptions.Add('ends with=$=');
   FJoinOptions.Add('ends not with=!$=');
end;

function Tsearchform.ControlByName(CtrlName: String): TControl;
var
   i: Integer;

begin
   for i := 0 to pred(self.ControlCount) do
   begin
      if self.Controls[i].Name = CtrlName then
      begin
         Result := self.Controls[i];
         break;
      end;
   end;
end;

procedure Tsearchform.searchBtnClick(Sender: TObject);
begin
   BuildSearchVars;
   Hide;
   mainform.LoadList;
end;

procedure Tsearchform.FormDestroy(Sender: TObject);
begin
   FreeAndNil(FTracFields);
   FreeAndNil(FJoinOptions);
end;

procedure Tsearchform.FormShow( Sender: TObject) ;
begin
   if flyLabel.Top + flyLabel.Height + 5 < Screen.Height then
      Self.Height := flyLabel.Top + flyLabel.Height + 5
   else
      Self.Height := Screen.Height - 80;
   self.Top := Round(Screen.Height / 2 - Self.Height / 2) - 20;
   if self.Top < 0 then
      self.Top := 0;
end;

procedure Tsearchform.resetBtnClick( Sender: TObject);
var
   i, q: Integer;
   str: TxmlRpcStruct;
   Group: TCheckGroup;

begin
   for i := 0 to pred(FTracFields.Count) do
   begin
      Str := FTracFields.Item[i].GetStruct;

      if Str.HasKey('type') then
      begin
         if (Str.Keys['type'].getString = 'text') or (Str.Keys['type'].getString = 'textarea') then
         begin
            TEdit(self.ControlByName(Str.Keys['name'].getString)).Text := '';
            TComboBox(self.ControlByName(Str.Keys['name'].getString + '_join')).ItemIndex := 0;
         end
         else if Str.Keys['type'].getString = 'select' then
         begin
            TCombobox(self.ControlByName(Str.Keys['name'].getString)).ItemIndex := -1;
            TComboBox(self.ControlByName(Str.Keys['name'].getString + '_join')).ItemIndex := 0;
         end
         else if Str.Keys['type'].getString = 'radio' then
         begin
            Group := TCheckGroup(self.ControlByName(Str.Keys['name'].getString));
            for q := 0 to pred(Group.ControlCount) do
            if Group.Controls[q] is TCheckBox then
               TCheckbox(Group.Controls[q]).Checked := false;
         end;
      end;
   end;
end;

procedure Tsearchform.AddTextElement(Structure: TxmlRpcStruct);
var
   Bez: TLabel;
   Edit: TEdit;
   Option: TCombobox;
   i: Integer;

begin
   Bez := TLabel.Create(self);
   Bez.Parent := self;
   Bez.Top := Y;
   Bez.Left := FLeft;
   if Structure.HasKey('label') then
      Bez.Caption := Structure.Keys['label'].getString
   else if Structure.HasKey('name') then
      Bez.Caption := Structure.Keys['name'].getString
   else
      Bez.Caption := '???';

   Option := TComboBox.Create(self);
   Option.Parent := self;
   Option.Top := Y;
   Option.Left := FElementLeft;
   Option.Width := 80;
   Option.Name := Structure.Keys['name'].getString + '_join';
   for i := 0 to pred(FJoinOptions.Count) do
      Option.Items.Add(FJoinOptions.Names[i]);
   Option.Text := '';
   Option.ItemIndex := 0;
   Option.Style := csDropDownList;

   Edit := TEdit.Create(self);
   Edit.Parent := self;
   Edit.Top := Y;
   Edit.Left := FElementLeft + 90;
   Edit.Width := self.Width - Edit.Left - FLeft;
   Edit.Name := Structure.Keys['name'].getString;
   Edit.Text := '';

   Y := Edit.Top + Edit.Height + FSpacer;
end;

procedure Tsearchform.AddSelectElement(Structure: TxmlRpcStruct);
var
   Bez: TLabel;
   Combo, Option: TCombobox;
   Ar: TxmlRpcArray;
   i: Integer;

begin
   Bez := TLabel.Create(self);
   Bez.Parent := self;
   Bez.Top := Y;
   Bez.Left := FLeft;
   if Structure.HasKey('label') then
      Bez.Caption := Structure.Keys['label'].getString
   else if Structure.HasKey('name') then
      Bez.Caption := Structure.Keys['name'].getString
   else
      Bez.Caption := '???';

   Option := TComboBox.Create(self);
   Option.Parent := self;
   Option.Top := Y;
   Option.Left := FElementLeft;
   Option.Width := 80;
   Option.Name := Structure.Keys['name'].getString + '_join';
   for i := 0 to pred(FJoinOptions.Count) do
      Option.Items.Add(FJoinOptions.Names[i]);
   Option.Text := '';
   Option.ItemIndex := 0;
   Option.Style := csDropDownList;

   Combo := TCombobox.Create(self);
   Combo.Parent := self;
   Combo.Top := Y;
   Combo.Left := FElementLeft + 90;
   Combo.Width := self.Width - Combo.Left - FLeft;
   Combo.Name := Structure.Keys['name'].getString;
   Combo.Text := '';
   Combo.Style := csDropDownList;

   if Structure.HasKey('options') then
   begin
      Ar := Structure.Keys['options'].GetArray;
      for i := 0 to pred(Ar.Count) do
         Combo.Items.Add(Ar.Item[i].GetString);
   end;

   Y := Combo.Top + Combo.Height + FSpacer;
end;

procedure Tsearchform.AddRadioElement(Structure: TxmlRpcStruct);
var
   Bez: TLabel;
   Group: TCheckGroup;
   Check: TCheckbox;
   Ar: TxmlRpcArray;
   i: Integer;

begin
   Bez := TLabel.Create(self);
   Bez.Parent := self;
   Bez.Top := Y;
   Bez.Left := FLeft;
   if Structure.HasKey('label') then
      Bez.Caption := Structure.Keys['label'].getString
   else if Structure.HasKey('name') then
      Bez.Caption := Structure.Keys['name'].getString
   else
      Bez.Caption := '???';

   Group := TCheckgroup.Create(self);
   Group.Parent := self;
   Group.Top := Y;
   Group.Left := FElementLeft;
   Group.Name := Structure.Keys['name'].getString;
   Group.Caption := '';
   Group.Columns := 4;
   //Group.ColumnLayout := clHorizontalThenVertical;
   Group.BorderWidth := 0;

   if Structure.HasKey('options') then
   begin
      Ar := Structure.Keys['options'].GetArray;
      for i := 0 to pred(Ar.Count) do
      begin
         Check := TCheckbox.Create(Group);
         Check.Parent := Group;
         Check.Name := Structure.Keys['name'].getString + '_' + Ar.Item[i].GetString;
         Check.Checked := false;
         Check.Caption := Ar.Item[i].GetString;
      end;
   end;

   Group.AutoSize := true;
   Y := Group.Top + Group.ClientHeight + FSpacer;
end;

procedure Tsearchform.AddCheckboxElement(Structure: TxmlRpcStruct);
var
   Check: TCheckbox;
   Bez: TLabel;

begin
   Bez := TLabel.Create(self);
   Bez.Parent := self;
   Bez.Top := Y;
   Bez.Left := FLeft;
   if Structure.HasKey('label') then
      Bez.Caption := Structure.Keys['label'].getString
   else if Structure.HasKey('name') then
      Bez.Caption := Structure.Keys['name'].getString
   else
      Bez.Caption := '???';

   Check := TCheckbox.Create(self);
   Check.Parent := self;
   Check.Top := Y;
   Check.Left := FElementLeft;
   Check.Name := Structure.Keys['name'].getString;
   Check.Checked := false;
   Check.Caption := '';

   Y := Check.Top + Check.ClientHeight + FSpacer;
end;

procedure Tsearchform.BuildSearchVars;
var
   i, q: Integer;
   str: TxmlRpcStruct;

   Edit: TEdit;
   Combo: TComboBox;
   Group: TCheckGroup;
   Check: TCheckBox;
   Join: TComboBox;

begin
   FSearch := '';
   //FSearch := 'max=' + max.Text;
   IF id.Text <> '' then
      FSearch := FSearch + '&id=' + id.Text;

   for i := 0 to pred(FTracFields.Count) do
   begin
      Str := FTracFields.Item[i].GetStruct;

      if Str.HasKey('type') then
      begin
         if (Str.Keys['type'].getString = 'text') or (Str.Keys['type'].getString = 'textarea') then
         begin
            Edit := TEdit(self.ControlByName(Str.Keys['name'].getString));
            Join := TComboBox(self.ControlByName(Str.Keys['name'].getString + '_join'));
            if Edit.Text <> '' then
               FSearch := Fsearch + '&' + Str.Keys['name'].getString + FJoinOptions.ValueFromIndex[Join.ItemIndex] + Edit.Text;
         end
         else if Str.Keys['type'].getString = 'select' then
         begin
            Combo := TCombobox(self.ControlByName(Str.Keys['name'].getString));
            Join := TComboBox(self.ControlByName(Str.Keys['name'].getString + '_join'));
            if Combo.ItemIndex > -1 then
               FSearch := Fsearch + '&' + Str.Keys['name'].getString + FJoinOptions.ValueFromIndex[Join.ItemIndex] + Combo.Items.Strings[Combo.ItemIndex];
         end
         else if Str.Keys['type'].getString = 'radio' then
         begin
            Group := TCheckGroup(self.ControlByName(Str.Keys['name'].getString));
            for q := 0 to pred(Group.ControlCount) do
            if Group.Controls[q] is TCheckBox then
            begin
               Check := TCheckbox(Group.Controls[q]);
               if Check.Checked then
                  FSearch := FSearch + '&' + Str.Keys['name'].getString + '=' + StringReplace(Check.Name, Str.Keys['name'].getString + '_', '', []);
            end;
         end;
      end;
   end;
   if (FSearch <> '') and (LeftStr(FSearch, 1) = '&') then
      FSearch := Copy(FSearch, 2, MaxInt);
end;

procedure Tsearchform.InitSearchFields;
var
   Obj: TObject;

begin
   Obj := self.ControlByName('owner');
   try
     if Assigned(Obj) then
     begin
         if Obj is TCombobox then
            TCombobox(Obj).ItemIndex := TCombobox(Obj).Items.IndexOf(serverform.User.Text)
         else if Obj is TEdit then
            TEdit(Obj).Text := serverform.User.Text;
     end;
   except
   end;

   max.Text := '500';

   Obj := self.ControlByName('status');

   if Obj is TCheckgroup then
   begin
//      Obj := TCheckgroup(Obj).ControlByName('status_new');
//      if Obj is TCheckbox then
//         TCheckBox(Obj).Checked := true;
   end;
end;

procedure Tsearchform.FinishSearchForm;
begin
   searchBtn.Top := Y + FSpacer;
   resetBtn.Top := Y + FSpacer;
   flyLabel.Top := searchBtn.Top + searchBtn.Height + FSpacer;
end;

initialization
  {$I trac_search.lrs}

end.
