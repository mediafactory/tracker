unit trac_columns;

{
    GPL licensed
    <c> Henrik Genssen hg@mediafactory.de
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { Tcolumnform }

  Tcolumnform = class(TForm)
     cancelBtn: TButton;
     okBtn: TButton;
     upBtn: TButton;
     downBtn: TButton;
     RemoveBtn: TButton;
     AddBtn: TButton;
     Label1: TLabel;
     Label2: TLabel;
     sortColumns: TListBox;
     selectedColumns: TListBox;
     availableColumns: TListBox;
     Notebook1: TNotebook;
     Page1: TPage;
     Page2: TPage;
     procedure AddBtnClick(Sender: TObject);
     procedure cancelBtnClick(Sender: TObject);
     procedure downBtnClick(Sender: TObject);
     procedure okBtnClick(Sender: TObject);
     procedure RemoveBtnClick(Sender: TObject);
     procedure upBtnClick(Sender: TObject);
  private
     procedure saveSelectedColumns;
     procedure updateFieldList;
     procedure updateFieldOrder;

  public
     procedure Init;

  end;

var
  columnform: Tcolumnform;

implementation

uses
   trac_main;

{ Tcolumnform }

procedure Tcolumnform.RemoveBtnClick(Sender: TObject);
begin
   if selectedColumns.ItemIndex >= 0 then
   begin
      availableColumns.Items.Add(selectedColumns.Items.Strings[selectedColumns.ItemIndex]);
      selectedColumns.Items.Delete(selectedColumns.ItemIndex);
      updateFieldList;
   end;
end;

procedure Tcolumnform.upBtnClick(Sender: TObject);
var
  Obj : TObject;
  Str : String;
  Index1, Index2: Integer;

begin
   if sortColumns.ItemIndex > 0 then
   begin
      Index1 := sortColumns.ItemIndex;
      Index2 := Index1 - 1;
      with sortColumns.Items do
      try
         beginUpdate;
         Obj:=Objects[Index1];
         Str:=Strings[Index1];
         Objects[Index1]:=Objects[Index2];
         Strings[Index1]:=Strings[Index2];
         Objects[Index2]:=Obj;
         Strings[Index2]:=Str;
      finally
        EndUpdate;
      end;
      sortColumns.ItemIndex := sortColumns.ItemIndex - 1;
      updateFieldOrder;
   end;
end;

procedure Tcolumnform.saveSelectedColumns;
begin
   selectedColumns.Items.SaveToFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'selectedfields.txt');

   mainform.FieldsSelected.Clear;
   mainform.FieldsSelected.AddStrings(selectedColumns.Items);
end;

procedure Tcolumnform.updateFieldList;
begin
   sortColumns.Clear;
   sortColumns.Items.AddStrings(selectedColumns.Items);

   saveSelectedColumns;
end;

procedure Tcolumnform.updateFieldOrder;
begin
   selectedColumns.Clear;
   selectedColumns.Items.AddStrings(sortColumns.Items);

   saveSelectedColumns;
end;

procedure Tcolumnform.AddBtnClick(Sender: TObject);
begin
   if availableColumns.ItemIndex >= 0 then
   begin
      selectedColumns.Items.Add(availableColumns.Items.Strings[availableColumns.ItemIndex]);
      availableColumns.Items.Delete(availableColumns.ItemIndex);
      updateFieldList;
   end;
end;

procedure Tcolumnform.cancelBtnClick(Sender: TObject);
begin
   Hide;
end;

procedure Tcolumnform.downBtnClick(Sender: TObject);
var
  Obj : TObject;
  Str : String;
  Index1, Index2: Integer;

begin
   if sortColumns.ItemIndex > 0 then
   begin
      Index1 := sortColumns.ItemIndex;
      Index2 := Index1 + 1;
      with sortColumns.Items do
      try
         beginUpdate;
         Obj:=Objects[Index1];
         Str:=Strings[Index1];
         Objects[Index1]:=Objects[Index2];
         Strings[Index1]:=Strings[Index2];
         Objects[Index2]:=Obj;
         Strings[Index2]:=Str;
      finally
        EndUpdate;
      end;
      sortColumns.ItemIndex := sortColumns.ItemIndex + 1;
      updateFieldOrder;
   end;
end;

procedure Tcolumnform.okBtnClick(Sender: TObject);
begin
   Hide;
   mainform.LoadList;
end;

procedure Tcolumnform.Init;
var
   i: Integer;

begin
   // columns select
   selectedColumns.Clear;
   availableColumns.Clear;

   selectedColumns.Items.AddStrings(mainform.FieldsSelected);
   for i := 0 to pred(mainform.FieldsAvailable.Count) do
   begin
      if selectedColumns.Items.IndexOf(mainform.FieldsAvailable.Strings[i]) < 0 then
         availableColumns.Items.Add(mainform.FieldsAvailable.Strings[i]);
   end;
   if selectedColumns.Items.IndexOf('id') < 0 then
      availableColumns.Items.Add('id');
   if selectedColumns.Items.IndexOf('time created') < 0 then
      availableColumns.Items.Add('time created');
   if selectedColumns.Items.IndexOf('time changed') < 0 then
      availableColumns.Items.Add('time changed');

   // column order
   sortColumns.Clear;
   sortColumns.Items.AddStrings(selectedColumns.Items);
end;

initialization
  {$I trac_columns.lrs}

end.

