unit trac_change_select;

{
    GPL licensed
    <c> Henrik Genssen hg@mediafactory.de
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { Tchangeselectform }

  Tchangeselectform = class( TForm)
    Label1: TLabel;
    notifyChkBox: TCheckBox;
    okBtn: TButton;
    cancelBtn: TButton;
    commentLabel: TLabel;
    commentMemo: TMemo;
    procedure FormShow( Sender: TObject) ;
  end;

var
  changeselectform: Tchangeselectform;

implementation

{ Tchangeselectform }

procedure Tchangeselectform.FormShow( Sender: TObject) ;
begin
   notifyChkBox.Checked := true;
   commentMemo.Text := '';
end;

initialization
  {$I trac_change_select.lrs}

end.

