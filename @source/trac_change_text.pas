unit trac_change_text;

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

  { Tchangetextform }

  Tchangetextform = class( TForm)
    notifyChkBox: TCheckBox;
    textEdit: TEdit;
    Label2: TLabel;
    okBtn: TButton;
    cancelBtn: TButton;
    commentLabel: TLabel;
    commentMemo: TMemo;
    procedure FormShow( Sender: TObject);

  end;

var
  changetextform: Tchangetextform;

implementation

{ Tchangetextform }

procedure Tchangetextform.FormShow( Sender: TObject) ;
begin
   notifyChkBox.Checked := true;
   commentMemo.Text := '';
end;

initialization
  {$I trac_change_text.lrs}

end.

