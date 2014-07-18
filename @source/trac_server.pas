unit trac_server;

{
    GPL licensed
    <c> Henrik Genssen hg@mediafactory.de
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, lHTTPUtil;

type

  { Tserverform }

  Tserverform = class(TForm)
     okBtn: TButton;
     cancelBtn: TButton;
     Label4: TLabel;
     URL: TEdit;
     Label1: TLabel;
     Label2: TLabel;
     Label3: TLabel;
     User: TEdit;
     Password: TEdit;
     procedure FormDestroy( Sender: TObject) ;
     procedure FormShow( Sender: TObject) ;
     procedure okBtnClick(Sender: TObject);
     procedure cancelBtnClick( Sender: TObject) ;
     procedure FormCreate( Sender: TObject);
  private
    FURLBase: String;
    FHistory: TStringList;

  public
    property URLBase: String read FURLBase;
  end; 

var
  serverform: Tserverform;

implementation

uses
   trac_main;

{ Tserverform }

procedure Tserverform.okBtnClick(Sender: TObject);
var
   Port: Word;
   Server, Endpoint: String;

begin
   mainform.UseSSL := DecomposeURL(serverform.URL.Text, Server, Endpoint, Port);
   mainform.Server := Server;
   mainform.URL := Endpoint;
   mainform.Port := Port;
   mainform.User := self.User.Text;
   mainform.Password := self.Password.Text;

   FURLBase := URL.Text;
   if RightStr(FURLBase, 1) <> '/' then
      FURLBase := FURLBase + '/';
   FURLBase := StringReplace(FURLBase, 'login/xmlrpc/', '', []);
   FURLBase := StringReplace(FURLBase, 'xmlrpc/', '', []);

   ForceDirectory(ExtractFileDir(GetAppConfigFile(false)));

   FHistory.Clear;
   FHistory.Add(User.Text + '@' + URL.Text);
   FHistory.SaveToFile(GetAppConfigFile(false));
   self.Hide;
   mainform.Show;
   mainform.Init;
end;

procedure Tserverform.FormDestroy( Sender: TObject) ;
begin
   FreeAndNil(FHistory);
end;

procedure Tserverform.FormShow( Sender: TObject) ;
begin
   if URL.Text <> '' then
      User.SetFocus;
   if User.Text <> '' then
      Password.SetFocus;
end;

procedure Tserverform.cancelBtnClick( Sender: TObject) ;
begin
   if mainform.Visible then
      Hide
   else
      Application.Terminate;
end;

procedure Tserverform.FormCreate( Sender: TObject) ;
begin
   FHistory := TStringList.Create;

   if FileExists(GetAppConfigFile(false)) then
      FHistory.LoadFromFile(GetAppConfigFile(false));
   if FHistory.Count > 0 then
   begin
      if Pos('@', FHistory.Strings[0]) > 0 then
      begin
         User.Text := LeftStr(FHistory.Strings[0], Pos('@', FHistory.Strings[0]) - 1);
         URL.Text := Copy(FHistory.Strings[0], Pos('@', FHistory.Strings[0]) + 1, MaxInt);
      end
      else
          URL.Text := FHistory.Strings[0];
   end;
end;

initialization
  {$I trac_server.lrs}

end.
