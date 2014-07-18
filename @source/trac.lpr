program tracker;

{
    GPL licensed
    <c> Henrik Genssen hg@mediafactory.de
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources,
  trac_server,
  trac_main,
  trac_columns,
  trac_search,
  trac_change_select, trac_change_text, SemaphorGridLPK;

{$IFDEF WINDOWS}{$R trac.rc}{$ENDIF}

begin
  {$I trac.lrs}
  Application.Initialize;
  Application.ShowMainForm := false;
  Application.CreateForm(Tmainform, mainform);
  Application.CreateForm(Tserverform, serverform);
  Application.CreateForm(Tcolumnform, columnform);
  Application.CreateForm(Tsearchform, searchform);
  Application.CreateForm( Tchangeselectform, changeselectform);
  Application.CreateForm( Tchangetextform, changetextform);
  serverform.Show;
  Application.Run;
end.

