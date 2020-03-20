program estate_informer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, multiloglaz, main
  { you can add units after this },LazUTF8, parser_utils, parsers, chromium_ext;

{$R *.res}

begin
  {$if declared(UseHeapTrace)}
    UseHeapTrace := False;
  {$ifend}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

