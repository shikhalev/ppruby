unit ppRubyDsgn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, RubyConnection;

procedure Register;

implementation

procedure Register;
 begin
  {$I rubyconnection_icon.lrs}
  RegisterComponents('Misc',[TRubyConnection]);
 end;

end.

