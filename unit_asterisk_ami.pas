Unit Unit_Asterisk_AMI;


Interface

Uses
  Sockets, SysUtils;

Type
  { Generic dynamic array of short strings }

  TStringArray = array of string;

  { A record to be filled with the AMI server's details,
    used for location information and authentication }

  TAMIPassport = record
    AMI_Addr: string;
    AMI_Port: integer;
    AMI_Username,
    AMI_Secret: string;
  end;


{ A generic procedure for sending any AMI action based on the provided fields }

Procedure AMISendAction(var AMI_sock: longint; const Fields: TStringArray);


{ Returns a new socket connected to AMI at the location specified in the
  Passport record. This socket can then be passed to any future call to
  AMISendAction() for as long as the authtimeout setting in manager.conf
  allows (a default of 30 seconds) }

Function AMILogin(var Passport: TAMIPassport): longint;


{=======================================================================================}


Implementation

Procedure AMISendAction(var AMI_sock: longint; const Fields: TStringArray);

Var 
  CRLF:              string = #13 + #10;
  Action:            string = '';
  Response:          string = '';
  Line:              string = '';
  Part:              Char;
  Field_count:       integer = 0;
  Response_complete: boolean;

begin
  If Length(Fields) mod 2 <> 0 then
  begin
    WriteLn('The fields array must contain an even number of elements');
    Exit();
  end;

  repeat
    Action := Action + Fields[Field_count * 2] + ':' + ' ' + Fields[Field_count * 2 + 1] + CRLF;

    Inc(Field_count);

    If Field_count = (Length(Fields) Div 2) then
      Action := Action + CRLF;

  until Field_count = (Length(Fields) Div 2);
  fpSend(AMI_sock, @Action + $1, Length(Action), 0);

  Sleep(100);

{
  repeat
    Line := '';
    repeat
      fpRecv(AMI_sock, @Part, SizeOf(Part), 0);
      Line := Line + Part;
    until Pos(CRLF, Line) <> 0;

    Response := Response + Line;
    Write(Line);
  until false;
}
  //WriteLn(Response);
end;



Function AMILogin(var Passport: TAMIPassport): longint;

Var
  AMI_sock:    longint;
  Bind_info,
  Remote_info: TSockAddr;

begin
  AMI_sock := fpSocket(AF_INET, SOCK_STREAM, IPPROTO_IP);

  with Bind_info do
  begin
    sin_family := AF_INET;
    sin_addr   := StrToNetAddr('0.0.0.0');
    sin_port   := HtoNs(0);
  end;

  If fpBind(AMI_sock, @Bind_info, SizeOf(Bind_info)) = -1 then
  begin
    WriteLn('Failed to bind local socket');
    Exit(-1);
  end;

  with Passport, Remote_info do
  begin
    sin_family := AF_INET;
    sin_addr   := StrToNetAddr(AMI_Addr);
    sin_port   := HtoNs(AMI_Port);
  end;

  If fpConnect(AMI_sock, @Remote_info, SizeOf(Remote_info)) = -1 then
  begin;
    WriteLn(Format('The AMI server located at "%s:%d" could not be reached',
    [Passport.AMI_Addr, Passport.AMI_Port]));
    Exit(-1);
  end;

  AMISendAction(AMI_sock,
  ['Action',  'Login',
   'Username', Passport.AMI_Username,
   'Secret',   Passport.AMI_Secret]);

  AMILogin := AMI_sock;
end;

End.
