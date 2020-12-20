Unit Unit_Asterisk_AMI;


Interface

Uses
  Sockets, SysUtils;

Type
  TStringArray = array of string;

  TAMIPassport = record
    AMI_Addr: string;
    AMI_Port: integer;
    AMI_Username,
    AMI_Secret: string;
  end;


{======================================================================================}


{ Returns a new socket connected to AMI at the location specified in the
  Passport record. This socket is then passed to all AMI actions below }


Procedure AMISendAction(var AMI_sock: longint; const Fields: TStringArray);

Function AMILogin(var Passport: TAMIPassport): longint;

Procedure AMIHangup(var AMI_sock: longint; Channel: string);




Implementation


Procedure AMISendAction(var AMI_sock: longint; const Fields: TStringArray);

Var 
  LE: string = #13 + #10;
  Msg:         string = '';
  Field_count: integer = 0;

begin
  If Length(Fields) mod 2 <> 0 then
  begin
    WriteLn('The fields array must contain an even number of elements');
    Exit();
  end;

  repeat
    Msg := Msg + Fields[Field_count * 2] + ':' + ' ' + Fields[Field_count * 2 + 1] + LE;

    If Field_count = ((Length(Fields) Div 2) - 1) then
      Msg := Msg + LE;

    Inc(Field_count);
  until Field_count = (Length(Fields) Div 2);

  Write(Msg);
  fpSend(AMI_sock, @Msg + $1, Length(Msg), 0);
end;



Function AMILogin(var Passport: TAMIPassport): longint;

Var
  AMI_sock:    longint;
  Bind_info,
  Remote_info: TSockAddr;
  Conn_in,
  Conn_out:    Text;
  Text_line:   string;

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
    Halt(1);
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
    Halt(1);
  end;

  AMISendAction(AMI_sock,
    ['Action',  'Login',
     'Username', Passport.AMI_Username,
     'Secret',   Passport.AMI_Secret]);

  AMILogin := AMI_sock;
end;



Procedure AMIHangup(var AMI_sock: longint; Channel: string);

Var
  Conn_in,
  Conn_out:  text;
  Text_line: string;

begin
  Sock2Text(AMI_sock, Conn_in, Conn_out);

  repeat
    ReadLn(Conn_in, Text_line);
    WriteLn(Text_line);
  until Text_line = '';

  Close(Conn_out);
  Close(Conn_in);
end;

End.
