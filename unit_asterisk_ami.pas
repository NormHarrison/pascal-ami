Unit Unit_Asterisk_AMI;


Interface

Uses
  Sockets, SysUtils;

Type
  TAMIPassport = record
    AMI_Addr: string;
    AMI_Port: integer;
    AMI_Username,
    AMI_Secret: string;
  end;


{======================================================================================}


{ Returns a new socket connected to AMI at the location specified in the
  Passport record. This socket is then passed to all AMI actions below }

Function AMILogin(var Passport: TAMIPassport): longint;

//Procedure AMIHangup(AMI_sock, Channel: string);




Implementation

Const
  LF = #10;
  CR = #13;


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

  Sock2Text(AMI_sock, Conn_in, Conn_out);  

  Write(Conn_out, 'Action: Login' + CR + LF,
  'Username: ' + Passport.AMI_Username + CR + LF,
  'Secret: ' + Passport.AMI_Secret + CR + LF);
  Write(Conn_out, CR + LF);

{
  repeat
    ReadLn(Conn_in, Text_line);
    WriteLn(Text_line);
  until Text_line = '';

  Close(Conn_out);
  Close(Conn_in);
  CloseSocket(AMI_sock);
}
end;




End.
