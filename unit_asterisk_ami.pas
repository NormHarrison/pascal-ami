Unit Unit_Asterisk_AMI;


Interface

Uses
  Socket, SysUtils;

Type
  TAMI_Passport = record
    AMI_Addr: string;
    AMI_Port: integer;
    AMI_Username,
    AMI_Secret: string;
  end;


{======================================================================================}


{ Returns a new socket connected to AMI at the location specified in the
  Passport record. This socket is then passed to all AMI actions below }

Function AMILogin(var Passport: TAMI_Passport): longinteger;

//Procedure AMIHangup(AMI_sock, Channel: string);




Implementation

Function AMILogin(var Passport: TAMI_Passport): longinteger;

Var
  AMI_sock:   longinteger;
  Bind_info,
  Remote_info: TSockAddr;

begin
  AMI_sock := fpSocket(AF_INET, SOCK_STREAM, IPPROTO_IP);

  with Bind_info do
    sin_family := AF_INET;
    sin_addr   := 0;
    sin_addr   := 0;
  end;

  If fpBind(AMI_sock, @Bind_info, SizeOf(Bind_info)) = -1 then
  begin
    WriteLn('Failed to bind local socket');
    Halt(1);
  end;

  with Passport, Remote_info do
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
end




End.
