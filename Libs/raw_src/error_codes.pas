(******************************************************************************************
* ErrorCodes of FTP and SMTP modules  
* (c) 2009 by SEW-EURODRIVE Argentina S.A.
*******************************************************************************************
* FIELDTEST VERSION 0.8!!! - 21.08.2009
*******************************************************************************************) 
VAR GLOBAL CONSTANT

(* FILE HANDLING ERRORS *)
 FH_FILEASYNC_FILENOTFOUND : WORD := 1;
 FH_FILEASYNC_FILE_NO_ACCESS : WORD := 2;
 FH_FILEASYNC_FILE_NOT_CLOSED : WORD := 3;
 FH_FILEASYNC_FILE_NOT_DELETED: WORD := 4;
 FH_FILEASYNC_TOO_MANY_OPEN_FILES : WORD := 6;
 FH_NO_QUEUE: WORD := 7;


(********************************* MC_SendEmail_SMTP *****************************)
(*ERRORS *)
SMTP_TIMEOUT: UDINT := 16#FE00403;								      (*403 - Timeout, The module can't comunicate with the server or it doesn't respond *)

(* SMTP  PROTOCOL ERRORS  *)
SMTP_SERV_NOT_AVAILABLE: 	UDINT := 16#FE00421;				  (*421 - Server not available *)
SMTP_REQ_MAIL_UNAVAILABLE: UDINT :=16#FE00450;				  (*450 - Requested mail unavailable *)
SMTP_REQ_ACT_ABORTED: UDINT := 16#FE00451;				  	  (*451 - Requested action aborted *)
SMTP_INSUF_SYS_STORAGE: UDINT := 16#FE00452;					  (*452 - Insuficinet system storage *)
SMTP_SYNTAX_ERROR: UDINT := 16#FE00500;						      (*500 - Syntac error *)
SMTP_SYNTAX_IN_ARGUMENT: UDINT := 16#FE00501;					  (*501 - Syntax in argument *)
SMTP_COMM_NOT_IMPLEMENTED: UDINT := 16#FE00502;				  (*502 - Command not implemented *)
SMTP_BAD_SEQUENCE_COMMAND: UDINT := 16#FE00503;			    (*503 - Bad sequence of command *)
SMTP_COMM_PARAM_NOT_IMPLEMENTED: UDINT := 16#FE00504;	  (*504 - Command parameter not implemented *)
SMTP_TTLS_NOT_STARTED: UDINT := 16#FE00530;					    (*530 - TTLS not Started *)
SMTP_AUTHENTICATION_UNSUCCESSFUL: UDINT := 16#FE00535;  (*535 - Authentication Unsuccessful *)
SMTP_MAILBOX_UNAVAILABLE: UDINT := 16#FE00550;				  (*550 - Mailbox unavailable *)
SMTP_USER_NOT_LOCAL: UDINT := 16#FE00551;						    (*551 - User isn't local*)
SMTP_EXCEEDED_STORAGE_ALLOCATION: UDINT  := 16#FE00552;	(*552 - Exceeded storage allocation *)
SMTP_MAILBOX_NAME_NOT_ALLOWED: UDINT := 16#FE00553;		  (*553 - mailbox name not allowed *)
SMTP_TRANSACTION_FAILED: UDINT:= 16#FE00554;					  (*554 - transaction failed *)

(************************************ MC_Client_FTP **********************************)
(* ERRORS *)
FTP_TIMEOUT:UDINT := 16#FF003B0;					              (*3B0 - Timeout, The module can't comunicate with the server or it doesn't respond *)
FTP_GET_IP:UDINT := 16#FF003B1;						              (*3B1 - The module can't get the ip address of the plc *)
FTP_FILE_MISSING:UDINT := 16#FF003B2;			              (*3B2 - File Missing *)

(*  FTP  PROTOCOL ERRORS *)
FTP_COMM_NOT_IMPLEMENTED_SUPERF: UDINT := 16#FF00202;		(*202 - Command not implemented *)
FTP_SERV_NOT_AVAILABLE: UDINT := 16#FF00421;					  (*421 - Server not available *)
FTP_CAN_NOT_OPEN_CONNECTION: UDINT :=16#FF00425;			  (*425 - Can't open connection *)
FTP_CONNECTION_CLOSED: UDINT := 16#FF00426;					    (*426 - Connection closed *)
FTP_REQ_FILE_ACTION_NOT_TAKEN: UDINT := 16#FF00450;			(*450 - Requested file action not taken *)
FTP_REQ_ACTION_ABORTED: UDINT := 16#FF00451;					  (*451 - Requested action aborted *)
FTP_REQ_ACTION_NOT_TAKEN_STORAGE: UDINT := 16#FF00452;	(*452 - Requested action not taken, storage *)
FTP_SYNTAX_ERROR: UDINT := 16#FF00500;							    (*500 - Syntax error *)
FTP_SYNTAX_ERROR_IN_ARGUMENTS: UDINT := 16#FF00501;			(*501 - Syntax Error in argument *)
FTP_COMM_NOT_IMPLEMENTED: UDINT := 16#FF00502;				  (*502 - Command not implemented *)
FTP_COMM_NOT_IMPLEMENTED_ARGUMENT: UDINT := 16#FF00504;	(*504 - Command parameter not implemented *)
FTP_NOT_LOGGED_IN: UDINT := 16#FF00530;							    (*530 - Not logged in *)
FTP_NEED_ACCOUNT_TO_STORING_FILE: UDINT := 16#FF00532;	(*532 - Need account to storing file *)
FTP_REQUESTED_ACTION_NOT_TAKEN: UDINT := 16#FF00550;		(*550 - Requested action not taken *)
FTP_ACTION_ABORTED: UDINT  := 16#FF00551;						    (*551 - Action aborted *)
FTP_EXCEEDED_STORAGE_ALLOCATION: UDINT := 16#FF00552;		(*552 - Exceeded storage allocation *)
FTP_FILENAME_NOT_ALLOWED: UDINT:= 16#FF00553;				    (*553 - Filename not allowed *)

END VAR

