(************************************************************************************
*
*                 Function "fbBase64Encoding" for MOVI-PLC (R)                 
*
************************************************************************************
* Provides functions for MOVI-PLC (R)
* (c) 2009 by SEW-EURODRIVE Argentina S.A.
************************************************************************************
*
* fbBase64Encoding
*
* FUNCTION
* Convert a Bytes to Base64 Encoding 
* based on RFC 3548
*	
************************************************************************************)
FUNCTION BLOCK fbBase64Encoding
VAR INPUT
	Enable: BOOL;			  (* Enable the module *)
	InputBuffer:DWORD; 	(* Address (ADR) of the source data*)
	Size:DWORD; 			  (* Length of the source in Bytes *)
	OutputBuffer:DWORD; (* Address (ADR) of the destination date converted in Base 64  *)
END VAR

VAR OUTPUT
	Done:BOOL;			(* Converted *)
	Busy:BOOL;			(* The module is working *)
	Error:BOOL;			(* Error *)
	ErrorID:DWORD;	(* Error ID *)
	dwRead:DWORD;		(* Length at output in bytes *)
END VAR

{library private}
{flag nowatch on}
VAR
	nSteps: DWORD;							          (* Steps of the general FOR *)
	nIndex: UINT;								          (* Steps of the Base 64 Index *)
	nSend: UINT;							  	        (* Steps to send the Base 64 converted *)
	pByteArray: POINTER TO BYTE;			    (* Pointer to the input array *)
	pOutByteArray: POINTER TO BYTE;		  	(* Pointer to the output array *)
	dwBitPattern :DWORD;				      		(* Bit Pattern of Base 64 *)
	ayBase64Index: ARRAY [0..3] OF BYTE;	(* Index of Base 64 to convert in ASCII *)
	ayBase64Dec: ARRAY[0..3] OF BYTE;		  (* Converted Index in ASCII *)
	bPaddingOne: BOOL;						        (* Enable padding One *)
	bPaddingTwo: BOOL;						        (* Enable padding Two *)
	dwInputAddress: DWORD;					      (* To calculate the Length at output *)
END VAR
{flag off}


(*****************************************************************************************************************************************************
	Programmed:	14.08.2009	Cristian Adamo  <cristian.adamo@sew-eurodrive.com.ar>  -  SEW EURODRIVE Argentina S.A.	
	Current Version: 1.000 		
	Change:	Ver			Date			  Author				  Description:
				  v1.000	24.08.2009	Cristian Adamo	Generation of Program
******************************************************************************************************************************************************)

IF (Enable) THEN
	dwInputAddress := OutputBuffer;

	FOR nSteps := 0 TO Size-1 BY 3	DO
		Busy := TRUE;

		IF (Size MOD 3 = 1 AND nSteps = Size-1) THEN
			(* Padding for two Sextets-Streams missing  *)
			pByteArray := InputBuffer+nSteps;
			dwBitPattern :=  SHL( BYTE_TO_DWORD(pByteArray^), 16);
			bPaddingTwo := TRUE;
		ELSE IF (Size MOD 3 = 2 AND nSteps = Size-2) THEN
      (* Padding for one Sextet-Streams missing  *)
			pByteArray := InputBuffer+nSteps;
			dwBitPattern := SHL(BYTE_TO_DWORD(pByteArray^),16);
			pByteArray := InputBuffer+(nSteps+1);
			dwBitPattern :=  dwBitPattern + SHL(BYTE_TO_DWORD(pByteArray^),8);
			bPaddingOne := TRUE;
		ELSE
			pByteArray := InputBuffer+nSteps;
			dwBitPattern := SHL(BYTE_TO_DWORD(pByteArray^),16);
			pByteArray := InputBuffer+(nSteps+1);
			dwBitPattern :=  dwBitPattern + SHL(BYTE_TO_DWORD(pByteArray^),8) ;
			pByteArray := InputBuffer+(nSteps+2);
			dwBitPattern := dwBitPattern + BYTE_TO_DWORD(pByteArray^);
		END IF

		(* Generates the Base64 Index *)
		ayBase64Index[0] := DWORD_TO_BYTE(SHR((dwBitPattern AND 2#111111000000000000000000), 18));
		ayBase64Index[1] := DWORD_TO_BYTE(SHR((dwBitPattern AND 2#111111000000000000), 12));
		ayBase64Index[2] := DWORD_TO_BYTE(SHR((dwBitPattern AND 2#111111000000), 6));
		ayBase64Index[3] := DWORD_TO_BYTE(dwBitPattern AND 2#111111);

		(* Base64 Index To ASCII *)
		(* ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/ *)
		FOR nIndex := 0 TO 3 DO
			CASE ayBase64Index[nIndex] OF
				0..25:	(* Upper Case - 65 to 90 - A to Z*)
						ayBase64Dec[nIndex] := 65+ayBase64Index[nIndex];
				26..51:	(* Lower Case - 97 to 122 - a to z*)
						ayBase64Dec[nIndex] := 97+(ayBase64Index[nIndex]-26);
				52..61:	(* Numbers - 48 to 57 - 0 to 9*)
						ayBase64Dec[nIndex] := 48+(ayBase64Index[nIndex]-52);
				62: 	(* Sum symbol - 43 *)
						ayBase64Dec[nIndex] := 43;
				63: 	(* Slash symbol - 47 *)
						ayBase64Dec[nIndex] := 47;
			END CASE
		END FOR

		(* Padding *)
		IF (bPaddingTwo) THEN
			ayBase64Dec[2] := 61;
			ayBase64Dec[3] := 61;
		ELSE IF (bPaddingOne) THEN
			ayBase64Dec[3]:= 61;
		END IF

		(* Store data in output array *)
		FOR nSend := 0 TO 3 DO
			pOutByteArray := OutputBuffer;
			pOutByteArray^:= ayBase64Dec[nSend];
			OutputBuffer := OutputBuffer+1;
		END FOR

		bPaddingTwo := FALSE;
		bPaddingOne := FALSE;

	END FOR

	Busy := FALSE;
	dwRead := (pOutByteArray-dwInputAddress)+1;
	Done := TRUE;

ELSE
	bPaddingTwo := FALSE;
	bPaddingOne := FALSE;
	Busy := FALSE;
	Done := FALSE;
	Error := FALSE;
	ErrorID := 0;
END IF



