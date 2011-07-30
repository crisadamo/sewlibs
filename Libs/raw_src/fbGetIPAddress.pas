(*******************************************************************************
*               Function Block "fbGetIpAddress" for MOVI-PLC (R)
********************************************************************************
* Provides functions for MOVI-PLC (R)
* (c) 2009 by SEW-EURODRIVE Argentina S.A.
********************************************************************************
*
* fbGetIpAddress
*
* FUNCTION
* Returns the IP address of the Local MoviPLC
*
********************************************************************************
* STYLE: tab: 2 (as space) ; Font Size: 12 ; Column: 80
*******************************************************************************)

FUNCTION BLOCK fbGetIpAddress
VAR INPUT
  Enable: BOOL;
END VAR

VAR OUTPUT
  Done: BOOL;
  Error: BOOL;
  ErrorID: UDINT;
  IPaddress: STRING(15);
END VAR

{library private}
{flag nowatch on}
VAR
  fbGetIP: PlcParameterMaster;
  dwIPTemp: DWORD;
  yIPTempOctet1: BYTE;
  yIPTempOctet2: BYTE;
  yIPTempOctet3: BYTE;
  yIPTempOctet4: BYTE;
  bFlagRead: BOOL;
END VAR
{flag off}

(*******************************************************************************
 * Programmed:	12.03.2009  Cristian Adamo <cristian.adamo@gmail.com>
 * Current Version: 1.000
 * Change:	Ver     Date        Author          Description:
 *				  v1.000  12.03.2009  Cristian Adamo  Generation of Program
 ******************************************************************************)
fbGetIP(
  Execute := bFlagRead,
  Service := ML_PRM_SVC_READ,
  Index := 10489,
  Subindex := 1,
  DataIn := 0
);

IF (Enable) THEN
  bFlagRead := TRUE;

  (* Convert UDINT TO IP ADDRESS*)
  IF (fbGetIP.DataOut > 0) THEN
    dwIPTemp := UDINT_TO_DWORD(fbGetIP.DataOut);

    yIPTempOctet1 := DWORD_TO_BYTE(
                    SHR((dwIPTemp AND 2#11111111000000000000000000000000), 24));
    yIPTempOctet2 := DWORD_TO_BYTE(
                      SHR((dwIPTemp AND 2#111111110000000000000000), 16));
    yIPTempOctet3 := DWORD_TO_BYTE(SHR((dwIPTemp AND 2#1111111100000000), 8));
    yIPTempOctet4 := DWORD_TO_BYTE(dwIPTemp AND 2#11111111);

    IPaddress := BYTE_TO_STRING(yIPTempOctet1);
    IPaddress := CONCAT(IPaddress, CONCAT('.', BYTE_TO_STRING(yIPTempOctet2)));
    IPaddress := CONCAT(IPaddress, CONCAT('.', BYTE_TO_STRING(yIPTempOctet3)));
    IPaddress := CONCAT(IPaddress, CONCAT('.', BYTE_TO_STRING(yIPTempOctet4)));
  END IF;

  Done := fbGetIP.Done;
  Error := fbGetIP.Error;
  ErrorID := fbGetIP.ErrorID;
ELSE
  bFlagRead := FALSE;
  Done := FALSE;
  Error := FALSE;
  ErrorID := 0;
END IF;
