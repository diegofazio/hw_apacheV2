/*
**  hw_apache.prg -- Apache harbour module V2
** (c) DHF, 2020-2021
*/
EXTERN ValToChar
EXTERN ObjToChar
#ifdef __PLATFORM__WINDOWS
#include "hwcore.hbx"
#endif

#define CRLF hb_OsNewLine()
#include "hbthread.ch"
#include "hbclass.ch"

THREAD STATIC request_rec
THREAD STATIC HWBody

STATIC hPP

FUNCTION Main()

   IF hPP == nil
      hPP = __pp_Init()
      __pp_Path( hPP, "~/harbour/include" )
      __pp_Path( hPP, "c:\harbour\include" )
      IF ! Empty( hb_GetEnv( "HB_INCLUDE" ) )
         __pp_Path( hPP, hb_GetEnv( "HB_INCLUDE" ) )
      ENDIF
   ENDIF

   __pp_AddRule( hPP, "#xcommand ? [<explist,...>] => HW_PRINT( '<br>' [,<explist>] )" )
   __pp_AddRule( hPP, "#xcommand ?? [<explist,...>] => HW_PRINT( [<explist>] )" )
   __pp_AddRule( hPP, "#define CRLF hb_OsNewLine()" )
   __pp_AddRule( hPP, "#xcommand TEXT <into:TO,INTO> <v> => #pragma __cstream|<v>:=%s" )
   __pp_AddRule( hPP, "#xcommand TEXT <into:TO,INTO> <v> ADDITIVE => #pragma __cstream|<v>+=%s" )
   __pp_AddRule( hPP, "#xcommand TEMPLATE [ USING <x> ] [ PARAMS [<v1>] [,<vn>] ] => " + ;
      '#pragma __cstream | HW_PRINT( InlinePrg( %s, [@<x>] [,<(v1)>][+","+<(vn)>] [, @<v1>][, @<vn>] ) )' )
   __pp_AddRule( hPP, "#xcommand BLOCKS [ PARAMS [<v1>] [,<vn>] ] => " + ;
      '#pragma __cstream | HW_PRINT( ReplaceBlocks( %s, "{{", "}}" [,<(v1)>][+","+<(vn)>] [, @<v1>][, @<vn>] ) )' )
   __pp_AddRule( hPP, "#command ENDTEMPLATE => #pragma __endtext" )
   __pp_AddRule( hPP, "#xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }" )
   __pp_AddRule( hPP, "#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->" )
   __pp_AddRule( hPP, "#xcommand FINALLY => ALWAYS" )
   __pp_AddRule( hPP, "#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ] => ;" + ;
      "IF <v1> == NIL ; <v1> := <x1> ; END [; IF <vn> == NIL ; <vn> := <xn> ; END ]" )

RETURN NIL

FUNCTION HW_Thread( r )

   LOCAL cFileName

   request_rec := r

   ErrorBlock( {| oError | HW_Print( GetErrorInfo( oError ) ), Break( oError ) } )

   cFileName = HW_FileName()

   IF File( cFileName )

      IF Lower( Right( cFileName, 4 ) ) == ".hrb"

         hb_hrbDo( hb_hrbLoad( 2, cFileName ), HW_Args() )

      ELSE

         hb_SetEnv( "PRGPATH", ;
            SubStr( cFileName, 1, RAt( "/", cFileName ) + RAt( "\", cFileName ) - 1 ) )
         cCode := MemoRead( cFileName )
         HW_Execute( cCode )

      ENDIF

   ELSE

      HW_EXITSTATUS( 404 )

   ENDIF

RETURN

FUNCTION HW_Execute( cCode, ... )

   LOCAL pHrb, uRet, lReplaced := .T.
   LOCAL cHBheaders1 := "~/harbour/include"
   LOCAL cHBheaders2 := "c:\harbour\include"

   ErrorBlock( {| oError | HW_PRINT( GetErrorInfo( oError, @cCode ) ), Break( oError ) } )

   WHILE lReplaced
      lReplaced = ReplaceBlocks( @cCode, "{%", "%}" )
      cCode = __pp_Process( hPP, cCode )
   END

   oHrb = HB_CompileFromBuf( cCode, .T., "-n", "-I" + cHBheaders1, "-I" + cHBheaders2, ;
      "-I" + hb_GetEnv( "HB_INCLUDE" ), hb_GetEnv( "HB_USER_PRGFLAGS" ) )

   IF ! Empty( oHrb )
      pHrb = hb_hrbLoad( 2, oHrb )
      uRet = hb_hrbDo( pHrb, ... )
   ENDIF

RETURN uRet


FUNCTION GetRequestRec()

RETURN request_rec

FUNCTION hb_HtmlEncode( cString )

   LOCAL cChars :=  hb_UTF8ToStr( "<>ÉÍÑÓÚáéíñóú" )
   LOCAL aHtmlUnicEntities := { ;
      "&lt;", ;
      "&gt;", ;
      "&Eacute;", ;
      "&Iacute;", ;
      "&Ntilde;", ;
      "&Oacute;", ;
      "&Uacute;", ;
      "&aacute;", ;
      "&eacute;", ;
      "&iacute;", ;
      "&ntilde;", ;
      "&oacute;", ;
      "&uacute;" }

RETURN hb_StrReplace( hb_UTF8ToStr( cString ), cChars, aHtmlUnicEntities )

FUNCTION ObjToChar( o )

   LOCAL hObj := { => }, aDatas := __objGetMsgList( o, .T. )
   LOCAL hPairs := { => }, aParents := __clsGetAncestors( o:ClassH )

   AEval( aParents, {| h, n | aParents[ n ] := __className( h ) } )

   hObj[ "CLASS" ] = o:ClassName()
   hObj[ "FROM" ]  = aParents

   AEval( aDatas, {| cData | hPairs[ cData ] := __objSendMsg( o, cData ) } )
   hObj[ "DATAs" ]   = hPairs
   hObj[ "METHODs" ] = __objGetMsgList( o, .F. )

RETURN ValToChar( hObj )

// ----------------------------------------------------------------//

FUNCTION ValToChar( u )

   LOCAL cType := ValType( u )
   LOCAL cResult

   DO CASE
   CASE cType == "C" .OR. cType == "M"
      cResult = u

   CASE cType == "D"
      cResult = DToC( u )

   CASE cType == "L"
      cResult = If( u, ".T.", ".F." )

   CASE cType == "N"
      cResult = AllTrim( Str( u ) )

   CASE cType == "A"
      cResult = hb_ValToExp( u )

   CASE cType == "O"
      cResult = ObjToChar( u )

   CASE cType == "P"
      cResult = "(P)"

   CASE cType == "S"
      cResult = "(Symbol)"

   CASE cType == "H"
      cResult = StrTran( StrTran( hb_jsonEncode( u, .T. ), CRLF, "<br>" ), " ", "&nbsp;" )
      IF Left( cResult, 2 ) == "{}"
         cResult = StrTran( cResult, "{}", "{=>}" )
      ENDIF

   CASE cType == "U"
      cResult = "nil"

   OTHERWISE
      cResult = "type not supported yet in function ValToChar()"
   ENDCASE

RETURN cResult


FUNCTION InlinePRG( cText, oTemplate, cParams, ... )

   LOCAL nStart, nEnd, cCode, cResult

   IF PCount() > 1
      oTemplate = Template()
      IF PCount() > 2
         oTemplate:cParams = cParams
      ENDIF
   ENDIF

   WHILE ( nStart := At( "<?prg", cText ) ) != 0
      nEnd  = At( "?>", SubStr( cText, nStart + 5 ) )
      cCode = SubStr( cText, nStart + 5, nEnd - 1 )
      IF oTemplate != nil
         AAdd( oTemplate:aSections, cCode )
      ENDIF
      cText = SubStr( cText, 1, nStart - 1 ) + ( cResult := ExecInline( cCode, cParams, ... ) ) + ;
         SubStr( cText, nStart + nEnd + 6 )
      IF oTemplate != nil
         AAdd( oTemplate:aResults, cResult )
      ENDIF
   END

   IF oTemplate != nil
      oTemplate:cResult = cText
   ENDIF

RETURN cText

// ----------------------------------------------------------------//

FUNCTION ExecInline( cCode, cParams, ... )

   IF cParams == nil
      cParams = ""
   ENDIF

RETURN HW_Execute( "function __Inline( " + cParams + " )" + hb_osNewLine() + cCode, ... )

// ----------------------------------------------------------------//

FUNCTION ReplaceBlocks( cCode, cStartBlock, cEndBlock, cParams, ... )

   LOCAL nStart, nEnd, cBlock
   LOCAL lReplaced := .F.

   hb_default( @cStartBlock, "{{" )
   hb_default( @cEndBlock, "}}" )
   hb_default( @cParams, "" )

   WHILE ( nStart := At( cStartBlock, cCode ) ) != 0 .AND. ;
         ( nEnd := At( cEndBlock, cCode ) ) != 0
      cBlock = SubStr( cCode, nStart + Len( cStartBlock ), nEnd - nStart - Len( cEndBlock ) )
      cCode = SubStr( cCode, 1, nStart - 1 ) + ;
         ValToChar( Eval( &( "{ |" + cParams + "| " + cBlock + " }" ), ... ) ) + ;
         SubStr( cCode, nEnd + Len( cEndBlock ) )
      lReplaced = .T.
   END

RETURN If( hb_PIsByRef( 1 ), lReplaced, cCode )

// ----------------------------------------------------------------//

CLASS Template

   DATA aSections INIT {}
   DATA aResults  INIT {}
   DATA cParams
   DATA cResult

ENDCLASS

FUNCTION HW_LoadHRB( cHrbFile_or_oHRB )

   LOCAL lResult := .F.
   LOCAL pHrb

   IF ValType( cHrbFile_or_oHRB ) == "C"
      IF File( hb_GetEnv( "PRGPATH" ) + "/" + cHrbFile_or_oHRB )
         pHrb := hb_hrbLoad( 2, hb_GetEnv( "PRGPATH" ) + "/" + cHrbFile_or_oHRB )
         AAdd( M->getList, pHrb )
         lResult = .T.
      ENDIF
   ENDIF

   IF ValType( cHrbFile_or_oHRB ) == "P"
      pHrb := hb_hrbLoad( 2, cHrbFile_or_oHRB )
      AAdd( M->getList, pHrb )
      lResult = .T.
   ENDIF

RETURN lResult

// ----------------------------------------------------------------//
FUNCTION HW_GetCookies()

   LOCAL cCookies := HW_GetEnv( "HTTP_COOKIE" )
   LOCAL aCookies := hb_ATokens( cCookies, ";" )
   LOCAL cCookie, hCookies := { => }
   LOCAL hHeadersOut := HW_HeadersOut(), cCookieHeader

   if( hb_HHasKey( hHeadersOut, "Set-Cookie" ) )
      cCookieHeader := hHeadersOut[ "Set-Cookie" ]
      cCookieHeader := Left( cCookieHeader, At( ';', cCookieHeader ) - 1 )
      AAdd( aCookies, cCookieHeader )
   ENDIF

   FOR EACH cCookie in aCookies
      hb_HSet( hCookies, LTrim( SubStr( cCookie, 1, At( "=", cCookie ) - 1 ) ), ;
         SubStr( cCookie, At( "=", cCookie ) + 1 ) )
   NEXT

RETURN hCookies

// ----------------------------------------------------------------//

FUNCTION HW_SetCookie( cName, cValue, nSecs, cPath, cDomain, lHttps, lOnlyHttp )

   LOCAL cCookie := ''

   // check parameters
   hb_default( @cName, '' )
   hb_default( @cValue, '' )
   hb_default( @nSecs, 3600 )   // Session will expire in Seconds 60 * 60 = 3600
   hb_default( @cPath, '/' )
   hb_default( @cDomain, '' )
   hb_default( @lHttps, .F. )
   hb_default( @lOnlyHttp, .F. )

   // we build the cookie
   cCookie += cName + '=' + cValue + ';'
   cCookie += 'expires=' + HW_CookieExpires( nSecs ) + ';'
   cCookie += 'path=' + cPath + ';'
   cCookie += 'domain=' + cDomain + ';'
   cCookie += iif( lOnlyHttp, 'HttpOnly', '' ) + ';'

   HW_HeadersOutSet( "Set-Cookie", cCookie )

RETURN NIL

// ----------------------------------------------------------------//
// CookieExpire( nSecs ) builds the time format for the cookie
// Using this model: 'Sun, 09 Jun 2019 16:14:00'

FUNCTION HW_CookieExpires( nSecs )

   LOCAL tNow := hb_DateTime()
   LOCAL tExpire   // TimeStampp
   LOCAL cExpire   // TimeStamp to String

   hb_default( @nSecs, 60 ) // 60 seconds for this test

   tExpire = hb_NToT( ( hb_TToN( tNow ) * 86400 - hb_UTCOffset() + nSecs ) / 86400 )

   cExpire = CDoW( tExpire ) + ', '
   cExpire += AllTrim( Str( Day( hb_TToD( tExpire ) ) ) ) + ;
      ' ' + CMonth( tExpire ) + ' ' + AllTrim( Str( Year( hb_TToD( tExpire ) ) ) ) + ' '
   cExpire += AllTrim( Str( hb_Hour( tExpire ) ) ) + ':' + AllTrim( Str( hb_Minute( tExpire ) ) ) + ;
      ':' + AllTrim( Str( hb_Sec( tExpire ) ) )

RETURN cExpire

// ----------------------------------------------------------------//

FUNCTION HW_PostPairs( lUrlDecode )

   LOCAL aPairs := hb_ATokens( HW_Body(), "&" )
   LOCAL cPair, uPair, hPairs := { => }
   LOCAL nTable, aTable, cKey, cTag

   hb_default( @lUrlDecode, .T. )
   cTag = If( lUrlDecode, '[]', '%5B%5D' )

   FOR EACH cPair in aPairs
      IF lUrlDecode
         cPair = hb_urlDecode( cPair )
      ENDIF

      IF ( uPair := At( "=", cPair ) ) > 0
         cKey = Left( cPair, uPair - 1 )
         IF ( nTable := At( cTag, cKey ) ) > 0
            cKey = Left( cKey, nTable - 1 )
            aTable = hb_HGetDef( hPairs, cKey, {} )
            AAdd( aTable, SubStr( cPair, uPair + 1 ) )
            hPairs[ cKey ] = aTable
         ELSE
            hb_HSet( hPairs, cKey, SubStr( cPair, uPair + 1 ) )
         ENDIF
      ENDIF
   NEXT

RETURN hPairs

// ----------------------------------------------------------------//

FUNCTION HW_GetPairs()

   LOCAL aPairs := hb_ATokens( HW_Args(), "&" )
   LOCAL cPair, aPair, hPairs := { => }

   FOR EACH cPair in aPairs
      aPair = hb_ATokens( cPair, "=" )
      IF Len( aPair ) == 2
         hPairs[ hb_UrlDecode( aPair[ 1 ] ) ] = hb_UrlDecode( aPair[ 2 ] )
      ELSE
         hPairs[ hb_UrlDecode( aPair[ 1 ] ) ] = ""
      ENDIF
   NEXT

RETURN hPairs

// ----------------------------------------------------------------//

FUNCTION GetErrorInfo( oError, cCode )

   LOCAL EOF := '<br>'
   LOCAL n, cInfo := '<html>'
   LOCAL aLines, nLine
   LOCAL nFix := 0
   LOCAL errline := 0

   cInfo += "Error: " + oError:description + EOF

   IF ! Empty( cCode )
      aLines = hb_ATokens( cCode, Chr( 10 ) )
      IF '#line' $ aLines[ 1 ]
         nFix = Val( SubStr( aLines[ 1 ], 6 ) ) - 2
      ENDIF
      errline := iif( 'line:' $ oError:operation, Val( SubStr( oError:operation, RAt( ":", oError:operation ) + 1 ) ), ProcLine( 2 ) )
   ENDIF

   IF ! Empty( oError:operation )
      IF errline > 0
         IF 'line:' $ oError:operation
            cInfo += "operation: " + 'line:' + AllTrim( str ( errline + nFix ) ) + EOF
         ELSE
            cInfo += "operation: " + oError:operation + EOF
         ENDIF
      ELSE
         cInfo += "operation: " + oError:operation + EOF
      ENDIF
   ENDIF

   IF ! Empty( oError:filename )
      cInfo += "filename: " + oError:filename + EOF
   ENDIF

   IF ValType( oError:Args ) == "A"
      FOR n = 1 TO Len( oError:Args )
         cInfo += "[" + Str( n, 4 ) + "] = " + ValType( oError:Args[ n ] ) + ;
            "   " + ValToChar( oError:Args[ n ] ) + ;
            If( ValType( oError:Args[ n ] ) == "A", " Len: " + ;
            AllTrim( Str( Len( oError:Args[ n ] ) ) ), "" ) + EOF
      NEXT
   ENDIF

   n = 2
   WHILE ! Empty( ProcName( n ) )
      IF AllTrim( ProcName( n ) ) != 'HW_EXECUTE' .AND. AllTrim( ProcName( n ) ) != 'HW_THREAD' .AND. AllTrim( ProcName( n ) ) != 'HB_HRBDO' .AND. AllTrim( ProcName( n ) ) != 'HB_COMPILEFROMBUF'
         cInfo += "called from: " + If( ! Empty( ProcFile( n ) ) .AND. ProcFile( n ) != 'pcode.hrb', ProcFile( n ) + ", ", "" ) + ;
            ProcName( n )  + EOF
// , line: " + AllTrim( Str( ProcLine( n ) ) ) + EOF
      ENDIF
      n++
   END

   IF ! Empty( cCode ) .AND. errline != 0
      cInfo += "Source:" + EOF
      IF errline < 2
         desde = 1
         hasta = 5
      ELSE
         desde = errline - 2
         hasta = errline + 2
      ENDIF
      FOR i = desde TO hasta
         IF i > 0
            IF i == errline
               cInfo += StrZero( errline + nFix, 4 ) + " =>" + hb_HtmlEncode( aLines[ errline ] ) + EOF
            ELSE
               IF i > 0
                  cInfo += StrZero( i + nFix, 4 ) + " " + hb_HtmlEncode( aLines[ i ] )  + EOF
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   cInfo += '</html>'

RETURN cInfo

// ----------------------------------------------------------------//

FUNCTION ExecuteHrb( oHrb, cArgs )

   ErrorBlock( {| oError | HW_PRINT( GetErrorInfo( oError ) ), Break( oError ) } )

RETURN hb_hrbDo( oHrb, cArgs )

// ----------------------------------------------------------------//

FUNCTION Include( cFile )

   LOCAL cPath := HW_GetEnv( "DOCUMENT_ROOT" )

   hb_default( @cFile, '' )
   cFile = cPath + cFile

   IF "Linux" $ OS()
      cFile = StrTran( cFile, '\', '/' )
   ENDIF

   IF File( cFile )
      RETURN MemoRead( cFile )
   ELSE
      HW_PRINT( "Archivo " + cFile + " no encontrado" )
   ENDIF

RETURN ""

// ----------------------------------------------------------------//

FUNCTION HW_HashGet( cKey, xDefault )

   LOCAL xRet

   hb_default( @xDefault, NIL )
   hb_mutexLock( HW_Mutex() )
   xRet := hb_HGetDef( HW_Hash(), cKey, xDefault )
   hb_mutexUnlock( HW_Mutex() )

RETURN xRet

// ----------------------------------------------------------------//

FUNCTION HW_HashSet( cKey, xValue )

   hb_mutexLock( HW_Mutex() )
   hb_HSet( HW_Hash(), cKey, xValue )
   hb_mutexUnlock( HW_Mutex() )

RETURN

// ----------------------------------------------------------------//

FUNCTION HW_BODY()

   IF Empty( HWBody )
      HWBody = HW_GetBody()
   ENDIF

RETURN HWBody

// ----------------------------------------------------------------//

FUNCTION HW_PRINT( ... )

   LOCAL cBuffer := ''
   LOCAL aParams := hb_AParams()

   FOR i = 1 TO Len( aParams )
      cBuffer += valtochar( aParams[ i ] )
   NEXT

   AP_RPUTS( cBuffer )

RETURN
