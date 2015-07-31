'FlowScript "platform." plugin

namespace FlowPlatform
    declare function init() as integer
    declare function flowCommand( byref table as string, byref plugin as string, byref cmd as string, byref arg as string ) as string
    
    declare function _okValue( v as string ) as string
    declare function _okSetPart( table as string, key as string, value as string ) as string
    declare function _okNewBundle( key as string, value as string ) as string
    declare function _readFile( table as string ) as string
    declare function _writeFile( table as string, arg as string ) as string
    declare function _listFilesIn( path as string ) as string
    declare function _loadConfig( n as string ) as string
    declare function _createDateSerial() as string
    declare function _createDateBundle() as string

    function init() as integer
        return Flow.addPlugin( @FlowPlatform.flowCommand )
    end function
    
    function flowCommand( byref table as string, byref plugin as string, byref cmd as string, byref arg as string ) as string
        if( plugin <> "platform" ) then
            return Flow.CMD_NOT_OURS
        end if
        dim as string tmp_s, ts
        dim as integer ti
        select case( cmd )
            case "ondonegoto", "onerrorgoto", "file", "args"
                return FlowPlatform._okSetPart( table, cmd, arg )
                
            case "newfilespec", "filespec",  _
                    "newexecspec", "execspec"
                return FlowPlatform._okNewKit( "file", arg )
            
            case "readfile"
                return FlowPlatform._readFile( table )
            case "writefile"
                return FlowPlatform._writeFile( table, arg )
                
            case "getcurrentpath"
                return FlowPlatform._okValue( curdir )
            case "setcurrentpath"
                chdir iif( (len( arg ) > 0), arg, table )
            case "listfiles"
                return FlowPlatform._okValue( FlowPlatform._listFilesIn( table ) )
                
            case "loadconfig"
                return FlowPlatform._okValue(   _
                        FlowPlatform._loadConfig( table )  _
                    )
            case "getdateserial"
                return FlowPlatform._okValue( FlowPlatform._createDateSerial() )
            case "getdatekit"
                return FlowPlatform._okValue( FlowPlatform._createDateBundle() )
            
            case "setenv", "setenvironmentvariable"
                setenviron (arg & "=" & table)
            case "getenv", "getenvironmentvariable"
                return FlowPlatform._okValue( environ( table ) )
            
            case "exec"
                ts = Flow.getPart( table, "ondonegoto" )
                if( exec( Flow.getPart( table, "file" ), Flow.getPart( table, "args" ) ) = -1 ) then
                    ts = Flow.getPart( table, "onerrorgoto", ts )
                end if
                return (Flow.CMD_GOTO & ts)
                
            case "getrandomnumber":
                ti = val( Flow.getPart( table, "from" ) )
                ti = ((rnd() * (val( Flow.getPart( table, "to" ) ) - ti)) + ti)
                return FlowPlatform._okValue( str( ti ) )
            
            case "browseto"
                Utils.browseTo( table )
                
            case "getid"
                #ifdef __FB_WIN32__
                    FlowPlatform._okValue( "native/w32" )
                #endif
                #ifdef __FB_LINUX__
                    FlowPlatform._okValue( "native/linux" )
                #endif
                #ifdef __FB_DOS__
                    FlowPlatform._okValue( "native/dos" )
                #endif
                                
        end select
        return (Flow.CMD_ERROR & "Invalid command: platform." & cmd)
    end function
    
    function _okValue( v as string ) as string
        return (Flow.CMD_OK_SET & v)
    end function
    
    function _okSetPart( table as string, key as string, value as string ) as string
        return (Flow.CMD_OK_SET & Flow.setPart( table, key, value ))
    end function
    
    function _okNewBundle( key as string, value as string ) as string
        return (Flow.CMD_OK_SET & Flow.newBundleWithKV( key, value ))
    end function
        
    function _readFile( table as string ) as string
        dim as integer er = 0
        dim as string res = Utils.readFile( Flow.getPart( table, "file" ), er )
        if( er = 0 ) then
            dim as string cont = Flow.getPart( table, "ondonegoto" )
            Flow.setTableValue( res )
            return (Flow.CMD_GOTO & cont)
        else
            return (Flow.CMD_GOTO & Flow.getPart( table, "onerrorgoto" ))
        end if
    end function
    
    function _writeFile( table as string, arg as string ) as string
        ' table is a filespec with "value" set to what is to be written
        ' arg will replace "value" if not empty
        if( arg <> "" ) then
            arg = Flow.getPart( table, "value" )
        end if
        dim as integer er = Utils.writeFile( Flow.getPart( table, "file" ), arg )
        if( er = FALSE ) then
            dim as string cont = Flow.getPart( table, "ondonegoto" )
            Flow.setTableValue( "1" )
            return (Flow.CMD_GOTO & cont)
        else
            Flow.setTableValue( "0" )
            return (Flow.CMD_GOTO & Flow.getPart( table, "onerrorgoto" ))
        end if        
    end function
    
    function _listFilesIn( path as string ) as string
        if( len( path ) > 0 ) then
            #ifdef __FB_WIN32__
                if( mid( path, (len( path ) - 1) ) <> "\" ) then
                    path &= "\"
                end if
            #else
                if( mid( path, (len( path ) - 1) ) <> "/" ) then
                    path &= "/"
                end if
            #endif
        end if
        dim as string file = dir( (path & "*") )
        dim as string result = "", z
        dim as integer index = 0, l
        while( len( file ) > 0 )
            z = str( index )
            result = Flow.setPart( result, z, file )
            file = dir()
            index += 1
        wend
        return Flow.setPart( result, "_count", str( index ) )
    end function
    
    function _loadConfig( n as string ) as string
        dim as string key = "", value, l
        dim as integer ff = freefile(), e
        dim as string prefix = "", result = ""
        if( open( n for input as ff ) = 0 ) then
            while( not eof( ff ) )
                line input #ff, l
                if( (asc( l, 1 ) = 91) and (asc( l, len( l ) ) = 93) ) then  '[ and ]
                    prefix = (mid( l, 2, (len( l ) - 2) ) & ".")
                elseif( asc( l, 1 ) = 35 ) then
                    ' comment
                else
                    e = instr( l, "=" )
                    if( e > 0 ) then
                        result = Flow.setPart(   _
                                result,  _
                                (prefix & mid( l, 1, (e - 1) )),  _
                                mid( l, (e + 1) )  _
                            )
                    end if
                end if
            wend
        end if
        return result
    end function
    
    function _createDateSerial() as string
        dim ts as string = (date() & time())
        return (mid( ts, 7, 4 ) & mid( ts, 1, 2 ) & mid( ts, 4, 2 ) & _
                mid( ts, 11, 2 ) & mid( ts, 14, 2 ) & mid( ts, 17, 2 ) )
    end function
    
    function _createDateBundle() as string
        dim as string ts = (date() & time())
        dim as string p = ""
        p = Flow.setPart( p, "year", mid( ts, 7, 4 ) )
        p = Flow.setPart( p, "month", mid( ts, 1, 2 ) )
        p = Flow.setPart( p, "day", mid( ts, 4, 2 ) )
        p = Flow.setPart( p, "hour", mid( ts, 11, 2 ) )
        p = Flow.setPart( p, "minute", mid( ts, 14, 2 ) )
        p = Flow.setPart( p, "day", mid( ts, 17, 2 ) )
        return p
    end function
end namespace

    