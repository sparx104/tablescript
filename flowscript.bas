' tokeniser
#include "../lib/libmain.bas"

'includes for modules are after TScript

'#define TSCRIPT_LOG

#ifdef TSCRIPT_LOG
#define TSCRIPT_LOG_LABELS
#endif

' these are used internally as results for boolean operators
#define OK_SET_TRUE "21"
#define OK_SET_FALSE "20"
#define RETURN_OK_SET(n) return( (Flow.CMD_OK_SET & n) )
#define RETURN_ERROR(n) return( (Flow.CMD_ERROR & n) )

namespace Flow
    ' These are for returning from flowCommand()
    const as string  _
            CMD_ERROR   = "0",  _
            CMD_OK      = "1",  _
            CMD_OK_SET  = "2",  _
            CMD_GOTO    = "3",  _
            CMD_SKIP    = "4",  _
            CMD_STOP    = "5",  _
            CMD_NOT_OURS = "9"
            
    declare function addPlugin( pl as function( as string, as string, as string, as string ) as string ) as integer
    
    declare function load( script as string, vars as DICTSTRING = "" ) as integer
    declare function run( from as string = "" ) as integer

    ' although this is byref don't change it - the return value can change if it needed, it's just to
    '  save copying
    declare function flowCommand( byref table as string, byref plugin as string, byref cmd as string, byref arg as string ) as string

    declare function setPart( d as string, sk as string, sv as string ) as string
    declare function getPart( from as string, gk as string, def as string = "" ) as string
    declare function newBundle( from as string = "", keys as string = "" ) as string
    declare function newBundleWithKV( key as string, value as string ) as string
    
    declare function _getVar( k as string, er as byte ptr ) as string
    declare sub _setVar( k as string, v as string )
    declare function _toBool( v as string ) as integer
    
    dim _plugins( 0 to 14 ) as function( byref as string, byref as string, byref as string, byref as string ) as string
    dim _plugin_count as integer

    dim as string _script
    dim as DICTSTRING _vars
    dim as DICTSTRING _labels
    
    dim as string _table
    
    dim as string _last_value
    dim as string _last_getset
    
    function addPlugin( pl as function( as string, as string, as string, as string ) as string ) as integer
        if( Flow._plugin_count < ubound( Flow._plugins ) ) then
            Flow._plugins(Flow._plugin_count) = pl
            Flow._plugin_count += 1
            return TRUE
        end if
        return FALSE
    end function
    
    function load( src as string, vars as DICTSTRING = "" ) as integer
        ' first, we look for labels, each must be on a new line
        Flow._labels = Dict.create()
        Flow._script = (src & "\n")
        Flow._vars = vars
        dim as integer s = 1, e = instr( Flow._script, !"\n" )
        while( e > 0 )
            while( s < e )
                if( asc( Flow._script, s ) = 58 ) then        ' 58 is :
                    Dict.set( Flow._labels, mid( Flow._script, s, (e - s - 1) ), s ) 
                    ' note: we set to s above as this will "run" the label (allowing logging etc. as well
                    '  as causing the line end reset code to trigger,
                    '  we could use e to indicate the end of the label instead but wouldn't get the
                    '  free "end-of-line reset" then or logging.
                elseif( asc( Flow._script, s ) <= 32 ) then
                    ' nothing
                else
                    exit while
                end if
                s += 1
            wend
            s = (e + 1)
            e = instr( s, Flow._script, !"\n" )
        wend
        return TRUE
    end function
    
    function run( from as string = "" ) as integer
        dim as integer call_stack(30)
        dim as integer call_stack_ptr
        dim as integer s_idx, l = len( Flow._script ), t
        dim as integer in_q = FALSE, wait_eol = FALSE
        dim as string c, token = "", arg, current_app = "", ts
        dim as integer s = 1
        dim as byte er      'used for error in _getVar()
        if( len( from ) > 0 ) then
            s = Dict.intValueOf( Flow._labels, from, -1 )
            if( s = -1 ) then
                Utils.echoError( "[Flow] Missing label: " & from & "  @ 0")
                return 0
            end if
        end if
        Flow.call_stack_ptr = 0
        
        for s_idx = s to l
            c = chr( asc( Flow._script, s_idx ) )
            if( wait_eol ) then             ' we're looking the end of the line ignoring until then
                if( c = !"\n" ) then
                    wait_eol = FALSE
                    token = ""
                    ' we'll continue with c=end_of_line and no token so the parser can
                    ' reset as normal for encountering end of line
                else
                    continue for
                end if
            end if
            if( in_q ) then                 ' we're in quotes
                if( c = "|" ) then
                    s_idx += 1
                    select case( chr( asc( Flow._script, s_idx ) ) )
                        case "q":   token &= """"
                        case "n":   token &= !"\n"
                        case "t":   token &= !"\t"
                        case "b":   token &= "|"
                    end select
                elseif( c = """" ) then
                    in_q = FALSE        ' we do not add the ending quote to the string
                else
                    token &= c
                end if
            elseif( c = !"\r" ) then
                ' ignore these, we only break on \n, lazy but it works - no-one uses just \r any more
            elseif( c = "#" ) then
                wait_eol = TRUE
            elseif( c = """" ) then
                token &= """"       ' add initial "
                in_q = TRUE
            elseif( (c = ",") or (c = ";") or (c = "[") or (c = "]") ) then
                ' we ignore these
            elseif( (c = " ") or (c = !"\t") or (c = !"\n") ) then
                if( len( token ) > 0 ) then
                    select case chr( asc( token, 1 ) )
                        case ">", "<", ":", "~":
                            arg = mid( token, 2 )
                            token = chr( asc( token, 1 ) )
                        case else:
                            t = instr( token, "(" )
                            if( t > 0 ) then
                                if( (chr( asc( token, len( token ) ) ) = ")") ) then
                                    arg = mid( token, (t + 1), (len( token ) - t - 1) )
                                    token = mid( token, 1, (t - 1) )
                                else
                                    Utils.echoError( "[Flow] Invalid command spec: " & token & "  @ " & s_idx)
                                    return 0
                                end if
                            else
                                arg = ""
                            end if
                    end select
                    if( len( token ) > 0 ) then
                        ' > & : are a special cases as they're plain strings (should be var)
                        ' being treated as though they were text strings
                        if( (token <> ">") andalso (token <> ":") ) then
                            if( asc( arg, 1 ) = 34 ) then           '34 is "
                                'quoted string
                                arg = mid( arg, 2 )
                            elseif( asc( arg, 1 ) = 58 ) then       ' 58 is :
                                'arg = mid( arg, 2 )
                            elseif( instr( "+-0123456789", chr( asc( arg, 1 ) ) ) > 0 ) then
                                'number
                                arg = str( val( arg ) )
                            elseif( len( arg ) > 0 ) then
                                'must be a variable
                                er = 0
                                ts = Flow._getVar( arg, @er )
                                if( er = 1 ) then
                                    arg = ts
                                else
                                    Utils.echoError( ("[Flow] Undefined variable: " & arg & "  @ " & s_idx) )
                                    return 0
                                end if
                            end if
                        end if
                        ' these need access to the program counter so must be in here
                        if( token = "gosub" ) then
                            if( call_stack_ptr < (ubound( call_stack ) - 1) ) then
                                call_stack_ptr += 1
                                call_stack(call_stack_ptr) = s_idx
                                token = "goto"      ' use goto to do the work
                            end if
                        elseif( token = "return" ) then
                            if( call_stack_ptr > 0 ) then
                                s_idx = (call_stack(call_stack_ptr) - 1)
                                call_stack_ptr -= 1
                                token = ""
                            end if
                        end if
                        select case( token )
                            case ">"
                                Flow._setVar( arg, Flow._table )
                                'Dict.set( Flow._vars, arg, Flow._table )
                            case "<"
                                Flow._table = arg
                            case "~"
                                Dict.remove( Flow._vars, arg )
                            case ":"
                                #ifdef Flow_LOG_LABELS
                                    Utils.echo( ("[Flow] Encountered Label: " & arg & "  @ " & s_idx & !"\n") )
                                #endif
                            case ""
                                ' do nothing
                            case else
                                token = lcase( token )
                                t = instr( token, "." )
                                if( t = 0 ) then
                                    current_app = Utils.EMPTY_STRING
                                elseif( t = 1 ) then
                                    ' remove the .
                                    token = mid( token, 2 )
                                else
                                    current_app = mid( token, 1, (t - 1) )
                                    token = mid( token, (t + 1) )
                                end if
                                arg = Flow.flowCommand( Flow._table, current_app, token, arg )
                                select case( (asc( arg, 1 ) - asc( "0" )) )
                                    case 0:     ' CMD_ERROR
                                        Utils.echoError( ("[Flow] " & mid( arg, 2 ) & "  @ " & s_idx) )
                                        return 0
                                    case 1:     ' CMD_OK
                                    case 2:     ' CMD_OK_SET
                                        Flow._table = mid( arg, 2 )
                                    case 3:     'goto
                                        s_idx = Dict.intValueOf( Flow._labels, mid( arg, 2 ), -1 )
                                        if( s_idx = -1 ) then
                                            Utils.echoError( ("[Flow] Missing label: " & mid( arg, 2 ) & "  @ " & s_idx) )
                                            return 0
                                        end if
                                        s_idx -= 1      ' "next" will add one 
                                    case 4:     ' CMD_SKIP
                                        wait_eol = TRUE
                                    case 5:     ' CMD_STOP
                                        exit for
                                end select
                        end select
                        token = ""
                    end if
                end if
                if( c = !"\n" ) then
                    'end of line resets
                    'table = ""
                end if
            else
                token &= c
            end if
        next
        return 1
    end function
    
    ' this returns the action and value as [action][value...] where action
    ' is 0-9:
    '	0 	error	fail with message in value
    '	1	ok	ok, ignore value
    '	2	set	ok, put "value" on the table
    '	3	goto	goto the label in value
    '	4	skip	stop parsing the line
    '	5	stop	stop the script, return value
    '	9	not_me	special value for plugins to indicate that command is not theirs
    '
    ' it also has the special case of handling plugins
    function flowCommand( table as string, plugin as string, cmd as string, arg as string ) as string
        if( len( plugin ) > 0 ) then
            ' this is a plugin
            if( Flow._plugin_count > 0 ) then
                dim as integer i
                dim as string res
                for i = 0 to (Flow._plugin_count - 1)
                    res = Flow._plugins(i)( table, plugin, cmd, arg )
                    if( asc( res, 1 ) <> 57 ) then       ' 57 is "9" - NOT_OURS
                        return res
                    end if
                next
            end if
            RETURN_ERROR( ("No such class " & plugin) )
        end if
        dim as string ts
        select case( cmd )
            'variables
            case "getvar"
                dim as byte er = 0
                ts = Flow._getVar( arg, @er )
                if( er = 1 ) then
                    RETURN_OK_SET( ts )
                else
                    RETURN_ERROR( ("Missing variable: " & arg) )
                end if
                
            'flow control
            case "goto"
                return (Flow.CMD_GOTO & arg)
            case "stop"
                return Flow.CMD_STOP
            
            'output
            case "echo"
                Utils.echo( table )
                return "1"
                
            'strings
            case "findindexof"
                RETURN_OK_SET( (instr( table, arg ) - 1) )
            case "findanycaseindexof", "findindexofanycase"
                RETURN_OK_SET( (instr( lcase( table ), lcase( arg ) ) - 1) )
            case "croprightoffat", "cropto"
                RETURN_OK_SET( mid( table, 1, val( arg ) ) )
            case "cropleftoffat", "cropfrom":
                RETURN_OK_SET( mid( table, (val( arg ) + 1) ) )
            case "cutstring"
                ts = Flow.newBundleWithKV( "start", mid( table, 1, val( arg ) ) )
                ts = Flow.setPart( ts, "end", mid( table, (val( arg ) + 1) ) )
                RETURN_OK_SET( ts )
            case "getlength", "length"
                RETURN_OK_SET( str( len( table ) ) )
                
            'parts
            case "newbundle"
                RETURN_OK_SET( Flow.newBundle( table, arg ) )
            case "part"
                Flow._last_value = arg
                Flow._last_getset = "part"
                return Flow.CMD_OK
                
            'get/set
            case "set"
                select case( Flow._last_getset )
                    case "part":
                        table = Flow.setPart( table, Flow._last_value, arg )
                        RETURN_OK_SET( table )
                    case else:
                        RETURN_ERROR( "SET with unknown modifier-type" )
                end select
            case "get"
                select case( Flow._last_getset )
                    case "part":
                        RETURN_OK_SET( Flow.getPart( table, Flow._last_value ) )
                    case else:
                        RETURN_ERROR( "GET with unknown modifier-type" )
                end select
                
            ' conditionals
            case "?", "true?"
                if( (table <> "") andalso (val( table ) <> 0) ) then
                    return Flow.CMD_OK
                else
                    return Flow.CMD_SKIP
                end if
            case "false?"
                if( (table = "") orelse (val( table ) = 0) ) then
                    return Flow.CMD_OK
                else
                    return Flow.CMD_SKIP
                end if
                
            ' conditional operations
            case "is", "isequalto", "is=", "is=="
                return iif( (table = arg), OK_SET_TRUE, OK_SET_FALSE )
            case "isgt", "isgreaterthan", "isover", "is>"
                return iif( (val( table ) > val( arg )), OK_SET_TRUE, OK_SET_FALSE )
            case "isgte", "isgreaterthanorequalto", "is>="
                return iif( (val( table ) >= val( arg )), OK_SET_TRUE, OK_SET_FALSE )
            case "islt", "islessthan", "isunder", "is<"
                return iif( (val( table ) < val( arg )), OK_SET_TRUE, OK_SET_FALSE )
            case "islte", "islessthanorequalto", "is<="
                return iif( (val( table ) <= val( arg )), OK_SET_TRUE, OK_SET_FALSE )
            case "isnot", "isnotequalto", "is<>", "is!="
                return iif( (table <> arg), OK_SET_TRUE, OK_SET_FALSE )
            
            ' maths ops
            case "append", "join"
                return (Flow.CMD_OK_SET & table & arg)
            case "add", "increaseby"
                return (Flow.CMD_OK_SET & (val( table ) + val( arg )))
            case "subtract", "decreaseby"
                return (Flow.CMD_OK_SET & (val( table ) - val( arg )))
            case "multiplyby"
                return (Flow.CMD_OK_SET & (val( table ) * val( arg )))
            case "divideby"
                return (Flow.CMD_OK_SET & (val( table ) / val( arg )))
            case "moddivby"
                return (Flow.CMD_OK_SET & (val( table ) mod val( arg )))
            
            'boolean ops
            case "and"
                RETURN_OK_SET( iif( (Flow._toBool( table ) and Flow._toBool( arg )), "1", "0" ) )
            case "or"
                RETURN_OK_SET( iif( (Flow._toBool( table ) or Flow._toBool( arg )), "1", "0" ) )
            case "not"
                RETURN_OK_SET( iif( Flow._toBool( table ), "0", "1" ) )
                
            case else
                ' TODO: plugins
        end select
        return ("0Command not recognised: " & cmd)
    end function
    
    sub setTableValue( v as string )
        Flow._table = v
    end sub
    
    function setPart( d as string, sk as string, sv as string ) as string
        dim ofs as integer = 1
        dim k as string
        dim as integer m = 0
        dim as string result = ""
        dim as string s = k
        dim as integer i
        dim as integer rstart
        while( ofs <= len( d ) )
            dim o as integer
            dim rl as integer = 0
            rstart = ofs
            o = asc( d, ofs )
            ofs += 1
            while( o >= 97 )
                rl = ((rl + (o - 97)) shl 4)
                o = asc( d, ofs )
                ofs += 1
            wend    
            rl += (o - 65)
            o = ofs
            ofs += rl
            if( m = 0 ) then
                k = mid( d, o, rl )
                if( k = sk ) then
                    m = 2   ' don't write the key and value back, we will add at the end
                else
                    result &= mid( d, rstart, (rl + (o - rstart)) )
                    m = 1   ' write the key and value
                end if
            else
                if( m = 1 ) then
                    result &= mid( d, rstart, (rl + (o - rstart)) )
                end if
                m = 0
            end if
        wend
        s = sk
        for i = 0 to 1
            dim l as integer = len( s )
            dim z as string = chr( (65 + (l and 15)) )
            while( l > 15 )
                l = (l shr 4)
                z = (chr( (97 + (l and 15)) ) & z)
            wend
            result &= z
            result &= s
            s = sv
        next
        return result
    end function
    
    function getPart( from as string, gk as string, def as string = "" ) as string
        dim ofs as integer = 1
        dim k as string
        dim as integer m = 0
        
        while( ofs < len( from ) )
            dim o as integer
            dim l as integer = 0
            o = asc( from, ofs )
            ofs += 1
            while( o >= 97 )
                l = ((l + (o - 97)) shl 4)
                o = asc( from, ofs )
                ofs += 1
            wend    
            l += (o - 65)
            o = ofs
            ofs += l
            if( m = 0 ) then
                k = mid( from, o, l )
                m = 1
            else
                if( k = gk ) then
                    return mid( from, o, l )
                end if
                m = 0
            end if
        wend        
        return def
    end function

    function containsPart( from as string, gk as string ) as integer
        dim ofs as integer = 1
        dim k as string
        dim as integer m = 0
        
        while( ofs < len( from ) )
            dim o as integer
            dim l as integer = 0
            o = asc( from, ofs )
            ofs += 1
            while( o >= 97 )
                l = ((l + (o - 97)) shl 4)
                o = asc( from, ofs )
                ofs += 1
            wend    
            l += (o - 65)
            o = ofs
            ofs += l
            if( m = 0 ) then
                k = mid( from, o, l )
                m = 1
            else
                if( k = gk ) then
                    return TRUE
                end if
                m = 0
            end if
        wend        
        return FALSE
    end function
    
    function newBundle( from as string = "", keys as string = "" ) as string
        if( len( keys ) = 0 ) then
            return ""
        end if
        keys &= "/"
        dim as integer s = 1, e = instr( keys, "/" )
        dim as string key, result = ""
        while( e > 0 )
            key = mid( keys, s, (e - s) )
            result = Flow.setPart( result, key, Flow.getPart( from, key ) )
            s = (e + 1)
            e = instr( s, from, "/" )
        wend
        return result
    end function
    
    function newBundleWithKV( key as string, value as string ) as string
        dim as string p = ""
        return Flow.setPart( p, key, value )
    end function
    
    function _getVar( k as string, er as byte ptr ) as string
        dim as integer e = instr( k, "'" )
        if( e > 0 ) then
            dim as string bundle = mid( k, 1, (e - 1) )
            k = mid( k, (e + 1) )
            if( len( bundle ) = 0 ) then
                bundle = Flow._table
            else
                if( Dict.containsKey( Flow._vars, bundle ) ) then
                    bundle = Dict.valueOf( Flow._vars, bundle )
                else
                    *er = 0
                    return Utils.EMPTY_STRING
                end if
            end if
            if( Flow.containsPart( bundle, k ) ) then
                *er = 1
                return Flow.getPart( bundle, k )
            end if
        else
            if( Dict.containsKey( Flow._vars, k ) ) then
                *er = 1
                return Dict.valueOf( Flow._vars, k )
            end if
        end if
        *er = 0
        return Utils.EMPTY_STRING
    end function
    
    sub _setVar( k as string, v as string )
        dim as integer e = instr( k, "'" )
        if( e > 0 ) then
            dim as byte using_table = 0
            dim as string bundle = mid( k, 1, (e - 1) )
            dim as string b
            k = mid( k, (e + 1) )
            if( len( bundle ) = 0 ) then
                using_table = 1
                b = Flow._table
            else
                if( Dict.containsKey( Flow._vars, bundle ) ) then
                    b = Dict.valueOf( Flow._vars, bundle )
                else
                    b = ""
                end if
            end if
            b = Flow.setPart( b, k, v )
            if( using_table ) then
                Flow._table = b
            else
                Dict.set( Flow._vars, bundle, b )
            end if
        else
            Dict.set( Flow._vars, k, v )
        end if
    end sub
    
    function _toBool( v as string ) as integer
        if( val( v ) = 0 ) then
            return FALSE
        end if
        return TRUE
    end function
            
end namespace

dim as string script_file = command(1)
if( script_file = "" ) then
    script_file = "test.txt"
end if

dim as integer er = 0
dim as string script = Utils.readFile( script_file, er )
if( er <> 0 ) then
    print "Unable to load script file: " & script_file
    end
end if

#include "modplatform.bas"
FlowPlatform.init()

dim as DICTSTRING vars = Dict.create()

Flow.load( script, vars )
Utils.echo( !"\n\nResult: " & Flow.run() )

'sleep
end


