' tokeniser
#include "../lib/libmain.bas"

'#define TSCRIPT_LOG

#ifdef TSCRIPT_LOG
#define TSCRIPT_LOG_LABELS
#endif

' these are used internall as results for boolean operators
#define OK_SET_TRUE "21"
#define OK_SET_FALSE "20"
#define RETURN_OK_SET(n) return( (TScript.CMD_OK_SET & n) )
#define RETURN_ERROR(n) return( (TScript.CMD_ERROR & n) )

namespace TScript
    ' These are for returning from tableCommand()
    const as string  _
            CMD_ERROR   = "0",  _
            CMD_OK      = "1",  _
            CMD_OK_SET  = "2",  _
            CMD_GOTO    = "3",  _
            CMD_SKIP    = "4",  _
            CMD_STOP    = "9"
    
    declare function load( script as string, vars as DICTSTRING = "" ) as integer
    declare function run( from as string = "" ) as integer

    declare function tableCommand( table as string, cmd as string, arg as string ) as string

    declare function _setPart( d as string, sk as string, sv as string ) as string
    declare function _getPart( from as string, gk as string ) as string
    declare function _newParts( from as string = "", keys as string = "" ) as string

    dim as string _script
    dim as DICTSTRING _vars
    dim as DICTSTRING _labels
    
    dim as string _last_value
    dim as string _last_getset
    
    function load( src as string, vars as DICTSTRING = "" ) as integer
        ' first, we look for labels, each must be on a new line
        TScript._labels = Dict.create()
        TScript._script = (src & "\n")
        TScript._vars = vars
        dim as integer s = 1, e = instr( TScript._script, !"\n" )
        while( e > 0 )
            while( s < e )
                if( asc( TScript._script, s ) = 58 ) then        ' 58 is :
                    Dict.set( TScript._labels, mid( TScript._script, (s + 1), (e - s - 2) ), s ) 
                    ' note: we set to s above as this will "run" the label (allowing logging etc. as well
                    '  as causing the line end reset code to trigger,
                    '  we could use e to indicate the end of the label instead but wouldn't get the
                    '  free "end-of-line reset" then or logging.
                elseif( asc( TScript._script, s ) <= 32 ) then
                    ' nothing
                else
                    exit while
                end if
                s += 1
            wend
            s = (e + 1)
            e = instr( s, TScript._script, !"\n" )
        wend
        return TRUE
    end function
    
    function run( from as string = "" ) as integer
        dim as string table
        dim as integer call_stack(30)
        dim as integer call_stack_ptr
        dim as integer s_idx, l = len( TScript._script ), t
        dim as integer in_q = FALSE, wait_eol = FALSE
        dim as string c, token = "", arg, current_app = ""
        dim as integer s = 1
        if( len( from ) > 0 ) then
            s = Dict.intValueOf( TScript._labels, from, -1 )
            if( s = -1 ) then
                Utils.echoError( "[TScript] Missing label: " & from & "  @ 0")
                return 0
            end if
        end if
        TScript.call_stack_ptr = 0
        
        for s_idx = s to l
            c = chr( asc( TScript._script, s_idx ) )
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
                    select case( chr( asc( TScript._script, s_idx ) ) )
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
                                    Utils.echoError( "[TScript] Invalid command spec: " & token & "  @ " & s_idx)
                                    return 0
                                end if
                            else
                                arg = ""
                            end if
                    end select
                    if( len( token ) > 0 ) then
                        ' > & : are a special cases as they're plain strings (should be var)
                        ' being treated as though they were text strings
                        if( (token <> ">") and (token <> ":") ) then
                            if( asc( arg, 1 ) = 34 ) then           '34 is "
                                'quoted string
                                arg = mid( arg, 2 )
                            elseif( asc( arg, 1 ) = 58 ) then       ' 58 is :
                                arg = mid( arg, 2 )
                            elseif( instr( "+-0123456789", chr( asc( arg, 1 ) ) ) > 0 ) then
                                'number
                                arg = str( val( arg ) )
                            elseif( len( arg ) > 0 ) then
                                'must be a variable
                                'if( TScript._containsVar( arg ) ) then
                                if( Dict.containsKey( TScript._vars, arg ) ) then
                                    arg = Dict.valueOf( TScript._vars, arg )
                                else
                                    Utils.echoError( ("[TScript] Undefined variable: " & arg & "  @ " & s_idx) )
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
                                'TScript._setVar( arg, table )
                                Dict.set( TScript._vars, arg, table )
                            case "<"
                                table = arg
                            case "~"
                                Dict.remove( TScript._vars, arg )
                            case ":"
                                #ifdef TSCRIPT_LOG_LABELS
                                    Utils.echo( ("[TScript] Encountered Label: " & arg & "  @ " & s_idx & !"\n") )
                                #endif                            
                            case ""
                                ' do nothing
                            case else
                                token = lcase( token )
                                t = instr( token, "." )
                                if( t = 1 ) then
                                    token = (current_app & token)
                                elseif( t > 1 ) then
                                    'TODO: check this works
                                    current_app = mid( token, (t + 1) )
                                end if
                                arg = TScript.tableCommand( table, token, arg )
                                select case( (asc( arg, 1 ) - asc( "0" )) )
                                    case 0:     'error, fail with given message
                                        Utils.echoError( ("[TScript] " & mid( arg, 2 ) & "  @ " & s_idx) )
                                        return 0
                                    case 1:     'ok, do nothing else
                                    case 2:     'set_ok, put result on the table
                                        table = mid( arg, 2 )
                                    case 3:     'goto
                                        s_idx = Dict.intValueOf( TScript._labels, mid( arg, 2 ), -1 )
                                        if( s_idx = -1 ) then
                                            Utils.echoError( ("[TScript] Missing label: " & mid( arg, 2 ) & "  @ " & s_idx) )
                                            return 0
                                        end if
                                        s_idx -= 1      ' "next" will add one 
                                    case 4:     'wait_eol
                                        wait_eol = TRUE
                                    case 5:
                                        exit for 'stop
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
    function tableCommand( table as string, cmd as string, arg as string ) as string
        select case( cmd )
            'flow control
            case "goto"
                return ("3" & arg)
            case "stop"
                return "5"
            
            'output
            case "echo"
                Utils.echo( table )
                return "1"
                
            'strings
            case "findindexof"
                RETURN_OK_SET( (instr( table, arg ) - 1) )
            case "keepto", "keepupto"
                RETURN_OK_SET( mid( table, 1, (val( arg ) + 1) ) )
            case "keepfrom":
                RETURN_OK_SET( mid( table, (val( arg ) + 1) ) )
                
            'parts
            case "newparts", "newdict"
                RETURN_OK_SET( TScript._newParts( table, arg ) )
            case "part"
                TScript._last_value = arg
                TScript._last_getset = "part"
                return TScript.CMD_OK
                
            'get/set
            case "set"
                select case( TScript._last_getset )
                    case "part":
                        table = TScript._setPart( table, TScript._last_value, arg )
                        RETURN_OK_SET( table )
                    case else:
                        RETURN_ERROR( "SET with unknown modifier-type" )
                end select
            case "get"
                select case( TScript._last_getset )
                    case "part":
                        RETURN_OK_SET( TScript._getPart( table, TScript._last_value ) )
                    case else:
                        RETURN_ERROR( "GET with unknown modifier-type" )
                end select
                
            ' conditionals
            case "?", "true?"
                if( (table <> "") andalso (cint( table ) <> 0) ) then
                    return TScript.CMD_OK
                else
                    return TScript.CMD_SKIP
                end if
            case "false?"
                if( (table = "") orelse (cint( table ) = 0) ) then       '48 is 0
                    return TScript.CMD_OK
                else
                    return TScript.CMD_SKIP
                end if
                
            ' conditional operations
            case "is", "isequalto"
                return iif( (table = arg), OK_SET_TRUE, OK_SET_FALSE )
            case "gt", "greaterthan", "isgreatherthan"
                return iif( (val( table ) > val( arg )), OK_SET_TRUE, OK_SET_FALSE )
            case "gte", "greaterthanorequalto", "isgreatherthanorequalto"
                return iif( (val( table ) >= val( arg )), OK_SET_TRUE, OK_SET_FALSE )
            case "lt", "lessthan", "islessthan"
                return iif( (val( table ) < val( arg )), OK_SET_TRUE, OK_SET_FALSE )
            case "lte", "lessthanorequalto", "islessthanorequalto"
                return iif( (val( table ) <= val( arg )), OK_SET_TRUE, OK_SET_FALSE )
            case "not", "isnotequalto"
                return iif( (table <> arg), OK_SET_TRUE, OK_SET_FALSE )
                
            ' maths ops
            case "append", "join"
                return (TScript.CMD_OK_SET & table & arg)
            case "add", "increaseby"
                return (TScript.CMD_OK_SET & (val( table ) + val( arg )))
            case "subtract", "decreaseby"
                return (TScript.CMD_OK_SET & (val( table ) - val( arg )))
            case "multiplyby"
                return (TScript.CMD_OK_SET & (val( table ) * val( arg )))
            case "divideby"
                return (TScript.CMD_OK_SET & (val( table ) / val( arg )))
            case "moddivby"
                return (TScript.CMD_OK_SET & (val( table ) mod val( arg )))
                
            case else
                ' TODO: plugins
        end select
        return ("0Command not recognised: " & cmd)
    end function
    
    function _setPart( d as string, sk as string, sv as string ) as string
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
    
    function _getPart( from as string, gk as string ) as string
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
        return ""
    end function

    function _newParts( from as string = "", keys as string = "" ) as string
        if( len( keys ) = 0 ) then
            return ""
        end if
        keys &= "/"
        dim as integer s = 1, e = instr( keys, "/" )
        dim as string key, result = ""
        while( e > 0 )
            key = mid( keys, s, (e - s) )
            result = TScript._setPart( result, key, TScript._getPart( from, key ) )
            s = (e + 1)
            e = instr( s, from, "/" )
        wend
        return result
    end function
end namespace

dim as string script = Utils.readFile( "test.txt" )
dim as DICTSTRING vars = Dict.create()

TScript.load( script, vars )
Utils.echo( !"\n\nResult: " & TScript.run() )

sleep
end


