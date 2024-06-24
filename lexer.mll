{
  open Parser
}

let non_zero_digit=['1'-'9']
let digit = ['0'-'9']
let zero= ['0']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alphanumeric = lowercase | uppercase | digit
let variable_start = uppercase | '_'
let variable_char = alphanumeric|uppercase
let identifier = lowercase (alphanumeric | '_')*
let variable = variable_start variable_char*
let number = (zero)|(('-')* (non_zero_digit) (digit)*)
let float_num= ('-')* (digit)+ ('.') (digit)+ 

rule token = parse
  | [' ' '\t' '\r' '\n']    { token lexbuf } (* Skip whitespace *)
  | '('                       { LEFTP }
  | ')'                       { RIGHTP }
  | ','                       { COMMA }
  | '.'                       { PERIOD}
  | ":-"                      { COLON_DASH }
  | "?-"                      { GOAL_HEAD }
  | number as dig_            { INT dig_ }
  | ("true"|"false") as bool_ { BOOL bool_ }
  | float_num as float_       { FLOAT float_ }
  | identifier as id          { ID id }
  | variable as var_          { VARIABLE var_}
  | '['                       { LEFT_SQ_BRACKET }
  | ']'                       { RIGHT_SQ_BRACKET }
  | ';'                       { SEMICOLON }
  (* | "/*"                      { LEFT_COMMENT }
  | "*/"                      { RIGHT_COMMENT } *)
  | '~'                       { EOF }
