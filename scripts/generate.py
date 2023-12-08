import re

template = '''#include "lexer.hh"

#include <string>

constexpr auto TokenType2String(int token) -> const std::string {{
  switch (token)  {{
{} 
  default:
    return "<Error>";
  }}
}}
'''

template_case_return =  '''  case TokenType::{}:
    return "<{}>";
'''

pattern = r"(k[^,]*),"
case_return = ''

with open("src/lexer.hh") as fr: 
  for line in fr:
    match = re.search(pattern, line)
    if match:
      token_type = match.group(1)
      case_return += template_case_return.format(token_type, token_type[1:])
  
  with open("src/token2string.hh", '+w') as fw:
    fw.write(template.format(case_return)) 
